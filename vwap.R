# vwap.R: prototyping vwap algo prep in R
#
# Tito Ingargiola

library(data.table)
library(lubridate)
library(PerformanceAnalytics)
library(futile.logger)
library(ggplot2)
library(TTR)

SPY.FILE <- 'spy16.Rdata'

# pulls spy from bbrg and writes to file
pull.from.bbrg <- function(spy.file=SPY.FILE) {
  library(Rblpapi)
  blpConnect()
  spy16 <- getTicks(security="SPY US EQUITY", 
		  startTime=as.POSIXct("2016-01-01 0:0:0"))
  save(spy16,file=spy.file)
}

# reads bbrg file and preps for use
init <- function(spy.file=SPY.FILE) {
  load(file=spy.file)
  spydt <- as.data.table(spy16)
  # set tz
  attr(spydt$times, "tzone") <- 'America/New_York'
  spydt$date <- as.Date(spydt$times)
  setnames(spydt,'times','time')
  setcolorder(spydt, c('date','time','value','size'))
  spydt <<- spydt[size>0]
}

# calcs and optionally plots vwap for specified day 
vwap4day <- function(day='2016-04-20', trades=spydt, plot=TRUE) {
  flog.info('%s',format(day))
  DT <- trades[date==day]
  DT[,cumsize:=cumsum(size)]
  
  # calculate vwap
  DT[,vwap:=value]
  for (i in 2:nrow(DT))
    set(DT, i, 6L, ((DT$vwap[i-1]*DT$cumsize[i-1]) 
						+ (DT$value[i]*DT$size[i]))/DT$cumsize[i])
  dts <- merge( price=xts(DT$value,order.by=DT$time),
		  vwap=xts(DT$vwap,order.by=DT$time))
  if (plot)  print(autoplot(dts,facets=NULL,main=day))
  dts
}  

# calcs and optionally plots segments of vwap for specified day
vwapsegs4day <- function(day='2016-04-20', trades=spydt, plot=TRUE, 
		seglength=10, pdf.file=NULL) {
	
  flog.info('%s',format(day))
  DT <- trades[date==day]

  begin <- DT$time[1]
  end <- DT$time[nrow(DT)]
  
  if (plot==TRUE && !is.null(pdf.file) ) {
    pdf(file=pdf.file,onefile=TRUE);
  }
  
  segst <- begin
  allsegs <- list()
  
  while(segst<=end) {
    nseg <- segst + minutes(seglength)
    seg <- DT[ time >= segst & time < nseg]
    seg[,cumsize:=cumsum(size)]
    seg[,vwap:=value]
    for (i in 2:nrow(seg))
      set(seg, i, 6L, ((seg$vwap[i-1]*seg$cumsize[i-1]) 
						  + (seg$value[i]*seg$size[i]))/seg$cumsize[i])
    allsegs <- c(allsegs,seg$vwap)
    segst <- nseg
    if (plot & segst <= end) {
      dts <- merge( price=xts(seg$value,order.by=seg$time),
			  vwap=xts(seg$vwap,order.by=seg$time))
      print(autoplot(dts,facets=NULL,main=segst))
    }
      
  }
  segnm <- paste('seg',seglength,sep='')
  DT[,segnm] <- unlist(allsegs)

  # calculate vwap
  DT[,vwap:=value]
  cumsize <- cumsum(DT$size)
  #browser()
  for (i in 2:nrow(DT))
    set(DT, i, 6L, ((DT$vwap[i-1]*cumsize[i-1]) 
						+ (DT$value[i]*DT$size[i]))/cumsize[i])
  dts <- merge( price=xts(DT$value,order.by=DT$time),
		  vwap=xts(DT$vwap,order.by=DT$time),
		  segnm=xts(unlist(allsegs),order.by=DT$time))
  if (plot)  print(autoplot(dts,facets=NULL,main=day))

  if (plot==TRUE && !is.null(pdf.file) ) {
    dev.off()
  }

  dts
}  

#  calcs scurve for a given day
scurve4day <- function(day='2016-04-20', trades=spydt) {
#  flog.info('%s',format(day))
  DT <- trades[date==day]

  dmin <- as.integer(minute(DT$time))
  dhour <- as.integer(hour(DT$time))
  DT$daymins <- dhour*60+dmin-570
  
  bymin <- DT[,sum(size),by=daymins]
  setnames(bymin,'V1','volume')
  bymin[,pv:=volume/sum(bymin$volume)]

  df<- data.frame(bymin$pv) 
  names(df) <- day
  df
}

# given a tgt/scurve and smoothed vol, returns a trading trajectory with
#   uncertainty bands and optionally plots
trading.trajectory <- function( tgt, sigma, n=.1, plot=TRUE) {
  mintgt <- pmax(0,(tgt - (n * sigma )))
  maxtgt <- pmin(1,( tgt + (n * sigma)))
  #browser()
  mintgt[length(mintgt)] <- maxtgt[length(maxtgt)] <- 1
  mintgt[1] <- maxtgt[1] <- 0
  traj <- data.frame(mintgt,tgt,maxtgt)
  
  if (plot) {
    plot(tgt,type='l', main='trajectory with uncertainty bands')
    lines( maxtgt,col='blue')
    lines( mintgt,col='red')
  }
  traj
}

# generate interesting charts and emit relevant output for strategy
play <- function() {
  init()
  alldays <- sort(unique(spydt$date))
  alldays <- head(alldays,length(alldays)-1)
  
  #generate all scurves and then plot them
  allscurves <- do.call('cbind',(lapply(alldays, scurve4day)))
  # and the median scurve & scurve/tgt
  scurve <- apply(allscurves,1,mean)
  plot(cumsum(scurve),type='b', main='Daily scurves')
  lapply(1:ncol(allscurves),function(day) {
    lines(cumsum(allscurves[,day]),col=colors()[day])
  })
  dev.new()
  tgt <- cumsum(scurve)
  # plot tgt
  plot(cumsum(scurve),type='b', main="'typical' scurve")

  #now let's treat volatility 
  sigma <- apply(allscurves,1,function(x){sd(x)/mean(x)})
  # too raw, let's smooth
  x <- 1:length(sigma)
  lo <- loess(sigma~x)
  sigma.smoothd <- predict(lo)
  dev.new()
  plot(sigma,main='Smoothing vol')
  lines(predict(lo),col='red',lwd='2')
  
  write.csv(data.frame(tgt,sigma.smoothd), "tgtsigma.csv")
  
  # now we can generate a baseline trajectoy
  traj <- trading.trajectory(tgt,sigma.smoothd)

  # if we want a smaller trading horizon, we need to rescale
  unscaled <- tgt[31:150]
  tgt10noon <- (unscaled-min(unscaled))/(max(unscaled)-min(unscaled))
  ssigma10noon <- sigma.smoothd[31:150]
  # and then everything else is the same
  traj <- trading.trajectory(tgt10noon,ssigma10noon)
}

