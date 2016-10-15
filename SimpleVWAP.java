package stratbox.strats;

import java.awt.EventQueue;
import java.io.BufferedReader;
import java.io.File;
import java.io.StringReader;
import java.nio.file.Files;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Random;

import javax.swing.JFrame;

import org.apache.commons.lang.ArrayUtils;
import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.data.time.Minute;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;

import stratbox.model.CCY;
import stratbox.model.Contract;
import stratbox.model.ContractExtent;
import stratbox.model.Execution;
import stratbox.model.INSTR;
import stratbox.model.Order;
import stratbox.model.PositionChange;
import stratbox.model.PositionRecord;
import stratbox.model.mktData.Quote;
import stratbox.model.mktData.Quote.Type;
import stratbox.model.mktData.Tick;
import stratbox.model.strategy.BasicStrategy;
import stratbox.model.strategy.BasicTactic;
import stratbox.model.strategy.Descriptor;
import stratbox.model.strategy.Param;
import stratbox.model.strategy.Strategy;
import stratbox.model.strategy.StrategyEvent;
import stratbox.model.strategy.StrategyFactory;
import stratbox.util.Time;

/**
 * Basic, single contract vwap order with uncertainty bands.
 * 
 * @author Tito Ingargiola
 */
public class SimpleVWAP extends BasicTactic {

    static Logger _Log = Logger.getLogger(SimpleVWAP.class);

    final static String _Desc = "Target VWAP for name.";

    public final static String Target = "vwap target";
	
    public final static String Ucurve = "ucurve file";
	
    public static Descriptor Descriptor() throws Exception {
	ArrayList<Param> params = new ArrayList<Param>();
	Contract.Extent ce = ContractExtent.Default();
	
	params.add(new Param("Target", Target, true, 
			     ce.getContract(INSTR.STK, "SMART", "SPY", CCY.USD) ));
	params.add(new Param("BuySell", "BuySell", false, Order.Action.BUY));
	params.add(new Param("Quantity", "Quantity", false, 10000));
	params.add(new Param("Ucurve", Ucurve, false, 
			     new File("src/Rstrats/tgtsigma.csv")));
	Time.OfDay start = new Time.OfDay
	    (2+Time.MinsSinceMidnight(new GregorianCalendar()));
	Time.OfDay end = new Time.OfDay(start.msm()+10);
	params.add(new Param("Start", "Start", false,start)); 
	params.add(new Param("End", "End", false, end));
	params.add(new Param("Aggression", "Aggression", true, .1));
	params.add(new Param("Plot", "Plot", false, true));
	
	Descriptor _desc = new Descriptor
	    ("stratbox.strats.SimpleVWAP", _Desc, params);
	
	return _desc;
    }

    public SimpleVWAP(Strategy strat, Descriptor d) { super(strat, d); }

    /** listen for select strategyEvents */
    public void strategyEvent(StrategyEvent event) {
	super.strategyEvent(event);
	switch (event.type) {
	case Activated:                 _init();	       break;
	case DescriptorChanged:	        _init();               break;
	case PositionChanged:	        _posnChange(event);    break;
	}
    }
    
    /**
     * mkt data goes here...
     */
    public void quote(Quote q) {
	long nowms = _now();
	Time.OfDay now = new Time.OfDay(Time.MinsSinceMidnight(nowms));
	if(now.msm() < _start.msm() || now.msm() >= _end.msm()) { // haven't started
	    return;
	} 
	Type t = q.type();
	if ( t == Type.ASK || t == Type.BID || t == Type.VOLUME) return;
	
	if (t == Type.TRADE) {
	    Tick tic = (Tick)q;
	    if (tic.size() > 0 && tic.price() > 0 && tic.contract()==_tgt) {
		_updateVwap(tic);
	    } else {
		//_Log.warn("Garbage tick: "+tic);
		return;
	    }	
	}
		
	if (nowms < _nextLook) { return; }
	
	// where are we in our trajectory?
	double complete = _mySz / (double)_qty ;
	// where should we be?
	
	Minute nowmin = new Minute(new Date(nowms));
	int idx = -1;
	try {
	    idx = _lb.getIndex(nowmin) + 1;
	} catch(Exception e) { 
	    _Log.error(nowmin + " " +e.getMessage()); 
	}
	
	if ( idx < 0 ) {
	    _Log.warn(now+" Couldn't get idx for "+nowmin);
	    return;
	}
	double lo = _lb.getValue(idx).doubleValue();
	double tgt = _traj.getValue(idx).doubleValue();
	double hi = _ub.getValue(idx).doubleValue();
	
	_Log.info(nowmin+" "+_df.format(complete)+" "+_df.format(lo)
		  +":"+_df.format(tgt)+":"+_df.format(hi));
	
	long tenish = 3000+((long)((new Random()).nextDouble()*(9000-3000)));
	_nextLook = nowms + tenish;//wait 10ish secs (we want to randomize a bit)
	
	try {
	    int currtgt = (int)Math.floor(tgt * _qty);
	    int shortfall = currtgt - _mySz;
	    _Log.info("(traj * tgt) - qty = Shortfall: ("+tgt+" * "+_qty
		      +") - "+_mySz+" = "+shortfall);
	    
	    _Log.info("My px: "+_mySz+"@"+_myPx+ " VWAP: "+_vwapPx);
	    
	    if ((_buysell == Order.Action.BUY && shortfall > 1 )
		||  (_buysell != Order.Action.BUY && shortfall <  -1 )) {
		Order o = _orderF().mktOrder
		    (_strat, _sc().defaultAccount(), _tgt,Math.abs(shortfall),
		     _buysell, Order.TIF.DAY);
		_execP().placeOrder(o);
	    }
	} catch (Exception e) {
	    _Log.error(e.getMessage(), e);
	}
    }
    
    /** executions go here **/
    public void execution(Execution exec) {
	_Log.info(exec);
	if (exec.type()== Execution.Type.FILL && exec.contract()==_tgt) {
	    //_myPx = ((_myPx*_mySz) + (exec.price()*exec.filled()))
	    //		/(_vwapSz+(int)exec.filled());
	    //_mySz += (int)exec.filled();	
	    PositionRecord p = _posn(_tgt);
	    _mySz = (int)p.qty();
	    _myPx = p.openAvgPrice();
	    _Log.info("My px: "+_mySz+"@"+_myPx+ " VWAP: "+_vwapPx);
	}
    }

    // /////// IMPL -------

    void _updateVwap( Tick trd ) {
	_mkttrades.add(trd);
	_vwapPx = ((_vwapPx*_vwapSz) + (trd.price()*trd.size()))
	    /(_vwapSz+trd.size());
	_vwapSz += (int)trd.size();	
	_Log.info("My px: "+_mySz+"@"+_myPx+ " VWAP: "+_vwapPx);
	if(Double.isNaN(_vwapPx )) {
	    _Log.info("why nan?");			
	}
    }
    
    /** react to position changed events */
    void _posnChange(StrategyEvent event) {
	PositionChange pc = (PositionChange) event.obj;
	if (pc == null || pc.after == null || pc.after.contract() == null)
	    return;
	Contract c = pc.after.contract();
	PositionRecord pr=_posn(c);
	_Log.info("posnChange: "+pr);
    }
    
    /** read metadata descriptor and store values */
    void _readDesc() {
	_tgt =((Contract) _desc.valueOf("Target"));
	_buysell =((Order.Action) _desc.valueOf("BuySell"));
	_qty =((Integer) _desc.valueOf("Quantity"));
	if (_buysell != Order.Action.BUY) _qty *= -1;
	_start = (Time.OfDay)_desc.valueOf("Start");
	_end = (Time.OfDay)_desc.valueOf("End");
	_aggr = ((Double)_desc.valueOf("Aggression")).doubleValue();
	_plot = (Boolean)_desc.valueOf("Plot");
	
	try {
	    _readTrajectory();
	} catch(Exception e) { _Log.error(e.getMessage(), e); }
    }
    
    // read ucurve/sigma file and generate daily and order trajectories
    void _readTrajectory() throws Exception {
	File file = (File)_desc.valueOf("Ucurve");
	String ucrvstr = new String(Files.readAllBytes(file.toPath()));
	BufferedReader bin = new BufferedReader(new StringReader(ucrvstr));
	bin.readLine(); // skip first line
	String line = bin.readLine();
	int i = 0;
	while(line != null) {
	    String[] tokenz = line.split(",");
	    try {
		_scrv[i] = Double.parseDouble(tokenz[0]);
		_sigma[i++] = Double.parseDouble(tokenz[1]);
	    } catch(NumberFormatException e) { 
		_Log.info(Arrays.toString(tokenz));
		_Log.error(e.getMessage(),e);
	    }
	    line = bin.readLine();
	}
	_trajectories(_scrv,_sigma, Time.OfUSEquities.Open.msm());
	_ordTrajectory();
    }
    
    // calculates vwap trajectory with uncertainty bands
    TimeSeriesCollection _trajectories(double[] scrv, double[] sigma, int msm) {
	GregorianCalendar now = _gcNow();
	int hours = (int)(msm/60);
	int mins = (int)(msm%60);
	Minute curr = new Minute(mins,hours,now.get(Calendar.DAY_OF_MONTH),
				 now.get(Calendar.MONTH)+1,now.get(Calendar.YEAR));
	TimeSeries tgt = new TimeSeries("tgt", Minute.class);
	TimeSeries lb = new TimeSeries("lb", Minute.class);
	TimeSeries ub = new TimeSeries("ub", Minute.class);
	
	for (int i = 0; i < scrv.length; i++) {
	    //_Log.debug(curr);
	    try {
		double t = scrv[i];
		double s = sigma[i];
		tgt.add(curr, t);
		lb.add(curr, Math.max(0,t - (_aggr * s)));
		ub.add(curr, Math.min(1,t + (_aggr * s)));
		if (i==0) {
		    lb.update(curr,0);					
		    ub.update(curr,0);					
		}
		if (i==scrv.length-1) {
		    lb.update(curr,1);					
		    ub.update(curr,1);										
		}
		curr = (Minute)curr.next();
	    } catch(Exception e) { _Log.error(e.getMessage(), e); }
	    
	}
	TimeSeriesCollection tsc = new TimeSeriesCollection(tgt);
	tsc.addSeries(lb);
	tsc.addSeries(ub);
	if (_plot) { _plot(tsc); }
	return tsc;
    }
    
    // calculate order trajectory
    void _ordTrajectory() {
	// we need to rescale over order trajectory
	int startMin = _start.msm() - Time.OfUSEquities.Open.msm();
	int endMin = _end.msm() - Time.OfUSEquities.Open.msm();
	int len = endMin-startMin+1;
	double[] otraj = new double[len];
	_Log.info("Trading trajectory is from "+_start+" - "+_end+" ("+len+")");
	System.arraycopy(_scrv, startMin, otraj, 0, otraj.length);
	double[] osigma = new double[len];
	System.arraycopy(_sigma, startMin, osigma, 0, osigma.length);
	List<Double> c = Arrays.asList(ArrayUtils.toObject(otraj));
	double min = Collections.min(c);
	double max = Collections.max(c);
	for (int i = 0; i < otraj.length; i++) {
	    otraj[i] = (otraj[i] - min)/(max-min);			
	}
	_tstraj = _trajectories(otraj,osigma,_start.msm());
	_traj = _tstraj.getSeries(0);
	_lb = _tstraj.getSeries(1);
	_ub = _tstraj.getSeries(2);
    }
	
	
    // plot a trajectory
    void _plot(TimeSeriesCollection tsc) {
	final JFreeChart chart = ChartFactory.createTimeSeriesChart
	    ("", "Minutes", "", tsc, false, false, false);
	EventQueue.invokeLater(new Runnable() {
		public void run() {
		    JFrame f = new JFrame();
		    ChartPanel p = new ChartPanel(chart);
		    p.setPreferredSize( new java.awt.Dimension( 560 , 370 ) );         
		    p.setMouseZoomable( true , false );         
		    f.setContentPane(p);
		    f.pack();
		    f.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		    f.setVisible(true);
		}
	    });		
    }
    
    /** necessary to play nice in stratbox */
    public SimpleVWAP clone() throws CloneNotSupportedException {
	SimpleVWAP clone = (SimpleVWAP) super.clone();
	clone._init();
	return clone;
    }
    
    /** initialize strategy */
    void _init() {
	_qty = 0;
	_vwapPx = 0;
	_vwapSz=0;
	_myPx = 0;
	_mySz=0;
	
	_scrv = new double[391];
	_sigma = new double[391];
	_aggr = 0;
	_nextLook = 0;
	
	_tstraj = null;
	_traj = null;
	_ub = null;
	_lb = null;
	_df = new DecimalFormat("#.##");
	_tgt = null;
	_buysell = null;
	
	_start = null;
	_end = null;
	_plot = null;
	_mkttrades = new ArrayList<Tick>(); 
	
	_readDesc();
	Time.OfDay now = new Time.OfDay();
	int diff = _start.msm() - now.msm(); 
	if (diff > 0) { _Log.info("Waiting "+diff+" minutes to start..."); }
	try { _subscribe(_tgt); } 
	catch (Exception e) { _Log.error(e.getMessage(), e); }
    }
    
    int _qty, _vwapSz, _mySz;
    double _vwapPx, _myPx, _aggr;
    double[] _scrv;
    double[] _sigma;
    long _nextLook;
    
    TimeSeriesCollection _tstraj;
    TimeSeries _traj, _ub,_lb;
    DecimalFormat _df;
    Contract _tgt;
    Order.Action _buysell;
    
    Time.OfDay _start,_end;
    Boolean _plot;
    ArrayList<Tick> _mkttrades; 
    
    // simple test driver
    public static void main(String[] args) throws Exception {
	BasicConfigurator.configure();
	StrategyFactory sf = StrategyFactory.Instance
	    (new File("env/Tactics.xml"));
	_Log.info("Built: "+sf);
	Descriptor[] descs = { Descriptor() };
	BasicStrategy strat = sf.stratWith("vwap", descs, false);
	_Log.info("Built: "+strat);
	SimpleVWAP vwap = (SimpleVWAP)strat.tactics()[0];
	vwap._init();
	_Log.info("done"); 	
    }   
}
