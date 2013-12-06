package csp.solver;

import csp.util.*;
import csp.solver.ASSolverPermut;

/** ASSolverPermutRW is the parallel implementation of Random Walk Adaptive Search solver
 * 	in the X10 language. This implementation use distributed isolated instances
 * 	of the solver, each one with a diferent seeds in order to have differents 
 * 	scanning walks in the search space.
 * 
 *  This implementation distribute the solver instances across places.
 * 
 * 	@author Danny Munera
 *  @version 0.1 	9 April, 2013  -> Fist Version
 * 					10 April, 2013 -> Changes queens by costas problem
 * 					12 April, 2013 -> TLP support
 */
import x10.util.Random;

import x10.array.*;
import x10.compiler.Inline;
import x10.util.concurrent.AtomicBoolean; 

/**
 * Each place has solvers, a PlaceLocalHandle[ASSolverPermutRW(sz)].
 * The standard way for code at place p to see the state at place q is
 * to execute 
 * <verbatim>
 * at(q) async { 
 *    val thisSolver = solvers(); 
 *    ... now thisSolver.csp gets you the local model,
 *    ...  thisSolver.solver gets you the local solver,
 *    ... etc
 *  >
 * </verbatim>
 */
public class ASSolverPermutRW(sz:Long,poolSize:Int) implements ParallelSolverI {  
    property sz()=sz;
    // Shared state, accessible from any place, via at(
	var csp_:ModelAS(sz);
	var solver:ASSolverPermut(sz);
	var time:Long;
	var winPlace : Place;
	
	val updateI : Int;
	val commOption : Int;
	
	var bcost : Int;
	var stats:CSPStats = null;
	var accStats:CSPStats = null;
	/** Comunication Variables*/
	val ep = new ElitePool( sz, poolSize ); 
	val thEnable : Int; 
	
	var conf: ASSolverConf(sz); // var because it needs to be set in solve.
	
	//Hybrid approach
	val nbExplorerPT : Int;
	val nTeams : Int;
	
	/**
	 * 	Constructor of the class
	 */
	public def this(vectorSize:Long, upI : Int, commOpt : Int , thread : Int , ps : Int, npT : Int ){
	    property(vectorSize,ps);
	    accStats = new CSPStats();
	    stats = new CSPStats();
		updateI = upI; 
		commOption = commOpt;
		thEnable = thread;
		
		nbExplorerPT = npT; // will be a parameter 
		nTeams = Place.MAX_PLACES as Int / nbExplorerPT ;
	}
	
	/** 
	 * 	Solve the csp problem with MAX_PLACES instance of AS solver
	 * 	The first one that reach a valid solution sends a kill to the others
	 * 	to finish the process.
	 * 
	 * 	@param size size of the csp problem
	 * 	@param cspProblem code with the problem to be solved (1 for Magic Square Problems, other number for Queens Problem)
	 * 	@return cost of the solution
	 */
	public def solve(st:PlaceLocalHandle[ParallelSolverI(sz)], cspGen:()=>ModelAS(sz) ):void { 
	    val solvers= st;
	    assert solvers() == this : "Whoa, basic plumbing problem -- I am not part of solvers!";
		val size = sz as Int;
		var extTime : Long = -System.nanoTime();
		
		val random = new Random();
		
		//val seed = random.nextLong();
		//Console.OUT.println("seed:"+seed);
		var nsize:Int = size;

		csp_ = cspGen(); // use the supplied generator to generate the problem
		
		conf = new ASSolverConf(sz, 1n /*ASSolverConf.USE_PLACES*/, solvers, updateI,0n, commOption, poolSize, nTeams );
		val ss = st() as ParallelSolverI(sz);
		//solver = new ASSolverPermut(sz, nsize, seed, ss);
		
		//Do I need to get more elegant way to obtain different seeds here? 
		//I'm currently using the solver id (place id) as seed
		solver = new ASSolverPermut(sz, nsize, here.id, ss);

		var cost:Int = x10.lang.Int.MAX_VALUE;
		
		/***/
		//taking the time only to solve the problem, not included the time to signal the others explorers

		Logger.debug(()=>" Start solve process: solver.solve() function ");
		
		time = -System.nanoTime();
		cost = solver.solve(csp_);
		time += System.nanoTime();
		
		if (cost == 0n){ 
		    // A solution has been found! Huzzah! 
		    // Light the candles! Kill the blighters!
		    val home = here.id;
		    
		    val winner:Boolean;
		    finish winner = at(Place.FIRST_PLACE) solvers().announceWinner(solvers, home);
		    
		    winPlace = here;
		    bcost = cost;
		 
		    if (winner) {
		    	setStats(solvers);
		    	//Utils.show("Solution is " + (csp_.verified()? "ok" : "WRONG") , csp_.variables);
		    	Console.OUT.println("Solution is " + (csp_.verified()? "ok" : "WRONG"));
		    	csp_.displaySolution();
		    }
		}
		extTime += System.nanoTime();
		//stats.time = extTime/1e9;
		//val stats_=stats;
		//Logger.debug(()=> "updating accStats");
		
		// accumulate results in place 0, need a better way at scale.
		//at (Place.FIRST_PLACE)  st().accStats(stats_);	
	}
	
	@Inline public def getIPVector(csp_:ModelAS(sz), myCost:Int):Boolean 
	  = conf.getIPVector(csp_, myCost);
	@Inline public def communicate(totalCost:Int, variables:Rail[Int]{self.size==sz} ):Int
	  = conf.communicate(totalCost, variables);
	
	@Inline public def intraTI():Int = conf.intraTI;
	@Inline public def restartPool():void { conf.restartPool();}
	
	val monitor = new Monitor("ASSolverPermutRW"); 
	public def kill() {
	        solver.kill=true;
	}
	val winnerLatch = new AtomicBoolean(false);
	public def announceWinner(ss:PlaceLocalHandle[ParallelSolverI(sz)], p:Long):Boolean {
	    val result = winnerLatch.compareAndSet(false, true);
	   
	  // Logger.info(()=> "announceWinner result=" + result + " for " + p + " this=" + this );
	    if (result) {
	         for (k in Place.places()) 
	            if (p != k.id) 
	                at(k) async ss().kill();
	    }
	    return result;
	}
	/**
	 * Called by winning place to set the stats at place zero so they
	 * can be printed out.
	 */
	def setStats(ss:PlaceLocalHandle[ParallelSolverI(sz)]  ){
		val winPlace = here.id;
		val time = time/1e9;
		val iters = solver.nbIterTot;
		val locmin = solver.nbLocalMinTot;
		val swaps = solver.nbSwapTot;
		val reset = solver.nbResetTot;
		val same = solver.nbSameVarTot;
		val restart = solver.nbRestart;
		val change = solver.nbChangeV;
	
		finish at (Place.FIRST_PLACE) async
		  	ss().setStats(0n, winPlace as Int, 0n, time, iters, locmin, swaps, reset, same, restart, change,0n);
	}
	public def setStats(co : Int, p : Int, e : Int, t:Double, it:Int, loc:Int, sw:Int, re:Int, sa:Int, rs:Int, ch:Int, 
	        fr : Int) {
	    stats.setStats(co, p, e, t, it, loc, sw, re, sa, rs, ch, fr);
	    accStats(stats);
	}
	
	public def printStats(count:Int):void {
	    stats.print(count);
	}
	public def printAVG(count:Int):void {
	    accStats.printAVG(count);
	}
	public def tryInsertVector(cost:Int, variables:Rail[Int]{self.size==sz}, place:Int) {
	    ep.tryInsertVector(cost, variables, place);
	}
	public def getRemoteData():Maybe[CSPSharedUnit(sz)]=ep.getRemoteData();
	public def worstCost()=ep.worstCost;
	public def clear(){
	    winnerLatch.set(false);
	    ep.clear();
	    }
	public def accStats(c:CSPStats):void {
	    accStats.accStats(c);
	}
	
}
public type ASSolverPermutRW(s:Long)=ASSolverPermutRW{self.sz==s};
