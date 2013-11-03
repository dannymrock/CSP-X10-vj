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

public class ASSolverPermutRW(sz:Long,poolSize:Int){  
	var csp:ModelAS(sz);
	var solver:ASSolverPermut(sz);
	var time:Long;
	var winPlace : Place;
	
	val updateI : Int;
	val commOption : Int;
	
	var bcost : Int;
	public val stats  = new CSPStats();
	val accStats = new CSPStats();
	
	/** Comunication Variables*/
	val comm = new CommData( sz, poolSize ); 
	val thEnable : Int; 
	
	var conf: ASSolverConf(sz);
	//Hybrid approach
	val nbExplorerPT : Int;
	val nTeams : Int;
	
	//var solvers: PlaceLocalHandle[ASSolverPermutRW(sz)];
	/**
	 * 	Constructor of the class
	 */
	public def this(vectorSize:Long, upI : Int, commOpt : Int , thread : Int , ps : Int, npT : Int ){
	    property(vectorSize,ps);
		
		updateI = upI; 
		commOption = commOpt;
		thEnable = thread;
		
		nbExplorerPT = npT; // will be a parameter 
		nTeams = Place.MAX_PLACES as Int / nbExplorerPT ;
	}
	
	/** 
	 * 	Solve the csp problem with MAX_PLACES instance of AS solver
	 * 	The first one that reach a valid solution send a kill to the others
	 * 	to finish the process.
	 * 	@param size size of the csp problem
	 * 	@param cspProblem code with the problem to be solved (1 for Magic Square Problems, other number for Queens Problem)
	 * 	@return cost of the solution
	 */
	public def solve(st:PlaceLocalHandle[ASSolverPermutRW(sz)], cspGen:()=>ModelAS(sz) ) : CSPStats{ 
	    val solvers=st;
	    assert solvers() == this : "Whoa, basic plumbing problem -- I am not part of solvers!";
		val size = sz as Int;
		var extTime : Long = -System.nanoTime();
		val random = new Random();
		
		val seed = random.nextLong();
		//val seed1 = 1234L;
		
		var nsize:Int = size;

		csp = cspGen();
		
	
		conf = new ASSolverConf(sz, 1n /*ASSolverConf.USE_PLACES*/, solvers, updateI,0n, commOption, poolSize, nTeams );
		solver = new ASSolverPermut(sz, nsize, seed, solvers());
		
		
		var cost:Int = x10.lang.Int.MAX_VALUE;
		
		/***/
		
		//timeDist(here.id) = -System.nanoTime();
		cost = solver.solve(csp);
		//	timeDist(here.id) += System.nanoTime();
		
		if (cost == 0n){
		    for (k in Place.places()) 
		        if (here.id != k.id) 
		            at(k) async solvers().solver.kill = true;
		    winPlace = here;
		    bcost = cost;
		    setStats(solvers);
		}
		extTime += System.nanoTime();
		stats.time = extTime/1e9;
		this.clear();
		return stats; 
	}
	
	def setStats(ss:PlaceLocalHandle[ASSolverPermutRW(sz)]  ){
		val winPlace = here.id;
		//val time = (timeDist(winPlace))/1e9;
		val iters = solver.nbIterTot;
		val locmin = solver.nbLocalMinTot;
		val swaps = solver.nbSwapTot;
		val reset = solver.nbResetTot;
		val same = solver.nbSameVarTot;
		val restart = solver.nbRestart;
		val change = solver.nbChangeV;
		
		
		at (Place.FIRST_PLACE) ss().stats.setStats(0n, winPlace as Int, 0n, 0n, iters, locmin, swaps, reset, same, restart, change,0n);
		//val winstats = new CSPStats
	}
	public def printStats(count:Int):void {
	    stats.print(count);
	}
	
	public def clear(){comm.clear();}
}
public type ASSolverPermutRW(s:Long)=ASSolverPermutRW{self.sz==s};
