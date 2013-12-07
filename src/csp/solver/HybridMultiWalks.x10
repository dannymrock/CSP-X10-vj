package csp.solver;
import csp.util.*;
import x10.util.concurrent.AtomicBoolean;
import x10.compiler.Inline;

public class HybridMultiWalks (sz:Long,poolSize:Int) implements ParallelSolverI {
	property sz()=sz;
	var csp_:Rail[ModelAS(sz)];
	var solver:Rail[ASSolverPermut(sz)];
	
	//Hybrid approach
	val nbExplorerPT : Int;
	val nTeams : Int;
	var stats:CSPStats = null;
	var accStats:CSPStats = null;
	/** Comunication Variables*/
	var commM : CommManager(sz);
	
	//???
	val updateI : Int;
	val commOption : Int;
	
	val winnerLatch = new AtomicBoolean(false);
	
	//var time:Long;
	
	/**
	 * 	Constructor of the class
	 */
	public def this(vectorSize:Long, upI : Int, commOpt : Int , thread : Int , ps : Int, npT : Int ){
		property(vectorSize,ps);
		accStats = new CSPStats();
		stats = new CSPStats();
		updateI = upI; 
		commOption = commOpt;
		// thEnable = thread;
		 
		nbExplorerPT = npT; // will be a parameter 
		nTeams = Place.MAX_PLACES as Int / nbExplorerPT;
		// Arrays with the problems and solver to be dsitributed at each activity
		csp_ = new Rail[ModelAS(sz)](nbExplorerPT);
		solver = new Rail[ASSolverPermut(sz)](nbExplorerPT);
	}
	
	
	public def solve(st:PlaceLocalHandle[ParallelSolverI(sz)], cspGen:()=>ModelAS(sz) ):void { 
		val solvers= st;
		assert solvers() == this : "Whoa, basic plumbing problem -- I am not part of solvers!";
		val size = sz as Int;
		var extTime : Long = -System.nanoTime();
		
		var nsize:Int = size;
		commM = new CommManager(sz, 1n, solvers, updateI,0n, commOption, poolSize, nTeams );
		val ss = st() as ParallelSolverI(sz);

		Logger.debug(()=>{"  HybridMultiWalks: spawning explorer activities "});
		
		finish for (exID in csp_.range()) async {
		
			Logger.debug(()=>{"  HybridMultiWalks: explorer activity "+exID+" ready"});
			
			csp_(exID) = cspGen(); // use the supplied generator to generate the problem
			solver(exID) = new ASSolverPermut(sz, nsize, here.id, ss);
			
			var time : Long = -System.nanoTime();
			val cost = solver(exID).solve(csp_(exID));
			
			
			if (cost == 0n){ 
				// An explorer has found a solution! Huzzah! 
				// Light the candles! Kill the blighters!

				Logger.debug(()=>{"  HybridMultiWalks: explorer activity "+exID+" has found a solution"});
				val home = here.id;
				val winner:Boolean;
				finish winner = at(Place.FIRST_PLACE) solvers().announceWinner(solvers, home);
				
				//winPlace = here;
				//bcost = cost;
				
				if (winner) {
					time += System.nanoTime();
					setStats(solvers, exID, time);
					//Utils.show("Solution is " + (csp_.verified()? "ok" : "WRONG") , csp_.variables);
					Console.OUT.print("Solution is " + (csp_(exID).verified()? "ok " : "WRONG "));
					//csp_(exID).displaySolution();
				}
			}	
		}
	}
	
	public def announceWinner(ss:PlaceLocalHandle[ParallelSolverI(sz)], p:Long):Boolean {
		val result = winnerLatch.compareAndSet(false, true);
		Logger.debug(()=> "  HybridMultiWalks: announceWinner result=" + result + " for " + p + " this=" + this );
		if (result) {
			for (k in Place.places()) 
				//if (p != k.id) // I need to kill the remaining explorers in the same place (hybrid model) 
					at(k) async ss().kill();
		}
		return result;
	}
	
	public def kill() {
		for(i in solver.range())
			solver(i).kill=true;
	}
	
	public def clear():void {
		winnerLatch.set(false);
		commM.restartPool();
	}
	
	/**
	 * Called by winning place to set the stats at place zero so they
	 * can be printed out.
	 */
	def setStats(ss:PlaceLocalHandle[ParallelSolverI(sz)], exID:Long, t:Long ){
		val winPlace = here.id;
		val time = t/1e9;
		val iters = solver(exID).nbIterTot;
		val locmin = solver(exID).nbLocalMinTot;
		val swaps = solver(exID).nbSwapTot;
		val reset = solver(exID).nbResetTot;
		val same = solver(exID).nbSameVarTot;
		val restart = solver(exID).nbRestart;
		val change = solver(exID).nbChangeV;
		
		finish at (Place.FIRST_PLACE) async
		ss().setStats(0n, winPlace as Int, exID as Int, time, iters, locmin, swaps, reset, same, restart, change,0n);
	}
	
	public def printStats(count:Int):void {
		stats.print(count);
	}
	
	public def printAVG( count:Int):void {
		accStats.printAVG(count);
	}
	
	public def setStats(co:Int, p:Int, e:Int, t: Double, it:Int, loc:Int, sw:Int, re:Int, sa:Int, rs:Int, ch:Int, fr:Int):void {
		stats.setStats(co, p, e, t, it, loc, sw, re, sa, rs, ch, fr);
		accStats(stats);
	}
	
	public def accStats(st:CSPStats):void {
		accStats.accStats(st);
	}
	
	
	// Communication functions
	
	// public def communicate(var totalCost:Int, var variables:Rail[Int]{self.size==sz}) {
	// 	ep.tryInsertVector(totalCost, variables, here.id  as Int);
	// }
	public def communicate(totalCost:Int, variables:Rail[Int]{self.size==sz}){
		commM.communicate(totalCost, variables);
	}
	@Inline public def getIPVector(csp_:ModelAS(sz), myCost:Int):Boolean 
	= commM.getIPVector(csp_, myCost);
			
	public def getRemoteData():Maybe[CSPSharedUnit(sz)] {
		// TODO: auto-generated method stub
		return null;
	}
			
	@Inline public def intraTI():Int = commM.intraTI;
	
	public def tryInsertVector(var cost:Int, var variables:Rail[Int]{self.size==sz}, var place:x10.
			lang.Int):void {
		// TODO: auto-generated method stub
	}
						
}
public type HybridMultiWalks(s:Long)=HybridMultiWalks{self.sz==s};
