package csp.solver;
import csp.util.*;
import x10.util.concurrent.AtomicBoolean;
import x10.compiler.Inline;
import x10.util.Random;

public class HybridMultiWalks (sz:Long,poolSize:Int) implements ParallelSolverI {
    property sz()=sz;
    val csp_:Rail[ModelAS(sz)];
    val solver:Rail[ASSolverPermut(sz)];
	
    //Hybrid approach
    val nbExplorerPT : Int;
    val nTeams : Int;
    
    val stats = new CSPStats();
    val accStats = new CSPStats();
    
    /** Comunication Variables*/
    var commM : CommManager(sz);
	
	//???
    val updateI : Int;
    //val commOption : Int;
    val interTeamInterval:Long;
	
    val winnerLatch = new AtomicBoolean(false);
	
    //var time:Long;
	val minDistance : Double;
	
	var interTeamDone:Boolean=false;
	
	/**
	 * 	Constructor of the class
	 */
	public def this(vectorSize:Long, upI : Int, interTI : Long , thread : Int , ps : Int, npT : Int, minD:Double ){
		property(vectorSize,ps);
		updateI = upI; 
		//commOption = commOpt;
		// thEnable = thread;
		interTeamInterval = interTI;
		 
		nbExplorerPT = npT; // will be a parameter 
		nTeams = Place.MAX_PLACES as Int / nbExplorerPT;
		// Arrays with the problems and solver to be dsitributed at each activity
		csp_ = new Rail[ModelAS(sz)](nbExplorerPT);
		solver = new Rail[ASSolverPermut(sz)](nbExplorerPT);
		minDistance = minD;
	}
	
	
    public def solve(st:PlaceLocalHandle[ParallelSolverI(sz)], cspGen:()=>ModelAS(sz) ):void { 
	    val solvers= st;
	    assert solvers() == this : "Whoa, basic plumbing problem -- I am not part of solvers!";
	    val size = sz as Int;
	    var extTime : Long = -System.nanoTime();
		
	    var nsize:Int = size;
	    commM = new CommManager(sz, 1n, solvers, updateI,0n, poolSize, nTeams );
	    val ss = st() as ParallelSolverI(sz);

	    Logger.debug(()=>{"  HybridMultiWalks: spawning explorer activities "});
		
		finish{
			if (interTeamInterval!=0) async interTeamActivity(solvers);
                
			for (exID in csp_.range()) async {
				
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
	}
	
	public def announceWinner(ss:PlaceLocalHandle[ParallelSolverI(sz)], p:Long):Boolean {
		val result = winnerLatch.compareAndSet(false, true);
		Logger.debug(()=> "  HybridMultiWalks: announceWinner result=" + result + " for " + p + " this=" + this );
		if (result) {
			for (k in Place.places()) 
				//if (p != k.id) // I need to kill the remaining explorers in the same place (hybrid model) 
				at(k) async {ss().kill();
			}
		}
		return result;
	}
	
	
	public def kill() {
		interTeamDone = true;
		for (s in solver) s.kill=true;
	}
	
    public def clear():void {
	    winnerLatch.set(false);
	    interTeamDone = false;
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
	        ss().setStats(0n, winPlace as Int, exID as Int, time, iters, locmin, 
	                      swaps, reset, same, restart, change,0n);
	}
	
    public def printStats(count:Int):void {
	    stats.print(count);
    }
	
    public def printAVG( count:Int):void {
	    accStats.printAVG(count);
    }
	
    public def setStats(co:Int, p:Int, e:Int, t: Double, it:Int, loc:Int, sw:Int, re:Int, 
			sa:Int, rs:Int, ch:Int, fr:Int):void {
	    stats.setStats(co, p, e, t, it, loc, sw, re, sa, rs, ch, fr);
	    accStats(stats);
    }
	
    public def accStats(st:CSPStats):void {
	    accStats.accStats(st);
    }
	
	
    // Communication functions
	
    public def interTeamActivity(st:PlaceLocalHandle[ParallelSolverI(sz)]){
    	while (! interTeamDone) {
    		val r = new Random();

    		//Runtime.worker().sleep(SLEEP_INTERVAL);
    		if (!System.sleep(interTeamInterval)){ 
    			Logger.info(()=>"interTeamActivity error: cannot execute sleep");
    			break;
    		}
    		// woken up
    		Logger.debug(()=>{" interTeamActivity - run : woken up (every "+interTeamInterval+" ms)"});
    		if(r.nextInt(100n) < 25n) interTeamComm(st);			
    		// get current conf from here (randomly choose one explorer)
    		// check the distance against a anotherandom place.
    		// if distance is lower than minimum distans permited, execute corrective action.
    	}
    }
    
    // public def communicate(var totalCost:Int, var variables:Rail[Int]{self.size==sz}) {
    // 	ep.tryInsertVector(totalCost, variables, here.id  as Int);
    // }
    public def communicate(totalCost:Int, variables:Rail[Int]{self.size==sz}){
	    commM.communicate(totalCost, variables);
    }
    @Inline public def getIPVector(csp_:ModelAS(sz), myCost:Int):Boolean 
								 = commM.getIPVector(csp_, myCost);
			
	public def getPoolData():Maybe[CSPSharedUnit(sz)] {
		// TODO: auto-generated method stub
		return null;
	}
			
    @Inline public def intraTI():Int = commM.intraTI;
	
	public def tryInsertVector(var cost:Int, var variables:Rail[Int]{self.size==sz}, var place:x10.
			lang.Int):void {
		// TODO: auto-generated method stub
	}
			
	public def getCurrentData():Maybe[CSPSharedUnit(sz)]{
		val r = new Random();
		val ex = r.nextLong(csp_.size);
        var sol:CSPSharedUnit(sz) = new CSPSharedUnit(sz,solver(ex).total_cost,csp_(ex).variables, 
             here.id as Int);
		return new Maybe(sol);
	}
	
	
	//val SLEEP_INTERVAL= 1000;
	
	public def interTeamComm(ss:PlaceLocalHandle[ParallelSolverI(sz)]){
		Logger.debug(()=>{"MW - interTeamComm : entering..."});
		
		val r = new Random();
		//Compare against a random team                
		var remote :Long = r.nextLong(Place.MAX_PLACES);
		while (here.id == remote){
			remote = r.nextLong(Place.MAX_PLACES);
		}
		
		// get current configuration and cost from local and  remote Team
		val localConf = getCurrentData();
		val remoteConf = at(Place(remote)) ss().getCurrentData();
		
		//compute distance between Teams
		val dis = distance(localConf().vector, remoteConf().vector);
		val rem=remote;
		Logger.debug(()=>{"MW - interTeamComm : distance between "+here.id+" and "+rem+" is= "+dis});
		
		if (dis < minDistance){
			//solverP take corrective action
			for(i in solver.range()){
				Logger.debug(()=>"MW - interTeamComm : force Restart");
				solver(i).forceRestart();
			}
		}
	}
	
	def distance(conf1 : Valuation(sz), conf2 : Valuation(sz)) : Double {
		var count : Int = 0n;
		for (i in 0n..(sz as Int - 1n)){
			//Logger.debug("comparing: "+conf1(i)+" - "+conf2(i));
			if(conf1(i) == conf2(i)) count++; 
		}
		val dis = 1.0 - ( count as Double / sz );
		return dis;
	} 
}
public type HybridMultiWalks(s:Long)=HybridMultiWalks{self.sz==s};
