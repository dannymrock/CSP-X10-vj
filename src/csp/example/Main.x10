package csp.example;

import csp.solver.*;
import csp.util.*;
import x10.util.Team;

/** Main
 * 	Main class of the project. 
 * 
 * 	@author Danny Munera
 *  @version 0.1 	9 April, 2013 	-> First Version
 */

import x10.io.File;
import x10.io.FileWriter;
import x10.util.OptionsParser;
import x10.util.Option;
import x10.util.Random;

public class Main {

    public static struct CSPProblem(kind:Int) {
        public def make(size:Long, vectorSize:Long, seed:Long):ModelAS(vectorSize) {
            if (kind==MAGIC_SQUARE_PROBLEM) return new MagicSquareAS(size as Int, vectorSize, seed);
            if (kind==COSTAS_PROBLEM) return new CostasAS(vectorSize, seed);
            if (kind==ALL_INTERVAL_PROBLEM) return new AllIntervalAS(vectorSize, seed, true);
            if (kind==LANGFORD_PROBLEM) return new LangfordAS(size, vectorSize, seed);
            if (kind==STABLE_MARRIAGE_PROBLEM) return new StableMarriageAS(vectorSize, seed);
            return new PartitAS(vectorSize, seed);
        }
        public def make_SM(size:Long, vectorSize:Long, seed:Long, mPrefs:Rail[Rail[Int]],wPrefs:Rail[Rail[Int]])
        	:ModelAS(vectorSize) {
        	return new StableMarriageAS(vectorSize, seed, mPrefs, wPrefs);
        }
    }
 
    public static val UNKNOWN_PROBLEM=0n;
	public static val MAGIC_SQUARE_PROBLEM = 1n;
	public static val COSTAS_PROBLEM = 2n;
	public static val ALL_INTERVAL_PROBLEM = 3n;
	public static val LANGFORD_PROBLEM = 4n;
	public static val PARTIT_PROBLEM = 5n;
	public static val STABLE_MARRIAGE_PROBLEM = 6n;
	
	public static def main(args:Rail[String]):void{
	
		val r = new Random();
		val opts = new OptionsParser(args, new Rail[Option](0L), [
		    Option("p", "", "Problem Selection (magic-square, costas, all-interval or langford)"),
		    Option("s", "", "Size of the problem"),
		    Option("b", "", "Number of benchmark tests"),
		    Option("m", "", "Solver mode distribution 0 for Places \"n\" for Activities (n number of activities). Default 0."),
		    Option("t", "", "Using threads."),
		    Option("c", "", "Communication option: 0 no comm 1 for \"place 0\", 2 for all-to-all and 3 for neighbors"),
		    Option("i", "", "Intra-team Communication Interval (iterations) . Default 0 - no communication."),
		    Option("j", "", "Inter-team Communication Interval (tim miliseconds) . Default 0 - no communication."),
		    Option("n", "", "nodes_per_team parameter. Default 4."),
		    Option("k", "", "poolsize."),
		    Option("d", "", "minimum permisible distance.")
		    ]);
		
		val cspProblem  = opts("-p", "MSP");
		val size        = opts("-s", 10n);
		val testNo      = opts("-b", 10n);
		val solverMode  = opts("-m", 0n);
		val threads     = opts("-t", 0n);
		val comm        = opts("-c", 0n);
		val intraTI     = opts("-i", 0n);
		val interTI     = opts("-j", 0);
		val nodesPTeam  = opts("-n", 1n);
		val poolSize    = opts("-k", 4n);
		val minDistance = opts("-d", 0.3);
		var vectorSize:Long=0;
		//at(Main.param) Main.param().poolSize = poolSize;

		Console.OUT.println("CSP Problem: "+cspProblem+" Size: "+size+"\nNumber of repetitions: "+testNo+
							"\nSolverMode: "+(solverMode==0n ?"Only Places":"Hybrid (Places and Activities)")+
							"\nCommunication strategy: "+comm+"\nIntra-Team Comm. inteval: "+intraTI+" iterations"+
							"\nInter-Team Comm. inteval: "+interTI+" ms"+"\nMinimum permissible distance: "+minDistance+
							"\nPool Size: "+poolSize);
		
		var param : Int = UNKNOWN_PROBLEM;
		//var file : String = "";
				
		if (cspProblem.equalsIgnoreCase("MSP")) {
			Logger.debug(()=>{"Magic Square Problem"});
			param = MAGIC_SQUARE_PROBLEM;
			vectorSize= size*size;
		} else if(cspProblem.equals("CAP")){
			Logger.debug(()=>{"Costas Array Problem"});
			param = COSTAS_PROBLEM;
			vectorSize= size;
		} else if(cspProblem.equals("AIP")){
			Logger.debug(()=>{"All-Interval Array Problem"});
			param = ALL_INTERVAL_PROBLEM;
			vectorSize=size;
		} else if(cspProblem.equals("LNP")){
			Logger.debug(()=>{"Langford Pairing Problem"});
			param = LANGFORD_PROBLEM;
			vectorSize=2*size;
		} else if(cspProblem.equals("NPP")){
			Logger.debug(()=>{"Number Partition Problem"});
			param = PARTIT_PROBLEM;
			vectorSize=size;
		} else if(cspProblem.equals("SMP")){
			Logger.debug(()=>{"Stable Marriage Problem"});
			param = STABLE_MARRIAGE_PROBLEM;
			vectorSize=size;
		} else{
			Console.OUT.println("Error: Type a valid CSP example: MSP, CAP, AIP, LNP, NPP o SMP"); 
			return;
		}
		
		/*
		 *  Creating objects for solver execution
		 */
		val accStats = new CSPStats();
		val vectorSz = vectorSize;
		val solvers:PlaceLocalHandle[ParallelSolverI(vectorSz)];
		if (solverMode == 0n){
			Console.OUT.println("Using multi-walks with "+Place.MAX_PLACES+" Places");
			Console.OUT.println("There are "+Place.MAX_PLACES/nodesPTeam+" teams each one with "+nodesPTeam+" explorer places. "+
					Place.MAX_PLACES+" explorers in total (places)");
			
			solvers = PlaceLocalHandle.make[ParallelSolverI(vectorSz)](PlaceGroup.WORLD, 
					()=>new PlacesMultiWalks(vectorSz, intraTI, interTI, threads, poolSize, nodesPTeam) as ParallelSolverI(vectorSz));
			
		} else{
			Console.OUT.println("Using multi-walks with "+Place.MAX_PLACES+" places and "+nodesPTeam+" activities");
			Console.OUT.println("There are "+Place.MAX_PLACES+" teams each one with "+nodesPTeam+
					" explorer activities. "+Place.MAX_PLACES*nodesPTeam+" explorers in total (places and activities)");
			
			solvers = PlaceLocalHandle.make[ParallelSolverI(vectorSz)](PlaceGroup.WORLD, 
					()=>new HybridMultiWalks(vectorSz, intraTI, interTI, threads, poolSize, nodesPTeam, minDistance) as ParallelSolverI(vectorSz));
		}
		
		Console.OUT.println("|Count| Time (s) |  Iters   | Place |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| Change|  FR |");
		Console.OUT.println("|-----|----------|----------|-------|----------|----------|----------|-------|-----|-------|-----|");
		
		/*
		 *  Execution loop
		 */
		for (var j : Int = 1n; j <= testNo ; j++ ){
			
			//Solve the problem
			//val stats:CSPStats;
			val problem = param;
			val random = new Random();
			val seed = random.nextLong();
			val cspGen:()=>ModelAS(vectorSz);
			if (problem==STABLE_MARRIAGE_PROBLEM){
				val mPref = StableMarriageAS.createPrefs(size as Long, r.nextLong());
				val wPref = StableMarriageAS.createPrefs(size as Long, 	r.nextLong());
				cspGen=():ModelAS(vectorSz)=> CSPProblem(problem).make_SM(size as Long, vectorSz, seed, mPref, wPref);
			}else{
			
				cspGen=():ModelAS(vectorSz)=> CSPProblem(problem).make(size as Long, vectorSz, seed);
			}
			
			
			Logger.debug(()=>" Start broadcatFlat: solvers().solve function ");
			
			
			// PlaceGroup.WORLD.broadcastFlat(()=>{
			// 	solvers().solve(solvers, cspGen);
			// });
			
			finish for (p in Place.places()) at (p) async{
				solvers().solve(solvers, cspGen);
			}
			
			Logger.debug(()=>" End braodcastFlat: solvers().solve function");
			
			Console.OUT.printf("\r");
			solvers().printStats(j);
			solvers().printAVG(j);
			Console.OUT.flush();
			
			Logger.debug(()=>" Start broadcatFlat: solvers().clear function ");
			
			
			finish for (p in Place.places()) at (p) async{	
				solvers().clear();
			}

			Logger.debug(()=>" Start broadcatFlat: solvers().clear function ");
		}
		Console.OUT.printf("\r");
		Console.OUT.println("|-----|----------|----------|-------|----------|----------|----------|-------|-----|-------|-----|");
		solvers().printAVG(testNo);
		//accStats.printAVG(testNo);
		Console.OUT.printf("\n");
		
		return;
	}

	
}
