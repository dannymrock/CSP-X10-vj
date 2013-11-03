package csp.example;

import csp.solver.*;
import csp.util.*;

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
	//static val paramObj = new Parameters();
	//static val param = GlobalRef[Parameters](new Parameters());
    public static struct CSPProblem(kind:Int) {
        public def make(size:Long, vectorSize:Long, seed:Long):ModelAS(vectorSize) {
            if (kind==MAGIC_SQUARE_PROBLEM) return new MagicSquareAS(size, vectorSize, seed);
            if (kind==COSTAS_PROBLEM) return new CostasAS(vectorSize, seed);
            if (kind==ALL_INTERVAL_PROBLEM) return new AllIntervalAS(vectorSize, seed, true);
            if (kind==LANGFORD_PROBLEM) return new LangfordAS(size, vectorSize, seed);
            return new PartitAS(vectorSize, seed);
        }
    }
    /*
     * 
     (param == 1n)? new MagicSquareAS(size, vectorSz, seed):
         (cspProblem == 2n) ? new CostasAS(size, seed) : 
             (cspProblem == 3n) ? new AllIntervalAS(size, seed, true) :
                 (cspProblem == 4n) ? new LangfordAS(size, seed) :
                     new PartitAS(sz, seed);
                 */
    public static val UNKNOWN_PROBLEM=0n;
	public static val MAGIC_SQUARE_PROBLEM = 1n;
	public static val COSTAS_PROBLEM = 2n;
	public static val ALL_INTERVAL_PROBLEM = 3n;
	public static val LANGFORD_PROBLEM = 4n;
	public static val PARTIT_PROBLEM = 5n;
	
	public static def main(args:Rail[String]):void{
	
		val opts = new OptionsParser(args, new Rail[Option](0L), [
		    Option("p", "", "Problem Selection (magic-square, costas, all-interval or langford)"),
		    Option("s", "", "Size of the problem"),
		    Option("b", "", "Number of benchmark tests"),
		    Option("m", "", "Solver mode distribution 0 for Places \"n\" for Activities (n number of activities). Default 0."),
		    Option("t", "", "Using threads."),
		    Option("c", "", "Communication option: 0 no comm 1 for \"place 0\", 2 for all-to-all and 3 for neighbors"),
		    Option("i", "", "Intra-team Communication Interval (iterations) . Default 1."),
		    Option("j", "", "Inter-team Communication Interval (iterations) . Default 0."),
		    Option("n", "", "nodes_per_team parameter. Default 4."),
		    Option("k", "", "poolsize."),
		    Option("d", "", "minimum permisible distance.")
		    ]);
		
		val cspProblem  = opts("-p", "magic-square");
		val size        = opts("-s", 10n);
		val testNo      = opts("-b", 10n);
		val solverMode  = opts("-m", 0n);
		val threads     = opts("-t", 0n);
		val comm        = opts("-c", 0n);
		val intraTI     = opts("-i", 1n);
		val interTI     = opts("-j", 0n);
		val nodesPTeam  = opts("-n", 1n);
		val poolSize    = opts("-k", 4n);
		val minDistance = opts("-d", 0.3);
		var vectorSize:Long=0;
		//at(Main.param) Main.param().poolSize = poolSize;

		Console.OUT.println("CSP Problem: "+cspProblem+" Size: "+size+"\nNumber of repetitions: "+testNo+
							"\nSolverMode: "+solverMode+"\nCommunication strategy: "+comm+
				            "\nIntra-Team Comm. inteval: "+intraTI+"\nInter-Team Comm. inteval: "+interTI+
				            "\nMinimum permissible distance: "+minDistance+
							"\nPool Size: "+poolSize);
		
		var param : Int = UNKNOWN_PROBLEM;
		//var file : String = "";
				
		if (cspProblem.equalsIgnoreCase("magic-square")) {
			//Console.OUT.println("Magic Square Problem");
			param = MAGIC_SQUARE_PROBLEM;
			vectorSize= size*size;
		} else if(cspProblem.equals("costas")){
			//Console.OUT.println("Costas Array Problem");
			param = COSTAS_PROBLEM;
			vectorSize= size;
		} else if(cspProblem.equals("all-interval")){
			//Console.OUT.println("All-Interval Array Problem");
			param = ALL_INTERVAL_PROBLEM;
			vectorSize=size;
		} else if(cspProblem.equals("langford")){
			//Console.OUT.println("Langford Pairing Problem");
			param = LANGFORD_PROBLEM;
			vectorSize=2*size;
		} else if(cspProblem.equals("partit")){
			//Console.OUT.println("Number Partition Problem");
			param = PARTIT_PROBLEM;
			vectorSize=size;
		} else{
			Console.OUT.println("Error: Type a valid CSP example: magic-square"); 
			return;
		}
		
		/*
		 *  Creating objects for solver execution
		 */
		var timeStart : Long;
		var cost : Int;
		var timeEnd : Long;
		var sumTimes: Long = 0L;
		val accStats = new CSPStats();
		
		// communication interval = 10
		val vectorSz=vectorSize;
		val solvers = PlaceLocalHandle.make[ASSolverPermutRW(vectorSz)](PlaceGroup.WORLD, 
		        ()=>new ASSolverPermutRW(vectorSz, intraTI, comm, threads, poolSize, nodesPTeam) as ASSolverPermutRW(vectorSz)); 
	//	val solverT = new CooperativeMW(intraTI, interTI, threads, poolSize, nodesPTeam, minDistance);

		if (solverMode == 0n){
			Console.OUT.println("Using multi-walks with "+Place.MAX_PLACES+" Places");
			Console.OUT.println("There are "+Place.MAX_PLACES+" teams each one with "+nodesPTeam+" explorer places. "+
					Place.MAX_PLACES*nodesPTeam+" explorers in total (places)");
		} else{
			Console.OUT.println("Using multi-walks with "+Place.MAX_PLACES+" places and "+nodesPTeam+" activities");
			Console.OUT.println("There are "+Place.MAX_PLACES+" teams each one with "+nodesPTeam+
					" explorer activities. "+Place.MAX_PLACES*nodesPTeam+" explorers in total (places and activities)");
		}
		
		Console.OUT.println("|Count| Time (s) |  Iters   | Place |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| Change|  FR |");
		Console.OUT.println("|-----|----------|----------|-------|----------|----------|----------|-------|-----|-------|-----|");
		
		/*
		 *  Execution loop
		 */
		for (var j : Int = 1n; j <= testNo ; j++ ){
			
			//Solve the problem
			//val stats:CSPStats;
			val problem=param;
			val random = new Random();
			val seed = random.nextLong();
			val cspGen=():ModelAS(vectorSz)=> CSPProblem(problem).make(size as Long, vectorSz, seed);
			
			//if (solverMode == 0n){ 
				PlaceGroup.WORLD.broadcastFlat(()=>{solvers().solve(solvers, cspGen);});
			//}else{
		//		stats = solverT.solve(size,param);
			//}
			//accStats.accStats(stats);
			Console.OUT.printf("\r");
			solvers().printStats(j);
			accStats.printAVG(j);
			Console.OUT.flush();
			
			//clean solver
			
		}
		Console.OUT.printf("\r");
		Console.OUT.println("|-----|----------|----------|-------|----------|----------|----------|-------|-----|-------|-----|");
		accStats.printAVG(testNo);
		Console.OUT.printf("\n");
		
		return;
	}

	
}
