package csp.solver;

import csp.util.*;
import csp.util.Maybe;
/**	This class containts all the basic configuration info, for the
 * 	Adaptive search solver x10 implementation ASSolverPermut
 * 	
 * 	@autor Danny Munera
 * 	
 * Every place has an ASSolverPermutRW. This points to an ASSolverConf.
 * comm is stored in ASSolverPermutRW.
 */
public class ASSolverConf(sz:Long, poolSize:Int) {
	
	public static USE_ACTIVITIES  = 0n; 
	public static USE_PLACES  = 1n;
	
	public static ALL_TO_ZERO = 0n;
	public static ALL_TO_ALL = 1n;
	public static ALL_TO_NEIGHBORS = 2n;
	public static ALL_TO_GROUP = 3n;
	
	/** Solver use activities or places */
	var solverMode : Int;
	
	/** Number of iterations between each communication activity */
	var intraTI : Int;
	
	/** Number of iterations between each communication activity */
	var interTI : Int;
	
	/** inter-places reset enable */
	var commOption : Int;
	
	/** probability of change vector if bad cost */
	//val pChange : Int;
	
	var delta : Int=0n;
	
	val noGroups : Int;
	val myGroupId : Int;
	val random = new x10.util.Random();
	
	/**
	 * The reference to all team members, for communication.
	 */
	val solvers:PlaceLocalHandle[ASSolverPermutRW(sz)];
	
	def this( sz:Long, solverModeIn : Int , ss: PlaceLocalHandle[ASSolverPermutRW(sz)], // commR : GlobalRef[CommData{self.sz==sz}], 
	        intraTeamI : Int, interTeamI : Int , cOption : Int , ps : Int, nG : Int){
		property(sz, ps);
		solvers = ss;
	    solverMode = solverModeIn;
		intraTI = intraTeamI;
		interTI = interTeamI;
		commOption = cOption;
		//pChange = 10;
		//refCommDist = commD ;
		noGroups = nG;
		myGroupId = here.id as Int % noGroups;
		delta = 0n;
		
		//Console.OUT.println("I'm "+here.id+ " and my group is "+myGroupId);
	}
	
	public def setValues(toSet: ASSolverConf{self.sz==this.sz}){
		this.solverMode = toSet.solverMode;
		this.intraTI = toSet.intraTI;
		this.interTI = toSet.interTI;
	}
	/**
	 * 	communicate the vector if Searching thread totalCost is better than worstCost in the pool
	 *  @return 0 if good cost, -1 if bad cost
	 */
	public def communicate( totalCost : Int, variables : Rail[Int]{self.size==sz} ) : Int {
	    if (commOption==0n) return 0n;
	    if (solverMode == USE_PLACES) {
	        /************************** Comm Places *******************************/
	        //Console.OUT.println("Solver Mode USE_PLACES, communication interval= "+commI);
	        val placeid = here.id as Int;
	        //val variables = csp.variables; 
	        
	        // All-to-one place 0
	        if (commOption == ALL_TO_ZERO){
	            //Console.OUT.println("All-to-one");
	            at(Place(0)) solvers().comm.tryInsertVector( totalCost , variables, placeid); 
	        }else if(commOption == ALL_TO_ALL){
	            // All-to-All	
	            //Console.OUT.println("All-to-all");
	            for (p in Place.places()) 
	                if (here != p) {
	                    at(p) async 
	                    solvers().comm.tryInsertVector( totalCost , variables, placeid);
	                }
	        }else if (commOption == ALL_TO_NEIGHBORS){ 
	            //Neighbors
	            //Console.OUT.println("Neighbors");
	            val placeup = here.id + 1;
	            val placedown = here.id  - 1;
	            if (placeup < Place.MAX_PLACES){
	                at(Place(placeup)) async solvers().comm.tryInsertVector( totalCost , variables, placeid);
	            }
	            if (placedown >= 0L){
	                at(Place(placeup)) async solvers().comm.tryInsertVector( totalCost , variables, placeid);
	            }
	        } /* vj: Not sure what is going on here yet.
	         * else if(commOption == ALL_TO_GROUP){
	         * val r = arrayRefs(myGroupId);
	         * at(r) async r().tryInsertVector( totalCost , variables, placeid);
	         * }*/
	        
	        //Debug
	        // if(here.id  == myGroupId){ //group heed
	        //   	Console.OUT.println("I'm "+myGroupId+" head group, here my pool Vectors");
	        //   	at(arrayRefs(myGroupId))arrayRefs(myGroupId)().printVectors();
	        // }
	        /*********************************************************/
	    }else if (solverMode == USE_ACTIVITIES){
	        //Console.OUT.println("Solver Mode USE_ACTIVITIES, communication interval= "+commI);
	    }else{
	        Console.OUT.println("ERROR: Unknown solver mode");
	    }
	    return 0n;
	}
	
	
	//public def getRandomVector( ) : Rail[Int]{ 
		//val vectorOut = (at(commRef)commRef().getVector());
		//return vectorOut;
	//}
	
	def communicationTarget():Place {
	    val P=Place.MAX_PLACES;
	    if (commOption == ALL_TO_ZERO)  return Place.FIRST_PLACE;
	    if (commOption == ALL_TO_NEIGHBORS) {
	        if (here.id+1 < P) return Place(here.id+1);
	        if (here.id-1 >= 0) return Place(here.id-1);
	    }
	    if (commOption == ALL_TO_ALL) return Place(random.nextLong()%P);
	    // vj: Dont know what groups are about yet.
	    return Place.FIRST_PLACE;
	}
	/**
	 *  get Inter Place Vector.This should be considered to have 
	 * modified csp_ in place, if the return value is 1n (success).
	 * If the return value is -1n (fail), csp_ will not be changed.
	 * 
	 */
	public def getIPVector(csp_ : ModelAS(sz), myCost : Int) : Int{ // csp renamed csp_ to avoid issue with codegen in managed backend
		val place:Place=communicationTarget();
		val a = at(place) solvers().comm.getRemoteData();
		if ( a!=null && (myCost + delta) > a().cost ){					 
		    csp_.setVariables(a().vector);
		    return 1n; 	// success
		}
		return -1n;
	}
	
	
	public def getWorstCostInPool():Int = at(communicationTarget()) solvers().comm.worstCost;
	
	
	public def restartPool():void {
	    at(communicationTarget()) solvers().comm.clear();
	}
	
	
	public def updateVector(variables:Rail[Int], totalcost:Int){
		
		
	}
}
public type ASSolverConf(s:Long)=ASSolverConf{self.sz==s};
