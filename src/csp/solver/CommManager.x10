package csp.solver;

import csp.util.*;

/**	This class containts all the basic CommManager configuration info, for the
 * 	Adaptive search solver x10 implementation ASSolverPermut
 * 	
 * 	@autor Danny Munera
 * 	
 * Every place has an ASSolverPermutRW. This points to an CommManager.
 * comm is stored in ASSolverPermutRW.
 */
public class CommManager(sz:Long, poolSize:Int) {
	
	
	public static USE_PLACES  = 0n;
	public static USE_ACTIVITIES  = 1n; 
	
	public static NO_COMM = 0n;
	public static ALL_TO_ZERO = 1n;
	public static ALL_TO_ALL = 2n;
	public static ALL_TO_NEIGHBORS = 3n;
	public static TEAM = 4n;
	
	
	/**elite pool
	 */
	val ep = new ElitePool( sz, poolSize ); 
	
	/** Solver use activities or places */
	var solverMode : Int;
	
	/** Number of iterations between each communication activity */
	var intraTI : Int;
	
	/** Number of iterations between each communication activity */
	var interTI : Int;
	
	/** inter-places reset enable */
	//var commOption : Int;
	
	/** probability of change vector if bad cost */
	//val pChange : Int;
	
	var delta : Int=0n;
	
	val nbTeams : Int;
	val myTeamId : Int;
	val random = new x10.util.Random();
	
	/**
	 * The reference to all team members, for communication.
	 */
	val solvers:PlaceLocalHandle[ParallelSolverI(sz)];
	
	def this( sz:Long, solverModeIn : Int , ss: PlaceLocalHandle[ParallelSolverI(sz)], 
	        intraTeamI : Int, interTeamI : Int ,  ps : Int, nT : Int){
		property(sz, ps);
		solvers = ss;
	    solverMode = solverModeIn;
		intraTI = intraTeamI;
		interTI = interTeamI;
		//commOption = cOption;
		nbTeams = nT;
		myTeamId = here.id as Int % nbTeams;

		val m = myTeamId; val s = solverMode;
		Logger.debug(()=>{(s==0n ? ("My team is: " + m):("My team is:"+here.id))});
		
	}
	
	public def setValues(toSet: ASSolverConf{self.sz==this.sz}){
		this.solverMode = toSet.solverMode;
		this.intraTI = toSet.intraTI;
		this.interTI = toSet.interTI;
	}
	/**
	 * 	communicate the vector if Searching thread totalCost is better than worstCost in the pool
	 * 
	 */
	public def communicate(totalCost : Int, variables : Rail[Int]{self.size==sz} ) {
		Logger.debug(()=>" communicate: entering.");
	    val placeid = here.id as Int;
	    if (solverMode == USE_PLACES) {
	    	/************************** Comm Places *******************************/
	    	//Console.OUT.println("Solver Mode USE_PLACES, communication interval= "+commI);
	    	Logger.debug(()=>"CommManager: solver mode -> Places.");
	    	val ss = solvers;
	    	//val variables = csp.variables; 
	    	
	    	if (Place(myTeamId)==here){
	    		Logger.debug(()=>"CommManager: try to insert in local place: "+here);
	    		ep.tryInsertVector( totalCost , variables, placeid);
	    	}else{
	    		Logger.debug(()=>"CommManager: try to insert in remote place: "+Place(myTeamId));
	    		at(Place(myTeamId)) async ss().tryInsertVector( totalCost , variables, placeid);
	    	}
	    	//Debug
	    	// if(here.id  == myGryoupId){ //group heed
	    	//   	Console.OUT.println("I'm "+myGroupId+" head group, here my pool Vectors");
	    	//   	at(arrayRefs(myGroupId))arrayRefs(myGroupId)().printVectors();
	    	// }
	    	/*********************************************************/
	    }else if (solverMode == USE_ACTIVITIES){
	    	Logger.debug(()=>"CommManager: solver mode: Activities.");
	    	Logger.debug(()=>"CommManager: try to insert in local place. ");
	    	ep.tryInsertVector( totalCost , variables, placeid);
	    }else{
	        Console.OUT.println("ERROR: Unknown solver mode");
	    }
	    return;
	}
	
	/**
	 *  get Inter Place Vector.This should be considered to have 
	 * modified csp_ in place, if the return value is 1n (success).
	 * If the return value is -1n (fail), csp_ will not be changed.
	 * 
	 */
	public def getIPVector(csp_ : ModelAS(sz), myCost : Int):Boolean { // csp renamed csp_ to avoid issue with codegen in managed backend
		// if (commOption == NO_COMM) return false;
		Logger.debug(()=> "CommManager: getIPVector: entering.");
		if (solverMode == USE_PLACES) {
			Logger.debug(()=>"CommManager: getIPVector solver mode: Places.");
			val place = Place(myTeamId);
			val ss=solvers;
			val a : Maybe[CSPSharedUnit(sz)];
			if (place == here )
				a = ep.getRemoteData();
			else{
				 a = at(place) ss().getPoolData();
			}
			//if (place.id==0)Console.OUT.println(here+" comm to "+place+" and get "+a().cost);
			if ( a!=null && (myCost + delta) > a().cost ){					 
				csp_.setVariables(a().vector);
				return true; 
			}
			return false;
		}else if (solverMode == USE_ACTIVITIES){
			Logger.debug(()=>"CommManager: getIPVector solver mode: Act.");
			val a = ep.getRemoteData();
			if ( a!=null && (myCost + delta) > a().cost ){
				csp_.setVariables(a().vector);
				return true; 
			}
			return false;
		}else{
			Console.OUT.println("ERROR: Unknown solver mode");
		}
		return false;
	}
	
	public def restartPool(){
		Logger.debug(()=>"CommManager: clear Pool.");
		ep.clear();
	}
	
	
	
}
public type CommManager(s:Long)=CommManager{self.sz==s};
