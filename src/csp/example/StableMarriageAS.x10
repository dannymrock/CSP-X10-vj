package csp.example;
import csp.solver.ModelAS;

public class StableMarriageAS extends ModelAS {
	/** number of blocking pairs
	 */
	var nbp:Int;
	
	public def this (lengthProblem : Long , seed : Long ):StableMarriageAS(lengthProblem){
		super(lengthProblem, seed );
		nbp = 0n;
		initParameters();
	}
	
	
	/** initParameters
	 *  It is necessary to fine tune the parameters for this problem
	 */
	private def initParameters(){
		solverParams.probSelectLocMin = 66n;
		solverParams.freezeLocMin = 1n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit = 1n;
		solverParams.resetPercent = 25n;
		solverParams.restartLimit = 10000000n;
		solverParams.restartMax = 0n;
		solverParams.baseValue = 0n;
		solverParams.exhaustive = true;
		solverParams.firstBest = false;
	}
	
	/**
	 * 	Computes the cost of the solution
	 * 	count the number of blocking pairs for the current Match
	 * 	@return cost
	 */
	public def cost() : Int 
	{
		var m:Int;
		for(m=0n; m<length;m++){
			var w:Int = variables(m);
			//current pair m w
			
		}
		
		return 0n;
	}
	
}

public type StableMarriageAS(s:Long)=StableMarriageAS{self.sz==s};
	