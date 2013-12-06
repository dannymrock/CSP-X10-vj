package csp.example;
import csp.solver.ModelAS;
import x10.array.Array_2;
import csp.util.Utils;

public class StableMarriageAS extends ModelAS {
	/** number of blocking pairs
	 */
	var nbBlockingPairs:Int;
	
	val nbBPperMan : Rail[Int];
	
	/**
	 * Preference table for stable marriage problem
	 */
	// //Paper data
	// val menPref = [[4n,6n,0n,1n, 5n,7n,3n,2n],
	//               [1n,2n,6n,4n,3n,0n,7n,5n],
	//               [7n,4n,0n,3n,5n,1n,2n,6n],
	//               [2n,1n,6n,3n,0n,5n,7n,4n],
	//               [6n,1n,4n,0n,2n,5n,7n,3n],
	//               [0n,5n,6n,4n,7n,3n,1n,2n],
	//               [1n,4n,6n,5n,2n,3n,7n,0n],
	//               [2n,7n,3n,4n,6n,1n,5n,0n]];
	// val womenPref = [[4n,2n,6n,5n,0n,1n,7n,3n],
	//              [7n,5n,2n,4n,6n,1n,0n,3n],
	//              [0n,4n,5n,1n,3n,7n,6n,2n],
	//              [7n,6n,2n,1n,3n,0n,4n,5n],
	//              [5n,3n,6n,2n,7n,0n,1n,4n],
	//              [1n,7n,4n,3n,5n,2n,6n,0n],
	//              [6n,4n,1n,0n,7n,5n,3n,2n],
	//              [6n,3n,0n,4n,1n,2n,5n,7n]];

	// val a : Rail[Rail[Long]{self!=null}] = [[1,2,3],[4,5, ]];
	val menPref : Rail[Rail[Int]];
	
	val womenPref : Rail[Rail[Int]];
	
	public def this (lengthProblem : Long , seed : Long ):StableMarriageAS(lengthProblem){
		super(lengthProblem, seed );
		nbBlockingPairs = 0n;
		nbBPperMan = new Rail[Int](length,0n);
		initParameters();
		
		menPref = new Rail[Rail[Int]](length); 
		var i:Long=0;
		for( i=0; i<length; i++)
			menPref(i) = new Rail[Int](r.randomPermut(length as Int,0n));
		
		womenPref = new Rail[Rail[Int]](length); 
		for( i=0; i<length; i++)
			womenPref(i) = new Rail[Int](r.randomPermut(length as Int,0n));
				
		//printPreferencesTables();
	}
	
	
	/** initParameters
	 *  It is necessary to fine tune the parameters for this problem
	 */
	private def initParameters(){
		
		solverParams.probSelectLocMin = 66n;
		solverParams.freezeLocMin = 1n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit = 5n;
		solverParams.resetPercent = 25n;
		solverParams.restartLimit = 100n;
		solverParams.restartMax = 10n;
		solverParams.baseValue = 0n;
		solverParams.exhaustive = false;
		solverParams.firstBest = false;
		// solverParams.probSelectLocMin = 6n;
		// solverParams.freezeLocMin = 5n;
		// solverParams.freezeSwap = 0n;
		// solverParams.resetLimit = length/2n;
		// solverParams.resetPercent = 10n;
		// solverParams.restartLimit = 1000n;
		// solverParams.restartMax = 10n;
		// solverParams.baseValue = 0n;
		// solverParams.exhaustive = true;
		// solverParams.firstBest = true;
	}
	
	/**
	 * 	Computes the cost of the solution
	 * 	count the number of blocking pairs for the current Match
	 * 	@return cost
	 */
	public def costOfSolution( shouldBeRecorded : Int ) : Int {
		nbBlockingPairs = 0n;
		nbBPperMan.clear();
		var m:Int;
		//pw : M-partner of w
		var pw:Int;
		//pm : M-partner of m
		var pm:Int;
		//For each man
		for(m=0n; m<length;m++){
			pm = variables(m);
			// Obtain prefereces of this man
			val manPref = menPref(m); 
			// for each w such that m prefers w to pm
			for(w in manPref){
				if(w==pm) break;
				val womanPref = womenPref(w);
				pw = find(variables,w);
				var j:Int=0n;
				for(j=0n; j<pw; j++){
					if(womanPref(j)==m){
						nbBlockingPairs++;
						nbBPperMan(m)++;
					}
				}
			}
		}
		return nbBlockingPairs;
	}
	
	/** 
	 * 	costOnVariable( i : Int ) : Int
	 * 	This function computes the cost of individual variable i
	 * 	@param i This is the variable that we want to know the cost
	 *  @return Int value with the cost of this variable
	 */
	public def costOnVariable( i : Int ) : Int{		
		return nbBPperMan(i);
	}
	
	/**
	 *  executedSwap( i1 : Int, i2 : Int)
	 *  This function updates the values of the object data structures for the problem due to the 
	 *  completion of a swap between two variables
	 *  @param i1 First variable already swapped
	 *  @param i2 Second variable already swapped
	 */
	public def executedSwap( i1 : Int, i2 : Int) {
		this.costOfSolution(1n);
	}
	
	
	public def find(vec:Rail[Int],value:Int):Int{
		var i :Int=0n;
		for(i=0n;i<vec.size ;i++)
			if (vec(i)==value) return i;
		Console.OUT.println("Error: value not found!!!");
		return -1n;
	}
	
	public def displaySolution(){
		var i:Int=0n;
		Console.OUT.println("\nMatching  m -> w:");
		for (i=0n; i<length;i++){
			Console.OUT.print(i+" -> "+variables(i)+"\t");
		}
		Console.OUT.println(" ");
	}
	
	/**
	 *  costIfSwap(current_cost : Int, i1 : Int, i2 : Int) : Int
	 *  This function computes the cost of the problem if there is a swap between variable
	 *  i1 and i2.
	 * 	@param current_cost The current cost of the problem
	 *  @param i1 first variable to swap
	 *  @param i2 second variable to swap
	 *  @return cost of the problem if the swap is done
	 */
	public def costIfSwap(current_cost:Int,var i1:Int, var i2:Int):Int
	{
		var x : Int;
		var r : Int;

		x = variables(i1);
		variables(i1) = variables(i2);
		variables(i2) = x;

		r = costOfSolution(0n);

		variables(i2) = variables(i1);
		variables(i1) = x;

		return r;
	}
	
	/**
	 *  CHECK_SOLUTION
	 * 
	 *  Checks if the solution is valid.
	 */

	public  def verified():Boolean {
		var m:Int;
		//pw : M-partner of w
		var pw:Int;
		//pm : M-partner of m
		var pm:Int;
		//For each man
		for(m=0n; m<length;m++){
			pm = variables(m);
			// Obtain prefereces of this man
			val manPref = menPref(m); 
			// for each w such that m prefers w to pm
			for(w in manPref){
				if(w==pm) break;
				val womanPref = womenPref(w);
				pw = find(variables,w);
				var j:Int=0n;
				for(j=0n; j<pw; j++){
					if(womanPref(j) == m){
						Console.OUT.println("Not stable marriage blocking pair m:"+m+", w:"+w);
						return false;
					}
						
				}
				
			}
		}
		return true;
	}
	
	
	private def printPreferencesTables(){
		Console.OUT.println("\nMen Preferences");
		var i:Int = 0n;
		for (i=0n; i<length; i++){
			Console.OUT.print(i+": ");
			for(j in menPref(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
		Console.OUT.println("Women Preferences");
		for (i=0n; i<length; i++){
			Console.OUT.print(i+": ");
			for(j in womenPref(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
	}
}

public type StableMarriageAS(s:Long)=StableMarriageAS{self.sz==s};
	