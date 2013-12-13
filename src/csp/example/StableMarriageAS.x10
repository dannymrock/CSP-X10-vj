package csp.example;
import csp.solver.ModelAS;
import x10.array.Array_2;
import csp.util.*;

public class StableMarriageAS extends ModelAS {
	/** number of blocking pairs
	 */
	var nbBlockingPairs:Int;
	
	val nbBPperMan : Rail[Int];
	val variablesW : Rail[Int];
    val err : Rail[Int];
	
	val menPref : Rail[Rail[Int]];
	
	val womenPref : Rail[Rail[Int]];
	
	public def this (lengthProblem : Long , seed : Long ):StableMarriageAS(lengthProblem){
		super(lengthProblem, seed );
		nbBlockingPairs = 0n;
		nbBPperMan = new Rail[Int](length,0n);
		variablesW = new Rail[Int](length,0n);
        err = new Rail[Int](length,0n);
		initParameters();
        val rr=r, l=length as Int;
        menPref = new Rail[Rail[Int]](length, (Long) => new Rail[Int](rr.randomPermut(l, 0n)));
        womenPref = new Rail[Rail[Int]](length, (Long) => new Rail[Int](rr.randomPermut(l, 0n)));

		//printPreferencesTables();
	}
	
	public def this (lengthProblem : Long , seed : Long, mPrefs:Rail[Rail[Int]],wPrefs:Rail[Rail[Int]]):
		StableMarriageAS(lengthProblem){
		super(lengthProblem, seed );
		nbBlockingPairs = 0n;
		nbBPperMan = new Rail[Int](length,0n);
		variablesW = new Rail[Int](length,0n);
		err = new Rail[Int](length,0n);
		initParameters();
		val rr=r, l=length as Int;
		menPref = mPrefs;
		womenPref = wPrefs;
		//Logger.info(()=>{"Preferences"});
		//printPreferencesTables();
	}

	// public def setPrefs(mPrefs:Rail[Rail[Int]],wPrefs:Rail[Rail[Int]]){
	// 	menPref = mPrefs;
	// 	womenPref = wPrefs;		
	// }
	
	
	
	
	/** initParameters
	 *  It is necessary to fine tune the parameters for this problem
	 */
	private def initParameters(){
		
		solverParams.probSelectLocMin =  200n; // try ~80%  
		solverParams.freezeLocMin = 1n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit =2n; //may be 1 is better for size 30 try()
		solverParams.resetPercent = 4n;
		solverParams.restartLimit = 1000000000n;
		solverParams.restartMax = 0n;
		solverParams.baseValue = 0n;
		solverParams.exhaustive = false;
		solverParams.firstBest = false;
		
	    solverParams.probChangeVector = 50n;
	}
	
	/**
	 * 	Computes the cost of the solution
	 * 	count the number of blocking pairs for the current Match
	 * 	@return cost
	 */
     public def costOfSolution( shouldBeRecorded : Int ) : Int {	
         var w:Int, m1:Int;
         var i:Int, j:Int;
         var pm:Int, pw:Int; //w_of_m, m_of_w;
         var r:Int = 0n;
       
         err.clear();
         for (m in variables.range())
             variablesW(variables(m)) = m as Int;
         
         for (m in variables.range()){
        	 pm = variables(m);
        	 loop: for(i = 0n; (w = menPref(m)(i)) != pm; i++){
        		 pw = variablesW(w);
        		 for(j = 0n; (m1 = womenPref(w)(j)) != pw; j++){
        			 if (m1 == m as int){
        				 var endJ:Int;
        				 for(endJ = j; womenPref(w)(endJ) != pw; endJ++)
        				 { }
        				 val z = endJ - j;
        				 if (z > err(m)) {
        					 r += j as Int + 1n;
        					 err(m) = z; //z*z;
        					 // printf("j=%d  i=%d  m=%d\n", j, i, m);
        				 }
        				 //goto undominated_only;
        				 //break;
        				 break loop;
        			 }
        		 }
        	 }
        	 //undominated_only:
         }
         return r;	
     }
	
	/** 
	 * 	costOnVariable( i : Int ) : Int
	 * 	This function computes the cost of individual variable i
	 * 	@param i This is the variable that we want to know the cost
	 *  @return Int value with the cost of this variable
	 */
	public def costOnVariable( i : Int ) : Int{		
		return err(i);
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
	
	public def displaySolution(){
		var i:Int=0n;
		Console.OUT.println("\nMatching  m->w:");
		for (i=0n; i<length;i++){
			Console.OUT.print(i+"->"+variables(i)+"   ");
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
		var w:Int, m1:Int;
		var i:Int, j:Int;
		var pm:Int, pw:Int; //w_of_m, m_of_w;
		var r:Int = 0n;

		variablesW.clear();
		for (m in variables.range())
			variablesW(variables(m)) = m as Int;
		
		for (m in variables.range()){
			pm = variables(m);
			for(i = 0n; (w = menPref(m)(i)) != pm; i++){
				pw = variablesW(w);
				for(j = 0n; (m1 = womenPref(w)(j)) != pw; j++){
					if (m1 == m as Int){
						Console.OUT.println("Not stable marriage, blocking pair m: "+m+", w: "+w);
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
	
	static def createPrefs(l:Long,seed:Long):Rail[Rail[Int]]{
		val r = new RandomTools(seed);
		val prefs = new Rail[Rail[Int]](l, (Long) => new Rail[Int](r.randomPermut(l, 0n)));
		return prefs;
	}
	
}

public type StableMarriageAS(s:Long)=StableMarriageAS{self.sz==s};
	