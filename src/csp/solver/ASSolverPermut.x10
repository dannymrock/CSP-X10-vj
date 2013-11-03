package csp.solver;

import csp.util.*;

/** ASSolverPermut is the implementation of Adaptive Search solver
 * 	in the x10 lenguage.
 *  Implementation specialized in Permuts Problems.
 * 
 *  This x10 code is an adaptation of the Adaptive Search Algoritm C implementation by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013 -> first version
 * 				 April 12, 2013 -> Exahustive search implemented
 * 	
 */

import x10.util.Random;
public class ASSolverPermut(sz:Long) {

	val mark : Rail[Int]; 
	val size : Int;  
	val solverP : ASSolverParameters; 
	val solvers : ASSolverPermutRW(sz);
	
	//var nb_var_to_reset : Int; 
	
	var max_i : Int;		//static int max_i ALIGN;		/* swap var 1: max projected cost (err_var[])*/
	var min_j : Int;
	
	var best_cost : Int;
	var new_cost : Int;
	var total_cost : Int;
	val random : RandomTools;
	var kill : Boolean;
	
	var list_i_nb : Int;
	var list_j_nb : Int;
	var list_i : Rail[Int]; 
	val list_ij : Rail[PairAS];
	var nb_var_marked : Int;
	//val varRegion : Region(1);
	/** Number of iterations to update kill status */
	//val updateP : Int;
	
	/**	Statistics	*/
	var nbRestart : Int;
	var nbIter : Int;
	var nbReset : Int;	
	var nbSwap : Int;
	var nbSameVar : Int;
	var nbLocalMin : Int;
	/** Number time to change vector due to communication */ 
	var nbChangeV : Int;
	
	/** Total Statistics */
	var nbIterTot : Int;
	var nbResetTot : Int;	
	var nbSwapTot : Int;
	var nbSameVarTot : Int;
	var nbLocalMinTot : Int; 
	
	/** For Exhaustive search */
	var nbListIJ : Int;
	
	
	/** Diversification approach **/
	var alMaxI : Int;
	var alMinJ : Int;
	/**
	 *  Constructor of the class
	 * 	@param sizeOfProblem size of the problem to solve
	 *  @seed seed for the randomness in the object.
	 * 
	 */
	public def this(sz:Long, sizeOfProblem : Int , seed : Long, ss:ASSolverPermutRW(sz)) {
	    property(sz);
		size = sizeOfProblem;
		//varRegion = 0..(size - 1);
		mark = new Rail[Int] (size, 0n);
		list_i = new Rail[Int](size, 0n);
		list_ij = new Rail[PairAS](size);
		solverP = new ASSolverParameters();
		random = new RandomTools(seed);
		nb_var_marked = 0n;
		nbRestart = 0n;
		//updateP = updateI; //Default value 
		kill = false;
		solvers = ss;    //set??
		nbChangeV = 0n;
		
		
	}
	
	/**
	 *  solve( csp : ModelAS ) : Int
	 *  Solve a csp Problem through the Adaptive Search algoritm
	 * 	@param csp The model of the problem to solve
	 *  @return the final total cost after solving process (If success returns 0)
	 */ 
	public def solve( csp : ModelAS{self.sz==this.sz} ) : Int { //
		
		var nb_in_plateau:Int; 
		
		csp.setParameters(solverP);
		
		//nb_var_to_reset = (((size * solverP.resetPercent) + (100) - 1) / (100));
		if (solverP.nbVarToReset == -1n){
			solverP.nbVarToReset = (((size * solverP.resetPercent) + (100n) - 1n) / (100n));
			if (solverP.nbVarToReset < 2n)
			{
				solverP.nbVarToReset = 2n;
				//Console.OUT.printf("increasing nb var to reset since too small, now = %d\n", solverP.nbVarToReset);
			}
		}
		
		csp.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
		//Main.show("initial= ",csp.variables);
		
		mark.clear();
		list_i.clear();
		
		nbRestart = 0n;
		nbSwap = 0n;
		nbIter = 0n;
		nbSameVar = 0n;
		nbLocalMin = 0n;
		nbReset = 0n;
		nbChangeV = 0n;
		
		nb_in_plateau = 0n;
		
		nbIterTot = 0n;
		nbResetTot = 0n;	
		nbSwapTot = 0n;
		nbSameVarTot = 0n;
		nbLocalMinTot = 0n; 
		
		
		total_cost = csp.costOfSolution(1n);
		best_cost = total_cost;
		var best_of_best: Int = x10.lang.Int.MAX_VALUE ;
		
		//var slope : Int = 0;
		//var antcost : Int = total_cost;
		
		while( total_cost != 0n ){
			if (best_cost < best_of_best)
				best_of_best = best_cost;
	
			nbIter++;
	  
			if (nbIter >= solverP.restartLimit){
				if(nbRestart < solverP.restartMax){
					csp.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
					mark.clear();
					nbRestart++;
					//Update Total statistics
					nbIterTot += nbIter;
					nbResetTot += nbReset;	
					nbSwapTot += nbSwap;
					nbSameVarTot += nbSameVar;
					nbLocalMinTot += nbLocalMin; 
					//Restart local var
					nbSwap = 0n;
					nbIter = 0n;
					nbSameVar = 0n;
					nbLocalMin = 0n;
					nbReset = 0n;
					nb_in_plateau = 0n;
					
					best_cost = total_cost = csp.costOfSolution(1n);
					best_of_best = x10.lang.Int.MAX_VALUE ;
					//restart pool?
					solvers.conf.restartPool();
					//Console.OUT.println("Restart...");
					continue;
				}
				break; 
			}
			
			if( !solverP.exhaustive ){
				max_i = selectVarHighCost( csp );
				//Console.OUT.print("max_i= "+max_i);
				min_j = selectVarMinConflict( csp );
				//Console.OUT.println("  min_j= "+min_j);
			} else {
				selectVarsToSwap( csp );
				//Console.OUT.println("max_i= "+max_i+"  min_j= "+min_j);
			}
			
			//Console.OUT.println("----- iter no: "+nbIter+", cost: "+total_cost+", nb marked: "+nb_var_marked+" ---, nb_swap= "+nbSwap);
			
			if (total_cost != new_cost)
			{
				if (nb_in_plateau > 1n)
			 	{
			 		//Console.OUT.println("end of plateau, length: "+ nb_in_plateau);
			 	}
			 	nb_in_plateau = 0n;
			}
			if (new_cost < best_cost)
				best_cost = new_cost;
			
			nb_in_plateau++;
			
			if (min_j == -1n) //What??
				continue;
			
	 		if (max_i == min_j)
			{	
	 			
	 			//val res = solverC.communicate(total_cost, csp,commRefs);
	 			//if (min_j != alMinJ)
	 				//Console.OUT.println("lmin_j = "+ min_j+ " alMinJ = "+alMinJ);
	 			
	 			nbLocalMin++;
				mark(max_i) = nbSwap + solverP.freezeLocMin; //Mark(max_i, freeze_loc_min);
				//Console.OUT.println("nb_var_marked "+nb_var_marked+"solverP.resetLimit= "+solverP.resetLimit);
	 			if (nb_var_marked + 1 >= solverP.resetLimit)
	 			{
	 				
	 				// do reset or get some vector from the comm pool
	 				/*if (random.randomInt(100) < solverP.probChangeVector){
	 					val result = solverC.getIPVector(csp, total_cost );
	 					if (result == -1)
	 						doReset(solverP.nbVarToReset,csp);//doReset(nb_var_to_reset,csp);
	 					else{
	 						nbChangeV++;
	 						nbSwap += size ; //I don't know what happened here with costas reset
	 						mark.clear();
	 						total_cost = csp.costOfSolution(1);
	 					}
	 				}else{*/
	 				
	 				
		 				//Console.OUT.println("\tTOO MANY FROZEN VARS - RESET");
		 				doReset(solverP.nbVarToReset,csp);//doReset(nb_var_to_reset,csp);
		 				//Main.show("after reset= ",csp.variables);
	 				//}
	 				
	 				
	 			}
			}
			else
			{
				mark(max_i) = nbSwap + solverP.freezeSwap; //Mark(max_i, ad.freeze_swap);
				mark(min_j) = nbSwap + solverP.freezeSwap; //Mark(min_j, ad.freeze_swap);
			
				csp.swapVariables(max_i, min_j);//adSwap(max_i, min_j,csp);
				nbSwap++;
				csp.executedSwap(max_i, min_j);
				total_cost = new_cost;
				
				//slope = antcost - total_cost;
				//antcost = total_cost;
				//Console.OUT.println("slope in "+here.id+" : "+slope+ " total cost : "+total_cost);
			}
	 		
	 		Runtime.probe();		// Give a chance to the other activities
	 		if (kill)				// Check if other place or activity have finished
	 			break;
	 		
	 		if( nbIter % solvers.conf.intraTI == 0n ){
	 			//Console.OUT.println("In ");
	 			//Chang//
	 			val res = solvers.conf.communicate( total_cost, csp.variables); 
	 			if (random.randomInt(100n) < solverP.probChangeVector){
	 				val result = solvers.conf.getIPVector(csp, total_cost );
	 				if (result != -1n){
	 					nbChangeV++;
	 					nbSwap += size ; //I don't know what happened here with costas reset
	 					mark.clear();
	 					total_cost = csp.costOfSolution(1n);
	 					//Console.OUT.println("Changing vector in "+ here);
	 				}
	 					
	 			}	
	 			//Console.OUT.println("Print Vectors("+here.id+") :");
	 			//myComm.printVectors();
	 			//Main.show("Vector ",csp.variables);
	 			
	 		}
	 		
	 		//Main.show("new vector ",csp.variables);
		}
		
		nbIterTot += nbIter;
		nbResetTot += nbReset;	
		nbSwapTot += nbSwap;
		nbSameVarTot += nbSameVar;
		nbLocalMinTot += nbLocalMin; 
		
		//if(!kill){
			//Console.OUT.println(here);
			//Main.show("final= ",csp.variables);
		//}
		
		//Console.OUT.println("Cost = "+total_cost);
		
		return total_cost;
	}
	
	/**
	 * 	selectVarHighCost( csp : ModelAS ) : Int
	 * 	Select the maximum cost variable of the problem 
	 *  Also computes the number of marked variables.
	 *  @param csp problem model
	 * 	@return the index of the variable with high individual cost
	 */
	public def selectVarHighCost( csp : ModelAS ) : Int{
		
		var i: Int;
		var x: Int;
		var max: Int;
	
		list_i_nb = 0n; //Number of elements
		max = 0n;
		nb_var_marked = 0n;
		i = -1n; 
		//Console.OUT.println("Aqui");
		while(++i < size) 
		{
			if (nbSwap < mark(i))
			{
				nb_var_marked++;
				continue;
			}
			//Console.OUT.println("Aqui");
			x = csp.costOnVariable(i);
			//Console.OUT.println("var: "+i+" cost= "+x);
			if (x >= max){
				if (x > max){
					max = x;
					list_i_nb = 0n;
				}
				list_i(list_i_nb++) = i; 
			}
		}
		
		x = random.randomInt(list_i_nb);
		//Console.OUT.println("list_i_nb "+list_i_nb+ " x "+x+" list_i(x) "+list_i(x));
		max_i = list_i(x); //This max_i must be local or only returns the value
		nbSameVar += list_i_nb;
		
		// get alternative maxI for communication pourposses
		x = random.randomInt(list_i_nb);
		alMaxI = list_i(x); // I hope list_i_nb are > 1 
		
		return max_i;
	}
	
	/**
	 *	selectVarMinConflict( csp : ModelAS) : Int
	 *	Computes swap and selects the minimum of cost if swap
	 * 	@param csp problem model
	 * 	@return the index of the variable with minimum individual cost if swap
	 */
	public def selectVarMinConflict( csp : ModelAS) : Int {
		var j: Int;
		var x: Int;
		var flagOut:Boolean = false; 
		var lmin_j : Int = -1n;
		
		//loop: 
		do{
			flagOut = false;
			list_j_nb = 0n;
	 		new_cost = total_cost;
	 		//Console.OUT.println("total_cost"+total_cost);
	 		j = -1n;
	 	
		 	while(++j < size) 
		 	{	
		 		//Console.OUT.println("swap "+j+"/"+max_i);
		 		x = csp.costIfSwap(total_cost, j, max_i);
		 		//Console.OUT.println("swap "+j+"/"+max_i+"  Cost= "+x);
		 		
		 		if (solverP.probSelectLocMin <= 100n && j == max_i)
		 			continue;
		 		
		 		//
		 		if (x < new_cost){
		 			list_j_nb = 1n;
		 			new_cost = x;
		 			lmin_j = j;
		 			
		 			//For alternative move 
		 			alMinJ = j;
		 			
		 			if (solverP.firstBest)
		 			{
		 				return lmin_j;         
		 			}
		 		} else if (x == new_cost){
		 			if (random.randomInt(++list_j_nb) == 0n)
		 				lmin_j = j;
		 			
		 			//Select alternative move
		 			if (random.randomInt(list_j_nb) == 0n)
		 				alMinJ = j;
		 			
		 		}
		 	}
	 	
		 	if (solverP.probSelectLocMin <= 100n)
		 	{
		 		if (new_cost >= total_cost && 
		 			(random.randomInt(100n) < solverP.probSelectLocMin ||(list_i_nb <= 1n && list_j_nb <= 1n)))
		 		{
		 			lmin_j = max_i;
		 			return lmin_j;
		 		}
		
		 		if (list_j_nb == 0n)
		 		{
		 			//Console.OUT.println("list_i_nb= "+list_i_nb);
		 			nbIter++;
		 			x = random.randomInt(list_i_nb);
		 			max_i = list_i(x);
		 			flagOut = true;
		 		}
		 	}
		}while(flagOut);
	 	//Console.OUT.println("list_J = "+ list_j_nb);
		
		//Chang//
		//Here communicate alternative vector with some probability
		/*if (lmin_j != alMinJ && solverC.commOption != 0){//if (solverC.commOption != 0){//
			//Console.OUT.println("lmin_j = "+ lmin_j+ " alMinJ = "+alMinJ);
			var altConf : Rail[Int] = new Rail[Int](0..(size-1));
			Array.copy(csp.variables, altConf);
			// swap var
			val aux = altConf(alMinJ);
			altConf(alMinJ) = altConf(max_i);
			altConf(max_i) = aux;
			
			val res = solverC.communicate( new_cost, altConf);
		}*/
		
		return lmin_j;
	}
	
	/**
	 * 	doReset( var n : Int, csp : ModelAS )
	 * 	Performs the reset over the problem model csp
	 *  @param n number of variables to reset
	 * 	@param csp Model to reset
	 */
	public def doReset( var n : Int, csp : ModelAS ) {
		
		var cost : Int = -1n;		//reset(n, csp);
				
		cost = csp.reset( n, total_cost );
		nbSwap += n ; //I don't know what happened here with costas reset
		
		mark.clear();
		nbReset++;
		//Console.OUT.println("Do reset...: "+ nbReset);
		total_cost = (cost < 0n) ? csp.costOfSolution(1n) : cost; //Arg costofsol(1)
	}
	
// 	public def changeVector(csp : ModelAS){
// 		var ipVector : Int = -1;
// 		
// 		//Main.show("antes= ",csp.variables);
// 		ipVector = solverC.getIPVector(csp, total_cost, commRefs);
// 		//Main.show("despues= ",csp.variables);
// 		
// 		if (ipVector == 1){
// 			nbChangeV++;
// 			nbSwap += size;
// 			//Console.OUT.println("do change vector");
// 			mark.clear();
// 			total_cost = csp.costOfSolution(1); //Arg costofsol(1)
// 		}
// 
// 	}
	
	
	/**
	 * 	Clear function
	 */
	public def clear(){
	}
	
	/**
	 *  Computes max_i and min_j, the 2 variables to swap.
	 *  All possible pairs are tested exhaustively.
	 */
	public def selectVarsToSwap(csp : ModelAS)
	{
		var i : Int;
		var j : Int;
		var x : Int;
		
		nbListIJ = 0n;
		new_cost = x10.lang.Int.MAX_VALUE ;
		nb_var_marked = 0n;
		
		//Console.OUT.println("TC =>"+total_cost);
 
 		i = -1n;
 		while(++i < size) // false if i < 0
 		{
			if ( nbSwap < mark(i) )
 			{
 				nb_var_marked++;
 			}
 			j = i; //j = -1;
 			while(++j < size) //while((unsigned) (j = Next_J(i, j, i + 1)) < (unsigned) ad.size) // false if j < 0
 			{
 				//Console.OUT.println("SWAP "+i+" <-> "+j);
 				x = csp.costIfSwap(total_cost, i, j);
 				//Console.OUT.println("cost = "+x);
 
 				if (x <= new_cost)
 				{
 					if (x < new_cost)
 					{
 						new_cost = x;
 						nbListIJ = 0n;
 						if (solverP.firstBest == true && x < total_cost)
 						{
 							max_i = i;
 							min_j = j;
 							return; 
 						}
 					}
 					list_ij(nbListIJ) = new PairAS();
 					list_ij(nbListIJ).i = i;
 					list_ij(nbListIJ).j = j;
 					nbListIJ = (nbListIJ + 1n) % size;
 				}
 			}
 		}
 
 		nbSameVar += nbListIJ;
 
 		if (new_cost >= total_cost)
 		{
 			if (nbListIJ == 0n || 
 					(( solverP.probSelectLocMin <= 100n) && random.randomInt(100n) < solverP.probSelectLocMin))
 			{
 				for(i = 0n; nbSwap < mark(i); i++)
 				{}
 				max_i = min_j = i;
 				return;//goto end;
 			}
 
 			if (!(solverP.probSelectLocMin <= 100n) && (x = random.randomInt(nbListIJ + size)) < size)
 			{
 				max_i = min_j = x;
 				return;//goto end;
 			}
 		}
 
 		x = random.randomInt(nbListIJ);
 		max_i = list_ij(x).i;
 		min_j = list_ij(x).j;
 		return;
	}
	
	public def testSelectVarHighCost(csp: ModelAS){ 
		var test:Int;
		
		csp.setParameters(solverP);
		csp.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
		
		mark.clear();
		csp.costOfSolution(1n);
		var timeStart :Long = x10.lang.System.nanoTime();
		test = selectVarHighCost(csp);
		var timeEnd :Long = x10.lang.System.nanoTime(); 
		
		Console.OUT.println("max_i= "+test);
		
		return timeEnd-timeStart;
	}
}
public type ASSolverPermut(s:Long)=ASSolverPermut{self.sz==s};