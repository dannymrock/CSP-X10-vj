package csp.solver;

import csp.util.*;


public class CommData(sz:Long, poolSize:Int) {
	var nbEntries : Int = 0n;
	val bestPartialSolutions = new Rail(poolSize, CSPSharedUnit(sz,0n as Int,null,0n as Int));
	var bestCost : Int = Int.MAX_VALUE;
	var worstCost : Int = Int.MAX_VALUE;
	val random = new RandomTools(123L);
	val monitor = new Monitor();
	
	public def isGoodCost(cost : Int) : Boolean {
		if (nbEntries == 0n) return true;
		for (i in 0..(nbEntries-1)){
			if (cost <= bestPartialSolutions(i).cost)
				return true;
		}
		return false;
	}
	public def tryInsertVector(cost:Int, variables:Rail[Int]{self.size==sz}, place:Int) {
	    monitor.atomicBlock(()=>tryInsertVector0(cost,variables,place));
	}
	public def tryInsertVector0( cost : Int , variables : Rail[Int]{self.size==sz}, place : Int ):Unit {
		
		if (cost >= worstCost) return Unit();

		var i : Int;
		//Console.OUT.println("in");
		if( nbEntries < poolSize ){
			//insert in the last place
			//Console.OUT.println("insert cost "+cost);
			//Main.show("insert vector", variables);
			//Console.OUT.println("insert vector with cost "+cost);
			bestPartialSolutions( nbEntries++ ) = new CSPSharedUnit(sz, cost, Utils.copy(variables), place );
			if (cost < bestCost){ 
				bestCost = cost;
				//Console.OUT.println("New Best Cost = "+bestCost+" in place "+place);
			}	
			
		}else{
			// No place available select a victim
			
			var equal : Boolean = false;
			var victim : Int = 0n;
			var nvic : Int = 0n;
			var costToChange : Int = cost;
			
			for (i = 0n; i < nbEntries; i++){
				if (worstCost == bestPartialSolutions(i).cost){
					if (random.randomInt(++nvic) == 0n)
						victim = i;
				}
				
				if (cost == bestPartialSolutions(i).cost){
					if (compareVectors(variables, bestPartialSolutions(i).vector))
						return Unit();
				}
			}	
			//Console.OUT.println("insert vector with cost "+cost);	
			bestPartialSolutions(victim) = new CSPSharedUnit( variables.size, cost, Utils.copy(variables), place);
			
			if (cost <= bestCost){ 
				bestCost = cost;
				//Console.OUT.println("New Best Cost = "+bestCost+" in place "+place);
			}				
		}
		updateWorstCost();
		return Unit();
	}
	
	
	public def compareVectors (vec1 : Rail[Int], vec2 : Rail[Int]):Boolean{
		for (i in 0..( vec1.size-1))
			if(vec1(i) != vec2(i)) return false;
		return true;
	}
	
	public def updateWorstCost(){
		var wc : Int = 0n;
		for(i in 0..(nbEntries-1))
			if (bestPartialSolutions(i).cost > wc) wc = bestPartialSolutions(i).cost; 
		worstCost = wc;	
	}
	
	public def printVectors(){
		for(i in 0..(nbEntries-1)) {
			Console.OUT.print(i+". Cost = "+bestPartialSolutions(i).cost+" place "+bestPartialSolutions(i).place);
			Utils.show(" Vector",bestPartialSolutions(i).vector);
		}
	}
	
	/**
	 * Get some vector from the best solutions.
	 */
	public def getRemoteData():Maybe[CSPSharedUnit(sz)]=
	  monitor.atomicBlock(()=>nbEntries==0n? null : new Maybe(bestPartialSolutions(random.randomInt(nbEntries))));
		
	public def clear(){
		nbEntries = 0n;
		bestCost = Int.MAX_VALUE;
		worstCost = Int.MAX_VALUE;
	}
	
}

