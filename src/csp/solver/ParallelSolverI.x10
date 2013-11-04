package csp.solver;

public interface ParallelSolverI {
    property sz():Long;
    
    def restartPool():void;
    
    def getIPVector(csp_:ModelAS(sz), myCost:Int):Int; 
    
    def communicate(totalCost:Int, variables:Rail[Int]{self.size==sz}):Int;
  
    public def intraTI():Int;
    
}
public type ParallelSolverI(s:Long)=ParallelSolverI{self.sz==s};