package csp.solver;
import csp.util.Maybe;

/**
 * A solver runs a local solver in every place, within the frame of a 
 * ParallelSolverI instance. All communication by a solver with other solvers
 * in the team is mediated through the frame.
 * 
 * <p> Therefore to design a new parallel solver, simply have it implement
 * ParallelSolverI. 
 * 
 */
public interface ParallelSolverI {
    property sz():Long;
    
    
    def clear():void;
    
    def worstCost():Int;
    
    def solve(st:PlaceLocalHandle[ParallelSolverI(sz)], cspGen:()=>ModelAS(sz) ):void;
    
    /**
     * Get some best solution from the communication partner. If its
     * cost is less than myCot, update csp_ in place, and return true,
     * else return false. 
     */
    def getIPVector(csp_:ModelAS(sz), myCost:Int):Boolean; 
    
    def communicate(totalCost:Int, variables:Rail[Int]{self.size==sz}):Int;
    
    def tryInsertVector(cost:Int, variables:Rail[Int]{self.size==sz}, place:Int):void;
  
    def intraTI():Int;
    
    /**
     * Send a signal to the associated solver to kill it. The solver will 
     * kill itself the next time it checks for the kill signal.
     * 
     */
    def kill():void;
    
    /**
     * When a place p has a solution, it invokes this method. The first place
     * to execute this method during the program run will be declared the winner;
     * for it, the method will return true. Any subsequent invocation will
     * return false.
     * 
     * <p> In the invocation that returns true, kill() is invoked at every place.
     */
    def announceWinner(ss:PlaceLocalHandle[ParallelSolverI(sz)], p:Long):Boolean;
    
    def setStats(co : Int, p : Int, e : Int, t:Double, it:Int, loc:Int, sw:Int, re:Int, sa:Int, rs:Int, ch:Int, 
            fr : Int):void;
    
    def getRemoteData():Maybe[CSPSharedUnit(sz)];
    
    def accStats(CSPStats):void;
    def printStats(count:Int):void;
    def printAVG(count:Int):void;
    
    
}
public type ParallelSolverI(s:Long)=ParallelSolverI{self.sz==s};