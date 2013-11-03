package csp.solver;

import csp.util.*;


public struct CSPSharedUnit(sz:Long, cost:Int, vector:Rail[Int]{self.size==sz}, place:Int) {}
public type CSPSharedUnit(s:Long)=CSPSharedUnit{self.sz==s};