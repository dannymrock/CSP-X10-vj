package csp.example;

/**
*  Xref Class
*  Data structure that helps to make the Magic Square funtions easier
*/
class XRef {
    var d1 : Boolean;
    var d2 : Boolean;
    var l : Int;
    var c : Int;
    
    public def this(){ d1 = false; d2 = false; l = 15n; c = 15n;}
    public def xSet(line : Int, col : Int, diag1 : Boolean, diag2 : Boolean){
        this.l = line; this.c = col; this.d1 = diag1; this.d2 = diag2;}
    public def getL() : Int{ return this.l; }
    public def getC() : Int{ return this.c; }
}