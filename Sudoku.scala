/** Sudoku solver.
  * Basic usage: 
  * # scala Sudoku filename: solves the puzzle in filename
  * # scala Sudoku --all: solves all the puzzles in this directory
  * # scala Sudoku --allPoss: solves all the possible puzzles in this directory
  * Options:
  * # -n n: repeat n times
  * # -a: use the AdvancedPartial
  */

import ox.cads.util.Profiler
import java.util.concurrent.atomic.AtomicBoolean
import ox.cads.collection.Pool
import ox.cads.collection.TerminationDetectingPool
import ox.cads.collection.LockFreeStack

class PooledStack[Partial] extends LockFreeStack[Partial] with Pool[Partial]  {
  def add(partial: Partial){
    this.push(partial)
  }
  //return type is Option[Partial] not sure what this means
  def get() : Option[Partial] = {
    this.pop
  }

}

object Sudoku{
  /** Solve the puzzle defined by init */
  def solve(init: Partial){
    // Stack to store partial solutions that we might back-track to.
    val stack = new scala.collection.mutable.Stack[Partial]
    stack.push(init)
    var done = false

    while(!stack.isEmpty && !done){
      val partial = stack.pop
      if(partial.complete){  // done!
	partial.printPartial; done = true
      }
      else{
	// Choose position to play
	val(i,j) = partial.nextPos;
	// Consider all values to play there
	for(d <- 1 to 9)
	  if(partial.canPlay(i,j,d)){
	    val p1 = partial.play(i,j,d); stack.push(p1)
	  }
      }
    } // end of while
  }

  def solveConcurrent(init: Partial){
    // Stack to store partial solutions that we might back-track to.
    //val stack = new scala.collection.mutable.Stack[Partial]
    val stack = new PooledStack[Partial]
    val terminationDetectingPool = new TerminationDetectingPool(stack, 10)
    stack.add(init)
    val done = new AtomicBoolean(false)
    def worker = {
    while(!done.get()){
      val val_pop = terminationDetectingPool.get
      if(val_pop == None){done.set(true)}
      else {val partial = val_pop.get
      if(partial.complete){  // done!
	if(done.compareAndSet(false, true)){ partial.printPartial}
      }
      else{
	// Choose position to play
	val(i,j) = partial.nextPos;
	// Consider all values to play there
	for(d <- 1 to 9)
	  if(partial.canPlay(i,j,d)){
	    val p1 = partial.play(i,j,d); terminationDetectingPool.add(p1)

          }
	  }
      }}
    } // end of while
  ox.cads.util.ThreadUtil.runSystem(10, worker)}

  /** A list of files containing possible puzzles */
  private val allPossibleFiles = 
    List("test1.sud", "test2.sud", "test3.sud", "test4.sud", "test5.sud",
	 "test6.sud", "test7.sud", "test8.sud", "test9.sud", "test10.sud")
  /** A list of files containing puzzles, including one impossible one. */
  private val allFiles = allPossibleFiles ++ List("impossible.sud")

  def main(args:Array[String]) = {
    val t0 = System.currentTimeMillis()

    // options
    var count = 1 // number of tests
    var fname = "" // filename
    var adv = false // are we using the AdvancedPartial?
    var all = false // are we doing all files in allFiles?
    var allPoss = false // are we doing all files in allPossibleFiles?
    // parse command line arguments
    var i = 0
    while(i < args.length){
      if (args(i)=="-n"){ count = args(i+1).toInt; i+=2 }
      else if (args(i)=="-a"){ adv = true; i+=1 }
      else if (args(i) == "--all"){ all = true; i += 1 }
      else if (args(i) == "--allPoss"){ allPoss = true; i += 1 }
      else{ fname = args(i); i += 1 }
    }
    assert(all || allPoss || fname != "")

    // Initialise partial from file fname
    def mkPartial(fname: String) = {
      val partial = if(adv) new AdvancedPartial else new SimplePartial
      partial.init(fname)
      partial
    }

    // Solve count times
    for(i <- 0 until count)
      if(all) for(f <- allFiles){ println(f); solveConcurrent(mkPartial(f)) }
      else if(allPoss) 
	for(f <- allPossibleFiles){ println(f); solveConcurrent(mkPartial(f)) }
      else solveConcurrent(mkPartial(fname))

    println("Time taken: "+(System.currentTimeMillis()-t0))
    Profiler.report
  }    
}

