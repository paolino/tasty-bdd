# Behavior-driven development 

## A BDD framework with:

* A type constrained language to express
  *  *Given* as ordered preconditions or
  *  *GivenAndAfter* as oredered preconditions with reversed order of teardown actions
  *  One only *When* to introduce a last precondition and catch it's output to be fed to
  *  Some *Then* tests that will receive the output of *When* 
* One interpreter for IO
* One driver for Tasty test library
* Support for --fail-fast flag which will not execute the teardown actions of the failed test

## Example

```
t1' = (
      Given (print "Given 1")
    . Given (print "Given 2")
    . GivenAndAfter (print "Given before After 3" >> return "After After 3") (print)
    . GivenAndAfter (print "Given before After 4" >> return "After After 4") (print)
    . When (print "when ptek" >> return ([1..5]++[500..506]) :: IO [Int])
    . Then (@?= ([1..5]++[600..606]))
    . Then (\r -> length r @?= 4)
    . Then (\_ -> print "Then 5")
    )
```
