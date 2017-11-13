# Behavior-driven development 

## A Behavior Driven Development framework with:

* A type constrained language to express
  *  *Given* as ordered preconditions or
  *  *GivenAndAfter* as oredered preconditions with reversed order of teardown actions
  *  One only *When* to introduce a last precondition and catch it's output to be fed to
  *  Some *Then* tests that will receive the output of *When* 
* One interpreter for IO
* One driver for Tasty test library
* Support for --fail-fast flag which will not execute the teardown actions of the failed test
* A rudimentary form of value introspection to show the differences on equality failure `@?=`

## Background

[Behavior Driven Development](https://en.wikipedia.org/wiki/Behavior-driven_development) is a software development process that emerged from test-driven development (TDD) and is based on principles of [Hoare Logic](https://en.wikipedia.org/wiki/Hoare_logic). The process requires a strict structure of the tests - {Given} When {Then} - to make them understandable.

## Example

```
t1 :: TestTree
t1 = testBdd "Test sequence" (
      Given (print "Some effect")
    . Given (print "Another effect")
    . GivenAndAfter (print "Aquiring resource" >> return "Resource 1")
                    (print . ("Release "++))
    . GivenAndAfter (print "Aquiring resource" >> return "Resource 2")
                    (print . ("Release "++))
    . When (print "Action returning" >> return ([1..10]++[100..106]) :: IO [Int])
    . Then (@?= ([1..10]++[700..706]))
    )
```
