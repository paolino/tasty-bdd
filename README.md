# Behavior-driven development 

## A [Haskell](https://www.haskell.org/) Behavior Driven Development framework featuring:

* A type constrained language to express
  *  *Given* as ordered preconditions or
  *  *GivenAndAfter* as oredered preconditions with reversed order of teardown actions (sort of resource management)
  *  One only *When* to introduce a last precondition and catch it's output to be fed to
  *  Some *Then* tests that will receive the output of *When*
* Support for do notation via free monad for composing _givens_ and _thens_ 
* One monad independent pure interpreter
* One driver for the great [tasty](https://github.com/feuerbach/tasty) test library,  monad parametrized
* Support for [tasty-fail-fast](https://hackage.haskell.org/package/tasty-fail-fast) strategy flag which will not execute the teardown actions of the failed test
* A sophisticated form of value introspection to show the differences on equality failure from [tree-diff](https://github.com/phadej/tree-diffdifftree) package 

## Background

[Behavior Driven Development](https://en.wikipedia.org/wiki/Behavior-driven_development) is a software development process that emerged from test-driven development (TDD) and is based on principles of [Hoare Logic](https://en.wikipedia.org/wiki/Hoare_logic). The process requires a strict structure of the tests - {Given} When {Then} - to make them understandable.

## Example

### Using bare language

```
exampleL :: TestTree
exampleL = testBdd "Test sequence" $
      Given (print "Some effect")
    . Given (print "Another effect")
    . GivenAndAfter (print "Aquiring resource" >> return "Resource 1")
                    (print . ("Release "++))
    . GivenAndAfter (print "Aquiring resource" >> return "Resource 2")
                    (print . ("Release "++))
    . When (print "Action returning" >> return ([1..10]++[100..106]) :: IO [Int])
    . Then (@?= ([1..10]++[700..706]))
    
```

### Using free monads

```
exampleF :: TestTree
exampleF = testBdd "Test sequence" $ hoare g w t
    where
    g = do
          given_ $ print "Some effect"
          given_ $ print "Another effect"
          givenAndAfter_ (print "Aquiring resource" >> return "Resource 1")
                    $ print . ("Release "++)
          givenAndAfter_ (print "Aquiring resource" >> return "Resource 2")
                    $ print . ("Release "++)
    w =  when_ $ print "Action returning" >> return ([1..10]++[100..106]) :: IO [Int]
    t = then_ (@?= ([1..10]++[700..706])
    
```

