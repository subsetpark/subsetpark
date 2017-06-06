Title: Unit Testing in Nim
Date: 2017-05-07
status: post

## Unit Testing in Nim and Python

Here's a snippet of a unittest test case that I wrote in Python some years ago:

```py
class TestClasses(TestCase):

    def setUp(self):
        self.ace_diamonds = Card(Rank.Ace, Suit.DIAMONDS)
        self.king_clubs = Card(Rank.King, Suit.CLUBS)
        self.ace_clubs = Card(Rank.Ace, Suit.CLUBS)
        
    def test_card(self):
        self.assertGreater(self.ace_diamonds, self.king_clubs)
        self.assertEquals(self.ace_diamonds, self.ace_clubs)
```

As you might guess, it was testing a class involved in a card game. It should be familiar to most Python programmers; `TestClasses` is a class inheriting from `unittest.TestCase`, which defines a method `test_card`. The test runner of my choice will read the module, instantiate the test case class, and then run any methods beginning with `test_`. In that method, I set up a few objects and then run some asserts on them.

Now, notice straight away---Python has an `assert` keyword, but I don't use it here. Instead I use a couple of unittest's own assertion methods, which are numerous; there's one for most every kind of relationship you might want to assert over. Why use them instead of bare assert? Because the methods, knowing what kind of assertion they are and having access to the arguments, are able to produce much more informative error messages. If I change one of these assertions so it fails, here's the traceback:

```py
Traceback (most recent call last):
  File "/home/zax/code/pyquet/tests/test-pyquet.py", line 20, in test_card
    self.assertLess(ace_diamonds, king_clubs)
AssertionError: Ace♢ not less than King♧
```

unittest is able to call the Card class's `__repr__` method and produce a failure message that tells me why the assertion failed.

Compare that to what you see if you just use `assert`:

```py
Traceback (most recent call last):
  File "/home/zax/code/pyquet/tests/test-pyquet.py", line 20, in test_card
    assert ace_diamonds < king_clubs
AssertionError
```

Speaking more generally, unittest can evaluate all kinds of expressions when performing asserts. Compare

```
Traceback (most recent call last):
  File "/home/zax/code/pyquet/tests/test-pyquet.py", line 21, in test_card
    self.assertLess(math.sqrt(5), math.sqrt(2))
AssertionError: 2.23606797749979 not less than 1.4142135623730951
```

to

```py
Traceback (most recent call last):
  File "/home/zax/code/pyquet/tests/test-pyquet.py", line 21, in test_card
    assert math.sqrt(5) < math.sqrt(2)
AssertionError
```

## Unit testing in Nim

Here's a pretty straightforward translation of the above method into Nim:

```nimrod
type
  Card = object
    rank: Rank
    suit: Suit
  Rank = enum
    crSeven
    crEight
    crNine
    crTen
    crJack
    crQueen
    crKing
    crAce
  Suit = enum
    csClubs = "♧"
    csDiamonds = "♢"
    csHearts = "♡"
    csSpades = "♤"

proc `<`(a,b: Card): bool = a.rank < b.rank

when isMainModule:
  let
    aceDiamonds = Card(rank: crAce, suit: csDiamonds)
    kingClubs = Card(rank: crKing, suit: csClubs)
    aceClubs = Card(rank: crAce, suit: csClubs)

  assert aceDiamonds > kingClubs
  assert aceDiamonds == aceClubs
```

We declare our types, implement `<` (the `>` operator is just sugar for backwards `<`) and use `isMainModule`[^main] to do some quick tests. When we compile and run, we get a traceback:

```
Traceback (most recent call last)
unittest.nim(34)         unittest
system.nim(3518)         failedAssertImpl
system.nim(3510)         raiseAssert
system.nim(2620)         sysFatal
Error: unhandled exception: aceDiamonds == aceClubs  [AssertionError]
```

Now, we already get a little bit for free here; the `AssertionError` is kind enough to isolate the expression that evaluated as false (we conclude that `aceDiamonds` is not equal to `aceClubs`), but it's not exactly a *test*. The rest of the module wouldn't get run, for instance, and we don't get any visual feedback about the other check.

[^main]: This is nice; it means that if this module were to be imported and used in some application, that logic wouldn't even be compiled into the binary, let alone evaluated. Like `if __name__ == "__main__"` on steroids, as they said in the 90s.

So we decide to use Nim's own [unittest](https://nim-lang.org/docs/unittest.html) module.

```nimrod
when isMainModule:
  import unittest
  suite "test card relations":

    setup:
      let
        aceDiamonds = Card(rank: crAce, suit: csDiamonds)
        kingClubs = Card(rank: crKing, suit: csClubs)
        aceClubs = Card(rank: crAce, suit: csClubs)

    test "greater than":
      check:
        aceDiamonds > kingClubs
        aceClubs > kingClubs
    test "equal to":
      check aceDiamonds == aceClubs
```

This should all be pretty readable. We declare a `suite`---that's like a `TestCase`---with a `setup` block---that's like a `setUp()` method---and then two `test`s. In the tests we use the `check` statement to do our assertions.

Now our test output is a little more informative:

```
[Suite] test card relations
  [OK] greater than
    pyquest.nim(35,24): Check failed: aceDiamonds == aceClubs
    aceDiamonds was (rank: crAce, suit: ♢)
    aceClubs was (rank: crAce, suit: ♧)
  [FAILED] equal to
```

It turns out we forgot to implement `==`. Without a `==` proc written for the `Card` type, Nim was using default object comparison. Implementing that proc produces this test output:

```
[Suite] test card relations
  [OK] greater than
  [OK] equal to
```

### Checkpoint A

The above is all you need to understand writing simple unit tests in Nim. We can see that the basic structure of a Nim unittest follows very closely to that of a unittest TestCase: test case, setup, tests, teardown.

### Checkpoint B

One thing that's worth pointing out is that while the structure of the two test suites is very similar, the style of the code is different. The Python code is structured as a class, where each test is a method. Further, the assertions are methods of the base class that the test case inherits from. This makes sense, as Python is a fundamentally object-oriented language, and class inheritance is one of the main ways that things get done in a Python program.

On the other hand, the Nim code is a little more stripped down; the same behavior is accomplished, but there's no base class to inherit from, and no `self` that's passed from method to method. And the assertions inside the `check` calls are expressed as normal expressions, rather than specialty methods expressing specific relations. And yet the same `check` call is able to evaluate an expression as well as output its location in the source, without requiring a traceback.

In short, this is possible in Nim in a way that it isn't possible in Python because Nim has an extremely powerful *macro* system. 

It's not my intention to dive into the specifics of writing macros in Nim here. Nim macros, like macros in any language, are extremely powerful and can be quite difficult to reason about, as they allow the programmer to operate at two levels at once: both within the evaluation context of the language, as well as above it, interacting with and modifying the syntax tree of the language itself.

But this also means that they are extremely effective at creating *[DSLs](https://en.wikipedia.org/wiki/Domain-specific_language)* in ways that other languages simply don't have at their disposal. Statements like `check` and `suite` in the Nim unittest module almost act like additions to the syntax of the language itself; they're not functions or classes, operating at runtime; they operate at compilation, accepting the tests written by the programmer as [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) objects and manipulating them until the resulting code is quite different indeed. Not everybody likes macros, and with good reason; but I think a testing framework like this is a great example of their utility. They allow us to bring to bear a specialized and significantly higher level of expressiveness. The drawback is that the "rules" no longer apply; if you weren't familiar with the unittest module, you might not understand how this code compiled at all. But writing tests, in my view, is a perfect example of a situation where the programmer benefits from the power of a DSL.[^pytest]

[^pytest]: There is another well-established testing framework in Python: [pytest](https://docs.pytest.org/en/latest/). Which, it should be noted, can do exactly what I've made a lot of noise about unittest not being able to do; it allows you to use normal `assert foo == bar` statements in your tests. In some ways, this is the exception that proves the rule when it comes to macros; the wizardry behind pytest's [assertion introspection](http://pybites.blogspot.com/2011/07/behind-scenes-of-pytests-new-assertion.html) is heavy-duty enough that it might as well be a macro itself.

### Checkpoint C

I feel that the story of testing in Nim is far from over. unittest is powerful and elegant, and admirably simple; the entire module is about 400 lines. But it's not as full-featured as frameworks that have been around in Python for much longer. 

There's also [einheit](https://github.com/jyapayne/einheit), which I have not tried but ironically was designed with the intention of drawing more inspiration from Python's unittest. 

There is nothing at all equivalent to [Hypothesis](http://hypothesis.works/), the masterful property-based testing framework for Python. Hopefully some day we'll have a full-featured quickcheck analog at our disposal in Nim.
