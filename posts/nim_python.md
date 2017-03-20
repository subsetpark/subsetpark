title: Nim for Python Programmers
date: 2017-02-19
status: post

I've been spending a lot of time with the [Nim][] programming language recently. It's a great language: its principles, roughly, are to achieve the efficiency and speed of C with a much greater level of safety and expressiveness. It also so happens to take a lot of its surface-level syntactic cues from Python, including code structuring through significant whitespace and colons---that plus its emphasis on expressiveness make it very appealing and inviting to a Python programmer (like myself) who wants greater type safety and greater speed in their code.

[Nim]: http://nim-lang.org

The [Nim for Python Programmers][nim-python] page on the [Nim Wiki][nim-wiki] has a useful overview of some of the most salient differences between Nim and Python for those just diving in. After all, it's *not* actually just Python With Types; it's more like C With All The Features That Make Python So Pleasant (and some more besides). Things like homogeneous arrays/lists, significantly reduced emphasis on object-oriented design, static typing, strings vs. chars---all of those will crop up if you sit down and start typing Python into `foo.nim`.

[nim-python]: https://github.com/nim-lang/Nim/wiki/Nim-for-Python-Programmers
[nim-wiki]: https://github.com/nim-lang/Nim/wiki

I want to spend a little time, however, thinking about some of the more subtle differences between Nim and Python: the things that will crop up after you've learned the Nim syntax but are still writing the language with a Pythonic frame of mind.

# Types vs. Types

Something that Python programmers will do now and then (and probably more than we think we do) is *runtime type inspection*, or *type introspection*. That is, we will examine the type of an object in our code and make decisions based on that type. We might have code like this:

```py
def foo(d):
    if isinstance(d, date):
        d = datetime.combine(d, time(0))
    # keep going...
```

That is, when we enter `foo` we don't actually know *what* `d` is, so we find out what it is as a part of our runtime logic and take some action based on what we find out. In this case we are performing some operation on `d` that expects a datetime (maybe it relies on a method on the `datetime` class), but (hopefully!) we don't actually care what time of day it contains, so we create a dummy datetime. But we could also raise an error, log something, or even take a completely different action.

In a way it's kind of the shadow side to [duck typing][duck]. Under duck typing, we try not to care *what* the type is of any object; we just expect it to have the methods we care about. But sometimes we have to care; then we can check out the type or class of an object in the middle of a function and make a decision at runtime.

[duck]: https://en.wikipedia.org/wiki/Duck_typing

This kind of behavior, to the Nim compiler, is nonsensical. In Nim and many of its statically typed brethren, *a type isn't a type if you don't know it at compile time*. That is, in a language like Nim, you already as the programmer need to know exactly what `d` is when you're writing `foo`. After all, you need to tell the compiler[^adt]:

```nimrod
proc foo(d: Datetime)
```

[^adt]: In this particular example, the Nim `times` module doesn't distinguish between Dates and Datetimes; there's just Time, which has all the precision of Datetime. From a Python perspective this seems restrictive; however, after a little while you might realize that the reason you need both `date` and `datetime` in Python is because they're not types, but *classes*---they implement useful and sometimes mutually exclusive methods. When you boil away the methods and you're left with just the data and its structure, you don't need nearly as many types in Nim as you need classes in Python.

## You Can't Do That

This is what we mean by *type safety*. The part of type safety that we all can get behind and appreciate is that the language won't let us do something we obviously didn't mean to do, like call `foo(28)` or `foo("bacon")`. But what can we do if we want to accept *either* a date *or* a datetime?

Well, the first answer is that as you write more type-safe code, you will find that this doesn't actually crop up nearly as often as it does in a language like Python. Odds are that when I wrote the Python function above, it came out like that because I wrote a function that took a datetime `foo`, and then wrote some logic that called it with a datetime:

```py
today = utcnow()
foo(today)
```

And then a while later, I had some other totally different logic that called that same function:

```py
requested_date = date(requested_year, requested_month, requested_day)
foo(requested_date)
```

And it called it with a date, which caused an exception (at runtime). Whoops. I might not have even noticed---I'm sure there are lots of places where I do this exact same thing and it *doesn't* crash, because you can do an awful lot of the same things with dates that you can do with datetimes. So I added the line to just do the right thing and convert it and not crash.

But the only reason I wrote the second chunk of logic in the way that I did is because I didn't remember and didn't particularly care that my first function actually worked on datetimes, not dates! 

And in fact, it's just as possible that I had *both* of those call sites looking just like they do above, and it was the original function `foo` that I changed. Maybe the first version of the function only called the attributes of `d` that are shared in common between dates and datetimes, but then I added some functionality that only worked for datetimes. Now suddenly half of my invocations are broken. Let's hope they were all tested.

If I had been writing in a more type-safe language, the compiler would have yelled at me whenever I called `foo` on a `date` (whether or not `foo` relied on any methods that only exist for datetimes). This means we don't have to roll the dice when making the above sorts of changes. So I'd always know what sorts of data were being passed into the function and what it relied on, even as those things changed over time.

## You Can Do Cool Stuff

So what are we really asking above? It comes down to a question of *polymorphism*: how do we implement the same, or similar behaviors for different data types? 

After all, `foo` above is not actually the same behavior for both types. It's a little more like this:

```py

class Datetime:

    def foo(self):
        """
        Do cool stuff here
        """

class Date:

    def foo(self):
        dt = Datetime(self, time(0))
        dt.foo()
```

That is, the behavior of `foo` when `d` is a `date` is a superset of the behavior when `d` is a `datetime`. We could argue the merits of Python's thoroughly class-based, object-oriented approach, but it's not available in Nim. 

Luckily, Nim has a trick that Python doesn't have, which makes up for quite a bit of where we'd use classes for polymorphism: [procedure overloading](https://en.wikipedia.org/wiki/Function_overloading). You must declare the types of all procedure parameters in Nim; however, you also are able to declare multiple procedures with the same name (and even arity), with different parameter types.

```nimrod
proc foo(d: Datetime) = 
  echo "This is one great procedure!"

proc foo(d: Date) =
  echo "I'm getting there..."
  d.toDatetime().foo()
```

What this means is that you can achieve some of the semantics of OO style through totally different (and arguably much simpler) means. One of the really elegant things about the OO message-passing style is that the messages you pass to objects can have really powerful, unified semantics; you're essentially saying, "in my domain, `foo` has meaning. It's up to everybody to define what `foo` means to them." This is often much nicer than having to write `fooDate` when you're operating on Dates, and `fooDatetime` when you're operating on Datetimes. When you're writing your business logic, you don't care about what type every object is. If something doesn't implement `foo` yet, you'll fill it in.

In fact, using procedure overloading you can accomplish much the same flow. To adopt a slightly simpler example, we'll use the classic barnyard example:

```py
class Cow:
    def speak(self):
        print "moo"

class Chicken:
    def speak(self):
        print "bok bok"

class Farmer:
    """
    TODO: implement `speak`
    """
```

You know you can do nice things like write

```py
...
denizen = get_barnyard_denizen()
denizen.speak()
```

And if `denizen` happens to be a `Cow`, you'll get "moo", and if it happens to be a `Chicken`, you'll get "bok bok". Nice. And if it happens to be a `Farmer`, you'll blow up at runtime. Not so nice.

What's nice is that with procedure overloading you can write Nim code like this

```nimrod
type
  Cow = object
  Chicken = object
  Farmer = object

proc speak(c: Cow) =
    echo "moo"

proc speak(c: Chicken) =
    echo "bok bok"
```

And the message-like semantic concentration of `speak` is preserved. And with Nim's *unified call syntax*, we can express `speak(c)` as `c.speak()` to preserve method-style invocation. And what else is nice is that if we have some `Farmer` `f`, and call `speak(f)`, we will at compile-time know that `speak` is not implemented for `Farmer`:

```
scratch.nim(13, 2) Error: type mismatch: got (Farmer)
but expected one of:
proc speak(c: Cow)
proc speak(c: Chicken)
```

## With Great Power

What you *can't* do in Nim is implement `get_barnyard_denizen`[^nottrue]. Because the return type of `get_barnyard_denizen` is going to be something like `Cow | Chicken | Farmer`, and that doesn't compile.

[^nottrue]: This is not true. You can do pretty much anything in Nim; there are simple forms of [type inheritance][inheritance] to let you create simple object hierarchies. You can use [generics][] to write procedures that work transparently on multiple types, even types that haven't been defined by the time you define your procedure. And finally, you can bypass the type checker entirely with [templates][] and [macros][]. So you can write pretty much anything. However, for the programmer beginning with Nim, for the programmer beginning with statically typed languages, and just for most simple applications, it's very useful to understand the proper structure of a typed program.

[inheritance]: https://nim-lang.org/docs/tut2.html#object-oriented-programming
[generics]: https://nim-lang.org/docs/tut2.html#generics
[templates]: https://nim-lang.org/docs/tut2.html#templates
[macros]: https://nim-lang.org/docs/tut2.html#macros

So that's a limitation. That is probably going to be your first indication that *types are not classes*. In fact, if you have a function in your program that *might* return a `Cow`, and *might* return a `Chicken`, depending on some factor not known at compile time (the contents of a text file, maybe, or a random number), then maybe they actually *are* the *same type*. Maybe when you boil them down to the actual structure of their data, they're the same; maybe it's just the behavior that differs. In Python we would use a class to implement that, but we don't need to do that in Nim. For instance, we might restructure the program like this:

```nimrod
import random

type
  DenizenKind = enum
    chicken, cow, farmer
  BarnyardDenizen = object
    kind: DenizenKind

proc getBarnyardDenizen(): BarnyardDenizen =
  let newKind = random(0..3).DenizenKind
  result = BarnyardDenizen(kind: newKind)

proc speak(bd: BarnyardDenizen) =
  case bd.kind:
    of cow:
      echo "moo"
    of chicken:
      echo "bok bok"
    else:
      echo "we haven't figured this out yet"

randomize()
let bd = getBarnyardDenizen()
bd.speak()
```

Now we have one type, and what needs to be determined at runtime---the kind of our denizen---is a value, accessible at runtime. Meanwhile, we've still managed to isolate and control this now-runtime-dependent value in a type-safe way using an enum. In this particular case, our need for polymorphism has gone away entirely.

Of course, as our program increases and we need to do many different things with these different types of denizens, we'll probably want to make use of some of the techniques available at [^nottrue] to keep our code manageable and expressive.

## The Future is Bright

It's clear that writing in Nim requires a keener understanding of the structure and relationships between the data in our programs. At the same time, I strongly believe that writing in it will be a much more pleasant experience than the statically-typed, compiled status quo for any programmer used to the expressiveness and freedom of writing in Python or other dynamically typed, interpreted languages.
