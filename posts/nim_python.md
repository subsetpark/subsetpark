title: Nim for Python Programmers
date: 2017-02-19
status: post

I've been spending a lot of time with the [Nim][] programming language recently. It's a great language: its principles, roughly, are to achieve the efficiency and speed of C with a much greater level of safety and expressiveness. It also so happens to take a lot of its surface-level syntactic cues from Python, including code structuring through significant whitespace and colons---that plus its emphasis on expressiveness make it very appealing and inviting to a Python programmer (like myself) who wants greater type safety and greater speed in their code.

[Nim]: http://nim-lang.org

The [Nim for Python Programmers][nim-python] page on the [Nim Wiki][nim-wiki] has a useful overview of some of the most salient differences between Nim and Python for those just diving in. After all, it's *not* actually just Python With Types; it's more like C With All The Features That Make Python So Pleasant (and some more besides). Things like homogeneous arrays/lists, significantly reduced emphasis on object-oriented design, static typing, strings vs. chars---all of those will crop up if you sit down and start typing Python into `foo.nim`.

[nim-python]: https://github.com/nim-lang/Nim/wiki/Nim-for-Python-Programmers
[nim-wiki]: https://github.com/nim-lang/Nim/wiki

I want to spend a little time, however, thinking about some of the more subtle differences between Nim and Python: the things that will crop up after you've learned the Nim syntax but are still writing the language with a Pythonic frame of mind.

# Types, Classes, and Types

Something that Python programmers will do now and then (and probably more than we think we do) is *runtime type inspection*, or *type introspection*. In other words, we will examine the type of an object in our code and make decisions based on that type. We might have code like this:

```py
def time_travel(d):
    if isinstance(d, date):
        d = datetime.combine(d, time(0))
    # keep going...
```

That is, when we enter `time_travel` we don't actually know *what* `d` is, so we find out what it is as a part of our runtime logic and take some action based on what we find out. In this case we are performing some operation on `d` that expects a datetime (maybe it relies on a method on the `datetime` class), but (hopefully!) we don't actually care what time of day it contains, so we create a dummy datetime. But we could also raise an error, log something, or even take a completely different action.

In a way it's kind of the shadow side to [duck typing][duck]. Under duck typing, we try not to care *what* the type is of any object; we just expect it to have the methods we care about. But sometimes we have to care; then we can check out the type or class of an object in the middle of a function and make a decision at runtime.

[duck]: https://en.wikipedia.org/wiki/Duck_typing

This kind of behavior, to the Nim compiler, is nonsensical. In Nim and many of its statically typed brethren, *a type isn't a type if you don't know it at compile time*. That is, in a language like Nim, you as the programmer already need to know exactly what `d` is when you're writing `time_travel`. After all, you need to tell the compiler[^adt]:

```nimrod
proc timeTravel(d: Datetime)
```

[^adt]: In this particular example, the Nim `times` module doesn't distinguish between Dates and Datetimes; there's just Time, which has all the precision of Datetime. From a Python perspective this seems restrictive; however, after a little while you might realize that the reason you need both `date` and `datetime` in Python is because they're not types, but *classes*---they implement useful and sometimes mutually exclusive methods. When you boil away the methods and you're left with just the data and its structure, you don't need nearly as many types in Nim as you need classes in Python.

## You Can't Do That

This is what we mean by *type safety*. The part of type safety that we all can get behind and appreciate is that the language won't let us do something we obviously didn't mean to do, like call `time_travel(28)` or `time_travel("bacon")`. But what can we do if we want to accept *either* a date *or* a datetime?

Well, the first answer is that as you write more type-safe code, you will find that this doesn't actually crop up nearly as often as it does in a language like Python. Odds are that when I wrote the Python function above, it came out like that because I wrote a function that took a datetime `time_travel`, and then wrote some logic that called it with a datetime:

```py
today = utcnow()
time_travel(today)
```

And then a while later, I had some other totally different logic that called that same function:

```py
requested_date = date(requested_year, requested_month, requested_day)
time_travel(requested_date)
```

And it called it with a date, which caused an exception (at runtime). Whoops. I might not have even noticed---I'm sure there are lots of places where I do this exact same thing and it *doesn't* crash, because you can do an awful lot of the same things with dates that you can do with datetimes. So I added the line to just do the right thing and convert it and not crash.

But the only reason I wrote the second chunk of logic in the way that I did is because I didn't remember and didn't particularly care that my first function actually worked on datetimes, not dates! 

And in fact, it's just as possible that I had *both* of those call sites looking just like they do above, and it was the original function `time_travel` that I changed. Maybe the first version of the function only called the attributes of `d` that are shared in common between dates and datetimes, but then I added some functionality that only worked for datetimes. Now suddenly half of my invocations are broken. Let's hope they were all tested.

If I had been writing in a more type-safe language, the compiler would have yelled at me whenever I called `time_travel` on a `date` (whether or not `time_travel` relied on any methods that only exist for datetimes). This means we don't have to roll the dice when making the above sorts of changes. I'd always know what sorts of data were being passed into the function and what it relied on, even as those things changed over time.

## You Can Do Cool Stuff

So, what are we really trying to do, with all this type inspection that's suddenly off-limits? Inspecting the type/class of an object at runtime is always the means to some end; what is it accomplishing for us? 

It comes down to a question of *polymorphism*: how do we implement the same or similar behaviors for different data types? 

I say "similar"---after all, in `time_travel` above the behavior is not actually the same for both types. It's a little more like this:

```py

class MyDatetime:

    def time_travel(self, time=None):
        """
        Do cool stuff here
        """
        time = time or self.time()
        ...

class MyDate(MyDatetime):

    def time_travel(self):
        """
        Construct a datetime and then do cool stuff
        """
        dummy_time = time(0)
        super().time_travel(time=dummy_time)
```

That is, the behavior of `time_travel` when `d` is a `date` is a superset of the behavior when `d` is a `datetime`. We could argue the merits of Python's thoroughly class-based, object-oriented approach, but it's not available in Nim. 

Luckily, Nim has a trick that Python doesn't have, which makes up for quite a bit of where we'd use classes for polymorphism: [procedure overloading](https://en.wikipedia.org/wiki/Function_overloading). You must declare the types of all procedure parameters in Nim; however, you are also able to declare multiple procedures with the same name (and even arity), with different parameter types.

```nimrod
proc timeTravel(d: Datetime) = 
  echo "This is one great procedure!"

proc timeTravel(d: Date) =
  echo "I'm getting there..."
  d.toDatetime().timeTravel()
```

In Python, our second `timeTravel` would clobber the first; in Nim, we have actually defined two separate procedures and the compiler will resolve procedure calls to the right one based on the number and type of the arguments given at the call site.

What this means is that you can achieve some of the semantics of OO style through totally different (and arguably much simpler) means. 

One of the really elegant things about the OO message-passing style is that the messages you pass to objects can have really powerful, unified semantics; you're essentially saying, "in my domain, `timeTravel` has meaning. It's up to everybody to implement what `timeTravel` means to them." This is often much nicer than having to write `timeTravelDate` when you're operating on Dates, and `timeTravelDatetime` when you're operating on Datetimes. When you're writing your business logic, you don't care about what type every object is. If something doesn't implement `timeTravel` yet, you'll fill it in.

In fact, using procedure overloading you can accomplish much the same flow. To adopt a slightly simpler example, we'll use the classic barnyard scenario:

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
    raise NotImplementedError()
```

You know you can do nice things like write

```py
...
denizen = randomly_get_barnyard_denizen()
denizen.speak()
```

And if `denizen` happens to be a `Cow`, you'll see "moo", and if it happens to be a `Chicken`, you'll see "bok bok". Nice. (And if it happens to be a `Farmer`, you'll blow up at runtime[^notimplemented]. Not so nice.)

[^notimplemented]: Alternately, if you didn't raise inside Farmer.speak(), you would just have a subtle runtime bug where one third of your calls fails silently (literally).

The good news is that with procedure overloading you can write Nim code like this

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

What you *can't* do in Nim is implement `randomly_get_barnyard_denizen`. Because the return type of `randomly_get_barnyard_denizen` is going to be something like `Cow | Chicken | Farmer`, and that doesn't compile.

```nimrod
proc randomlyGetBarnyardDenizen(): Farmer | Chicken | Cow =
  let randomChoice = random(2)
  case randomChoice:
    of 0:
      result = Farmer()
    of 1:
      result = Chicken()
    else:
      result = Cow()

randomize()

discard randomlyGetBarnyardDenizen()
```

Gets you:

```
scratch.nim(14, 14) Error: type mismatch: got (Chicken) but expected 'Farmer = object'
```

So that's a limitation. That is probably going to be your first indication that *types are not classes*. In fact, if you have a function in your program that *might* return a `Cow`, and *might* return a `Chicken`, depending on some factor not known at compile time (the contents of a text file, maybe, or a random number), then maybe they are actually the *same type*. Maybe when you boil them down to the actual structure of their data, and where they're used, they're the same; maybe it's just the behavior that differs. In Python we would use a class to implement this, but we don't need to do that in Nim. 

In Python we are used to setting up a bunch of classes in order to concretize our domain; if we have three types of thing (three "nouns"), we'll make three classes. Type-safe programming asks us to think a little more rigorously about the ontology of our application. It's not just a matter of laying out the objects that we think make up our domain; do our procedures reflect that ontology as well?

With this in mind, we might decide to use a simple `enum` to capture the variations amongst the different different barnyard denizens, and restructure the program like this:

```nimrod
import random

type
  DenizenKind = enum
    chicken, cow, farmer
  BarnyardDenizen = object
    kind: DenizenKind

proc randomlyGetBarnyardDenizen(): BarnyardDenizen =
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
let bd = randomlyGetBarnyardDenizen()
bd.speak()
```

Now we have one type, and what needs to be determined at runtime---the kind of our denizen---is a value, accessible at runtime. Meanwhile, we've still managed to isolate and control this now-runtime-dependent value in a type-safe way using an enum. In this particular case, our need for polymorphism has gone away entirely.

Of course, as our program increases in complexity and we need to do many different things with these different types of denizens, we'll probably want to make use of the more advanced features provided by the language when it comes to polymorphism and expressiveness; [inheritance][], [generics][], [templates][], and [macros][] all provide different and more complex ways to implement powerful, polymorphic behavior.

[inheritance]: https://nim-lang.org/docs/tut2.html#object-oriented-programming
[generics]: https://nim-lang.org/docs/tut2.html#generics
[templates]: https://nim-lang.org/docs/tut2.html#templates
[macros]: https://nim-lang.org/docs/tut2.html#macros

## The Future is Bright

It's clear that writing in Nim requires a keener understanding of the structure and relationships between the data in our programs. At the same time, I strongly believe that writing in it will be a much more pleasant experience than the statically-typed, compiled status quo for any programmer used to the expressiveness and freedom of writing in Python or other dynamically typed, interpreted languages.
