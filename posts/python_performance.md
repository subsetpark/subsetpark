title: Disincentivizing Performance in Python
date: 2018-04-22
status: draft

Here's an example Python class:

```py
class O:
    def __init__(self):
        self.x = random.randint(0, 100)
        self.y = random.randint(0, 100)
        self.z = random.randint(0, 100)
        self.should_foo = random.choice([True, False])
```

It does very little but randomly populate itself with a few values. Here's
some code that does some trivial computation with that class:

```py
Os = [O() for _ in range(10000)]

def as_object():
    acc = 0
    acc2 = 0

    for o in Os:
        val = (o.x + o.y) * o.z
        acc += val
        if o.should_foo:
            acc2 += val
```

It instantiates a bunch of objects, then iterates over them, doing some
trivial arithmetic with their values and totting them up. Aside from the
direly uninformative variable names, this is straightforward, idiomatic
Python code.

We can benchmark it:

```py
def bench(f):
    time = timeit.timeit(
        "{}()".format(f),
        setup="from __main__ import {}".format(f),
        number=10000)

    print("{}: {}".format(f, time))
```

```
[i] >>> bench("as_object")
as_object: 18.172908133999954
[i] >>> bench("as_object")
as_object: 18.14020858299955
[i] >>> bench("as_object")
as_object: 18.176712284999667
```

This is not very fast. There's lots of reasons for that: Python is
dynamic, interpreted language, and thus can take advantage of no
compile-time optimizations or shortcuts; moreover, as compile-time
languages go, it is highly metaprogrammable, and thus the interpreter
has to go around inspecting classes for methods and the like every time
they are called, because class/object attributes and magic methods can
be monkey-patched (or worse!) at any time.

In other words, there's a lot of message-passing (in the original OO
sense) overhead involved on top of the work of adding things up. There
are some tricks we can pull to reduce some of that overhead: basically,
there are ways we can *reduce* the flexibility of the Python runtime in
order to save time or space.

## Python performance hacks

One of those tricks is the use of `__slots__`, a somewhat obscure class
variable that can be instantiated on any class that we declare. It looks
like this:

```py
class O2:
    __slots__ = ('x', 'y', 'z', 'should_foo')

    def __init__(self):
        self.x = random.randint(0, 100)
        self.y = random.randint(0, 100)
        self.z = random.randint(0, 100)
        self.should_foo = random.choice([True, False])
```

What is going on here? We are short-circuiting the normal Python way of
object attribute lookup.

### Object dictionaries

The [Python data model reference][model_ref] says:

> The default behavior for attribute access is to get, set, or delete the attribute from an object’s dictionary. For instance, `a.x` has a lookup chain starting with `a.__dict__['x']`, then `type(a).__dict__['x']`, and continuing through the base classes of `type(a)` excluding metaclasses.

We can see what they mean:

```
[i] >>> o = O()
[i] >>> o.new_var = True
[i] >>> o.__dict__
{'x': 29, 'y': 90, 'z': 77, 'should_foo': True, 'new_var': True}
[i] >>> type(o).__dict__
mappingproxy({'__module__': 'bench', '__init__': <function O.__init__ at 0x7f459b3bc510>, '__dict__': <attribute '__dict__' of 'O' objects>, '__weakref__': <attribute '__weakref__' of 'O' objects>, '__doc__': None})
```

When we instantiate one of our objects, it has a `__dict__` attribute
which is bound to a normal Python dictionary. Inside of this dictionary
are what we think of as the *instance variables* for that object. Anything
that is bound to `self` inside one of the object's methods, or anything
that is bound to a new variable on that object, is inserted into the
dictionary at `__dict__`. Attribute lookup is then simply a matter
of looking up a key inside that object's dictionary. As we can see,
the object's class has its own dictionary, that works the same way,
and will be checked if the key can't be found on the object.[^1]

[^1]: It's for reasons like this that one understands the [amazing
lengths][dicts] to which the Python team has gone to optimize dictionary
lookups.

[model_ref]: https://docs.python.org/3.6/reference/datamodel.html
[dicts]: https://www.youtube.com/watch?v=p33CVV29OG8

### `__slots___`

Dictionary lookups in Python 3.6 are pretty fast, but that's still a lot
of work for something so elemental as object attributes. Especially if
we know ahead of time that we don't intend, perhaps, to add or delete
object attributes dynamically, we can use `__slots__` to bypass the
creation of a `__dict__` altogether.

```
[i] >>> o2 = O2()
[i] >>> o2.__dict__
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'O2' object has no attribute '__dict__'
```

Given a list `O2s` just like our `Os` above, but made out of this new
class, we can do the same operation and compare the result:

```
[i] >>> bench('as_optimized_object')
as_optimized_object: 15.154766511999696
[i] >>> bench('as_optimized_object')
as_optimized_object: 15.482156292999207
[i] >>> bench('as_optimized_object')
as_optimized_object: 15.32934930599913
```

A fairly substantial savings---especially considering that `Os` and `O2s`
were instantiated before our benchmark, and therefore the savings gained
during object creation aren't factored in here.

Unfortunately, this improved efficiency comes at a cost: in addition
to the list of usage details as long as my arm in the model reference,
the use of `__slots__` will entail behavior like this:

```
[i] >>> o2.new_var = True
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'O2' object has no attribute 'new_var'
```

Which is thoroughly confusing to anybody not acquainted with the implementation of the class (and the use and purpose of `__slots__`).

However. 15 seconds is still an eternity in computing terms. And a brief perusal of the data model reference above might give us the inkling that there is more fat to cut, if we should be so called.[^2]

[^2]: This is not a meaningless parenthetical. This entire post must be understood in the context---emphasized and reemphasized, and not wrongly, by Python programmers---of the real-world requirements and constraints of one's program. Often, especially when world-class retardants like the internet and databases are involved, the performance of the Python interpreter will be a drop in the bucket.

### Bare tuples

Even after we make use of `__slots__`, there are still many hoops that the language must jump through, on account of its flexibility and expressiveness, to do the right thing. Even when, in a simple case like ours, the right thing seems obvious. One thing, would be to abandon the use of objects altogether.[^3] Here's an example that removes object attribute lookup by using a simpler data structure.

[^3]: In Python, of course, tuples are also objects.

```py
def new_t():
    return (
        random.randint(0, 100),
        random.randint(0, 100),
        random.randint(0, 100),
        random.choice([True, False])
    )


Ts = [new_t() for _ in range(10000)]

def as_tuple():
    acc = 0
    acc2 = 0

    for a in As:
        val = (a[0] + a[1]) * a[2]
        acc += val
        if a[3]:
            acc2 += val
```

It's the same exact logic, except we just use a tuple of four elements to represent our object. Now we just have to remember that the order of the attributes is: (`x`, `y`, `z`, `should_foo`).

When benchmarked, we see a further improvement:

```
[i] >>> bench('as_tuple')
as_tuple: 13.292414839000502
[i] >>> bench('as_tuple')
as_tuple: 13.429743416149721
[i] >>> bench('as_tuple')
as_tuple: 13.433415019850962
```

Though at this point our code is anything but idiomatic. Our `new_t()` function gives us no clues as to the meaning of the values we generate, and `as_tuple()` is harder to read---tuple indices instead of attribute names giving us equally little help in understanding the purpose of the function---and harder to edit or extend---it will be easy to forget that mapping of attributes to indices and make a mistake.

## Expressiveness vs. performance

In programming languages there is a generalized trade-off between *expressiveness*, which I'll very loosely define as "the ability to write code that conforms to the programmer's line of reasoning (as opposed to the dictates of the language or architecture)", and *performance*. *High-level* languages, like Python, are ones that rely on abstractions and flexibility to favor the former over the latter.

There is, however, a lateral move that disrupts this dichotomy: compilation.

The addition of a compilation pass between writing and executing code opens the door for theoretically unlimited code transformations. In practice, of course, compilers are not omniscient, and need help. When it comes to performance, they need help with being able to make assumptions about the code as it is written: the types of variables when they're evaluated; what code will be evaluated when a function is called or a condition is met. The more of this that a language requires to be nailed down at compile time, the more amenable it is to optimization, and the more the programmer can rely on the compiler to generate performant code without his or her having to write it.
