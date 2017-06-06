title: Compile-Time Sort in Nim
date: 2017-06-06
status: post

## Compile-Time Sort in D

Michael Parker recently wrote a blog post showing how to implement a [compile-time quicksort in D][D] after Björn Fahller recently wrote a blog post showing how to implement a [compile-time quicksort in C++17][C]. It's a skillful demonstration that employs D's powerful compile-time programming capabilities to write code that, while not always useful, is quite concise.

[D]: https://dlang.org/blog/2017/06/05/compile-time-sort-in-d/
[C]: http://playfulprogramming.blogspot.kr/2017/06/constexpr-quicksort-in-c17.html

Reading it, of course, the thought that came to my mind was "Surely Nim can do better."[^1]

First let's take a quick look at Michael's D code:

```D
void main() {
    import std.algorithm, std.stdio;
    enum a = [ 3, 1, 2, 4, 0 ];
    static b = sort(a);
    writeln(b); // [0, 1, 2, 3, 4]
}
```

I won't rehash his whole post, which is lucid and pleasurable to read, but here's the basic gist: we declare an *enum*, which is a special kind of constant[^2] only available at compile time. The call to `sort` is prepended by the `static` keyword, which binds the result of a compile-time evaluation to a static variable available at runtime. The call to `writeln` is runtime-only, and thus happens whenever `main()` is called.

[^1]: Not really. It was more like, *that's super cool! Can Nim do that?*

[^2]: It turns out that in D, if you declare an enum with only one value (in this case a list), it is declared to be a *manifest constant*. That seems to me like a slightly strange way of getting a compile-time-only value, but I'm not a D programmer.

This is a nicely modern approach, bereft of boilerplate, fairly easy to understand, and allows the programmer to execute arbitrary code at compile-time. Michael gives a nice example of some complicated mathy code that is used to validate numeric parameters both at runtime and compile time. It does have a few constraints, though: 

> The fundamental requirements for CTFE eligibility are that a function must be portable, free of side effects, contain no inline assembly, and the source code must be available. 

## Compile-Time Sort in Nim

Now let's look at an equivalent program written in Nim:

```nimrod
import algorithm

static:
  var a = @[3, 1, 2, 4, 0]
  sort(a, system.cmp)

const b = a
echo b
```

You can probably see that the structure is very similar. There are a couple subtle differences, so we'll go through it. The first difference is that the first two statements, equivalent to the first two statements in the D code are evaluated in a `static` block. There's nothing about the statements themselves specific to compile-time execution; rather it's the context provided by their scope that indicates it's happening at compile time. Now that we're outside the `static` block, we're back in the runtime; we bind `b` outside of the static context with a `const`[^3], and echo during execution of the program itself.

[^3]: The `const` keyword behaves very similarly to D's manifest constants; usages in the compiled C code are replaced directly by the assigned value.

Really, the only differences here are syntactic; I prefer Nim's designation of execution context with blocks rather than the somewhat more occult special-casing of single-value `enum`s, but they have equal expressive power. But let's look at what the Nim docs say about `static`:

> A static statement/expression can be used to enforce compile time evaluation explicitly. Enforced compile time evaluation can even evaluate code that has side effects[.]

Whoa! That's a difference. Unlike in the D example, Nim static statements *can* perform IO. So that means we could write:

```nimrod
import algorithm

static:
  var a = @[3, 1, 2, 4, 0]
  sort(a, system.cmp)
  echo a

const b = a
echo b
```

And the constant is echoed once during compilation, then again whenever you run the application.

## Compile-Time Side Effects

What this means is that in addition to computationally intensive code, we can perform other useful operations at compile time. Here's a reformatted [example from the Nim forums][forums] that demonstrates effectful code during compilation:

```nimrod
import strutils, tables

static:
  let configLines = "configfile.ini".slurp().splitLines()

  var keyValues = newSeq[(string, string)]()

  for line in configLines:
    if line.len > 0 and line[0] != ';':
      let splitLine = line.split('=')
      keyValues.add((splitLine[0].strip().toLowerAscii(), splitLine[1].strip()))

const config = keyValues.toTable()
```

[forums]: https://forum.nim-lang.org/t/2708

Here, we statically read a config file during compilation and build a table out of it, which we can then bind into the runtime scope as a constant available at runtime. This is a neat trick, allowing us to easily parameterize builds (for instance).
