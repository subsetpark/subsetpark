Title: On the Appeal of Lisp
Date: 2020-05-17
status: draft

I've started playing around with a new language, the first time in a
while: [Janet][], a Lisp-alike whose syntax and standard library is most
directly influenced, it seems to me, by Clojure, but is written in C and
starts up a hell of a lot more quickly---lending it well to a niche
that I've idly liked to fill for a while: that of writing performant,
small utilities for the command line (if I wanted to write Lisp on
a server, I presumably would just write Clojure directly, where the
startup time of the JVM is not really a problem).

[Janet]: https://janet-lang.org/

I say "Lisp-alike" because---appearances very much aside, as Janet looks like this:

```
(use joy)

(defn home [request]
  (text/plain "You found joy!"))

  (def routes [[:get "/" home]])

  (def handler (handler routes))

  (server handler 8000)
```

and I think most people would say: parentheses, `def/defn`, prefix
notation, it's a Lisp, let's go home---there is a very vocal contingent
*around* that will go to extreme pains to point out that
**Janet is not a Lisp**. *qv*:

> I never thought I'd see a Lisp without lists. Oh wait, I didn't because
> this isn't a Lisp.

and

> Lisp is a specific implementation of symbolic and list processing
> with specific shapes of data structures, names of functions and their
> semantics, treatment of Boolean conditions, syntax, and everything else.

I think some of the elements of the latter list of criteria are
potentially misleading, as Janet *does* have those, so let's focus on
what it conspicuously doesn't have: lists.

Not "lists" in the generic sense of "ordered primitive data structures",
but "lists" in the specific sense of "linked lists". Janet hasn't got
them; its two generic sequential data structures, the *array* and the
*tuple*, are based on C arrays, can be O(1) indexed into rather than
processed as pairs of cons cells. And because they're *not* implemented
as pairs of cons cells, the `car`/`cdr` set of functions is not available,
and the paradigm of handling the head of a list and recursing on its
tail is not the default way to handle data structures.

I will not spend too much time dwelling on this distinction, or how
asinine I find this kind of terminological essentialism. But it *has*
got me thinking about the eternal mystic appeal of Lisps. For a long
time now I've wanted to sharpen my Lisp (or S-expression handling,
or quote-unquote-quasiquoting) sensibilities. I've always half-hoped for
the opportunity to write LFE or Clojure in production, and I've played
on my own time with a half-dozen Scheme implementations in the past.

What's the appeal of Lisp? If Janet appeals to me, it's clearly *not*
cons cells and linked lists. And it's not *exactly* macros, or just *any*
macros, as I write syntactic macros all day long in Elixir and while I
appreciate them for what they give me, they don't seem to have scratched
the itch that has me peeking over the fence at the world of Lisp.

Let's consider Elixir at greater length, as Elixir macros and Lisp macros
might be closest to a minimal pair within my own
experience[^nim_macros]. Elixir's macro system is quite powerful,
providing hygienic syntactic macros which can be leveraged for
common-or-garden code generation up to full-blown DSLs.

[^nim_macros]: I spent a good while writing Nim, but: neither was it as long as
  I've written Elixir, nor did
  [macros](https://nim-by-example.github.io/macros/) or their cousins,
  templates, play nearly the same role in my use of the language as
  they do in Elixir and they do in the hands of a seasoned [Let Over
  Lambda][lol]er. Plus, you can do more functional programming in Lisp
  (and *only* functional programming in Elixir).

[lol]: https://letoverlambda.com/

As an example, we can easily get the AST of a function definition (here
the trivial identity function) as an AST data structure, expressed as
an ordinary Elixir data structure:

```ex
iex(1)> quote do
...(1)>   def id(x), do: x
...(1)> end
{:def, [context: Elixir, import: Kernel],
 [{:id, [context: Elixir], [{:x, [], Elixir}]}, [do: {:x, [], Elixir}]]}
```

We can operate on this data structure however we like at compile time,
renaming things, replacing its contents, *et cetera*. And indeed macros
are quite common (relatively speaking) in Elixir; the `use` directive
which is found at the top of many modules calls its target module's
`__using__` macro, which can inject arbitrary code into the calling
module: features as simple as importing a few other modules, saving you a
couple lines of code, to providing default implementations of functions,
registering attributes, and more.

Inside of a `quote` block we can pretty easily interpolate values from
the environment, with `unquote`:

```ex
iex(1)> y = :ok
:ok
iex(2)> quote do
...(2)>   def it(_x), do: unquote(y)
...(2)> end
{:def, [context: Elixir, import: Kernel],
 [{:it, [context: Elixir], [{:_x, [], Elixir}]}, [do: :ok]]}
```

And yet there is something different between this and its Janet equivalent:

```clj
janet:1:> (def y :ok)
:ok
janet:2:> ~(defn id [_x] ,y)
(defn id [_x] :ok)
```

It's not just that the operation is easier, in the sense of requiring
less extra typing. While functionally equivalent, the syntax of Elixir
is almost infinitely more complex than that of a Lisp (any Lisp), in
the sense that there *is* some. And this shows up in the return values
of each quote operation: Elixir's looks quite different indeed the code
that was written to produce it, whereas Janet's is as close as possible.

No duh, says the reader, you have just discovered the principle of
[homoiconicity][]. Not quite, though. To be homoiconic a language
must represent its AST in data structures primitive to the language
itself. Elixir manifestly does this; an Elixir AST node is an Elixir
tuple with three elements. There is a difference, which might be strictly
perceptual, but since the process I'm interested in is the one happening
in my own mind, you'll forgive me if I treat it as material. There is
a thing that happens (or promises to happen, or seems like it would
happen) when the syntax of a language is sufficiently minimal that the
language *as written by humans* is almost visually indistinguishable
from the representation of the data structures that encode an instance
of that syntax.

[homoiconicity]: https://en.wikipedia.org/wiki/Homoiconicity

There's another trick here: Lisp has *symbols*. These are truly mysterious
and have even less of an analogue in other languages that I've used.

Let's return to the Elixir example above. Modulo some reader sugar,
every alphabetical object in that AST is encoded as an *atom*, one of the
core data types. Atoms are lovely and every language should have them,
or something like them: human-readable constants whose only value is
their identity. Great for pattern-matching against. Individisble (as
the name suggests).

Janet has them too; there they're called *keywords*, and they have the
same semantics. They happen to be represented the same way, as a colon
followed by a string of characters, as in `:ok` above. But evaluation #2
of the janet interpreter demonstrates a distinction between two primitive
types: `:ok` is a keyword, but the other alphabetical elements don't have
any colons in front of them because they aren't keywords, they're symbols.

In Janet, as, I would guess, in any other Lisp, the *type* of the
unevaluated words of the source code is *symbol*. Is that even expressible
in most other languages? What is the *type* of `def id(x), do: x`? I can
tell you the type of the AST that you get when it's evaluated as source
code. And I can tell you what it *does* when that AST is evaluated by the
Elixir compiler. On the other hand, I can tell you exactly what the type
of `(use joy)` is, in Janet: it's a tuple of two symbols[^tuple-not-list].

[^tuple-not-list]: Of course, in a proper Lisp, it would be a list,
not a tuple. Let us never forget.

Is this a meaningful distinction? Is it really more meaningful to talk
about the actual text `(use joy)` as though it were data? Or is it a
category mistake brought on by the fact that the Janet parser happens to
use that text as both a meaningful input as well as the representation
of its output?

Parsing is only one step in the journey, of course. Symbols don't
only have a textual representation; they also have an evaluation. And
in reality, keywords are very similar to symbols. Sometimes you can
just say that a keyword is a special kind of symbol that starts with a
colon. But keywords evaluate to *themselves*, whereas symbols evaluate
to... *something else*, at least by default.

So what? So do references in C, and I don't find myself gravitating
towards C in search of mystical experiences.

We're getting kind of swimmy here. And implementation-specific. The
point is that these questions are *much closer to the surface* in Lisp
than in Elixir or any other macro-ful language I've used. The presence
of symbols in the primitive inventory of this language induces in the
programmer the sensation of being at all times suspended in an infinite
cycle of evaluation, which can always be trivially advanced some number
of steps.

## A caveat

I want to stress at this point that I haven't written any of the above
in an attempt to demonstrate why Janet in particular or Lisp in general
is *better* than Elixir, or even why Lisp macros are *better* than
Elixir macros. I love Elixir and never find myself cursing the distance
imposed between me and its AST by a more complex syntax and the absence
of symbols as a first-class data type.

There is, of course, a [school](https://letoverlambda.com/) that quite
sincerely believes that

> Macros are what make lisp the greatest programming language in the world.

If I am on my way there, I certainly haven't been convinced yet. It's
quite important, whenever waxing philosophical about the sublime pleasures
of metaprogrammic, to draw a clear distinction between the utility of
such a thing and the way it makes your brain feel. Indeed, when I consider
that the standard pitch for the *superiority* of macros is that it lets
you write languages to solve your problem instead of programs, I get
awfully worried. I quite like being able to reason through code that I
haven't written, and a good first step towards that goal is everybody
settling on writing their code in roughly the same codebase.
