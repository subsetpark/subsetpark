Title: _ Date: 2018-10-21 status: post

Friends, I am at least in this respect a rare breed: an Elixir programmer
who has never written Ruby.

In fact, I'm one of the few people I know who had an interest in
Erlang before he had heard of Elixir. Its cussedness and eccentricity
has always appealed to me: it is purely functional, Prolog-syntaxed
and singly-assigned, among other things, so it has a deep flavor of
obscurity and impenetrability. But in fact it's allergic to academia; it's
dynamically typed, of all things, and it's been around and in production
use for unfashionably long. It's true that its syntax is unusual; it's
also true that I've always found it—and I say this without irony or
hipster self-consciousness—particularly elegant and terse, to the
extent that it's often been my whiteboarding pseudocode syntax when I
had the choice of weapons.

I was never a professional Erlang programmer; I cut my teeth on Python
and began working in Python, but Erlang was the second language I learned
after that and the one that I looked for excuses to write in (in addition
to Nim, which would come a little later, but which I would also be able
to finagle into production at MakeSpace).

So here was my impression of Elixir for the first several years of my
passing acquaintance with it: it's Erlang with Ruby syntax. Here's how
it goes:

1. For better or worse, the first thing that most people mention when
Erlang comes up is that the syntax is unfamiliar and confusing.

2. Ruby is one of the most popular programming languages on the web,
and in particular became the gold standard for programmer-friendliness
in languages, of which syntax is one of the most salient facets.

3. At some point somebody realized that the guts and semantics of Ruby
are underdeveloped and underconsidered, and that the promise of OOP
in all things delivered by it and its cousins (Python being the most
obvious one) has significant disadvantages, especially later in the game,
and therefore decided that all Erlang, whose BEAM and OTP have decades
of demonstrated advantage at running web-like systems, needs to be the
web language of the future is a syntax like the one we know and love in
Ruby. Thus Elixir was born.

So I, on the other hand, am the only programmer under the age of 35 who
actually *likes* Erlang's syntax, and I already appreciate the BEAM
and OTP. Elixir is not worth bothering with, and given that it's an
additional layer of complication on top of Erlang it's strictly worse
than writing in vanilla Erlang.

I even had the misfortune to be sure enough of myself to express
this opinion in public—not just in a public forum, but one
frequented by José Valim, preternaturally patient BDFL of the Elixir
language. Someone had posted something along the lines of "what's the
deal with Elixir? Should I learn it instead of Erlang?" and I responded
with something along the lines of, "it's just Erlang with a different
syntax. Learn the real thing."

My memory of José's reply was something along the lines of, "that's
a bit reductionist; there's also macros." I was a little embarrassed,
not only because I didn't know that José was going to see my response,
but because I didn't even know there *were* macros in Elixir.[^zulip]
It's a bit telling that this is my memory, because it's not actually what
José said. I won't quote him directly, but I don't think it's breaking
too many confidences to give a brief rundown of the features that José
mentioned as distinct (aside from macros), as of early 2015, from Erlang:

- lazy collections;
- more robust structured data;
- better tooling, incl. testing frameworks and Mix

This is telling because at the time I thought, "well, sure, macros;
but Erlang has parse transforms (not that I know how to use them)
and macros are weird and shouldn't be used that much anyway." I wasn't
a macro guy and, in fact, still am not; I don't use macros that much
today. On the other hand, the part of his reply that I don't remember,
the part about lazy collections, structs and tooling, I use *constantly*.

[^zulip]: I am now embarrassed all over again, because going back and
reading my early posts in this forum about Erlang I see that not only
did I make an ass of myself up there but this was a couple years after
*José himself* had been the person who was most responsive to my initial
questions about programming in Erlang.

This is the thesis of Elixir the language: it's not Erlang with Ruby
syntax; it's Erlang with a robust and useful standard library and tooling.

It's not a coincidence that my first several impressions of the
language were that it was the same thing with an unnecessary Ruby skin
on top. That's how it's often sold to people. And that's how it's ofen
bought, too; a great number of Elixir programmers, judging from the
blogposts and podcasts, are ex-Rubyists who went looking for something
more robust and scalable. You'd be forgiven if you thought that the
raison d'etre of Elixir was to make a BEAM language that was appealing
to Ruby programmers.

This means that much of the discourse about Elixir has a weird Rubyist
bias; Elixir is very often discussed from a perspective where it is
implicit that Ruby is the baseline, what is familiar, the default. "How
we did it in Ruby" is often the starting point for discussing patterns of
programming in Elixir. If Elixir is our New World, then Ruby is our Old
one, and Elixir is dotted with New Spains and New Amsterdams that refer,
consciously or not, back to it.

But the thing about the Ruby syntax in Elixir is that it is a complete
red herring. It is by far the most superficial and least consequential
element of the language. If you like Ruby syntax, if you dislike Ruby
syntax, it doesn't really matter. You learn it and then you move on. It
disappears almost immediately. This is perhaps the first insight coming
to Elixir from Erlang: the base semantics are almost *identical*. That
is, almost every element of the Erlang language is preserved wholesale;
Elixir consists almost entirely of goodies that are piled on top. This
is very reassuring to the Erlanger. You don't lose any of your intuition
for what makes good code.[^assign-once]

[^assign-once]: The sole exception to this is single assignment, which
Elixir does away with, allowing the same variable to be bound with
multiple values. This introduces some complication in the sense that one
needs to learn how to "pin" variables with `^`, and I often find myself
thinking that the fact that pipes result in so many fewer variables
being bound in the first place means that single-assignment would be
even nicer in Elixir than Erlang; you get the benefits of knowing what
`x` is whenever you see it, plus you have to write `x1 = ...`, `x2 =
...` much less. Once again to his credit, José and the team didn't
implement variable rebinding simply because the alternative was scary
and confusing to Ruby programmers, and [his logic for it][rebinding]
is consistent and reasonable.

[rebinding]: http://blog.plataformatec.com.br/2016/01/comparing-elixir-and-erlang-variables/

The second thing you discover as a reluctant Erlang-to-Elixir convert
is that the standard library *matters*. There's two qualities here, and
again neither one of them is as simple as coddling Rubyists who can't
deal with the quirks of the Erlang standard library (1-based indexing,
inconsistent naming or argument order...).

The first aspect of this is that the specific modules that are pure Elixir
innovations are actually relatively few and clearly well-considered. Big
ones are, as pointed out above, `Stream` and `Struct`. Stream, the lazy
collections module, is lovely precisely because there's essentially
nothing linguistic about it; under the hood one can imagine it's got
all kinds of protocols and macros and other Elixiry things that make
a purist blanch, but it's presented to the user simply as exactly the
sort of all-purpose highly functional library that you could see missing
from the Erlang standard library already. Struct is more baked into the
language itself; There's a Struct syntax based on maps and Structs often
seem to result in the kind of OO-style data+methods object that you
expect out of Python and would never see in Erlang. The tradeoff: you
get a certain amount of compile-time checks of attribute access. Static
guarantees might be the only thing I would be willing to trade away some
of Erlang's datatype minimalism for.

The second aspect is that if you are given the opportunity to greenfield
an entire standard library at once on top of everything you get from
Erlang already, you can implement some pretty elegant features. You
can, for instance, make sure that the "object" of a function is in the
first argument position for every single function, making it possible
to introduce the pipe operator `|>` quite simply.

I fucking love pipes. There are relatively extremely few assignments in
Elixir. Not only because it's functional and immutable, but because at
every turn the programmer is encouraged, syntactically, to think about
their code as a series of data transformations chained together. If the
programmer can do that, they are rewarded with an extremely terse and
expressive, at times scarily close to *point free*, structure where very
little has to be put down that is not directly pertinent to what needs
to be communicated. This makes debugging, rearranging, refactoring more
straightforward. It reduces parenthesis noise. It's a big win. Again,
it's not just that it's less Erlangy for the arbitrary reason that we
distrust the Erlang syntax; it makes it easier to write better code.

The third thing you discover is that, just maybe, some of this new
syntactic sugar is kind of nice. I'll use keyword arguments as an
example. In Elixir you can approximate Python-style positional and named
function arguments (`f(x, y, z=True)`) in syntax: `f x, y, z: true`. This
is exactly the kind of unnecessary imposition of Rubyist tropes that
you're on the lookout for—until you realize that this is actually sugar
over an *extremely* idiomatic Erlangism, the tagged value. `x, y, z: true`
is precisely equal to [x, y, {:z, :true}], which is the existing Erlang
convention for passing optional parameters to a function.

It's this sort of thing that ends up being quite reassuring. Sometimes,
because Elixir has gotten some heat on it, you read overheated blog
posts that talk about Phoenix and microservices in the way that other
people talk about Node or React: that is, with an air of hype and an
uncomplicated assurance that this is the new best way to build web apps
that seems to maybe have come more from reading blog posts than from
suffering through the building of many systems. This is disturbing, and
with good reason: Erlang is a language that is shaped by a deep pragmatism
and formed out of a series of concentric approaches to solving the same
class of problems over decades. It's odd and a little creaky in places
but that's because it has always been shaped by very real needs. And
that is how we got OTP, which is not something to be abandoned lightly.

That's why it's reassuring to see that the creators of this language
understand the Erlang idiom and don't tread too heavily over it. If you
want to run things on the BEAM but decide that what it's really missing
in order to be a world class language is multiple inheritance and object
methods, I'll scream bloody murder. But it is possible to reflect on what
is essential to Erlang and to imagine how we could extend it if we took
it upon ourselves to design several layers of expressiveness on top of it.

Of course, the simplicity and flatness of Erlang isn't a mirage, and I
find myself missing it at times. Elixir has polymorphisms Erlang could
never dream of, chief among them protocols. It also has macros and
DSLs. That means there are many more places where the sacred compact is
broken. There are many more places where you don't know, from reading the
code, what function is being called. You don't know where something is
landing because it depends on whether somewhere else somone has defined
an implementation or not. You don't know what the actual functions and
data structures are because you are in the middle of an expressive and
complicated macro. By the time you factor in tail-call optimization,
the traceback looks nothing like your code. These are Elixir problems.

And Elixir is not yet the citizen of the BEAM it could be. When I was a
distrustful Erlang programmer, there was a sort of Ugly American quality
to Elixir's position in the BEAM ecosystem: it was incredibly convenient
to call Erlang code from Elixir, but it was decidedly nontrivial to go
in the other direction. You needed other applications to be started, you
needed to bring the Elixir runtime up in your Erlang program, and nobody
seemed particularly interested in making it easier for you. Now that I'm
on the other side, I feel less distrustful, but I still would hate to see
this community grow and grow while remaining blithely ignorant of what
else is being created on the BEAM. While there's the proximate promise
of being able to write your own code and take advantage of a healthy
ecosystem and a great virtual machine, I very much hope that we'll want to
reach for a larger goal of true language interoperability, where Erlang,
Elixir, LFE, Alpaca and who knows what else code can sit in the same
repo and call each other's bytecode without knowing or caring what it was
originally written in. If the distinction between Elixir and Erlang could
truly be reduced to one of taste we'd be in a very healthy position. It
doesn't seem that we're there yet, and as long as Elixir is the hotshit
language that's getting the most attention and the most momentum, most
Elixir programmers won't feel any impetus to further that goal.

This sense is my biggest worry, or at least the place where I feel the
greatest need to contribute. For instance, take the relative indifference
of the Elixir community to dialyzer. In Erlang it feels like a pillar;
in Elixir it's sometimes a curiosity. And with all the generated code in
your average Elixir project, getting dialyzer to play nice and produce
actionable feedback can be daunting to say the least. So, what—do we
just abandon it? My intuition tells me that the more Elixir code that can
be written to take advantage of the conventions that came before, the
more robust it will be and the more everyone will benefit. The keyword
syntax is a wonderful example of that. But when the new features Elixir
brings to the table come into conflict with what's already in place,
it's harder to say what the best way forward is. Elixir's tooling is
wonderful, and generally better than Erlang's. Would that have been
possible if Elixir had built on top of rebar? I have no idea.
