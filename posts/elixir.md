Title: Sketches of Elixir
Date: 2018-10-21
status: post

Friends, I am now at least in this respect a rare breed: an Elixir
programmer who has never written Ruby.

I was never a professional Erlang programmer; I cut my teeth on Python
and began working in Python, but Erlang was the language I learned
after that and the one that I looked for excuses to write in (in addition
to [Nim][], which would come a little later, but which I would also be able
to finagle into production at MakeSpace).

[Nim]: https://nim-lang.org/

So here was my impression of Elixir for the first several years of my
passing acquaintance with it: it's Erlang with Ruby syntax. I had never
ended up writing in Ruby because I had started out with Python, and I
was already happy with Erlang and its syntax. So I had a fairly simple
understanding of the Elixir language and the roots of its popularity.

For better or worse, the first thing that most people mention when
Erlang comes up is that the syntax is unfamiliar and confusing; at the
very least it's controversial (I love it). Ruby, on the other hand,
is vastly more popular than Erlang. So there's a significant population that finds Erlang syntax eccentric and Ruby conventional and pleasing.

On the other hand, there are many reasons to recommend the BEAM, OTP,
and the Erlang Runtime System. And concurrent programming and functional
programming are now vastly more mainstream than they might have been when
Erlang was first developed. So there is a steady stream of object-oriented
and/or web application programmers (Ruby or otherwise) taking an interest
in Erlang. It would make sense that programmers coming from a Ruby background would find Elixir more palatable.

And marks of the Ruby frame of reference are all over Elixir; its creator
after all was a [member of the Rails core team][rails], and its influence is
manifest in several areas of the language---especially those parts
of it visible from the outside. Not just the syntax, but also a much
greater emphasis on programming for the web, including a Rails-like web
framework, [Phoenix][phoenix].

[rails]: http://plataformatec.com.br/crafting-rails-applications
[phoenix]: https://phoenixframework.org/

So I didn't see much of a need for Elixir; it seemed to be a solution
for problems I didn't have.

Not to mention, I had a sort of misguided purist's attitude towards
the whole thing. Not that I had the years of Erlang experience to back
this up, but new Elixir programmers seemed to me to be *arrivistes*,
or maybe just tourists, barging in and setting up conferences about
their new concurrent Ruby. And it was being heralded as the next big
thing while Erlang was ignored.

I was even sure enough of myself to express this opinion (the part about
the language, not the *arrivistes*) in public---not just in a discussion
forum, but one frequented by José Valim, preternaturally patient BDFL
of the Elixir language. Someone had posted something along the lines of
"what's the deal with Elixir? Should I learn it instead of Erlang?" and
I responded with something along the lines of, "it's just Erlang with
a different syntax. Learn the real thing."

My memory of José's reply was something like, "that's a bit reductionist;
there's also macros." I was embarrassed, not only because I didn't
know that José was going to see my response, but because I didn't even
know there *were* macros in Elixir.[^zulip] It's telling that this is my
memory, because on looking back it's not actually what José said. I won't
quote him directly, but I don't think it's breaking too many confidences
to give a brief rundown of the features that José cited as distinct
(aside from macros), as of early 2015, from Erlang:

- lazy collections;
- more robust structured data (structs);
- better tooling, incl. testing frameworks and Mix

This is telling because at the time I thought, "well, sure, macros;
but Erlang has parse transforms (not that I know how to use them)
and macros are weird and shouldn't be used that much anyway." I wasn't
a macro guy and, in fact, still am not. I don't use macros that much
today. On the other hand, the part of his reply that I don't remember,
the part about lazy collections, structs and tooling, I use *constantly*.

[^zulip]: I am now embarrassed all over again, because going back and
reading my early posts in this forum about Erlang I see that not only
did I make an ass of myself up there but this was a couple years after
*José himself* had been the person who was most responsive to my initial
questions about programming in Erlang.

I started working at [Frame.io][frame] in August 2018. For many reasons,
it was a really exciting opportunity for me, one of them being that almost
the entire backend application is written in Elixir. For a long time I
had been thinking, "wouldn't it be cool to some day get to write Erlang
for a living?" and I wasn't so much of a chauvinist that I didn't see the
chance to write Elixir full-time to be, modulo some `def/end`s, just that.

[frame]: http://frame.io

So here's the actual value proposition for the Elixir language: it's not
Erlang with Ruby syntax; it's Erlang with a robust and useful standard
library and tooling.

The thing about the Ruby syntax in Elixir is that it is a complete red
herring. It is by far the most superficial and least consequential element
of the language. If you like Ruby syntax, if you dislike Ruby syntax, it
doesn't really matter. You learn it and then you move on. It disappears
almost immediately. This is perhaps the first thing I noticed coming to
Elixir from Erlang: the base semantics are almost *identical*. That is,
almost every element of the Erlang language is preserved wholesale;
Elixir consists almost entirely of goodies that are piled on top. This
is very reassuring to the Erlanger. You don't lose any of your intuition
for what makes good code.[^assign-once]

[^assign-once]: The sole exception to this is single assignment, which
Elixir does away with, allowing the same variable to be bound more than
once. This introduces some complication in the sense that one needs to
learn how to "pin" variables with `^`, and I often find myself thinking
that the fact that pipes result in so many fewer variables being bound in
the first place means that single-assignment would be even nicer in Elixir
than Erlang; you get the benefits of knowing what `x` is whenever you see
it, plus you have to write `x1 = ...`, `x2 = ...` much less. Once again to
his credit, José and the team didn't implement variable rebinding simply
because the alternative was scary and confusing to Ruby programmers,
and [his logic for it][rebinding] is consistent and reasonable.

[rebinding]: http://blog.plataformatec.com.br/2016/01/comparing-elixir-and-erlang-variables/

The second thing you discover as a reluctant Erlang-to-Elixir convert
is that the standard library *matters*. There's two qualities here, and
again neither one of them is as simple as coddling Rubyists who can't
deal with the quirks of the Erlang standard library (1-based indexing,
inconsistent naming or argument order...).

The first aspect of this is that the specific modules that are pure Elixir
innovations are relatively few and obviously well-considered. Big ones
are, as mentioned, `Stream` and `Struct`. Stream, the lazy collections
module, is lovely precisely because there's nothing unique to Elixir
about it; under the hood one imagines it's got all kinds of protocols
and macros and other Elixiry things, but it's presented to the user
simply as exactly the sort of all-purpose highly functional library that
you could want from the Erlang standard library too. Structs are more
baked into the language itself; There's a struct syntax based on maps
and structs often seem to result in the kind of OO-style data+methods
object that you expect out of Python and would never see in Erlang. The
tradeoff: you get a certain amount of compile-time checks of attribute
access. Static guarantees might be the only thing I would be willing to
trade away some of Erlang's datatype minimalism for.

The second aspect is that if you are given the opportunity to greenfield
an entire standard library at once on top of everything you get from
Erlang already, it turns out you can implement some pretty elegant
features. You can, for instance, make sure that the "object" of a
function is in the first argument position for every single function,
making it possible to introduce the pipe operator `|>` quite simply.

I fucking love pipes. Relatively speaking, there are extremely few
assignments in Elixir. Not only because it's functional and immutable,
but because the programmer is encouraged, syntactically, to think about
their code as a series of data transformations chained together. If the
programmer can do that, they are rewarded with an extremely terse and
expressive, at times scarily close to *point free*, structure. This makes
debugging, rearranging, refactoring more straightforward. It reduces
parenthesis noise. It's a big win. Again, it's not just that it's less
Erlangy for the arbitrary reason that we distrust the Erlang syntax. It
makes it easier to write better code.

The third thing you discover is that, just maybe, some of this new
syntactic sugar is kind of nice. I'll use keyword arguments as an
example. In Elixir you can approximate Python-style positional and
named function arguments (`f(x, y, z=True)`) in syntax: `f x, y, z:
true`. This is exactly the kind of unnecessary imposition of Rubyist
tropes that you're on the lookout for---until you realize that this
is actually sugar over an *extremely* idiomatic Erlangism, the tagged
value. `[x, y, z: true]` is precisely equal to `[x, y, [{:z, :true}]]`,
which is the existing Erlang convention for passing optional parameters
to a function, but easier to type and read.

This sort of thing ends up being quite reassuring. Sometimes, because
Elixir has gotten some heat on it, you read blog posts that talk about
Phoenix and microservices in the way that other people talk about Node
or React---that is, with an air of hype and untroubled confidence that
this is the new best way to build web apps that seems to have come more
from reading blog posts than from suffering through the building of
many systems. A new, up-and-coming technology always carries with it
a penumbra of thought leadership and startup marketing masquerading as
how-to blog posts.

But in fact the creators of this language seem to understand the Erlang
idiom very well and don't tread too heavily over it. It is possible
to reflect on what is essential to Erlang and to imagine how we could
extend it if we took it upon ourselves to design several layers of
expressiveness on top of it. And the OTP model is very much at work in
the Elixir ecosystem: almost all Elixir applications are built on the
OTP application model, and the `gen_*` concept is even extended with new
behaviours like `GenStage`.

Of course, the simplicity and flatness of Erlang is not nothing, and
Erlang has traded some of that away. With protocols, `__using__` macros
and more, Elixir has many new forms of code reuse and polymorphism than
are possible or idiomatic in Erlang. That means there are many more
places with *magic* in them, where an imported module or expanded macro
does things that aren't obvious by reading the code.

That said, I'm enormously happy to be working with Elixir. The experience
of writing Elixir is a pleasure and there is no better foundation than
the BEAM and OTP.

In fact, working in Elixir full-time has renewed my interest in
the entire BEAM ecosystem. Elixir has demonstrated that the bones of
Erlang are valuable and applicable enough to serve as the foundation for
programming languages of all flavors. And Elixir programmers are treated
with an ecumenical view of the entire BEAM; the Erlang standard library,
as well as any Erlang code in the path, is available transparently to
be called from Elixir.

How can we expand that relationship in the other direction? Calling
Elixir code from Erlang, [while technically possible][erlang-call],
is not quite as convenient as just chucking some module files into
`/src`. What can we do to make compiled Elixir code as transparently
available to Erlang programmers? Or, for that matter, to [LFE][]
programmers? How far can we go in making tools like [rebar3][] and
mix functionally equivalent? How far can we get towards a truly
language-agnostic workflow, running OTP applications with modules written
in LFE, Elixir, Erlang, [Joxa][joxa]... You get the point.

[erlang-call]: https://joearms.github.io/published/2017-12-18-Calling-Elixir-From-Erlang.html
[LFE]: http://lfe.io/
[rebar3]: https://www.rebar3.org
[joxa]: http://joxa.org/

This is what I'd love to see come out of the Elixir language and
community. It's got a wonderful momentum right now; lots of interest,
lots of new contributors, and a thoughtful and motivated core team. We
have the opportunity to work towards a totally integrated BEAM ecosystem.
