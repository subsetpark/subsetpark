title: The Dark Path, or, What if I Don't Want to Quit My Job?
date: 2017-01-12
status: post

[Uncle Bob][], noted programming pundit, has written a strange [blog post](http://blog.cleancoder.com/uncle-bob/2017/01/11/TheDarkPath.html). It's a litany against, as near as I can tell, a certain attitude around static typing: that the purpose of strong static typing, and by extension programming language features, is to provide a high degree of security against certain classes of programmer errors: uncaught exceptions, unchecked nulls, using Javascript, et cetera.

[Uncle Bob]: https://en.wikipedia.org/wiki/Robert_Cecil_Martin

Bob says: 

> The question is: Whose job is it to manage that risk? Is it the language’s job? Or is it the programmer’s job.

(It is, per Bob, the programmer's job.) 

Later:

> Now, ask yourself why these defects happen too often. If your answer is that our languages don’t prevent them, then I strongly suggest that you quit your job and never think about being a programmer again; because defects are never the fault of our languages. Defects are the fault of programmers. It is programmers who create defects – not languages.

> And what is it that programmers are supposed to do to prevent defects? I’ll give you one guess. Here are some hints. It’s a verb. It starts with a “T”. Yeah. You got it. TEST!

That is, Bob strongly argues that the proper way to protect against programmer errors is *testing*, and that it is very wrong to design programming languages to do the work of testing.[^1]

[^1]: In the last paragraphs, Bob proposes a scenario where all programmers, needing to escape the shackles of these picky, overly verbose languages and get actual work done, override or bypass all the safety mechanisms in our language, and cause a (literal) nuclear meltdown. This is rather absurd and I won't address it any more than to point out that many ordinary programmers intuitively grasp the basic value of safety and specificity, which is why in Python it is widely considered extremely bad practice to write `except:` or `except Exception:` even though having to specify all of the exception classes we are interested in is arguably a safety that we could override at no loss of expressiveness.

What a weird article. And what a funny conclusion. Especially because Bob is certainly not the first person to establish a contrast between tests and types; it's an insight I have heard many times, from many quarters, but always until now formulated in the opposite direction: something along the lines of "I've noticed that dynamic typing forces you to write a lot more tests."

But in addition to coming off as somewhat cranky, Bob also strikes me here as deeply *naive*. I think that he's violating a pattern that I've come to place a lot of faith in: that any sufficiently *mature* programmer (engineer, computer scientist, or otherwise) eventually views the apparatuses of computer programming as prostheses, supports, and correctives for human reasoning---and views human reasoning as deeply fallible. In other words, they turn out to be very humble about their own ability as programmers.

And I'm talking big-brain men and women like [Lamport][], [Knuth][], [Stroustrop][], [Edwin Brady], [Kernighan][] and others. Scientists and engineers who I can be pretty confident are better-educated, smarter, and more knowledgeable about our shared domain than I will ever be. Even if they are strident and opinionated (and funnily enough Brady, being the author of the typiest and big-brainiest language I know, is probably least so) on one topic or another, they all say things like, "now, I never actually remember the order of the arguments here", or, "I can never keep all of [foo] in my head at once", or, "[bar] really helps you when you can't figure out how to write things". And not only as a justification for types, mind you. It's why Lamport advocates for [formal specification](https://en.wikipedia.org/wiki/TLA%2B)---not because he's so god-damned brilliant that he actually knows how to write math and I don't, but because he recognizes that even his ability to reason about and design software is *insufficient* without mechanical help.

[Lamport]: https://en.wikipedia.org/wiki/Leslie_Lamport
[Knuth]: https://en.wikipedia.org/wiki/Donald_Knuth
[Stroustrop]: https://en.wikipedia.org/wiki/Bjarne_Stroustrup
[Kernighan]: https://en.wikipedia.org/wiki/Brian_Kernighan
[Edwin Brady]: https://en.wikipedia.org/wiki/Idris_(programming_language)

I think this is ultimately the most, maybe the only, mature way to view programming languages and techniques. It's why testing is *also* a great practice. The basic insight that:

- I am not quite as smart as I felt whenever I wrote whatever I wrote;
- I am not quite so smart that *any* amount of education or experience will cleanse me of basic lapses of reasoning and memory;
- I am capable of learning pretty much anything, and no amount of learning will make me much smarter than I am now.

What do I/we do when we finally realize that, oh, dang, maybe I can be a brilliant computer scientist *and* a dumb human who can only hold 7 +/- 2 things in his monkey brain at any time? Uncle Bob suggests I "quit [my] job and never think about being a programmer again", to which I will respectfully demur. I like programming, and I think I'm good at it; I'm just stupid and I have a lousy memory. Instead I will do what my dumb human forebears have done for millenia, and use tools. Here follows an abridged list of prostheses developed by humans to assist and supplement the act of reasoning:

- Unit tests
- Type systems
- Code review
- Programming languages
- Logic
- Mathematics

And more importantly I will strive to *remember* my dumbness and be *humble*; to accept help wherever I can get it; and to meet my fellow humans in a spirit of dumb equality.
