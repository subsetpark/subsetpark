Title: Thoughts on "Death of a Language Dilettante" 
Date: 2016-05-30
status: post

Let’s establish a few terms.

*Computation* is what computers do. And a *programming language* is a system for expressing computation.

I take it as an article of faith that the value of a programming language is the degree to which the language *makes it easier to reason about computation*. This is a departure from the implicitly stated value of a programming language in [Hague's article][article], which is to "reduce the pain of programming." I think the latter is reducible to the former. But if you don't say that outright, I think you can miss it. A lot of Hague's article expresses a sense that he and others are using the *wrong* programming language; that there are certain languages that are appropriate to use, and others that are inappropriate to use, and that too many people are using the inappropriate ones. That is, it seems that a lot of his dilettantism is driven by a sense of anxiety about his chosen milieu and tools at a point in time.

[article]: http://prog21.dadgum.com/219.html

This is an understandable anxiety, and one I share every time I have to write JavaScript: 1) there is considerable pain and uncertainty, and 2) I have the sense that the language I'm writing in is somewhat to blame.

But it's not why I am a programming language dilettante. My solution to that anxiety is to generally avoid writing JavaScript, which means generally half-assing the frontend components of the web code that I write. 

*Programming*, I should say, is *reasoning about computation*. That is, not exclusively writing the code the first time, but also coming back to it and making changes, and having a solid enough mental model to explain the behavior of the system to others. And in my opinion there *are* some languages that make this easier than others, and I'm actively interested in learning them and working in them. This does not mean that the languages which make it easiest to reason about computation are the appropriate ones to work in. A good, proper, modern functional language is a delight to me, but I am primarily a Python programmer and will reach for Python most times. This is not simply because I haven't put in enough effort to learning J---that's one reason among many that Python, which is probably not as *good* a language as J, is much more often the right tool for the job.

So that's not why I want to learn J, or why I have chosen to learn those non-Pythonic languages that I have learned to some extent, like Clojure and Erlang. Because the nice thing about sexy, functional languages like Clojure and Erlang, which are by my lights *better* than Python, or at least *absolutely* worth putting the time and effort into learning as a practice of language dilettantism, is that the ability to reason about computation is highly portable. And every time I immerse myself in a thoroughly different way of reasoning, such as is provided by all of the above examples, the important thing that's happening is going on in my head, not in the computer. Except for the problems to which it is uniquely suited, the value of most Erlang programs that I write is not that they are in Erlang. None of the exercises or [toy applications][toy] and [cellular automata][cell] that I wrote in Erlang are better than an equivalent program in JavaScript because they are in Erlang. The value in them was in their writing (and editing, and explaining), and in the new pathways and perspectives that they introduced into my habits of reasoning. These are habits which I may blessedly carry around with me wherever I go.

[toy]: https://github.com/subsetpark/erlsaunt
[cell]: https://github.com/subsetpark/systems

After all, I think it's quite arguable that idiomatic Python style (and Python is language and community with a particularly acute focus on idiomatic style, much for the better) has shifted (perhaps with the rest of the mainstream programming community) to a greater embrace of functional principles. Generations of Python programmers, myself included, have bit by bit spent a few weeks or months trying and mostly failing to wrap their heads around Haskell, but have returned to the fold with a greater appreciation for referential transparency, for the minimization of mutable state, for orderly and perhaps somewhat austere scoping (maybe even for types?). Clawing our way through the first half of [Learn You a Haskell For Great Good!][learnyou] has given us new skills in the art of reasoning about computation, which is roughly equivalent to making us better programmers.

[learnyou]: http://learnyouahaskell.com/ 

And this is the blessing of dilettantism. I think that's an important word, if somewhat inapt for much of the habits and motivations described in Hague's article. Dilettantism is not technocracy or progressivism, and the dilettante need not spend their life in search for the *better* and ultimately *best* way. The dilettante allows themselves to sample and dabble, to dip a few toes, to paddle around, to put up a repo with the first 7 [Project Euler][euler] problems, and then to recede and carry on their merry way. Once in a while they will hit on something marvelous that really speaks to them and become a True Believer. This is a grand thing and I live my life in the eternal faint promise that at some point in my life I will spend my whole days thoroughly ensconced in one tiny corner of the programming language firmament that has made itself known to me as The Best Way, where all the libraries are well-formed and all the code is concise, and until that day I am content and grateful to dabble, with the knowledge that every new expression of this same computation that all computers do might grant me a new and valuable perspective.

[euler]: https://projecteuler.net/
