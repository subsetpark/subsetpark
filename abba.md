Title: Abba
Date: 2014-03-19
Status: draft

My first major project at [Hacker School][hs] is called [Abba][http://github.com/subsetpark/abba], the Abbreviation Engine. In order to understand why Abba exists I need to backtrack a little.

# the New Abbreviations

Since around 2006 I have written in an alphabetic shorthand that I call (when I have to write about it in places like this) [the New Abbreviations][tna]. If you asked me for a snappy one-liner I'd say (and have said) it's basically a 'human-readable non-lossy compression algorithm for text'. It's also of the same general family as the [*abbreviationes*][abbreviations] of the Middle Ages. It looks like this:

![TNA_sample](http://i.imgur.com/048po.jpg)

[hs]: http://hackerschool.com
[tna]: https://thoughtstreams.io/zdsmith/new-abbreviations/
[abbreviations]: http://en.wikipedia.org/wiki/Scribal_abbreviation

Very simply, the New Abbreviations (and alphabetic shorthands in general) are a matter of matching some simple lettern patterns and replacing them with different symbols. 

I've written this way for a while. It's a long-standing project of mine of which I'm rather fond. And on the face of it, this should be very simple to digitize. 

There are just a couple factors which make it non-trivial to implement as a simple set of find-and-replace rules:

1. Positioning rules. This shorthand was designed with English text in mind, and so there are certain assumptions about character placement built in. For instance: one very common word that's abbreviated in this shorthand is the word "we", which comes out as *w̃*. That's pretty readable, especially when you combine it with its fellows, *m̃* for "me" and *ũ* for "us". But it wouldn't be as useful to read and write if you simply replaced the sequence "we" every time those two letters came together, leaving *sw̃ll* and *sw̃ep* and others. So you replace "we" the word only when it stands alone. And similarly, you replace the prefix "con" and the suffix "ion". So you need to translate human understandings of morphology, to an extent, into abbreviation rules.

2. Non-Unicode glyphs. Above we saw that "we" gets abbreviated as *w̃*, which is all well and good because we can type out *w̃* with relatively little difficulty. But shorthands are not typed, they're written; and thus they'll tend to contain symbols that you can't necessarily represent in computer text—and certainly not with perfect semantic fidelity. In my shorthand, for instance, "er", when found in the middle of a word, is rendered with a little curlicue above the x-height, as seen in the wikipedia entry for [Scribal Abbrevations][abbreviations] under "forms". There's no unicode character for that that I've been able to find. So you can't simply replace several letters with one letter—sometimes what you're inserting won't be a letter at all.

# Implementation

It was with these technical hurdles in mind that I decided to come up with a Python application that could do the work of my shorthand system. 

Of course, it would hardly be worth my while to write a program that only spoke the New Abbreviations. It only made sense to write a more generic shorthand engine, which could understand the NA as a ruleset, as well as any other hypothetical alphabetical shorthand that somebody else might like to come up with. 

In any case, the first thing I knew was that you couldn't just stick funny unicode letters into a given text and call it a day. If you could I never would have written a program in the first place; I would have just assembled a huge unwieldy collection of unicode symbols and sufficed myself with a lot of copy and pasting. 

Instead, I figured: since strings and lists (of characters) are so closely related in Python, why not 