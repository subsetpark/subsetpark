Title: Digital Minimalism for the Working Hacker 
Date: 2018-01-14 
status: post

Effectively speaking, I've always been a Vim user. When I got my first shell account, I started with Vim (In the Paleozoic, Mesozoic and Cenozoic ages of `vi`, `vim` and `neovim`, I came of age somewhere in the late Jurassic): my first CS teacher had studied at MIT and so was a dyed-in-the-wool LISP hacker; accordingly, he set up a Linux server in the back of the server closet at my high school, gave me a shell account, and introduced me to `emacs`. By that point I already found myself inclined to the left-hand side of the dichotomy between what felt like the lightness and agility of Vim (shorter startup times, fewer keypresses) and the power and flexibility of Emacs.

------------------------------------------------------------------------

Last year, around this time, I got a Linux laptop. At the time my motivations---at least the ones apparent to me---were primarily practical and maybe a little political: a long-time Mac user, I was now doing enough debugging of applications running on Linux servers that it seemed sensible to reduce the number of differences between my local and production development environments; plus Apple had just released their new MacBooks without the escape key, and that whole thing felt like a bridge too far at the time.

I had already gotten very used to the benefits of dynamic tiling window managers, and so when I set up my Linux machine I happily moved on from [the best that could be done under trying circumstances] to [i3], a window manager with a very slight footprint (resource-wise and otherwise) and a lot of keyboard-centric power and flexibility.

Using Linux can lead, for better or for worse, to a pretty thorough customization of one's computing environment. There's a few factors there: a centralized, text-based package manager lends itself to an extremely rapid iteration on one's setup; the fact that comparatively so few batteries are included in a distro like [Arch] means one can't really avoid that process in the first place; and the fact that nearly everything in one's system is still configured by one text file or another lends itself much more naturally to things like source-versioning one's dotfiles and starting to write little built-to-purpose shell scripts.

At the same time, there were and are certainly aesthetic forces at work. I'm a backend programmer, and (insofar as computing goes) the spiritual descendent of the old-school hacker class, and identify as such. I'm not great with graphics, or user interface design. I like plaintext. I don't use Instagram. In other words, a stripped-down, austere aesthetic (graphical and otherwise) appeals to me. I love to use LaTeX, and what's more I love the default stylesheet it comes in; I love the historical and aesthetic suggestions of [Computer Modern].

So there's been a sort of regression, maybe, back to some vaguely-imagined hacker past, and a stripping down of the complexity and graphical sophistication of the interfaces I use when I use my computer.

Around the same time that I was moving to Linux, I found myself reading a few [blog posts] on the topic of *syntax highlighting* (of all things). The basic argument went like this: syntax highlighting is crutch; it seems to make it easier to read code, but actually it allows us to skim and "read" through pattern recognition on colors and shapes rather than actually reading, as a text, the code we're working on. That's a moderately interesting hypothesis. By itself I don't think I would find it that compelling, except maybe that one of the anti-highlighting camp's most vocal members is Rob Pike, the computer scientist whose most interesting work, arguably, is the speculative-fiction operating system [Plan 9], with which I've had an enduring if largely unconsummated fascination for a while, and the similarly iconoclastic text editor [`acme`][], whose UI innovations I find perennially fascinating if, in practice, completely incompatible with how I like to work[^1]. Some combination of these influences---an interest in an experimental weaning off from syntax highlighting, a vague association with the UNIX Shangri-La of a bygone time and place, and an aesthetic kinship with a text editor that I loved but with whom I would just would never make it work---led me to the [acme-colors] theme for Vim. 

[acme]: http://acme.cat-v.org/

Not the most momentous development, I know. But there were two innovations (new to me, anyway) that ended up having a strong impact on how I worked:

-   Acme's color scheme is dark-on-light, black text on a sort of cream-colored background. I had a been a staunch light-on-dark user since my very first days on the terminal.
-   The acme-colors theme introduces no extra colors into the minimalist palette of the editor; but rather cleverly, relies on traditional text formatting---italic, bold, underline---to do its highlighting.

![acme-colors sample]

I liked this scheme very much (and still do, thankfully) and themed everything else relevant (terminal, etc) with the same colors.

------------------------------------------------------------------------

One of the interesting ways in which usage of vi and usage of Emacs differed---and I'll speak in the past tense because it was certainly true in the past, when resources were more constrained, and because I think I first saw it pointed out in a historical context, maybe in an article by [jwz]---is that because Emacs was *bigger* than vi, and more powerful, it took longer to start up; and because it took longer to start up (and presumably because its buffer management and the like was extremely powerful), Emacs users tended to have one or more instances of Emacs kept running, and would return to them and open a new buffer when they wanted to edit a new file. This was in contrast to vi users, who would tend to launch a new vi process from wherever they were in the filesystem, and then exit the process when they were done with the file they needed to edit.

It's not difficult to see this continued in Emacs users' tendencies to write and use extensions that allow them to perform a variety of tasks sometimes only tangentially related to text editing from within Emacs. Emacs Lisp is substantially more powerful and expressive than Vimscript (and surely moreso for whatever preceded it under vi) and it's not unusual to read and write email, use IRC, manage Git branches from Emacs. These same feats, while technically possible under Vim, almost never work as well if implemented, and are generally discouraged on a cultural level anyway.

There's a value judgment you can make here, depending on your taste; now, as when I was first presented with a choice, I find the sense of bulk implied here distasteful. Someone not entirely different from me would appreciate the sense of flexibility and universality granted by having an self-sufficient *environment* within which to work. But more is suggested in this distinction. It's not simply a matter of which is *better*, of trying to come to some sense of relative worth by establishing differences in efficiency, number of keystrokes, interface latency, et cetera. We're also presented with two very distinctive perceptual models.

There's a way in which you could say that Emacs is a *place* whereas vi is a *tool*. One *goes* or *returns to* Emacs whereas one *opens* vi; Emacs *lives somewhere* in your window manager or process space, whereas vi doesn't exist when you're not using it.

------------------------------------------------------------------------

Maybe the last influence that has been working on me is the enormous resource inefficiency of the modern web app. Slack, and [Electron] in general, is a [notorious case][slack-resources]; the standalone client takes up a huge amount of resources relative to what it's good for, and so I very much enjoyed the transition, over some time, from the standalone app, to the website in a tab in my browser, to a [plugin] for the command line IRC client [WeeChat]. Along a similar vein, I went tooling around not too long ago seeing if there was a way to stream music from a service like [Google Music] from the command line. It turns out there is, and after some hacking I got a whole setup going with [mopidy] and [`mpd`][mpd]. Thus it was with some satisfaction that I closed the Google Music tab in my browser and never opened it again.

[slack-resources]: https://medium.com/@matt.at.ably/wheres-all-my-cpu-and-memory-gone-the-answer-slack-9e5c39207cab
[mpd]: https://musicpd.org

This is all well and good. Command line tools are more powerful and more efficient than GUIs, and you get to feel like a hacker. That's fun and presumably profitable. But there's been another shift in all these different steps and migrations. Every change I've made in my working environment over the past year or so has also been a *lessening of immersiveness*. My interfaces have become *less rich*, *less colorful*, *less dynamic*.

This stands in direct contrast to the promises of modern computing. Apps and websites are praised for their immersiveness, and with good reason: today's web app is a seamless, rich experience, with CSS animations, soft-focus background videos, responsive, hand-tuned widgets. We're promised future interfaces that will be more immersive, more engaging, more *realistic*[^2]. We have haptic feedback now, and have foregone digital smell-o-vision for the imminent potential of VR. But I wonder if immersion is exactly the sort of thing we want to be avoiding. 

Listen: I take it as axiomatic for our purposes that a sense of bodily and sensory presence---mindfulness--is a good thing. I, like many members of my generation, have decided to work hard to reembed myself in my body and lived experience. So that's where I'm coming from.

From that perspective, the greatest satisfaction I've been getting out of computing---the reason I've actually had a lot of fun putting all my dotfiles together over the last little while---is that I've actually been *extracting* myself from the sensory experience of my computer. Google Music is a rich, pleasurable interface---but actually the beauty of [`mpc`][mpc] is that it's not just more convenient to hit a hotkey rather than switch windows, click, move, click; it's that when I use mpc I don't *go* anywhere.

[mpc]: https://musicpd.org/clients/mpc/

This is where this sense of *being a place* comes in. And this is why I suspect that there might be reasons to prefer Vim over Emacs beyond simple working preference. Google Music is a *place*. It is a fixed, concrete entity in my experience of my computer: it's a tab that stays open (because it would take too long to load otherwise), that contains internal navigational state, and which I maintain my own navigational state in order to get back to (second screen, first virtual desktop, first window, third tab from the left). That's a lot! It not only consumes computing resources to stay open, and cognitive resources to keep track of, but it also provides a place that must be *entered* in order to make use of it. When I am inside Google Music, I am not in my chair anymore, not inside my own senses.

I think the Acme colors have also pulled me out of the text a little. The benefits of no syntax highlighting also result in a less immersive experience. Less sensation and information for my attention to get caught up in. But I also find myself staring at a screen configured more or less the same as documents that human beings have been handling since---certainly since the advent of mechanical typewriters, and when I'm not typing then since farther back than that. This is a very useful recontextualization. There had been a trick of the eye happening; a change of coloring had provided the temporary sensation that what was in fact still simply words on a page was *somewhere*[^3]. I suspect it's not entirely useful to get overly wrapped up in the metaphor that the thing in front of me---the flat plane of my screen, encased as it is in a black plastic display, that stays exactly where it is and turns out of sight when I stand up from my desk and walk away---is actually a somewhere. For whatever reason I imagine Bob Cratchit, or Bartleby the Scrivener. I wonder if they ever forgot they were at their benches and imagined themselves to be inside their ledgers instead.[^absorption]

------------------------------------------------------------------------

I'm interested in this direction, and I'd like to encourage it. How? One way, obviously, is to do everything on the command line. But there's only so much you can do, or should. GUIs themselves are not evil, and in fact not even necessarily inefficient. Web pages, for instance. Can you say with a straight face that a text-based browser is more efficient and powerful than Firefox? Or, alternately, that you can get everything you want out of the web without images, Javascript, videos? Netflix? This isn't even about the often-bandied dictum that the keyboard is always more effective than the mouse, that you must murder the mouse, or train yourself never to use your trackpad. I use the mouse constantly when I'm using Vim.

It's more about the flow of attention between me and my web browser. Web browsers are very nice. But: the modern web browser is without a doubt a *place*. It is the most mountainous program I and most other people are ever running; it's always open, it's got a huge assortment of tabs open from various points in time, and if I close it I'll probably lose something. It is my Emacs: comparatively hulking, immobile, unaccommodating. I come to it, it does not come to me.[^4].

In the case of any big, stationary application my question is the same: can I change my habits of use from Emacs-like to vi-like? With Firefox this is almost impossible; it takes too long to load. And I don't always know what to do with every tab that I open, or don't know how long I'll need it, so I can't always close it and forget about it. It's not surprising that this should be the case; the technical requirements for a web browser are huge, and the range of things I can use the web for is equally huge, so multiplied together they result in a big program that sticks around.

Even for an application like web browsing or managing email, there are tools and programs written that afford this kind of use case. There are minimalist browsers like [`surf`][surf] and [`uzbl`][uzbl] that do very little, much less than you'd expect of a browser, and are designed to be strung along with a bunch of other little tools until you have what you need. I'm not interested in optimizing my web browsing experience within an inch of its life, or of going all Minority Report on my bookmarks. But would it be possible to stick those tools together so that actually I could banish the browser as a conceptual entity entirely from my computer, so that web pages show up when I need them, and otherwise are not heard from? To render the browser as uninteresting and unremarkable as it should be?

[surf]: https://surf.suckless.org/
[uzbl]: https://www.uzbl.org/

If this were just a matter of the efficiency gained by switching to command-line tools, or of writing shell scripts rather than using something out of the box, I probably wouldn't bother; and I certainly wouldn't bother blogging about it. The benefits of optimizing one's workflow are at best uninteresting. But I reflect on the potential of a system that *isn't there when I'm not using it*, and that I find very appealing. I like using my computer, I like programming. I don't think it's valuable to treat it as an experience to be minimized, or as a necessary evil in this modern world. But I think it's worth the work to reclaim it for myself; to reassert myself in my own chair and fully extract myself from it. That's how I can use it to its fullest.

[^1]: I don't think it's unfair or cranky to characterize Plan 9 as Pike's most interesting work, even as he has become vastly more influential for his work on the [Go] programming language, seeing as how Go's primary design principle is arguably to be as uninteresting as possible, at scale.

[^2]: In the sense of constituting or assembling a fascimile of a reality.

[^3]: quick: imagine **cyberspace**. When I do that, I see a black field of infinite depth, and in it are floating colorful digital shapes, letters and polygons. I'm a child of *Neuromancer*.

[^4]: The other place that remains in my windowing environment is my email client. My concerns are roughly the same. There's a lot of state, a lot of unrelated documents that end up being opened up next to each other, and a long load time.

    I bet there's some interesting thinking to be done about whether there is something in what we use these applications for that leads to this kind of behavior, and whether it could be resolved by changing how we handle the data. In a sense the whole GTD/Inbox Zero movement was about this: when you get an email, *capture* it by parsing in some way: transform from unstructured to structured data, maybe by making a note in your to-do list and archiving the email. Your to-do list is not necessarily a place; when well-maintained it's something you can summon up when you need it and dismiss when you're done. Of course, if handled poorly, your to-do list can become another place, an entity that sticks around keeps its own state and requires you to come to it.

[^absorption]: There's different kinds of absorption, of course. I'm sure that a 19th century clerk dreamt of figures and sometimes might have felt himself to be merging with the rows in his logbook, just as I dream of programming and forget to pee sometimes when I'm doing it. There's no part of me that wants to abolish that.

  [the best that could be done under trying circumstances]: http://ianyh.com/amethyst/
  [i3]: https://i3wm.org/
  [Arch]: https://www.archlinux.org/
  [Computer Modern]: https://en.wikipedia.org/wiki/Computer_Modern
  [blog posts]: https://www.robertmelton.com/project/syntax-highlighting-off/
  [Plan 9]: https://9p.io/plan9/
  [acme-colors]: https://github.com/plan9-for-vimspace/acme-colors
  [acme-colors sample]: ../images/acme-colors.png
  [jwz]: https://www.jwz.org/about.html
  [Electron]: https://electronjs.org/
  [plugin]: https://github.com/wee-slack/wee-slack
  [WeeChat]: https://weechat.org/
  [Google Music]: https://play.google.com/music/listen#/home
  [mopidy]: https://www.mopidy.com/
  [Go]: https://golang.org/
