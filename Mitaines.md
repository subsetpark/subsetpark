Title: Mitaines
date: 2014-04-6

In French Canada, according to to [Pagat.com][1], there's a game called [*Mitaines*][2], which means "Mittens" in French. I'm not sure when I first heard about this game, but it stuck with me. I couldn't fathom, judging from the rules, how it was played, but I knew that the game consisted of building combinations that were called *mittens*, *gloves*, and *socks*. This struck me as adorable and odd. I couldn't find any other information about it; if you google "Mitaines card game", the only results aside from Pagat are pages that seem to derive entirely from Pagat, and earlier works by me, including the [stream][3] on [Thoughtstreams.io][4] that this article is based on. But Pagat is pretty authoritative so I persist in believing that this game exists.

I don't know when I first found out about Mitaines, but I first [mentioned it][5] on the internet in February 2009. It next comes up for me in August of 2010, but only to say that I had forgotten about it. It wasn't until late 2012 that I actually went to the trouble of learning how to play the game. 

## How You Play

I'll try to establish a basic account of how we play the game. The 'we' is important here: I have never been taught this game by a French Canadian, who might have learned it from other French Canadians. I have never played this game with a French Canadian. I barely know any French Canadians. I have indeed never met anybody who has heard of Mitaines before. In other words, I have no acquaintance with anything you could call 'authentic' Mitaines. 

But this is how it works: each player takes turns playing to a central pile. This pile itself is not particularly important. You can clear it by playing either a Jack or whatever card is currently on top, but clearing it does almost nothing, most of the time[^2]. There are two things it does:

[^2]:   The exception here is the very first pile of the game, which includes a four-card flop; any valuable combinations that appear in that flop go to whoever clears the first pile of the game.

1. If the ♦10 is in that pile, you score points.
2. It forces whoever comes after you to play to an empty table.

I don't want to poo-poo the ♦10; it's worth 50 points and 50 points is nothing to sneeze at. Nevertheless, I'm used to playing Fishing games (which is what Pagat classifies this as) where the things played to the middle are the things being contested. In this case the pile in the middle is more like the rubbish heap on top of which something of potential interest is sitting.

The meat of the game is in the aforementioned mittens, socks and gloves. These are pairs, threes of a kind, and fours of a kind, in Poker terminology. And they're captured (in this case the game is very much like other Fishing games—[Scopa][8] comes to mind—where a valuable card or cards in your hand are of no use if you can't safely capture them, which usually requires at least briefly exposing them to danger before they can be whisked off the board) by announcing them and then playing them, one card per turn, for as many turns as it takes to play them all. 

Since it's pretty unusual to draw a lot of threes and fours into a single hand, the game provides a lot of opportunities for you to piggyback off of other players' cards, often coopting their own combinations in the process. I might start to play a mitten, but you can interrupt me if you've got something higher, and I can interrupt you back, each time folding the original calls into the current call, even if the cards themselves don't end up getting played. So you're not exactly playing combinations, or not just playing combinations, but really creating successively grander structures that are uneasily superpositioned until somebody finally completes a combination play and scores for everything that had been called before.

Anyway, as I said in my Thoughtstreams post, I worked it out from the printed rules with a friend, and we played through a few hands to get a feel for the mechanics. I liked it but, at least with two players, it was a bit uneventful. 

## Why It's a Little Boring

The issue, by our reckoning, is with the distribution of combinations. With six-card hands and a full 52-card deck, mitten draws (that's a pair) are quite common, and glove and sock draws are incredibly rare. Of course, you want pairs to more more common than trips and fours, and you'll expect to build some higher combinations off of others—but in its standard (?) state, Mitaines seems almost entirely to be a game of pairs. 

## What I Did About It

The Thoughtstream I was keeping, linked above, goes into more detail about my process in working with Mitaines. I'll present an overview here.

My first step was to model the likelihood of drawing the various scoring combinations in the game using binomial coefficients and combinatorics. To this end I used the OS X application [Soulver][], which acts like a live calculator and is quite easy to use. 

<iframe src="|filename|/static/mitaines.html" width="800px" height="1235px"></iframe>

The **Event Ratios** section expresses the likelihood of any given draw as a ratio of the rarest (ordinary) draw, socks: four of a kind. As we can see, one is 861 times more likely to draw a pair than to draw four of a kind when playing with six cards and a 52-card deck. Which is fine for poker, but one would like to see them a bit more often in this game of playing to the middle.

Here's what happens if we adjust the hand size upward, to eight cards, and strip out four ranks from the deck, leaving a 36-card deck, with 32 cards to be played in two deals after a four-card initial flop. 

<iframe src="|filename|/static/mitainesadjusted.html" width="800px" height="1235px"></iframe>

We see that mittens are now 110 times more likely than socks, not 861. The ratios have not been artifically rebalanced, and it's still no sure thing that a player can draw one of the 12 groups of four cards that get you a sock (Jacks are a special category), but it's in the realm of possibility. 

Especially because much of the play around gloves and socks happens with multiple players building a glove off of someone else's mitten, or a sock off of someone else's glove. When each player has a bigger pool to work from, the opportunities for exciting combinations increase.

## Scoring Adjustments

The current scoring schedule doesn't fully jibe with the likelihood of various combinations. As we can see on the Pagat page, a sock, when captured, is worth exactly 20 times a mitten—neither 861 nor indeed 110 times. It's understandable that it shouldn't be worth quite as much, as there are more ways to score a sock than there are to draw one. Nevertheless there's the feeling that things could be in better proportion.

As can be seen from my Thoughtstreams stream and the calculations I was doing, I also considered adopting an adjusted scoring schedule based on the adjusted ratios for combinations. This endeavor, unfortunately, didn't go as well. I'm not convinced it's an unworthy one but simply fixing points on a ratio sequence makes the game much more difficult to score, and doesn't seem to be particularly worth it. 

This is especially true when we consider that one *should* take building combinations into account when designing a scoring schedule; while we can trust that we can only consider the odds of a particular draw when designing hand size, we really must consider the odds of actually capturing that combination—that is, when drawn or built from someone else—when scoring. The strategic effectiveness is also part of a larger combination's worth; while a combination is being played out, if it can't be beaten, the other players are forced to discard potentially valuable cards. For now, I've found that the existing schedule works sufficiently well when playing with the adjusted hand sizes.

It might be fun, at some point, to come up with an AI that's smart enough at Mitaines that one could run some Monte Carlo simulations and come up with hard data on the relationship between combinations drawn and combinations scored for. Then you could come up with a real scoring schedule.

[1]:	http://pagat.com
[2]:	http://www.pagat.com/fishing/mitaines.html
[3]:	https://thoughtstreams.io/zdsmith/mitaines/
[4]:	http://thoughtstreams.io
[5]:	http://www.lastplanetojakarta.com/forums/index.php?topic=9929.85
[6]:	http://en.wikipedia.org/wiki/English_As_She_Is_Spoke
[7]:	http://www.imdb.com/title/tt0472027/?ref_=fn_al_tt_1
[8]:	http://www.pagat.com/fishing/scopone.html
[Soulver]: http://www.acqualia.com/soulver/