title: Designing Tie Knots by Random Walks
date: 2014-03-25

I'm a fan of [85 Ways to Tie a Tie][85], by Thomas Fink and Young Mao. It's an exploration of a mathematical model for necktie knots. I'll let them [explain][designing]:

>  We have developed a mathematical model of tie knots, and provide a map between tie knots and persistent random walks on a triangular lattice.

What this means for the likes of me is that they a) articulated, essentially, the full array of knots it's possible to tie with a necktie, and b) provided an unambiguous notation for their tying. Thus as additions to the traditional [Windsor][], [Four-in-Hand][], [Pratt][], etc. (all also notatable in the *85 Ways* system), they furnished knots like the [Plattsburgh and Cavendish][encyc]. 

The Plattsburgh, for instance, can be notated thus: 

>  Lo Ci Ro Ci Lo Ri Co T
  
I would read that: *Left out, Center in, Center in, Left out, Right in, Center Out, Through,* where Left/Right/Center is the direction of the turn, and in/out is the orientation of the tie blade. Fink & Mao go on to analyze the aesthetic properties of knots according to certain metrics, like symmetry and balance. So we can demonstrate, for instance, that the Plattsburgh has a symmetry of 0 (quite good), and a balance of 1 (also quite good), and it will have a very broad shape.

I decided I'd like to explore this concept with a Python library. Ultimately it might be fun to make a full-fledged web app but in any case, with a project like this, it seemed prudent to focus on coming up with an effective representation of the knot as a data structure.

## Implementation

My first intuition was to represent the set of all possible knot sequences as a simple binary tree. When tying the tie, you have two initial choices: Left in or Left out. After that, and after every other step, you can go in whichever of the three directions you haven't just gone in, and the orientation flips with each generation. So *Li* has *Co* and *Ro* for children, *Co* has *Li* and *Ri*, *Ro* has *Li* and *Ci*. Thus under ordinary circumstances, if a turn is a single node on the tree, the tying of a tie can be represented as two mirrored binary trees, with *Li* and *Lo* as their two roots.

In theory this was simple enough. But when I actually drew out the trees they were not nearly as regular as I expected. Maintaining a consistent ordering, where children are numbered from left to right (so it would be L -> C R; C -> L R; R -> L C), one quickly finds it hard to draw any generalizations about the behavior of the tree, and hard to discern any patterns when traversing it. 

I asked [Alan][] for a little guidance on how to better understand the graph of possible routes in my knot, and it was his suggestion that got me set on the right path: 'deterministic' trees, where the children of any node can be determined without bothering with the shape of the graph, are perhaps best considered as state machines rather than graphs. So each node can basically report on its children based on its own state. 

This model works very well for the necktie knots: you can easily say, given the direction and orientation of any individual turn: *to find the next possible steps in our path, choose from the other directions and other orientation* and you have your choices. In other words, 'flip the bit' of the current node. Using Python's built-in set datatypes you get a simple and fairly elegant way to do XOR operations on node attributes.

    ::::python
    def flip(value):
        # 'Flip the bit' of the provided value, providing its alternate(s)
        if value in DIRECTIONS:
            return list(DIRECTIONS - set([value]))
        elif value in BITS:
            return (BITS - set([value])).pop()
        else:
            return value

So then it makes sense for individual nodes (turns in our necktie knot) to be the active class in this model. A Node object has a direction and an orientation, and using this information and the above helper function, it's able to report on its children—the possible next steps in the tie.

    ::::python
    >>> n = Node("Li")
    >>> n.get_children()
    {'Co', 'Ro'}

### The Walk

The next step is to actually tie the tie. Using this model a recursive function makes sense; the nodes don't care where they are in the tie so we can do the same operation in any place.

    ::::python
    def random_walk(walk=[]):
            if not walk:
                return random_walk([starter()])
            elif walk[-1] == Node('Ti') and tiable(walk):
                return walk
            elif walk[-1] == Node('Ti') and not tiable(walk):
                return random_walk(walk[:-4])
            elif len(walk) > 9:
                return random_walk(walk[:-4])
            else:
                walk.append(random.choice(walk[-1].get_children()))
                return random_walk(walk)

Here we recursively walk through the tie, which is just a list of nodes. If it doesn't exist, we start one, and if it's done (all knots end with 'Co Ti'; 'Ti' is added as a possible child for 'Co' nodes) we return it. And if it's somewhere in the middle we make a random choice of the node's children and take it on the end[^1]. 

This works, and it seems relatively elegant to me. But you can get a into a lot of trouble with the middle two cases. 

See, here's the thing: in one sense, when constructing these paths you don't need to know anything about where you are in the path to know what your potential children are. On the other hand, for something to function recognizably as a necktie, the path needs to obey certain added constraints on top of always being able to find a child node. Fink & Mao, when coming up with the set of all possible neckties, came up with two necessary constraints.

1. Length. No knot can be more than ten steps long, including the final 'through' step. This is simply because after ten steps, you're quite likely to run out of tie.
2. Tiability. To avoid looking like something that was tied in the dark, while blackout drunk, all ties must end with one of the two sequences *Lo Ri Co Ti* or *Ro Li Co TI*. I invite the reader to demonstrate this fact for themselves by attempting to tie and wear the following knot: *Lo Ri Lo Ci Ro Li **Co Ri** Co Ti*.

In other words, a knot might be *syntactically* correct (the above example can be physically tied, and obeys the notational rules) while being *semantically* gibberish. And we want to restrict ourselves to the set of knots that obey both constraints—of which there are exactly 85.

This is where inefficiency enters our recursive approach in a major way. In the above (simplified) code, if we end up with a knot that has grown to exceed our twin maxims of *brevity* and *quality* (to play [Grice][] for a second), we have to basically rewind and try again. This gets us there eventually but there's a lot of wasted cycles.

So I did implement that recursive approach, and I did implement a couple optimizations that allow us to throw out fewer knots along the way. But after some encouragement from [Tom][] I decided I'd like to also implement an approach that never made a 'wrong move' in its random tie-building.

## Syntax/Semantics, or Rules/Moves

What I came up with is a function that runs alongside the get_children() function and reports the legal moves given the state of the path at that time, irrespective of the active node. One that, in other words, encodes the 'rules of play'—the constraints that a knot must fulfill in order to be included in our set of 85, regardless of how we're getting there. 

    ::::python
    def legal_moves(self):
        legal_moves = set([])
        if self.penultimate():
            legal_moves.add('Co')
        elif self.antepenultimate() and not self.finishable():
            legal_moves.update(['Li', 'Ri'])
            if self.two_away():
                legal_moves.add('Co')
        elif self.preantepenultimate() and not self.finishable():
            legal_moves.update(['Ro', 'Lo'])
            if self.two_away():
                legal_moves.add('Co')
        elif self.mid_knot():
            legal_moves.update(['Ri', 'Ro', 'Li', 'Lo', 'Ci', 'Co'])
        if self.finishable():
            legal_moves.add('Ti')
        return legal_moves

This function has no particular interest in the moves available to the most recent node in the walk. It generates a set of potential next states based entirely on, if you will, the position of the board. It's hard not to lapse into gaming metaphors here, though the linguistic one above also seems apt.

    :::python
    def legal_intersection(self):
        return list(self.get_children() & self.legal_moves())

In either case the mechanism is one of intersecting the 'powers' of an actor—this piece moves in a line, or jumps by two, but can't go diagonal; this word can come after the following part of speech—with an overall constraint based on the total structure over time, or on relations between that actor and other pieces on the board: the king cannot put himself into check, or "the red dog climbs the stairs" is not admissable if we haven't heard from any red dog thus far.

    ::::python
    >>> k = Knot("Li Co Ri")
    >>> k.legal_intersection()
    ['Lo', 'Co']
    >>> l = Knot("Li Co Ri Co Li Ro")
    >>> l.legal_intersection()
    ['Li']

So here we have a knot *k*: earlier in the 'game', the available moves are the total set of moves that the active piece is capable of making. In the next case we see a possible development of *k*. We are now in the countdown to the end of play, and thus our moves are constrained. We only have one choice available that's the only move that will result in a successful knot at the end.

## Next Steps

So there we have just about all there is to say about this particular project. I'm very interested in this rules/moves or syntax/semantics intersection model, though, and I'd like to build on it. I'd especially like to find a more elegant and extensible way to structure the `legal_moves` function, which right now is just a series of conditional statements. It's clear that there's a way to generalize the state of play, such that you could make a series of informed conclusions without having to manually write a bunch of tests. And it's also clear that this model is applicable to more than just neckties–and given that it's applicable to neckties, it's also applicable to more than just board games and conversations!

[^1]: For a depressingly long time this function wasn't doing at all what I expected. It's because I had my last two lines on one line. It's intuitive to think of something like "return walk.append(...)" as "append something onto walk and then return walk", but it really means "append something onto walk in-place and then return None, which is the returned value of an in-place operation."

[encyc]: http://www.tcm.phy.cam.ac.uk/~tmf20/tieknots.shtml
[designing]: http://www.tcm.phy.cam.ac.uk/~tmf20/TIES/PAPERS/paper_nature.pdf
[Pratt]: http://www.tie-a-tie.net/pratt.html
[Windsor]: http://www.tie-a-tie.net/windsor.html
[Four-in-Hand]: http://www.tie-a-tie.net/fourinhand.html
[85]: http://en.wikipedia.org/wiki/The_85_Ways_to_Tie_a_Tie
[Alan]: https://github.com/happy4crazy
[Grice]: http://www.sas.upenn.edu/~haroldfs/dravling/grice.html
[Tom]: https://github.com/thomasballinger
