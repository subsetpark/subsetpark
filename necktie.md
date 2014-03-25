# Designing Tie Knots by Random Walks

I'm a fan of [85 Ways to Tie a Tie][85], by Thomas Fink and Young Mao. It's an exploration of a mathematical model for necktie knots. I'll let them [explain][designing]:

>  We have developed a mathematical model of tie knots, and provide a map between tie knots and persistent random walks on a triangular lattice.

What this means for the likes of me is that they a) articulated, essentially, the full array of knots it's possible to tie with a necktie, and b) provided an unambiguous notation for their tying. Thus as additions to the traditional [Windsor][], [Four-in-Hand][], [Pratt][], etc. (all also notatable in the *85 Ways* system), they furnished knots like the [Plattsburgh and Cavendish][encyc]. 

The Plattsburgn, for instance, can be notated thus: 

>  Lo Ci Ro Ci Lo Ri Co T
  
I would read that: *Left out, Center in, Center in, Left out, Right in, Center Out, Through,* where Left/Right/Center is the direction of the turn, and in/out is the orientation of the tie blade. Fink & Mao go on to analyze the aesthetic properties of knots according to certain metrics, like symmetry and balance. So we can demonstrate, for instance, that the Plattsburgh has a symmetry of 0 (quite good), and a balance of 1 (also quite good), and it will have a very broad shape.

I decided I'd like to explore this concept with a Python library. Ultimately it might be fun to make a full-fledged web app but in any case, with a project like this, it seemed prudent to focus on coming up with an effective representation of the knot as a data structure.

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
    >>>



[encyc]: http://www.tcm.phy.cam.ac.uk/~tmf20/tieknots.shtml
[designing]: 
[Pratt]: 
[Windsor]: 
[Four-in-Hand]: 
[85]: 
