import timeit
import random


class O:
    def __init__(self):
        self.x = random.randint(0, 100)
        self.y = random.randint(0, 100)
        self.z = random.randint(0, 100)
        self.should_foo = random.choice([True, False])


class O2:
    __slots__ = ('x', 'y', 'z', 'should_foo')

    def __init__(self):
        self.x = random.randint(0, 100)
        self.y = random.randint(0, 100)
        self.z = random.randint(0, 100)
        self.should_foo = random.choice([True, False])


def new_a():
    return [
        random.randint(0, 100),
        random.randint(0, 100),
        random.randint(0, 100),
        random.choice([True, False])
    ]

def new_t():
    return (
        random.randint(0, 100),
        random.randint(0, 100),
        random.randint(0, 100),
        random.choice([True, False])
    )


Os = [O() for _ in range(10000)]
O2s = [O2() for _ in range(10000)]
As = [new_a() for _ in range(10000)]
Ts = [new_t() for _ in range(10000)]


def as_object():
    acc = 0
    acc2 = 0

    for o in Os:
        val = (o.x + o.y) * o.z
        acc += val
        if o.should_foo:
            acc2 += val


def as_optimized_object():
    acc = 0
    acc2 = 0

    for o in O2s:
        val = (o.x + o.y) * o.z
        acc += val
        if o.should_foo:
            acc2 += val


def as_list():
    acc = 0
    acc2 = 0

    for a in As:
        val = (a[0] + a[1]) * a[2]
        acc += val
        if a[3]:
            acc2 += val

def as_tuple():
    acc = 0
    acc2 = 0

    for t in Ts:
        val = (t[0] + t[1]) * t[2]
        acc += val
        if t[3]:
            acc2 += val


def bench(f):
    time = timeit.timeit(
        "{}()".format(f),
        setup="from __main__ import {}".format(f),
        number=10000)

    print("{}: {}".format(f, time))


if __name__ == "__main__":
    bench("as_object")
    bench("as_list")
    bench("as_optimized_object")
