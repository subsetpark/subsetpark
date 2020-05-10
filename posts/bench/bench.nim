import nimbench, random

randomize()

type
  O = object
    x, y, z: int
    shouldFoo: bool
  A = array[0..3, int]

var
  Os = newSeq[O](1_000_000)
  As = newSeq[A](1_000_000)

for i in Os.low..Os.high:
  let
    x = rand(100)
    y = rand(100)
    z = rand(100)
    shouldFoo = rand(1)
  Os[i] = O(
    x: x,
    y: y,
    z: z,
    shouldFoo: shouldFoo.bool
  )
  As[i] = [x, y, z, shouldFoo]

bench(asObject, m):
  var acc, acc2: int
  for _ in 1..m:
    for o in Os:
      let val = (o.x + o.y) * o.z

      acc += val
      if o.shouldFoo:
        acc2 += val

  doNotOptimizeAway(acc)
  doNotOptimizeAway(acc2)

bench(asArray, m):
  var acc, acc2: int
  for _ in 1..m:
    for a in As:
      let val = (a[0] + a[1]) * a[2]

      acc += val
      if a[3] > 0:
        acc2 += val

  doNotOptimizeAway(acc)
  doNotOptimizeAway(acc2)

runBenchmarks()
