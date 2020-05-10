import random

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

proc asObject() =
  var acc, acc2: int
  for o in Os:
    let val = (o.x + o.y) * o.z

    acc += val
    if o.shouldFoo:
      acc2 += val

  echo acc
  echo acc2

proc asArray() =
  var acc, acc2: int
  for a in As:
    let val = (a[0] + a[1]) * a[2]

    acc += val
    if a[3] > 0:
      acc2 += val

  echo acc
  echo acc2

asObject()
asArray()
