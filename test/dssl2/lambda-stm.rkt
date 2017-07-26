#lang dssl2

let sum = 0
let actual = map((lambda x:
      sum = sum + x
      2 * x), [1, 2, 3])

assert_eq sum, 6
assert_eq actual, [2, 4, 6]

