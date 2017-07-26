#lang dssl2

let x = if True:
    5
else: 6

assert_eq x, 5
