(*
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 1: Basic F#, recursion.

  ------------------------------------
  Name: Tavo Annus
  Student ID or Uni-ID: taannu
  ------------------------------------


  Answer all the questions below.  You answers to questions should be
  correct F# code written after the question in comments. This file is
  an F# script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error somewhere
  and your result will not be evaluated.

  This coursework will be graded.

  To submit the coursework you will be asked to
  
  1) Check out your  GIT repository
  from the server gitlab.cs.ttu.ee using instructions on page
  https://courses.cs.ttu.ee/pages/ITT8060

  2) Put your solution into a file coursework1/coursework1.fsx
  in the repository. Commit it and push it to the server!
  It is your responsibility to make sure you have pushed the solution
  to the repository!

  NB! It is very important to make sure you use the exact name using
  only small caps. Files submitted under wrong name may not get a
  grade.

  Also, use the exact function and identifier names with precise types
  as specified in the question.

  The F# interpreter should be able to load your solution without
  errors.

  NB! In this coursework you are not allowed to use functions from the
  standard library except when the task indicates so. Recursion has to
  be done explicitly. It is also not allowed to use mutable values,
  i.e. variables.


  The deadline of submission of this coursework is September 17.
*)







// 1. Associate an identifier `intAndBool` with a value that is a pair of
// an `int` and a `bool`.
let intAndBool = (1, true)











// 2. Define a function
// 
//   atMostHalf : int -> int
// 
// so that `atMostHalf n` evaluates to an integer `m` such that
// 
//   2 * m <= n < 2 * (m + 1)
// 
// You may assume that the argument is non-negative.
let atMostHalf (n : int) : int = n / 2
  









// 3. Define a function
// 
// avgAndEq : int * int * int -> float *  (bool * bool * bool)
// 
// so that `avgAndEq (i1, i2, i3)` evaluates to a pair of a float and a
// triple of bool values so that the float value is the average of i1, i2
// and i3 and the three bool values are true precisely when,
// respectively, i1 = i2, i2 = i3, and i3 = i1.
//
// Hint:
//   float : int -> float
let avgAndEq (i1: int, i2: int, i3: int) : float * (bool * bool * bool) = 
  (float (i1 + i2 + i3) / 3.0, (i1 = i2, i2 = i3, i3 = i1))











// 4. Define a function
// 
//   multSkipFromTo : int -> int -> int -> int
// 
// so that `multFromTo k m n` evaluates to an integer
// 
//   m * (m + k) * ((m + k) + k) * ... * n
// 
// In other words, multiply numbers from `m` to `n`, but also skip some
// of them (given by `k`).
// 
// You may assume that `m <= n` and `k > 0`.
// 
// Examples:
// 
//   multSkipFromTo 1 1 5 = 1 * 2 * 3 * 4 * 5
// 
//   multSkipFromTo 3 1 5 = 1 * 4 * 5
// 
//   multSkipFromTo 4 1 5 = 1 * 5
// 
// Use recursion. Do not use the `match` construct.
let rec multSkipFromTo (k : int) (m : int) (n : int) : int =
  if m < n then m * multSkipFromTo k (m + k) n
  // else if m < n + k - 1 then n // <- Why you do this, soo ugly  >:D
  else n










// 5. Consider the function given by
// 
//   f n = n / 2        if n is even
//   f n = 3 * n + 1    otherwise
// 
// The 3n + 1 conjecture says that, for any positive n, the sequence
// 
//   n, f n, f (f n), f (f (f n)), ...
// 
// will always reach 1.
// 
// Your task is to define a function
// 
//   threeN : int -> int
//
// so that `threeN n` evaluates to the number of steps needed for `n` to
// reach `1` according to `f`. In other words, how many times do we have
// to apply the function `f` to `n` to reach `1`.
// 
// You may assume that the argument is positive.
// 
// Use recursion.
// 
// Number of steps needed to reach `1` is the sequence
// https://oeis.org/A006577
let rec threeN (n : int) : int = 
  if n = 1 then 0 
  else if n % 2 = 0 then 1 + threeN (n / 2)
  else 1 + threeN (3 * n + 1)








// 6. Define a function
// 
//   notFibonacci : int -> int * int
// 
// such that
// 
//   fst (notFibonacci 0) = 2
//   fst (notFibonacci 1) = 1
//   fst (notFibonacci n) = fst (notFibonacci (n - 2)) +
//                          fst (notFibonacci (n - 1))
// 
// and
// 
//   snd (notFibonacci n)
// 
// is the number of times the function `notFibonacci` is used. Note that
// both `notFibonacci 0` and `notFibonacci 1` should use the function
// only once (i.e., no recursive calls) and `notFibonacci n` should
// perform two recursive calls (for `n >= 2`).
// 
// You may assume that the argument is non-negative.
// 
// Use recursion.
let rec notFibonacci (n : int) : (int * int) =
  match n with
    | 0 -> (2, 1)
    | 1 -> (1, 1)
    | _ -> 
      let a = notFibonacci (n - 2)
      let b = notFibonacci (n - 1)
      ((fst a) + (fst b), 1 + (snd a) + (snd b))









// 7. Define the functions
// 
//   sinApprox : int -> float -> float
// 
//   cosApprox : int -> float -> float
// 
// so that
// 
//   sinApprox d : float -> float
//   
//   cosApprox d : float -> float
// 
// approximate the sine and cosine functions where `d` is the
// approximation parameter which we call depth. You may assume that depth
// is non-negative.
// 
// The idea is to base these functions on the trigonometric identities
// 
//   sin(x) = 2 * sin(x / 2) * cos(x / 2)
// 
//   cos(x) = cos^2(x / 2) - sin^2(x / 2)
// 
// and the fact that
// 
//   sin(x) ~ x
// 
//   cos(x) ~ 1
// 
// for small values of `x`.
// 
// We consider `x` to be small at depth `0`. Otherwise it is not
// small.
// 
// Use recursion. To avoid infinite recursion we decrease the depth by
// one every time we use the trigonometric identity.
//
// You can compare your approximations with results from
// `System.Math.Sin` and `System.Math.Cos`.
//
// sinApprox 10 1.0
let rec sinApprox (n : int) (a : float) : float =
  if n = 0 then a
  else 2.0 * (sinApprox (n - 1) (a / 2.0)) * (cosApprox (n - 1) (a / 2.0))

and cosApprox (n : int) (a : float) : float =
  if n = 0 then 1.0
  else (cosApprox (n - 1) (a / 2.0)) ** 2.0 - (sinApprox (n - 1) (a / 2.0)) ** 2.0


