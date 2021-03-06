(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 9: asynchronous programming

  ------------------------------------
  Name: Tavo Annus
  Student ID: taannu
  ------------------------------------


  Answer the questions below. Your answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework9/coursework9.fsx
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect, then it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

  NB! Do not delete the stubs we have provided! If you did not manage
  to complete the implementation then keep the incomplete stub and
  make sure that it still is typeable as required by the task.

  The submission deadline is Sunday, December 19.

*)


// Our representation of complex numbers.
type Complex = double * double



(*
   Question 1

   The Mandelbrot set.
   
   Formal definition:
   https://en.wikipedia.org/wiki/Mandelbrot_set#Formal_definition

   Basic properties:
   https://en.wikipedia.org/wiki/Mandelbrot_set#Basic_properties

   Define the function

   mandelbrot : int -> Complex -> bool

   so that 'mandelbrot n' is the characteristic function of the
   approximation of the Mandelbrot set by n iterations of the
   quadratic map.

   In other words, 'mandelbrot n' is a function which decides whether
   a complex number c belongs to the "n-th approximation" of the
   Mandelbrot set.

   In other words, define the function so that given

   n : int
   c : Complex

   'mandelbrot n c' evaluates to true precisely when, according to n
   iterations of the quadratic map, c belongs to the Mandelbrot set.

   The quadratic map is given in "Formal definition" and a condition
   for deciding whether c belongs to Mandelbrot (upto n iterations) is
   given in "Basic properties".

   Here are some hints:
   - Start by computing the z_1, z_2, ..., z_n for c. Based on this
     sequence of values decide whether c belongs to the (approximation
     of the) Mandelbrot set or not.

   - How to add and multiply complex numbers?

   - What is the absolute value of a complex number?

*)

let mandelbrot (n: int) (c: Complex): bool =
  let (a0, b0) = c
  let rec helper n (a, b) =
    if a * a + b * b > 4.0 then false
    else if n <= 0 then true
    else helper (n - 1) (a * a - b * b + a0, 2.0 * a * b + b0)
  helper n (0.0, 0.0)


(*
   Question 2

   Define the function
 
   divide : int -> int -> (int * int) seq

   so that given

   m : int
   n : int

   'divide m n' evaluates to a sequence of ranges

   (s_1, e_1), ..., (s_m, e_m)

   so that:
   * s_1 = 0 and e_m = n - 1

   * s_{i + 1} = e_i + 1


   That is to say that:   
   * for every x : int such that 0 <= x < n there exists an i such
     that s_i <= x <= e_i

   * the sequence of (s_i, e_i) covers 0, ..., n - 1 without overlap,
     i.e., if s_i <= x <= e_i and s_j <= x <= e_j, then i = j.

   You may think of n as the number of things we have and m as the
   number of buckets among which we distribute them.

   Try to divide fairly.
*)

let divide (m: int) (n: int): (int * int) seq =
  let perChunk = n / m
  let andOnes = n - perChunk * m
  let d i = if i < andOnes then i else andOnes
  seq {
    for i in 0..(m-1) do
      yield i * perChunk + (d i), (i + 1) * perChunk + (d (i + 1)) - 1
  }



(*
   Question 3

   Define the function
   
   mandelbrotAsync :  int
                   -> int
		   -> (int -> unit)
		   -> (int -> unit)
		   -> Complex []
		   -> Async<bool []>

   so that given

   m      : int
   n      : int
   start  : int -> unit
   finish : int -> unit
   cs     : Complex []

   'mandelbrotAsync m n s e cs' creates an asynchronous computation
   which computes an array of booleans with the same size as cs. An
   element in this array is true precisely when the complex number at
   the corresponding position in cs belongs to the Mandelbrot set
   (according to n iterations).

   Use the 'divide' function to divide the work into m chunks (ranges)
   and then evaluate those in parallel using the Async mechanism.

   Whenever this function starts computing on a chunk of input (s, e),
   the function must apply the function 'start' to 's'. When it has
   completed working on the chunk, then it must apply 'finish' to 'e'.

*)

let mandelbrotAsync m n start finish cs : Async<bool[]> =
  async {
    let! nested = divide m (Array.length cs) |>
                  Seq.map (fun (s, e) -> 
                    async {
                      start s
                      let res = [| s .. e |] |> Array.map (mandelbrot n << Array.get cs)
                      finish e
                      return res
                    }
                  ) |> Async.Parallel
    return Array.collect id nested
  }





(*
  Question 4

  Define the function

  display : int -> bool [] -> string

  so that given

  n  : int
  bs : bool []

  'display n bs' represents the Boolean array bs as rectangle of
  characters with true represented by '*' and false represented by
  '.'.

  The parameter n : int is the length of the rows in the string
  representation.

  For example, if
  
  n  = 2 
  bs = [| true; false; false; false; true; true; false |]

  then

  display n bs = "*.\n..\n**\n.."

  (Note the handling of missing values in the example.)

*)

let display n (bs: bool []) : string = 
  let padTrailing s = if String.length s >= n then s else s + String.replicate (n - String.length s) "."
  bs |> Array.chunkBySize n
  |> Array.map (Array.map (fun b -> if b then "*" else ".") >> String.concat "" >> padTrailing)
  |> String.concat "\n"





(*

  You may use the function display to display the Mandelbrot pattern
  that you compute using mandelbrotAsync.

  For example, you can take an array of complex numbers representing a
  rectangular grid from (-3, -3) to (1, 3) with steps of 0.2 and then
  use display with an appropriate row length.

*)







// The next questions are about observables.
//
// You are not allowed to write any mutable code. Solve these using
// the combinators from FSharp.Control.Observable module.


(*
   Question 5

   Define the function

   accumulate : ('t -> 'a -> 't * 'u option) -> 't -> IObservable<'a> -> IObservable<'u>

   so that given

   f   : 't -> 'a -> 't * 'u option
   t   : 't
   obs : IObservable<'a>

   'accumulate f t obs' accumulates observable events of obs into an
   accumulator of type 't and emits an observable event u when
   'snd (f acc a)' evaluates to 'Some u' for an observed event 'a'.

*)

let accumulate (f : 't -> 'a -> 't * 'u option) (t : 't) (obs : System.IObservable<'a>) : System.IObservable<'u> =
  obs |> Observable.scan (fun (i, _) v ->  f i v) (t, None)
  |> Observable.choose snd




(*
   Question 6

   Define the function

   chunks : int -> IObservable<'a> -> IObservable<'a list>

   so that given

   n   : int
   obs : IObservable<'a>

   'chunks n obs' is the observable of chunks (or pieces) of length n
   of the observable obs.

   If n = 3 and obs emits events a_1, a_2, a_3, ... then

   'chunks n obs' emits [a_1; a_2; a_3], [a_4; a_5; a_6], ...

*)

let chunks (n : int) (obs : System.IObservable<'a>) : System.IObservable<'a list> =
  obs |> Observable.scan (fun s v -> if List.length s = n then [v] else v :: s) []
  |> Observable.filter (fun os -> List.length os = n)
  |> Observable.map List.rev





(*
   Question 7

   Define the function

   sliding : int -> IObservable<'a> -> IObservable<'a list>

   so that given

   n   : int
   obs : IObservable<'a>

   'sliding n obs' is the observable of sliding windows of length n
   over the observable obs.

   If n = 3 and obs emits events a_1, a_2, a_3, ... then

   'sliding n obs' emits [a_1; a_2; a_3], [a_2; a_3; a_4], ...

*)

let sliding (n : int) (obs : System.IObservable<'a>) : System.IObservable<'a list> =
  let removeLast = List.rev << List.tail << List.rev
  obs |> Observable.scan (fun s v -> if List.length s = n then v :: (removeLast s) else v :: s) []
  |> Observable.filter (fun os -> List.length os = n)
  |> Observable.map List.rev



(*
   Question 8

   Define the function

   limit : IObservable<unit> -> IObservable<'a> -> IObservable<'a>

   so that given

   clock : IObservable<unit>
   obs   : IObservable<'a>

   'limit clock obs' emits events from obs.

   The observable starts in the emit mode: the next observed event of
   obs can be emitted. After that the observable transitions to
   discard mode: all observed events of obs are discarded until an
   event from clock is observed after which the observable transitions
   back to emit mode and the process repeats.

*)

type Either<'a, 'b> = Left of 'a | Right of 'b 

let limit (clock : System.IObservable<unit>) (obs : System.IObservable<'a>) : System.IObservable<'a> =
  let clock' : System.IObservable<Either<unit, 'a>> = Observable.map Left clock
  let obs' : System.IObservable<Either<unit, 'a>> = Observable.map Right obs
  Observable.merge clock' obs'
  |> Observable.scan (fun (emit, msg) e -> match e with 
                                           | Left _ -> (true, None) 
                                           | Right msg -> (false, if emit then Some(msg) else None)) (true, None) 
  |> Observable.choose snd





(*
   Question 9

   Define the function

   alarm : int -> int -> IObservable<unit> -> IObservable<int> -> IObservable<unit>

   so that given

   n         : int
   threshold : int
   clock     : IObservable<unit>
   obs       : IObservable<int>

   'alarm n threshold clock obs' emits an event when the average value
   of last n events of obs has been greater than the threshold.

   Furthermore, after emitting an event, this observable goes into
   silent mode until the next clock event is observed.

   In silent mode, even if there is cause for an alarm, no event is
   emitted.

*)

let alarm (n : int) (threshold : int) (clock : System.IObservable<unit>) (obs : System.IObservable<int>) : System.IObservable<unit> =
  let removeLast = List.rev << List.tail << List.rev
  let clock' : System.IObservable<int option> = Observable.map (fun _ -> None) clock
  let obs' : System.IObservable<int option> = Observable.map Some obs
  Observable.merge obs' clock'
  |> Observable.scan (fun (os, _) e ->
      match (e) with
      | Some o -> (o :: (if List.length os >= n then removeLast os else os), true)
      | None -> (os, false)) ([], true)
  |> Observable.filter (fun (v, b) -> match b with
                                      | true -> List.average (List.map float v) > float threshold
                                      | false -> true)
  |> Observable.scan (fun (emit, ev) (os, b) -> match b with
                                                | true -> (false, if emit then Some () else None)
                                                | false -> (true, None)) (true, None)
  |> Observable.choose snd



