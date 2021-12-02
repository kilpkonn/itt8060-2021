(*
  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 8: Sequences, laziness and computation expressions

  ------------------------------------------------------------------------------
  Name: Tavo Annus
  Student ID: taannu
  ------------------------------------------------------------------------------


  Answer the questions below. Your answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework8.fsx in directory coursework8.


  The deadline for completing the above procedure is Sunday, December ?, 2021.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)




(*
  Task 1: Pascal's triangle

             1
            1 1
           1 2 1
          1 3 3 1
         1 4 6 4 1
        ...........
       .............
      ............... 
  

  Define the function

    next : int list -> int list

  that, given a row of the triangle, computes the next row. The
  function List.windowed may be useful here.


  Define the sequence

    triangle : int list seq

  which consists of the rows of Pascal's triangle (represented as int
  list). Do not use sequence expressions. Define this using
  Seq.unfold.


  Define the function

    evens : int -> int list

  so that

    evens n

  evaluates to a list (of length n) consisting of the sums of elements
  in the first n rows of Pascal's triangle that have an even number of
  elements.

*)
let next (xs : int list) : int list =
  1 :: (List.windowed 2 xs |> List.map List.sum) @ [1]  // PERF: Slow here, hopeffully ok tho

let triangle : int list seq =
  [1] |> Seq.unfold (fun r -> Some (r, next r))

let evens (n : int) : int list =
  [1..2..(2 * n)] |> List.map (fun x -> 1 <<< x) 




(*
  Task 2

  Define the function

    generate : 'a list -> ('a list -> 'a) -> 'a seq

  so that

    generate xs f

  evaluates to a sequence consisting of the elements in xs followed by
  elements computed by the function f.

  More precisely, if List.length xs = n, then s_i (the i-th element in
  the sequence) is

  * the i-th element of the list xs   if i < n
  * f [s_{i - n}; ... ; s_{i - 1}]     otherwise

  Note that f must be applied to lists of same length as xs.

  You may assume that xs is not empty.

  Define this using sequence expressions.

  Make sure that the calculation of an element of the sequence uses
  the function f at most once.

  The function Seq.cache may be useful here.

*)
let rec generate (xs : 'a list) (f : 'a list -> 'a) : 'a seq =
  let baseSeq = Seq.ofList xs

  let rec helper mySeq =
    seq {
      let curr = f (Seq.toList mySeq)
      let tmpSeq = Seq.tail mySeq
      yield curr
      yield! helper (Seq.append tmpSeq (Seq.singleton curr))
    } |> Seq.cache
  
  seq {
    yield! baseSeq
    yield! helper baseSeq
  }





(*
  Task 3: Longest common subsequence
  
  We have two arrays, xs and ys, and we wish to find the length of the
  longest common subsequence in these two arrays.
  
  Example:
  
  - xs = [| 1; 2; 3; 4 |]
  - ys = [| 5; 1; 6; 4 |]
  
  Length of the longest common subsequence is 2.
  
  This can be solved using dynamic programming.
  
  Let D be a two-dimensional array that holds the results of the
  subproblems:
  - D[i, j] is the length of the lcs of xs[1..i] and ys[1..j].
  
  Solving the subproblems:
  - if xs[i] = ys[j] then we follow one subproblem (shorten both sequences):
      D[i, j] = D[i - 1, j - 1] + 1
  
  - otherwise we take the maximum of two subproblems:
      D[i, j] = max D[i - 1, j] D[i, j - 1]
  
  - base cases:
      D[i, 0] = D[0, j] = 0
  
  
  Observation: it is not necessary to fill the entire table D to
  calculate D[i, j].
  
  If we decide to fill only those parts of the table that are necessary
  to compute D[i, j], then we need to be careful to not use the values
  in the unfilled parts in our calculation.
  
  However, we can use lazy values instead and let the runtime figure out
  which entries in the table and in which order need to be calculated.
  
  Define the function
  
    lcs : ((int * int) -> unit) -> 'a [] -> 'a [] -> Lazy<int> [,]
  
  so that
  
    lcs m xs ys
  
  evaluates to the table D for xs and ys except that the entries in the
  table are lazy. An entry in the table is computed only when we ask for
  the value (of the Lazy<int>) or the computation of another entry
  requires the value of this entry.
  
  The function m must be applied to (i, j) when the entry D[i, j] is
  actually computed. For example, you can use printfn as m to make the
  order of calculations visible.

*)
let lcs (m : (int * int) -> unit) (xs : 'a []) (ys : 'a []) : Lazy<int> [,] =
  // NOTE: Maybe should recursive build up to make use cache of lazy
  // let rec calcElem x y =
  //   // failwith $"{xs} - {ys} - {x} - {y}"
  //   m (x, y)
  //   if x = 0 || y = 0 then lazy 0
  //   else if Array.get xs (x - 1) = Array.get ys (y - 1) then lazy ((calcElem (x - 1) (y - 1)).Value + 1)
  //   else lazy (max (calcElem (x - 1) y).Value (calcElem x (y - 1)).Value)
  // Array2D.init (xs.Length + 1) (ys.Length + 1) (fun x y -> calcElem x y)
  let eval m (xs : 'a []) (ys : 'a []) (xrow : Lazy<int> []) (ylast : Lazy<int>) (x : int) (y : int) : Lazy<int> =
    printfn $"x: {x}, y: {y}, xs: {xs.Length}, ys: {ys.Length}"
    if x = 0 || y = 0 then lazy (m (x, y); 0)
    else if Array.get xs (x - 1) = Array.get ys (y - 1) then lazy (m(x, y); (Array.get xrow (x - 1)).Value + 1)
    else lazy (m (x, y); max (Array.get xrow (x-1)).Value ylast.Value)

  (Array.mapFold (fun xrow x ->
    // printfn $"{xrow} @ {x}"
    let row = fst (Array.mapFold (fun ylast y ->
                                    let curr = eval m xs ys xrow ylast x y
                                    (curr, curr)
                                  ) (lazy 0) [|0..(ys.Length)|])
    (row, row)
  ) (Array.init (xs.Length) (fun _ -> lazy 0)) [|0..(xs.Length)|]) |> fst |> array2D






(*
  Task 4:

  A function from a type 'env to a type 'a can be seen as a computation that
  computes a value of type 'a based on an environment of type 'env. We call such
  a computation a reader computation, since compared to ordinary computations,
  it can read the given environment. Below you find the following:

    • the definition of a builder that lets you express reader computations
      using computation expressions

    • the definition of a reader computation ask : 'env -> 'env that returns the
      environment

    • the definition of a function runReader : ('env -> 'a) -> 'env -> 'a that
      runs a reader computation on a given environment

    • the definition of a type Expr of arithmetic expressions

  Implement a function eval : Expr -> Map<string, int> -> int that evaluates
  an expression using an environment which maps identifiers to values.
  
  NB! Use computation expressions for reader computations in your implementation.
  
  Note that partially applying eval to just an expression will yield a function of
  type map <string, int> -> int, which can be considered a reader computation.
  This observation is the key to using computation expressions.

  The expressions are a simplified subset based on
  Section 18.2.1 of the F# 4.1 specification:
  https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf

*)

type ReaderBuilder () =
  member this.Bind   (reader, f) = fun env -> f (reader env) env
  member this.Return x           = fun _   -> x

// implementation Monad? (a -> b) where
//   (reader : a -> b) -> (f : a -> b -> c) -> (a -> c)
//   reader >>= f = (\x -> f x (reader x))
//   pure x = (\_ -> x)

let reader = new ReaderBuilder ()

let ask = id

let runReader = (<|)

type Expr =
  | Const  of int          // constant
  | Ident  of string       // identifier
  | Neg    of Expr         // unary negation, e.g. -1
  | Sum    of Expr * Expr  // sum 
  | Diff   of Expr * Expr  // difference
  | Prod   of Expr * Expr  // product
  | Div    of Expr * Expr  // division
  | DivRem of Expr * Expr  // division remainder as in 1 % 2 = 1
  | Let    of string * Expr * Expr // let expression, the string is the identifier.


let eval (e:Expr) : (Map<string, int> -> int) =
  reader {
    match e with
    | Const n -> return n
    | Ident x ->
      let! env = ask
      return env.Item x
    // | Neg e -> return ReaderBuilder.Bind (eval e) (fun n -> -n)
    | _ -> return 0
  }

// //Example:
// //keeping in mind the expression: let a = 5 in (a + 1) * 6
// let expr = Let ("a",Const 5, Prod(Sum(Ident("a"),Const 1),Const 6))
// eval expr Map.empty<string,int>
// should return 36     

