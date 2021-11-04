(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 6: Tail recursion

  ------------------------------------------------
  Name: Tavo Annus
  Student ID: taannu
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework6.fsx in directory coursework6.

  The deadline for completing the above procedure is Sunday, November 7, 2021.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.
*)

(*
  Task 1:

  Write a function
  pHoldsForAllSequentialElements : (int -> int -> bool) -> int list -> bool

  that when calling the function

  pHoldsForAllSequentialElements p xs

  will check if predicate p holds for all consequtive elements in the list. If it does,
  the function should return true, else false.

  In the case there is less than two elements in the list it is assumed that
  the predicate does not hold as there are no sequential elements present.

  NB! You are required to implement the function in a tail recursive way using
  explicit recursion.
*)
let pHoldsForAllSequentialElements (p : int -> int -> bool) (xs : int list) : bool =
  let rec helper acc p xs = match xs with
                            | x :: y :: [] -> acc && p x y
                            | x :: y :: xs -> if acc then helper (p x y) p (y :: xs) else false 
                            | _ -> false
  helper true p xs

(*
  Task 2:

  Write a function
  createTwoTuplesOfList : 'a -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s and returns a list of two element tuples of 'a-s that are taken
  sequentially from the list passed as the second argument to the function.
  In case the list has odd number of elements make the first argument of the
  function be the second element in the tuple. 
  Make sure your implementation uses explicit tail recursion.
*)
// NOTE: 0 [1;2;3] -> [(1,0); (2,0); (3,0)] <- Don't think that's correct, someone needs to eat their words again...
let createTwoTuplesOfList<'a> (x :'a) (xs : 'a list) : ('a * 'a) list =
  let len = (List.length xs) % 2
  let rec helper acc z xs = match xs with
                            | [] -> List.rev acc
                            | x :: [] -> [(x, z)]
                            | x :: y :: xs -> helper ((x, if len = 0 then y else z) :: acc) x xs
  helper [] x xs


(*
  Task 3:

  Write a function
  createTwoTuplesOfListFold : 'a -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s and returns a list of two element tuples of 'a-s that are taken
  sequentially from the list passed as the second argument to the function. In case
  the list has odd number of elements make the first argument of the function be the
  second element in the tuple. 
  Make sure your implementation uses List.fold or List.foldBack appropriately.
  Test yourself if this implementation appears to be tail recursive.
*)
let createTwoTuplesOfListFold<'a> (x : 'a) (xs :'a list) : ('a * 'a) list =
  List.foldBack (fun y acc -> (y, x) :: acc) xs []

(*
  Task 4:

  Below you find the definition of a type Tr of leaf-labeled trees. Write a
  function
  
  medianAndAverageInTree : int Tree -> int * float
  
  that returns a pair where the first element is the median label in the
  given tree and the second an average across all nodes. The median is the label
  for which the difference in counts of elements to the right and left is
  either 0 or the count of elements to the right is exactly 1 greater than the
  count of elements to the left. The average is the sum of all elements divided with
  the number of elements.
  Use continuation-passing style in your implementation and perform the operations
  in a single pass of the tree.
*)

type 'a Tr =
  | Lf   of 'a
  | Br of 'a Tr * 'a Tr

let rec merge (xs : int list) (ys : int list) : int list =
  let rec insert acc v zs = match zs with
                            | [] -> List.rev (v :: acc)
                            | z::zs -> 
                              if v > z then (List.rev acc) @ (v :: z :: zs)
                              else insert (z :: acc) v zs
  match xs with
  | [] -> ys
  | x :: xs -> merge xs (insert [] x ys)

let medianAndAverageInTree (tree : int Tr) : (int * float) =
  let rec helper tree k : int list = match tree with
                                     | Lf x -> k [x]
                                     | Br (l, r) -> 
                                       helper l (fun ls -> helper r (fun rs -> k (merge ls rs)))
  let vs = helper tree id
  let avg = List.map float vs |> List.average
  let med = List.sort vs |> List.item (List.length vs / 2)
  (med, avg)
