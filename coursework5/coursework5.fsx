(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Performing queries on FSharpON data structures

  ------------------------------------
  Name: Tavo Annus
  Tallinn University of Technology Student ID
  or Uni-ID: taannu
  ------------------------------------


  Answer the questions below. You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework5/coursework5.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect it will not be graded.

  Your solution must not use mutable state and imperative features
  like for loops.
*)


(*

For introduction to FSharpON please check coursework4.fsx for references.

In this coursework we continue with the topic of trees and object notation. This
time the task is, given a description of a set of values in an Ecma,
how to retrieve, modify and delete those values. This is a bit similar
to questions 6 and 7 in coursework 4.

The following material of basic definitions is taken from CW4. You are
free to use your solution to CW4 or try something different.

*)






type Name = string


// 0. Define data structure(s) for representing FsharpON (same as in CW4)
//
type Ecma = 
  | Object of (Name * Ecma) list
  | Float of float
  | Bool of bool
  | String of string
  | List of Ecma list
  | Null

(*
Define the function

  mkObject : unit -> Ecma

that creates a representation for an empty object structure.
*)

let mkObject () = Object []


(*
Define the function

  mkNumber : float -> Ecma

that creates a representation for the given floating-point number.
*)

let mkNumber (number : float) = Float number

(*
Define the function

  mkBool : bool -> Ecma

that creates a representation for the given Boolean value.
*)

let mkBool (value : bool) = Bool value

(*
Define the function

  mkString : string -> Ecma

that creates a representation for the given string value.
*)

let mkString (value: string) = String value

(*
Define the function

 mkArray : Ecma list -> Ecma

that creates a representation for an array whose elements are
represented by the given list of `Ecma` values.
*)

let mkArray (arr : Ecma list) = List arr

(*
Define the function

  mkNull : unit -> Ecma

that creates a representation of the ECMA-404 `null` value.
*)

let mkNull () = Null

// Define the function
//
//   addNameValue : Name * Ecma -> Ecma -> Ecma
//
// so that
//
//   addNameValue (n, v) e
//
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an object representation
//
// - a representation for the object e extended with the name-value
//   pair (n, v), otherwise.
let addNameValue (name : string, value : Ecma) (obj : Ecma) =
  match obj with
  | Object items -> Object (items @ [(name, value)])
  | e -> e




// Define the function
//
//   addValue : Ecma -> Ecma -> Ecma
//
// so that
//
//   addValue v e
//
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an array representation
//
// - a representation for the array e with the value v added as the last
//   element, otherwise.
let addValue (v : Ecma) (obj : Ecma) =
  match obj with
  | List items -> List (items @ [v])
  | e -> e








////////////////////////////////////////////////////////////////////////
// The content of this coursework begins here //////////////////////////


// You are given a type of expressions.

type BExpr = True
           | Not      of BExpr
           | And      of BExpr * BExpr
           | Or       of BExpr * BExpr
           | HasKey   of Name
           | HasStringValue of string
           | HasNumericValueInRange of (float*float)
           | HasBoolValue of bool
           | HasNull

(*

The type BExpr is just a discriminated union. The intended
interpretation of values of type BExpr is as predicates on values of
type Ecma.

 - True: evaluates to true on any Ecma

 - Not b: evaluates to true on precisely those Ecma for which b
          evaluates to false

 - And (b1, b2): evaluates to true on precisely those Ecma for which
                 both b1 and b2 evaluate to true

 - Or (b1, b2): evaluates to true on precisely those Ecma for which at least
                one of b1 and b2 evaluates to true

 - HasKey k: evaluates to true on precisely those Ecma that are objects and
             that contain a key k.

 - HasStringValue s: evaluates to true on precisely those Ecma that are 
            either Ecma strings with value s, objects that contain a value s,
            or arrays that contain a value s.

 - HasNumericValueInRange (xmin,xmax): evaluates to true on precisely those Ecma
               that either are
               numeric Ecma with value in closed range xmin,xmax,
               objects with a numeric value in closed range xmin,xmax,
               arrays with a numeric value in closed range xmin,xmax.

  - HasBoolValue b: evaluates to true on precisely those Ecma that are either
                    Boolean Ecma with value b,
                    objects that contain a Boolean value b,
                    arrays that contain a Boolean value b.
  - HasNull : evaluates to true on precisely those Ecma that are either
                    null Ecmas,
                    objects that conitain a null value,
                    arrays that contain a null value.

*)



// Here is a type of selector expressions.

type Selector = Match     of BExpr
              | Sequence  of Selector * Selector
              | OneOrMore of Selector


(*

The type Selector is just a discriminated union. The intended
interpretation of values of type Selector on values of type Ecma is as
sets of values in that Ecma. We also refer to the set of values
described by s : Selector as the set of values selected by s.

 - Match b: the singleton set consisting of the root value if the
            expression b evaluates to true and the empty set otherwise.
 
 - Sequence (s, s'): the set consisting of those values in the Ecma tree
                     that are selected by the selector s' starting from
                     any child value of a value that is selected by the
                     selector s (starting from the root value).

                     In other words, first determine the set of values
                     selected by s (starting from the root value). For
                     every child c of a value in this set, determine
                     the set of values selected by s' (starting from c)
                     and take the union of such sets as the result.

                     In other words, start from the root value with the
                     selector s. For the values that it selects,
                     continue with selector s' from their child values
                     and collect the results.
 
 - OneOrMore s: select the values selected by the selector s and, in
                addition, from the child values of the values in this
                set select the values selected by OneOrMore s.
 
                Thus, you can think of the values selected by OneOrMore s
                as the union of the following sets:
                - values selected by s
                - values selected by Sequence (s, OneOrMore s)
*)




// 1. Translate the following informal descriptions into values of
// type BExpr and Selector.
// 
// Define the values b1, b2 and b3 of type BExpr so that:
// 
//  - b1 evaluates to true on those Ecma that are object values
//    containing the keys "blue" and "left" but do not have the key "red".
// 
//  - b2 evaluates to true on those Ecma that are numeric values with
//    the value in the range [-5, 5).
// 
//  - b3 evaluates to true on those Ecma that have the string value "b3"
//    or that are object values which have the key "b3".
//
// Define the values s1, s2 and s3 of type Selector so that:
// 
//  - s1 selects all object values with key "abc" that are at depth 3
// 
//  - s2 selects all values v such that v is a child of some value
//    and all of the ancestors of v have the string value "xyz"
// 
//  - s3 selects all values v such that:
//    * v is a child of a value t
//    * t does not have a string value "xyz"
//    * t is the root value
// 
// We consider the root value to be at depth 1.
let b1 = And (And (HasKey "blue", HasKey "left"), Not(HasKey "red"))
let b2 = And (Not (HasNumericValueInRange (5.0, 6.0)), HasNumericValueInRange (-5.0, 5.0))
let b3 = Or (HasStringValue "b3", HasKey "b3")

let s1 = Sequence (Match True, Sequence (Match True, Match (HasKey "abc")))
// let s2 = OneOrMore (Sequence (Sequence (Match True, (Match (HasStringValue "xyz"))), OneOrMore (Match True)))
let s2 = OneOrMore (Sequence (Match (HasStringValue "xyz"), Match True))
let s3 = Sequence (Match (Not (HasStringValue "xyz")), Match True)



// 2. Define the function
//
// eval : BExpr -> Ecma -> bool
//
// which evaluates the given expression on the given Ecma.
//
// Evaluating a BExpr only considers properties of the root value of
// the Ecma and its immediate child values that are leaves (if there are any).
//
// In other words, for any  b : BExpr  and  e : Ecma
//
//    eval b e = eval b e'
//
// where e' is e with all of its non-leaf child values replaced
// with the representation for null.
let rec eval (expr : BExpr) (e : Ecma) : bool =
  match expr with
  | True -> true
  | Not expr -> not (eval expr e)
  | And (expr1, expr2) -> (eval expr1 e) && (eval expr2 e)
  | Or (expr1, expr2) -> (eval expr1 e) || (eval expr2 e)
  | HasKey key ->
    match e with | Object o -> List.exists (fun (n, _) -> n = key) o | _ ->false
  | HasStringValue str ->
    match e with
    | String s -> s = str
    | Object o -> List.exists (fun (_, v) -> match v with | String s -> s = str | _ -> false) o
    | List l -> List.exists (fun v -> match v with | String s -> s = str | _ -> false) l
    | _ -> false
  | HasNumericValueInRange (first, last) ->
    match e with
    | Float n -> first <= n && n <= last
    | Object o -> List.exists (fun (_, v) -> match v with | Float n -> first <= n && n <= last | _ -> false) o
    | List l -> List.exists (fun v -> match v with | Float n -> first <= n && n <= last | _ -> false) l
    | _ -> false
  | HasBoolValue v ->
    match e with
    | Bool b -> b = v
    | Object o -> List.exists (fun (_, x) -> match x with | Bool b -> b = v | _ -> false) o
    | List l -> List.exists (fun x -> match x with | Bool b -> b = v | _ -> false) l
    | _ -> false
  | HasNull ->
    match e with
    | Null -> true
    | Object o -> List.exists (fun (_, v) -> match v with | Null | Object _ | List _  -> true | _ -> false) o
    | List l -> List.exists (fun v -> match v with | Null | Object _ | List _ -> true | _ -> false) l
    | _ -> false



type Description = Key   of string
                 | Index of int

type Path = Description list


// 3. Define the function
//
// select : Selector -> Ecma -> (Path * Ecma) list
//
// that computes the set of values in the given Ecma described by the
// given Selector. The result is a list of pairs where the second
// component is a selected value and the first component is the full path
// to this value.
//
// The path to the root value is the empty list.
//
// If you follow a child value of an object, then you add the key of
// that value to the path. If you follow a child of an array, then you
// add the index of that value in the array to the path (the oldest value
// has index 0).
//
// The order of values in the result list must respect the order of values
// in the given Ecma. More precisely, in the result list:
// - a value must appear before any of its children
// - a value must not appear before its older siblings and their
//   descendants
//
// This task is similar to evaluating a BExpr on an Ecma. The difference is
// that instead of a BExpr we have a Selector and instead of a bool we
// compute a (Path * Ecma) list. In this case we also consider child
// values.
let test = Object [
  ("abc", Bool false);
  ("xs", List[
    Object [("a", String "a")]; Float 1.0; Bool true;
    Object [("b", String "b")]]);
  ("xyz", Object [("a", Float 1.0); ("b", Object [("b", String "b")])]);
  ("ws", Bool false)
]

let rec select (s : Selector) (e : Ecma) : (Path * Ecma) list =
  match s with
  | Match expr ->
    if eval expr e then [([], e)] else []
  | Sequence (s1, s2) ->
    match e with
    | Object o -> 
      let selectHelper = fun (n, v) ->
        let s1Res = select s1 e  // Or v, desc fucked up again
        let doS2 = fun (path, ecma) ->
          let s2Res = select s2 ecma
          List.map (fun (pth, ecm) -> ((Key n) :: (path @ pth), ecm)) s2Res
        if s1Res <> [] then List.collect doS2 s1Res else []
      List.collect selectHelper o
    | List l -> 
      let selectHelper = fun (i, acc) v ->
        let s1Res = select s1 e  // Or v, desc fucked again
        let doS2 = fun (path, ecma) ->
          let s2Res = select s2 ecma
          List.map (fun (pth, ecm) -> ((Index i) :: (path @ pth), ecm)) s2Res
        if s1Res <> [] then (i + 1, acc @ (List.collect doS2 s1Res)) else (i + 1, acc)
      snd (List.fold selectHelper (0, []) l)
    | _ -> []
  | OneOrMore s ->
    match e with
    | Object o -> (select s e) @ (List.collect (fun (_, v) -> select s v) o)
    | List l -> (select s e) @ (List.collect (select s) l)
    | _ -> select s e






// 4. Define the function
//
// update :  (string -> string)
//        -> (float  -> float)
//        -> Selector
//        -> Ecma
//        -> Ecma
//
// such that
//
//    update su nu s e
//
// evaluates to an Ecma that is otherwise the same as e except that,
// for the values selected by s, the string values and numeric values
// of that value have been updated according to the functions su and nu.

let rec mapPath (f : Ecma -> Ecma option) (path : Path) (e : Ecma) : Ecma option =
  match path with
  | [] -> f e
  // | [Key p] -> 
  //   match e with
  //   | Object o -> 
  //     Object (List.fold (fun acc (n, v) -> 
  //       if n = p then match f v with | Some v -> (n, v) :: acc | None -> acc
  //       else (n, v) :: acc) [] o)
  //   | _ -> Some e
  // | [Index p] -> 
  //   match e with
  //   | List l -> 
  //     List (snd (List.fold (fun (i, acc) v -> 
  //       if i = p then (i+1, match f v with | Some v -> v :: acc | None -> acc) 
  //       else (i+1, v :: acc)) (0, []) l))
  //   | _ -> Some e
  | (Key p) :: ps ->
    match e with
    | Object o ->
      Object (List.map (fun (n, v) -> if n = p then (n, (mapPath f ps v)) else (n, Some v)) o
      |> List.filter (fun v -> (snd v).IsSome)
      |> List.map (fun (n, v) -> (n, v.Value))) |> Some
    | _ -> Some e
  | (Index p) :: ps ->
    match e with
    | List l -> 
      List ((snd (List.fold (fun (i, acc) v -> 
        if i = p then (i+1, (mapPath f ps v) :: acc)
        else (i+1, (Some v) :: acc)) (0, []) l))
      |> List.filter (fun v -> v.IsSome)
      |> List.map (fun v -> v.Value)) |> Some
    | _ -> Some e

let rec map (f : Ecma -> Ecma option) (ps : Path list) (e : Ecma) : Ecma option =
  match ps with
  | [] -> Some e
  | p :: ps -> Option.bind (map f ps) (mapPath f p e)



let update (sFn : string -> string) (nFn : float -> float) (s : Selector) (e : Ecma) : Ecma =
  let mapVal v = Some (match v with | String s -> String (sFn s) | Float n -> Float (nFn n) | _ -> v)
  let paths = List.map fst (select s e)
  (map mapVal paths e).Value  // Very nice F#






// 5. Define the function
//
// delete : Selector -> Ecma -> Ecma option
//
// which removes from the given Ecma all values that are selected by
// the given Selector. Removing a value means removing the entire
// subtree rooted at that value.
//
// The result should be `None` when after the delete operation there
// is no `Ecma` value left. Otherwise use `Some`.
let delete (s : Selector) (e : Ecma) : Ecma option =
  let paths = List.map fst (select s e)
  map (fun _ -> None) paths e








// 6. Using the function update, define the functions
//
//   toZero : float -> Selector -> Ecma -> Ecma
//
// and
//
//   truncate : int -> Selector -> Ecma -> Ecma
//
// so that
//
//   toZero x s e
//
// evaluates to an Ecma that is otherwise the same as e except that the
// values selected by s have each of their numeric values y replaced by 0
// if y is in the range [-x, x].
//
//   truncate n s e
//
// evaluates to an Ecma that is otherwise the same as e except that the
// values selected by s have each of their string values y truncated to
// length n.
//
// These functions should not be defined recursively; define them in
// terms of update.
let toZero (n : float) (s : Selector) (e : Ecma) : Ecma = 
  update id (fun v -> if abs v < n then 0.0 else v) s e  // Or Sequence(s, HasNumericValueInRange(-n, n))

let truncate (n : int) (s : Selector) (e : Ecma) : Ecma =
  update (fun v -> if v.Length < n then v else v.Substring(0, n)) id s e





// 7. Using the function update, define the function
// 
//   mapEcma : (string -> string) -> (float -> float) -> Ecma -> Ecma
//
// such that
//
//   mapEcma f g e
//
// evaluates to an Ecma obtained by updating every value in the
// given Ecma value according to f and g.
//
// This function should not be defined recursively; define it in
// terms of update.
let mapEcma (strFn : string -> string) (nFn : float -> float) (e : Ecma) : Ecma =
  let s = OneOrMore (Match True)
  update strFn nFn s e
