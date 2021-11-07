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
  | Str of string
  | Array of Ecma list
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

let mkString (value: string) = Str value

(*
Define the function

 mkArray : Ecma list -> Ecma

that creates a representation for an array whose elements are
represented by the given list of `Ecma` values.
*)

let mkArray (arr : Ecma list) = Array arr

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
  | Array items -> Array (items @ [v])
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
let b2 = And (Not (HasNumericValueInRange (5.0, 5.0)), HasNumericValueInRange (-5.0, 5.0))
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
    | Str s -> s = str
    | Object o -> List.exists (fun (_, v) -> match v with | Str s -> s = str | _ -> false) o
    | Array l -> List.exists (fun v -> match v with | Str s -> s = str | _ -> false) l
    | _ -> false
  | HasNumericValueInRange (first, last) ->
    match e with
    | Float n -> first <= n && n <= last
    | Object o -> List.exists (fun (_, v) -> match v with | Float n -> first <= n && n <= last | _ -> false) o
    | Array l -> List.exists (fun v -> match v with | Float n -> first <= n && n <= last | _ -> false) l
    | _ -> false
  | HasBoolValue v ->
    match e with
    | Bool b -> b = v
    | Object o -> List.exists (fun (_, x) -> match x with | Bool b -> b = v | _ -> false) o
    | Array l -> List.exists (fun x -> match x with | Bool b -> b = v | _ -> false) l
    | _ -> false
  | HasNull ->
    match e with
    | Null -> true
    | Object o -> List.exists (fun (_, v) -> match v with | Object [] | Array [] -> false | Null | Object _ | Array _  -> true | _ -> false) o
    | Array l -> List.exists (fun v -> match v with Object [] | Array [] -> false | Null | Object _ | Array _ -> true | _ -> false) l
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
  ("xs", Array[
    Object [("a", Str "a")]; Float 1.0; Bool true;
    Object [("b", Str "b")]]);
  ("xyz", Object [("a", Float 1.0); ("b", Object [("b", Str "b")])]);
  ("ws", Bool false)
]

let st1 = Sequence (Match True, Sequence (Match True, Match True))
let et1 = Object [("a", Object [
  ("age", Str "oldest")]); 
  ("b", Object [("age", Str "middle")]);
  ("a", Object [("age", Str "oldest")])
]
let st2 = Match (Or (HasKey "a", Or (HasKey "age", HasStringValue "youngest")))
let et2 = Object [
  ("a", Object [("age", Str "oldest")]);
  ("b", Object [("age", Str "middle")]);
  ("a", Object [("age", Str "oldest")])
]

let st3 = Sequence (Match True, Match True)
let et3 = Array [Array []; Float 1.154041161]

let rec select (s : Selector) (e : Ecma) : (Path * Ecma) list =
  let prefix p ps =  List.map (fun (a, b) -> (p :: a, b)) ps
  match s with
  | Match expr ->
    if eval expr e then [([], e)] else []
  | Sequence (s1, s2) ->
    let s1Res = select s1 e
    let helper (path, ecma) : (Path * Ecma) list =
      let doS2 n v : (Path * Ecma) list = List.map (fun (pth, ecm) -> (path @ (n :: pth), ecm)) (select s2 v)
      match ecma with
      | Object o -> List.collect (fun (n, v) -> doS2 (Key n) v) o
      | Array l -> snd (List.fold (fun (i, acc) v -> (i+1, (doS2 (Index i) v) @ acc)) (0, []) l) |> List.rev
      | _ -> [] // v -> List.map (fun (pth, ecm) ->  (path @ pth, ecm)) (select s2 v)
    List.collect helper s1Res
  | OneOrMore (OneOrMore s) -> select (OneOrMore s) e
  | OneOrMore s ->
    select s e @ select (Sequence (s, (OneOrMore s))) e
   //  match e with
   //  | Object o -> 
   //    (select s e) @ (List.collect (fun (n, v) -> prefix (Key n) (select (OneOrMore s) v)) o)
   //  | Array l ->
   //    // if l = [] then [] else failwith $"s: ${s.ToString()} e: ${e.ToString()}"
   //    let helper (i : int, acc : (Path * Ecma) list) (v : Ecma) : int * ((Path * Ecma) list) = 
   //      (i+1, (prefix (Index i) (select (OneOrMore s) v)) @ acc)
   //    (select s e) @ (snd (List.fold helper (0, []) l))
   //  | _ -> select s e





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

let su = Sequence (Sequence (Match True, Match True), Match True)
let eu = Object [("a", Object [("age", Str "oldest"); ("height", Float 1.9); ("ok", Bool false)]); ("b", Object [("age", Str "middle"); ("height", Array [])]); ("a", Object [("age", Str "youngest"); ("height", Float 2.01); ("ok", Bool true)])]

let su2 = Match (HasKey " ,q=3U")
let eu2 = Object [
  (" ,q=3U", Object [("3)u:\R", Float -1.038539229)])
]  

let rec map (f : Ecma -> Ecma option) (s : Selector) (e : Ecma) : Ecma option =
  match s with
  | Match expr ->
    // printfn $"Eval: ${e.ToString()}"
    if eval expr e then f e else Some e
  | Sequence (s1, s2) ->
    let s1Res = select s1 e |> List.map fst
    let correctKey y xs = List.exists (fun xs -> match xs with | (Key x) :: _ -> x = y | [] -> true |  _ -> false) xs 
    let correctIdx y xs = List.exists (fun xs -> match xs with | (Index x) :: _ -> x = y | [] -> true |  _ -> false) xs
    let head xs = match xs with | x :: _ -> Some x | _ -> None
    let tail xs = match xs with _ :: xs -> xs | [] -> []
    let tailAll = List.map tail
    let filterPths n = List.filter (fun ys -> match ys with | y :: ys -> n = y | _ -> false)
    let rec doS2 (paths : Path list) e =
      // printfn $"pth: ${paths.ToString()}, s: ${s.ToString()}"
      match e with
      | Object o -> 
        List.foldBack (fun (n, v) acc ->
          if List.contains [] paths then
            match Option.bind (doS2 (tailAll (filterPths (Key n) paths))) (map f s2 v) with
            | Some v -> (n, v) :: acc
            | None -> acc
          else if not (correctKey n paths) then (n, v) :: acc
          else
            match doS2 (tailAll (filterPths (Key n) paths)) v with
            | Some v -> (n, v) :: acc
            | None -> acc
        ) o [] |> Object |> Some  // NOTE: Who decided to flip order of arguments for foldBack ?!?
      | Array l ->
        List.fold (fun (i, acc) v ->
          if List.contains [] paths then
            match Option.bind (doS2 (tailAll (filterPths (Index i) paths))) (map f s2 v) with
            | Some v -> (i+1, v::acc)
            | None -> (i+1, acc)
          else if not (correctIdx i paths) then (i+1, v :: acc)
          else
            match doS2 (tailAll (filterPths (Index i) paths)) v with
            | Some v -> (i+1, v :: acc)
            | None -> (i+1, acc)
        ) (0, []) l |> snd |> List.rev |> Array |> Some
      | e -> Some e  // map f s2 v // Or no?
    doS2 s1Res e
  | OneOrMore (OneOrMore s) -> map f (OneOrMore s) e
  | OneOrMore s ->
    // failwith $"s: ${s.ToString()} e: ${e.ToString()}"
    Option.bind (map f (Sequence (s, (OneOrMore s)))) (map f s e)


let update (sFn : string -> string) (nFn : float -> float) (s : Selector) (e : Ecma) : Ecma =
  let mapVal v = match v with 
                     | Str s -> Str (sFn s) 
                     | Float n -> Float (nFn n)
                     | _ -> v
  let mapVal v = match v with 
                     | Str s -> Str (sFn s) 
                     | Float n -> Float (nFn n)
                     | Object o -> List.map (fun (n, v) -> (n, mapVal v)) o |> Object
                     | Array a -> List.map (mapVal) a |> Array
                     | _ -> v

  (map (fun v -> mapVal v |> Some) s e).Value  // Very nice F#






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

let ds = Sequence (Sequence (Match True, Match True), Match (Or (HasKey "ok", HasBoolValue false)))
let de = Object [
  ("a", Object [
    ("age", Str "oldest");
    ("ok", Bool false);
    ("height", Float 1.9)
  ]); 
  ("b", Object [
    ("age", Str "middle"); 
    ("height", Array [])
  ]); 
  ("a", Object [
    ("age", Str "youngest");
    ("height", Float 2.01);
    ("ok", Bool true)
  ])
]

let delete (s : Selector) (e : Ecma) : Ecma option =
  let delFn = (fun v -> None)
  // let s = match s with
  //        | Sequence (s1, s2) -> Sequence (Sequence (s1, Match True), s2)
  //        | v -> v
  map delFn s e








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
  update (fun v -> if v = null || v.Length < n then v else v.Substring(0, n)) id s e
  // BUG:               ^ very good usage of types I see >:D





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

