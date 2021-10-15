(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: FSharpON

  ------------------------------------
  Name: Tavo Annus
  Tallinn University of Technology Student ID
  or Uni-ID: taannu
  ------------------------------------

  Answer the questions below. Your answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework4/coursework4.fsx.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect it will not be graded.

  Your solution must not use mutable state and imperative features
  like for loops.
  
*)


(*

The ECMA-404 standard specifies a textual syntax for structured data
interchange.

The specification is available here:
https://www.ecma-international.org/wp-content/uploads/ECMA-404_2nd_edition_december_2017.pdf

The goal of this coursework is to develop a partial implementation of
this specification. In particular, our first goal is to define in F#
the datatype(s) suitable for representing the abstract syntax tree of
this data interchange format. The second goal is to define some
operations on this representation.

*)

// We have the following type alias.

type Name = string

//// Task 1 ////

// Define the type `Ecma` for representing the possible values of the
// ECMA-404 interchange format.
//
// The type must satisfy the constraint `equality`.

type Ecma = 
  | Object of (Name * Ecma) list
  | Float of float
  | Bool of bool
  | String of string
  | List of Ecma list
  | Null



// Define the following functions for creating ECMA-404
// representations of the given data.

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






//// Task 2 ////

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






//// Task 3 ////

// Define the function
// 
//   countValues : Ecma -> int
// 
// that counts the number of ECMA values in the given representation.
// 
// Keep in mind that both objects and arrays are themselves values and
// may contain other values inside.
//
// Furthermore, the following should hold:
//
//   1 + countValues e <= countValues (addValue v e)             // if e is an array representation
//
//   1 + countValues e <= countValues (addNameValue (n, v) e)    // if e is an object representation
let rec countValues (e : Ecma) : int =
  match e with
  | Object o -> List.fold (fun acc (_, v) -> acc + countValues v) 1 o
  | List l -> List.fold (fun acc v -> acc + countValues v) 1 l
  | _ -> 1





//// Task 4 ////

type Path = Name list


// Define the function
// 
//   listPaths : Ecma -> Path list
// 
// that computes the full path for all the values in the given ECMA
// representation.
// 
// A path is just a list of names that take us from the root of the
// representation to a particular value.
// 
// For arrays, we consider the same path to take us to the array and to
// all of the elements in the array. Thus, for an array, we include the
// path to it and its elements only once in the result. 
// 
// If `e : Ecma` represents the following structure
// 
//   {
//     "abc" : false,
//     "xs"  : [ { "a" : "a" }, 1.0, true, { "b" : "b" }, false ],
//     "xyz" : { "a" : 1.0,
//               "b" : { "b" : "b" } },
//     "ws"  : [ false ]
//   }
// 
// then  `listPaths e` should result in
// 
//   [
//     [];
//     ["abc"];
//     ["xs"];
//     ["xs"; "a"];
//     ["xs"; "b"];
//     ["xyz"];
//     ["xyz"; "a"];
//     ["xyz"; "b"];
//     ["xyz"; "b"; "b"];
//     ["ws"]
//   ]
// 
// The ordering of paths in the result list matters:
// - paths to (sub)values in an array respect the order of elements in
//   the array
// 
// - paths to values in an object respect the order in which the values
//   were added to the object (most recently added appears last).
//
// Note that the empty list denotes the path to the root object.

let test = Object [
  ("abc", Bool false);
  ("xs", List[
    Object [("a", String "a")]; Float 1.0; Bool true;
    Object [("b", String "b")]]);
  ("xyz", Object [("a", Float 1.0); ("b", Object [("b", String "b")])]);
  ("ws", Bool false)
  ]

let rec listPaths (e : Ecma) : Path list =
  let prefixAll v xs = List.map (fun ys -> v :: ys) xs  // TODO: Why this broken: List.map ( (::) v) xs
  match e with
  | Object o -> [] :: List.collect (fun (name, value) -> prefixAll name (listPaths value)) o
  | List l -> [] :: List.filter (fun v -> match v with [] -> false | _ -> true) (List.collect (listPaths) l)
  | _ -> [[]]




//// Task 5 ////

// Define the function
// 
//   show : Ecma -> string
// 
// that computes a string representation of the given ECMA representation
// in such a way that the ordering requirements from the previous task are
// respected.
// 
// The result should not contain any whitespace except when this
// whitespace was part of a name or a string value.
let rec show (e : Ecma) : string =
  match e with
  | Object o -> "{" + (List.fold(fun acc (n, v) -> acc + (if acc = "" then "\"" else ",\"") + n + "\":"+ show v) "" o) + "}"
  | List l -> "[" + (List.fold(fun acc v -> acc + (if acc = "" then "" else ",") + show v) "" l) + "]"
  | String s -> "\"" + s + "\""
  | Null -> "null"
  | Float n -> string n
  | Bool b -> if b then "true" else "false"






//// Task 6 ////

// Define the function
// 
//   delete : Path list -> Ecma -> Ecma
// 
// so that
// 
//   delete ps e
// 
// evaluates to a representation `e'` that is otherwise the same as `e` but
// all name-value pairs with paths in the path list `ps` have been removed.
//
// When the user attempts to delete the root object, delete should throw
// an exception. Hint: use `failwith` in the appropriate case.
let testDelete = Object [
  ("abc", Bool false);
  ("abc", Bool false);
  ("xs", List[
    Object [("a", String "a")]; Float 1.0; Bool true;
    Object [("b", String "b")]]);
  ("xyz", Object [("a", Float 1.0); ("b", Object [("b", String "b")])]);
  ("ws", Bool false)
  ]


let rec deletePath (path : Path) (e : Ecma) : Ecma =
  match path with
  | [] -> failwith "Cannot delete root"
  | [p] -> 
    match e with
    | Object o ->
      Object (List.filter (fun (n, v) -> n <> p) o)
    | List l -> List (List.map (deletePath [p]) l)
    | e -> e
  | p :: ps ->
    match e with
    | Object o -> 
      Object (List.map (fun (n, v) -> if n = p then (n, (deletePath ps v)) else (n, v)) o)
    | List l -> List (List.map (fun v -> match v with
                                         | Object o -> deletePath (p :: ps) v 
                                         | List l -> deletePath (p :: ps) v 
                                         | o -> o) l)
    | e -> e

let rec delete (ps : Path list) (e : Ecma) : Ecma =
  match ps with
  | [] -> e
  | p :: ps -> delete ps (deletePath p e)



//// Task 7 ////

// Define the function
// 
//   withPath : Path list -> Ecma -> Ecma list
// 
// so that
// 
//   withPath ps e
// 
// evaluates to a list of object representations consisting of those
// objects in `e` that are represented by a path from the list `ps`.
// 
// The result list must respect the ordering requirements from Task 4.

// let rec selectPath (ps : Path) (e : Ecma) : Ecma =
//   match ps with
//   | [] -> e
//   | p :: ps ->
//     match e with
//     | Object o ->
//       Object (List.fold (fun acc (n, v) -> if n = p then (n, (selectPath ps v)) :: acc else acc) [] o)
//     | List l ->
//       List (List.fold (fun acc o -> match selectPath (p :: ps) o with
//                                     | Object [] -> acc
//                                     | Object x -> (Object x) :: acc
//                                     | _ -> acc) [] l)
//     | _ -> e // TODO: Check
// 
// let withPath (ps : Path list) (e : Ecma) : Ecma list =
//   List.map (fun p -> selectPath p e) ps

// let withPath (ps : Path list) (e : Ecma) : Ecma list =
//   List.map (fun p -> delete (List.filter ((<>) p) (listPaths e)) e) ps

let rec selectPath (ps : Path) (e : Ecma) : Ecma list =
  match ps with
  | [] -> match e with | Object o -> [e] | _ -> []
  | p :: ps ->
    match e with
    | Object o ->
      List.distinct (List.collect (fun (n, v) -> if n = p then selectPath ps v else []) o)
    | List l ->
      List.distinct (List.collect (fun o -> selectPath (p :: ps) o) l)
    | _ -> []

let withPath (ps : Path list) (e : Ecma) : Ecma list =
  List.distinct (List.collect (fun p -> selectPath p e) ps)
