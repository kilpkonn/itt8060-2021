(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

  ------------------------------------
  Name: Tavo Annus
  Tallinn University of Technology Student ID
  or Uni-ID: taannu
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2021 under your name, into a file coursework2/coursework2.fsx.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.

  Deadline for submitting the solution is September 24 AoE, 2021.
*)

// You are given a type BibliographyItem that has the following structure:
// string list * string * (int * int) * int
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents a pair containing the starting page number and ending page number of the paper.
// * The fourth field represents the year of publication

type BibliographyItem = string list * string * (int * int) * int

// 1. Create a value bibliographyData : BibliographyItem list that contains
// at least 7 different publications on your favourite topic from https://dblp.uni-trier.de/ 
// Please note that you need not read the whole papers, just pick 7 papers that look interesting to you from the database.
let bibliographyData: BibliographyItem list = [
  (["Jones, Simon Peyton"; "Vytiniotis, Dimitrios"; "Fitzgibbon, Andrew"; "Shaikhha, Amir"], "Efficient differentiable programming in a functional array-processing language.", (1, 30), 2019);
  (["Jones, Simon Peyton"; "Eisenbach, Susan"; "Field, Tony"; "Kiss, Csongor"], "Higher-order type-level programming in Haskell.", (1, 26), 2019);
  (["Jones, Simon Peyton"; "Ariola, Zena"; "Sullivan, Zachary"; "Downen, Paul"], "Codata in Action.", (119, 146), 2019);
  (["Jones, Simon Peyton"; "Ariola, Zena"; "Sullivan, Zachary"; "Downen, Paul"], "Making a faster Curry with extensional types.", (58, 70), 2019);
  (["Jones, Simon Peyton"; "Breitner, Joachim"; "Eisenberg, Richard"], "Type variables in patterns.", (94, 105), 2018);
  (["Jones, Simon Peyton"; "Vytiniotis, Dimitrios"; "Hage, Jurriaan"; "Serrano, Alejandro"], "Guarded impredicative polymorphism.", (783, 796), 2018);
  (["Jones, Simon Peyton"; "Gordon, Andrew"; "Sarkar, Advait"], "Calculation View: multiple-representation editing in spreadsheets.", (85, 93), 2018)
  ]


// 2. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns 
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You need to use the support honouring the default culture for ordering strings, i.e. the built in
// compare does not work properly by default. Look at the
// documentation of the .Net comparison for strings: System.String.Compare
// If the first authors are the same
// then the precedence should be determined by the next author.
// Please note that your implementation should be recursive over the input lists.
//
// The sort order in .Net is defined using System.Globalization.CultureInfo:
// https://docs.microsoft.com/en-us/dotnet/api/system.globalization
// Please note that your solution should not force a particular sort order!
let rec compareLists (xs : string list) (ys : string list) : int =
  match (xs, ys) with
    | ([], []) -> 0
    | ([],  _) -> -1
    | (_,  []) -> 1
    | (x :: xs, y :: ys) -> match System.String.Compare(x, y) with  // <- Can we write it in "functional" way?
                            | 0 -> compareLists xs ys
                            | n -> n

// 3. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
// Use solution from task 3.
let compareAuthors ((xs, _, _, _) : BibliographyItem) ((ys, _, _, _) : BibliographyItem) : int = compareLists xs ys

// 4. Make a function
// compareAuthorsYears : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are 
// the same then according to years.
let compareAuthorsYears ((xs, _, _, yearA) : BibliographyItem) ((ys, _, _, yearB) : BibliographyItem) : int = 
  match compareLists xs ys with
  | 0 -> compare yearA yearB
  | n -> n

// 5. Make a function 
// sortBibliographyByNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the number of pages in the
// publication in ascending order.
// If two items are at the same level in the sort order, their order should be preserved.

let rec merge (xs : 'a list) (ys : 'a list) =
  match (xs, ys) with
  | ([], ys) -> ys
  | (x :: xs, ys) -> x :: (merge xs ys)

let rec filter (f: 'a -> bool) (xs : 'a list) =
  match xs with
  | [] -> []
  | x :: xs -> 
    match f x with
    | true -> x :: (filter f xs)
    | false -> filter f xs

let rec sortBy (f: 'a -> 'a -> int) (xs : 'a list) =
  match xs with
  | [] -> []
  | x :: xs ->
    let smaller = filter ((<) 0 << f x) xs
    let bigger = filter ((>) 0 << f x) xs
    let same = x :: filter ((=) 0 << f x) xs
    merge (sortBy f smaller) (merge same (sortBy f bigger))

let sortBibliographyByNumPages (xs : BibliographyItem list) : BibliographyItem list =
  sortBy (fun (_, _, (s1, e1), _) (_, _, (s2, e2), _) -> (e1 - s1) - (e2 - s2)) xs

// 6. Make a function 
// sortBibliographyByAuthorNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and number of pages in the publication in ascending order
// If two items are at the same level in the sort order, their order should be preserved.
let sortBibliographyByAuthorNumPages (xs : BibliographyItem list) : BibliographyItem list =
  let cmp = fun (a1, _, (s1, e1), _) (a2, _, (s2, e2), _) -> 
    match compareLists a1 a2 with
    | 0 -> (e1 - s1) - (e2 - s2)
    | n -> n
  sortBy cmp xs


// 7. Make a function
// groupByAuthor : BibliographyItem list -> (string * BibliographyItem list) list
// where the return list contains pairs where the first element is the name of a single 
// author and the second element a list of bibliography items that the author has co-authored.

let rec contains (x : 'a) (xs : 'a list) =
  match xs with
  | [] -> false
  | y :: ys -> if y = x then true else contains x ys

let rec groupByAuthor (items : BibliographyItem list) : (string * BibliographyItem list) list =
  match items with
  | [] -> []
  | x :: xs ->
    let (authors, _, _ ,_) = x
    let rec filterItems = fun (author : string) (bs : BibliographyItem list) -> // ((string, BibliographyItem list), BibliographyItem list)
      match bs with
      | [] -> (author, [])
      | b :: bs ->
        let (authors, _, _, _) = b
        match contains author authors with
        | true -> 
          let (_, rest) = filterItems author bs
          (author, b :: rest)
        | false -> filterItems author bs
    let rec authorsAcc (authors: string list) (xs: BibliographyItem list) = 
      match xs with
      | [] -> []
      | x :: xs ->
        match authors with
        | a :: authors -> 
          let (_, filtered) = filterItems a xs
          let rest = authorsAcc authors (x :: xs) 
          let rest = filter (fun (v, _) -> not (System.String.Equals(a, v))) rest
          (a, x :: filtered) :: rest
        | [] -> groupByAuthor xs
    authorsAcc authors (x :: xs)

