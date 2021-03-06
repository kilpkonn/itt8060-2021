(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 7: property-based testing

  ------------------------------------
  Name: Tavo Annus
  Student ID: taannu
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework7/coursework7.fsx .
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is
  incorrect it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

  NB! Do not delete the stubs we have provided! If you did not manage
  to complete the implementation then keep the incomplete stub and
  make sure that it still is typeable as described in the question.
*)

#if INTERACTIVE 
//#r    "FsCheck.dll" // in .Net Framework and .Net core versions prior to 5.0
#r    "nuget: FsCheck, Version=2.14.3" // You can also use nuget directly with F# 5 and 6.
#load "FileSystem.fs"
#endif

open FsCheck

open FileSystem



(*
   Question 1

   Define the predicates

   fsTreeWf : FsTree -> bool

   pathWf   : Path -> bool

   that evaluate to true on precisely those values of FsTree and Path
   that are well-formed.

   The directory names in a well-formed Path cannot be empty.

   A value of type FsTree is well-formed when:
   - the path to any directory in the filesystem is well-formed
   
   - the filesystem is deterministic (for any given path, there is at
     most one directory in the filesystem with that path)

   Define these predicates so that they traverse their input only
   once.
*)
let rec fsTreeWf (t : FsTree) : bool =
  let subdirs = t.children |> List.map (fun v -> v.name)
  let currOk = List.distinct subdirs = subdirs && t.name <> ""
  let childOk = List.forall fsTreeWf t.children
  currOk && childOk


let pathWf (p : Path) : bool =
  p <> [] && List.forall (fun v -> v <> "") p




(*
   Question 2

   Define an FsCheck property

   createIsWf : Path -> FsTree -> Property

   which checks that creating a well-formed path p in a well-formed
   filesystem fs results in a well-formed filesystem.

   Define this using a conditional property (==>).

   Is this a good way to check such a property? Why? No, overhead

   What percentage of the generated test inputs trivially satisfy this
   property? Depends on depth and width but big
*)
let createIsWf (p : Path) (t : FsTree) : Property =
  (pathWf p && fsTreeWf t) ==> (lazy fsTreeWf (create p t)) |@ $"${p}  <->  ${t}"





(*
   Question 3

   Define a generator

   wfTrees : Gen<FsTree>

   that only generates well-formed filesystems.


   Define a generator

   wfPaths : Gen<Path>

   that only generates well-formed paths.


   Define these generators in such a way that none of the generated
   data is wasted (i.e., discarded). In other words, all the data that
   you (randomly) generate must be used in the the output of the
   generator.

   You may wish to use the predicates defined above to check that the
   generators indeed only generate well-formed data. Or that the
   predicates are defined correctly.
*)
let randStr =
  "qwertyuiopasdfghjklzxcvbnm,.;:_-+0123456789".ToCharArray()
      |> Gen.elements
      |> Gen.nonEmptyListOf
      |> Gen.map (List.toArray >> System.String)


let wfTrees : Gen<FsTree> =
  let rec wfTree (n : string) (k : int) : Gen<FsTree> =
    match k with
    | m when m <= 0 -> Gen.constant { name = n; children = [] }
    | _ ->
      gen {
        let! i = Gen.choose (1, 2 * k)
        let names = Gen.sample 10 (i) randStr |> set |> Set.toList
        let c = names |> List.collect (fun m -> Gen.sample 1 1 (wfTree m (k / 4)))
        return { name = n; children = c}
      }
  Gen.sized (wfTree "root")


let wfPaths : Gen<Path> = 
  Gen.listOf randStr |> Gen.map (fun ps -> (Gen.sample 5 1 randStr) @ ps)





(*
   Question 4

   Define an FsCheck property

   deleteIsWellFormed : Path -> FsTree -> bool

   which checks that given
   
   p  : Path
   fs : FsTree

   we have that the result of deleting p from fs is well-formed.

   You may assume that this property is only used with "well-formed"
   generators (meaning that p and fs are well-formed).
*)
let deleteIsWellFormed (p : Path) (t : FsTree) : bool =
  fsTreeWf (delete p t)






(*
   Question 5

   Define an FsCheck property

   createCreates : Path -> FsTree -> bool

   which checks that given

   p  : Path
   fs : FsTree

   we have that the path p is included (exactly once) in the
   result of show after we have created the directory p in fs.

   You may assume that this property is only used with "well-formed"
   generators (meaning that p and fs are well-formed).
*)
let createCreates (p : Path) (t : FsTree) : bool =
  (List.filter (fun v -> v = p) (show (create p t)) |> List.length = 1)





(*
   Question 6

   Define an FsCheck property

   deleteDeletes : Path -> FsTree -> bool

   which checks that given

   p  : Path
   fs : FsTree

   we have that the path p is not in the result of show after we have
   deleted the directory p from fs.

   You may assume that this property is only used with "well-formed"
   generators (meaning that p and fs are well-formed).
*)
let deleteDeletes (p : Path) (t : FsTree) : bool =
  (not (List.contains p (show (delete p t))))




(*
   Question 7

   Define an FsCheck property

   showShowsEverything : FsTree -> bool

   which checks that given an
   
   fs : FsTree

   we have that by deleting one by one all of the items in the result
   of show fs we end up with an empty filesystem.

   You may assume that this property is only used with "well-formed"
   generators (meaning that fs is well-formed).
*)
let showShowsEverything (t : FsTree) : bool =
  (List.fold (fun acc p -> delete p acc) t (show t) |> isEmpty)



(*
   Question 8

   Define an FsCheck property

   createAndDelete : FsTree -> Path -> Path -> Property

   which checks that given
   
   fs : FsTree
   p1 : Path
   p2 : Path

   we have that, if p1 is not a prefix of p2, then

   1) creating directory p1 in fs
   2) creating directory p2 in the result
   3) deleting p1 from the result

   produces a filesystem that still contains p2.

   You may assume that this property is only used with "well-formed"
   generators (meaning that fs, p1 and p2 are well-formed).
*)
let createAndDelete (t : FsTree) (p1 : Path) (p2 : Path) : Property =
  let rec startsWith xs ys =
    match (xs, ys) with
    | ([], _) -> true
    | (x :: xs, y :: ys) -> if x = y then startsWith xs ys else false
    | _ -> false
  (not (startsWith p1 p2) && pathWf p1 && pathWf p2) ==>  // NOTE: <- why this no work?
    (lazy (t |> create p1 |> create p2 |> delete p1 |> show |> List.contains p2))
