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
  let currOk = List.distinct subdirs = subdirs && t.name.Length > 0
  let childOk = List.forall fsTreeWf t.children
  currOk && childOk


let pathWf (p : Path) : bool =
  p <> [] && List.forall (fun (v : string) -> v.Length > 0) p




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
  (pathWf p && fsTreeWf t && false) ==> fsTreeWf (create p t)





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
let wfTrees : Gen<FsTree> =
  let rec wfTree k : Gen<FsTree> =
    match k with
    | 0 -> gen {
        let! n = Arb.Default.NonEmptyString().Generator
        return { name = string n; children = [] }
      }
    | _ ->
      gen {
        let! n = Arb.Default.NonEmptyString().Generator
        let! i = Gen.choose (1, 4)
        let c = Gen.sample 1 5 (wfTree (k - i))
        return { name = string n; children = c}
      }
  Gen.sized wfTree


let wfPaths : Gen<Path> = 
  let rec wfPath k =
    gen {
      let! p = Arb.Default.List<string>().Generator
      let! ps = wfPath (k - 1)
      return if k <= 0 then [string p] else (string p) :: ps
    }
  Gen.sized wfPath





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
let deleteIsWellFormed (p : Path) (t : FsTree) : bool = true






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
let createCreates (p : Path) (t : FsTree) : bool = true





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
let deleteDeletes (p : Path) (t : FsTree) : bool = true




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
let showShowsEverything (t : FsTree) : bool = true



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
let createAndDelete (t : FsTree) (p1 : Path) (p2 : Path) : Property = failwith "todo"
