7(*

  ITT8060 -- Advanced Programming 2017
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 0: Getting started

  ------------------------------------
  Name: Tavo Annus
  Student ID: 211564IAPM
  ------------------------------------

  Answer the questions below.  You answers to questions 2--8 should be
  correct F# code written after the question. The F# code for question
  1 is written for you and serves as an example. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will NOT be graded but we encourage you to do it,
  you will not succeed in this course if you don't practice, and
  there's no time like the present! Also, you may find that parts of
  it appear in later courseworks.

*)
// module Coursework0
// 0. Find your way to the fsharp interactive (fsi) command prompt.
//    I.e. log in to a lab machine and start Visual Studio, install
//    VS/Mono on your laptop, etc.

// 1. Load the following function into fsi

let greeting name = printfn "Hello: %s" name 

// 2. Run the function 'greeting' and say hello to yourself.
//    Type the expression below.
greeting "Tavo"

// 3. Define
//    'splitAtChar : text:string -> sep:char -> list<string>'
let splitAtChar (text:string) (sep:char) : (string List)= text.Split sep |> Array.toList 

// 4. Modify splitAtSpaces to use splitAtChar

// 5. Define 'sentenceCount : text:string -> int'
let sentenceCount (text:string) = (text.Split [|'.'; '!'; '?'|] |> Array.length) - 1

// 6. Define 'stats : text:string -> unit'
//    which prints the same stats as showWordCount +
//    the number of sentences and average length of sentences
//    hint: try float: 'int -> float'
let stats (text: string) = 
  let sentences : int = sentenceCount text
  printf "Sentences: %d\n" sentences
  printf "Avg length: %f\n" <| (text.Split ' ' |> Array.length |> float) / (float sentences)
  ()

// 7. Use the 'http' function from the lecture to download the file
//    http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt as a string
//    NOTE: you cannot use this function in tryfsharp. Instead you can
//    paste the text into your file as a string and process it locally
open System.IO
open System.Net
let http (url: string) = 
    let req = System.Net.WebRequest.Create url
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html


let resp = http "http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt"
File.WriteAllText ("./out.txt", resp)

