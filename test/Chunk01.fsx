// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
open System.Text


#load "..\src\RewriteYourStrings\Internal\Levenshtein.fs"
#load "..\src\RewriteYourStrings\RewriteMonad.fs"
#load "..\src\RewriteYourStrings\Query.fs"
#load "..\src\RewriteYourStrings\Transform.fs"
open RewriteYourStrings.RewriteMonad
open RewriteYourStrings

type Chunk = 
    | Island of string
    | Water of string



// Should coalesce water...
let characterChunker (waterChars : char []) (source : string) : Chunk list = 
    let make1 (chars : char list) : string option = 
        if List.isEmpty chars then 
            None 
        else chars |> List.rev |> List.toArray |> System.String |> Some        
    let rec water (input : char list) acc cont = 
        match input with
        | [] ->  
            match make1 acc with
            | Some tok -> cont [Water tok]
            | None -> cont []
        | c1 :: rest -> 
            if Array.contains c1 waterChars then
                water rest (c1::acc) cont
            else
               match make1 acc with
               | Some tok -> island input [] (fun xs -> cont (Water tok::xs))
               | None -> island input [] cont
    and island (input : char list) acc cont = 
        match input with
        | [] ->  
            match make1 acc with
            | Some tok -> cont [Island tok]
            | None -> cont []
        | c1 :: rest -> 
            if Array.contains c1 waterChars then
               match make1 acc with
               | Some tok -> water input [] (fun xs -> cont (Island tok::xs))
               | None -> water input [] cont                   
            else
               island rest (c1::acc) cont
    water (source.ToCharArray() |> Array.toList) [] (fun xs -> xs)
                       
let unchunk (chunks : Chunk list) : string =     
    let build1 (sb:StringBuilder) tok = 
        match tok with 
        | Island s -> sb.Append s
        | Water s -> sb.Append s
    List.fold build1 (new StringBuilder ()) chunks  
        |> fun sb -> sb.ToString()


let mapIsland (mapper : string -> string) (source : Chunk list) : Chunk list = 
    let rec work xs cont = 
        match xs with 
        | [] -> cont []
        | Water tok :: rest -> 
            work rest (fun vs ->
            cont (Water tok :: vs))
        | Island tok :: rest -> 
            let ans1 = mapper tok |> Island
            work rest (fun vs -> 
            cont (ans1 :: vs))
    work source (fun xs -> xs)



let camel1 (input : string) : string = 
    if input.Length > 0 then
        let front : string = input.Substring(0,1).ToUpper()
        let tail = input.Substring(1).ToLower()
        front + tail
    else
        input

let camelCase (input : string) : string = 
    characterChunker [| ' ' |] input 
        |> mapIsland camel1
        |> unchunk

let camelCase01 () = 
    camelCase "hello world this is a message."