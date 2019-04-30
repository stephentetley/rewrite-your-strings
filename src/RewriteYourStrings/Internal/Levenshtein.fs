// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace RewriteYorStrings.Internal


[<RequireQualifiedAccess>]
module Levenshtein = 


    let inline private arraySwap (a:int[]) (b:int[]) : unit = 
        for i = 0 to (a.Length-1) do a.[i] <- b.[i]

    /// This is the Wikipedia two-matrix rows algorithm
    /// https://en.wikipedia.org/wiki/Levenshtein_distance#Iterative_with_two_matrix_rows
    let distance (s:string) (t:string) : int =
        let m = s.Length
        let n = t.Length
        let v0 : int [] = Array.zeroCreate (n+1)
        let v1 : int [] = Array.zeroCreate (n+1)
        for i = 0 to n do v0.[i] <- i
        for i = 0 to (m-1) do
            v1.[0] <- i + 1
            for j = 0 to (n-1) do
                let substitutionCost = if s.[i] = t.[j] then 0 else 1
                let a = v1.[j] + 1
                let b = v0.[j + 1] + 1
                let c = v0.[j] + substitutionCost
                v1.[j+1] <- min a (min b c)
            arraySwap v0 v1
        v0.[n]