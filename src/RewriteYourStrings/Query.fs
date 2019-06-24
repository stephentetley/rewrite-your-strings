﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace RewriteYourStrings

[<AutoOpen>]
module Query =

    open System.Text.RegularExpressions

    open RewriteYourStrings.Internal
    open RewriteYourStrings.RewriteMonad



    // ************************************************************************
    // Queries
    
    type StringQuery<'a> = RewriteMonad<'a>

    let queryInput (errMsg:string) 
                   (query:RegexOptions -> string -> 'a) : RewriteMonad<'a> =
        RewriteMonad <| fun opts input ->
            try 
                Ok(input, query opts input)
            with
            | _ -> Error errMsg

    let length : StringQuery<int> = 
        getInput () |>> String.length
        

    let equals (target:string) : StringQuery<bool> = 
        queryInput "equals" (fun _ input -> input = target)


    let isMatch (pattern:string) : StringQuery<bool> = 
        queryInput "equals" 
            <| fun opts input -> Regex.IsMatch(input = input
                                              , pattern = pattern
                                              , options = opts)

    let levenshteinDistance (target:string) : StringQuery<int> = 
        queryInput "levenshteinDistance"
            <| fun _ input -> Levenshtein.distance input target


