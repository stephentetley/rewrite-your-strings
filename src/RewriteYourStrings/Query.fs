// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace RewriteYourStrings

[<AutoOpen>]
module Query =

    open System.Text.RegularExpressions

    open RewriteYourStrings.Internal
    open RewriteYourStrings.RewriteMonad


    let internal guardOperation (operation : RegexOptions -> string -> 'a option) 
                                (regexOpts : RegexOptions)
                                (input : string) : 'a option = 
        try 
            let ans = operation regexOpts input in ans
        with
        | _ -> None


    // ************************************************************************
    // Queries
    
    type StringQuery<'a> = StringRewriter<'a>

    let primitiveQuery (query : RegexOptions -> string -> 'a option) : StringQuery<'a> =
        rewrite { 
            let! source = getInput ()
            let! opts = askOptions ()
            match guardOperation query opts source with
            | None -> return! rewriteError "primitiveQuery"
            | Some ans -> return ans
        }

    let queryOption (query : string -> 'a option) : StringQuery<'a> =
        primitiveQuery (fun _ input -> query input)

    let stringQuery (query : string -> 'a) : StringQuery<'a> =
        primitiveQuery (fun _ input -> query input |> Some)


    let length : StringQuery<int> = 
        stringQuery String.length
        

    let equals (target:string) : StringQuery<bool> = 
        stringQuery (fun input -> input = target)

    let forallChars (predicate : char -> bool) : StringQuery<bool> = 
        stringQuery (String.forall predicate)


    let isLiteralMatch (sub : string) : StringQuery<bool> = 
        stringQuery <| fun s -> s.Contains(sub)

    let isRegexMatch (pattern:string) : StringQuery<bool> = 
        primitiveQuery 
            <| fun opts input -> 
                Regex.IsMatch( input = input
                                , pattern = pattern
                                , options = opts) |> Some


    let matchValue (pattern:string) : StringQuery<string> = 
        primitiveQuery 
            <| fun opts input -> 
                        let rmatch = Regex.Match( input = input
                                                , pattern = pattern
                                                , options = opts)
                        if rmatch.Success then Some rmatch.Value else None


    let levenshteinDistance (target:string) : StringQuery<int> = 
        stringQuery <| fun s -> Levenshtein.distance s target


