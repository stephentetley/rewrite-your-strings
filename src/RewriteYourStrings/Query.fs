// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace RewriteYourStrings

[<AutoOpen>]
module Query =

    open System.Text.RegularExpressions

    open RewriteYourStrings.Internal
    open RewriteYourStrings.RewriteMonad



    // ************************************************************************
    // Queries
    
    type StringQuery<'a> = StringRewriter<'a>

    let query (query : RegexOptions -> string -> 'a) : StringRewriter<'a> =
        rewrite { 
            let! source = getInput ()
            let! opts = askOptions ()
            return! (liftOperation <| fun _ -> query opts source)
        }

    let length : StringQuery<int> = 
        getInput () |>> String.length
        

    let equals (target:string) : StringQuery<bool> = 
        query (fun _ input -> input = target)


    let isMatch (pattern:string) : StringQuery<bool> = 
        query <| fun opts input -> 
                    Regex.IsMatch( input = input
                                 , pattern = pattern
                                 , options = opts)


    let matchValue (pattern:string) : StringQuery<string> = 
        fromOptionM 
            << query 
            <| fun opts input -> 
                        let rmatch = Regex.Match( input = input
                                                , pattern = pattern
                                                , options = opts)
                        if rmatch.Success then Some rmatch.Value else None


    let levenshteinDistance (target:string) : StringQuery<int> = 
        query <| fun _ input -> Levenshtein.distance input target


