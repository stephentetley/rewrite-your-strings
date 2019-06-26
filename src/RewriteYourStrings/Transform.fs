// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace RewriteYourStrings

[<AutoOpen>]
module Transform =

    open System.Text.RegularExpressions

    open RewriteYourStrings.RewriteMonad
    
    

    type Rewrite = StringRewriter<unit>

    /// TODO we need a naming scheme.
    /// e.g Should we follow KURE, then we are unlikely to have name clashes?
    /// This might be uncharacteristic for F# though

    let modify (operation:string -> string) : Rewrite =
        rewrite { 
            let! source = getInput ()
            return! setInput (operation source)
        }

    let identity : Rewrite = 
        mreturn ()

    let constR (literal : string) = 
        modify (fun _ -> literal)

    // ************************************************************************
    // Combinators 


    let either (ma:StringRewriter<'a>) (mb:StringRewriter<'a>) : StringRewriter<'a> = 
        progressive ma <||> progressive mb

    let choice (rewriters:StringRewriter<'a> list) : StringRewriter<'a> = 
        let rec work (fs:StringRewriter<'a> list) = 
            match fs with
            | [] -> rewriteError "choice"
            | (mf :: rest) -> altM (progressive mf) (work rest)
        work rewriters

    let whenM (cond : StringQuery<bool>) 
              (successR : Rewrite) = 
        rewrite { 
            let! ans = cond
            if ans then 
                return! successR
            else 
                return! identity
        } 

    let unlessM (cond : StringQuery<bool>) 
                (failureR : Rewrite) = 
        rewrite { 
            let! ans = cond
            if ans then 
                return! identity
            else 
                return! failureR
        } 





    let withInput (errMsg:string) (operation:RegexOptions -> string -> string) : Rewrite =
        StringRewriter <| fun opts input ->
            try 
                let output = operation opts input in Ok((), output)
            with
            | _ -> Error errMsg

    let stringMap (errMsg:string) (charOp:char -> char) : Rewrite =
        withInput errMsg <| fun _ s -> String.map charOp s 

    let append (tail : string) : Rewrite = 
        modify (fun s -> s + tail)

    let trim : Rewrite = 
        withInput "trim" <| fun _ s -> s.Trim()

    let trimStart : Rewrite = 
        withInput "trimStart" <| fun _ s -> s.TrimStart()

    let trimEnd : Rewrite = 
        withInput "trimEnd" <| fun _ s -> s.TrimEnd()

    let padLeft (totalWidth:int) (paddingChar:char) : Rewrite = 
        withInput "padLeft" <| fun _ s -> s.PadLeft(totalWidth, paddingChar)

    let padRight (totalWidth:int) (paddingChar:char) : Rewrite = 
        withInput "padRight" <| fun _ s -> s.PadRight(totalWidth, paddingChar)

    let toUpper : Rewrite = 
        stringMap "toUpper" System.Char.ToUpper

    let toLower : Rewrite = 
        stringMap "toUpper" System.Char.ToLower

    let takeLeft (len : int) : Rewrite = 
        modify (fun s -> s.Substring(0, len))

    let dropLeft (len : int) : Rewrite = 
        modify (fun s -> s.Substring(len))

    let takeRight (len : int) : Rewrite = 
        rewrite { 
            let! size = length
            return! modify (fun s -> s.Substring(size - len))
        }
        

    let dropRight (len : int) : Rewrite = 
        rewrite { 
            let! size = length
            return! modify (fun s -> s.Substring(0, size - len))
        }

    /// Returns input string if nothing is replaced.
    let replaceAllRe (pattern:string) 
                     (replacement:string) : Rewrite = 
        withInput "replaceAllRe" <| fun opts input -> 
            let regexp = new Regex(pattern = pattern, options = opts) 
            regexp.Replace(input = input, replacement = replacement)


    /// Returns input string if nothing is replaced.
    let replaceCountRe (pattern:string) 
                       (count:int)
                       (replacement:string) : Rewrite = 
        withInput "replaceCountRe" <| fun opts input -> 
            let regexp = new Regex(pattern = pattern, options = opts)
            regexp.Replace(input, replacement, count)
                

    /// Returns input string if nothing is replaced.
    let replace1Re (pattern:string) 
                   (replacement:string) : Rewrite = 
        replaceCountRe pattern 1 replacement <&?> "replace1Re"


    let prefix (front : string) : Rewrite = 
        modify (fun s -> front + s)

    
        

    let suffix (tail : string) : Rewrite = append tail


    let replacePrefix (front : string) : Rewrite = 
        let len = front.Length
        dropLeft len .>>  prefix front 



    let replaceSuffix (tail : string) : Rewrite = 
        let len = tail.Length
        dropLeft len .>>  append tail 
