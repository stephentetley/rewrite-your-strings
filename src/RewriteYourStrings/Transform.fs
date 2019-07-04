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






    /// Exceptions should be caught.
    let primitiveRewrite (operation : RegexOptions -> string -> string option) : Rewrite =
        rewrite { 
            let! source = getInput ()
            let! regexOpts = askOptions ()
            match guardOperation operation regexOpts source with
            | None -> return! rewriteError "primitiveRewrite"
            | Some ans -> return! setInput ans
        }

    let stringRewriteOption (operation : string -> string option) : Rewrite =
        primitiveRewrite (fun _ input -> operation input)

    let stringRewrite (operation:string -> string) : Rewrite =
        primitiveRewrite (fun _ input -> operation input |> Some)


    
    
    let identity : Rewrite = 
        mreturn ()

    let constR (literal : string) = 
        stringRewrite (fun _ -> literal)

    // ************************************************************************
    // Combinators 

    
    let execRewrite (action : Rewrite)
                    (input : string) : StringRewriter<string> = 
        rewrite { 
            let! start = getInput ()
            do! setInput input
            let! _ = action
            let! ans = getInput ()
            do! setInput start
            return ans
        }
    
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


    let stringMap (errMsg : string) (charOp : char -> char) : Rewrite =
        stringRewrite <| fun s -> String.map charOp s 

    let replace (oldValue : string) (newValue : string) : Rewrite = 
        stringRewrite (fun s -> s.Replace(oldValue = oldValue, newValue = newValue))


    let charReplace (oldChar : char) (newChar : char) : Rewrite = 
        stringRewrite (fun s -> s.Replace(oldChar = oldChar, newChar = newChar))


    let append (tail : string) : Rewrite = 
        stringRewrite (fun s -> s + tail)

    let trim : Rewrite = 
        stringRewrite <| fun s -> s.Trim()

    let trimStart : Rewrite = 
        stringRewrite <| fun s -> s.TrimStart()

    let trimEnd : Rewrite = 
        stringRewrite <| fun s -> s.TrimEnd()

    let padLeft (totalWidth:int) (paddingChar:char) : Rewrite = 
        stringRewrite <| fun s -> s.PadLeft(totalWidth, paddingChar)

    let padRight (totalWidth:int) (paddingChar:char) : Rewrite = 
        stringRewrite <| fun s -> s.PadRight(totalWidth, paddingChar)

    let toUpper : Rewrite = 
        stringMap "toUpper" System.Char.ToUpper

    let toLower : Rewrite = 
        stringMap "toUpper" System.Char.ToLower

    let takeLeft (len : int) : Rewrite = 
        stringRewrite <| fun s -> s.Substring(0, len)

    let dropLeft (len : int) : Rewrite = 
        stringRewrite <| fun s -> s.Substring(len)

    let takeRight (len : int) : Rewrite = 
        rewrite { 
            let! size = length
            return! stringRewrite (fun s -> s.Substring(size - len))
        }
        

    let dropRight (len : int) : Rewrite = 
        rewrite { 
            let! size = length
            return! stringRewrite (fun s -> s.Substring(0, size - len))
        }

    /// Returns input string if nothing is replaced.
    let replaceAllRe (pattern:string) 
                     (replacement:string) : Rewrite = 
        primitiveRewrite <| fun opts input -> 
            let regexp = new Regex(pattern = pattern, options = opts) 
            regexp.Replace(input = input, replacement = replacement) |> Some


    /// Returns input string if nothing is replaced.
    let replaceCountRe (pattern:string) 
                       (count:int)
                       (replacement:string) : Rewrite = 
        primitiveRewrite <| fun opts input -> 
            let regexp = new Regex(pattern = pattern, options = opts)
            regexp.Replace(input, replacement, count) |> Some
                

    /// Returns input string if nothing is replaced.
    let replace1Re (pattern:string) 
                   (replacement:string) : Rewrite = 
        replaceCountRe pattern 1 replacement <&?> "replace1Re"
        

    let namedRegexMatch (pattern : string) (name : string) : Rewrite = 
        primitiveRewrite <| fun opts input -> 
            let rmatch = Regex.Match(input = input, pattern = pattern)
            if rmatch.Success then 
                rmatch.Groups.Item(name).Value |> Some
            else
                None
            
            
    let prefix (front : string) : Rewrite = 
        stringRewrite (fun s -> front + s)

    
        

    let suffix (tail : string) : Rewrite = append tail


    let replacePrefix (front : string) : Rewrite = 
        let len = front.Length
        dropLeft len .>>  prefix front 



    let replaceSuffix (tail : string) : Rewrite = 
        let len = tail.Length
        dropLeft len .>>  append tail 
