// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace RewriteYourStrings

module RewriteMonad =

    open System.Text.RegularExpressions

    open RewriteYorStrings.Internal

    type ErrMsg = string

    type private Answer<'a> = Result<string * 'a, ErrMsg> 

    type RewriteMonad<'a> = 
        RewriteMonad of (RegexOptions -> string -> Answer<'a>)

    let inline private apply1 (ma: RewriteMonad<'a>) 
                              (options:RegexOptions)
                              (input: string) : Answer<'a>= 
        let (RewriteMonad f) = ma in f options input


    let inline mreturn (x:'a) : RewriteMonad<'a> = 
        RewriteMonad <| fun opts input -> Ok (input, x)


    let inline private bindM (ma: RewriteMonad<'a>) 
                        (f :'a -> RewriteMonad<'b>) : RewriteMonad<'b> =
        RewriteMonad <| fun opts input -> 
            match apply1 ma opts input with
            | Error msg -> Error msg
            | Ok (input2, a) -> apply1 (f a) opts input2

    let inline private zeroM () : RewriteMonad<'a> = 
        RewriteMonad <| fun _ _ -> Error "zeroM"

    /// "First success" 
    /// If mfirst fails tray to apply msecond
    let inline private combineM (mfirst:RewriteMonad<'a>) 
                                (msecond:RewriteMonad<'a>) : RewriteMonad<'a> = 
        RewriteMonad <| fun opts input -> 
            match apply1 mfirst opts input with
            | Error msg -> apply1 msecond opts input
            | Ok a -> Ok a


    let inline private delayM (fn:unit -> RewriteMonad<'a>) : RewriteMonad<'a> = 
        bindM (mreturn ()) fn 

    type RewriteMonadBuilder() = 
        member self.Return x            = mreturn x
        member self.Bind (p,f)          = bindM p f
        member self.Zero ()             = zeroM ()
        member self.Combine (ma,mb)     = combineM ma mb
        member self.Delay fn            = delayM fn
        member self.ReturnFrom(ma)      = ma


    let (rewriteMonad:RewriteMonadBuilder) = new RewriteMonadBuilder()

    // ****************************************************
    // Run

    /// This runs the finalizer on userResources
    let runRewriteMonad (input:string) 
                        (ma:RewriteMonad<'a>) : Result<string *'a, ErrMsg> = 
        apply1 ma RegexOptions.None input

    let rewrite (ma:RewriteMonad<'a>) (input:string) 
                 : Result<string, ErrMsg> = 
        match apply1 ma RegexOptions.None input with
        | Error msg -> Error msg
        | Ok(output, _) -> Ok output

    let unsafeRewrite (ma:RewriteMonad<'a>) (input:string) : string = 
        match rewrite ma input with
        | Error msg -> failwith msg
        | Ok ans -> ans


    // ****************************************************
    // Errors

    let rewriteError (msg: string) : RewriteMonad<'a> = 
        RewriteMonad <| fun _ _ -> Error msg


    let swapError (msg: string) 
                  (ma: RewriteMonad<'a>) : RewriteMonad<'a> = 
        RewriteMonad <| fun opts input ->
            match apply1 ma opts input with
            | Error _ -> Error msg
            | Ok a -> Ok a


    /// Execute an action that may throw a system exception.
    /// Capture the exception with try ... with
    /// and return the answer or the expection message in the monad.
    let attemptM (ma: RewriteMonad<'a>) : RewriteMonad<'a> = 
        RewriteMonad <| fun opts input -> 
            try
                apply1 ma opts input
            with
            | ex -> Error (sprintf "attemptM: %s" ex.Message)

    // ****************************************************
    // Regex Options

    let optionsLocal (modify : RegexOptions -> RegexOptions)
                     (ma: RewriteMonad<'a>) : RewriteMonad<'a> = 
        RewriteMonad <| fun opts input -> 
            apply1 ma (modify opts) input

    let ignoreCase (ma: RewriteMonad<'a>) : RewriteMonad<'a> = 
        optionsLocal (fun opts -> RegexOptions.IgnoreCase + opts) ma


    // ****************************************************
    // Monadic operations


    let assertM (cond:RewriteMonad<bool>) 
                (failMsg:string) : RewriteMonad<unit> = 
        rewriteMonad { 
            match! cond with
            | true -> return ()
            | false -> rewriteError failMsg |> ignore
        }

    let whenM (cond:RewriteMonad<bool>) 
              (failMsg:string) 
              (successOp:unit -> RewriteMonad<'a>) = 
        rewriteMonad { 
            let! ans = cond
            if ans then 
                let! res = successOp ()
                return res
            else rewriteError failMsg |> ignore
        } 

    /// fmap 
    let fmapM (fn:'a -> 'b) (ma:RewriteMonad<'a>) : RewriteMonad<'b> = 
        RewriteMonad <| fun opts input -> 
           match apply1 ma opts input with
           | Error msg -> Error msg
           | Ok (input1, a) -> Ok (input1, fn a)


    // liftM (which is fmap)
    let liftM (fn:'a -> 'x) (ma:RewriteMonad<'a>) : RewriteMonad<'x> = 
        fmapM fn ma

    let liftM2 (fn:'a -> 'b -> 'x) 
               (ma:RewriteMonad<'a>) 
               (mb:RewriteMonad<'b>) : RewriteMonad<'x> = 
        rewriteMonad { 
            let! a = ma
            let! b = mb
            return (fn a b)
        }

    let liftM3 (fn:'a -> 'b -> 'c -> 'x) 
               (ma:RewriteMonad<'a>) 
               (mb:RewriteMonad<'b>) 
               (mc:RewriteMonad<'c>) : RewriteMonad<'x> = 
        rewriteMonad { 
            let! a = ma
            let! b = mb
            let! c = mc
            return (fn a b c)
        }

    let liftM4 (fn:'a -> 'b -> 'c -> 'd -> 'x) 
               (ma:RewriteMonad<'a>) 
               (mb:RewriteMonad<'b>) 
               (mc:RewriteMonad<'c>) 
               (md:RewriteMonad<'d>) : RewriteMonad<'x> = 
        rewriteMonad { 
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            return (fn a b c d)
        }


    let liftM5 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'x) 
               (ma:RewriteMonad<'a>) 
               (mb:RewriteMonad<'b>) 
               (mc:RewriteMonad<'c>) 
               (md:RewriteMonad<'d>) 
               (me:RewriteMonad<'e>) : RewriteMonad<'x> = 
        rewriteMonad { 
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            let! e = me
            return (fn a b c d e)
        }

    let liftM6 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x) 
               (ma:RewriteMonad<'a>) 
               (mb:RewriteMonad<'b>) 
               (mc:RewriteMonad<'c>) 
               (md:RewriteMonad<'d>) 
               (me:RewriteMonad<'e>) 
               (mf:RewriteMonad<'f>) : RewriteMonad<'x> = 
        rewriteMonad { 
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            let! e = me
            let! f = mf
            return (fn a b c d e f)
        }


    let tupleM2 (ma:RewriteMonad<'a>) 
                (mb:RewriteMonad<'b>) : RewriteMonad<'a * 'b> = 
        liftM2 (fun a b -> (a,b)) ma mb

    let tupleM3 (ma:RewriteMonad<'a>) 
                (mb:RewriteMonad<'b>) 
                (mc:RewriteMonad<'c>) : RewriteMonad<'a * 'b * 'c> = 
        liftM3 (fun a b c -> (a,b,c)) ma mb mc

    let tupleM4 (ma:RewriteMonad<'a>) 
                (mb:RewriteMonad<'b>) 
                (mc:RewriteMonad<'c>) 
                (md:RewriteMonad<'d>) : RewriteMonad<'a * 'b * 'c * 'd> = 
        liftM4 (fun a b c d -> (a,b,c,d)) ma mb mc md

    let tupleM5 (ma:RewriteMonad<'a>) 
                (mb:RewriteMonad<'b>) 
                (mc:RewriteMonad<'c>) 
                (md:RewriteMonad<'d>) 
                (me:RewriteMonad<'e>) : RewriteMonad<'a * 'b * 'c * 'd * 'e> = 
        liftM5 (fun a b c d e -> (a,b,c,d,e)) ma mb mc md me

    let tupleM6 (ma:RewriteMonad<'a>) 
                (mb:RewriteMonad<'b>) 
                (mc:RewriteMonad<'c>) 
                (md:RewriteMonad<'d>) 
                (me:RewriteMonad<'e>) 
                (mf:RewriteMonad<'f>) : RewriteMonad<'a * 'b * 'c * 'd * 'e * 'f> = 
        liftM6 (fun a b c d e f -> (a,b,c,d,e,f)) ma mb mc md me mf

    let pipeM2 (ma:RewriteMonad<'a>) 
               (mb:RewriteMonad<'b>) 
               (fn:'a -> 'b -> 'x) : RewriteMonad<'x> = 
        liftM2 fn ma mb

    let pipeM3 (ma:RewriteMonad<'a>) 
               (mb:RewriteMonad<'b>) 
               (mc:RewriteMonad<'c>) 
               (fn:'a -> 'b -> 'c -> 'x) : RewriteMonad<'x> = 
        liftM3 fn ma mb mc

    let pipeM4 (ma:RewriteMonad<'a>) 
               (mb:RewriteMonad<'b>) 
               (mc:RewriteMonad<'c>) 
               (md:RewriteMonad<'d>) 
               (fn:'a -> 'b -> 'c -> 'd -> 'x) : RewriteMonad<'x> = 
        liftM4 fn ma mb mc md

    let pipeM5 (ma:RewriteMonad<'a>) 
               (mb:RewriteMonad<'b>) 
               (mc:RewriteMonad<'c>) 
               (md:RewriteMonad<'d>) 
               (me:RewriteMonad<'e>) 
               (fn:'a -> 'b -> 'c -> 'd -> 'e ->'x) : RewriteMonad<'x> = 
        liftM5 fn ma mb mc md me

    let pipeM6 (ma:RewriteMonad<'a>) 
               (mb:RewriteMonad<'b>) 
               (mc:RewriteMonad<'c>) 
               (md:RewriteMonad<'d>) 
               (me:RewriteMonad<'e>) 
               (mf:RewriteMonad<'f>) 
               (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x) : RewriteMonad<'x> = 
        liftM6 fn ma mb mc md me mf

    /// Left biased choice, if ``ma`` succeeds return its result, otherwise try ``mb``.
    let altM (ma:RewriteMonad<'a>) (mb:RewriteMonad<'a>) : RewriteMonad<'a> = 
        combineM ma mb


    /// Haskell Applicative's (<*>)
    let apM (mf:RewriteMonad<'a ->'b>) (ma:RewriteMonad<'a>) : RewriteMonad<'b> = 
        rewriteMonad { 
            let! fn = mf
            let! a = ma
            return (fn a) 
        }



    /// Perform two actions in sequence. 
    /// Ignore the results of the second action if both succeed.
    let seqL (ma:RewriteMonad<'a>) (mb:RewriteMonad<'b>) : RewriteMonad<'a> = 
        rewriteMonad { 
            let! a = ma
            let! b = mb
            return a
        }

    /// Perform two actions in sequence. 
    /// Ignore the results of the first action if both succeed.
    let seqR (ma:RewriteMonad<'a>) (mb:RewriteMonad<'b>) : RewriteMonad<'b> = 
        rewriteMonad { 
            let! a = ma
            let! b = mb
            return b
        }


    /// Optionally run a computation. 
    /// If the build fails return None otherwise retun Some<'a>.
    let optionalM (ma:RewriteMonad<'a>) : RewriteMonad<'a option> = 
        RewriteMonad <| fun opts input ->
            match apply1 ma opts input with
            | Error _ -> Ok (input, None)
            | Ok (input1, a) -> Ok (input1, Some a)


    let optionFailM (errMsg:string)
                    (ma:RewriteMonad<'a option>) : RewriteMonad<'a> = 
        bindM ma (fun opt -> 
                    match opt with
                    | Some ans -> mreturn ans
                    | None -> rewriteError errMsg)


    let kleisliL (mf:'a -> RewriteMonad<'b>)
                 (mg:'b -> RewriteMonad<'c>)
                 (source:'a) : RewriteMonad<'c> = 
        rewriteMonad { 
            let! b = mf source
            let! c = mg b
            return c
        }

    /// Flipped kleisliL
    let kleisliR (mf:'b -> RewriteMonad<'c>)
                 (mg:'a -> RewriteMonad<'b>)
                 (source:'a) : RewriteMonad<'c> = 
        rewriteMonad { 
            let! b = mg source
            let! c = mf b
            return c
        }

    // ****************************************************
    // Recursive functions

    /// Implemented in CPS
    let sequenceM (actions: RewriteMonad<'a> list) : RewriteMonad<'a list> = 
        RewriteMonad <| fun opts input -> 
            let rec work (acts:RewriteMonad<'a> list) (input1:string) 
                         (fk: ErrMsg -> Answer<'a list>) 
                         (sk: string -> 'a list -> Answer<'a list>) = 
                match acts with
                | [] -> sk input1 []
                | ma :: rest -> 
                    match apply1 ma opts input1 with
                    | Error msg -> fk msg
                    | Ok (input2, ans1) -> 
                        work rest input2 fk (fun input3 anslist ->
                        sk input3 (ans1::anslist))
            work actions input (fun msg -> Error msg) (fun st ans -> Ok (st, ans))

    /// Implemented in CPS
    let sequenceMz (actions: RewriteMonad<'a> list) : RewriteMonad<unit> = 
        RewriteMonad <| fun opts input -> 
            let rec work (acts:RewriteMonad<'a> list) (input1:string) 
                         (fk: ErrMsg -> Answer<unit>) 
                         (sk: string -> Answer<unit>) = 
                match acts with
                | [] -> sk input1
                | ma :: rest -> 
                    match apply1 ma opts input1 with
                    | Error msg -> fk msg
                    | Ok (input2, _) -> 
                        work rest input2 fk (fun input3 ->
                        sk input3)
            work actions input (fun msg -> Error msg) (fun st -> Ok (st, ()))

    /// Implemented in CPS 
    let mapM (mf: 'a -> RewriteMonad<'b>) 
             (source:'a list) : RewriteMonad<'b list> = 
        RewriteMonad <| fun opts input -> 
            let rec work (xs:'a list) (input1:string) 
                         (fk: ErrMsg -> Answer<'b list>) 
                         (sk: string -> 'b list -> Answer<'b list>) = 
                match xs with
                | [] -> sk input1 []
                | y :: ys -> 
                    match apply1 (mf y) opts input1 with
                    | Error msg -> fk msg
                    | Ok (input2, ans1) -> 
                        work ys input2 fk (fun input3 anslist ->
                        sk input3 (ans1::anslist))
            work source input (fun msg -> Error msg) (fun st ans -> Ok (st, ans))

    /// Flipped mapM
    let forM (source:'a list) 
             (mf: 'a -> RewriteMonad<'b>) : RewriteMonad<'b list> = 
        mapM mf source

    /// Forgetful mapM
    let mapMz (mf: 'a -> RewriteMonad<'b>) 
              (source:'a list) : RewriteMonad<unit> = 
        RewriteMonad <| fun opts input -> 
            let rec work (xs:'a list) (input1:string) 
                         (fk: ErrMsg -> Answer<unit>) 
                         (sk: string -> Answer<unit>) = 
                match xs with
                | [] -> sk input1
                | y :: ys -> 
                    match apply1 (mf y) opts input1 with
                    | Error msg -> fk msg
                    | Ok (input2, _) -> 
                        work ys input2 fk (fun input3 ->
                        sk input3)
            work source input (fun msg -> Error msg) (fun st -> Ok (st, ()))

    /// Flipped mapMz
    let forMz (source:'a list) 
              (mf: 'a -> RewriteMonad<'b>) : RewriteMonad<unit> = 
        mapMz mf source


    /// Implemented in CPS 
    let mapiM (mf:int -> 'a -> RewriteMonad<'b>) 
              (source:'a list) : RewriteMonad<'b list> = 
        RewriteMonad <| fun opts input -> 
            let rec work (xs: 'a list) (n: int) (input1: string) 
                         (fk: ErrMsg -> Answer<'b list>) 
                         (sk: string -> 'b list -> Answer<'b list>) = 
                match xs with
                | [] -> sk input1 []
                | y :: ys -> 
                    match apply1 (mf n y) opts input1 with
                    | Error msg -> fk msg
                    | Ok (input2, ans1) -> 
                        work ys (n+1) input2 fk (fun input3 anslist ->
                        sk input3 (ans1::anslist))
            work source 0 input (fun msg -> Error msg) (fun st ans -> Ok (st, ans))

    /// Flipped mapMi
    let foriM (source:'a list) 
              (mf: int -> 'a -> RewriteMonad<'b>)  : RewriteMonad<'b list> = 
        mapiM mf source

    /// Forgetful mapiM
    let mapiMz (mf: int -> 'a -> RewriteMonad<'b>) 
              (source:'a list) : RewriteMonad<unit> = 
        RewriteMonad <| fun opts input -> 
            let rec work (xs:'a list) (n:int) (input1:string) 
                         (fk: ErrMsg -> Answer<unit>) 
                         (sk: string -> Answer<unit>) = 
                match xs with
                | [] -> sk input1
                | y :: ys -> 
                    match apply1 (mf n y) opts input1 with
                    | Error msg -> fk msg
                    | Ok (input2, _) -> 
                        work ys (n+1) input2 fk (fun input3 ->
                        sk input3)
            work source 0 input (fun msg -> Error msg) (fun st -> Ok (st, ()))

    /// Flipped mapiMz
    let foriMz (source:'a list) 
               (mf: int -> 'a -> RewriteMonad<'b>) : RewriteMonad<unit> = 
        mapiMz mf source

    // ****************************************************
    // Operators

    // ****************************************************
    // Errors

    /// Operator for swapError
    let ( <?&> ) (msg:string) (ma:RewriteMonad<'a>) : RewriteMonad<'a> = 
        swapError msg ma

    /// Operator for flip swapError
    let ( <&?> ) (ma:RewriteMonad<'a>) (msg:string) : RewriteMonad<'a> = 
        swapError msg ma


    // ****************************************************
    // Monadic operations

    /// Bind operator
    let ( >>= ) (ma:RewriteMonad<'a>) 
              (fn:'a -> RewriteMonad<'b>) : RewriteMonad<'b> = 
        bindM ma fn

    /// Flipped Bind operator
    let ( =<< ) (fn:'a -> RewriteMonad<'b>) 
              (ma:RewriteMonad<'a>) : RewriteMonad<'b> = 
        bindM ma fn


    /// Operator for fmap.
    let ( |>> ) (ma:RewriteMonad<'a>) (fn:'a -> 'b) : RewriteMonad<'b> = 
        fmapM fn ma

    /// Flipped fmap.
    let ( <<| ) (fn:'a -> 'b) (ma:RewriteMonad<'a>) : RewriteMonad<'b> = 
        fmapM fn ma

    /// Operator for altM
    let ( <||> ) (ma:RewriteMonad<'a>) 
               (mb:RewriteMonad<'a>) : RewriteMonad<'a> = 
        altM ma mb 


    /// Operator for apM
    let ( <**> ) (ma:RewriteMonad<'a -> 'b>) 
               (mb:RewriteMonad<'a>) : RewriteMonad<'b> = 
        apM ma mb

    /// Operator for fmapM
    let ( <&&> ) (fn:'a -> 'b) (ma:RewriteMonad<'a>) : RewriteMonad<'b> = 
        fmapM fn ma



    /// Operator for seqL
    let (.>>) (ma:RewriteMonad<'a>) 
              (mb:RewriteMonad<'b>) : RewriteMonad<'a> = 
        seqL ma mb

    /// Operator for seqR
    let (>>.) (ma:RewriteMonad<'a>) 
              (mb:RewriteMonad<'b>) : RewriteMonad<'b> = 
        seqR ma mb



    /// Operator for kleisliL
    let (>=>) (mf : 'a -> RewriteMonad<'b>)
              (mg : 'b -> RewriteMonad<'c>)
              (source:'a) : RewriteMonad<'c> = 
        kleisliL mf mg source


    /// Operator for kleisliR
    let (<=<) (mf : 'b -> RewriteMonad<'c>)
              (mg : 'a -> RewriteMonad<'b>)
              (source:'a) : RewriteMonad<'c> = 
        kleisliR mf mg source


    // ****************************************************
    // Rewriting

    type Rewrite = RewriteMonad<unit>

    let withInput (errMsg:string) (operation:RegexOptions -> string -> string) : Rewrite =
        RewriteMonad <| fun opts input ->
            try 
                let output = operation opts input in Ok(output, ())
            with
            | _ -> Error errMsg

    let stringMap (errMsg:string) (charOp:char -> char) : Rewrite =
        withInput errMsg <| fun _ s -> String.map charOp s 


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

    // ************************************************************************
    // Combinators 

    /// Run the rewrite, succeed if the input has changed. Fail on no change.
    let progressive (ma:RewriteMonad<'a>) : RewriteMonad<'a> = 
        RewriteMonad <| fun opts input -> 
            match apply1 ma opts input with
            | Error msg -> Error msg
            | Ok(output,a) -> 
                if output <> input then 
                    Ok(output,a)
                else Error "progressive"

    let either (ma:RewriteMonad<'a>) (mb:RewriteMonad<'a>) : RewriteMonad<'a> = 
        progressive ma <||> progressive mb

    let choice (rewriters:RewriteMonad<'a> list) : RewriteMonad<'a> = 
        let rec work (fs:RewriteMonad<'a> list) = 
            match fs with
            | [] -> rewriteError "choice"
            | (mf :: rest) -> altM (progressive mf) (work rest)
        work rewriters


    // ************************************************************************
    // Queries
    
    type Query<'a> = RewriteMonad<'a>

    let queryInput (errMsg:string) 
                   (query:RegexOptions -> string -> 'a) : RewriteMonad<'a> =
        RewriteMonad <| fun opts input ->
            try 
                Ok(input, query opts input)
            with
            | _ -> Error errMsg

    let equals (target:string) : Query<bool> = 
        queryInput "equals" (fun _ input -> input = target)


    let isMatch (pattern:string) : Query<bool> = 
        queryInput "equals" 
            <| fun opts input -> Regex.IsMatch(input = input
                                              , pattern = pattern
                                              , options = opts)

    let levenshteinDistance (target:string) : Query<int> = 
        queryInput "levenshteinDistance"
            <| fun _ input -> Levenshtein.distance input target


