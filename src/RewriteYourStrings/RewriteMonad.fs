// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace RewriteYourStrings

module RewriteMonad =

    open System.Text.RegularExpressions

    open RewriteYourStrings.Internal

    type ErrMsg = string

    type private Answer<'a> = Result<'a * string, ErrMsg> 

    type StringRewriter<'a> = 
        StringRewriter of (RegexOptions -> string -> Answer<'a>)

    let inline private apply1 (ma: StringRewriter<'a>) 
                              (options : RegexOptions)
                              (input : string) : Answer<'a>= 
        let (StringRewriter f) = ma in f options input


    let inline mreturn (x:'a) : StringRewriter<'a> = 
        StringRewriter <| fun opts st -> Ok (x, st)


    let inline private bindM (ma: StringRewriter<'a>) 
                        (f :'a -> StringRewriter<'b>) : StringRewriter<'b> =
        StringRewriter <| fun opts st -> 
            match apply1 ma opts st with
            | Error msg -> Error msg
            | Ok (a, st1) -> apply1 (f a) opts st1

    let inline private zeroM () : StringRewriter<'a> = 
        StringRewriter <| fun _ _ -> Error "zeroM"

    /// "First success" 
    /// If mfirst fails tray to apply msecond
    let inline private combineM (mfirst:StringRewriter<'a>) 
                                (msecond:StringRewriter<'a>) : StringRewriter<'a> = 
        StringRewriter <| fun opts input -> 
            match apply1 mfirst opts input with
            | Error msg -> apply1 msecond opts input
            | Ok a -> Ok a


    let inline private delayM (fn:unit -> StringRewriter<'a>) : StringRewriter<'a> = 
        bindM (mreturn ()) fn 

    type StringRewriterBuilder() = 
        member self.Return x            = mreturn x
        member self.Bind (p,f)          = bindM p f
        member self.Zero ()             = zeroM ()
        member self.Combine (ma,mb)     = combineM ma mb
        member self.Delay fn            = delayM fn
        member self.ReturnFrom(ma)      = ma


    let (rewrite:StringRewriterBuilder) = new StringRewriterBuilder()

    // ****************************************************
    // Run

    /// Run a rewrite, return either resulting string or fail message.
    let runStringRewriter (ma : StringRewriter<'a>) 
                          (input : string) : Result<'a * string, ErrMsg> = 
        match apply1 ma RegexOptions.None input with
        | Error msg -> Error msg
        | Ok ans-> Ok ans

    let runRewrite (ma : StringRewriter<'a>) 
                   (input : string) : Result<string, ErrMsg> = 
        match apply1 ma RegexOptions.None input with
        | Error msg -> Error msg
        | Ok (_, str) -> Ok str


    let unsafeRewrite (ma:StringRewriter<'a>) (input:string) : string = 
        match runRewrite ma input with
        | Error msg -> failwith msg
        | Ok ans -> ans


    let rewriteOrId (ma:StringRewriter<'a>) (input:string) : string = 
        match runRewrite ma input with
        | Error _ -> input
        | Ok ans -> ans

    // ****************************************************
    // Input

    let getInput () : StringRewriter<string> =
        StringRewriter <| fun _ input -> Ok (input, input)

        
    let setInput (source : string) : StringRewriter<unit> =
            StringRewriter <| fun _ _ -> Ok ((), source)

    // ****************************************************
    // Errors

    let rewriteError (msg: string) : StringRewriter<'a> = 
        StringRewriter <| fun _ _ -> Error msg


    let swapError (msg: string) 
                  (ma: StringRewriter<'a>) : StringRewriter<'a> = 
        StringRewriter <| fun opts input ->
            match apply1 ma opts input with
            | Error _ -> Error msg
            | Ok a -> Ok a


    /// Execute an action that may throw a system exception.
    /// Capture the exception with try ... with
    /// and return the answer or the expection message in the monad.
    let attemptM (ma: StringRewriter<'a>) : StringRewriter<'a> = 
        StringRewriter <| fun opts input -> 
            try
                apply1 ma opts input
            with
            | ex -> Error (sprintf "attemptM: %s" ex.Message)

    // ****************************************************
    // Regex Options

    let optionsLocal (modify : RegexOptions -> RegexOptions)
                     (ma: StringRewriter<'a>) : StringRewriter<'a> = 
        StringRewriter <| fun opts input -> 
            apply1 ma (modify opts) input

    let ignoreCase (ma: StringRewriter<'a>) : StringRewriter<'a> = 
        optionsLocal (fun opts -> RegexOptions.IgnoreCase ||| opts) ma


    // ****************************************************
    // Monadic operations


    let assertM (cond:StringRewriter<bool>) 
                (failMsg:string) : StringRewriter<unit> = 
        rewrite { 
            match! cond with
            | true -> return ()
            | false -> rewriteError failMsg |> ignore
        }

    let whenM (cond:StringRewriter<bool>) 
              (failMsg:string) 
              (successOp:unit -> StringRewriter<'a>) = 
        rewrite { 
            let! ans = cond
            if ans then 
                let! res = successOp ()
                return res
            else rewriteError failMsg |> ignore
        } 

    /// fmap 
    let fmapM (fn:'a -> 'b) (ma:StringRewriter<'a>) : StringRewriter<'b> = 
        StringRewriter <| fun opts st -> 
           match apply1 ma opts st with
           | Error msg -> Error msg
           | Ok (a, st1) -> Ok (fn a, st1)


    // liftM (which is fmap)
    let liftM (fn:'a -> 'x) (ma:StringRewriter<'a>) : StringRewriter<'x> = 
        fmapM fn ma

    let liftM2 (fn:'a -> 'b -> 'x) 
               (ma:StringRewriter<'a>) 
               (mb:StringRewriter<'b>) : StringRewriter<'x> = 
        rewrite { 
            let! a = ma
            let! b = mb
            return (fn a b)
        }

    let liftM3 (fn:'a -> 'b -> 'c -> 'x) 
               (ma:StringRewriter<'a>) 
               (mb:StringRewriter<'b>) 
               (mc:StringRewriter<'c>) : StringRewriter<'x> = 
        rewrite { 
            let! a = ma
            let! b = mb
            let! c = mc
            return (fn a b c)
        }

    let liftM4 (fn:'a -> 'b -> 'c -> 'd -> 'x) 
               (ma:StringRewriter<'a>) 
               (mb:StringRewriter<'b>) 
               (mc:StringRewriter<'c>) 
               (md:StringRewriter<'d>) : StringRewriter<'x> = 
        rewrite { 
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            return (fn a b c d)
        }


    let liftM5 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'x) 
               (ma:StringRewriter<'a>) 
               (mb:StringRewriter<'b>) 
               (mc:StringRewriter<'c>) 
               (md:StringRewriter<'d>) 
               (me:StringRewriter<'e>) : StringRewriter<'x> = 
        rewrite { 
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            let! e = me
            return (fn a b c d e)
        }

    let liftM6 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x) 
               (ma:StringRewriter<'a>) 
               (mb:StringRewriter<'b>) 
               (mc:StringRewriter<'c>) 
               (md:StringRewriter<'d>) 
               (me:StringRewriter<'e>) 
               (mf:StringRewriter<'f>) : StringRewriter<'x> = 
        rewrite { 
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            let! e = me
            let! f = mf
            return (fn a b c d e f)
        }


    let tupleM2 (ma:StringRewriter<'a>) 
                (mb:StringRewriter<'b>) : StringRewriter<'a * 'b> = 
        liftM2 (fun a b -> (a,b)) ma mb

    let tupleM3 (ma:StringRewriter<'a>) 
                (mb:StringRewriter<'b>) 
                (mc:StringRewriter<'c>) : StringRewriter<'a * 'b * 'c> = 
        liftM3 (fun a b c -> (a,b,c)) ma mb mc

    let tupleM4 (ma:StringRewriter<'a>) 
                (mb:StringRewriter<'b>) 
                (mc:StringRewriter<'c>) 
                (md:StringRewriter<'d>) : StringRewriter<'a * 'b * 'c * 'd> = 
        liftM4 (fun a b c d -> (a,b,c,d)) ma mb mc md

    let tupleM5 (ma:StringRewriter<'a>) 
                (mb:StringRewriter<'b>) 
                (mc:StringRewriter<'c>) 
                (md:StringRewriter<'d>) 
                (me:StringRewriter<'e>) : StringRewriter<'a * 'b * 'c * 'd * 'e> = 
        liftM5 (fun a b c d e -> (a,b,c,d,e)) ma mb mc md me

    let tupleM6 (ma:StringRewriter<'a>) 
                (mb:StringRewriter<'b>) 
                (mc:StringRewriter<'c>) 
                (md:StringRewriter<'d>) 
                (me:StringRewriter<'e>) 
                (mf:StringRewriter<'f>) : StringRewriter<'a * 'b * 'c * 'd * 'e * 'f> = 
        liftM6 (fun a b c d e f -> (a,b,c,d,e,f)) ma mb mc md me mf

    let pipeM2 (ma:StringRewriter<'a>) 
               (mb:StringRewriter<'b>) 
               (fn:'a -> 'b -> 'x) : StringRewriter<'x> = 
        liftM2 fn ma mb

    let pipeM3 (ma:StringRewriter<'a>) 
               (mb:StringRewriter<'b>) 
               (mc:StringRewriter<'c>) 
               (fn:'a -> 'b -> 'c -> 'x) : StringRewriter<'x> = 
        liftM3 fn ma mb mc

    let pipeM4 (ma:StringRewriter<'a>) 
               (mb:StringRewriter<'b>) 
               (mc:StringRewriter<'c>) 
               (md:StringRewriter<'d>) 
               (fn:'a -> 'b -> 'c -> 'd -> 'x) : StringRewriter<'x> = 
        liftM4 fn ma mb mc md

    let pipeM5 (ma:StringRewriter<'a>) 
               (mb:StringRewriter<'b>) 
               (mc:StringRewriter<'c>) 
               (md:StringRewriter<'d>) 
               (me:StringRewriter<'e>) 
               (fn:'a -> 'b -> 'c -> 'd -> 'e ->'x) : StringRewriter<'x> = 
        liftM5 fn ma mb mc md me

    let pipeM6 (ma:StringRewriter<'a>) 
               (mb:StringRewriter<'b>) 
               (mc:StringRewriter<'c>) 
               (md:StringRewriter<'d>) 
               (me:StringRewriter<'e>) 
               (mf:StringRewriter<'f>) 
               (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x) : StringRewriter<'x> = 
        liftM6 fn ma mb mc md me mf

    /// Left biased choice, if ``ma`` succeeds return its result, otherwise try ``mb``.
    let altM (ma:StringRewriter<'a>) (mb:StringRewriter<'a>) : StringRewriter<'a> = 
        combineM ma mb


    /// Haskell Applicative's (<*>)
    let apM (mf:StringRewriter<'a ->'b>) (ma:StringRewriter<'a>) : StringRewriter<'b> = 
        rewrite { 
            let! fn = mf
            let! a = ma
            return (fn a) 
        }



    /// Perform two actions in sequence. 
    /// Ignore the results of the second action if both succeed.
    let seqL (ma:StringRewriter<'a>) (mb:StringRewriter<'b>) : StringRewriter<'a> = 
        rewrite { 
            let! a = ma
            let! b = mb
            return a
        }

    /// Perform two actions in sequence. 
    /// Ignore the results of the first action if both succeed.
    let seqR (ma:StringRewriter<'a>) (mb:StringRewriter<'b>) : StringRewriter<'b> = 
        rewrite { 
            let! a = ma
            let! b = mb
            return b
        }


    /// Optionally run a computation. 
    /// If the build fails return None otherwise retun Some<'a>.
    let optionalM (ma:StringRewriter<'a>) : StringRewriter<'a option> = 
        StringRewriter <| fun opts st ->
            match apply1 ma opts st with
            | Error _ -> Ok (None, st)
            | Ok (a, st1) -> Ok (Some a, st1)


    let optionFailM (errMsg:string)
                    (ma:StringRewriter<'a option>) : StringRewriter<'a> = 
        bindM ma (fun opt -> 
                    match opt with
                    | Some ans -> mreturn ans
                    | None -> rewriteError errMsg)


    let kleisliL (mf:'a -> StringRewriter<'b>)
                 (mg:'b -> StringRewriter<'c>)
                 (source:'a) : StringRewriter<'c> = 
        rewrite { 
            let! b = mf source
            let! c = mg b
            return c
        }

    /// Flipped kleisliL
    let kleisliR (mf:'b -> StringRewriter<'c>)
                 (mg:'a -> StringRewriter<'b>)
                 (source:'a) : StringRewriter<'c> = 
        rewrite { 
            let! b = mg source
            let! c = mf b
            return c
        }

    // ****************************************************
    // Recursive functions

    /// Implemented in CPS
    let sequenceM (actions: StringRewriter<'a> list) : StringRewriter<'a list> = 
        StringRewriter <| fun opts input -> 
            let rec work (rules : StringRewriter<'a> list) 
                         (st : string) 
                         (fk : ErrMsg -> Answer<'a list>) 
                         (sk : 'a list -> string -> Answer<'a list>) = 
                match rules with
                | [] -> sk [] st
                | rule1 :: rest -> 
                    match apply1 rule1 opts st with
                    | Error msg -> fk msg
                    | Ok (a1, st1) -> 
                        work rest st1 fk (fun acc st2 ->
                        sk (a1::acc) st2)
            work actions input (fun msg -> Error msg) (fun ans st -> Ok (ans, st))

    /// Implemented in CPS
    let sequenceMz (actions: StringRewriter<'a> list) : StringRewriter<unit> = 
        StringRewriter <| fun opts input -> 
            let rec work (rules : StringRewriter<'a> list) 
                         (st : string) 
                         (fk : ErrMsg -> Answer<unit>) 
                         (sk : string -> Answer<unit>) = 
                match rules with
                | [] -> sk st
                | rule1 :: rest -> 
                    match apply1 rule1 opts st with
                    | Error msg -> fk msg
                    | Ok (_, st1) -> 
                        work rest st1 fk sk
            work actions input (fun msg -> Error msg) (fun st -> Ok ((), st))

    /// Implemented in CPS 
    let mapM (mf: 'a -> StringRewriter<'b>) 
             (source : 'a list) : StringRewriter<'b list> = 
        StringRewriter <| fun opts input -> 
            let rec work (xs : 'a list) 
                         (st : string) 
                         (fk : ErrMsg -> Answer<'b list>) 
                         (sk : 'b list -> string -> Answer<'b list>) = 
                match xs with
                | [] -> sk [] st
                | y :: ys -> 
                    match apply1 (mf y) opts st with
                    | Error msg -> fk msg
                    | Ok (a1, st1) -> 
                        work ys st1 fk (fun acc st2 ->
                        sk (a1::acc) st2)
            work source input (fun msg -> Error msg) (fun ans st -> Ok (ans, st))

    /// Flipped mapM
    let forM (source:'a list) 
             (mf: 'a -> StringRewriter<'b>) : StringRewriter<'b list> = 
        mapM mf source

    /// Forgetful mapM
    let mapMz (mf : 'a -> StringRewriter<'b>) 
              (source : 'a list) : StringRewriter<unit> = 
        StringRewriter <| fun opts input -> 
            let rec work (xs : 'a list) 
                         (st : string) 
                         (fk : ErrMsg -> Answer<unit>) 
                         (sk : string -> Answer<unit>) = 
                match xs with
                | [] -> sk st
                | y :: ys -> 
                    match apply1 (mf y) opts st with
                    | Error msg -> fk msg
                    | Ok (_, st1) -> 
                        work ys st1 fk sk
            work source input (fun msg -> Error msg) (fun st -> Ok ((), st))

    /// Flipped mapMz
    let forMz (source:'a list) 
              (mf: 'a -> StringRewriter<'b>) : StringRewriter<unit> = 
        mapMz mf source


    /// Implemented in CPS 
    let mapiM (mf:int -> 'a -> StringRewriter<'b>) 
              (source:'a list) : StringRewriter<'b list> = 
        StringRewriter <| fun opts input -> 
            let rec work (xs : 'a list) 
                         (n : int) 
                         (st : string) 
                         (fk : ErrMsg -> Answer<'b list>) 
                         (sk : 'b list -> string -> Answer<'b list>) = 
                match xs with
                | [] -> sk [] st
                | y :: ys -> 
                    match apply1 (mf n y) opts st with
                    | Error msg -> fk msg
                    | Ok (a1, st1) -> 
                        work ys (n+1) st1 fk (fun acc st2 ->
                        sk (a1::acc) st2)
            work source 0 input (fun msg -> Error msg) (fun ans st -> Ok (ans, st))

    /// Flipped mapMi
    let foriM (source:'a list) 
              (mf: int -> 'a -> StringRewriter<'b>)  : StringRewriter<'b list> = 
        mapiM mf source

    /// Forgetful mapiM
    let mapiMz (mf: int -> 'a -> StringRewriter<'b>) 
              (source:'a list) : StringRewriter<unit> = 
        StringRewriter <| fun opts input -> 
            let rec work (xs : 'a list) 
                         (n : int) 
                         (st : string) 
                         (fk : ErrMsg -> Answer<unit>) 
                         (sk : string -> Answer<unit>) = 
                match xs with
                | [] -> sk st
                | y :: ys -> 
                    match apply1 (mf n y) opts st with
                    | Error msg -> fk msg
                    | Ok (_, st1) -> 
                        work ys (n+1) st1 fk sk
            work source 0 input (fun msg -> Error msg) (fun st -> Ok ((), st))

    /// Flipped mapiMz
    let foriMz (source:'a list) 
               (mf: int -> 'a -> StringRewriter<'b>) : StringRewriter<unit> = 
        mapiMz mf source

    // ****************************************************
    // Operators

    // ****************************************************
    // Errors

    /// Operator for swapError
    let ( <?&> ) (msg:string) (ma:StringRewriter<'a>) : StringRewriter<'a> = 
        swapError msg ma

    /// Operator for flip swapError
    let ( <&?> ) (ma:StringRewriter<'a>) (msg:string) : StringRewriter<'a> = 
        swapError msg ma


    // ****************************************************
    // Monadic operations

    /// Bind operator
    let ( >>= ) (ma:StringRewriter<'a>) 
              (fn:'a -> StringRewriter<'b>) : StringRewriter<'b> = 
        bindM ma fn

    /// Flipped Bind operator
    let ( =<< ) (fn:'a -> StringRewriter<'b>) 
              (ma:StringRewriter<'a>) : StringRewriter<'b> = 
        bindM ma fn


    /// Operator for fmap.
    let ( |>> ) (ma:StringRewriter<'a>) (fn:'a -> 'b) : StringRewriter<'b> = 
        fmapM fn ma

    /// Flipped fmap.
    let ( <<| ) (fn:'a -> 'b) (ma:StringRewriter<'a>) : StringRewriter<'b> = 
        fmapM fn ma

    /// Operator for altM
    let ( <||> ) (ma:StringRewriter<'a>) 
               (mb:StringRewriter<'a>) : StringRewriter<'a> = 
        altM ma mb 


    /// Operator for apM
    let ( <**> ) (ma:StringRewriter<'a -> 'b>) 
               (mb:StringRewriter<'a>) : StringRewriter<'b> = 
        apM ma mb

    /// Operator for fmapM
    let ( <&&> ) (fn:'a -> 'b) (ma:StringRewriter<'a>) : StringRewriter<'b> = 
        fmapM fn ma



    /// Operator for seqL
    let (.>>) (ma:StringRewriter<'a>) 
              (mb:StringRewriter<'b>) : StringRewriter<'a> = 
        seqL ma mb

    /// Operator for seqR
    let (>>.) (ma:StringRewriter<'a>) 
              (mb:StringRewriter<'b>) : StringRewriter<'b> = 
        seqR ma mb



    /// Operator for kleisliL
    let (>=>) (mf : 'a -> StringRewriter<'b>)
              (mg : 'b -> StringRewriter<'c>)
              (source:'a) : StringRewriter<'c> = 
        kleisliL mf mg source


    /// Operator for kleisliR
    let (<=<) (mf : 'b -> StringRewriter<'c>)
              (mg : 'a -> StringRewriter<'b>)
              (source:'a) : StringRewriter<'c> = 
        kleisliR mf mg source


    // ************************************************************************
    // Combinators 

    /// Run the rewrite, succeed if the input has changed. Fail on no change.
    let progressive (ma:StringRewriter<'a>) : StringRewriter<'a> = 
        StringRewriter <| fun opts input -> 
            match apply1 ma opts input with
            | Error msg -> Error msg
            | Ok(a, st1) -> 
                if st1 <> input then 
                    Ok(a, st1)
                else Error "progressive"




