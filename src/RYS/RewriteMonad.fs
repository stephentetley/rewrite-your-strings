// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace RYS

module RewriteMonad =


    type ErrMsg = string

    type private Answer<'a> = Result<string * 'a, ErrMsg> 

    type RewriteMonad<'a> = 
        RewriteMonad of (string -> Answer<'a>)

    let inline private apply1 (ma: RewriteMonad<'a>) 
                              (input: string) : Answer<'a>= 
        let (RewriteMonad f) = ma in f input


    let inline mreturn (x:'a) : RewriteMonad<'a> = 
        RewriteMonad <| fun input -> Ok (input, x)


    let inline private bindM (ma: RewriteMonad<'a>) 
                        (f :'a -> RewriteMonad<'b>) : RewriteMonad<'b> =
        RewriteMonad <| fun input -> 
            match apply1 ma input with
            | Error msg -> Error msg
            | Ok (input2, a) -> apply1 (f a) input2

    let inline private zeroM () : RewriteMonad<'a> = 
        RewriteMonad <| fun _ -> Error "zeroM"

    /// "First success" 
    /// If mfirst fails tray to apply msecond
    let inline private combineM (mfirst:RewriteMonad<'a>) 
                                (msecond:RewriteMonad<'a>) : RewriteMonad<'a> = 
        RewriteMonad <| fun input -> 
            match apply1 mfirst input with
            | Error msg -> apply1 msecond input
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
        apply1 ma input

    let rewrite (ma:RewriteMonad<'a>) (input:string) 
                 : Result<string, ErrMsg> = 
        match apply1 ma input with
        | Error msg -> Error msg
        | Ok(output, _) -> Ok output

