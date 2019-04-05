// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#load "..\src\RYS\RewriteMonad.fs"

open RYS.RewriteMonad

let demo01 () : Result<string, ErrMsg> = 
    let proc = 
        rewriteMonad { return 1 }
    rewrite proc "hello world" 


