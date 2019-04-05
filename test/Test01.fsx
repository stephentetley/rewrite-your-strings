// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
open System.Text.RegularExpressions


#load "..\src\RYS\RewriteMonad.fs"

open RYS.RewriteMonad
    
let demo01 () : string = 
    let proc = 
        rewriteMonad { return 1 }
    unsafeRewrite proc "hello world" 


let demo02 () = 
    let proc = replaceAllRe "world" "world!"
    unsafeRewrite proc "hello world" 



let demo03 () = 
    let proc = replaceAllRe "2" "two"
    unsafeRewrite proc "1-2-3-2-1" 

let demo04 () = 
    let proc = replace1Re "1" "one"
    unsafeRewrite proc "1-2-3-2-1" 

