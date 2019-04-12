// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
open System.Text.RegularExpressions


#load "..\src\RewriteYourStrings\RewriteMonad.fs"

open RewriteYourStrings.RewriteMonad
    
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

let demo05 () = 
    let proc = choice [replaceAllRe "2" "two"; replaceAllRe "1" "one"; replaceAllRe "3" "three"]
    unsafeRewrite proc "3-4-5-6-1-0" 


let demo06 () = 
    unsafeRewrite toUpper "hello world" 


let demo07 () = 
    unsafeRewrite (sequenceMz [trim; toUpper]) "hello world    "



