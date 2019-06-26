// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\lib\netstandard2.0"
#r "ExcelProvider.Runtime.dll"

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\typeproviders\fsharp41\netstandard2.0"
#r "ExcelDataReader.DataSet.dll"
#r "ExcelDataReader.dll"
#r "ExcelProvider.DesignTime.dll"
open FSharp.Interop.Excel

#load "..\src\RewriteYourStrings\Internal\Levenshtein.fs"
#load "..\src\RewriteYourStrings\RewriteMonad.fs"
#load "..\src\RewriteYourStrings\Query.fs"
#load "..\src\RewriteYourStrings\Transform.fs"
open RewriteYourStrings.RewriteMonad
open RewriteYourStrings

type InstTable = 
    ExcelFile< FileName = @"G:\work\Projects\rtu\edms_uploads_2.xlsx",
               SheetName = "Sheet1!",
               ForceString = true >

type InstRow = InstTable.Row


let readInstTable () : seq<InstRow> = 
    let isBlank (row : InstRow) = match row.GetValue(0) with null -> true | _ -> false 
    let allrows = (new InstTable ()).Data
    allrows |> Seq.filter (not << isBlank)

let mm3xFirmware : Rewrite = 
    whenM (isLiteralMatch "MM3x RTU Firmware") (constR "<<<Wrong title, remake file>>>" )

let erskineTooLong1 : Rewrite = 
    replace1Re "Erskine Battery Asset Replacement Install Sheet" "Erskine Battery Replacement"

let erskineTooLong2 : Rewrite = 
    replace1Re "Erskine Battery Asset Replacement" "Erskine Battery Replacement"

let rewriteTitle : Rewrite = 
    trim >>. choice [ mm3xFirmware; erskineTooLong1; erskineTooLong2 ]

let main () = 
    readInstTable () 
        |> Seq.map (fun row -> row.Title , rewriteOrId rewriteTitle row.Title)
        |> Seq.iter (fun (a,b) -> printfn "%s,%s" (a.Trim()) b) 




