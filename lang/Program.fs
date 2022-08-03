open System 
open System.IO
open Parser
open ProjectParser
open ProjectIntepreter
open SVGmaker

(* Creates output directory if not already exists
 * returns path to output dir. 
 *)
let makeOutputDir =
    let outputDirPath = Directory.GetCurrentDirectory() + "/output"
    // check if output dir exists 
    if not (Directory.Exists(outputDirPath)) then 
        // create output dir 
        Directory.CreateDirectory(outputDirPath) |> ignore
        outputDirPath
    else 
        outputDirPath
        
(* Create output file with name inside output dir 
 * and write contents to file or overwrites output file.
 *)
let makeOutputFile fileName contents outputDirPath = 
    let filePath =  outputDirPath + "/" + fileName + ".html"
    File.WriteAllText(filePath, contents)

(* Output file is saved as myFile.html in the output directory *)
[<EntryPoint>]
let main argv = 
    let msg = "Usage: dotnet run <filepath> [debug]"
    if argv.Length <> 1 && argv.Length <> 2 then // check for correct number of arguments
        printfn "%s" msg
        exit 1
    else 
        let file = argv.[0]  
        // check that input ends with extension .pk
        if file[(file.Length - 3)..(file.Length - 1)] <> ".pk" then 
            failwith (file + " does not have a valid extension. The input file must have extension .pk")
        else 
            let fileName = file[0..(file.Length - 4)]
            let input = File.ReadAllText file
            let debugFlag = if argv.Length = 2 then true else false
            let ast_maybe = parse input debugFlag
            match ast_maybe with 
            | Some(ast) -> 
               // printfn "%A" (ast)
                let contents = makeSVG (eval ast "" Map.empty)
                //printfn "EVAL: %A" (contents)
                makeOutputFile fileName contents (makeOutputDir)
            | None -> printfn "Invalid program"
            exit 0 
  

    

 
