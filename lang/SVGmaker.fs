module SVGmaker 

open System 
open System.IO

(* Generates a xtml file 
 * input: a string
 * output: a string encoding a html file 
*)

let setupSVG = 
    "<!DOCTYPE html>\n
    <html>\n
    <body>\n"

let endSVG = 
    "</body>\n
    </html>\n"


(* Takes string generated by evaluator and creates a string *)
let makeSVG str = 
    setupSVG + str + endSVG 



    