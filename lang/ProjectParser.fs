module ProjectParser

open Parser
open PankoAST

(* TODO:
 * Give user option to turn on/off debug flag for parsers
 * More shapes! (curves,...)
 * Dan's feedback: brushes out of brushes? 
   A brush with multiple shapes! Brush-ception. 
*)

(* NOTE: I implemented scope in a very cursed way so 
   the inner scope can overwrite the outer scope's env 
   Rmb to note this in the tutorial /
   fix it in the future 
*)

(* TEST 
 * evaluate paintfun 
 *)

let reserved = [ "paint"; "gradient"; "canvas"; "background"; "randLoc"; "repeat" ] 


(* HELPER PARSERS FOR COMMON SYMBOLS *)
(* pad p
 *   Parses p, surrounded by optional whitespace.
 *)
let pad p = pbetween pws0 pws0 p <!> "pad"

(* PARSE VARIABLES *)
(* pvarchar and pvar are Dan's code from lecture 22 Spring '22*)
let pvarchar = pletter <|> pdigit <!> "pvarchar" 
let pvar = pseq pletter (pmany0 pvarchar |>> stringify) 
            (fun (c: char, s: string) -> (string c) + s)
            |>> (fun v ->
                                if List.contains v reserved then
                                    failwith ("'" + v + "' is a reserved word.")
                                else
                                    v
            ) |>> Var <!> "pvar"

// parses comma
let pcomma = (pad (pchar ',')) <!> "comma"

// pnumber is from class lecture 20 (Spring '22)
let pnumber =
    pmany1 pdigit
    |>> (fun ds -> int (stringify ds))
    <!> "number"

(* PARSE CANVAS *)
let pcanvassize = pseq (pbetween (pws1) (pws1) (pnumber)) pnumber id <!> "canvas size"
let pcanvas = pbetween (pstr "canvas") pnl pcanvassize |>> Canvas <!> "canvas" 

(* PARSE COLOR CODE *)
let phexdigit = pdigit <|> pchar 'A' <|> pchar 'B' 
                <|> pchar 'C' <|> pchar 'D'
                <|> pchar 'E' <|> pchar 'F'
                <|> pchar 'a' <|> pchar 'b' 
                <|> pchar 'c' <|> pchar 'd'
                <|> pchar 'e' <|> pchar 'f' <!> "hex digit"
let phexpair = pseq phexdigit phexdigit (fun (a,b) -> stringify([a;b])) <!> "two hex digits"
let pcolorcode = pseq (pseq phexpair phexpair (fun (a,b) -> a + b)) phexpair (fun (a,b) -> a + b) <!> "color code"
let pcolor = pcolorcode |>> Col <!> "color"

(* PARSE BACKGROUND *)
let pbackground = pbetween (pstr "background") pnl (pright pws0 pcolorcode) |>> Background <!> "background"

(* PARSE SHAPE *)

// PARSE RECTANGLE 
let prectwidth = pnumber <!> "rectangle width"
let prectheight = pnumber <!> "rectangle length"
let prectanglesize = pseq (pad prectwidth) (pad prectheight) id <!> "rectangle size"
let prectangle = pright (pstr "rectangle") (pad prectanglesize) |>> Rectangle 


let prandrectwidth =  pseq (pad pnumber) (pad pnumber) id <!> "rand rectangle width"
let prandrectheight = pseq (pad pnumber) (pad pnumber) id <!> "rand rectangle width"
let prandrectsize = pseq (pad prandrectwidth) (pad prandrectheight) id <!> "rand rectangle size"
let prandrectangle = pright (pstr "rectangle") (pad prandrectsize) |>> RectangleRand 

// PARSE CIRCLE 
let pcircle = pright (pstr "circle") (pad pnumber) |>> Circle <!> "circle"

let prandcirle = pright (pstr "circle") (pseq (pad pnumber) (pad pnumber) id) |>> CircleRand <!> "circle"

let pshape = prandrectangle <|> prectangle <|> prandcirle <|> pcircle <!> "shape"

(* PARSE BRUSH STROKE *)
let pbrushstroke = pright (pstr "stroke") (pad pshape) |>> BrushStroke <!> "brushstroke"

(* PARSE COORDINATES *)
let pcoord = ((pad pnumber) |>> Coord) 

(* PARSE RANDOM *)
let prandomrange = pseq (pad pnumber) (pad pnumber) id
let prandom = pright (pstr "randLoc") (pad prandomrange) |>> Rand

(* PARSE ELEMENT *)
let pelement = pbrushstroke <|> pcolor <|> pcoord <|> prandom <!> "element"

(* PARSE ASSIGNMENT *)


let passignmentname = pleft (pleft pvar pws0) (pad (pchar '=')) <!> "front of assignment"

let passignment = pseq (pleft passignmentname pws0) (pleft pelement pws0) id |>> Assignment <!> "assignment"


(* PARSE CALLS TO PAINT FUNCTION 
 * paint myBrush color xcoord ycoord
 *)
// parses the name of the brush stroke 
let ppaint1 = pright (pad (pstr "paint")) (pad pvar) <!> "paint fun 1"

// parses the color and the coordinates
let ppaint2 = pseq (pad (pcolor <|> pvar)) (pseq (pad (pcoord <|> pvar)) (pad (pcoord <|> pvar)) id) id <!> "paint fun 2"

let ppaintfun = pseq ppaint1 ppaint2 (fun (var, (col, (x, y))) -> (var, col, x, y)) |>> PaintFun <!> "paint fun"

(* PARSE CALLS TO GRADIENT FUNCTION 
 * gradient functions are similar to paintfun functions
 * except two colors are given and the color of the brushstroke
 * is a random color "between" the two colors:
 * gradient myBrush color1 color2 xcoord ycoord
 *)
// parses the brush stroke 
let pgradient1 = pright (pad (pstr "gradient")) (pad pvar) <!> "gradient 1"

// parses the two colors and coordinates 
let pgradient2 = pseq (pseq (pad (pcolor <|> pvar)) (pad (pcolor <|> pvar)) id) (pseq (pad (pcoord <|> pvar)) (pad (pcoord <|> pvar)) id) id  <!> "gradient 2"

let pgradient = pseq pgradient1 pgradient2 (fun (var, ((col1, col2), (x, y))) -> (var, col1, col2, x, y)) |>> Gradient <!> "gradient"

(* PARSE EXPRESSION *)

let pexpr,pexprImpl = recparser()

let pexprs = pmany1 (pright (pmany0 pnl) pexpr) <!> "expressions list" //new line is here, after every expression

(* PARSE REPEATS *)

let prepeatblock = pbetween (pad (pchar '(')) (pad (pchar ')')) pexprs

let prepeat = pseq (pright (pstr "repeat") (pad pnumber)) prepeatblock id |>> Repeat

(* EXPR PARSER IMPLEMENTATION  *)

pexprImpl := prepeat <|> ppaintfun <|> pgradient <|> passignment  <!> "expression"

let pprelim = pseq (pright (pmany0 pnl) (pad pcanvas)) (pright (pmany0 pnl) (pad pbackground)) id <!> "canvas and background"

(* PARSE PAINTING *)

let pbasepainting = pleft pprelim (pmany0 pnl) |>> BasePainting <!> "base painting"

let pfullpainting = (pseq pprelim pexprs (fun ((canvas,bg), exprs) -> FullPainting(canvas, bg, exprs)) <!> "full painting")  

let ppainting = pfullpainting <|> pbasepainting

let pgrammar = pleft ppainting peof <!> "grammar"

(* PARSE GRAMMAR *)
let parse (s: string) (debugFlag: bool): Painting option  = 
    let input = (if debugFlag then debug else prepare) s
    match (pgrammar input) with 
    | Success(ast,_) -> Some(ast)
    | Failure(_,_) -> 
        None 





