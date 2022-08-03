module PankoAST

(* Abstract Syntax Tree *)

type Canvas = int * int

type Color = string // hex color code: 000000 - ffffff

type Background = string

type Coordinate = int * int
type Shape = 
| Rectangle of width: int * height: int
| Circle of radius: int
| RectangleRand of (int*int) * (int*int)
| CircleRand of radius: int*int 
// more shapes later 

type Element = 
| BrushStroke of Shape 
| Col of Color
| Coord of int 
| Var of string
| Rand of int * int 
| Int of int 
| Envi of Env
and Env = Map<string, Element> 

//type Env = Map<string, Element> 

//type List = Element list 

// fix this to be recursive with and type.. later 


type Expr = 
| Assignment of Element * Element 
// paintfun: brushstroke, color, coordx, coordy 
| PaintFun of Element * Element * Element * Element 
| Gradient of Element * Element * Element * Element * Element  
| Repeat of int * Expr list

type Exprs = Expr list 


// each painting has exactly 1 specification for canvas at top 
//type Painting = Canvas * Exprs
type Painting = 
| FullPainting of Canvas * Background * Exprs 
| BasePainting of Canvas * Background



(*
type Env = Map<Variable, String> * Env option 
| _ -> failwith "impossible" 
*)