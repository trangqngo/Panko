module ProjectIntepreter

open System
open ProjectParser
open PankoAST 
open ColorConverter

let rand = Random()
//NOTE: all eval functions return a tuple of (str, env) where str is the accumulated 
// svg content and env is the list of environments of variables (list of scopes)
 
let evalCanvas (canvas: Canvas) (res: string) (env: Env) : string*Env = 
    match canvas with 
        | (w, h) ->
            let res' = res + "<svg width =" + "\"" + string(w) + "\"" + " height=" + "\"" + string(h) + "\"" + ">\n"
            res', env 

let evalBackground (bg: Background) (res: string) (env: Env) : string*Env = 
    let res' = res +  "<rect width=\"100%\" height=\"100%\" fill=\"#" + string(bg) + "\"" + "/>\n"
    res', env 

let evalAssignment (name: Element) (element: Element) (res: string) (env: Env) : string*Env = 
    match element with 
    | Var(varName) -> 
        failwith("Illegal assignment: " + varName + " is not a brushstroke, a color, or coordinate.")
    | _ ->
        match name with 
        | Var(varName) ->
            match element with 
            | Rand(lb,ub) ->
                let e = Coord(rand.Next(lb,ub))
                let env' = env.Add(varName,e)
                res, env'
            | _ -> 
                let env' = env.Add(varName, element)
                // return the updated environment 
                res, env'
        | _ -> 
            failwith("Illegal assignment: " + string(name) + " is not a valid variable name.")

let evalCircle (r: int) (col: Color) (coordx: int) (coordy: int) (res: string) (env: Env) : string*Env = 
    let res' = res + "<circle cx = \"" + string(coordx) + "\"" + " cy = \"" + string(coordy) + "\"" + " r=" + "\"" + string(r) + "\"" +
                                " fill = " + "\"#" + col  + "\"" + "/>\n"
    res', env

let evalRectangle (w: int) (h: int) (col: Color) (coordx: int) (coordy: int) (res: string) (env: Env) : string*Env = 
    let res' = res + "<rect x = \"" + string(coordx) + "\"" + " y = \"" + string(coordy) + "\"" + " width=" + "\"" + string(w) + "\"" +
                               " height=" + "\"" + string(h) + "\"" + " fill = " + "\"#" + col  + "\"" + "/>\n"
    res', env
    

let evalBrushStroke (shape: Shape) (col: Color) (coordx: int) (coordy:int) (res: string) (env: Env) : string*Env = 
    match shape with 
    | Rectangle(w,h) ->
        evalRectangle w h col coordx coordy res env
    | RectangleRand((w1,w2),(h1,h2)) ->
        if w1 >= w2 then 
            failwith ("("+ string(w1) + ", " + string(w2) + ") is not a valid range. The first value should be smaller than the second value.")
        else if h1 >= h2 then 
            failwith ("("+ string(h1) + ", " + string(h2) + ") is not a valid range. The first value should be smaller than the second value.")   
        else 
            let w = rand.Next(w1, w2)
            let h = rand.Next(h1, h2)
            evalRectangle w h col coordx coordy res env 
    | Circle(r) ->
        evalCircle r col coordx coordy res env 
    | CircleRand(r1,r2) -> 
        if r1 >= r2 then 
            failwith ("("+ string(r1) + ", " + string(r2) + ") is not a valid range. The first value should be smaller than the second value.")
        else 
            let r = rand.Next(r1,r2)
            evalCircle r col coordx coordy res env 


let evalPaintFun (name: Element) (col: Element) (x: Element) (y: Element) (res: string) (env: Env) : string*Env = 
    // helper func: not outside because depends on env...
    // check if variable is saved in envi
    // "extract" all variables -> Elements
    let varToElement (e: Element): Element = 
        match e with 
        | Var(varName) ->
            // this is a variable, let's get its value
            if env.ContainsKey varName then 
                env.Item varName    
            else 
                failwith ("Undefined variable '" + varName + ".")
        | ele -> ele 

    let args = [name; col; x; y]
    let eleList = args |> List.map varToElement 
    // now all variables' values have been extracted 
    let brush = eleList.Head 
    let col = eleList.Tail.Head
    let x = eleList.Tail.Tail.Head
    let y = eleList.Tail.Tail.Tail.Head
    match x with 
    | Coord(x) -> 
        let coordx = x 
        match y with 
        | Coord(y) ->
            let coordy = y
            match col with 
            | Col(colStr) ->
                let col = colStr
                match brush with 
                | BrushStroke(shape) ->
                    evalBrushStroke shape col coordx coordy res env 
                | _ -> failwith (string(brush) + " is not a valid brush stroke.")
            | _ -> failwith (string(col) + " is not a valid color.")
        | _ -> failwith (string(y) + " is not a valid y-coordinate.")  
    | _ -> failwith (string(x) + " is not a valid x-coordinate.") 

// given two colors, return a random color that is "between" the two colors 
let getRandomColor (col1: Color) (col2: Color) : Color = 
    let r1,g1,b1 = hexToRGB col1
    let r2,g2,b2 = hexToRGB col2
    let r = if r1>r2 then rand.Next(r2,r1) else  rand.Next(r1,r2)
    let g = if g1>g2 then rand.Next(g2,g1) else  rand.Next(g1,g2)
    let b = if b1>b2 then rand.Next(b2,b1) else  rand.Next(b1,b2)
    (r,g,b) |> RGBToHex |> Color
            
let evalGradient (name: Element) (col1: Element) (col2: Element) (x: Element) (y: Element) (res: string) (env: Env) : string * Env = 
    let varToElement (e: Element): Element = 
        match e with 
        | Var(varName) ->
            // this is a variable, let's get its value
            if env.ContainsKey varName then 
                env.Item varName    
            else 
                failwith ("Undefined variable '" + varName + ".")
        | ele -> ele 

    let args = [col1; col2]
    let eleList = args |> List.map varToElement 
    let col1 = eleList.Head
    let col2 = eleList.Tail.Head
    // now both colors are color codes 
    match col1 with 
    | Col(colorStr1) ->
        match col2 with 
        | Col(colorStr2) ->
            let col = getRandomColor colorStr1 colorStr2 |> Col 
            evalPaintFun name col x y res env
        | _ -> failwith (string col2 + " is not a valid color.")
    | _ -> failwith (string col1 + " is not a valid color.")


let rec evalExpr (expr: Expr) (res: string) (env: Env) : string*Env = 
    match expr with 
    | Assignment(str, element) -> 
        // returns the new env
        evalAssignment str element res env 
    | PaintFun(var, col, x, y) ->
        evalPaintFun var col x y res env
    | Repeat(num, exprs) ->
        // NOTE: because of the simplistic way scopes are implemented 
        // inner scope can overwrite outer scope variables 
        let rec evalExprs (exprs: Expr list) (res: string) (env: Env) : string*Env = 
            match exprs with 
            | [] -> res, env
            | e::es ->
                let res', env' = evalExpr e res env
                evalExprs es res' env'
        let rec evalRepeat num exprs res env : string * Env= 
            if num = 0 then 
                res, env
            else 
                let res', env' = evalExprs exprs res env
                evalRepeat (num-1) exprs res' env' 
        // return the old env (outer scope) - all variables in this current scope is wiped
        let res',_ = evalRepeat num exprs res env 
        res', env
    | Gradient(var, col1, col2, x, y) -> 
        evalGradient var col1 col2 x y res env
        
        
let rec evalExprs (exprs: Expr list) (res: string) (env: Env) : string*Env = 
    match exprs with 
    | [] -> res, env
    | e::es ->
        let res', env' = evalExpr e res env
        evalExprs es res' env'

let eval (painting: Painting) (res: string) (env: Env) : string = 
// let eval (painting: Painting) (res: string) (env: Env) : string = 
    match painting with 
    | FullPainting(canvas, background, exprs) -> 
        let resAfterCanvas, envAfterCanvas = evalCanvas canvas res env 
        let resBg, envBg = evalBackground background resAfterCanvas envAfterCanvas 
        let content, envi = evalExprs exprs resBg envBg 
        content + "</svg>\n" 
    | BasePainting(canvas, background) ->
        let resAfterCanvas, envAfterCanvas = evalCanvas canvas res env 
        let resBg, envBg = evalBackground background resAfterCanvas envAfterCanvas 
        printfn "%A" (resBg)
        resBg + "</svg>\n" 

    


        