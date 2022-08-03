namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open PankoAST
open ProjectParser
open ProjectIntepreter

[<TestClass>]
type TestClass () =

    (* PARSER TESTS *)
    // test parser for brush stroke assignment 
    [<TestMethod>]
    member this.BrushStrokeAssignmentParsesCorrectly () = 
        let program = "myBrush = stroke rectangle 200 100"
        let ast_maybe = passignment (prepare program)
        let expected = Assignment (Var "myBrush", BrushStroke (Rectangle (200, 100)))
        match ast_maybe with
        | Success(res, _) -> 
            Assert.AreEqual(expected, res)
        | Failure _ -> 
            Assert.IsTrue false

    // test parser for paint function        
    [<TestMethod>]
    member this.BasicPaintFunParsesCorrectly () = 
        let program = "paint myBrush 6AB6AB 300 300"
        let ast_maybe = ppaintfun (prepare program)
        let expected = PaintFun (Var "myBrush", Col "6AB6AB" , Coord 300, Coord 300)
        match ast_maybe with
        | Success(res, _) -> 
            Assert.AreEqual(expected, res)
        | Failure _ -> 
            Assert.IsTrue false

    // test parser for repeat command 
    [<TestMethod>]
    member this.RepeatParsesCorrectly () = 
        let program = "repeat 10 (\n paint myBrush 6AB6AB 300 300 \n )"
        let ast_maybe = prepeat (prepare program)
        let expected = Repeat (10, [PaintFun (Var "myBrush", Col "6AB6AB" , Coord 300, Coord 300)])
        match ast_maybe with
        | Success(res, _) -> 
            Assert.AreEqual(expected, res)
        | Failure _ -> 
            Assert.IsTrue false

    (* EVALUATOR TESTS *)

    // test basic paint function evaluator
    [<TestMethod>]
    member this.TestParsedBasicPaintFun () = 
        let program = "myBrush = stroke rectangle 200 50 \n paint myBrush 6AB6AB 300 300"
        let ast_maybe = pexprs (prepare program)
        match ast_maybe with 
        | Success(ast,_) -> 
            let actual, _ = evalExprs ast "" Map.empty
            let expected =  "<rect x = \"300\" y = \"300\" width=\"200\" height=\"50\" fill = \"#6AB6AB\"/>\n"
            Assert.AreEqual(expected, actual)
        | Failure _ -> 
            Assert.IsTrue false
    
   
