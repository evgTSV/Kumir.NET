module LexerTests

open System.Collections.Generic
open System.IO
open Kumir.NET.Tests
open Kumir.NET.Lexer
open Kumir.NET.Syntax
open Xunit

let private testTokenization (source:string) testFunc =
    use stream = new StringReader(source)
    let sourceFile = FakeSource(stream)
    let lexer = Lexer(sourceFile)
    let tokens = lexer.Tokenization.Value
    testFunc tokens

[<Fact>]
let ``Empty source test`` () =
    let source = ""
    testTokenization source (fun tokens ->
        (tokens :> IEnumerable<Token>) |> Assert.Single |> ignore
        Assert.Equal(tokens.Read.TokenType, End)
        )
    
let AssignNewVariablesTest (tokens:TokenStream) (definedType:TokenType) (value:TokenType) =
    let expectedTokens = [|
        definedType; TokenType.Identifier; TokenType.Assign; value; TokenType.End
    |]
    Assert.Equal(expectedTokens.Length, tokens.Length)
    Assert.Equivalent(expectedTokens, tokens.tokens |> Seq.map _.TokenType)
    
[<Theory>]
[<InlineData("цел числецо := 10")>]
[<InlineData("цел ЧИСЛО:= 10 ")>]
[<InlineData("цел ЧИСЛО_:= 10")>]
[<InlineData("цел _ЧИСЛО:=10")>]
[<InlineData("цел май_число :=  10  | майЧисло")>]
let ``Integer literal assign new variables test`` (source:string) =
    testTokenization source (fun tokens ->
        AssignNewVariablesTest tokens TokenType.KeywordInt TokenType.IntegerLiteral)

[<Theory>]
[<InlineData("цел числецо :=-10")>]
[<InlineData("цел числецо := -10")>]
[<InlineData("цел числецо := - 10")>]
let ``Negative int literal test`` (source:string) =
    testTokenization source (fun tokens ->
        let expectedTokens = [|
            TokenType.KeywordInt; TokenType.Identifier; TokenType.Assign; TokenType.Subtract; TokenType.IntegerLiteral; TokenType.End
        |]
        Assert.Equal(expectedTokens.Length, tokens.Length)
        Assert.Equivalent(expectedTokens, (tokens :> IEnumerable<Token>) |> Seq.map _.TokenType))
    
[<Theory>]
[<InlineData("вещ число := 10.")>]
[<InlineData("вещ Число:= 10.3 ")>]
[<InlineData("вещ ЧИСЛО_:= 10.0")>]
[<InlineData("вещ _число:=1,0")>]
[<InlineData("вещ моё_число :=  0,10 | тут_моё_число")>]
let ``Real number literal assign new variables test`` (source:string) =
    testTokenization source (fun tokens ->
        AssignNewVariablesTest tokens TokenType.KeywordFloat TokenType.RealNumberLiteral)
    
[<Theory>]
[<InlineData("лит нитка_глаголовая := \"Ку, Мир!\"")>]
[<InlineData("лит НиткаГлаголовая:= \"Ку, Мир!\" ")>]
[<InlineData("лит НиткаГлаголовая_:= \"Ку, Мир!\"")>]
[<InlineData("лит _НиткаГлаголовая:=\"Ку, Мир!\"")>]
[<InlineData("лит ___Нитка_Глаголовая___ :=  \"Ку, Мир!\"  | продам гараж 88005553535")>]
let ``String literal assign new variables test`` (source:string) =
    testTokenization source (fun tokens ->
        AssignNewVariablesTest tokens TokenType.KeywordString TokenType.StringLiteral)
    
[<Theory>]
[<InlineData("сим глагол := 'А'")>]
[<InlineData("сим ГЛАГОЛ:= '\n' ")>]
[<InlineData("сим ГЛАГОЛ_:= '\025'")>]
[<InlineData("сим _ГЛАГОЛ:='a'")>]
[<InlineData("сим мой_ГЛАГОЛ :=  'A'  | мойГЛАГОЛ")>]
let ``Char literal assign new variables test`` (source:string) =
    testTokenization source (fun tokens ->
        AssignNewVariablesTest tokens TokenType.KeywordChar TokenType.CharLiteral)
    
[<Theory>]
[<InlineData("лог булеан := да")>]
[<InlineData("лог БУЛЕАН:= нет ")>]
[<InlineData("лог БУЛЕАН_:= да")>]
[<InlineData("лог _БУЛЕАН:=нет")>]
[<InlineData("лог мой_БУЛЕАН :=  да  | мойЛОГ")>]
let ``Boolean literal assign new variables test`` (source:string) =
    testTokenization source (fun tokens ->
        AssignNewVariablesTest tokens TokenType.KeywordBool TokenType.BooleanLiteral)
    
[<Theory>]
[<InlineData("Точка новая := старая")>]
[<InlineData("Точка новая:= старая ")>]
[<InlineData("Точка новая_:= старая")>]
[<InlineData("Точка _новая:=старая")>]
[<InlineData("Точка мая_новая :=  старая  | новая <== старая")>]
let ``Custom type assign variables test`` (source:string) =
    testTokenization source (fun tokens ->
        AssignNewVariablesTest tokens TokenType.Identifier TokenType.Identifier)
    
[<Theory>]
[<InlineData("| проигнорено")>]
[<InlineData("| проигнорено")>]
[<InlineData("|")>]
let ``Ignore comment lexemes test`` (source:string) =
    testTokenization source Assert.Single

[<Fact>] 
let ``Multiline statement test``() =
    let source = """алг сим __ток_сим(арг лит исхКод, арг цел индекс) нач 
если индекс > длин(исхКод) или индекс < 1 то
знач := юнисимвол(0)
иначе
знач := исхКод[индекс]
все
кон"""
    let expectedTokens = [|
        KeywordAlgorithmHeader; KeywordChar; Identifier; OpenParen; KeywordAlgorithmArgument; KeywordString; Identifier; Comma; KeywordAlgorithmArgument; KeywordInt; Identifier; CloseParen; KeywordBeginAlgorithmImplementation; NewLine
        KeywordIf; Identifier; CloseAngleBracket; Identifier; OpenParen; Identifier; CloseParen; LogicalOr; Identifier; OpenAngleBracket; IntegerLiteral; KeywordThen; NewLine
        Identifier; Assign; Identifier; OpenParen; IntegerLiteral; CloseParen; NewLine
        KeywordElse; NewLine
        Identifier; Assign; Identifier; OpenBracket; Identifier; CloseBracket; NewLine
        KeywordBlockEnd; NewLine
        KeywordEndAlgorithmImplementation; End
    |]
    testTokenization source (fun tokens ->
        Assert.Equal(expectedTokens.Length, tokens.Length)
        Assert.Equivalent(expectedTokens, (tokens :> IEnumerable<Token>) |> Seq.map _.TokenType)
        )