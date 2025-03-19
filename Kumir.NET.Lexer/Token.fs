namespace Kumir.NET.Lexer

open Kumir.NET.Syntax

type Token =
    {
        TokenType:TokenType
        Location:Location
        Text:string
    }