namespace Kumir.NET.Lexer

open System.Text
open Kumir.NET.Syntax
open FSharp.Data.Validator

type ReadingState =
    | Start
    | Identifier of List<char>
    | StringLiteral of List<char>
    | CharLiteral of List<char>
    | Number of List<char> * isReal:bool
    | Operator of List<char>
    | PunctuationOperator of List<char>
    | SpecialSymbol of char
    | Comment

type Lexer(source:SourceFile) =
    member private this.prepareText (lexemes:List<char>) =
        let rec iterLexemes (list:List<char>) builder =
            if list.IsEmpty then builder().ToString()
            else iterLexemes list.Tail (fun() ->
                StringBuilder(list.Head.ToString()).Append(builder().ToString()))
        iterLexemes lexemes (fun() -> StringBuilder())
    
    member private this.getLocation (text:string) =
        let start = Position(
            source.CurrentPosition.Line,
            source.CurrentPosition.Column - text.Length - 1)
        let range = Range(start, text.Length)
        Location(source, range)
        
    member private this.makeToken (lexemes:List<char>, ?tokenType:TokenType) =
        let text = this.prepareText lexemes
        let ``type`` =
            if tokenType.IsSome then tokenType.Value
            else TokenRegexes.findMatchToken text
        let location = this.getLocation text
        { TokenType = ``type``; Location = location; Text = text }
        
    member private this.getState (lexeme:char) : ReadingState =
        match lexeme with
        | l when l = '\n' -> SpecialSymbol l
        | l when System.Char.IsLetter(l) || l = '_' -> Identifier(List.Empty)
        | l when System.Char.IsDigit(l) -> Number(List.Empty, false)
        | l when l = '/' && (match source.ReadChar() with | Char value -> value = '/' | _ -> false) -> Comment
        | '"' -> StringLiteral(List.Empty)
        | ''' -> CharLiteral(List.Empty)
        | l when System.Char.IsSymbol(l) -> Operator(List.Empty)
        | l when System.Char.IsPunctuation(l) -> PunctuationOperator(List.Empty)
        | _ -> Start
    
    member this.Tokenization : TokenStream Lazy = lazy (
        let tokenStream = TokenStream()
        
        let gotoByLexeme stateMachine (lexeme:Lexeme) =
            match lexeme with
            | Char(value) -> stateMachine (this.getState(value)) lexeme
            | Lexeme.End -> stateMachine Start Lexeme.End
          
        let rec next (state:ReadingState) (lexeme:Lexeme) =
            match lexeme with
            | Char(value) ->
                match state with
                | Start ->
                    if source.ReadChar() = Lexeme.End then
                            this.makeToken([value]) |> tokenStream.AddToken
                            next Start Lexeme.End
                    else match value with
                            | v when v |> (System.Char.IsWhiteSpace |=| System.Char.IsControl) ->
                                if v = '\n' then
                                    this.makeToken([value], TokenType.NewLine)
                                    |> tokenStream.AddToken
                                source.MoveAndRead() |> gotoByLexeme next 
                            | _ -> lexeme |> gotoByLexeme next 
                | Identifier(lexemes) ->
                    match value with
                    | v when v |> (System.Char.IsLetterOrDigit |=| (=) '_') ->
                        source.MoveAndRead() |> next (Identifier(v :: lexemes))
                    | _ ->
                        this.makeToken(lexemes) |> tokenStream.AddToken
                        lexeme |> gotoByLexeme next
                | StringLiteral(lexemes) ->
                    match value with
                    | v when v = '"' && lexemes.Length > 0 ->
                        this.makeToken('"' :: lexemes) |> tokenStream.AddToken
                        source.MoveAndRead() |> gotoByLexeme next
                    | v -> source.MoveAndRead() |> next (StringLiteral(v :: lexemes))
                | CharLiteral(lexemes) ->
                    match value with
                    | v when v = ''' && lexemes.Length > 0 ->
                        this.makeToken(''' :: lexemes) |> tokenStream.AddToken
                        source.MoveAndRead() |> gotoByLexeme next
                    | v ->
                        source.MoveAndRead() |>
                        next (CharLiteral(v :: lexemes))
                | Number(lexemes, isReal) ->
                    match value with
                    | v when System.Char.IsDigit(v) ->
                        source.MoveAndRead() |> next (Number(v :: lexemes, isReal))
                    | v when (v = '.' || v = ',') && not isReal ->
                        source.MoveAndRead() |> next (Number(v :: lexemes, true))
                    | _ ->
                        this.makeToken(lexemes) |> tokenStream.AddToken
                        lexeme |> gotoByLexeme next
                | Operator(lexemes) ->
                    match value with
                    | v when System.Char.IsSymbol(v) ->
                        source.MoveAndRead() |> next (Operator(v :: lexemes))
                    | _ ->
                        this.makeToken(lexemes) |> tokenStream.AddToken
                        lexeme |> gotoByLexeme next
                | PunctuationOperator(lexemes) ->
                    match value with
                    | v when System.Char.IsPunctuation v ->
                        match source.MoveAndRead() with
                        | Char c when c = '=' -> // Special case for ':='
                            this.makeToken([c; v]) |> tokenStream.AddToken
                            source.MoveAndRead() |> gotoByLexeme next
                        | c ->
                            this.makeToken([v]) |> tokenStream.AddToken
                            c |> gotoByLexeme next
                    | _ ->
                        this.makeToken(lexemes) |> tokenStream.AddToken
                        lexeme |> gotoByLexeme next
                | Comment ->
                    let rec skip (lexeme:Lexeme) =
                        match lexeme with
                        | Char value when value = '\n' -> source.MoveAndRead() |> gotoByLexeme next
                        | Lexeme.End -> next Start Lexeme.End
                        | _ -> skip (source.MoveAndRead())
                    skip lexeme
                | SpecialSymbol l ->
                    match source.MoveAndRead() with
                    | Char c when c = '\r' ->
                            this.makeToken([c; l], TokenType.NewLine) |> tokenStream.AddToken
                            source.MoveAndRead() |> gotoByLexeme next
                    | c ->
                        this.makeToken([l], TokenType.NewLine) |> tokenStream.AddToken
                        c |> gotoByLexeme next
                                                     
            | Lexeme.End ->
                match state with
                | Identifier lexemes -> this.makeToken(lexemes) |> tokenStream.AddToken
                | StringLiteral lexemes -> this.makeToken(lexemes) |> tokenStream.AddToken
                | CharLiteral lexemes -> this.makeToken(lexemes) |> tokenStream.AddToken
                | Number(lexemes, _) -> this.makeToken(lexemes) |> tokenStream.AddToken
                | Operator lexemes -> this.makeToken(lexemes) |> tokenStream.AddToken
                | PunctuationOperator lexemes -> this.makeToken(lexemes) |> tokenStream.AddToken
                | _ -> ()
                tokenStream.AddToken(this.makeToken(List.Empty, TokenType.End))
                
        next ReadingState.Start (source.MoveAndRead())
        tokenStream)