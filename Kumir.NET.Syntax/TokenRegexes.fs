namespace Kumir.NET.Syntax

open System
open System.Globalization
open FParsec

module TokenRegexes =
    
    [<Literal>]
    let IdentifierLengthLimit = 100
    
    let private identifier (lexeme:string) =
        if lexeme.Length > IdentifierLengthLimit then false
        else
        let idParser : Parser<string, unit> = many1Chars2 (letter <|> pchar '_') (letter <|> digit <|> pchar '_')
        match run idParser lexeme with
            | Success(_, _, position)
                when position.Index = lexeme.Length -> true
            | _ -> false
        
    let private stringLiteral(literal:string) =
        literal.StartsWith("\"") && literal.EndsWith("\"")
        
    let private charLiteral(literal:string) =
        literal.StartsWith("'") && literal.EndsWith("'") && literal.Length = 3
    
    let private int32Number(literal:string) =
        let isInt, _ = Int32.TryParse(literal)
        isInt
        
    let private int64Number(literal:string) =
        let isInt, _ = Int64.TryParse(literal)
        isInt
        
    let private realNumber(literal:string) =
        let isReal, _ = Double.TryParse(literal, CultureInfo.InvariantCulture)
        isReal
        
    let private booleanLiteral(literal:string) = literal = "да" || literal = "нет"

    let findMatchToken(lexeme:string) =
        match lexeme with
            | l when l = Environment.NewLine -> NewLine
            | l when (String.IsNullOrWhiteSpace l) -> Empty
                   
            | "(" -> OpenParen
            | ")" -> CloseParen
            | "[" -> OpenBracket
            | "]" -> CloseBracket
            | "{" -> OpenBrace
            | "}" -> CloseBrace
            | "<" -> OpenAngleBracket
            | ">" -> CloseAngleBracket
            
            | "=" -> Equal
            | "<>" -> NotEqual
            | "и" -> LogicalAnd
            | "или" -> LogicalOr
            | "не" -> LogicalNot
            | ">=" -> GreaterOrEqual
            | "<=" -> LessOrEqual
            
            | "++" -> Increment
            | "--" -> Decrement
            
            | "+" -> Add
            | "+=" -> AddAssign
            | "-" -> Subtract
            | "-=" -> SubtractAssign
            | "*" -> Multiple
            | "*=" -> MultipleAssign
            | "/" -> Divide
            | "/=" -> DivideAssign
            | "%" -> Module
            | "%=" -> ModuleAssign
            
            | "&" -> BitAnd
            | "&=" -> BitAndAssign
            | "|" -> BitOr
            | "|=" -> BitOrAssign
            | "^" -> BitXor
            | "^=" -> BitXorAssign
            | "~" -> BitNot
            | "<<" -> ShiftLeft
            | "<<=" -> ShiftLeftAssign
            | ">>" -> ShiftRight
            | ">>=" -> ShiftRightAssign
            
            | "->" -> TokenType.Array
            | "." -> Dot
            | "," -> Comma
            | ":" -> Colon
            | ";" -> Semicolon
            | ":=" -> Assign
            | "`" -> Backquote
            | "'" -> QuotationMark
            | "\"" -> DoubleQuotationMark
            | "$" -> DollarSign
            | "//" -> DoubleSlash
            | "?" -> QuestionMark
            | "??" -> DoubleQuestionMark
            | "#" -> Hash
            | "##" -> HashHash
            | "..." -> Ellipsis
            
            | l when stringLiteral l -> StringLiteral
            | l when charLiteral l -> CharLiteral
            | l when int32Number l -> IntegerLiteral
            | l when int64Number l -> LongIntegerLiteral
            | l when realNumber l -> RealNumberLiteral
            | l when booleanLiteral l -> BooleanLiteral
            
            | l when identifier l ->
                match l with
                | "знак" -> KeywordSigned
                | "беззнак" -> KeywordUnsigned
                | "длинный" -> KeywordLong
                | "короткий" -> KeywordShort
                | "ничего" -> KeywordVoid
                | "данные" -> KeywordStruct
                | "перечисление" -> EnumStruct
                | "объединение" -> KeywordUnion
                | "изменчивый" -> KeywordVolatile
                | "static" -> KeywordStatic
                | "sizeof" -> KeywordSizeof
                | "если" -> KeywordIf
                | "то" -> KeywordThen
                | "иначе" -> KeywordElse
                | "всё" -> KeywordBlockEnd
                | "все" -> KeywordBlockEnd
                | "перейти" -> KeywordGo
                | "выбор" -> KeywordSwitch
                | "при" -> KeywordCase
                | "остальное" -> KeywordDefault
                | "нц" -> KeywordBeginLoop
                | "кц" -> KeywordEndLoop
                | "для" -> KeywordFor
                | "раз" -> KeywordTimes
                | "пока" -> KeywordWhile
                | "от" -> KeywordFrom
                | "до" -> KeywordTo
                | "к" -> KeywordTo
                | "шаг" -> KeywordStep
                | "выход" -> KeywordBreak
                | "алг" -> KeywordAlgorithmHeader
                | "нач" -> KeywordBeginAlgorithmImplementation
                | "кон" -> KeywordEndAlgorithmImplementation
                | "дано" -> KeywordPreCondition
                | "надо" -> KeywordPostCondition
                | "утв" -> KeywordAssertion
                | "арг" -> KeywordAlgorithmArgument
                | "ввод" -> KeywordInput
                | "вывод" -> KeywordOutput
                | "Фввод" -> KeywordFileInput
                | "Фвывод" -> KeywordFileOutput
                | "нс" -> KeywordNewLine
                | "цел" -> KeywordInt
                | "вещ" -> KeywordFloat
                | "сим" -> KeywordChar
                | "лит" -> KeywordString
                | "лог" -> KeywordBool
                | "таб" -> KeywordArray
                | "исп" -> KeywordBeginModule
                | "использовать" -> KeywordUseModule
                | _ -> Identifier
            
            | _ -> Unknown