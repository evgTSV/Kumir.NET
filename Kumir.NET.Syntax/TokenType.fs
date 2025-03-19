namespace Kumir.NET.Syntax

type TokenType =
    // Specials
    /// Empty or whitespace
    | Empty
    /// New line token ('\n' or '\r\n')
    | NewLine
    /// End of source
    | End
    /// Unrecognized
    | Unknown
    /// Identifier
    | Identifier
    
    // Brackets
    /// '('
    | OpenParen
    /// ')'
    | CloseParen
    /// '['
    | OpenBracket
    /// ']'
    | CloseBracket
    /// '<'
    | OpenAngleBracket
    /// '>'
    | CloseAngleBracket
    /// '{'
    | OpenBrace
    /// '}'
    | CloseBrace
    
    // Logical operators
    /// '='
    | Equal
    /// '<>'
    | NotEqual
    /// 'или'
    | LogicalAnd
    /// 'и'
    | LogicalOr
    /// 'не'
    | LogicalNot
    /// '>='
    | GreaterOrEqual
    /// '<='
    | LessOrEqual
    
    // Increment and Decrement
    /// '++'
    | Increment
    /// '--'
    | Decrement
    
    // Standard math operations
    /// '+'
    | Add
    /// '+='
    | AddAssign
    /// '-'
    | Subtract
    /// '-='
    | SubtractAssign
    /// '*'
    | Multiple
    /// '*='
    | MultipleAssign
    /// '/'
    | Divide
    /// '/='
    | DivideAssign
    /// '%'
    | Module
    /// '%='
    | ModuleAssign

    // Bits operations
    /// '&'
    | BitAnd
    /// '&='
    | BitAndAssign
    /// '|'
    | BitOr
    /// '|='
    | BitOrAssign
    /// '^'
    | BitXor
    /// '^='
    | BitXorAssign
    /// '~'
    | BitNot
    /// '<<'
    | ShiftLeft
    /// '<<='
    | ShiftLeftAssign
    /// '>>'
    | ShiftRight
    /// '>>='
    | ShiftRightAssign
    
    // Others operators
    /// '->'
    | Array
    /// '.'
    | Dot
    /// ','
    | Comma
    /// ':'
    | Colon
    /// ';'
    | Semicolon
    /// ':='
    | Assign
    /// '#'
    | Hash
    /// '##'
    | HashHash
    /// '...'
    | Ellipsis
    /// '`'
    | Backquote
    /// '''
    | QuotationMark
    /// '"'
    | DoubleQuotationMark
    /// '$'
    | DollarSign
    /// '//'
    | DoubleSlash
    /// '?'
    | QuestionMark
    /// '??'
    | DoubleQuestionMark
    
    // Literals
    /// string literal
    | StringLiteral
    /// Char literal
    | CharLiteral
    /// int32 literal
    | IntegerLiteral
    /// int64 literal
    | LongIntegerLiteral
    /// int16 literal
    | ShortIntegerLiteral
    /// float literal
    | RealNumberLiteral
    
    // Keywords
    /// 'знак' keyword
    | KeywordSigned
    /// 'без знак' keyword
    | KeywordUnsigned
    
    /// 'длинный' keyword
    | KeywordLong
    /// 'короткий' keyword
    | KeywordShort

    /// 'ничего' keyword
    | KeywordVoid
    /// 'данные' keyword
    | KeywordStruct
    /// 'перечисление' keyword
    | EnumStruct
    
    /// 'объединение' keyword
    | KeywordUnion
    /// 'изменчивый' keyword
    | KeywordVolatile
    
    /// 'static' keyword
    | KeywordStatic
    /// 'sizeof' keyword
    | KeywordSizeof

    /// 'если' keyword
    | KeywordIf
    /// 'то' keyword
    | KeywordThen
    /// 'иначе' keyword
    | KeywordElse
    /// 'всё' or 'все' keyword
    | KeywordBlockEnd
    /// 'перейти' keyword
    | KeywordGo
    /// 'выбор' keyword
    | KeywordSwitch
    /// 'при' keyword
    | KeywordCase
    /// 'остальное' keyword
    | KeywordDefault

    /// 'нц' keyword (begin loop)
    | KeywordBeginLoop
    /// 'кц' keyword (end loop)
    | KeywordEndLoop
    /// 'для' keyword (for loop)
    | KeywordFor
    /// 'раз' keyword (times loop)
    | KeywordTimes
    /// 'пока' keyword (while loop)
    | KeywordWhile
    /// 'от' keyword (for loop 'from')
    | KeywordFrom
    /// 'до' or 'к' keyword (for loop 'to')
    | KeywordTo
    /// 'шаг' keyword (for loop 'step')
    | KeywordStep
    /// 'выход' keyword (loop break and algorithm return)
    | KeywordBreak

    /// 'алг' keyword (algorithm header)
    | KeywordAlgorithmHeader
    /// 'нач' keyword (begin algorithm implementation)
    | KeywordBeginAlgorithmImplementation
    /// 'кон' keyword (end algorithm implementation)
    | KeywordEndAlgorithmImplementation
    /// 'дано' keyword (algorithm pre-condition)
    | KeywordPreCondition
    /// 'надо' keyword (algorithm post-condition)
    | KeywordPostCondition
    /// 'утв' keyword (assertion)
    | KeywordAssertion
    /// 'арг' keyword (algorithm argument)
    | KeywordAlgorithmArgument

    /// 'ввод' keyword (terminal input)
    | KeywordInput
    /// 'вывод' keyword (terminal output)
    | KeywordOutput
    /// 'Фввод' keyword (file input)
    | KeywordFileInput
    /// 'Фвывод' keyword (file output)
    | KeywordFileOutput
    /// 'нс' keyword (new line symbol)
    | KeywordNewLine

    /// 'не' keyword (logical 'not')
    | KeywordNot
    /// 'и' keyword (logical 'and')
    | KeywordAnd
    /// 'или' keyword (logical 'or')
    | KeywordOr

    /// 'цел' keyword (integer type name)
    | KeywordInt
    /// 'вещ' keyword (floating point type name)
    | KeywordFloat
    /// 'сим' keyword (character type name)
    | KeywordChar
    /// 'лит' keyword (string type name)
    | KeywordString
    /// 'лог' keyword (boolean type name)
    | KeywordBool
    /// 'таб' keyword
    | KeywordArray

    /// 'исп' keyword (begin module)
    | KeywordBeginModule
    /// 'использовать' keyword (use module)
    | KeywordUseModule

    /// 'да' keyword (true constant value)
    | KeywordTrue
    /// 'нет' keyword (false constant value)
    | KeywordFalse