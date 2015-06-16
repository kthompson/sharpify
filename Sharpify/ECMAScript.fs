[<AutoOpen>]
module Sharpify.ECMAScript

open FParsec
open Sharpify.Syntax

let ecmaScript : Parser<Expression, unit>= 
  let ws = spaces
  let str = pstring 


  let BP (p: Parser<_,_>) stream =
    p stream // set a breakpoint here

  //  Ecma-262 - 7.8.1
  let NullLiteral =
    stringReturn "null" Null
    
  //  Ecma-262 - 7.8.2
  let BooleanLiteral =    
    stringReturn "true" (Boolean true)
    <|> stringReturn "false" (Boolean false)


  //  Ecma-262 - 7.8.3
  let NumericLiteral =
    //DecimalLiteral
    //<|> HexIntegerLiteral
    let format =     NumberLiteralOptions.AllowMinusSign
                     ||| NumberLiteralOptions.AllowFraction
                     ||| NumberLiteralOptions.AllowExponent
                     ||| NumberLiteralOptions.AllowHexadecimal

    numberLiteral format "number"
      |>> fun nl -> Numeric (float(nl.String))
  
  //  Ecma-262 - 7.8.4
  let StringLiteral =
    let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c // every other char is mapped to itself

    let unicodeEscape =
        // converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9

        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (str "\"") (str "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet)
      |>> fun x -> String x

  //  Ecma-262 - 7.8
  let Literal = 
    NullLiteral
    <|> (BooleanLiteral <?> "boolean")
    <|> NumericLiteral
    <|> (StringLiteral <?> "string")
    //TODO: <|> RegularExpressionLiteral
    |>> Literal

  let binaryExpression sep childExpr f = 
    let expr', expr'Ref = createParserForwardedToRef<Expression, unit>()

    let expr = (childExpr .>> ws) .>>. opt expr' |>> fun (a, b) ->
          match b with 
          | Some e -> f (a,  e)
          | None -> a

    do expr'Ref := sep >>. ws >>. expr .>>. opt expr' |>> fun (a, b) ->
          match b with 
          | Some e -> f (a,  e)
          | None -> a

    expr
//  let IdentifierName =
//    IdentifierStart
//    <|> IdentifierName IdentifierPart
//
//  let ReservedWord = 
//  let Identifier =
//    //IdentifierName but not ReservedWord
//    let firstLetter c = c = '$' || c = '_' || 
  
  let PrimaryExp, PrimaryExpRef = createParserForwardedToRef<Expression, unit>()

  //  Ecma-262 - 11.11  
  let BitwiseANDExpr = binaryExpression (str "&") PrimaryExp BitwiseAndExpression
  let BitwiseXORExpr = binaryExpression (str "^") BitwiseANDExpr BitwiseXorExpression
  let BitwiseORExpr = binaryExpression (str "|") BitwiseXORExpr BitwiseOrExpression
  let LogicalANDExpr = binaryExpression (str "&&") BitwiseORExpr AndExpression
  let LogicalORExpr = binaryExpression (str "||") LogicalANDExpr OrExpression

  let ConditionalExpression = 
    LogicalORExpr
    //<|> LogicalORExpression ? AssignmentExpression : AssignmentExpression


  //  Ecma-262 - 11.13
  let AssignmentExpr =
      ConditionalExpression
  //    LeftHandSideExpression = AssignmentExpression
  //    LeftHandSideExpression AssignmentOperator AssignmentExpression

  //  Ecma-262 - 11.14
  let Expr = sepBy (AssignmentExpr .>> ws) (str "," >>. ws) |>> Expressions
      


  //  Ecma-262 - 11.1.4
  let  ArrayLiteral =
    let Elision = skipMany (str ",") >>. ws
    let subExpr = Elision >>. AssignmentExpr
    let ElementList = 
      ws >>. sepBy (subExpr .>> ws) ((str "," >>. ws)) 
      |>> Array

    str "[" >>. ElementList .>> str "]"

    //  Ecma-262 - 11.1
  do PrimaryExpRef :=
    stringReturn "this" This
  //    Identifier
    <|>    Literal
    <|>    ArrayLiteral
  //    ObjectLiteral
  //    ( Expression )
    |>> PrimaryExpression
  //  Ecma-262 - 11.1.5
  //  ObjectLiteral :
  //    { }
  //    { PropertyNameAndValueList }
  //    { PropertyNameAndValueList , }

  //  PropertyNameAndValueList :
  //    PropertyAssignment
  //    PropertyNameAndValueList , PropertyAssignment

  //  PropertyAssignment :
  //    PropertyName : AssignmentExpression
  //    get PropertyName ( ) { FunctionBody }
  //    set PropertyName ( PropertySetParameterList ) { FunctionBody }

  //  PropertyName :
  //    IdentifierName
  //    StringLiteral
  //    NumericLiteral

  //  PropertySetParameterList :
  //    Identifier


  //  Ecma-262 - 11.2
  
  //  Ecma-262 - 11.2.1
  //  MemberExpression
  //    PrimaryExpression
  //    FunctionExpression
  //    MemberExpression [ Expression ]
  //    MemberExpression . IdentifierName
  //    new MemberExpression Arguments
  
  //  Ecma-262 - 11.2.2
  //  NewExpression :
  //    MemberExpression
  //    new NewExpression

  //  Ecma-262 - 11.2.3
  //  CallExpression :
  //    MemberExpression Arguments
  //    CallExpression Arguments
  //    CallExpression [ Expression ]
  //    CallExpression . IdentifierName

  //  Ecma-262 - 11.2.4
  //  Arguments :
  //    ( )
  //    ( ArgumentList )

  //  ArgumentList :
  //    AssignmentExpression
  //    ArgumentList , AssignmentExpression

  //  LeftHandSideExpression :
  //    NewExpression
  //    CallExpression

  let program = 
    Expr .>> eof

  program