[<AutoOpen>]
module Sharpify.ECMAScript

open FParsec
open Sharpify.Syntax

let ecmaScript : Parser<PrimaryExpression, unit>= 
  let ws = spaces
  let str = pstring 


  
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
    <|> BooleanLiteral
    <|> NumericLiteral
    <|> StringLiteral
    //TODO: <|> RegularExpressionLiteral
    |>> fun x -> Literal x

//  let IdentifierName =
//    IdentifierStart
//    <|> IdentifierName IdentifierPart
//
//  let ReservedWord = 
//  let Identifier =
//    //IdentifierName but not ReservedWord
//    let firstLetter c = c = '$' || c = '_' || 


  //  Ecma-262 - 11.1
  let  PrimaryExpression =
    stringReturn "this" This
  //    Identifier
    <|>    Literal
  //    ArrayLiteral
  //    ObjectLiteral
  //    ( Expression )
  //  Ecma-262 - 11.1.4
  //  ArrayLiteral :
  //    [ Elision-opt ]
  //    [ ElementList ]
  //    [ ElementList , Elision-opt ]
  //  ElementList :
  //    Elisionopt AssignmentExpression
  //    ElementList , Elisionopt AssignmentExpression
  //  Elision :
  //    ,
  //    Elision ,

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
    PrimaryExpression .>> eof

  program