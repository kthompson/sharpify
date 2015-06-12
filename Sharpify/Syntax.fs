[<AutoOpen>]
module Sharpify.Syntax

type Json = JString of string
          | JNumber of float
          | JBool   of bool
          | JNull
          | JList   of Json list
          | JObject of Map<string, Json>


//type Expression = Object

//type EcmaScript = Expression
//                | WhileStatement of Expression * Expression

type Literal =
    Null
  | Boolean of bool
  | Numeric of float
  | String of string
  | Regex of string

type PrimaryExpression =
    This
  | Literal of Literal