module Sharpify.Tests.EcmaScript


open Sharpify
open Sharpify.Syntax
open Xunit
open FParsec

let test str result =    
  match run ecmaScript str with
  | Success(result, _, _)   -> 
    printf "Success: %A" result
    Assert.True true
  | Failure(errorMsg, _, _) -> 
    printf "Failure: %s" errorMsg
    Assert.True false   

let number n = 
    n
    |> Numeric
    |> Literal
    |> PrimaryExpression 

let bool n = 
    n
    |> Boolean
    |> Literal
    |> PrimaryExpression 

[<Fact>]
let ``ecmaScript can parse literal expressions``() =         
    test @"true" (Boolean true)
    test @"false" (Boolean false)
    test @"null" Null
    test @"""Hello""" (String "Hello")
    test @"2.8" (Numeric 2.8)


[<Fact>]
let ``ecmaScript can parse primary expressions``() =         
    test @"this" This
    test @"false" (Literal (Boolean false))
    test @"null" (Literal Null)
    test @"""Hello""" (Literal (String "Hello"))
    test @"2.8" (Literal (Numeric 2.8))


    
[<Fact>]
let ``ecmaScript can parse bitwise expressions``() =    
  let six = number 6.0
  let two = number 2.0

  test @"6 ^ 2"
  <| BitwiseXorExpression (six, two)          
          
  test @"6 ^2"
  <| BitwiseXorExpression (six, two)  
         
  test @"6 | 2"
  <| BitwiseOrExpression (six, two)   
          
  test @"6 & 2"
  <| BitwiseAndExpression (six, two)  

  test @"6 &2"
  <| BitwiseAndExpression (six, two)  

  test @"6& 2"
  <| BitwiseAndExpression (six, two)  



[<Fact>]
let ``ecmaScript can parse logical expressions``() =  
  let six = number 6.0
  let two = number 2.0
  
  test @"6||2"
  <| OrExpression (six, two)          
          
  test @"6 && 2"
  <| AndExpression (six, two)  

[<Fact>]
let ``ecmaScript can parse array expressions``() =         
    let t = bool true
    let f = bool false
    let one = number 1.0

    test @"[]" 
    <| Array []

    test @"[true, false]" 
    <| Array [t;f]

    test @"[true, false]" 
    <| Array [t;f]

    test @"[true,false,1, 1.0, false ,, false, false,,,,1]" 
    <| Array [t;f;one;one;f;f;f;one]

