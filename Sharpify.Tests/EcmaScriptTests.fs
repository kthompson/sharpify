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