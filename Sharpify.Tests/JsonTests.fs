module Sharpify.Tests.JsonTests

open Sharpify
open Xunit
open FParsec

let test str result =    
  match run json str with
  | Success(result, _, _)   -> 
    printf "Success: %A" result
    Assert.True true
  | Failure(errorMsg, _, _) -> 
    printf "Failure: %s" errorMsg
    Assert.True false   

[<Fact>]
let ``Parser Can Parse variable declaration with Number``() =         
    test @"var x = 5"

[<Fact>]
let ``Can parse JSON Object``() = 
  let json = 
    """{
        "glossary": {
            "title": "example glossary"
        },
        "value" : 2
    }"""
  test json (JObject (Map.empty
               .Add("glossary", JObject (Map.empty.Add("title", JString "example glossary")))
               .Add("value", JNumber 2.0)))
        
                    

[<Fact>]
let ``Can parse JSON string``() = 
  test @"""hello world""" (JString "hello world")

[<Fact>]
let ``Can parse JSONNullLiteral``() = 
  test @"null" JNull

[<Fact>]
let ``Can parse JSONBooleanLiteral``() = 
  test @"true" (JBool true)
  test @"false" (JBool false)

[<Fact>]
let ``Can parse JSONNumber``() = 
  test @"2.5" (JNumber 2.5)
  test @"8" (JNumber 8.0)
  test @"-8.2" (JNumber -8.2)
  test @"-25" (JNumber -25.0)
  test @"9e-10" (JNumber 9e-10)
  test @"8e+45" (JNumber 8e45)
  test @"8E-5" (JNumber 8e-5)
  test @"8E+5" (JNumber 8e5)

[<Fact>]
let ``Can parse JSONArray``() = 
  test @"[1,2,3,4]" (JList [JNumber 1.0; JNumber 2.0; JNumber 3.0; JNumber 4.0])
  test @"[""1"",""2"",""3"",""4""]" (JList [JString "1"; JString "2"; JString "3"; JString "4"])
  test @"[1, 2,3, 4 ]" (JList [JNumber 1.0; JNumber 2.0; JNumber 3.0; JNumber 4.0])

