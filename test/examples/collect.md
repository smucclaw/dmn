|C (advertising)|Age (input, number)|To Advertise (output, String)|
|---|---|---|
|1|>18|"Cars"|
|2|>12|"Videogames"|
|3|-|"Toys"|

advertising(13, a)

// python translation
// def advertising(age):
//    results = []
//    if age > 18 :
//        results.append( 'Cars' )
//    if age > 12 :
//        results.append( 'Videogames' )
//    if True :
//        results.append( 'Toys' )
//    return results
//a = advertising(13)

// possible simala translation?
// #eval let
//   advertising = fun (input_advertising) => 
//      if   input_advertising.Age > 18
//      then {`To Advertise` = {1 = 'Cars}} 
//      if   input_advertising.Age > 12
//      then {`To Advertise` = {2 = 'Videogames}} 
//      if true
//      then {`To Advertise` = {3 = 'Toys}}
// in advertising({Age = 13})