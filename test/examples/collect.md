|C (advertising)|Age (input, Int)|To Advertise (output, String)|
|---|---|---|
|1|>18|"Cars"|
|2|>12|"Videogames"|
|3|-|"Toys"|

advertising(13, a)

// #eval let
//   advertising = fun (input_advertising) => 
//      if   input_advertising.Age > 18
//      then {`To Advertise` = {1 = 'Cars}} 
//      else
//        if   input_advertising.Age > 12
//        then {`To Advertise` = {2 = 'Videogames}} 
//        else
//          {`To Advertise` = {3 = 'Toys}}
// in advertising({Age = 13})