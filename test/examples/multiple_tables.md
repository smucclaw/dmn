|U (A)|season (input, String)|guests (input, Int)|veg guests (input, Bool)|Dish (output, String)|
|---|---|---|---|---|
|1|"Fall"|<=8|false| "Spareribs"|
|2|"Winter"|<=8|false|"Roastbeef"|
|3|"Spring"|<=4|false|"Fancy Steak"|
|4|"Spring"|[5..8]|false|"Stew"|
|5|"Summer"|<=8|false|"Light salad and a nice steak"|
|6|-|>8|false|"Stew"|
|7|||true|"Pasta"|

|F (B)| Dish (input, String) | Children (input, Bool) | Beverages (output, String) |
|-------------|----------------------|------------------------|----------------------------|
|1|"Spareribs"|false|"beer"|
|2|"Stew"|false|"wine"|
|3|"Roastbeef"|false|"soda"|
|4|"Steak"||"lemonade"|
|5||true|"apple juice"|

A("Fall", 5, true, d)

B(d, false, b)