|U (TableA)|season (input, String)|guests (input, Int)|veg guests (input, Bool)|Dish (output, String)|Dessert (output, String)|
|---|---|---|---|---|---|
|1|"Fall"|<=8|false| "Spareribs"|"Cake"|
|2|"Winter"|<=8|false|"Roastbeef"|"Brownie"|
|3|"Spring"|<=4|false|"Fancy Steak"|"Pie"|
|4||[5..8]|false|"Stew"|"Pie"|
|5|"Summer"|<=8|false|"Light salad and a nice steak"|"Ice cream"|
|6|-|>8|false|"Stew"|"Cake"|
|7|||true|"Pasta"|"Cake"|

|F (TableB)| Dish (input, String) | Dessert (input, String)| Children (input, Bool) | Beverages (output, String) |
|-------------|----------------------|------------------------|----------------------------|---|
|1|"Spareribs"|"Cake"|false|"beer"|
|2|"Stew"||false|"wine"|
|3|"Roastbeef"||false|"soda"|
|4|"Steak"|||"lemonade"|
|5|||true|"apple juice"|

|U (TableC)| Beverages (input, String) | Alcoholic (output, Bool) |
|---|---|---|
|1|"beer"|true|
|2|"wine"|true|
|3|"soda"|false|
|4|"lemonade"|false|
|5|"apple juice"|false|

TableA("Fall", 5, true, dish, dessert)

TableB(dish, dessert, false, beverage)

TableC(beverage, alc)