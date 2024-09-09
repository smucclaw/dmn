|U (TableA)|season (input, String)|guests (input, Int)|veg_guests (input, Bool)|Dish (output, String)|Dessert (output, String)|
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

TableA("Fall", 5, true, dish, dessert)

TableB(dish, dessert, false, beverage)