|U|Season (input, string)|guest count (input, int)|vegetarian guests (input, bool)|Dish (output, string)|
|---|---|---|
|1|"Fall"|<=8|false|"Spareribs"|
|2|"Winter"|<=8|false|"Roastbeef"|
|3|"Spring"|<=4|false|"Fancy Steak"|
|4|"Spring"|[5..8]|false|"Stew"|
|5|"Summer"|<=8|false|"Light salad and a nice steak"|
|6||>8|false|"Stew"|
|7|||true|"pasta"|

|F|Dish (input, string)|Children (input, bool)|Beverages (output, string)|
|1||true|"apple juice"|
|2|"Spareribs"|-|"beer"|
|3|"Stew"|-|"wine"|
|4|"Roastbeef"|-|"soda"|
|5|"Steak"|-|"lemonade"|