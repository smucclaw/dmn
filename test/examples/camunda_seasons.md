|U (TableA) |season (input, string)|guests (input, int)|vegetarian_guests (input, bool)|dish (output, string)|
|---|---|---|---|---|
|1|"Fall"|<=8|false|"Spareribs"|
|2|"Winter"|<=8|false|"Roastbeef"|
|3|"Spring"|<=4|false|"Fancy Steak"|
|4|"Spring"|[5..8]|false|"Steak"|
|5|"Summer"|<=8|false|"Light salad and a nice steak"|
|6||>8|false|"Stew"|
|7|||true|"pasta"|

TableA("Spring", 7, false, dish)