| U (Table A) | season (input, string) | guests (input, int) | veg guests (input, bool) | Dish (output, string)          |
| ----------- | ---------------------- | ------------------- | ------------------------ | ------------------------------ |
| 1           | "Fall"                 | <=8                 | false                    | "Spareribs"                    |
| 2           | "Winter"               | <=8                 | false                    | "Roastbeef"                    |
| 3           | "Spring"               | <=4                 | false                    | "Fancy Steak"                  |
| 4           | "Spring"               | [5..8]              | false                    | "Stew"                         |
| 5           | "Summer"               | <=8                 | false                    | "Light salad and a nice steak" |
| 6           |                        | >8                  | false                    | "Stew"                         |
| 7           |                        |                     | true                     | "Pasta"                        |

| F (Table B) | Dish (input, string) | Children (input, bool) | Beverages (output, string) |
|-------------|----------------------|------------------------|----------------------------|
| 1           | "Spareribs"          | -                      | "beer"                     |
| 2           | "Stew"               | -                      | "wine"                     |
| 3           | "Roastbeef"          | -                      | "soda"                     |
| 4           | "Steak"              | -                      | "lemonade"                 |
| 5           |                      | true                   | "apple juice"              |

A("Fall", 5, true, d)

B(d, false, b)