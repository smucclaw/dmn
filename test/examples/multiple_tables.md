| U (Table A) | season (input, string) | guests (input, int) | veg guests (input, bool) | Dish (output, string)          |
| ----------- | ---------------------- | ------------------- | ------------------------ | ------------------------------ |
| 1           | "Fall"                 | <=8                 | false                    | "Spareribs"                    |
| 2           | "Winter"               | <=8                 | false                    | "Roastbeef"                    |
| 3           | "Spring"               | <=4                 | false                    | "Fancy Steak"                  |
| 4           | "Spring"               | [5..8]              | false                    | "Stew"                         |
| 5           | "Summer"               | <=8                 | false                    | "Light salad and a nice steak" |
| 6           |                        | >8                  | false                    | "Stew"                         |
| 7           |                        |                     | true                     | "Pasta"                        |

| F   | Dish (input, string) | Children (input, bool) | Beverages (output, string) |
| 1   | -------------------- | ---------------------- | -------------------------- |
| 2   | "Spareribs"          | -                      | "beer"                     |
| 3   | "Stew"               | -                      | "wine"                     |
| 4   | "Roastbeef"          | -                      | "soda"                     |
| 5   | "Steak"              | -                      | "lemonade"                 |
| 6   |                      | true                   | "apple juice"              |

A.Out1 --> B.In2
A("Fall", 5, true, d)
B(d, false, b)
C("foo", 5, s, i)

d = Ap("Fall", 5, true) 
Table C (in string, in int, out string, out int)
(s, i) = C("foo", 5)