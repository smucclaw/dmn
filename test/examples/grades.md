| U (results) | Mark (input, Int) | Test (input, Bool) | Grade (output, String) | result (output, String) |
| ----------- | ----------------- | --- | ---------------------- | ----------------------- |
| 1           | >=70              | true | "A"                    | "pass"                  |
| 2           | [60..69]          | | "B"                    | "pass"                  |
| 3           | [50..59]          | | "C"                    | "pass"                  |
| 4           | [40..49]          | | "D"                    | "fail"                  |
| 5           | [30..39]          | | "E"                    | "fail"                  |
| 6           | [20..29]          | | "F"                    | "fail"                  |

results(50, true, grade, result)