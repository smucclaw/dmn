|U (PassFail) |Mark (input, number) |Grade (output, String)|
|---|---|---|
|1|>=50|"Pass"|
|2|<50|"Fail"|

|U (Attendance)|Attended (input, bool)|Attendance Pass (output, bool)|
|---|---|---|
|1|true|true|
|2|false|false|

|F (Final)|Grade (input, string)|Attendance Pass (input, bool)|Overall Result (output, bool)|
|---|---|---|---|
|1|-|false|false|
|2|"Fail"|-|false|
|3|"Pass"|true|true|

PassFail(60, grade)
Attendance(true, a)
Final(grade, a, result)