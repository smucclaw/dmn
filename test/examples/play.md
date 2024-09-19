|F (Play)|outlook (input, string)| temp(input, int) | humidity (Input, int) |windy (input, bool)| golf (output, bool)| swimming (output, int)|
|---|---|---|---|---|---|---|
|1|"sunny"|>80|>85|-|false|1|
|2|"overcast"||||true|1|
|3|"rain"|-|-|true|false|0|
|4|"rain"|-|-|false|true|0|
|5|"sunny"|[71..80)|(70..95]||false|1|
|6|"sunny"|<71|<=70|false|true|1|

|U (which)|golf (input, bool)|swimming (input, int)|choice (output, string)|
|---|---|---|---|
|1|-|1|"swimming"|
|2|true|0|"golf"|
|3|false|0|"stay at home"|

|F (wear sunglasses)|outlook (input, string)| sport(input, string)| sunglasses (output, bool)|
|---|---|---|---|
|1|"sunny"|"golf"|true|
|2|-||false|

Play("sunny", 75, 90, true, golf, swim)
which(golf, swim, choice)

wear sunglasses("sunny", choice, sunglasses)

//test comments