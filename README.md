# Quantexa Exercise

## Dependencies

- Scala 2.12.12
- Scalatest 3.0.8
- Spark 3.0.1 (Core and SQL)

## Run

The argument are, in order:

- Path to flights CSV
- Path to passengers CSV
- (Questions 4 and 5) Minimum number of flights together
- (Questions 4 and 5) Starting date
- (Questions 4 and 5) Ending date

### SBT

To run the code, simply use the `run` command in sbt. This will run all 4 Questions and print their result in the
console.

The arguments all have default values, but if you want to customize them, use this command instead:

```
run "Flight Data Assignment/flightData.csv" "Flight Data Assignment/passengers.csv" "5" "2017-05-01" "2017-08-01"
```

### Spark

First use `sbt clean` and `sbt package` to create the `.jar` file. Then run the following command:

```
%SPARK_HOME%/bin/spark-submit --class "Main" --master local[8] target/scala-2.12/quantexa-exercise_2.12-1.0.jar
```

## Test

To test the code, simply use the `test` command in sbt.

## Notes

- The main run only shows the first 20 results for questions 3 and 4, as it would overload the console otherwise. To
  print all results instead, remove `.take(20)` on lines 28 and 35 in `Main.scala`.
- In Question 3, the example use case of UK -> FR -> US -> CN -> UK -> DE -> UK seems obvious, but what I wasn't sure
  about was if for example the chain was UK -> FR -> US -> FR -> UK -> DE -> UK or even UK -> FR -> FR -> FR -> UK -> DE
  -> UK. The question says 'greatest number of countries' and not 'greatest number of distinct countries', so in doubt I
  figured it could be interesting to know how many times a passenger has _changed_ countries without going to the UK. In
  that case, the results for the three use cases mentioned above would be:
    - UK -> FR -> US -> CN -> UK -> DE -> UK: 3
    - UK -> FR -> US -> FR -> UK -> DE -> UK: 3
    - UK -> FR -> FR -> FR -> UK -> DE -> UK: 1

  The other interpretations are not difficult to implement from mine.
- Questions 4 and 5 are merged inside one method, as Question 4 is a specific case of Question 5 where `N = 3`,
  `from = "2017-01-01"`, and `to = "2017-12-31`.
- Question 4 takes only ~20 seconds to run with `Scala 2.13.4` compared to ~35 seconds with `Scala 2.12.12`, but
  unfortunately Spark requires at most `Scala 2.12.X`. Also, it takes a much longer time to run after the other 3
  questions, which is most likely a garbage collector issue.
