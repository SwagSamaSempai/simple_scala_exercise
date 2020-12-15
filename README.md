# Quantexa Exercise

## Dependencies

- AdoptOpenJDK 1.8.0_275 (or any other JDK)
- sbt 1.4.5
- Scala 2.12.12
- Scalatest 3.0.8
- Spark 3.0.1 (Core and SQL)

## Run

Running is not recommended, as the code isn't really working for Question 4, and Questions 2 and 3 are not implemented.
However, these methods technically work:

### SBT

To run the code, simply use the `run` command in sbt. This will run all 4 Questions and print their result in the
console.

The arguments all have default values.

### Spark

First use `sbt clean` and `sbt package` to create the `.jar` file. Then run the following command:

```
%SPARK_HOME%/bin/spark-submit --class "Main" --master local target/scala-2.12/quantexa-exercise_2.12-1.0.jar
```
