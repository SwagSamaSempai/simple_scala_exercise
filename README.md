# Quantexa Exercise

## Dependencies

- Scalatest 3.0.8

## Run

To run the code, simply use the `run` command in sbt.

If you want to customize the arguments, use this command instead:

```
run "Flight Data Assignment/flightData.csv" "Flight Data Assignment/passengers.csv" "5" "2017-05-01" "2017-08-01"
```

The argument are, in order:

- Path to flights CSV
- Path to passengers CSV
- (Questions 4 and 5) Minimum number of flights together
- (Questions 4 and 5) Starting date
- (Questions 4 and 5) Ending date
