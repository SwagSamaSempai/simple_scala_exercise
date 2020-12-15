import org.apache.spark.sql.SparkSession

object Main {
    def main(args: Array[String]): Unit = {

        val spark = SparkSession.builder().master("local[16]").appName("Main").getOrCreate()

        val questions = new Questions(if (args.length >= 1) args(0) else "Flight Data Assignment/flightData.csv",
                                      if (args.length >= 2) args(1) else "Flight Data Assignment/passengers.csv",
                                      if (args.length >= 3) args(2).toInt else 3,
                                      if (args.length >= 4) args(3) else "2017-01-01",
                                      if (args.length >= 5) args(4) else "2017-31-12"
                                      )

        val start = System.nanoTime()
        println("Question 1:")
        println("Month, Number of Flights")
        questions.question_1().show()
        println()

        println("Question 4 and 5:")
        println("Passenger 1 ID, Passenger 2 ID, Number of flights together, From, To")
        for (pair <- questions.question_4_and_5().take(20)) {
            println(pair.mkString(", "))
        }
        println()

        spark.close()
        val end = System.nanoTime()
        println("Elapsed time: " + (end - start) / 1000000 + "ms")
    }
}
