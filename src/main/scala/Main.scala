import scala.collection.mutable.ArrayBuffer

object Main extends App {
    def parse_csv(path: String): ArrayBuffer[Array[String]] = {
        val content = new ArrayBuffer[Array[String]]()
        val file = io.Source.fromFile(path)
        for (line <- file.getLines.drop(1)) {
            content += line.split(",")
        }
        file.close
        content
    }

    def question_1(): ArrayBuffer[Array[Int]] = {
        val flight_count = Array.fill(12)(0)
        for (flight <- flights) {
            flight_count(flight(4).split("-")(1).toInt - 1) += 1
        }
        val res = new ArrayBuffer[Array[Int]]()
        for (month <- 1 to 12) {
            res += Array(month, flight_count(month - 1))
        }
        res
    }

    val passengers = parse_csv("Flight Data Assignment/passengers.csv")
    val flights = parse_csv("Flight Data Assignment/flightData.csv")

    println("Question 1:")
    val res_q1 = question_1()
    println("Month, Number of Flights")
    for (month <- res_q1) {
        println(month.mkString(", "))
    }
    println()
}
