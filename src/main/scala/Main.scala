import scala.collection.immutable.ListMap
import scala.collection.mutable

object Main extends App {
    val t0 = System.nanoTime()

    def parse_flights(path: String): mutable.ArrayBuffer[Array[String]] = {
        val content = new mutable.ArrayBuffer[Array[String]]()
        val file = io.Source.fromFile(path)
        for (line <- file.getLines.drop(1)) {
            content += line.split(",")
        }
        file.close
        content
    }

    def parse_passengers(path: String): mutable.Map[Int, Array[String]] = {
        val content: mutable.Map[Int, Array[String]] = mutable.Map()
        val file = io.Source.fromFile(path)
        for (line <- file.getLines.drop(1)) {
            val split_line = line.split(",")
            content += (split_line(0).toInt -> Array(split_line(1), split_line(2)))
        }
        file.close
        content
    }

    def question_1(): mutable.ArrayBuffer[Array[Int]] = {
        val flight_count = Array.fill(12)(0)
        for (flight <- flights) {
            flight_count(flight(4).split("-")(1).toInt - 1) += 1
        }
        val res = new mutable.ArrayBuffer[Array[Int]]()
        for (month <- 1 to 12) {
            res += Array(month, flight_count(month - 1))
        }
        res
    }

    def question_2(): mutable.ArrayBuffer[Array[String]] = {
        val res = new mutable.ArrayBuffer[Array[String]]()
        val passenger_count = Array.fill(passengers.size)(0)
        for (flight <- flights) {
            passenger_count(flight(0).toInt - 1) += 1
        }
        for ((count, index) <- passenger_count.zipWithIndex.sortWith(_._1 > _._1).take(100)) {
            val id: Int = index.toInt + 1
            res += Array(id.toString, count.toString, passengers(id)(0), passengers(id)(1))
        }
        res
    }

    val flights = parse_flights("Flight Data Assignment/flightData.csv")
    val passengers = parse_passengers("Flight Data Assignment/passengers.csv")

    println("Question 1:")
    val res_q1 = question_1()
    println("Month, Number of Flights")
    for (month <- res_q1) {
        println(month.mkString(", "))
    }
    println()

    println("Question 2:")
    val res_q2 = question_2()
    println("Passenger ID, Number of Flights, First name, Last name")
    for (passenger <- res_q2) {
        println(passenger.mkString(", "))
    }
    println()

    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
}
