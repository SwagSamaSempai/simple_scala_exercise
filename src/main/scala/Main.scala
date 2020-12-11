import scala.collection.immutable.ListMap
import scala.collection.mutable

object Main extends App {
    val t0 = System.nanoTime()

    def parse_csv(path: String): mutable.ArrayBuffer[Array[String]] = {
        val content = new mutable.ArrayBuffer[Array[String]]()
        val file = io.Source.fromFile(path)
        for (line <- file.getLines.drop(1)) {
            content += line.split(",")
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
        val passenger_count: mutable.Map[String, Int] = mutable.Map()
        for (flight <- flights) {
            if (passenger_count.contains(flight(0))) {
                passenger_count(flight(0)) += 1
            } else {
                passenger_count += (flight(0) -> 1)
            }
        }
        val top100 = ListMap(passenger_count.toSeq.sortWith(_._2 > _._2): _*).take(100)
        val sorted_passengers = passengers.sortWith((x, y) => x(0).toInt < y(0).toInt)
        top100.foreach {
            case (id, count) => res += Array(id, count.toString, sorted_passengers(id.toInt - 1)(1), sorted_passengers(id.toInt - 1)(2))
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
