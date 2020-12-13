import scala.collection.mutable.{ArrayBuffer, Map}

object Main extends App {
    val start = System.nanoTime()

    def parse_flights(path: String): ArrayBuffer[Array[String]] = {
        val content = new ArrayBuffer[Array[String]]()
        val file = io.Source.fromFile(path)
        for (line <- file.getLines.drop(1)) {
            content += line.split(",")
        }
        file.close
        content
    }

    def parse_passengers(path: String): Map[Int, Array[String]] = {
        val content: Map[Int, Array[String]] = Map()
        val file = io.Source.fromFile(path)
        for (line <- file.getLines.drop(1)) {
            val split_line = line.split(",")
            content += (split_line(0).toInt -> Array(split_line(1), split_line(2)))
        }
        file.close
        content
    }

    def question_1(): Array[Array[Int]] = {
        val res = Array.ofDim[Int](12, 2)
        val flight_count = Array.fill(12)(0)
        for (flight <- flights) {
            flight_count(flight(4).split("-")(1).toInt - 1) += 1
        }
        for (month <- 1 to 12) {
            res(month - 1) = Array(month, flight_count(month - 1))
        }
        res
    }

    def question_2(): Array[Array[String]] = {
        val res = Array.ofDim[String](100, 4)
        val passenger_count = Array.fill(passengers.size)(0)
        for (flight <- flights) {
            passenger_count(flight(0).toInt - 1) += 1
        }
        val sorted_passenger_count = passenger_count.zipWithIndex.sortWith(_._1 > _._1).take(100)
        for (((count, index), counter) <- sorted_passenger_count.zipWithIndex) {
            val id = index.toInt + 1
            res(counter) = Array(id.toString, count.toString, passengers(id)(0), passengers(id)(1))
        }
        res
    }

    def question_3(): Array[Array[Int]] = {
        val res = Array.ofDim[Int](passengers.size, 2)
        val sorted_flights = flights.sortWith(_ (0).toInt < _ (0).toInt)
        var ids = Vector[Int]()
        for (flight <- sorted_flights) {
            ids = ids :+ flight(0).toInt
        }
        var current_index = 0
        var next_index = 0
        for (id <- Range(1, passengers.size + 1, 1)) {
            next_index = ids.indexOf(id + 1)
            if (next_index == -1) {
                next_index = flights.size
            }
            val pass_flights = sorted_flights.slice(current_index, next_index)
            current_index = next_index
            var flat_list = Vector[String]()
            for (flight <- pass_flights) {
                if (flight(2) != flight(3)) {
                    flat_list = flat_list :+ flight(2)
                }
            }
            flat_list = flat_list :+ pass_flights.last(3)
            var uk_indices = flat_list.zipWithIndex.filter(_._1 == "uk").map(_._2)
            if (uk_indices.size == 0) {
                res(id - 1) = Array(id, flat_list.size)
            } else {
                uk_indices = 0 +: uk_indices :+ uk_indices.size
                var differences = Vector[Int]()
                for (pair <- uk_indices.zip(uk_indices.tail)) {
                    differences = differences :+ pair._1 + pair._2
                }
                res(id - 1) = Array(id, if (differences.max > 1) differences.max - 1 else 0)
            }
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

    println("Question 3:")
    val res_q3 = question_3()
    println("Passenger ID, Longest Run")
    for (passenger <- res_q3) {
        println(passenger.mkString(", "))
    }
    println()

    var end = System.nanoTime()
    println("Elapsed time: " + (end - start) / 1000000 + "ms")
}
