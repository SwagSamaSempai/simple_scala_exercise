import scala.collection.mutable

object Main extends App {
    /** Returns all flights in a given CSV file.
     *
     * @param path CSV file path
     * @return ArrayBuffer[Array[String]]
     */
    def parse_flights(path: String) = {
        val content = new mutable.ArrayBuffer[Array[String]]()
        val file = io.Source.fromFile(path)
        for (line <- file.getLines.drop(1)) {
            content += line.split(",")
        }
        file.close
        content
    }

    /** Returns all passengers in a given CSV file.
     *
     * @param path CSV file path
     * @return Map[Int, Array[String]]
     */
    def parse_passengers(path: String) = {
        val content = mutable.Map[Int, Array[String]]()
        val file = io.Source.fromFile(path)
        for (line <- file.getLines.drop(1)) {
            val split_line = line.split(",")
            content += (split_line(0).toInt -> Array(split_line(1), split_line(2)))
        }
        file.close
        content
    }

    /** Returns the total number of flights for each month.
     * For each month, the format is:
     * Month, Number of Flights
     *
     * @param flights The flights' records
     * @return Array[Array[Int]]
     */
    def question_1(flights: mutable.ArrayBuffer[Array[String]]) = {
        val res = Array.ofDim[Int](12, 2)
        val flight_count = Array.fill(12)(0) // One row per month
        for (flight <- flights) {
            flight_count(flight(4).split("-")(1).toInt - 1) += 1 // Get only the month from the date
        }
        for (month <- 1 to 12) {
            res(month - 1) = Array(month, flight_count(month - 1))
        }
        res
    }

    /** Returns the names of the 100 most frequent flyers.
     * For each flyer, the format is:
     * Passenger ID, Number of Flights, First name, Last name
     *
     * @param flights    The flights' records
     * @param passengers The passenger's information
     * @return Array[Array[String]]
     */
    def question_2(flights: mutable.ArrayBuffer[Array[String]], passengers: mutable.Map[Int, Array[String]]) = {
        val res = Array.ofDim[String](100, 4)
        val passenger_count = Array.fill(passengers.size)(0)
        for (flight <- flights) {
            passenger_count(flight(0).toInt - 1) += 1 // Increment for each time a passenger has been on a flight
        }
        // First zipWithIndex to keep original indices (thus IDs), then sort by count, then take top 100
        val sorted_passenger_count = passenger_count.zipWithIndex.sortWith(_._1 > _._1).take(100)
        // zipWithIndex again, so as not to increment a counter manually
        for (((count, index), counter) <- sorted_passenger_count.zipWithIndex) {
            val id = index.toInt + 1 // IDs start from 1, indices from 0
            res(counter) = Array(id.toString, count.toString, passengers(id)(0), passengers(id)(1))
        }
        res
    }

    /** Returns the greatest number of countries a passenger has been without being in the UK.
     * For each passenger, the format is:
     * Passenger ID, Longest Run
     *
     * This method assumes the IDs of passengers start from 1, and are consecutive.
     *
     * @param flights        The flight's records
     * @param nbr_passengers The total number of passengers
     * @return Array[Array[Int]]
     */
    def question_3(flights: mutable.ArrayBuffer[Array[String]], nbr_passengers: Int) = {
        val res = Array.ofDim[Int](nbr_passengers, 2)
        // Sort the flights by passenger ID
        val sorted_flights = flights.sortWith(_ (0).toInt < _ (0).toInt)
        // Create a separate Vector to hold ID for indexing
        var ids = Vector[Int]()
        for (flight <- sorted_flights) {
            ids = ids :+ flight(0).toInt
        }
        // current_index is the first index of the current passenger, next_index is the first index of the next one
        var current_index = 0
        var next_index = 0
        for (id <- 1 to nbr_passengers) {
            next_index = ids.indexOf(id + 1) // Look up the first index of the next passenger
            if (next_index == -1) { // For the last passenger
                next_index = flights.size
            }
            val pass_flights = sorted_flights.slice(current_index, next_index) // Take only for current passenger
            current_index = next_index
            var flat_list = Vector[String]()
            // Instead of e.g. (ca,uk)(uk,fr), take [ca, uk, fr]
            for (flight <- pass_flights) {
                if (flight(2) != flight(3)) {
                    flat_list = flat_list :+ flight(2)
                }
            }
            flat_list = flat_list :+ pass_flights.last(3)

            // Find out when the passenger has been to the UK
            var uk_indices = flat_list.zipWithIndex.filter(_._1 == "uk").map(_._2)
            if (uk_indices.isEmpty) { // The passenger hasn't been to the UK, take the number of countries
                res(id - 1) = Array(id, flat_list.size)
            } else { // The passenger has been to the UK, find out the longest travel chain
                uk_indices = 0 +: uk_indices :+ uk_indices.size // To calculate first and last differences
                var differences = Vector[Int]()
                // Zip with tail of list to calculate differences between consecutive numbers
                for (pair <- uk_indices.zip(uk_indices.tail)) {
                    differences = differences :+ pair._1 + pair._2
                }
                res(id - 1) = Array(id, if (differences.max > 1) differences.max - 1 else 0)
            }
        }
        res
    }

    /** Runs all questions while printing their result, and times the whole process. */
    def run(): Unit = {
        val start = System.nanoTime()
        val flights = parse_flights("Flight Data Assignment/flightData.csv")
        val passengers = parse_passengers("Flight Data Assignment/passengers.csv")

        println("Question 1:")
        val res_q1 = question_1(flights)
        println("Month, Number of Flights")
        for (month <- res_q1) {
            println(month.mkString(", "))
        }
        println()

        println("Question 2:")
        val res_q2 = question_2(flights, passengers)
        println("Passenger ID, Number of Flights, First name, Last name")
        for (passenger <- res_q2) {
            println(passenger.mkString(", "))
        }
        println()

        println("Question 3:")
        val res_q3 = question_3(flights, passengers.size)
        println("Passenger ID, Longest Run")
        for (passenger <- res_q3) {
            println(passenger.mkString(", "))
        }
        println()

        val end = System.nanoTime()
        println("Elapsed time: " + (end - start) / 1000000 + "ms")
    }

    run()
}
