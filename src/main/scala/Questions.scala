import scala.collection.immutable.Queue
import scala.collection.mutable

class Questions(flights_path: String = "Flight Data Assignment/flightData.csv",
                passengers_path: String = "Flight Data Assignment/passengers.csv",
                N: Int = 3, from: String = "2017-01-01", to: String = "2017-12-31") {

    var flights: Seq[Vector[String]] = parse_csv(flights_path)
    var passengers: Seq[Vector[String]] = parse_csv(passengers_path)

    /** Returns all lines in a given CSV file.
     *
     * @param path CSV file path
     * @return Vector[Vector[String]]
     */
    def parse_csv(path: String): Vector[Vector[String]] = {
        val file = io.Source.fromFile(path)
        val lines = file.getLines.drop(1)
        val res = lines.map(line => line.split(",").toVector).toVector
        file.close
        res // Can't be returned directly because the file has to be closed first
    }

    /** Returns the total number of flights for each month.
     * For each month, the format is:
     *
     * Month, Number of Flights
     *
     * @return IndexedSeq[Vector[Int]]
     */
    def question_1(): Seq[Array[Int]] = {
        val flight_count = Array.fill(12)(0) // One row per month
        // Increment count for each flight that flew during the right month
        flights.foreach(flight => flight_count(flight(4).split("-")(1).toInt - 1) += 1)
        (1 to 12).map(month => Array(month, flight_count(month - 1)))
    }

    /** Returns the names of the 100 most frequent flyers.
     * For each flyer, the format is:
     *
     * Passenger ID, Number of Flights, First name, Last name
     *
     * @return Array[Vector[String]]
     */
    def question_2(): Array[Vector[String]] = {
        val passenger_count = Array.fill(passengers.size)(0)
        flights.foreach(flight => passenger_count(flight(0).toInt - 1) += 1)
        // First zipWithIndex to keep original indices (thus IDs), then sort by count, then take top 100
        val sorted_passenger_count = passenger_count.zipWithIndex.sortWith(_._1 > _._1).take(100)
        // Build the desired return vector for each passenger by getting their First name and Last name
        sorted_passenger_count.map(passenger => {
            val index = passenger._2
            Vector((index - 1).toString, passenger._1.toString, passengers(index)(1), passengers(index)(2))
        })
    }

    /** Returns the greatest number of countries a passenger has been without being in the UK.
     * For each passenger, the format is:
     *
     * Passenger ID, Longest Run
     *
     * This method assumes the IDs of passengers start from 1, and are consecutive.
     *
     * @return IndexedSeq[Vector[Int]]
     */
    def question_3(): Seq[Vector[Int]] = {
        // Sort the flights by passenger IDs
        val sorted_flights = flights.sortWith(_ (0).toInt < _ (0).toInt)
        // Create a separate Vector to hold flight IDs for indexing
        val ids = sorted_flights.map(flight => flight(0).toInt)
        // current_index is the first index of the current passenger, next_index is the first index of the next one
        var current_index = 0
        var next_index = 0
        (1 to passengers.size).map(id => {
            next_index = ids.indexOf(id + 1) // Look up the first index of the next passenger
            if (next_index == -1) next_index = ids.size // For the last passenger
            // Take flights only for current passenger
            val pass_flights = sorted_flights.slice(current_index, next_index)
            current_index = next_index
            // First flatten the matrix, then remove adjacent duplicates
            val flat_list = pass_flights.flatten(pass => Vector(pass(2), pass(3))).foldRight(List[String]())(
                (head, tail) => if (tail.isEmpty || tail.head != head) head :: tail else tail).toVector

            // Find out when the passenger has been to the UK
            val uk_indices = flat_list.zipWithIndex.filter(_._1 == "uk").map(_._2)
            Vector(id, if (uk_indices.isEmpty) { // The passenger hasn't been to the UK, take the number of countries
                flat_list.size
            } else { // The passenger has been to the UK, find out the longest travel chain
                // Add -1 and flat_list.size to calculate first and last differences
                val adjusted_uk_indices = -1 +: uk_indices :+ flat_list.size
                // Zip with tail of list to calculate differences between consecutive numbers
                val diffs = adjusted_uk_indices.zip(adjusted_uk_indices.tail).map(pair => pair._2 - pair._1)
                if (diffs.max > 1) diffs.max - 1 else 0
            })
        })
    }

    /** Returns the passengers who have been on more than N flights together within the range (from,to).
     * For each passenger pair, the format is:
     *
     * Passenger 1 ID, Passenger 2 ID, Number of flights together, From, To
     *
     * @return Set[Vector[Any]]: Set[Vector[Int, Int, Int, String, String]]
     */
    def question_4_and_5(): Set[Vector[Any]] = {
        // Take only the time range specified, then sort by flight ID
        val sorted_flights = flights.filter(
            flight => from <= flight(4) && flight(4) <= to).sortWith(_ (1).toInt < _ (1).toInt)
        // Create a separate Vector to hold IDs for indexing
        val ids = sorted_flights.map(flight => flight(1).toInt)
        // current_index is the first index of the current flight, next_index is the first index of the next one
        var current_index = 0
        var next_index = 0
        val together = mutable.Map[(Int, Int), Queue[String]]() // Any because we need an Int then a Queue
        var often_together = Set[(Int, Int)]()
        ids.foreach(id => {
            next_index = ids.indexOf(id + 1) // Look up the first index of the next flight
            if (next_index == -1) next_index = ids.size // For the last flight
            // Take passengers only for current flight
            val flight_passengers = sorted_flights.slice(current_index, next_index).map(flight => (flight(0), flight(4)))
            current_index = next_index
            // For each passenger, search flights with passengers with a higher ID (triangular matrix)
            flight_passengers.indices.foreach(i => {
                for (j <- i + 1 until flight_passengers.size) {
                    val pass_1 = flight_passengers(i) // Simple renaming
                    val pass_2 = flight_passengers(j) // Simple renaming
                    if (pass_1 != pass_2) {
                        // Extract passenger IDs, and sort them so that (1,2) is the same as (2,1) in the Map
                        val pair = if (pass_1._1 < pass_2._1) {
                            (pass_1._1.toInt, pass_2._1.toInt)
                        } else {
                            (pass_2._1.toInt, pass_1._1.toInt)
                        }
                        // If the passenger pair exists, increment the number of flights together
                        if (together.contains(pair)) {
                            // Increment the head, add the date to the tail
                            together(pair) = (together(pair).head.toInt + 1).toString +: together(pair).tail :+ pass_1._2
                            // Only add to the final set if the number of flights together is already >= N
                            if (together(pair).head.toInt >= N) {
                                often_together += pair
                            }
                        } else { // Otherwise, create an entry in the Map
                            together += (pair -> Queue("1", pass_1._2)) // Could also be pass_2, same flight date
                        }
                    }
                }
            })
        })
        often_together.map(pair => {
            // Sort the dates to easily get the first and last flight dates
            val sorted_dates = together(pair).tail.sorted
            Vector(pair._1, pair._2, together(pair).head, sorted_dates.head, sorted_dates.last)
        })
    }
}
