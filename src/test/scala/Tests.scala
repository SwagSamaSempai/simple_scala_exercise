import org.scalatest.FunSuite

class Tests extends FunSuite {
    val questions = new Questions("Flight Data Assignment/flightData.csv",
                                  "Flight Data Assignment/passengers.csv",
                                  3, "2017-01-01", "2017-31-12"
                                  )

    test("Correct number of flights") {
        assert(questions.flights.size == 100000)
    }

    test("Correct number of passengers") {
        assert(questions.passengers.size == 15500)
    }
}
