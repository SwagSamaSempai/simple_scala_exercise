import org.scalatest.{BeforeAndAfter, FunSuite}

class Tests extends FunSuite with BeforeAndAfter {
    val questions = new Questions("Flight Data Assignment/flightData.csv",
                                  "Flight Data Assignment/passengers.csv",
                                  3, "2017-01-01", "2017-31-12"
                                  )

    before {
        questions.N = 3
        questions.from = "2017-01-01"
        questions.to = "2017-31-12"
    }

    test("Correct number of flights parsed") {
        assert(questions.flights.size == 100000)
    }

    test("Correct number of passengers parsed") {
        assert(questions.passengers.size == 15500)
    }

    test("Question 1 computes the right amount of flights per month") {
        assert(questions.question_1() == Vector(
            Vector(1, 9700),
            Vector(2, 7300),
            Vector(3, 8200),
            Vector(4, 9200),
            Vector(5, 9200),
            Vector(6, 7100),
            Vector(7, 8700),
            Vector(8, 7600),
            Vector(9, 8500),
            Vector(10, 7600),
            Vector(11, 7500),
            Vector(12, 9400)
            ))
    }

    test("Question 2 computes the right amount of flights per passenger") {
        assert(questions.question_2().take(10) == Vector(
            Vector("2066", "32", "Yolande", "Pete"),
            Vector("1675", "27", "Katherina", "Vasiliki"),
            Vector("4825", "27", "Jaime", "Renay"),
            Vector("3171", "26", "Sunshine", "Scott"),
            Vector("8959", "26", "Ginny", "Clara"),
            Vector("286", "25", "Pamila", "Mavis"),
            Vector("758", "25", "Vernia", "Mui"),
            Vector("915", "25", "Anisha", "Alaine"),
            Vector("2855", "25", "Son", "Ginette"),
            Vector("5094", "25", "Blythe", "Hyon")
            ))
    }

    test("Question 3 computes the correct greatest number of countries a passenger has been " +
             "without being in the UK") {
        assert(questions.question_3().take(20) == Vector(
            Vector(1, 6),
            Vector(2, 2),
            Vector(3, 5),
            Vector(4, 9),
            Vector(5, 2),
            Vector(6, 9),
            Vector(7, 5),
            Vector(8, 4),
            Vector(9, 7),
            Vector(10, 2),
            Vector(11, 8),
            Vector(12, 4),
            Vector(13, 5),
            Vector(14, 4),
            Vector(15, 3),
            Vector(16, 5),
            Vector(17, 11),
            Vector(18, 15),
            Vector(19, 5),
            Vector(20, 7)
            ))
    }

    test("Question 4 finds only passengers who have been on more than 3 flights together") {
        val computed_values = questions.question_4_and_5()
        assert(3 <= computed_values.map(pair => pair(2).toString).min.toInt)
        assert("2017-01-01" <= computed_values.map(pair => pair(3).toString).min)
        assert("2017-12-31" >= computed_values.map(pair => pair(4).toString).min)
    }

    test("Question 5 finds only passengers who have been on more than N flights together in (from,to)") {
        questions.N = 5
        questions.from = "2017-05-01"
        questions.to = "2017-08-01"
        val computed_values = questions.question_4_and_5()
        assert(questions.N <= computed_values.map(pair => pair(2).toString).min.toInt)
        assert(questions.from <= computed_values.map(pair => pair(3).toString).min)
        assert(questions.to >= computed_values.map(pair => pair(4).toString).min)
    }
}
