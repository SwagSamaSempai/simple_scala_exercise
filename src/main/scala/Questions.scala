import org.apache.spark.sql.functions.{col, month}
import org.apache.spark.sql.types.{DateType, IntegerType, StringType, StructType}
import org.apache.spark.sql.{DataFrame, Encoder, Encoders, Row, SparkSession}

import scala.collection.immutable.Queue
import scala.collection.mutable

object Questions {
    /** Returns a DataFrame generated from a CSV file.
     *
     * @param path   CSV file path
     * @param schema Schema of the CSV
     * @return org.apache.spark.sql.DataFrame
     */
    def parse_csv(path: String, schema: StructType): DataFrame = {
        val spark = SparkSession.builder().master("local[8]").appName("Main").getOrCreate()
        spark.read.option("header", "true").schema(schema).csv(path)
    }

    /** Returns a DataFrame generated from a flights CSV with format:
     *
     * passengerId, flightId, from, to, date
     *
     * @param path CSV file path
     * @return Vector[Vector[String]]
     */
    def parse_flights(path: String): DataFrame = {
        val schema = new StructType()
            .add("passengerId", IntegerType)
            .add("flightId", IntegerType)
            .add("from", StringType)
            .add("to", StringType)
            .add("date", StringType)
        parse_csv(path, schema)
    }

    /** Returns a DataFrame generated from a passengers CSV with format:
     *
     * passengerId, firstName, lastName
     *
     * @param path CSV file path
     * @return Vector[Vector[String]]
     */
    def parse_passengers(path: String): DataFrame = {
        val schema = new StructType()
            .add("passengerId", IntegerType)
            .add("firstName", StringType)
            .add("lastName", StringType)
        parse_csv(path, schema)
    }
}

class Questions(flights_path: String = "Flight Data Assignment/flightData.csv",
                passengers_path: String = "Flight Data Assignment/passengers.csv",
                minimum: Int = 3, from_date: String = "2017-01-01", to_date: String = "2017-12-31") {

    val flights: DataFrame = Questions.parse_flights(flights_path)
    val passengers: DataFrame = Questions.parse_passengers(passengers_path)
    var N: Int = minimum
    var from: String = from_date
    var to: String = to_date

    /** Returns the total number of flights for each month.
     * For each month, the format is:
     *
     * Month, Number of Flights
     *
     * @return IndexedSeq[Vector[Int]]
     */
    def question_1(): DataFrame = {
        flights.withColumn("month", month(col("date"))).groupBy("month").count()
    }

    /** Returns the passengers who have been on more than N flights together within the range (from,to).
     * For each passenger pair, the format is:
     *
     * Passenger 1 ID, Passenger 2 ID, Number of flights together, From, To
     *
     * @return Set[Vector[Any]]: Set[Vector[Int, Int, Int, String, String]]
     */
    def question_4_and_5(): IndexedSeq[IndexedSeq[Any]] = {
        val spark = SparkSession.builder().master("local").appName("Main").getOrCreate()
        import spark.implicits._
        val pass_flights = (1 to passengers.count.toInt).map(id => {
            flights.filter(row => row.getInt(0) == id).map(row => row.getInt(0))
        })
        pass_flights.indices.map(i => {
            (i + 1 until pass_flights.size).map(j => {
                val intersection = pass_flights(i).intersect(pass_flights(j))
                if (intersection.count >= 3) {
                    Vector(i + 1, j + 1, intersection.count.toInt)
                }
            })
        })
    }
}
