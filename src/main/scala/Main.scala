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

    val passengers = parse_csv("Flight Data Assignment/passengers.csv")
    val flights = parse_csv("Flight Data Assignment/flightData.csv")
}
