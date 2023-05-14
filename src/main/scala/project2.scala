import java.io.File
import com.github.tototoshi.csv._
import scala.io.Source
import java.io.PrintWriter
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Random
import scala.io.StdIn.readLine
import java.time._
import scala.util.Try
import java.util.Date
import java.text.SimpleDateFormat
import java.time.temporal.{ChronoField, TemporalAccessor}

object project2 extends App {
  // Ensure that the type of storage_capacity of the subclass is Double or a subtype.
  abstract class Renewable[+A](val time: String, val id: String, val storage_capacity: A) {
  }

  // The SolarPanel class inherits properties from the Renewable class, while defining its own properties and methods.
  class SolarPanel(id: String, var direction: String, var angle: Double, storage_capacity: Double) extends Renewable("", id, storage_capacity) {
    // Adjustment of solar panels operation, change of direction and change of angle
    def changeDirection(newDirection: String): Unit = {
      direction = newDirection
    }
    def changeAngle(newAngle: Double): Unit = {
      angle = newAngle
    }
    val directionValue = getDirectionValue(direction)
    // When the value of the attribute changes, the energy generated also changes.
    var energyStorage: Double = angle * 10 * directionValue
  }

  // The WindTurbines class inherits properties from the Renewable class, while defining its own properties and methods.
  class WindTurbines(id: String, var direction: String, var speed:Double, var angle: Double, storage_capacity: Double) extends Renewable("", id, storage_capacity) {
    // Adjustment of wind turbines operation, change of speed, change of direction, change of angle
    def changeSpeed(newSpeed: Double): Unit = {
      speed = newSpeed
    }
    def changeDirection(newDirection: String): Unit = {
      direction = newDirection
    }
    def changeAngle(newAngle: Double): Unit = {
      angle = newAngle
    }
    val directionValue = getDirectionValue(direction)
    // When the value of the attribute changes, the energy generated also changes.
    var energyStorage: Double =  speed * 100 * directionValue
  }

  // The HydroPower class inherits properties from the Renewable class, while defining its own properties and methods.
  class HydroPower(id: String, var speed: Double, var status: Double, storage_capacity: Double) extends Renewable("", id, storage_capacity) {
    // Adjustment of hydropower operation, change of speed and change of status
    def changeSpeed(newSpeed: Double): Unit = {
      speed = newSpeed
    }
    def changeStatus(newStatus: Double): Unit = {
      status = newStatus
    }
    // When the value of the attribute changes, the energy generated also changes.
    var energyStorage: Double = speed * status * 1000
  }

  val random = new Random()
  // Generating random values based on the direction of renewable energy sources
  def getDirectionValue(direction: String): Int = direction match {
    case "north" | "east" | "south" | "west" => random.nextInt(3) + 1
    case _ => random.nextInt(4) + 1 // Generate a random number if no match in either direction
  }

  val f1 = new File("solarPanel.csv")
  val writer1 = CSVWriter.open(f1)
  writer1.writeAll(List(List("ID", "Panel Orientation", "Panel Angle(degree)", "Energy Storage Capacity(kWh)"),
    List("SP1", "west", "25", "212.89"),
    List("SP2", "north", "60", "802.89"),
    List("SP3", "north", "45", "194.92"),
    List("SP4", "east", "30", "950.5"),
    List("SP5", "south", "15", "411.21"),
  ))

  val f2 = new File("windTurbine.csv")
  val writer2 = CSVWriter.open(f2)
  writer2.writeAll(List(List("ID", "Wind Turbine Direction", "Rotational Speed(RPM)", "Blade Angle(degree)", "Energy Storage Capacity(kWh)"),
    List("WT1", "south", "11.3", "8.6", "112.12"),
    List("WT2", "west", "9.8", "6.2", "514.25"),
    List("WT3", "east", "12.7", "9.4", "615.13"),
    List("WT4", "northeast", "8.2", "4.9", "710.08"),
    List("WT5", "south", "10.5", "7.1", "129.45"),
  ))

  val f3 = new File("hydroPower.csv")
  val writer3 = CSVWriter.open(f3)
  writer3.writeAll(List(List("ID", "Generator Speed (RPM)", "Water Intake Pipe Status", "Energy Storage Capacity(kWh)"),
    List("HP1", "870", "3", "250.78"),
    List("HP2", "1430", "1", "450.56"),
    List("HP3", "920", "4", "300.45"),
    List("HP4", "1060", "2", "400.06"),
    List("HP5", "1250", "2", "350.35"),
  ))

  val f4 = new File("renewablePower.csv")
  val writer4 = CSVWriter.open(f4)
  writer4.writeAll(List(List("Time", "ID", "Energy Generated(kWh)"),
    List("12-02-2023 06:00:00", "HP5", "1234.35"),
    List("22-02-2023 10:00:00", "HP1", "525.28"),
    List("15-03-2023 08:00:00", "WT2", "2302.59"),
    List("21-03-2023 16:00:00", "HP3", "3620.47"),
    List("11-04-2023 14:00:00", "WT5", "220"),
    List("20-04-2023 20:00:00", "SP4", "1200.9"),
    List("28-04-2023 18:00:00", "WT3", "93.23"),
    List("30-04-2023 22:00:00", "SP2", "219.14"),
    List("01-05-2023 10:00:00", "SP2", "792.32"),
    List("01-05-2023 12:00:00", "SP3", "4201.54"),
    List("01-05-2023 12:10:00", "WT3", "0.28"),
    List("11-05-2023 08:15:00", "SP1", "991.11"),
    List("12-05-2023 12:00:00", "SP1", "4230.32"),
    List("11-05-2023 12:45:00", "WT2", "449.6"),
    List("11-05-2023 14:30:00", "WT4", "485.37"),
    List("11-05-2023 19:45:00", "HP3", "388.38"),
  ))


  // The options menu is displayed and the user need to enter
  def menu(): Unit = {
    var running = true
    while (running) {
      println("1. Print view")
      println("2. Energy generation")
      println("3. Analyse the data")
      println("4. Filter the data for time")
      println("5. Sort the data")
      println("6. Search the energy data use ID")
      println("7. Detecting equipment failure")
      println("8. Adjust power plant's operation")
      println("9. Exit")
      val input = readLine("Enter your choice: ")
      input match {
        case "1" => menu1()
        case "2" => menu2()
        case "3" => menu3()
        case "4" => menu4()
        case "5" => menu5()
        case "6" => menu6()
        case "7" => menu7()
        case "8" => menu8()
        case "9" =>
          println("Exit successfully")
          running = false
        case _ => println("Enter again")
      }
    }
  }
  menu()


  // Print views of individual data files as a tabular output.
  def menu1(): Unit = {
    println("-------------------------------------------------------------------------")
    println("Solar Panel view:")
    // Read the file and print each line
    val reader = CSVReader.open(new File("solarPanel.csv"))
    reader.all.foreach(row => {
      val data = "%-9s%-26s%-26s%-26s"
      println(data.format(row(0), row(1), row(2), row(3)))
    })
    reader.close()
    println("-------------------------------------------------------------------------")
    println("Wind Turbines view:")
    val reader1 = CSVReader.open(new File("windTurbine.csv"))
    reader1.all.foreach(row => {
      val data1 = "%-9s%-26s%-26s%-26s"
      println(data1.format(row(0), row(1), row(2), row(3),row(4)))
    })
    reader1.close()
    println("-------------------------------------------------------------------------")
    println("Hydro Power view:")
    val reader2 = CSVReader.open(new File("hydroPower.csv"))
    reader2.all.foreach(row => {
      val data2 = "%-9s%-26s%-26s%-26s"
      println(data2.format(row(0), row(1), row(2), row(3)))
    })
    reader2.close()
    println("-------------------------------------------------------------------------")
    println("Renewable Power view:")
    val reader3 = CSVReader.open(new File("renewablePower.csv"))
    reader3.all.foreach(row => {
      val data3 = "%-26s%-10s%-26s"
      println(data3.format(row(0), row(1), row(2)))
    })
    reader3.close()
    println("-------------------------------------------------------------------------")
  }


  // Record all existing energy generated from renewable sources
  def menu2(): Unit = {
    def readSolarPanels(filePath: String): List[SolarPanel] = {
      val reader = CSVReader.open(filePath)
      val rows = reader.all().tail // Skip the header row and read all rows of data
      reader.close()
      // Maps each row of data in the CSV file to a SolarPanel object and returns a List
      rows.map(row => new SolarPanel(row(0), row(1), row(2).toDouble, row(3).toDouble))
    }

    def readWindTurbines(filePath: String): List[WindTurbines] = {
      val reader = CSVReader.open(filePath)
      val rows = reader.all().tail // Skip the header row and read all rows of data
      reader.close()
      // Maps each row of data in the CSV file to a WindTurbines object and returns a List
      rows.map(row => new WindTurbines(row(0), row(1), row(2).toDouble, row(3).toDouble,row(4).toDouble))
    }

    def readHydroPower(filePath: String): List[HydroPower] = {
      val reader = CSVReader.open(filePath)
      val rows = reader.all().tail // Skip the header row and read all rows of data
      reader.close()
      // Maps each row of data in the CSV file to a HydroPower object and returns a List
      rows.map(row => new HydroPower(row(0), row(1).toDouble, row(2).toDouble, row(3).toDouble))
    }

    // Read data from three different CSV files
    val solarPanels = readSolarPanels("solarPanel.csv")
    val windTurbines = readWindTurbines("windTurbine.csv")
    val hydroPower = readHydroPower("hydroPower.csv")

    val data = solarPanels ++ windTurbines ++ hydroPower // Merge the three lists into one list and assign the value to data

    // Writing data to a CSV file
    def writeDataToFile[T <: Renewable[_]](data: List[T], filePath: String): Unit = {
      val writer = CSVWriter.open(filePath, append = true)
      val currentTime = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss").format(new Date()) // Create a date formatted string currentTime under the current time
      // Write the corresponding data to the CSV file for the different types of elements
      for (d <- data) {
        d match {
          case h: HydroPower =>
            writer.writeRow(List(s"${currentTime}", s"${h.id}", s"${h.energyStorage}"))
          case w: WindTurbines =>
            writer.writeRow(List(s"${currentTime}", s"${w.id}", s"${w.energyStorage}"))
          case s: SolarPanel =>
            writer.writeRow(List(s"${currentTime}", s"${s.id}", s"${s.energyStorage}"))
        }
      }
      writer.close()
    }
    writeDataToFile(data, "renewablePower.csv")
    println("Already filled in.")
  }


  // Analyse data
  def menu3(): Unit = {
    val reader1 = CSVReader.open(new File("renewablePower.csv"))
    val data1 = reader1.toStream.drop(1) // Skip the header row and read all rows of data
    val energy = data1.map(fields => fields(2).toDouble).toArray
    val reader = Source.fromFile("renewablePower.csv")
    val lines = reader.getLines().toSeq.tail // exclude the header line
    val data = lines.map(_.split(",").tail.tail.map(_.toDouble))

    // Calculating the average, using currying
    def calculateAverage(data: Stream[List[String]])(column: Int): Double = {
      val sum = data.foldLeft(0.0)((acc, row) => acc + row(column).toDouble)
      sum / data.length
    }
    // Bind the first parameter to the function to get a function that accepts the second parameter
    val avgFunction = calculateAverage(data1) _
    val energyAvg = String.format("%.2f", avgFunction(2)) // Retain two decimal places
    println(s"The average of the renewable power is $energyAvg")

    // Using type parameterization to calculate median values
    def calculateMedian[T](values: Array[T])(implicit num: Numeric[T]): Double = {
      // Sorting arrays
      val sortedValues = values.sorted
      if (sortedValues.length % 2 == 0) {
        // If the length of the array is even, take the average of the middle two numbers
        val mid = sortedValues.length / 2
        num.toDouble(num.plus(sortedValues(mid - 1), sortedValues(mid))) / 2.0
      } else {
        // If the length of the array is an odd number, take the middle number
        num.toDouble(sortedValues(sortedValues.length / 2))
      }
    }
    val PowerMid = String.format("%.2f", calculateMedian(energy))
    println(s"The median of the renewable power is $PowerMid")

    // Take the elements of the first column of this two-dimensional array to form an array and group them.
    // mapValues(_.size) means to regenerate a Map by taking the number of elements of each grouping as values.
    // Then use the maxBy(_. _2) method to find the key-value pair with the largest value in this Map, which is the mode value.
    val mode = data.map(_(0)).groupBy(identity).mapValues(_.size).maxBy(_._2)._1
    println("Mode for renewable power is " + mode)

    // Using type parameterization to calculate range
    def findMinMax[T](arr: Array[T], index: Int, min: T, max: T)(implicit num: Numeric[T]): (Double, T) = {
      if (index == arr.length) {
        // The array is traversed and the difference between the max and min value and the max value are returned
        (num.toDouble(num.minus(max, min)), max)
      } else {
        // Recursive function call to update the max and min values
        val newMin = if (num.lt(arr(index), min)) arr(index) else min
        val newMax = if (num.gt(arr(index), max)) arr(index) else max
        findMinMax(arr, index + 1, newMin, newMax)
      }
    }
    val (powerRange, windMax) = findMinMax(energy, 0, Double.MaxValue, Double.MinValue)
    val Range = String.format("%.2f", powerRange)
    println(s"The difference between the highest and lowest values is $Range")

    // Using type parameterization to midrange range
    def midrange[T](xs: Seq[T])(implicit num: Numeric[T]): Double = {
      // The midrange value is the average of the maximum and minimum values
      (num.toDouble(xs.min) + num.toDouble(xs.max)) / 2
    }
    val midranges = Seq(midrange(data.map(_(0).toDouble)))
    println("Midrange for renewable power is: " + midranges(0))
    reader1.close()
    reader.close()
  }

  // Filter data by hourly, daily, weekly and monthly
  def menu4(): Unit = {
    // Use currying to filter data based on the date entered by the user
    def FilterDate(inputDate: String)(data: List[List[String]]): Unit = {
      val timeFormatterList = List( // Contains several time formatters for parsing different formats of datetime strings
        DateTimeFormatter.ofPattern("MM-yyyy"),
        DateTimeFormatter.ofPattern("dd-MM-yyyy"),
        DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"),
      )
      // Check that each time format string can be parsed into a datetime string and check its precision
      val dateType = timeFormatterList
        .find(formatter => Try(formatter.parse(inputDate)).isSuccess)
        .map { formatter =>
          val temporalAccessor = formatter.parse(inputDate)
          if (temporalAccessor.isSupported(ChronoField.HOUR_OF_DAY)) { // The input format is dd-MM-yyyy HH:mm:ss
            "hour"
          } else if (temporalAccessor.isSupported(ChronoField.DAY_OF_MONTH)) { // The input format is dd-MM-yyyy
            "day"
          } else if (temporalAccessor.isSupported(ChronoField.MONTH_OF_YEAR)) { // // The input format is MM-yyyy
            "month"
          } else {
            ""
          }
        }
      // Assign the time precision string (if any) stored in the dateType variable to the timeType variable
      val timeType = dateType.getOrElse("")
      timeType match { // execute different filtering operations based on the precision of the date
        case "day" =>
          val filterType = readLine("Please enter the type of date to be filtered (day/week): ") // Enter the type of date to be filtered
          if (filterType == "day") {
            // Prints rows of data matching the input date
            val dataList = data
              .filter(row => row(0).startsWith(inputDate))
              .map { row =>
                s"${row(0)}, ${row(1)}, ${row(2)}"
              }
              .foreach(println)
          }
          else if (filterType == "week") {
            val inputDateFormat = DateTimeFormatter.ofPattern("dd-MM-yyyy") // Created an object with a "dd-MM-yyyy" pattern
            // Parses the inputDate into a LocalDate object representing the start of the week
            val startTime = LocalDate.parse(inputDate, inputDateFormat).atStartOfDay()
            val endTime = startTime.plusDays(7) // A week has seven days
            val dataList = data
              .filter { row =>
                // Check that the first column indicating the time is equal to or falls within the start and end time of the week
                val time = LocalDateTime.parse(row(0), DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"))
                time.isEqual(startTime) || (time.isAfter(startTime) && time.isBefore(endTime))
              }
              .map { row =>
                s"${row(0)}, ${row(1)}, ${row(2)}"
              }
              .foreach(println)
          }
        case "hour" =>
          // Parse the inputDate in the format "dd-MM-yyyy HH:mm:ss" into a LocalDateTime object as the filter start time
          val startTime = LocalDateTime.parse(inputDate, DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"))
          val endTime = startTime.plusHours(1) // Filtering per hour
          val dataList = data
            .filter { row =>
              val time = LocalDateTime.parse(row(0), DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"))
              time.isEqual(startTime) || (time.isAfter(startTime) && time.isBefore(endTime))
            }
            .map { row =>
              s"${row(0)}, ${row(1)}, ${row(2)}"
            }
            .foreach(println)
        case "month" =>
          // Parse the inputDate in the format "MM-yyyy" into a YearMonth object as the filter start time
          val dateMonth = YearMonth.parse(inputDate, DateTimeFormatter.ofPattern("MM-yyyy"))
          val startTime = dateMonth.atDay(1).atStartOfDay() // Set startTime to midnight of the first day of the month
          val endTime = dateMonth.atEndOfMonth().atTime(23, 59, 59) // Set endTime to 23:59:59 on the last day of the month
          val dataList = data
            .filter { row =>
              val time = LocalDateTime.parse(row(0), DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"))
              time.isEqual(startTime) || (time.isAfter(startTime) && time.isBefore(endTime))
            }
            .map { row =>
              s"${row(0)}, ${row(1)}, ${row(2)}"
            }
            .foreach(println)
        case _ => println("Incorrect date format entered.")
      }
    }
    val reader = CSVReader.open(new File("renewablePower.csv"))
    val data = reader.toStream.drop(1).toList
    reader.close()
    val inputDate = scala.io.StdIn.readLine("Enter target date (dd-MM-yyyy HH:mm:ss or dd-MM-yyyy or MM-yyyy): ")
    FilterDate(inputDate)(data) // Accepts a date string as the first parameter and then returns a function that accepts a list of data as the second parameter
  }


  def menu5():Unit = {
    val reader = CSVReader.open(new File("renewablePower.csv"))
    val data = reader.toStream.drop(1)
    // Sort data by the third column
    val sortedData = data.sortBy(row => row(2).toDouble)
    println("------------------------------------------------------")
    println("Sorted energy view:")
    val header = Seq("Time", "ID", "Energy Generated(kWh)")
    val formattedHeader = "%-25s%-10s%-25s".format(header(0), header(1), header(2))
    println(formattedHeader)
    sortedData.foreach { row =>
      val formattedRow = "%-25s%-10s%-25s".format(row(0), row(1), row(2))
      println(formattedRow)
    }
    println("------------------------------------------------------")
    reader.close()
  }


  def menu6(): Unit = {
    val reader = CSVReader.open(new File("renewablePower.csv")) //read the file
    val data = reader.toStream.drop(1)
    println("Please enter the ID you want to search:")
    val IDSearch = scala.io.StdIn.readLine()
    // Find data
    val searchData: Option[Seq[Seq[String]]] = if (data.exists(line => line(1) == IDSearch)) {
      // Find the data and filter out all matching rows
      Some(data.filter(line => line(1) == IDSearch))
    } else {
      // No data found
      None
    }
    // Processing result
    searchData match {
      case Some(lines) =>
        // Find the data and print it
        println(s"Found ${lines.size} row(s) data for ID $IDSearch.")
        println("------------------------------------------------------")
        outputSearchedData(lines)
        println("------------------------------------------------------")
      case None =>
        // Data not found, error reported
        println(s"No such ID $IDSearch exists.")
    }
    def outputSearchedData(lines: Seq[Seq[String]]): Unit = {
      println("Time                 ID   Energy Generated(kWh)")
      lines.foreach(line => println(line.mkString("  ")))
    }
  }

  def menu7():Unit = {
    val reader = CSVReader.open(new File("renewablePower.csv")) // Open the CSV file
    val data = reader.toStream.drop(1) //drop the first row (header)
    val energyStandard: Double = 100.0
    val checkEnergyGeneration: Seq[Either[String, (String, String, Double)]] = data.map { lines =>
      lines match {
        case Seq(time, id, energyGenerated)
          // If the energy generated is a valid number and greater than or equal to the energy standard, return the energy data as Right
          if energyGenerated.matches("""\d+(\.\d+)?""") =>
          val energy = energyGenerated.toDouble
          // If the energy generated is less than the energy standard, return an error message as Left
          if (energy < energyStandard) {
            Left(s"$id has low energy output, please operator to repair this source as soon as possible.")
          } else {
            Right((time, id, energy))
          }
        // If the energy value is invalid, return an error message as Left
        case Seq(_, id, energyGenerated) =>
          Left(s"Invalid energy value for $id")
        case _ =>
          Left("Invalid line format")
      }
    }
    //filteredData.collect { case Right((time, id, energy)) => (time, id, energy) }.foreach { case (time, id, energy) =>
    //val formattedRow = "%-25s%-10s%-25s".format(time, id, energy)
    //println(formattedRow)
    //}
    // Check if all renewable energy sources generate qualified energy
    val energyCompliance: Boolean = checkEnergyGeneration.forall {
      case Left(_) => false
      case Right((_, _, energy)) => energy >= energyStandard
    }
    // If all renewable energy sources generate qualified energy, print a success message
    if (energyCompliance) {
      println("All renewable energy sources generate qualified energy.")
    } else if (checkEnergyGeneration.exists(_.isRight)) {
      println("Low energy output detected:")
      checkEnergyGeneration.collect {
        case Left(error) => error
      }.foreach {
        error => println(error)
      }
      println("Other renewable energy sources have qualified energy generation.")
    }
    reader.close()
  }


  def menu8(): Unit = {
    println("1. Solar Panel")
    println("2. Wind Turbine")
    println("3. Hydropower")
    val input = readLine("Enter your choice：")
    //reads the user's input from the console
    input match {
      case "1" => Solar()
      case "2" => Wind()
      case "3" => Hydro()
      case _ => println("Enter again")
    }
  }


  def Solar(): Unit = {
    // Read solar panel data from file and convert to objects
    def readSolarPanelsFromFile(file: String): Seq[SolarPanel] = {
      val reader = Source.fromFile(file)
      val lines = reader.getLines().toSeq.tail // exclude the header line
      val data = lines.map(_.split(",").tail.tail.map(_.toDouble)) // Extract numerical data
      val panels = lines.map(line => {
        val Array(id, direction, angleStr, capacityStr) = line.split(",")
        val angle = angleStr.toDouble
        val capacity = capacityStr.toDouble
        new SolarPanel(id, direction, angle, capacity)
      })
      reader.close()
      panels // Return list of solar panels
    }

    // Look for solar panels by id
    def findSolarPanelById[T <: {val id: String}](panels: Seq[T], id: String): Option[T] = {
      panels.find(_.id == id)
    }

    // Modify the orientation of the solar panel
    def changeSolarPanelDirection(solarPanel: SolarPanel, newDirection: String): Unit = {
      solarPanel.changeDirection(newDirection)
    }

    // Modify the angle of a solar panel
    def changeSolarPanelAngle(solarPanel: SolarPanel, newAngle: Double): Unit = {
      solarPanel.changeAngle(newAngle)
    }

    // Store the modified solar panel data in a file
    def saveSolarDataToFile(solarData: Seq[SolarPanel], filename: String): Unit = {
      val pw = new PrintWriter(filename) // Open file for writing
      pw.println("ID, Panel Orientation, Panel Angle(degree), Energy Storage Capacity(kWh)")
      solarData.foreach { solarPanel =>
        pw.println(s"${solarPanel.id},${solarPanel.direction},${solarPanel.angle},${solarPanel.storage_capacity}")
      } // Open file for writing
      pw.close()
    }

    // Main program logic for Solar function
    val panels = readSolarPanelsFromFile("solarPanel.csv") // Load solar panel data from file
    println("1. Change direction")
    println("2. Change angle")
    val input = readLine("Enter your choice：") // Load solar panel data from file
    val inputId = scala.io.StdIn.readLine("Please enter the SolarPanel id：")
    input match {
      case "1" => {
        val inputDirection = scala.io.StdIn.readLine("Enter new direction：")
        findSolarPanelById(panels, inputId) match {
          case Some(solarPanel) =>
            // Change the direction of the specified solar panel
            changeSolarPanelDirection(solarPanel, inputDirection)
            // Save the modified solar panel data back to file
            saveSolarDataToFile(panels, "solarPanel.csv")
            println(s"Solar Panel ${inputId} change new direction to ${inputDirection}")
          case None =>
            println(s"Cannot find the Solar Panel${inputId}")
        }
      }
      case "2" => {
        val inputAngle = scala.io.StdIn.readLine("Enter new angle：").toDouble
        findSolarPanelById(panels, inputId) match {
          case Some(solarPanel) =>
            // Change the angle of the specified solar panel
            changeSolarPanelAngle(solarPanel, inputAngle)
            // Change the angle of the specified solar panel
            saveSolarDataToFile(panels, "solarPanel.csv")
            println(s"Solar Panel${inputId}change new direction to${inputAngle}")
          case None =>
            println(s"Cannot find the Solar Panel${inputId}")
        }
      }
      case _ => println("Enter again")
    }
  }


  def Wind(): Unit = {
    // takes a file path and returns a sequence of WindTurbines
    def readWindTurbinesFromFile(file: String): Seq[WindTurbines] = {
      val reader = Source.fromFile(file) // Open the file and read the lines
      val lines = reader.getLines().toSeq.tail // exclude the header line
      val data = lines.map(_.split(",").tail.tail.map(_.toDouble))  // Parse each line to extract the data and convert it to double
      // Create a WindTurbines object for each line
      val turbines = lines.map(line => {
        val Array(id, direction, speedStr, angleStr, capacityStr) = line.split(",")
        val speed = speedStr.toDouble
        val angle = angleStr.toDouble
        val capacity = capacityStr.toDouble
        new WindTurbines(id, direction, speed, angle, capacity)
      })
      reader.close()
      turbines
    }

    // Look for Wind Turbines by id
    def findWindTurbinesById[T <: {val id: String}](turbines: Seq[T], id: String): Option[T] = {
      turbines.find(_.id == id)
    }

    // Modify the direction of the Wind Turbines
    def changeWindTurbinesDirection(WindTurbines: WindTurbines, newDirection: String): Unit = {
      WindTurbines.changeDirection(newDirection)
    }

    // Modify the angle of the Wind Turbines
    def changeWindTurbinesAngle(WindTurbines: WindTurbines, newAngle: Double): Unit = {
      WindTurbines.changeAngle(newAngle)
    }

    // Modify the speed of the Wind Turbines
    def changeWindTurbinesSpeed(WindTurbines: WindTurbines, newSpeed: Double): Unit = {
      WindTurbines.changeSpeed(newSpeed)
    }

    // Store the modified Wind Turbines data in a file
    def saveWindDataToFile(WindData: Seq[WindTurbines], filename: String): Unit = {
      // Open a new file for writing
      val pw = new PrintWriter(filename)
      pw.println("ID,Wind Turbine Direction,Rotational Speed(RPM),Blade Angle(degree),Energy Storage Capacity(kWh)")
      // Write each WindTurbines object to the file
      WindData.foreach { windTurbines =>
        pw.println(s"${windTurbines.id},${windTurbines.direction},${windTurbines.speed},${windTurbines.angle},${windTurbines.storage_capacity}")
      }
      pw.close()
    }

    // Main program logic for Wind function
    val turbines = readWindTurbinesFromFile("windTurbine.csv")
    println("1. Change direction") // Print options for the user to choose
    println("2. Change speed")
    println("3. Change angle")
    val input = readLine("Enter your choice: ") // Prompt the user to enter their choice
    val inputId = scala.io.StdIn.readLine("Please enter the windTurbine id: ")
    input match {
      // If the user chooses to change the direction
      case "1" => {
        val inputDirection = scala.io.StdIn.readLine("Enter new direction: ")
        findWindTurbinesById(turbines, inputId) match {
          case Some(windTurbines: WindTurbines) =>
            // Change the wind turbine's direction to the new value
            changeWindTurbinesDirection(windTurbines, inputDirection)
            // Save the updated wind turbine data to the CSV file
            saveWindDataToFile(turbines, "windTurbine.csv")
            // Print a message confirming the change
            println(s"WindTurbine ${inputId} change new direction to ${inputDirection}")
          // If the wind turbine is not found
          case None =>
            println(s"Cannot find the Wind Turbine ${inputId}")
        }
      }
      case "2" => {
        val inputSpeed = scala.io.StdIn.readLine("Enter new speed: ").toDouble
        findWindTurbinesById(turbines, inputId) match {
          case Some(windTurbines: WindTurbines) =>
            // Change the wind turbine's speed to the new value
            changeWindTurbinesSpeed(windTurbines, inputSpeed)
            // Save the updated wind turbine data to the CSV file
            saveWindDataToFile(turbines, "windTurbine.csv")
            println(s"Wind Turbine ${inputId} change new speed to ${inputSpeed}")
          case None =>
            println(s"Cannot find the Wind Turbine ${inputId}")
        }
      }
      case "3" => {
        val inputAngle = scala.io.StdIn.readLine("Enter new angle: ").toDouble
        findWindTurbinesById(turbines, inputId) match {
          case Some(windTurbines: WindTurbines) =>
            // Change the wind turbine's angle to the new value
            changeWindTurbinesAngle(windTurbines, inputAngle)
            saveWindDataToFile(turbines, "windTurbine.csv")
            println(s"Wind Turbine ${inputId} change new angle to ${inputAngle}")
          case None =>
            println(s"Cannot find the Wind Turbine ${inputId}")
        }
      }
      case _ => println("Enter again")
    }
  }


  def Hydro(): Unit = {
    def readHydroPowerFromFile(file: String): Seq[HydroPower] = {
      // Open the file
      val reader = Source.fromFile(file)
      val lines = reader.getLines().toSeq.tail // exclude the header line
      val data = lines.map(_.split(",").tail.tail.map(_.toDouble))
      val hydroPower = lines.map(line => {
        val Array(id, speedStr, statusStr, capacityStr) = line.split(",")
        val speed = speedStr.toDouble
        val status = statusStr.toDouble
        val capacity = capacityStr.toDouble
        new HydroPower(id, speed, status, capacity)
      })
      // Close the file and return the sequence of HydroPower objects
      reader.close()
      hydroPower
    }

    // Look for HydroPower by id
    def findHydroPowerById[T <: {val id: String}](hydroPower: Seq[T], id: String): Option[T] = {
      hydroPower.find(_.id == id)
    }

    // Modify the speed of the Hydropower
    def changeHydroPowerSpeed(HydroPower: HydroPower, newSpeed:Double): Unit = {
      HydroPower.changeSpeed(newSpeed)
    }

    // Modify the pipe status of the Hydropower
    def changeHydroPowerStatus(HydroPower: HydroPower, newStatus: Double): Unit = {
      HydroPower.changeStatus(newStatus)
    }

    // Store the modified Hydropower data in a file
    def saveHydroDataToFile(HydroData: Seq[HydroPower], filename: String): Unit = {
      val pw = new PrintWriter(filename)
      pw.println("ID,Generator Speed (RPM),Water Intake Pipe Status,Energy Storage Capacity(kWh)")
      // Write each HydroPower object to the file in CSV format
      HydroData.foreach { hydroPower =>
        pw.println(s"${hydroPower.id},${hydroPower.speed},${hydroPower.status},${hydroPower.storage_capacity}")
      }
      pw.close()
    }

    // Main program logic for Hydro function
    val hydro = readHydroPowerFromFile("hydroPower.csv")// Read the hydro power data from the CSV file
    println("1. Change speed")
    println("2. Change pipe status")
    val input = readLine("Enter your choice：") // Get the user's choice
    val inputId = scala.io.StdIn.readLine("Please enter the hydropower id: ")
    input match {
      // If the user chose to change speed
      case "1" => {
        val inputSpeed = scala.io.StdIn.readLine("Enter new speed:").toDouble
        // Find the HydroPower object with the given ID
        findHydroPowerById(hydro, inputId) match {
          // If a matching HydroPower object was found
          case Some(hydroPower: HydroPower) =>
            // Change the speed of the HydroPower
            changeHydroPowerSpeed(hydroPower, inputSpeed)
            saveHydroDataToFile(hydro, "hydroPower.csv")
            println(s"HydroPower${inputId} change new speed to ${inputSpeed}")
          case None =>
            println(s"Cannot find the Hydro Power ${inputId}")
        }
      }
      case "2" => {
        val inputStatus = scala.io.StdIn.readLine("Enter new pipe status:").toDouble
        findHydroPowerById(hydro, inputId) match {
          case Some(hydroPower: HydroPower) =>
            // Change the pipe status of the Hydropower
            changeHydroPowerStatus(hydroPower, inputStatus)
            // Save the updated Hydropower data to the CSV file
            saveHydroDataToFile(hydro, "hydroPower.csv")
            println(s"Hydro Power${inputId} change new pipe status to ${inputStatus}")
          case None =>
            println(s"Cannot find the Hydro Power ${inputId}")
        }
      }
      case _ => println("Enter again")
    }
  }
}