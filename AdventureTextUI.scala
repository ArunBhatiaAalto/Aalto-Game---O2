package o1.adventure.ui

import o1.adventure._
import scala.io.StdIn._

/** The singleton object `AdventureTextUI` represents a fully text-based version of the
  * Adventure game application. The object serves as a possible entry point for the game,
  * and can be run to start up a user interface that operates in the text console.
  * @see [[AdventureGUI]] */
object AdventureTextUI extends App {

  private var examDone = false               // Maths & physics exam
  private var quizDone = false               // TEK - quiz, when "true" gives you access to snack
  private var snackEaten = false

  private val game = new Adventure
  private val player = game.player
  this.run()


  private def eatSnack() = {
    if (!snackEaten && quizDone) {
      println("\n")
      println("You have now eaten your snack.")
      game.timeLimit = game.timeLimit + 4
      println(s"You have now +4 turns available (in total ${game.timeLimit - game.turnCount}).")
      this.snackEaten = true
      player.remove("snack")
    }
    else {
      println("\n")
      println("You can't eat a snack right now.")
    }
  }


  private def stealItem() = {
    println("\n")
    println("You are on your way towards the library and you encounter a student from another university.")
    println("The student steals your new PoRa-badge!")
    println("Try to find the badge but keep an eye on your steps limit!")
    this.game.stealBadge()
  }

  private def eatLunch() = {
    if (player.location.name == "A-Block" && !player.lunch) {
      println("\n")
      println("You have now eaten lunch.")
      game.timeLimit = game.timeLimit + 7
      println(s"You have now +7 turns available (in total ${game.timeLimit - game.turnCount}).")
      player.lunch = true
    }
    else {
      println("\n")
      println("You can't eat a lunch right now.")
    }
  }

  /** Runs the game. First, a welcome message is printed, then the player gets the chance to
    * play any number of turns until the game is over, and finally a goodbye message is printed. */
  private def run() = {
    println(this.game.welcomeMessage)
    while (!this.game.isOver) {
      this.printAreaInfo()
      this.playTurn()
    }
    println("\n" + this.game.goodbyeMessage)
  }


  /** Prints out a description of the player character's current location, as seen by the character. */
  private def printAreaInfo() = {
    val area = this.player.location
    println("\n\n" + area.name)
    println("-" * area.name.length)
    println(area.fullDescription + "\n")
  }


  /** Requests a command from the player, plays a game turn accordingly, and prints out a report of what happened.  */
  private def playTurn() = {
    println()
    println("★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★")
    println()
    val command = readLine("Command: ")

    //commands other than these should not affect the turn count
    var commands = Vector("go north", "go south", "go east", "go west")
    val onlyDirection = command.drop(3) //only useful if 'go X'

    if (!commands.contains(command)) {
      game.timeLimit = game.timeLimit + 1
    } else if (commands.contains(command) && player.location.neighbor(onlyDirection).isEmpty) {
      game.timeLimit = game.timeLimit + 1
    }
    var turnReport = this.game.playTurn(command)
    if (!turnReport.isEmpty) {
      turnReport = turnReport + "\n" + "Turns left: " + (game.timeLimit - game.turnCount).toString
      println(turnReport)
    }
    if (command == "exam") exam1()

    if (command == "quiz") quiz2()

    if (command == "lunch") eatLunch()

    if (command == "snack") eatSnack()

    if (player.location.name == "A-Block" && !player.lunch) {
      println("\n")
      println("You can eat lunch at A-Block and gain +7 turns.")
      println("Type 'lunch' to eat lunch.")
    }

    if (player.location.name == "Library" && this.player.has("book")) {
      println("\nYou are now arriving to the Library.")
      println("Enter: 'returnbook' to return the Discrete Mathematics book.")
    }

    if ((player.location.name == "Library") && (this.player.has("badge"))) stealItem()

    if ((player.location.name == "Undergraduate center") && (!examDone)) {
      println("\nYou are now arriving at the Undergraduate center.")
      if (!examDone) println("Enter: 'exam' to do the maths & physics exam.")
    }

    if ((player.location.name == "Dipoli") && (!quizDone)) {
      println("\nYou are now arriving at Dipoli.")
      if (!quizDone) println("Enter: 'quiz' to do the TEK - general knowledge quiz.")
    }
  }

  private def exam1() = {
    var ret = "Maths & physics exam is not available now."
    if ((player.location.name == "Undergraduate center") && (!examDone)) {
      var correctAnswers = 0

      println("\nMaths & physics exam is starting now, if you score lower than 40% you will fail the exam and it'll be gameover.")
      println("This is a multiple choice exam, so only answer: 'a', 'b' or 'c':\n")

      var str = readLine("1) What is the 4th digit of Pi?: 3.14?\na) 7\nb) 3\nc) 1\n")
      if (str == "c") correctAnswers = correctAnswers + 1
      println("Pi = 3.1415926... -> 4th digit = 1\n")

      str = readLine("2) Can light bend around corners?\na) Yes\nb) No\nc) Only on Jupiter\n")
      if (str == "a") correctAnswers = correctAnswers + 1
      println("This is a basic property of light and all other waves.\n")

      str = readLine("3) What is the 3rd number of fibonacci squence?\na) 8\nb) 5\nc) 1\n")
      if (str == "c") correctAnswers = correctAnswers + 1
      println("Fibonacci Sequence = 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ... -> 3rd number = 1\n")

      str = readLine("4) How do projectors project the color black?\na) With xenon light\nb) Projectors do not project the color black\nc) With UV light\n")
      if (str == "b") correctAnswers = correctAnswers + 1
      println("Black is really the absence of light, and you can't project something that does not exist.\n")

      str = readLine("5) What is e^(i*π)?\na) 1\nb) 0\nc) -1\n")
      if (str == "c") correctAnswers = correctAnswers + 1
      println("Euler's identity: e^(i*π) + 1 = 0\n")

      str = readLine("6) Can helicopters land even if the engine dies?\na) Yes\nb) No\nc) Only if their altitude is less than 8000 feet\n")
      if (str == "a") correctAnswers = correctAnswers + 1
      println("Yes, They accomplish this via autorotation of the main rotor blades.\n")

      str = readLine("7) In a room of just 23 people what's the chance that two people have the same birthday?\na) 50 Percent\nb) 30 Percent\nc) 10 Percent\n")
      if (str == "a") correctAnswers = correctAnswers + 1
      println("It's called the Birthday Problem, in a room of 75 there's a 99% chance of two people matching.\n")


      str = readLine("8) Does time go faster at the bottom of a building compared to the top?\na) Yes\nb) No\n")
      if (str == "b") correctAnswers = correctAnswers + 1
      println("Time goes faster the farther away you are from the earth's surface, so no it does not.\n")

      str = readLine("9) What is -40 °F?\na) 100 °C\nb) 0 °C\nc) -40 °C\n")
      if (str == "c") correctAnswers = correctAnswers + 1
      println("-40 °C is equal to -40 °F\n")

      str = readLine("10) Can Water boil and freeze at the same time?\na) Yes\nb) No\n")
      if (str == "a") correctAnswers = correctAnswers + 1
      println("Yes, this is called the 'triple point'.\n")

      println("Your points = " + correctAnswers + "/10")
      if (correctAnswers < 4) {
        println("You failed the exam.")
        this.player.quit()
      }
      else {
        println("You passed the exam!\nPure genius or just guesswork?")
      }
      ret = ""
      examDone = true
      this.game.exam1Complete = true
    }
    println(ret)
  }

  private def quiz2() = {
    var ret = "TEK(Tekniikan akateemiset) general knowledge quiz is not available now."
    if ((player.location.name == "Dipoli") && (!quizDone)) {
      var correctAnswers = 0

      println("\nGeneral knowledge quiz is starting now, if you score lower than 40% you will fail the exam and it'll be gameover.")
      println("This is a multiple choice exam, so only answer: 'a', 'b' or 'c':\n")

      var str = readLine("1) Who was the second human on moon?\na) Neil Armstrong\nb) Lance Armstrong\nc) Buzz Aldrin\n")
      if (str == "c") correctAnswers = correctAnswers + 1
      println("Buzz Aldrin was the second human ever to walk on the moon.\n")

      str = readLine("2) What is the source of the River Nile?\na) Lake Victoria\nb) Lake Malawi\nc) Lake Kariba\n")
      if (str == "a") correctAnswers = correctAnswers + 1
      println("The Nile starts in Jinja, Uganda, at the shore of Lake Victoria.\n")

      str = readLine("3) What the best selling album ever?\na) Thriller - Michael Jackson\nb) Rumours - Fleetwood Mac\nc) Back in Black - AC/DC\n")
      if (str == "a") correctAnswers = correctAnswers + 1
      println("Thriller, estimated to have sold 66 million copies worldwide, is the best-selling album.\n")

      str = readLine("4) Who has the most Olympic medals of all time?\na) Usain Bolt\nb) Michael Phelps\nc) Paavo Nurmi\n")
      if (str == "b") correctAnswers = correctAnswers + 1
      println("American swimmer Michael Phelps is the most decorated Olympian, having won 28 medals.\n")

      str = readLine("5) What is the most sold soft drink in the world?\na) Red Bull\nb) Coca-Cola\nc) Pepsi\n")
      if (str == "b") correctAnswers = correctAnswers + 1
      println("Coca-Cola is the most popular soft drink in the world.\n")

      str = readLine("6) In what year was the first iPhone released?\na) 2009\nb) 2008\nc) 2007\n")
      if (str == "c") correctAnswers = correctAnswers + 1
      println("Apple launched the first iPhone in January 2007.\n")

      str = readLine("7) Which of these is not a U.S. state?\na) Alberta\nb) Nebraska\nc) Arkansas\n")
      if (str == "a") correctAnswers = correctAnswers + 1
      println("Alberta is one of the thirteen provinces of Canada.\n")

      str = readLine("8) What is the capital of Australia?\na) Sydney\nb) Canberra\nc) Melbourne\n")
      if (str == "b") correctAnswers = correctAnswers + 1
      println("Canberra is the capital of Australia.\n")

      str = readLine("9)How many kilometers are in a mile?\na) 1.61 km\nb) 1.53 km\nc) 1.48 km\n")
      if (str == "a") correctAnswers = correctAnswers + 1
      println("1 mi = 1.6093 km\n")

      str = readLine("10) What is the most sold car ever?\na) Volkswagen Beetle\nb) Ford Model T\nc) Toyota Corolla\n")
      if (str == "c") correctAnswers = correctAnswers + 1
      println("There have been more than 46 million Toyota Corollas sold around the world.\n")

      println("Your points = " + correctAnswers + "/10")
      if (correctAnswers < 4) {
        println("You failed the quiz.")
        this.player.quit()
      }
      else {
        println("You passed the exam!\nSecret classroom superpowers?\nYou won a snackbar! (Use 'snack' to gain extra steps.)")
      }
      ret = ""
      quizDone = true
      this.game.quiz2Complete = true
    }
    println(ret)
  }

}
