package o1.adventure

import scala.util.Random


/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure {

  /** The title of the game. */
  val title = "A day in a life of an Aalto-student."

  private val metro      = new Area("Metro", "The metro station of Aalto University.\nYou can take a metro home when all the tasks are completed.")
  private val aBlock     = new Area("A-Block", "You see many restaurants here.\nThere are also a lot of people here.")
  private val arts       = new Area("Arts", "A fancy big building.\nYou will find creative people here.")
  private val csBuilding = new Area("CS-Building", "Big screens and people walking with laptops in their hands everywhere.")
  private val nanotalo   = new Area("Nanotalo", "This laboratory is one of the leading low temperature and nanoeletronics research environments in the world.")
  private val ugCenter   = new Area("Undergraduate center", "Bachelor level courses, projects and events are held here.")
  private val library    = new Area("Library", "People come here to learn something new.\nMaybe you have learnt something new today?")
  private val chem       = new Area("CHEM building", "Here you will get basic knowledge of chemistry and biochemistry.")
  private val dipoli     = new Area("Dipoli", "This building uses materials extensively from Finnish nature, such as pinewood, copper and natural rocks.")
  private val sPark      = new Area("Sports Park", "You can find Polyteknikkojen Raittiusseura holding an event here.\nRemember to buy their badge (if you haven't already).")
  private val destination = metro


       metro.setNeighbors(Vector("north" -> aBlock,     "east" -> chem                                               ))
      aBlock.setNeighbors(Vector("north" -> arts,                           "south" -> metro                         ))
        arts.setNeighbors(Vector("north" -> csBuilding,                     "south" -> aBlock                        ))
  csBuilding.setNeighbors(Vector(                       "east" -> nanotalo, "south" -> arts                          ))
    nanotalo.setNeighbors(Vector(                       "east" -> ugCenter,                      "west" -> csBuilding))
    ugCenter.setNeighbors(Vector(                                           "south" -> library,  "west" -> nanotalo  ))
     library.setNeighbors(Vector("north" -> ugCenter,   "east" -> dipoli,   "south" -> chem                          ))
        chem.setNeighbors(Vector("north" -> library,                                             "west" -> metro     ))
      dipoli.setNeighbors(Vector(                       "east" -> sPark,                         "west" -> library   ))
       sPark.setNeighbors(Vector(                                                                "west" -> dipoli    ))


  private var badge = new Item("badge", "a PoRa-badge for your boiler suit (haalari)")
  sPark.addItem(badge)


  /** The character that the player controls in the game. */
  val player = new Player(metro)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of steps that this adventure game allows before player run out of energy. */
  var timeLimit = 10

  //steals the badge from student and adds it to random location
  def stealBadge() = {
    var places = Vector[Area](aBlock, arts, nanotalo, ugCenter, chem)
    var place = places(Random.nextInt(5))
    player.remove("badge")
    place.addItem(badge)
  }

  //these are needed to check the complete method and their values are conttrolled in file AdventureTextUI
  var exam1Complete = false
  var quiz2Complete = false

  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.player.location == this.destination && !{player.inventory.contains("book")} && player.inventory.contains(badge.name) && exam1Complete && quiz2Complete

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = {
    "\nYour objective is to complete a day as an Aalto-student\nThere are things you must do to win the game:\n\n    " +
    "- You need to attend and pass the maths/physics exam and\n      TEK general knowledge quiz. There will be 10 questions in each of the\n      " +
    "exams and you need to get minimum 40 % (4/10) in each one to pass.\n      If you fail one of these exams, the game ends immediately.\n      " +
    "The maths/physics exam will be held at the Undergraduate Center and\n      the TEK quiz at Dipoli.\n\n    " +
    "- You are a student so you need to eat something. You need to go to\n      the A-Block and have a meal (+7 more steps).\n      " +
    "This task is not necessary but you will most likely end up dying without eating.\n      You can also win a snackbar that may help you further.\n\n    " +
    "- Visit a sports event held by Polyteknikkojen Raittiusseura in the\n      sportspark. Remember to buy a PoRa-badge for your boiler suit (haalari).\n\n    " +
    "- Return a book 'Discrete Mathematics' to the library.\n\n    " +
    "- A day ends by going to the home, take a metro (when you have\n      completed all the task above) and go home." +
    "\n\n" +
    "At the beginning of a game you will have the energy for 10 steps (one step is moving in a direction). If you run out of these steps the " +
    "game\nwill end because your energy level was too low. Eat a meal to gain 7 steps more or win a snackbar from the quiz to get +4 steps (both can be done only once).\n\nYou can do the tasks " +
    "above in any order you like, but the last task (metro to home) needs to be done last.\n\nYou will most likely encounter a student from " +
    "another university who will try to steal your badge (the one you will buy from the sports event).\nThis student will steal your badge and " +
    "drop it in a random location. You will need to find the badge and pick it up before returning home.\n\n" +
    "You can get more information by typing 'help'.\n\n" +
    "–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––\n" +
    "                    A DAY IN A LIFE OF AALTO-STUDENT\n" +
    "             Complete all the tasks with in time to win the game.\n" +
    "                    Let's see if you can beat the game.\n" +
    "                                Good luck!\n" +
    "–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––\n"
  }

  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "Finally, it's time to go home!\n*You hear the metro arriving and hurry to it.*\nYOU WIN THE GAME!"
    else if (this.turnCount == this.timeLimit)
      "Oh no! You die of starvation! Keep an eye on your energy levels next time.\nGame over!"
    else  // game over due to player quitting
      "Better luck next time!"
  }


  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    var outcomeReport = action.execute(this.player)
    if (outcomeReport.isDefined) {
      this.turnCount += 1
    }
    outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
  }

}

