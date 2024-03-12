package o1.adventure

import scala.collection.mutable.Map


/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private var book = new Item("book", "Discrete Mathematics by Kenneth Rosen")
  private var itemInventory = Map[String, Item](book.name -> book)
  private var exam1Done = false
  private var quiz2Done = false
  private var thief = false
  var lunch = false


  def exam1() = {                                                          // Maths & physics exam
    var ret = ""
    if (!exam1Done && currentLocation.name == "Undergraduate center") {
      ret = "Hope you have learnt something new today!"
      exam1Done = true
    }
    ret
  }

  def quiz2() = {                                                          // TEK - quiz
    var ret = ""
    if (!quiz2Done && currentLocation.name == "Dipoli") {
      ret = "'Knowledge without reflection is a waste of time, reflection without knowledge is dangerous'."
      quiz2Done = true
      itemInventory += "snack" -> new Item("snack", "Mars Chocolate Bar, 230 kcal of energy")
    }
    ret
  }

  def returnBook(): String = {
    if (this.currentLocation.name == "Library" && this.has(book.name)) {
      this.itemInventory.remove(book.name)
      "You have successfully returned your book in time."
    } else if (!{this.has(book.name)}) {
      "You don't have anything to return."
    } else { "You are not in the library right now."  }
  }

  // when thief is available it's added to library location
  def thiefAvailable = this.thief

  def lunchEaten =  {
    ""
  }
  def snackEaten =  {
    ""
  }


  def remove(itemName: String): String = {
    var ret = ""
    var x = this.itemInventory.remove(itemName)
    if (x.isDefined) ret = "You are not carrying " + itemName + "anymore."
    ret
  }




  def get(itemName: String): String = {
    var ret = "There is no " + itemName + " here to pick up."
    var x = currentLocation.removeItem(itemName)
    if (x.isDefined) {
      ret = "You pick up the " + itemName + "."
      itemInventory += x.get.name -> x.get
      if ((itemName == "badge") && (!thief)) {
        thief = true
        ret = ret + "\nYou have a shiny new PoRa-badge. Keep an eye on it, someone else might want it too!"
      }
    }
    ret
  }

  def drop(itemName: String): String = {
    var ret = "You don't have that!"
    var x = this.itemInventory.remove(itemName)
    if (x.isDefined) {
      this.currentLocation.addItem(x.get)
      ret = "You drop the " + itemName + "."
      if ((itemName == "badge") && (thief)) {
        thief = false
      }
    }
    ret
  }

  def examine(itemName: String): String = {
    var ret = "If you want to examine something, you need to pick it up first."
    var x = itemInventory.get(itemName)
    if (x.isDefined) ret = "You look closely at the " + itemName + ".\n" + x.get.description
    ret
  }

  def inventory: String = {
    var ret = "You are empty-handed."
    if (itemInventory.nonEmpty) {
      ret = "You are carrying:"
      for (i <- itemInventory) {
        ret = ret + "\n" + i._1
      }
    }
    ret
  }

  def has(itemName: String): Boolean = {
    var ret = false
    if (itemInventory.contains(itemName)) ret = true
    ret
  }


  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the current location of the player. */
  def location = this.currentLocation


  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) = {
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if (destination.isDefined) {
      "You go " + direction + "."
    } else {
      "You can't go " + direction + "."
    }
  }



  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() = {
    "You rest for a while. Better get a move on, though."
  }


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }


  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

  def helpCommand = {
    "---------------------------\n" +
    "Use these commands to:\n" +
    "'go'         => to move in direction, 'go north' moves player north\n" +
    "'rest'       => Causes the player to rest for a short while\n" +
    "'quit'       => Ends the game\n" +
    "'get'        => Tries to pick up an item of the given name\n" +
    "'drop'       => Tries to drop an item of the given name\n" +
    "'examine'    => Causes the player to examine the item of the given name\n" +
    "'inventory'  => Causes the player to list what's in their backpack\n" +
    "'to-do'      => Prints a to-do-list\n" +
    "'returnbook' => When in the library, returns your Discrete Mathematics book\n" +
    "'spagu'      => Is it Wednesday yet?\n" +
    "'lunch'      => When in A-Block, player eats lunch\n" +
    "'snack'      => Eats a snack if snack is available\n" +
    "'exam/quiz'  => When in the correct area, attempts the corresponding exam/quiz\n" +
    "'help'       => Help-command\n" +
    "---------------------------"
  }

  def printToDoList = {
    var ret = ""
    ret = ret + "\nTasks you need to do to win the game: \n"
    ret = ret + "- Pass the maths & physics exam\n"
    ret = ret + "- Pass the TEK (Tekniikan akateemiset) general knowledge quiz\n"
    ret = ret + "- Get the PoRa-badge\n"
    ret = ret + "- Return a book 'Discrete Mathematics' to the library\n"
    ret = ret + "----------------------------\n"
    ret = ret + "Tasks you have have completed: \n"
    if (exam1Done) ret = ret + "- Passed the maths & physics exam\n"
    if (quiz2Done) ret = ret + "- Passed the TEK general knowledge exam\n"
    if (has("badge")) ret = ret + "- Collected the PoRa-badge\n"
    if (!{has(book.name)}) ret = ret + "- Returned the book to the library\n"
    if (!exam1Done && !quiz2Done && has(book.name) && !has("badge")) ret = ret + "- None\n"

    ret
  }



}


