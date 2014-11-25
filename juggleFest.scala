import util.control.Breaks._
import java.io._

/* 
* Daniel Middleton, JuggleFest 2014
* Written in Scala, because I thought that would be interesting
* And the libraries are good
* 
* Algorithm goes through each juggler, attempts to assign them to preferred circuit
* If unable to assign to preferred circuit, assigns to a random circuit
* Repeat process until all jugglers have been assigned
*
* Not all of the code is super pretty
* String processing could certainly have been done in a more robust, generic way
* If we expected to run this on a variety of input files
* But that wasn't really the point, so I didn't worry about it too much
*/
object JuggleFest {
	var jugglersPerCircuit = 0;
	def main(args: Array[String]) {
		val contents = io.Source.fromFile(args(0)).mkString.split("\n\r");
		val numRegex = """.\:(\d+)""".r
		// Collect circuits and jugglers into maps
		val circuits = contents(0).split("\n").foldLeft(Map.empty[String, Circuit])(
			(map: Map[String, Circuit], str: String) => {
				val chopped = str.split(" ")
				val name = chopped(1)
				val coordination = chopped(2).drop(2).toInt
				val endurance = chopped(3).drop(2).toInt
				val pizzazzMatcher = numRegex.findAllIn(chopped(4))
				pizzazzMatcher.next
				val pizzazz = pizzazzMatcher.group(1).toInt
				map + (name -> new Circuit(name, coordination, endurance, pizzazz))
			}
		)

		// First result of this split is a blank string, so use tail to skip it
		val jugglers = contents(1).split("\n").tail.foldLeft(Map.empty[String, Juggler])(
			(map: Map[String, Juggler], str: String) => {
				val chopped = str.split(" ")
				val name = chopped(1)
				val coordination = chopped(2).drop(2).toInt
				val endurance = chopped(3).drop(2).toInt
				val pizzazz = chopped(4).drop(2).toInt
				val preferences: Array[Circuit] = chopped(5).split(",").map(str => (circuits get str.trim) match {
					case Some(circuit) => circuit
					case None => throw new Exception("Invalid Input: No such circuit: " + str)
				});

				map + (name -> new Juggler(name, coordination, endurance, pizzazz, preferences));
			}
		)

		jugglersPerCircuit = jugglers.size / circuits.size

		// Assign jugglers to circuits
		// This is where all the action happens - kind of ugly, so many brackets
		// Repeat process until all jugglers have been selected
		while (jugglers.exists(!_._2.selected)) {
			jugglers.filter(!_._2.selected).foreach { case(name, juggler) => 
				// Process to assign an individual juggler
				// Every time a juggler passes through here they will be placed in a circuit
				// They might get kicked out of that circuit soon after, but that's okay
				// They'll just come back through here again until they stop getting kicked out

				// Attempt to place them in their preferred circuits first
				var counter = 0
				while (counter < 10 && !juggler.selected) {
					addJugglerToCircuit(juggler, juggler.preferences(counter))
					counter = counter + 1
				}

				// Not everyone can get their preferred circuits
				// Some of the jugglers should find new lines of work. Maybe as mimes?
				// We put the rest of the jugglers into random circuits
				if (!juggler.selected) {
					breakable {
						circuits.foreach { case(name, circuit) =>
							addJugglerToCircuit(juggler, circuit)
							if (juggler.selected) {
								// We found a match! Move on
								break
							}
						}
					}
				}
			}
		}

		// Create the output file
		val writer = new PrintWriter(new File("output.txt"))
		var sb = new StringBuilder
		circuits.foreach { case (name, circuit) =>
			// Delegate string building to the toStrings of Circuit and Juggler
			sb.append(circuit.toString)
			sb.append("\n")
		}
		writer.write(sb.toString)
		writer.close
	}

	def addJugglerToCircuit(juggler: Juggler, circuit: Circuit) {
		// Only add if there is room or if the juggler is a better fit
		if (!circuit.isFull || juggler.isBetterFit(circuit)) {
			circuit addJuggler juggler
		}
	}
}

class Circuit(val name: String, val coordination: Int, val endurance: Int, val pizzazz: Int) {
	// Keep selections sorted - worst fit will be first
	var selections = List.empty[Juggler]

	def isFull: Boolean = selections.size >= JuggleFest.jugglersPerCircuit
	def minimumFit: Int = selections.head.fit(this)

	def addJuggler(juggler: Juggler) {
		if (isFull) {
			// If full, cut off the head to make room
			selections.head.selected = false
			selections = selections.tail
		}
		// Sort to ensure that the head is always the juggler with the least fit
		selections = (juggler :: selections).sortWith(comparator);
		juggler.selected = true
	}

	def comparator(jugglerOne: Juggler, jugglerTwo: Juggler): Boolean = {
		// I was going to add a compare method to Juggler to provide a natural ordering
		// And then use a SortedSet to handle ordering things for me
		// But, ordering of jugglers only makes sense in the context of a circuit, which Juggler doesn't have
		// So, we'll do it this way instead. Still not too bad
		(jugglerOne fit this) < (jugglerTwo fit this)
	}

	override def toString: String = {
		var sb = new StringBuilder(name).append(" ")
		sb.append(selections.mkString(", "))
		sb.toString
	}
}

class Juggler(val name: String, val coordination: Int, val endurance: Int, val pizzazz: Int, val preferences: Array[Circuit]) {
	var selected = false;

	def fit(circuit: Circuit): Int = {
		// dot product of this and the circuit's attributes
		// Parentheses aren't strictly necessary, but it's nice to be explicit
		(coordination * circuit.coordination) + 
		(endurance * circuit.endurance) + 
		(pizzazz * circuit.pizzazz)
	}

	def isBetterFit(circuit: Circuit): Boolean = fit(circuit) > circuit.minimumFit 

	override def toString: String = {
		var sb = new StringBuilder(name)
		preferences foreach { circuit =>
			sb.append(" ").append(circuit.name).append(":").append(fit(circuit))
		}
		sb.toString
	}
}

JuggleFest.main(args);