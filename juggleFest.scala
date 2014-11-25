import collection.immutable.SortedMap
import java.io._

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
		// Compute fit for each juggler and their preferences
		// Add everyone to the first preference

		while (jugglers.exists(!_._2.selected)) {
			jugglers.filter(!_._2.selected).foreach { case(name, juggler) => 
				var counter = 0
				while (counter < 10 && !juggler.selected) {
					addJugglerToCircuit(juggler, juggler.preferences(counter))
					counter = counter + 1
				}
			}
		}

		val writer = new PrintWriter(new File("output.txt"))
		var sb = new StringBuilder
		circuits.foreach { case (name, circuit) =>
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
	// Keep selections naturally sorted - worst fit will be first
	var selections = List.empty[Juggler]

	def isFull: Boolean = selections.size >= JuggleFest.jugglersPerCircuit

	def addJuggler(juggler: Juggler) {
		if (isFull) {
			selections.head.selected = false
			selections = selections.tail
		}
		selections = (juggler :: selections).sortWith(comparator);
		juggler.selected = true
	}

	def comparator(jugglerOne: Juggler, jugglerTwo: Juggler): Boolean = {
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
		(coordination * circuit.coordination) + 
		(endurance * circuit.endurance) + 
		(pizzazz * circuit.pizzazz)
	}

	def isBetterFit(circuit: Circuit): Boolean = 
		circuit.selections.exists(fit(circuit) > _.fit(circuit)) 

	override def toString: String = {
		var sb = new StringBuilder(name)
		preferences foreach { circuit =>
			sb.append(" ").append(circuit.name).append(":").append(fit(circuit))
		}
		sb.toString
	}
}

JuggleFest.main(args);