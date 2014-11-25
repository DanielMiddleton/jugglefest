import collection.immutable.SortedMap
import java.io._

object JuggleFest {
	var jugglersPerCircuit = 0;
	def main(args: Array[String]) {
		val contents = io.Source.fromFile(args(0)).mkString.split("\n\r");
		// Collect circuits and jugglers into maps
		val circuits = contents(0).split("\n").foldLeft(Map.empty[String, Circuit])(
			(map: Map[String, Circuit], str: String) => {
				val chopped = str.split(" ")
				val name = chopped(1)
				val coordination = chopped(2)(2).asDigit
				val endurance = chopped(3)(2).asDigit
				val pizzazz = chopped(4)(2).asDigit
				map + (name -> new Circuit(name, coordination, endurance, pizzazz))
			}
		)

		// First result of this split is a blank string, so use tail to skip it
		val jugglers = contents(1).split("\n").tail.foldLeft(Map.empty[String, Juggler])(
			(map: Map[String, Juggler], str: String) => {
				val chopped = str.split(" ")
				val name = chopped(1)
				val coordination = chopped(2)(2).asDigit
				val endurance = chopped(3)(2).asDigit
				val pizzazz = chopped(4)(2).asDigit
				val preferences: Array[Circuit] = chopped(5).split(",").map(str => (circuits get str.trim) match {
					case Some(circuit) => circuit
					case None => throw new Exception("Invalid Input: No such circuit: " + str)
				});

				map + (name -> new Juggler(name, coordination, endurance, pizzazz, preferences));
			}
		)

		jugglersPerCircuit = jugglers.size / circuits.size
		println(jugglersPerCircuit);
		// Compute fit for each juggler and their preferences
		// Add everyone to the first preference

		var counter = 0;
		while (jugglers.exists(!_._2.selected)) {
			jugglers.filter(!_._2.selected).foreach { case(name, juggler) => 
				addJugglerToCircuit(juggler, juggler.preferences(counter))
			}
			counter = counter + 1
		}

		println(jugglers.filter(!_._2.selected).size)
	}

	def addJugglerToCircuit(juggler: Juggler, circuit: Circuit) {
		// Only add if there is room or if the juggler is a better fit
		if (!circuit.isFull) {
			circuit addJuggler juggler
		} else if (juggler isBetterFit circuit) {
			circuit removeJuggler (circuit.selections firstKey)
			circuit addJuggler juggler
		}
	}
}

class Circuit(val name: String, val coordination: Int, val endurance: Int, val pizzazz: Int) {
	// Keep selections naturally sorted - worst fit will be first
	var selections = SortedMap.empty[Int, Juggler];

	def isFull: Boolean = selections.size >= JuggleFest.jugglersPerCircuit

	def addJuggler(juggler: Juggler) {
		selections = selections + (juggler.fit(this) -> juggler);
		juggler.selected = true
	}

	def removeJuggler(fit: Int) {
		val juggler = selections get fit match {
			case Some(juggler) => juggler
			case None => throw new Exception("No such juggler: " + fit)
		}

		selections = selections - fit
		juggler.selected = false
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
		circuit.selections.exists(fit(circuit) > _._2.fit(circuit)) 
}

JuggleFest.main(args);