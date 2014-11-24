import collection.immutable.SortedMap

object JuggleFest {
	var jugglersPerCircuit = 0;
	def main(args: Array[String]) {
		val contents = io.Source.fromFile(args(0)).mkString.split("\n\n");
		
		// Collect circuits and jugglers into maps
		val circuitsString = contents(0).split("\n");
		val circuits = circuitsString.foldLeft(Map.empty[String, Circuit])(
			(map: Map[String, Circuit], str: String) => {
				val chopped = str.split(" ")
				val name = chopped(1)
				val coordination = chopped(2).toInt
				val endurance = chopped(3).toInt
				val pizzazz = chopped(4).toInt
				map + (name -> new Circuit(name, coordination, endurance, pizzazz))
			}
		)

		val jugglers = contents(0).split("\n").foldLeft(Map.empty[String, Juggler])(
			(map: Map[String, Juggler], str: String) => {
				val chopped = str.split(" ")
				val name = chopped(1)
				val coordination = chopped(2).toInt
				val endurance = chopped(3).toInt
				val pizzazz = chopped(4).toInt
				val preferences: Array[Circuit] = chopped(5).split(",").map(str => (circuits get str) match {
					case Some(circuit) => circuit
					case None => throw new Exception("Invalid Input: No such circuit: " + str)
				});

				map + (name -> new Juggler(name, coordination, endurance, pizzazz, preferences));
			}
		)
			


		jugglersPerCircuit = jugglers.size / circuits.size
		println(jugglersPerCircuit)

		// Compute fit for each juggler and their preferences


		def parseCircuit(map: Map[String, Circuit], str: String): Map[String, Circuit] = {
			val chopped = str.split(" ")
			val name = chopped(1)
			val coordination = chopped(2).toInt
			val endurance = chopped(3).toInt
			val pizzazz = chopped(4).toInt
			map + (name -> new Circuit(name, coordination, endurance, pizzazz))
		}

		def parseJuggler(map: Map[String, Juggler], str: String): Map[String, Juggler] = {
			val chopped = str.split(" ")
			val name = chopped(1)
			val coordination = chopped(2).toInt
			val endurance = chopped(3).toInt
			val pizzazz = chopped(4).toInt
			val preferences: Array[Circuit] = chopped(5).split(",").map(str => (circuits get str) match {
				case Some(circuit) => circuit
				case None => throw new Exception("Invalid Input: No such circuit: " + str)
			});

			map + (name -> new Juggler(name, coordination, endurance, pizzazz, preferences));
		}
	}
}

class Circuit(name: String, coordination: Int, endurance: Int, pizzazz: Int) {
	// Keep selections naturally sorted - worst fit will be first
	var selections = SortedMap.empty[Int, Juggler];

	def addJuggler(juggler: Juggler) {
		if (selections.size == JuggleFest.jugglersPerCircuit) {
			selections = selections - selections.firstKey;
		}

		selections = selections + (juggler.fit(this) -> juggler);
	}
}

class Juggler(name: String, coordination: Int, endurance: Int, pizzazz: Int, preferences: Array[Circuit]) {
	var selected = false;

	def fit(circuit: Circuit): Int = {
		// dot product of this and the circuit's attributes
		println(circuit);
		0
		/*(coordination * circuit.coordination) + 
		(endurance * circuit.endurance) + 
		(pizzazz * circuit.pizzazz)*/
	}

	def isBetterFit(circuit: Circuit): Boolean = 
		circuit.selections.exists(fit(circuit) > _._2.fit(circuit)) 
}