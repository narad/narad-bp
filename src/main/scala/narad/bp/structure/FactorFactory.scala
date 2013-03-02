package narad.bp.structure

object FactorFactory {
	def createUnaryFactor(idx: Int, name: String, pot: Potential): UnaryFactor = {
		val pots = if (pot == Double.PositiveInfinity) {  
				Array[Potential](new Potential(0.0, "-%s".format(name), false), new Potential(1.0, name, pot.isCorrect))
		}
		else {
				Array[Potential](new Potential(1.0, "-%s".format(name), false), pot)
		}
		return new UnaryFactor(idx, name, normalize(pots))		
	}


	def createNandFactor(idx: Int, name: String, pot: Potential): NandFactor = {
		val pots = Array.ofDim[Potential](2,2)
		if (pot == Double.PositiveInfinity) {
			pots(0)(0) = new Potential(0.0, "n/a", false)
			pots(0)(1) = new Potential(0.0, "n/a", false)
			pots(1)(0) = new Potential(0.0, "n/a", false)
			pots(1)(1) = new Potential(1.0, pot.name, pot.isCorrect)
		}
		else {
			pots(0)(0) = new Potential(1.0, "n/a", false)
			pots(0)(1) = new Potential(1.0, "n/a", false)
			pots(1)(0) = new Potential(1.0, "n/a", false)
			pots(1)(1) = pot
		}
		return new NandFactor(idx, name, pots)
	}

// NEEDS VERIFICATION
	def createImpliesFactor(idx: Int, name: String, pot: Potential): ImpliesFactor = {
		val pots = Array.ofDim[Potential](2,2)
		if (pot == Double.PositiveInfinity) {
			pots(0)(0) = new Potential(0.0, "n/a", false)
			pots(0)(1) = new Potential(0.0, "n/a", false)
      pots(1)(0) = new Potential(1.0, pot.name, pot.isCorrect)
      pots(1)(1) = new Potential(0.0, "n/a", false)
		}
		else {
			pots(0)(0) = new Potential(1.0, "n/a", false)
			pots(0)(1) = new Potential(1.0, "n/a", false)
			pots(1)(0) = pot
      pots(1)(1) = new Potential(1.0, "n/a", false)
    }
		return new ImpliesFactor(idx, name, pots)
	}

  def createHardImpliesFactor(idx: Int, name: String): HardLogicFactor = {
    val pots = Array.ofDim[Potential](2,2)
      pots(0)(0) = new Potential(1.0, "n/a", false)
      pots(0)(1) = new Potential(1.0, "n/a", false)
      pots(1)(0) = new Potential(0.0, "n/a", false)
      pots(1)(1) = new Potential(1.0, "n/a", false)
    return new HardLogicFactor(idx, name, pots)
  }


	def createEPUFactor(idx: Int, name: String, arity: Int): EPUFactor = {
//		val pots = Array.ofDim[Potential](2, arity){ new Potential(0.0, "n/a", false) }
		val pots = Array.tabulate[Potential](2, arity){case(i,j) => new Potential(0.0, "n/a", false)}
		pots(0)(0).value = 1.0
		for (i <- 1 until arity) {
			pots(1)(i).value = 1.0
		}
		return new EPUFactor(idx, name, pots)
	}

	def resize(opots: Array[Potential], arity1: Int, arity2: Int): Array[Array[Potential]] = {
		Array.tabulate[Potential](arity1, arity2){case(i,j) => 
			val offset = (arity2 * i) + j 
			opots(offset)
		}
	}
	
	def normalize(pots: Array[Potential]): Array[Potential] = {
		val cpots = pots.clone
		val sum = pots.foldLeft(0.0)(_+_.value)
		cpots.foreach(_.value /= sum)
		cpots
	}
}

	/*

	// E Pluribus Unum factor: map binary to multinomial variables
	class EPUFactor : public Table2Factor {
	public:
	  EPUFactor(const string& name, Vertex v, const Graph& g) : Table2Factor(name) {
	    vector<int> varity(2);
	    adjacent_arity(v, g, varity);
	    if ( varity[0] != 2 ) error("arity of 1st variable != 2");
	    // REprintf("# %s arity = %d, %d\n", name.c_str(), varity[0], varity[1]);
	    pots_.resize(varity[0], varity[1]);
	    pots_ = 0;
	    pots_(0,0) = 1.0;
	    for ( int val = 1; val < varity[1]; ++val ) pots_(1,val) = 1.0;
	  }
	};

	*/