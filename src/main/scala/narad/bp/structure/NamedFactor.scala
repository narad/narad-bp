package narad.bp.structure

import scala.collection.mutable.ArrayBuffer



class Named1Factor(idx: Int, name: String, pots: Array[Potential]) extends Factor(idx, name) { 

  def arity = 1

	def computeMessages(fg: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
		val edge = fg.edgesFrom(this).toArray.head
		edge.f2v = dampen(edge.f2v, pots.map(_.value), damp)
		return 0.0
	}
	
	
	def getBeliefs(graph: FactorGraph): Array[Potential] = {
		val beliefs = elementMultiplication(graph.edgesFrom(this).toArray.head.v2f, pots)
		normalize(beliefs)
		return beliefs
	}
}

/*
class Named2Factor(idx: Int, name: String, pots: Array[Potential]) extends Factor(idx, name){

	def computeMessages(fg: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
		val edge = fg.edgesFrom(this).toArray.first
		edge.f2v = dampen(edge.f2v, pots.map(_.value), damp)
		return 0.0
	}
	
	
	def getBeliefs(graph: FactorGraph): Array[Potential] = {
		val beliefs = elementMultiplication(graph.edgesFrom(this).toArray.first.v2f, pots)
		normalize(beliefs)
		return beliefs
	}	
}
*/







/*
class Named2Factor : public Table2Factor {
public:
  Named2Factor(const string& name, Vertex v, const Graph& g, const s2dmap& pmap, double defpot)
    : Table2Factor(name) {
    // REprintf("# Named2Factor...\n");
    vector<int> varity(2);
    adjacent_arity(v, g, varity);
    // REprintf("# %s arity = %d, %d\n", name.c_str(), varity[0], varity[1]);
    pots_.resize(varity[0], varity[1]);
    char buf[BUFSIZ];
    const char *pattern = name.c_str();
    for ( int val1 = 0; val1 < varity[0]; ++val1 ) {
      for ( int val2 = 0; val2 < varity[1]; ++val2 ) {
	sprintf(buf, pattern, val1, val2);
	s2dmap::const_iterator p = pmap.find(buf);
	pots_(val1, val2) = (p == pmap.end() ? defpot : p->second);
      }
    }
  }
  virtual void append_beliefs(Vertex v, const Graph& g, s2dmap& pots) const {
    Array2 a(get_beliefs(v, g));
    vector<int> varity(2);
    adjacent_arity(v, g, varity);
    char buf[BUFSIZ];
    const char *pattern = name_.c_str();
    double Z = sum(a);
    for ( int val1 = 0; val1 < varity[0]; ++val1 ) {
      for ( int val2 = 0; val2 < varity[1]; ++val2 ) {
	sprintf(buf, pattern, val1, val2);
	pots[buf] = a(val1, val2) / Z;
      }
    }
  }
};
*/










		//	  System.err.println("beliefs size = " + beliefs.size)
//		return pots

					//		println("Compute UNARY FACTOR message!  Factor = %s".format(this.toString))
					//		println("Edge: %s".format(edge.toString))
			//		println("EDGE %s Message has been set: (%d) %s".format(edge, edge.f2v.size, edge.f2v.mkString(", ")))
			/// NOT A VALID IMPLEMENTATION
	//		val npots  = new ArrayBuffer[Potential]
	//		val message = graph.edgesFrom(this).toArray.first.v2f
	//		println(" v2f = " + v2f.mkString(", "))
	//		val sum = message.foldLeft(0.0)(_+_)
	//		System.err.println("message = " + graph.edgesFrom(this).toArray.first.v2f.mkString(", "))


//		for (i <- 0 until pots.size) {
//			npots += Tuple(names(i), scores(i))
//		}
//		return npots.toArray
//	}


/*
class Named1Factor : public Table1Factor {
public:
  // TODO: Need to implement pegging with infinite weights
  Named1Factor(const string& name, Vertex v, const Graph& g, const s2dmap& pmap, double defpot)
    : Table1Factor(name) {
    vector<int> varity(1);
    adjacent_arity(v, g, varity);
    pots_.resize(varity[0]);
    char buf[BUFSIZ];
    const char *pattern = name.c_str();
    for ( int val = 0; val < varity[0]; ++val ) {
      sprintf(buf, pattern, val);
      s2dmap::const_iterator p = pmap.find(buf);
      if ( (pots_(val) = (p == pmap.end() ? defpot : p->second)) == R_PosInf ) {
	pots_(val) = 1;
	for ( int other = 0; other < varity[0]; ++other ) {
	  if ( other != val ) {
	    pots_(other) = 0;
	  }
	}
	break;
      }
    }
  }
  virtual void append_beliefs(Vertex v, const Graph& g, s2dmap& pots) const {
    Array1 a(get_beliefs(v, g));
    vector<int> varity(1);
    adjacent_arity(v, g, varity);
    char buf[BUFSIZ];
    const char *pattern = name_.c_str();
    double Z = sum(a);
    for ( int val = 0; val < varity[0]; ++val ) {
      sprintf(buf, pattern, val);
      pots[buf] = a(val) / Z;
    }
  }
};
*/