package narad.bp.structure


class Table1Factor(idx: Int, name: String, pots: Array[Potential]) extends Factor(idx, name) { //}, new UnaryFactorPotential(pots)) {
  private val M = pots.size

  def arity = 1

  def computeMessages(fg: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
    if (verbose) println("Computing table1factor message for %s".format(name))
    val edge = fg.edgesFrom(this).toArray.head
    if (verbose) println("pots = " + pots.map(_.value).mkString(", "))
    edge.f2v = dampen(edge.f2v, pots.map(_.value), damp)
    0.0
  }

  def getBeliefs(graph: FactorGraph): Array[Potential] = {
    val beliefs = elementMultiplication(graph.edgesFrom(this).toArray.head.v2f, pots) //ip) //pots)
    normalize(beliefs)
    beliefs
  }

  override def clamp() = {
    pots.foreach {p => if (p.isCorrect) p.value = 1.0 else p.value = 0.0 }
  }

}


class Table2Factor(idx: Int, name: String, pots: Array[Array[Potential]]) extends Factor(idx, name) {
  private val M = pots.size
  private val N = pots(0).size

  def arity = 2

  def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
    if (verbose) System.err.println("DEBUG:  Computing message in " + name)
    val edges = graph.edgesFrom(this).toArray

    assert(edges(0).v2f.size == pots.size, "Error edge size %d does not equal pots matrix dim %d".format(edges(0).v2f.size, pots.size))
    assert(edges(1).v2f.size == pots(0).size, "Error edge size %d does not equal pots matrix dim %d".format(edges(1).v2f.size, pots(0).size))

    val tmp1 = Array.tabulate(M, N){case(i,j) => pots(i)(j).value}
    val tmp2 = Array.tabulate(M, N){case(i,j) => pots(i)(j).value}
    for (i <- 0 until M; j <- 0 until N) {
      tmp1(i)(j) = tmp1(i)(j) * edges(0).v2f(i)
      tmp2(i)(j) = tmp2(i)(j) * edges(1).v2f(j)
    }

    val mess1 = new Array[Double](M)
    val mess2 = new Array[Double](N)

    for (i <- 0 until pots.size; j <- 0 until pots(i).size) {
      mess1(i) = mess1(i) + tmp2(i)(j)
      mess2(j) = mess2(j) + tmp1(i)(j)
    }

    edges(0).f2v = dampen(edges(0).f2v, mess1, damp)
    edges(1).f2v = dampen(edges(1).f2v, mess2, damp)
    if (verbose) System.err.println("DEBUG: mess 1 = [" + mess1.mkString(", ") + "]; damp = " + damp)
    if (verbose) System.err.println("DEBUG: mess 2 = [" + mess2.mkString(", ") + "]; damp = " + damp)
    0.0
  }

  def getBeliefs(graph: FactorGraph): Array[Potential] = {
    val edges = graph.edgesFrom(this).toArray
    val beliefs = pots.clone()

    for (i <- 0 until M; j <- 0 until N) {
      beliefs(i)(j).value *= edges(0).v2f(i) * edges(1).v2f(j)
    }
    val f = beliefs.flatten
    normalize(f)
    f
  }

  override def clamp() = {
    pots.flatten.foreach {p => if (p.isCorrect) p.value = 1.0 else p.value = 0.0 }
  }
}

class Table3Factor(idx: Int, name: String, pots: Array[Array[Array[Potential]]]) extends Factor(idx, name) {
  private val M = pots.size
  private val N = pots(0).size
  private val P = pots(0)(0).size

  def arity = 3

  def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
    val edges = graph.edgesFrom(this).toArray
    val tmp1 = Array.tabulate(M, N, P){case(i,j,k) => pots(i)(j)(k).value} //pots.clone()
    val tmp2 = Array.tabulate(M, N, P){case(i,j,k) => pots(i)(j)(k).value}
    val tmp3 = Array.tabulate(M, N, P){case(i,j,k) => pots(i)(j)(k).value}
    for (i <- 0 until M; j <- 0 until N; k <- 0 until P) {
      tmp1(i)(j)(k) = tmp1(i)(j)(k) * edges(1).v2f(j) * edges(2).v2f(k)
      tmp2(i)(j)(k) = tmp2(i)(j)(k) * edges(0).v2f(i) * edges(2).v2f(k)
      tmp3(i)(j)(k) = tmp3(i)(j)(k) * edges(0).v2f(i) * edges(1).v2f(j)
    }

    val mess1 = new Array[Double](M)
    val mess2 = new Array[Double](N)
    val mess3 = new Array[Double](P)

    for (i <- 0 until M; j <- 0 until N; k <- 0 until P) {
      mess1(i) = mess1(i) + tmp1(i)(j)(k)
      mess2(j) = mess2(j) + tmp2(i)(j)(k)
      mess3(k) = mess3(k) + tmp3(i)(j)(k)
    }

    edges(0).f2v = dampen(edges(0).f2v, mess1, damp)
    edges(1).f2v = dampen(edges(1).f2v, mess2, damp)
    edges(2).f2v = dampen(edges(2).f2v, mess3, damp)
    0.0
  }


  def getBeliefs(graph: FactorGraph): Array[Potential] = {
    val edges = graph.edgesFrom(this).toArray
    val beliefs = pots.clone()
    for (i <- 0 until M; j <- 0 until N; k <- 0 until P) {
      beliefs(i)(j)(k).value *= edges(0).v2f(i) * edges(1).v2f(j) * edges(2).v2f(k)
    }
    val f = beliefs.map(_.flatten).flatten
    normalize(f)
    f
  }
}



























/*
class Table3Factor : public Factor {
public:
  Table3Factor(const string& name) : Factor(name) {}
  Table3Factor(const string& name, const double *data, int dim1, int dim2, int dim3) : Factor(name) {
    pots_.resize(dim1, dim2, dim3);
    pots_ = Array3(const_cast<double *>(data), shape(dim1, dim2, dim3), duplicateData, fortranArray);
  }
  virtual double compute_messages(Vertex v, Graph& g, double damp) {
    vector<Edge> dests(3);
    all_out_edges(v, g, dests);
    firstIndex i; secondIndex j; thirdIndex k;

    Array3 tmp(pots_.shape());
    dvec mess(pots_.extent(firstDim));

    tmp = pots_ * g[dests[1]].v2f(j) * g[dests[2]].v2f(k);
    mess = sum(sum(tmp, k), j);
    damp_assign(g[dests[0]].f2v, mess, damp);

    mess.resize(pots_.extent(secondDim));
    tmp = pots_ * g[dests[0]].v2f(i) * g[dests[2]].v2f(k);
    mess = sum(sum(tmp(j, i, k), k), j);
    damp_assign(g[dests[1]].f2v, mess, damp);

    mess.resize(pots_.extent(thirdDim));
    tmp = pots_ * g[dests[0]].v2f(i) * g[dests[1]].v2f(j);
    mess = sum(sum(tmp(k, j, i), k), j);
    damp_assign(g[dests[2]].f2v, mess, damp);

    return 0;
  }
  Array3 get_beliefs(Vertex v, const Graph& g) const {
    Array3 a(pots_.shape());
    vector<Edge> dests(3);
    all_out_edges(v, g, dests);
    firstIndex i; secondIndex j; thirdIndex k;
    a = pots_ * g[dests[0]].v2f(i) * g[dests[1]].v2f(j) * g[dests[2]].v2f(k);
    return a;
  }
  virtual double entropy(Vertex v, const Graph& g) {
    Array3 a(pots_.shape());
    vector<Edge> dests(3);
    all_out_edges(v, g, dests);
    firstIndex i; secondIndex j; thirdIndex k;
    a = pots_ * g[dests[0]].v2f(i) * g[dests[1]].v2f(j) * g[dests[2]].v2f(k);
    a /= sum(a);
    double res = -sum(zapnan(a * log(a)));
    res += sum(zapnan(a * log(pots_)));
    return res;
  }
protected:
  Array3 pots_;
};

 */


//		val a = Array.tabulate(2,2)(case(i,j) => edges(0).v2f(i) * edges(1).v2f(j))
/*
		var prod = 1.0
		for (i <- 0 until 2; j <- 0 until 2) {
			System.err.println("table2m i = %d; j = %d = %f * %f".format(i, j, edges(0).v2f(i), edges(1).v2f(j)))
			prod *= edges(0).v2f(i) * edges(1).v2f(j)
		}
*/
//		System.err.println("prod = " + prod)
/*
		val beliefs = pots.flatten
		System.err.println("Pots = " + beliefs.mkString("\n"))
		beliefs.foreach(_.value *= prod)
		System.err.println("Pots After = " + beliefs.mkString("\n"))
		return beliefs
	}
*/

/*
  virtual void append_beliefs(Vertex v, const Graph& g, s2dmap& pots) const {
    Array2 a(get_beliefs(v, g));
    pots[name_] = a(1,0) / sum(a);
  }
*/













//		System.err.println("var1 (%d) = %s".format(var1.arity, edges(0).v2f.size))
//		System.err.println("var2 (%d) = %s".format(var2.arity, edges(1).v2f.size))

//		System.err.println("pots = (%d x %d)".format(pots.size, pots(0).size))
//		for (i <- 0 until pots.size) {
//			System.err.println("i = %d: %s".format(i, pots(i).map(_.name).mkString("\t|\t")))
//		}
//		System.err.println
//		val edges = graph.edgesFrom(this).toArray
//			System.err.println("edge 0 goes to " + edges(0).variable.name + " with message " + edges(0).v2f.mkString(", "))
//			System.err.println("edge 1 goes to " + edges(1).variable.name + " with message " + edges(1).v2f.mkString(", "))
//			val m1 = mDown(Array(0.2, 0.8), pots)

//println
//println("In = ")
//println(edges(0).v2f.mkString(", "))
//println("multiplied across = ")
//printMatrix(m2)
//println("summed down = ")
//println(sm2.mkString(", "))
//System.err.println("Arity of variable %s is %s;  Message to variable is [%s].".format(edges(1).variable.name, edges(1).variable.arity, sm2.mkString(", ")))

//println("In = ")
//println(edges(1).v2f.mkString(", "))

/*
      println("multiplied down = ")
      printMatrix(m1)
      println("summed across = ")
      println(sm1.mkString(", "))
      println("multed down = %d x %d".format(m1.size, m1(0).size))
      System.err.println("Arity of variable %s is %s;  Message to variable is [%s].".format(edges(0).variable.name, edges(0).variable.arity, sm1.mkString(", ")))
*/


/*
    val edges = fg.edgesFrom(this).toArray
    assert(edges.size == 2, "Binary Factor is improperly configured: has %d neighbors.".format(edges.size))

    val m1 = multiplyAcross(edges(0).v2f, pots)
    edges(1).f2v = dampen(edges(1).f2v, sumDown(m1), damp)

    val m2 = multiplyDown(edges(1).v2f, pots)
    edges(0).f2v = dampen(edges(0).f2v, sumAcross(m1), damp)
    return 0.0
  }
*/



/*
class Table2Factor(idx: Int, name: String, pots: Array[Array[Potential]]) extends Factor(idx, name) {

  def arity = 2

	def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
		if (verbose) System.err.println("DEBUG:  Computing message in " + name)
		val edges = graph.edgesFrom(this).toArray
//		System.err.println("Edges = " + edges)

    for (i <- 0 until pots.size; j <- 0 until pots(i).size){
      if (verbose) System.err.println("DEBUG:  POTS[%d,%d] = ".format(i, j) + pots(i)(j))
    }
    /*
    if (verbose) System.err.println("DEBUG: table2m[0,0] = " + edges(0).v2f(0) + " * " + edges(1).v2f(0))
    if (verbose) System.err.println("DEBUG: table2m[0,1] = " + edges(0).v2f(0) + " * " + edges(1).v2f(1))
    if (verbose) System.err.println("DEBUG: table2m[1,0] = " + edges(0).v2f(1) + " * " + edges(1).v2f(0))
    if (verbose) System.err.println("DEBUG: table2m[1,1] = " + edges(0).v2f(1) + " * " + edges(1).v2f(1))

    Array2 tmp(pots_.shape());
    dvec mess(pots_.extent(firstDim));

    tmp = pots_ * g[dests[1]].v2f(j);
	cout << "DEBUG:  TEMP1 = " << tmp << " " << endl;
    mess = sum(tmp, j);
	cout << "DEBUG: SUM 1 = " << mess << endl;
    mess /= sum(mess);
	cout << "DEBUG: MESS 1 = " << mess << "; damp = " << damp << endl;
    damp_assign(g[dests[0]].f2v, mess, damp);

    mess.resize(pots_.extent(secondDim));
    tmp = pots_ * g[dests[0]].v2f(i);
	cout << "DEBUG:  TEMP2 = " << tmp << " " << endl;
    mess = sum(tmp(j, i), j);
	cout << "DEBUG: SUM 1 = " << mess << endl;
    mess /= sum(mess);
    damp_assign(g[dests[1]].f2v, mess, damp);
	cout << "DEBUG: MESS 2 = " << mess << "; damp = " << damp << endl;
    return 0;
         */

    val var1 = edges(0).variable
		val var2 = edges(1).variable
    if (verbose) System.err.println("DEBUG:  Message in :" + edges(0).v2f.mkString(", "))
		val m2 = mDown(edges(0).v2f, pots)
    for (i <- 0 until m2.size; j <- 0 until m2(i).size){
      if (verbose) System.err.println("DEBUG:  TMP[%d,%d] = ".format(i, j) + m2(i)(j))
    }
    val sm2 = norm(sAcross(m2))
		edges(1).f2v = dampen(edges(1).f2v, sm2, damp)

    if (verbose) System.err.println("----------------")
    if (verbose) System.err.println("DEBUG:  Message in :" + edges(1).v2f.mkString(", "))
    val m1 = mAcross(edges(1).v2f, pots)
    for (i <- 0 until m1.size; j <- 0 until m1(i).size){
      if (verbose) System.err.println("DEBUG:  TMP[%d,%d] = ".format(i, j) + m1(i)(j))
    }
    val sm1 = norm(sDown(m1))
		edges(0).f2v = dampen(edges(0).f2v, sm1, damp)
    if (verbose) System.err.println("DEBUG: mess 1 = [" + sm1.mkString(", ") + "]; damp = " + damp)
    if (verbose) System.err.println("DEBUG: mess 2 = [" + sm2.mkString(", ") + "]; damp = " + damp)
		return 0.0
	}

 */
