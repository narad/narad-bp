package narad.bp.structure
import scala.collection.mutable.ArrayBuffer
import scala.util.matching._

class SegmentationFactor(idx: Int, name: String, slen: Int, maxSeg: Int) extends Factor(idx, name) {
	val indicesPattern = new Regex(".*\\(([0-9]+)\\,([0-9]+)\\)")

  def arity = slen

	def computeMessages(graph: FactorGraph, damp: Double, verbose: Boolean = false): Double = {
		if (verbose)		println("COMPUTING SEGMENTATION MESSAGE!")
		val pegs = new ArrayBuffer[(Int,Int)]
		val alpha = Array.ofDim[Double](slen+1)
		val beta  = Array.ofDim[Double](slen+1)
		val score = Array.ofDim[Double](slen+1, slen+1)
		val grad  = Array.ofDim[Double](slen+1, slen+1)
		if (verbose) System.err.println("QUEUE slen = " + slen)
		for (edge <- graph.edgesFrom(this)) {
			edge.variable.name match {
				case indicesPattern(start, end) => {					
					val i = start.toInt
					val k = end.toInt
					val w = k-i
					if (w <= maxSeg) {
						val m = edge.v2f
						score(i)(w) = Math.log(m(1)) - Math.log(m(0))
						if (verbose) System.err.println("QUEUE SCORE i = " + i + "; w = " + w + "; = " + score(i)(w))
						if (m(0) == 0) {
							if (verbose) System.err.println("pushing")
							score(i)(w) = 0
							pegs += Tuple(i, w)
						}
						grad(i)(w) = Double.NegativeInfinity							
					}
				}
				case _=>
			}
		}

		for (peg <- pegs) {
			val start = peg._1
			val end = start + peg._2
			for (i <- 0 until start; j <- start+1 until end if (j-i) <= maxSeg) score(i)(j-i) = Double.NegativeInfinity
			for (i <- start+1 until end; j <- end+1 to slen if (j-i) <= maxSeg) score(i)(j) = Double.NegativeInfinity
		}
		for (i <- 0 until score.size) {
			if (verbose) System.err.println("score: " + score(i).mkString("\t"))
		}

		alpha(0) = 0.0
		for (k <- 1 to slen) {
			alpha(k) = Double.NegativeInfinity
			for (w <- 1 to maxSeg if w <= k) {
				val i = k-w
				alpha(k) = logIncrement(alpha(k), alpha(i) + score(i)(w))
				if (verbose) System.err.println("QUEUE alpha(%d) = ".format(k) + alpha(k))
			}
		}

		beta(slen) = -1.0 * alpha(slen)
		for (i <- slen-1 to 0 by -1) {
			beta(i) = Double.NegativeInfinity
			for (w <- min(slen-i, maxSeg) to 1 by -1) {
				val k = i + w
				if (verbose) System.err.println("QUEUE beta k where k = " + k + " is = " + beta(k))
				if (verbose) System.err.println("QUEUE score iw where i = " + i + " and w = " + w + " is = " + score(i)(w))
				if (verbose) System.err.println("QUEUE alpha(%d) = ".format(i) + alpha(i))
//				if (verbose) System.err.println("QUEUE beta(%d) = ".format(i) + beta(k))
				beta(i)    = logIncrement(beta(i), beta(k) + score(i)(w))  
				if (verbose) System.err.println("QUEUE beta(%d) = ".format(i) + beta(i))
				grad(i)(w) = logIncrement(grad(i)(w), beta(k) + alpha(i))
				if (verbose) System.err.println("QUEUE grad(%d)(%d) = ".format(i, w) + grad(i)(w))
			}
		}

		for (edge <- graph.edgesFrom(this)) {
			edge.variable.name match {
				case indicesPattern(start, end) => {
					val i = start.toInt
					val k = end.toInt
					val w = k-i
					if (w <= slen) {
//						for (i <- 0 to (slen-w)) {
							val m = Array[Double](1-Math.exp(score(i)(w) + grad(i)(w)), Math.exp(grad(i)(w)))
//								truncate(1-Math.exp(score(i)(w) + grad(i)(w))), 
//								truncate(Math.exp(grad(i)(w))))
							if (score(i)(w) == Double.NegativeInfinity) m(1) = 0
							if (verbose) System.err.println("QUEUE mess(%d)(%d) = [%s]".format(i,w, m.mkString(", ")))
							edge.f2v = dampen(edge.f2v, m, damp)										
//						}
					}
				}
				case _ => System.err.println("ERROR IN CKY FACTOR - CONNECTED VAR (%score) DOES NOT MATCH PATTERN!".format(edge.variable.name))
			}
		}
		return 0
	}
	
	def getBeliefs(graph: FactorGraph): Array[Potential] = {
		return Array[Potential]()
	}
	
	def min(a: Int, b: Int): Int = {
		if (a < b) a else b
	}
}





































/*

class SegFactor : public Factor {
public:
SegFactor(const string& name, int slen, int maxSeg)
: Factor(name), slen_(slen), maxSeg_(maxSeg),
alpha(slen+1), beta(slen+1),
score(extents[slen+1][maxSeg+1]),
grad(extents[slen+1][maxSeg+1]) {}
virtual ~SegFactor() {}

virtual double compute_messages(Vertex v, Graph& g, double damp) {
EdgeIterator e = out_edges(v, g).first;
vector< pair<int, int> > pegs;
for ( int w = 1; w <= maxSeg_ && w <= slen_; ++w ) {
for ( int i = 0; i <= (slen_ - w); ++i ) {
const dvec& m = g[*e++].v2f;
score[i][w] = log(m(1)) - log(m(0));
if ( m(0) == 0 ) {
score[i][w] = 0;
pegs.push_back( make_pair(i, w) );
}
grad[i][w] = R_NegInf;
// REprintf("# in(%d,%d) = %f\n", i, w, score[i][w]);
}
}



for ( vector< pair<int, int> >::const_iterator it = pegs.begin();
it != pegs.end(); ++it ) {
int start = it->first;
int end = start + it->second;
// What kind of a HACK is this???
// Too late for arithmetic...
for ( int i = 0; i < start; ++i ) {
for ( int j = start + 1; j < end; ++j ) {
if ( (j-i) <= maxSeg_ ) score[i][j-i] = R_NegInf;
}
}
for ( int i = start + 1; i < end; ++i ) {
for ( int j = end + 1; j <= slen_; ++j ) {
if ( (j-i) <= maxSeg_ ) score[i][j-i] = R_NegInf;
}
}
}




alpha[0] = 0;
for ( int k = 1; k <= slen_; ++k ) {
alpha[k] = R_NegInf;
for ( int w = 1; w <= maxSeg_ && w <= k; ++w ) {
int i = k - w;
log_incr(alpha[k], alpha[i] + score[i][w]);
}
}


beta[slen_] = -alpha[slen_];
for ( int i = slen_ - 1; i >= 0; --i ) {
beta[i] = R_NegInf;
for ( int w = min(slen_ - i, maxSeg_); w >= 1; --w ) {
int k = i + w;
// REprintf("# [%d,%d] (%d) beta(%d) = %f\n", i, k, w, k, beta[k]);
log_incr(beta[i], beta[k] + score[i][w]);
log_incr(grad[i][w], beta[k] + alpha[i]);
}
}



e = out_edges(v, g).first;
for ( int w = 1; w <= maxSeg_ && w <= slen_; ++w ) {
for ( int i = 0; i <= (slen_ - w); ++i ) {
dvec m(2);
m(1) = exp(grad[i][w]);
m(0) = 1 - exp(score[i][w] + grad[i][w]);
if ( score[i][w] == R_NegInf ) m(1) = 0;
// REprintf("# out(%d,%d) = %f / %f\n", i, w, m(1), m(0));
damp_assign(g[*e++].f2v, m, damp);
}
}
return 0;
}












virtual double entropy(Vertex v, const Graph& g) {
EdgeIterator e = out_edges(v, g).first;
for ( int w = 1; w <= maxSeg_ && w <= slen_; ++w ) {
for ( int i = 0; i <= (slen_ - w); ++i ) {
const dvec& m = g[*e++].v2f;
score[i][w] = log(m(1)) - log(m(0));
grad[i][w] = R_NegInf;
// REprintf("# in(%d,%d) = %f\n", i, w, score[i][w]);
}
}
alpha[0] = 0;
for ( int k = 1; k <= slen_; ++k ) {
alpha[k] = R_NegInf;
for ( int w = 1; w <= maxSeg_ && w <= k; ++w ) {
int i = k - w;
log_incr(alpha[k], alpha[i] + score[i][w]);
}
}
beta[slen_] = -alpha[slen_];
for ( int i = slen_ - 1; i >= 0; --i ) {
beta[i] = R_NegInf;
for ( int w = min(slen_ - i, maxSeg_); w >= 1; --w ) {
int k = i + w;
// REprintf("# [%d,%d] (%d) beta(%d) = %f\n", i, k, w, k, beta[k]);
log_incr(beta[i], beta[k] + score[i][w]);
log_incr(grad[i][w], beta[k] + alpha[i]);
}
}
double res = alpha[slen_];
for ( int w = 1; w <= maxSeg_ && w <= slen_; ++w ) {
for ( int i = 0; i <= (slen_ - w); ++i ) {
res -= exp(grad[i][w] + score[i][w]) * score[i][w];
// REprintf("# out(%d,%d) = %f / %f\n", i, w, m(1), m(0));
}
}
return res;
}

*/