% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run.yates.algo.R
\name{run.yates.algo}
\alias{run.yates.algo}
\title{A Function To implement Yates' Algorithm to compute the Sum Squares
of (2^n) - 1 Factorial Effects in a 2^n Factorial Experiment.
The Factorial Experiment can be carried out using any one of the designs, i.e.,
CRD, RBD or LSD.}
\usage{
run.yates.algo(trt.combo, trt.total, n, r)
}
\arguments{
\item{trt.combo}{A factor type character vector taking input for the treatment
combinations in a 2^n experiment considered in the standard
order.}

\item{trt.total}{A numeric vector storing the corresponding treatment (treatment combination)
totals, for instance in a 2^2 experiment we have :- [1],[a],[b],[ab] .}

\item{n}{The number of Factors under consideration in a 2^n Factorial Experiment.}

\item{r}{The number of replicates/blocks, for a CRD : the number of replicates,
for a RBD : the number of blocks and for a LSD : the number of treatments itself.}
}
\value{
The Sum Of Squares of the 2^n - 1 Factorial Effects in a 2^n Factorial
        Experiment in the Standard Order, a numeric vector.
}
\description{
The Function implements Yates' Algorithm and returns the SS of the required
number of Factorial Effects in the given 2^n Factorial Experiment.

For Example, in case of a 2^2 experiment, the function would return,
SS(A), SS(B) and SS(AB) by implementing the Yates' Algorithm, i.e., the SS due to the 3 required Factorial Effects,
among which two are the Main Effects and one is the First Order Interaction Effect.

Note that, while entering the trt.combo or the trt vector as shown in the example
below, you have to maintain the same pattern and order of the assigned treatments
following which you have entered the response variable values y.
}
\examples{

# The Response Variable as provided in the given design layout.

y = c(90,74,81,83,77,81,88,73,
93,78,85,80,78,80,82,70,
98,85,88,84,82,85,88,79,
98,72,87,85,99,79,87,80,
95,76,83,86,90,75,84,80,
100,82,91,86,98,81,86,85)
# Number of Replicates or Blocks, whichever applicable in the considered Factorial
# Experiment.

r = 3

# Total number of factors in the 2^n Factorial Experiment under consideration.

n = 4

# The Treatment Allocation as mentioned in a factor type character vector.

trt = as.factor(c(rep(1:8,each=1,times=3),rep(9:16,each=1,times=3)))

# The Relevant Treatment Combinations in the 2^n Factorial Experiment in the order as mentioned.

trt.combo = as.factor(c('1','a','b','ab','c','ac','bc','abc',
                       'd','ad','bd','abd','cd','acd','bcd','abcd'))

# The Treatment Totals using the aggregate() function.
trt.total = aggregate(y,by = list(trt),sum)$x

# Finally calling the function run.yates.algo() to get the desired SS'.

SS.factorial.effects = run.yates.algo(trt.combo,trt.total,n,r)
}
\seealso{
A Special Mention : Prof. Debjit Sengupta who helped to visualize and develop the
concept eventually making it possible for me to implement it through coding.
}
\author{
Somjit Roy
}
