
#' @title A Function To implement Yates' Algorithm to compute the Sum Squares
#' of (2^n) - 1 Factorial Effects in a 2^n Factorial Experiment.
#' The Factorial Experiment can be carried out using any one of the designs, i.e.,
#' CRD, RBD or LSD.
#'
#' @description  The Function implements Yates' Algorithm and returns the SS of the required
#' number of Factorial Effects in the given 2^n Factorial Experiment.
#'
#' For Example, in case of a 2^2 experiment, the function would return,
#' SS(A), SS(B) and SS(AB) by implementing the Yates' Algorithm, i.e., the SS due to the 3 required Factorial Effects,
#' among which two are the Main Effects and one is the First Order Interaction Effect.
#'
#' Note that, while entering the trt.combo or the trt vector as shown in the example
#' below, you have to maintain the same pattern and order of the assigned treatments
#' following which you have entered the response variable values y.
#'
#' @author Somjit Roy
#'
#'
#'
#' @param trt.combo A factor type character vector taking input for the treatment
#'                  combinations in a 2^n experiment considered in the standard
#'                  order.
#' @param trt.total A numeric vector storing the corresponding treatment (treatment combination)
#'                  totals, for instance in a 2^2 experiment we have :- [1],[a],[b],[ab] .
#' @param n The number of Factors under consideration in a 2^n Factorial Experiment.
#' @param r The number of replicates/blocks, for a CRD : the number of replicates,
#'          for a RBD : the number of blocks and for a LSD : the number of treatments itself.
#'
#' @seealso A Special Mention : Prof. Debjit Sengupta who helped to visualize and develop the
#'concept eventually making it possible for me to implement it through coding.
#'
#' @return The Sum Of Squares of the 2^n - 1 Factorial Effects in a 2^n Factorial
#'         Experiment in the Standard Order, a numeric vector.
#' @export
#'
#' @examples
#'
#' # The Response Variable as provided in the given design layout.
#'
#' y = c(90,74,81,83,77,81,88,73,
#'93,78,85,80,78,80,82,70,
#'98,85,88,84,82,85,88,79,
#'98,72,87,85,99,79,87,80,
#'95,76,83,86,90,75,84,80,
#'100,82,91,86,98,81,86,85)

#'# Number of Replicates or Blocks, whichever applicable in the considered Factorial
#'# Experiment.
#'
#'r = 3
#'
#'# Total number of factors in the 2^n Factorial Experiment under consideration.
#'
#'n = 4
#'
#'# The Treatment Allocation as mentioned in a factor type character vector.
#'
#'trt = as.factor(c(rep(1:8,each=1,times=3),rep(9:16,each=1,times=3)))
#'
#'# The Relevant Treatment Combinations in the 2^n Factorial Experiment in the order as mentioned.
#'
#'trt.combo = as.factor(c('1','a','b','ab','c','ac','bc','abc',
#'                        'd','ad','bd','abd','cd','acd','bcd','abcd'))
#'
#'# The Treatment Totals using the aggregate() function.

#'trt.total = aggregate(y,by = list(trt),sum)$x
#'
#'# Finally calling the function run.yates.algo() to get the desired SS'.
#'
#'SS.factorial.effects = run.yates.algo(trt.combo,trt.total,n,r)

run.yates.algo = function(trt.combo,trt.total,n,r)
{
  df = data.frame(trt.combo,trt.total)

  C3 = array(dim=1)

  ncol = 3

  while(ncol < (n+3))
  {
    ct.add = 1; ct.sub = 1

    for(i in 1:(length(trt.combo)/2))
    {
      C3[i] = trt.total[ct.add] + trt.total[ct.add+1]
      ct.add = ct.add+2
    }


    for(i in ((length(trt.combo)/2)+1):length(trt.combo))
    {
      C3[i] = trt.total[ct.sub+1] - trt.total[ct.sub]
      ct.sub = ct.sub+2
    }

    df = cbind(df,C3)
    trt.total = C3
    C3 = array(0)
    ncol = ncol + 1
  }

  colnames(df) = 1:(n+2)

  factorial.effects.numerator = array(0)

  last.col = as.vector(t(df[-(1:(n+1))]))



  for(i in 2:length(last.col))
  {
    factorial.effects.numerator[i-1] = last.col[i]
  }

  SS = array(dim=1)

  for(i in 1:length(factorial.effects.numerator))
  {
    SS[i] = (factorial.effects.numerator[i]^2)/((2^n)*r)
  }

  return(SS)

}
