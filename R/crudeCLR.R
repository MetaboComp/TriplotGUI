#' Convenience function to calculate crude risk estimates using conditional logistic regression
#'
#' @param TPObject A TriPlotObject
#'
#' @param caseStat Vector with case/control status of TPO observations (score rows)
#' @param pair Vector with case/control pairs of TPO observations (score rows)
#' @param CI Confidence Interval for the risk estimate (defaults to 0.95)
#'
#' @return A risk estimation matrix with components in rows and (estimates, margin-of-error and p-values) in columns. The margin-of-error corresponds to half the width of the confidence interval (i.e. z * se).
#' @export
#'
#' @examples
#' See example under triPlot()
crudeCLR <- function(TPObject,
                     caseStat,
                     pair,
                     CI=0.95){

  ####add menu to ask outcome to be in the
  # Have you make the outcome a factor variable, if outcome is categorial, and a numeric variable if the outcome is numeric
  #if outcome is categorical glm
  # if numeric linear regression
  if(is.factor(outcome)){
    if(length(levels(factor))==1){stop("Outcome should not have only one level")}
    if(length(levels(factor))==2){type="binomial"}
    if(length(levels(factor))>2){type="multinomial"}
    if (length(TPOobject)!=length(pair)){
      stop("They should be at same length")


    }
  }


  if(is.numeric(outcome)){

  }

  alpha <- (1 - CI) / 2

  z <- abs(qnorm(alpha))   #qnorm is quantile function
  scores <- TPObject$scores   ##row is observations, column is components

  risk <- matrix(nrow=TPObject$nComp,   ##row is component
                 ncol=3)

  colnames(risk) <- c('Estimated mean','margin of error','p value')

  rownames(risk) <- colnames(scores)  ###row is components

  for (i in 1:TPObject$nComp) {
    clr <- summary(clogit(caseStat~scores[,i]+strata(pair)))  ##Conditional logistic regression
    risk[i,] <- clr$coefficients[c(1,3,5)]
    risk[i,2] <- z * risk[i,2]   ###calculate margin of error
    ##margin of error is different from standard of error, margin of error includes the consideration of alpha
    ##marigin of error is the distance from the mean value to oneside of the confidence interval
  }
  cat('\nFinished calculating risk for',TPObject$nComp,'components')
  cat('\n  column 1: mean values')
  cat('\n  column 2: margin-of-error')
  cat('\n  column 3: p-values')

  return(risk)
}


crudeLR <- function(TPObject,
                    caseStat,
                    CI=0.95){
  alpha <- (1 - CI) / 2

  z <- abs(qnorm(alpha))

  scores <- TPObject$scores
  risk <- matrix(nrow=TPObject$nComp,ncol=3)

  colnames(risk) <- c('est','moe','p')
  rownames(risk) <- colnames(scores)

  for (i in 1:TPObject$nComp) {
    clr <- summary(glm(caseStat~scores[,i],
                       family='binomial'))
    risk[i,] <- clr$coefficients[2,c(1,2,4)]

    risk[i,2] <- z * risk[i,2]
  }

  cat('\nFinished calculating risk for',TPObject$nComp,'components')
  cat('\n  column 1: mean values')
  cat('\n  column 2: margin-of-error')
  cat('\n  column 3: p-values')

  return(risk)
}
