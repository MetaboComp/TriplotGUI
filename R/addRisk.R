#' Add risk estimates to TriPlotObject
#'
#' @param TPObject A TriPlotObject
#' @param Risk A risk estimation matrix with components in rows and estimates, margin-of-error and (optionally) p-values in columns. The margin-of-error corresponds to half the width of the confidence interval (i.e. z * se).
#' @param name The name of the "risk"
#'
#' @return A TriPlotObject with added risk estimates
#' @export
#'
#' @examples
#' See example under triPlot()
addRisk <- function(TPObject,
                    Risk,
                    name=NULL) {
  cat('\nAdding risk to TPO')
  cat('\n------------------')
  cat('\nPlease add risks one by one with component in rows and estimates, se:s and (optionally) p-values in columns')
  cat('\nPlease ensure same number of components in TPO and risk matrix')
  nComp <- nrow(Risk)
  if (nComp != TPObject$nComp) {
    stop("Not same number of components in risk matrix and TPObject.")
  }
  if (ncol(Risk)<2) {
    stop("Please provide at least estimates and margins-of-error:s")
  }

  TPObject$nRisk <- TPObject$nRisk + 1

  TPObject$riskMatrix <- rbind(TPObject$riskMatrix,
                               Risk[,1])   ##this is a column in the Risk matrix.But it becomes a row here because it is only one column

  rownames(TPObject$riskMatrix)[TPObject$nRisk] <- name  ###give the name to the added risk

  TPObject$riskSE <- rbind(TPObject$riskSE,
                           Risk[,2])

  rownames(TPObject$riskSE)[TPObject$nRisk] <- name  ###give the name to the added risk

  if(ncol(Risk)>2) {
    TPObject$riskP <- rbind(TPObject$riskP,
                            Risk[,3])
    rownames(TPObject$riskP)[TPObject$nRisk] <- name
  }

  cat('\n\nTPO has',TPObject$nCorr,'attached correlations.')
  cat('\nTPO has',TPObject$nRisk,'attached risks.')

  return(TPObject)
}
