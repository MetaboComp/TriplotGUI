#' Add correlation matrix to TriPlotObject
#'
#' @param TPObject A TriPlotObject
#' @param Corr A correlation matrix; TPO components in columns and correlation variables in rows. Make sure that `Corr` has rownames.
#'
#' @return A TriPlotObject with added correlations
#' @export
#'
#' @examples
#' See example under triPlot()
#'
#'
addCorr <- function(TPObject,Corr) {

  cat('\nAdding correlation to TPO')
  cat('\n-------------------------')
  cat('\nPlease ensure same number of components in TPO and correlation matrix')

  nComp <- ncol(Corr)
  if (nComp != TPObject$nComp) {
    stop("Not same number of components in correlation matrix and TPObject.")}

  newCorr <- nrow(Corr) # How many new correlations to add
  oldCorr <- TPObject$nCorr

  #corrNumber <- seq(oldCorr+1,oldCorr+newCorr)

  TPObject$corrMatrix <- rbind(TPObject$corrMatrix,Corr)

  TPObject$nCorr <- oldCorr + newCorr

  cat('\n\nTPO has',TPObject$nCorr,'attached correlations.')

  cat('\nTPO has',TPObject$nRisk,'attached risks.')

  return(TPObject)
}
