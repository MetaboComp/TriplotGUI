#' Prepare TriPlotObject which memorize the matrix of scores, the matrix of loadings, number of observations and variables,
#' number of components, number of associated correlations, number of associated risks
#'
#' @param scores Scores have observations in rows and components (or factors) in columns
#' @param loadings Loadings have variables in rows and components (or factors) in columns
#' @param compLimit Option limit to the number of component
#'
#' @return A TriPlotObject
#' @export
#'
#' @examples
#' See example under triPlot()
makeTPO <- function(scores,
                    loadings,
                    compLimit) {
  cat('\nMaking TriPlotObject (TPO)')
  cat('\n--------------------------')
  a<-menu(c("Yes","No"),
          graphics = T,
          "Do you have scores as observations in rows and components in columns?")
  if(a==2){stop("Please make rows as observations,column as components")}
  b<-menu(c("Yes","No"),
          graphics = T,
          "Do you have scores as observations in rows and components in columns?")
  if(b==2){stop("Please make rows as variables, column as components")}

  if (ncol(scores)!=ncol(loadings)){
    stop("Scores and loadings should have same numbers of principle components")
    }

  nComp <- ncol(scores)    ###number of components
  if (nComp == 1) {        ###The number of component connot be
    stop("1 component only not implemented.")}

  ###The point is the limit of component cannot be bigger than the number of compLimit
  if (missing(compLimit)) {
    compLimit <- nComp
  }
  if(compLimit > nComp) {
    stop("compLimit cannot be larger than the number of components.")
  }
  if(compLimit < nComp) {
    cat('\nScores and loadings truncated from',nComp,'to',compLimit,'components.')
  }
  scores <- scores[,1:compLimit]  ##limit the number of components
  loadings <- loadings[,1:compLimit]
  nComp <- compLimit
  nObs <- nrow(scores)
  nVar <- nrow(loadings)

  cat('\n\nScore matrix has',nObs,'observations and',nComp,'components.')
  cat('\nLoading matrix has',nVar,'variables and',nComp,'components.')

  TPObject <- list()
  TPObject$scores <- scores
  TPObject$loadings <- loadings
  TPObject$nObs <- nObs
  TPObject$nVar <- nVar
  TPObject$nComp <- nComp
  TPObject$nCorr <- 0 # Initialize number of associated correlations
  TPObject$nRisk <- 0 # Initialize number of associated risks

  cat('\n\nTPO has',TPObject$nCorr,'attached correlations.')

  cat('\nTPO has',TPObject$nRisk,'attached risks.')

  return(TPObject)
}
