#' Add risk estimates to TriPlotObject
#'
#' @param TPObject A TriPlotObject
#' @param Risk A list of risk risk estimation matrix with components in rows and estimates, margin-of-error and (optionally) p-values in columns. The margin-of-error corresponds to half the width of the confidence interval (i.e. z * se).

#'
#' @return A TriPlotObject with added risk estimates
#' @export
#'
#' @examples
#' See example under triPlot()
addRisk <- function(TPObject,
                    Risk) {
  cat('\nAdding risk to TPO')
  cat('\n------------------')
  cat('\nPlease add risks one by one with component in rows and estimates, se:s and (optionally) p-values in columns')
  cat('\nPlease ensure same number of components in TPO and risk matrix')
  Risk_new<-list()

  for(i in 1:length(Risk))            ##length should be 1 or 2
  {Risk_new<-c(Risk_new,Risk[[i]])   ### numeric list and factor risk list

  }

  Risk_new_new<-list()
  Risk_new_names<-c()

  for(i in 1:length(Risk_new)){
    if(class(Risk_new[[i]])[1]!="list"){
    Risk_new_new<-c(Risk_new_new,list(Risk_new[[i]]))
    }else{
     Risk_new_new<-c(Risk_new_new,Risk_new[[i]])

    }

  if(is.null(names(Risk_new[[i]]))){
    Risk_new_names<-c(Risk_new_names,names(Risk_new)[i])
  }else{
  Risk_new_names<-c(Risk_new_names,paste0(names(Risk_new)[i],"_",names(Risk_new[[i]])))
  }
  }

  Risk_new<-Risk_new_new
  names(Risk_new)<-Risk_new_names
  nComp <- nrow(Risk_new[[1]])   ###

  if (nComp != TPObject$nComp) {
    stop("Not same number of components in risk matrix and TPObject.")
  }
  if (ncol(Risk_new[[1]])<2) {
    stop("Please provide at least estimates and margins-of-error:s")
  }

  TPObject$nRisk <- TPObject$nRisk + length(Risk_new)
  for(i in 1:length(Risk_new)){
  TPObject$riskMatrix <- rbind(TPObject$riskMatrix,
                               Risk_new[[i]][,1])   ##this is a column in the Risk matrix.But it becomes a row here because it is only one column
  TPObject$riskSE <- rbind(TPObject$riskSE,
                           Risk_new[[i]][,2])
  if(ncol(Risk_new[[1]])>2) {
    TPObject$riskP <- rbind(TPObject$riskP,
                            Risk_new[[i]][,3])

  }
  }



  rownames(TPObject$riskMatrix)<- names(Risk_new)  ###give the name to the added risk
  rownames(TPObject$riskSE) <- names(Risk_new)  ###give the name to the added risk

  if(ncol(Risk[[1]][[1]])>2) {
  rownames(TPObject$riskP) <-names(Risk_new)
  }



  cat('\n\nTPO has',TPObject$nCorr,'attached correlations.')
  cat('\nTPO has',TPObject$nRisk,'attached risks.')

  return(TPObject)
}
