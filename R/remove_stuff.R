#' Remove correlations from TriPlotObject
#'
#' @param TPObject A TriPlotObject
#' @param remove_corr Which correlation(s) to remove (either 'all', a number or a vector with names)
#' @param remove_risk Which correlation(s) to remove (either 'all', a number or a vector with names)
#'
#' @return A TriPlotObject with added risk estimates
#' @export
#'
#' @examples
#' See example under triPlot()
removestuff <- function(TPObject,
                       remove_corr=NULL,
                       remove_risk=NULL) {
  #####################################################################################################
  ##Remove the name would be more proper
  cat('\nRemoving correlations(s)/risk(s) from TPO')
  cat('\n-------------------------')
  if(!is.null(remove_corr)){

    if(is.character(remove_corr)){
    if(any(!remove_corr%in%c("all",rownames(TPObject$corrMatrix)))){
      stop("\n Some correlations are not orginally added, please recheck the names of variables")
    }
    }else if(is.numeric(remove_corr)){
      if(any(!remove_corr%in%c(1:nrow(TPObject$corrMatrix)))){
        stop("\n Some correlations are not orginally added, please recheck the names of variables")
      }
    }
  }




############################################################################################
  if(!is.null(remove_risk)){
    if(is.character(remove_risk)){
    if(any(!remove_risk%in%c("all",rownames(TPObject$riskMatrix)))){
      stop("\n Some risks are not orginally added, please recheck the names of variables")
    }
    }else if (is.numeric(remove_risk)){
      if(any(!remove_risk%in%c(1:nrow(TPObject$riskMatrix)))){
        stop("\n Some correlations are not orginally added, please recheck the names of variables")
      }
    }
  }
###################################################################################################

  if(!is.null(remove_corr)){
    if(is.character(remove_corr)){
  if (remove_corr[1]=='all') {
    TPObject$nCorr <- 0
    TPObject$corrMatrix <- NULL
  } else {

    TPObject$nCorr <- TPObject$nCorr - length(remove_corr)
    TPObject$corrMatrix <- TPObject$corrMatrix[-which(remove_corr==rownames(TPObject$corrMatrix)),]
  }
    }else{
      TPObject$nCorr <- TPObject$nCorr - length(remove_corr)
      TPObject$corrMatrix <- TPObject$corrMatrix[-remove_corr,]


}
}

  if(!is.null(remove_risk)){

  if(is.character(remove_risk)){
  if (remove_risk[1]=='all') {
    TPObject$nRisk <- 0
    TPObject$riskMatrix <- NULL
    TPObject$riskSE <- NULL
    TPObject$riskP <- NULL
  } else {
    TPObject$nRisk <- TPObject$nRisk - length(remove_risk)
    TPObject$riskMatrix <- TPObject$riskMatrix[-which(remove_risk==rownames(TPObject$riskMatrix)),]
    TPObject$riskSE <- TPObject$riskSE[-which(remove_risk==rownames(TPObject$riskSE)),]
    TPObject$riskP <- TPObject$riskp[-which(remove_risk==rownames(TPObject$riskP)),]
  }
  }else{
    TPObject$nRisk <- TPObject$nRisk - length(remove_risk)
    TPObject$riskMatrix <- TPObject$riskMatrix[-remove_risk,]
    TPObject$riskSE <- TPObject$riskSE[-remove_risk,]
    TPObject$riskP <- TPObject$riskp[-remove_risk,]


 }
}
  cat('\n\nTPO has',TPObject$nCorr,'attached correlations.')
  cat('\nTPO has',TPObject$nRisk,'attached risks.')

  return(TPObject)
}

