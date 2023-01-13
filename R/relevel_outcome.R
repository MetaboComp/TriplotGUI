#' relevel the outcome matrix
#' @param TPObject A TriPlotObject
#'
#' @param outcome  In the format of dataframe, could be binary, categorical (>2 groups) and numeric. It is recommeneded that you put outcome one at a time
#' @param REF A vector of which variabel is selected as reference
#' @return A list of risk estimation matrix with components in rows and (estimates, margin-of-error and p-values) in columns. The margin-of-error corresponds to half the width of the confidence interval (i.e. z * se).
#' @export
#'
#' @examples
#' See example under triPlot(
relevel_outcome <- function(outcome,
                            REF){

  if(missing(REF)){
    return(outcome)
    }else{
  dataframenew<-matrix(nrow=nrow(outcome),
                       ncol=ncol(outcome))
  dataframenew<-data.frame(dataframenew)
  colnames(dataframenew)<-colnames(outcome)
  rownames(dataframenew)<-rownames(outcome)
  a<-0
  for(i in 1:ncol(dataframenew)){
    if(class(outcome[,i])=="factor"){
      a<-a+1
    }
  }
  if(a==0){
    cat("There is no categorical variable. Do not need reference")
  }

  if(a!=0){
    if(length(REF)!=a){stop("The Reference options must be the same length as the number of the factor variables")}
  }
  b<-1
  for(i in 1:ncol(dataframenew)){
    if(class(outcome[,i])!="factor"){
      dataframenew[,i]<-outcome[,i]
    }
    if(class(outcome[,i])=="factor"){
      dataframenew[,i]<-relevel(outcome[,i],ref=REF[b])
      b<-b+1
    }

  }


  return(dataframenew)
  }   ## not missing RED
}
