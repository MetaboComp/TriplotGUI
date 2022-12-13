#' Convenience function for normal correlation of data with TriPlotObject scores
#'
#' @param TPObject A TriPlotObject
#' @param corrData Could be dataframe or matrix; Data to correlate to the TPO scores; observations in rows and variables in columns. Ensure that observations (rows) match between TPO scores and correlation data.
#' @param use See ?cor documentation (defaults to 'pairwise'). An optional character string giving a method for computing covariances in the presence of missing values.
#'  This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs".
#' @param method "pearson"(default),"spearman" or "kendall"
#' @param allowcategorical T or F allow categorical or not
#' @param patial T or F. Do partial correlation or not
#' @param confounder Potential confounder
#' @return A correlation matrix
#' @export
#'
#' @examples
#' See example under triPlot()
#'
makeCorr <- function(TPObject, ### scores
                     corrData, ### needs to be a dataframe with numeric values
                     use='pairwise',
                     method='spearman',
                     allowcategorical=F,
                     patial=F,
                     confounder=NULL
                     ) {
  library(ppcor)
  # library(StatTools)
  cat('\nMaking correlation between TPO and corrData')
  cat('\n-------------------------------------------')

  a<-menu(c("Yes","No"),
          graphics = F,
          "Do you have same number of rows for both scores in TPOobject and corrData")
  if(a==2){stop("Please make number of rows for scores in TPO object and corrData the same")}
  if(nrow(corrData) != TPObject$nObs) {
    stop("Not same number of observations in TPO and corrData.")
  }

  b<-menu(c("Yes","No"),
          graphics = F,
          "Do you have non-ordinal categorical (>2 catgories) variables coded as numeric")
  if(b==1){stop("please be careful about this and change it to factor variable")}


  if (!use%in%c("all.obs", "complete.obs", "pairwise.complete.obs",
                "everything", "na.or.complete")){
    stop("Correlation method  is not implemented")
  }

  if (!method%in%c("spearman","pearson","kendall")){
    stop("Correlation method  is not implemented")
  }


  if(allowcategorical==F&patial==F){
    for(i in 1:ncol(corrData)){
      if(class(corrData[,i])[1]%in%c("ordinal","factor","logical")){
        stop("Each variable in corrData should be numeric")
      }
  }
  scores <- TPObject$scores
  cor <- cor(corrData,    ##
             scores,
             use = use,
             method = method)

  }
  if(allowcategorical==F&patial==T){
    for(i in 1:ncol(corrData)){
      if(class(corrData[,i])[1]%in%c("ordinal","factor","logical")){
        stop("Each variable in corrData should be numeric")
      }
    }
    scores <- TPObject$scores
    cor <- pCor(scores,    ##
                corrData,
                confounder,
               cor_method = method)



  }

  pcor()

  partial_cor??????????????????????????


  cat('\n\nCorrelation matrix has',
      nrow(cor),'variables and',
      ncol(cor),'components')
  return(cor)
}
