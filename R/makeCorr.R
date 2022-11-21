#' Convenience function for normal correlation of data with TriPlotObject scores
#'
#' @param TPObject A TriPlotObject
#' @param corrData Data to correlate to the TPO scores; observations in rows and variables in columns. Ensure that observations (rows) match between TPO scores and correlation data.
#' @param use See ?cor documentation (defaults to 'pairwise'). An optional character string giving a method for computing covariances in the presence of missing values.
#'  This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs".
#' @param method "pearson"(default),"spearman" or "kendall"
#'
#' @return A correlation matrix
#' @export
#'
#' @examples
#' See example under triPlot()
#'
makeCorr <- function(TPObject,
                     corrData,
                     use='pairwise',
                     method='spearman') {
  cat('\nMaking correlation between TPO and corrData')
  cat('\n-------------------------------------------')
  a<-menu(c("Yes","No"),
          graphics = T,
          "Do you have same number of rows for both scores in TPOobject and corrData")
  if(a==2){stop("Please make number of rows for scores in TPO object and corrData the same")}

  if (!method%in%c("Spearman","pearson")){
    stop("Correlation method  is not implemented")
  }
  if(nrow(corrData) != TPObject$nObs) {
    stop("Not same number of observations in TPO and corrData.")}

  scores <- TPObject$scores

  cor <- cor(corrData,
             scores,
             use = use,
             method = method)
  partial_cor??????????????????????????
  cat('\n\nCorrelation matrix has',
      nrow(cor),'variables and',
      ncol(cor),'components')
  return(cor)
}
