#' Provide detailed information about TriPlotObject
#'
#' @param TPObject A TriPlotObject
#' @param heatmap Whether to plot heatmap with correlations and risks (defaults to TRUE)
#' @param colorScheme Which color scheme to use for heatmap: 'MO' (mixOmics original; default) or high-contrast 'BWR' (Blue/white/red)
#' @param scaleRisk Whether to scale value of risks to max of correlations (defaults to TRUE). This makes risk estimate scale comparable to correlations in the heatmap.
#' @param cluster Whether to cluster x and/or y axes (defaults to 'none'). See ?mixOmics::cim for details.
#' @param combine_corrrisk logical, put correlation and risk in one plot or not
#' @return Detailed information and heatmap of correlations and risks in TriPlotObject
#' @export
#'
#' @examples
#' See example under triPlot()
checkTPO <- function(TPObject,
                     heatmap = TRUE,
                     colorScheme = 'BWR',
                     scaleRisk = TRUE,
                     cluster = 'none',
                     combine_corrrisk=T) {
  nObs <- TPObject$nObs
  nVar <- TPObject$nVar
  nComp <- TPObject$nComp
  nCorr <- TPObject$nCorr
  nRisk <- TPObject$nRisk

  #### DETAILS,Cat everything here
  cat('\nChecking TriPlotObject (TPO)')
  cat('\n---------------------------')

  if (nObs == nrow(TPObject$scores) &
      nComp == ncol(TPObject$scores)) {
    cat('\nScore matrix has',
        nObs,
        'observations and',
        nComp,
        'components.')

  } else {
    stop('\nMismatch between $nObs and nrow(scores)')
  }

  if (nVar == nrow(TPObject$loadings) &
      nComp == ncol(TPObject$loadings)) {
    cat('\nLoading matrix has',
        nVar,
        'variables and',
        nComp,
        'components.')

  } else
    {stop('\nMismatch between $nVar and nrow(loadings)')}

  cat('\n\nTPO has', TPObject$nCorr, 'attached correlations:')

  if (TPObject$nCorr > 0) {
    for (i in 1:nCorr) {
      cat('\n  ', i, '.\t', rownames(TPObject$corrMatrix)[i], sep = '')
    }
  }

  cat('\n\nTPO has', TPObject$nRisk, 'attached risks:')
  if (TPObject$nRisk > 0) {
    for (i in 1:nRisk) {
      cat('\n  ', i, '.\t', rownames(TPObject$riskMatrix)[i], sep = '')
    }
  }

  # HEATMAP
  if (heatmap & (nCorr + nRisk) > 0) {  ##If heatmap is true and there is nCorr or nRisk
    if (colorScheme == 'BWR') {    ###this is to manually set a color shceme
      color <-colorRampPalette(c('blue', 'white', 'red'))(25)
      }else{
        color <- NULL
    }

    if (nCorr > 0) {
      testMatrix <- TPObject$corrMatrix   ##build matrix that has variables
    }
    if (nRisk > 0) {
      riskMatrix <- TPObject$riskMatrix   ##build matrix that has variables
    }
    if (nRisk > 0&nCorr>0) {
      riskMatrix <- TPObject$riskMatrix
      testMatrix <- TPObject$corrMatrix
        if (scaleRisk) {
          riskMatrix <-   ###Whether to scale value of risks to max of correlations
            max(abs(testMatrix)) / max(abs(riskMatrix)) * riskMatrix
          ##This scaling is to first, find the biggest value in the risk matrix (for example 20),
          ##divide the abs(test/corr matrix) by this value. Let's say the biggest value in abs(test/corr matrix) is 0.9
          ##Then this max(abs(testMatrix)) / max(abs(riskMatrix)) is no more than 0.9/20
          ##When it multiplies risk, it is scale to the level that no value is beyond (-0.9,0.9)
        }
    }
    library(gplots)
    if(is.null(TPObject$corrMatrix)&is.null(TPObject$riskMatrix)){
      cat("\n\nNo heatmap produced since no correlations or risks were found in the TPObject.")
    } else if (!is.null(TPObject$corrMatrix)&is.null(TPObject$riskMatrix)){

      heatmap.2(t(testMatrix),
                main="Correlation Heatmap",
                Rowv=F,
                Colv=F,

                 margins = c(10, 20), ##  margins for row and column name
                trace="none",
                col=color,    ## color
                na.color="gray80", ## color of na
                cexCol = 1,      ## size of the font in column
                cexRow = 1,      ## size of the font in rows
                xlab="variables",
                ylab="PCs",
                srtCol = 45,
                dendrogram="none",
                density.info="none"
        )


    }else if (is.null(TPObject$corrMatrix)&!is.null(TPObject$riskMatrix)){

      heatmap.2(t(riskMatrix),
                main="Risk Heatmap",
                margins = c(10, 20), ##  margins for row and column name
                trace="none",
                Rowv=F,
                Colv=F,
                col=color,    ## color
                na.color="gray80", ## color of na
                cexCol = 1,      ## size of the font in column
                cexRow = 1,      ## size of the font in rows
                xlab="variables",
                ylab="PCs",
                srtCol = 45,
                dendrogram="none",
                density.info="none"
      )

    }else if (!is.null(TPObject$corrMatrix)&!is.null(TPObject$riskMatrix)){

      if(combine_corrrisk==F){
      heatmap.2(t(testMatrix),
                main="Correlation Heatmap",
                margins = c(10, 20), ##  margins for row and column name
                trace="none",
                Rowv=F,
                Colv=F,
                col=color,    ## color
                na.color="gray80", ## color of na
                cexCol = 1,      ## size of the font in column
                cexRow = 1,      ## size of the font in rows
                xlab="variables",
                ylab="PCs",
                srtCol = 45,
                dendrogram="none",
                density.info="none"
      )


      heatmap.2(t(riskMatrix),
                main="Risk Heatmap",
                margins = c(10, 20), ##  margins for row and column name
                trace="none",
                Rowv=F,
                Colv=F,
                col=color,    ## color
                na.color="gray80", ## color of na
                cexCol = 1,      ## size of the font in column
                cexRow = 1,      ## size of the font in rows
                xlab="variables",
                ylab="PCs",
                srtCol = 45,
                dendrogram="none",
                density.info="none",
                key=F
      )
      }else {
        combined_matrix<-rbind(testMatrix,riskMatrix)
        heatmap.2(t(combined_matrix),
                  main="Correlation and Risk Heatmap",
                  margins = c(10, 20), ##  margins for row and column name
                  trace="none",
                  Rowv=F,
                  Colv=F,
                  col=color,    ## color
                  na.color="gray80", ## color of na
                  cexCol = 1,      ## size of the font in column
                  cexRow = 1,      ## size of the font in rows
                  xlab="variables",
                  ylab="PCs",
                  srtCol = 45,
                  dendrogram="none",
                  density.info="none",

        )
      }

  }
    ##This function generates color-coded Clustered Image Maps (CIMs) ("heat maps") to represent "high-dimensional" data sets.

  }
 # dev.off()
}
