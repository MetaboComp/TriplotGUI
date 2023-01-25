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
checkTPO_ggplot <- function(TPObject,
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
      testMatrix<-rotate_around_xory(testMatrix,NULL,1)
    }
    if (nRisk > 0) {
      riskMatrix <- TPObject$riskMatrix
      riskMatrix<-rotate_around_xory(riskMatrix,NULL,1)
    }
    if (nRisk > 0& nCorr>0) {
      riskMatrix <- TPObject$riskMatrix
      testMatrix <- TPObject$corrMatrix
      testMatrix<-rotate_around_xory(testMatrix,NULL,1)
      riskMatrix<-rotate_around_xory(riskMatrix,NULL,1)

      if (scaleRisk) {
        riskMatrix <-   ###Whether to scale value of risks to max of correlations
          max(abs(testMatrix)) / max(abs(riskMatrix)) * riskMatrix
        ##This scaling is to first, find the biggest value in the risk matrix (for example 20),
        ##divide the abs(test/corr matrix) by this value. Let's say the biggest value in abs(test/corr matrix) is 0.9
        ##Then this max(abs(testMatrix)) / max(abs(riskMatrix)) is no more than 0.9/20
        ##When it multiplies risk, it is scale to the level that no value is beyond (-0.9,0.9)
      }
    }
    library(ggplot2)
    library(reshape2)
    if(is.null(TPObject$corrMatrix )&is.null(TPObject$riskMatrix)){
      cat("\n\nNo heatmap produced since no correlations or risks were found in the TPObject.")
    } else if (!is.null(TPObject$corrMatrix )&is.null(TPObject$riskMatrix )){
      melt_testMatrix<-melt(t(testMatrix))
      colnames(melt_testMatrix)<-c("PCs","Variables","Values")
      p<-ggplot(
                mapping=aes(melt_testMatrix$Variables,
                      melt_testMatrix$PCs,
                      fill = melt_testMatrix$Values
                      ) )+
        geom_tile( height=1,
                  colour = "white") +
        theme_minimal() +
        scale_x_discrete(name="Correlation coefficents of variables",
                         expand=c(0,0)

        )+
        scale_y_discrete(name="PCs",
                         expand=c(0,0)

        )+
     scale_fill_gradient2(  name="Value",
                            low = "blue",
                            high = "red",
                            mid="white",
                            midpoint = 0)+
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
          #vjust = 0.5
        ))

      p



    }else if (is.null(TPObject$corrMatrix )&!is.null(TPObject$riskMatrix )){
      melt_riskMatrix<-melt(t(riskMatrix))
      colnames(melt_riskMatrix)<-c("PCs","Variables","Values")
      p<-ggplot(
        mapping=aes(melt_riskMatrix$Variables,
                    melt_riskMatrix$PCs,
                    fill = melt_riskMatrix$Values
        ) )+
        geom_tile( height=1,
                   colour = "white") +
        theme_minimal() +
        scale_x_discrete(name="Risk coefficents of variables",
                         expand=c(0,0)

        )+
        scale_y_discrete(name="PCs",
                         expand=c(0,0)

        )+
        scale_fill_gradient2(  name="Value",
                               low = "blue",
                               high = "red",
                               mid="white",
                               midpoint = 0)+
        theme(
          axis.text.x = element_text(
            angle = 45,
            hjust = 1
            #vjust = 0.5
          ))

        p


    }else if (!is.null(TPObject$corrMatrix )&!is.null(TPObject$riskMatrix )){
      show_legend=T
      if(max(testMatrix)<=max(riskMatrix)&min(testMatrix)>=min(riskMatrix)){
        show_legend=F

      }


       melt_testMatrix<-melt(t(testMatrix))
        colnames(melt_testMatrix)<-c("PCs","Variables","Values")
        p1<-ggplot(
          mapping=aes(melt_testMatrix$Variables,
                      melt_testMatrix$PCs,
                      fill = melt_testMatrix$Values
          ) )+
          geom_tile( height=1,
                     show.legend = show_legend,
                     colour = "white") +
          theme_minimal() +
          scale_x_discrete(name="Correlation coefficents of variables",
                           expand=c(0,0)

          )+
          scale_y_discrete(name="PCs",
                           expand=c(0,0)

          )+
          scale_fill_gradient2(  name="Value",
                                 low = "blue",
                                 high = "red",
                                 mid="white",
                                 midpoint = 0)+
          theme(
            axis.text.x = element_text(
              angle = 45,
              hjust = 1
              #vjust = 0.5
            ))
          melt_riskMatrix<-melt(t(riskMatrix))
        colnames(melt_riskMatrix)<-c("PCs","Variables","Values")
        p2<-ggplot(
          mapping=aes(melt_riskMatrix$Variables,
                      melt_riskMatrix$PCs,
                      fill = melt_riskMatrix$Values
          ) )+
          geom_tile( height=1,
                     show.legend = T,
                     colour = "white") +
          theme_minimal() +
          scale_x_discrete(name="Scaled Risk coefficents of variables",
                           expand=c(0,0)

          )+
          scale_y_discrete(name="PCs",
                           expand=c(0,0)

          )+
          scale_fill_gradient2(  name="Value",
                                 low = "blue",
                                 high = "red",
                                 mid="white",
                                 midpoint = 0)+
          theme(
            axis.text.x = element_text(
              angle = 45,
              hjust = 1
              #vjust = 0.5
            ))

      if(combine_corrrisk==F){
        p1
        p2


      }else {
        library(ggpubr)
       p<-ggarrange(p1,p2)
       p

      }

    }
    ##This function generates color-coded Clustered Image Maps (CIMs) ("heat maps") to represent "high-dimensional" data sets.

  }

}
