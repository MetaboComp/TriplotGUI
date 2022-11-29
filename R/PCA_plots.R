#' PCA_plots
#' @param plottype scree score loading 2 options
#' @param pc_num number of components
#' @param scale true by default
#' @param center true by defaule
#' @param dataframe row as observations, column as variables
#' @param rotate 	"none", "varimax", "quartimax", "promax", "oblimin", "simplimax", and "cluster"
#' @param pc_type prcomp or principle
#' @param size_variable
#' @param size_variable_name
#' @param color_variable
#' @param color_variable_name
#' @param shape_variale
#' @param shape_variable_name
#' @param scale_scoreloading
#' @param first_PC
#' @param second_PC
#' @param loadings_name
#' @return A PCA plot
#' @export
#'
#' @examples
#'
#'
############add clustering
PCA_plots<-function(dataframe,
                    plottype=c("scree","score","loading","scoreloading"),
                    pc_type=c("prcomp","principle"),
                    pc_num=5,
                    scale=T,
                    center=T,
                    rotate="none",
                    size_variable=NULL,
                    size_variable_name=NULL,
                    color_variable=NULL,
                    color_variable_name=NULL,
                    shape_variable=NULL,
                    shape_variable_name=NULL,
                    scale_scoreloading=T,
                    first_PC=1,
                    second_PC=2,
                    loadings_name=T
){
  library(ggplot2)
  library(ggrepel)
  library(tidyverse)
  if(missing(pc_type)){pc_type="prcomp"}
  if(missing(plottype)){plottype="scoreloading"}
  dataframe<-as.data.frame(dataframe)
  dataframe_numeric<-dataframe
  for(i in 1:ncol(dataframe)){
    dataframe_numeric[,i]<-as.numeric(dataframe[,i])
  }

  if(!pc_type%in%c("prcomp","principal")){
    stop("This method is not implemented")
  }
  if(!plottype%in%c("scree","score","loading","scoreloading")){
    stop("This method is not implemented")
  }
  if(is.null(color_variable_name)){color_variable_name<-"color_variable"}
  if(is.null(shape_variable_name)){shape_variable_name<-"shape_variable"}
  if(is.null(size_variable_name)){size_variable_name<-"size_variable"}
  pca_object<-list()
  plot_object<-list()

  if(pc_type=="prcomp"){
  pca <-prcomp(dataframe_numeric,  ## since it has no rotation pc_num is not set for now here
               scale. = scale,   ## scale to unit variance
               center=center   ## center to 0
               )
  scores=pca$x## Extract scores
  loadings=pca$rotation## Extract loadings

  scores <- pca$x %>%
    as.data.frame()
  loadings<- pca$rotation %>%
    as.data.frame()
  pca_object$scores<-scores
  pca_object$loadings<-loadings
  variance<-pca$sdev %>%    ##egienvalue are the same as principal
    as_tibble() %>%    ## Change the standard deviations into a tibble dataframe
    frac_sd()

  }

  if(pc_type=="principal"){

    if(!rotate%in%c("none", "varimax", "quartimax", "promax", "oblimin", "simplimax","cluster")){
      stop("This rotate method nor supported")
    }

    library(psych)
    if(scale==T){dataframe_numeric<-scale(dataframe_numeric,
                                          center=rep(0,ncol(dataframe_numeric)))}  ##scale to unit variance,center to mean
    pca <-principal(dataframe_numeric,  ##scale
              nfactors=pc_num,  ##choose one value
              rotate=rotate,
              scores=T)

    scores <- pca$scores %>%
      as.data.frame()

    loadings<- pca$loadings
    class(loadings)<-"matrix"
    loadings<-as.data.frame(as.matrix(loadings))
    pca_object$scores<-scores
    pca_object$loadings<-loadings

    variance<-pca$values[1:pc_num] %>%
      as_tibble() %>%
      frac_var()
  }


  frac_sd<- function(x){ x^2/sum(x^2)}
  frac_var <- function(x){ x/sum(x)}
  if(plottype=="scree"){

    library(scales)
    library(dplyr)
    if(pc_type=="prcomp"){
    scree_plot<-pca$sdev %>%    ##egienvalue are the same as principal
      as_tibble() %>%    ## Change the standard deviations into a tibble dataframe
      frac_sd() %>%     ## calculate the variance explained by the component
      mutate(Comp = factor(colnames(pca$x),levels=colnames(pca$x))) %>%   ## Add a new column: the PC name for each variance value
      slice(1:pc_num) %>%    ##choose the first 9 component
      ggplot(aes(x=Comp, y = value)) +
      geom_bar(stat = "identity",
               fill = "#4DC5F9") +   ## make a barplot to see the variance
      geom_hline(yintercept = 0.03,  ## add a horizontal line
                 linetype=2) +
      xlab("Principal Components") +
      scale_y_continuous(name = "Variance Explained",
                         breaks = seq(0,0.8,0.1),
                         labels = percent_format(accuracy = 5L)) +  ## Change the percentage from decimals to % (0.7-->70%)
      theme_classic(base_size = 14)
    plot_object$scree_plot<-scree_plot
    }
    if(pc_type=="principal"){
      # library(GPArotation)
      scree_plot<-pca$values[1:pc_num] %>%   ## Is this correct???????
        as_tibble() %>%    ## Change the standard deviations into a tibble dataframe
        frac_var() %>%     ## calculate the variance explained by the component
        mutate(Comp = factor(colnames(pca$scores),levels=colnames(pca$scores))) %>%   ## Add a new column: the PC name for each variance value
        slice(1:pc_num) %>%    ##choose the first 9 component
        ggplot(aes(x=Comp, y = value)) +
        geom_bar(stat = "identity",
                 fill = "#4DC5F9") +   ## make a barplot to see the variance
        geom_hline(yintercept = 0.03,  ## add a horizontal line
                   linetype=2) +
        xlab("Principal Components") +
        scale_y_continuous(name = "Variance Explained",
                           breaks = seq(0,0.8,0.1),
                           labels = percent_format(accuracy = 5L)
                           ) +  ## Change the percentage from decimals to % (0.7-->70%)
        theme_classic(base_size = 14)
      plot_object$scree_plot<-scree_plot

    }


  }
  if(plottype=="score"){


    for(i in 1:ncol(dataframe)){
      if(is.factor(dataframe[,i])){
        dataframe[,i]<-StatTools::factor_samesequence(dataframe[,i])
      }
    }
   ## A PCA plot for PC1 and PC2
    score_plot<-ggplot(scores,
           aes(x=scores[,first_PC],
               y=scores[,second_PC],
               color=color_variable
               )) +   ## different time different color
      geom_point(
                 aes(size = size_variable,
                     shape=shape_variable)) +  ## Different replicate different shape
      geom_vline(xintercept = 0,
                 linetype=2) +
      geom_hline(yintercept = 0,
                 linetype=2)  +

      scale_x_continuous(name = paste("Score PC", first_PC, "(",variance[first_PC],"%) variance explained"),
                         limits=c(-max(abs(scores[,first_PC])),max(abs(scores[,first_PC])))) +
      scale_y_continuous(name = paste("Score PC", second_PC, "(",variance[second_PC],"%) variance explained"),
                         limits=c(-max(abs(scores[,second_PC])),max(abs(scores[,second_PC])))) +
      scale_color_discrete(name = paste(color_variable_name)) +
      scale_size_continuous(name = paste(size_variable_name)) +
      scale_shape_discrete(name = paste(shape_variable_name)) +

      theme_bw() +
      theme(panel.grid.major = element_blank(),  ## remove grid
            panel.grid.minor = element_blank())  ## remove grid
    plot_object$score_plot<-score_plot
    }

  if(plottype=="loading"){
    if(loadings_name==T){

   loading_plot<-ggplot(loadings,
          aes(x=loadings[,first_PC],
              y=loadings [,second_PC] ))+
     geom_point() +
     geom_segment(aes(xend=loadings[,first_PC],  ## x positions dots to be connected (a vector)
                      yend=loadings [,second_PC]), ## y positions dots to be connected (a vector)
                  x=0, ## Starting x position of lines
                  y=0, ## Starting y position of lines
                  color="Grey") +  ## color of the line
     geom_label_repel(aes(x=loadings[,first_PC],
                    y=loadings[,second_PC],
                    label=rownames(loadings)),   ## Add label
                size=2,
                vjust="outward"
                ) +    ## The label is adjusted outwarded
     geom_vline(xintercept = 0,
                linetype=2) +
     geom_hline(yintercept = 0,
                linetype=2)  +
     #geom_text(size = 3, check_overlap = T)  +
     scale_x_continuous(name = paste("Loading PC", first_PC, "(",variance[first_PC],"%) variance explained"),
                        limits =c(-max(abs(loadings[,first_PC])),max(abs(loadings[,first_PC])))) +
     scale_y_continuous(name = paste("Loading PC", second_PC, "(",variance[second_PC],"%) variance explained"),
                        limits=c(-max(abs(loadings[,second_PC])),max(abs(loadings[,second_PC])))) +
     #scale_color_discrete(name = paste(color_variable_name)) +
     #scale_size_continuous(name = paste(size_variable_name)) +
     #scale_shape_discrete(name = paste(shape_variable_name)) +

     theme_bw() +
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())
   plot_object$loading_plot<-loading_plot
    }
    if(loadings_name==F){

      loading_plot<-ggplot(loadings,
                           aes(x=loadings[,first_PC],
                               y=loadings [,second_PC] ))+
        geom_point() +
        geom_segment(aes(xend=loadings[,first_PC],  ## x positions dots to be connected (a vector)
                         yend=loadings [,second_PC]), ## y positions dots to be connected (a vector)
                     x=0, ## Starting x position of lines
                     y=0, ## Starting y position of lines
                     color="Grey") +  ## color of the line
        geom_vline(xintercept = 0,
                   linetype=2) +
        geom_hline(yintercept = 0,
                   linetype=2)  +
        #geom_text(size = 3, check_overlap = T)  +
        scale_x_continuous(name = paste("Loading PC", first_PC, "(",variance[first_PC],"%) variance explained"),
                           limits =c(-max(abs(loadings[,first_PC])),max(abs(loadings[,first_PC])))) +
        scale_y_continuous(name = paste("Loading PC", second_PC, "(",variance[second_PC],"%) variance explained"),
                           limits=c(-max(abs(loadings[,second_PC])),max(abs(loadings[,second_PC])))) +
        #scale_color_discrete(name = paste(color_variable_name)) +
        #scale_size_continuous(name = paste(size_variable_name)) +
        #scale_shape_discrete(name = paste(shape_variable_name)) +

        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      plot_object$loading_plot<-loading_plot
    }
  }

  if(plottype=="scoreloading"){

    for(i in 1:ncol(dataframe)){
      if(is.factor(dataframe[,i])){
        dataframe[,i]<-StatTools::factor_samesequence(dataframe[,i])
      }
    }

    if(scale_scoreloading==T){
      if(loadings_name==T){
    ## A PCA plot for PC1 and PC2
    scoreloading_plot<-ggplot(scores,
           aes(x=scores[,first_PC],
               y=scores[,second_PC]
           )) +   ## different time different color
      geom_point(
        aes(size = size_variable,
            shape=shape_variable,
            color=color_variable)) +  ## Different replicate different shape
      geom_vline(xintercept = 0,
                 linetype=2) +
      geom_hline(yintercept = 0,
                 linetype=2)  +
      geom_segment(data=loadings,
                   aes(xend=loadings[,first_PC]*(max(abs(scores[,first_PC]))/max(abs(loadings[,first_PC]))),  ## x positions dots to be connected (a vector)
                       yend=loadings[,second_PC]*(max(abs(scores[,second_PC]))/max(abs(loadings[,second_PC])))
                       ), ## y positions dots to be connected (a vector)
                   x=0, ## Starting x position of lines
                   y=0, ## Starting y position of lines
                   color="Grey") +  ## color of the line
      geom_label_repel(data=loadings,
                    aes(x=loadings[,first_PC]*(max(abs(scores[,first_PC]))/max(abs(loadings[,first_PC]))),
                     y=loadings[,second_PC]*(max(abs(scores[,second_PC]))/max(abs(loadings[,second_PC]))),
                     label=rownames(loadings) ),   ## Add label
                 size=2,
                 vjust="inward",
                 hjust="inward") +
      scale_y_continuous(name = paste("Score PC", second_PC) ,
                         limits =1.1*c(-max(abs(scores[,second_PC])),max(abs(scores[,second_PC]))),
                         sec.axis = sec_axis(trans=~./(max(abs(scores[,second_PC]))/max(abs(loadings[,second_PC]))),
                                             name =  paste("Loading PC", second_PC, "(",variance[second_PC],"%) variance explained")  ,
                                             breaks=waiver()
                                             #              breaks =seq(-max(abs(loadings[,second_PC])),max(abs(loadings[,second_PC])),10)
                         )
      ) +
      scale_x_continuous(name = paste("Score PC", first_PC),
                         limits =1.01*c(-max(abs(scores[,first_PC])),max(abs(scores[,first_PC]))),
                         sec.axis = sec_axis(trans=~./(max(abs(scores[,first_PC]))/max(abs(loadings[,first_PC]))),
                                             name =  paste("Loading PC", first_PC, "(",variance[first_PC],"%) variance explained") ,
                                             breaks=waiver()
                                             #breaks =seq(-max(abs(scores[,first_PC])),max(abs(scores[,first_PC])))
                                             )
                         ) +


      scale_color_discrete(name = paste(color_variable_name)) +
      scale_size_continuous(name = paste(size_variable_name)) +
      scale_shape_discrete(name = paste(shape_variable_name)) +

      theme_bw() +
      theme(panel.grid.major = element_blank(),  ## remove grid
            panel.grid.minor = element_blank())  ## remove grid
    plot_object$scoreloading_plot<-scoreloading_plot
      }
      if(loadings_name==F){
        ## A PCA plot for PC1 and PC2
        scoreloading_plot<-ggplot(scores,
                                  aes(x=scores[,first_PC],
                                      y=scores[,second_PC]
                                  )) +   ## different time different color
          geom_point(
            aes(size = size_variable,
                shape=shape_variable,
                color=color_variable)) +  ## Different replicate different shape
          geom_vline(xintercept = 0,
                     linetype=2) +
          geom_hline(yintercept = 0,
                     linetype=2)  +
          geom_segment(data=loadings,
                       aes(xend=loadings[,first_PC]*(max(abs(scores[,first_PC]))/max(abs(loadings[,first_PC]))),  ## x positions dots to be connected (a vector)
                           yend=loadings[,second_PC]*(max(abs(scores[,second_PC]))/max(abs(loadings[,second_PC])))
                       ), ## y positions dots to be connected (a vector)
                       x=0, ## Starting x position of lines
                       y=0, ## Starting y position of lines
                       color="Grey") +  ## color of the line

          scale_y_continuous(name = paste("Score PC", second_PC) ,
                             limits =1.1*c(-max(abs(scores[,second_PC])),max(abs(scores[,second_PC]))),
                             sec.axis = sec_axis(trans=~./(max(abs(scores[,second_PC]))/max(abs(loadings[,second_PC]))),
                                                 name =  paste("Loading PC", second_PC, "(",variance[first_PC],"%) variance explained")  ,
                                                 breaks=waiver()
                                                 #              breaks =seq(-max(abs(loadings[,second_PC])),max(abs(loadings[,second_PC])),10)
                             )
          ) +
          scale_x_continuous(name = paste("Score PC", first_PC),
                             limits =1.01*c(-max(abs(scores[,first_PC])),max(abs(scores[,first_PC]))),
                             sec.axis = sec_axis(trans=~./(max(abs(scores[,first_PC]))/max(abs(loadings[,first_PC]))),
                                                 name =  paste("Loading PC", first_PC, "(",variance[second_PC],"%) variance explained") ,
                                                 breaks=waiver()
                                                 #breaks =seq(-max(abs(scores[,first_PC])),max(abs(scores[,first_PC])))
                             )
          ) +


          scale_color_discrete(name = paste(color_variable_name)) +
          scale_size_continuous(name = paste(size_variable_name)) +
          scale_shape_discrete(name = paste(shape_variable_name)) +

          theme_bw() +
          theme(panel.grid.major = element_blank(),  ## remove grid
                panel.grid.minor = element_blank())  ## remove grid
        plot_object$scoreloading_plot<-scoreloading_plot
      }




    }
    if(scale_scoreloading==F){
      if(loadings_name==T){
      scoreloading_plot<-ggplot(scores,
                                aes(x=scores[,first_PC],
                                    y=scores[,second_PC]
                                )) +   ## different time different color
        geom_point(
          aes(size = size_variable,
              shape=shape_variable,
              color=color_variable)) +  ## Different replicate different shape
        geom_vline(xintercept = 0,
                   linetype=2) +
        geom_hline(yintercept = 0,
                   linetype=2)  +
        geom_segment(data=loadings,
                     aes(xend=loadings[,first_PC],  ## x positions dots to be connected (a vector)
                         yend=loadings[,second_PC]
                     ), ## y positions dots to be connected (a vector)
                     x=0, ## Starting x position of lines
                     y=0, ## Starting y position of lines
                     color="Grey") +  ## color of the line
        geom_label(data=loadings,
                   aes(x=loadings[,first_PC],
                       y=loadings[,second_PC],
                       label=rownames(loadings) ),   ## Add label
                   size=2,
                   vjust="inward",
                   hjust="inward") +
        scale_y_continuous(name = paste("Score PC", second_PC, "(",variance[second_PC],"%) variance explained") ,
                           limits =1.01*c(-max(abs(scores[,second_PC])),max(abs(scores[,second_PC])))
        ) +
        scale_x_continuous(name = paste("Score PC", first_PC, "(",variance[first_PC],"%) variance explained"),
                           limits =1.01*c(-max(abs(scores[,first_PC])),max(abs(scores[,first_PC])))
        ) +


        scale_color_discrete(name = paste(color_variable_name)) +
        scale_size_continuous(name = paste(size_variable_name)) +
        scale_shape_discrete(name = paste(shape_variable_name)) +

        theme_bw() +
        theme(panel.grid.major = element_blank(),  ## remove grid
              panel.grid.minor = element_blank())  ## remove grid
      plot_object$scoreloading_plot<-scoreloading_plot

      }


    if(loadings_name==F){
      scoreloading_plot<-ggplot(scores,
                                aes(x=scores[,first_PC],
                                    y=scores[,second_PC]
                                )) +   ## different time different color
        geom_point(
          aes(size = size_variable,
              shape=shape_variable,
              color=color_variable)) +  ## Different replicate different shape
        geom_vline(xintercept = 0,
                   linetype=2) +
        geom_hline(yintercept = 0,
                   linetype=2)  +
        geom_segment(data=loadings,
                     aes(xend=loadings[,first_PC],  ## x positions dots to be connected (a vector)
                         yend=loadings[,second_PC]
                     ), ## y positions dots to be connected (a vector)
                     x=0, ## Starting x position of lines
                     y=0, ## Starting y position of lines
                     color="Grey") +  ## color of the line

        scale_y_continuous(name = paste("Score PC", second_PC, "(",variance[second_PC],"%) variance explained") ,
                           limits =1.01*c(-max(abs(scores[,second_PC])),max(abs(scores[,second_PC])))
        ) +
        scale_x_continuous(name = paste("Score PC", first_PC, "(",variance[first_PC],"%) variance explained"),
                           limits =1.01*c(-max(abs(scores[,first_PC])),max(abs(scores[,first_PC])))
        ) +


        scale_color_discrete(name = paste(color_variable_name)) +
        scale_size_continuous(name = paste(size_variable_name)) +
        scale_shape_discrete(name = paste(shape_variable_name)) +

        theme_bw() +
        theme(panel.grid.major = element_blank(),  ## remove grid
              panel.grid.minor = element_blank())  ## remove grid
      plot_object$scoreloading_plot<-scoreloading_plot

    }
  }





  }


  plot_object$pca_pbject<-pca_object



  return(plot_object)
}
