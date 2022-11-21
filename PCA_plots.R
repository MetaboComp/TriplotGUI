#' @param plottype scree score loading 2 options
#' @param pc_num number of components
#' @param scale true by default
#' @param center true by defaule
#' @param dataframe row as observations, column as variables
#' @param pc_type prcomp or principle

#' @param size_variable
#' @param size_variable_name
#' @param color_variable
#' @param color_variable_name
#' @param shape_variale
#' @param shape_variable_name
#' @param first_PC
#'  @param second_PC
#' @return A PCA plot
#' @export
#'
#' @examples
PCA_plots<-function(plottype=c("scree","score","loading","scoreloading"),
                    pc_type=c("prcomp","principle"),
                    pc_num=5,
                    scale=T,
                    center=T,
                    dataframe,

                    size_variable=0.3,
                    size_variable_name=NULL,
                    color_variable=NULL,
                    color_variable_name=NULL,
                    shape_variable=NULL,
                    shape_variable_name=NULL,
                    first_PC=1,
                    second_PC=2
){
  dataframe<-as.data.frame(dataframe)
  dataframe_numeric<-dataframe
  for(i in 1:ncol(dataframe)){
    dataframe_numeric[,i]<-as.numeric(dataframe[,i])
  }
  library(ggplot2)
  if(!pc_type%in%c("prcomp","principle")){
    stop("This method is not implemented")
  }
  if(!plottype%in%c("scree","score","loading","scoreloading")){
    stop("This method is not implemented")
  }
  if(is.null(color_variable_name)){color_variable_name<-"color_variable"}
  if(is.null(shape_variable_name)){shape_variable_name<-"shape_variable"}
  if(is.null(size_variable_name)){size_variable_name<-"size_variable"}


  if(pc_type=="prcomp"){
  pca <-prcomp(dataframe_numeric,
               scale. = scale,
               center=center)
  scores=pca$x## Extract scores
  loadings=pca$rotation## Extract loadings
  frac_var <- function(x){ x^2/sum(x^2)}

  if(plottype=="scree"){
    library(scales)
    library(dplyr)
    pca$sdev %>%
      as_tibble() %>%    ## Change the standard deviations into a tibble dataframe
      frac_var() %>%     ## calculate the variance explained by the component
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
  }
  if(plottype=="score"){

    scores <- pca$x %>%
      as.data.frame()
    for(i in 1:ncol(dataframe)){
      if(is.factor(dataframe[,i])){
        dataframe[,i]<-StatTools::factor_samesequence(dataframe[,i])
      }
    }
   ## A PCA plot for PC1 and PC2
    ggplot(scores,
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
      scale_x_continuous(name = paste("PC", first_PC)) +
      scale_y_continuous(name = paste("PC", second_PC)) +
      scale_color_discrete(name = paste(color_variable_name)) +
      scale_size_continuous(name = paste(size_variable_name)) +
      scale_shape_discrete(name = paste(shape_variable_name)) +

      theme_bw() +
      theme(panel.grid.major = element_blank(),  ## remove grid
            panel.grid.minor = element_blank())  ## remove grid

    }

  if(plottype=="loading"){
   loadings<- pca$rotation %>%
      as.data.frame()

   ggplot(loadings,
          aes(x=loadings[,first_PC],
              y=loadings [,second_PC] ))+
     geom_point() +
     geom_segment(aes(xend=loadings[,first_PC],  ## x positions dots to be connected (a vector)
                      yend=loadings [,second_PC]), ## y positions dots to be connected (a vector)
                  x=0, ## Starting x position of lines
                  y=0, ## Starting y position of lines
                  color="Grey") +  ## color of the line
     geom_label(aes(x=loadings[,first_PC],
                    y=loadings[,second_PC],
                    label=rownames(loadings)),   ## Add label
                size=2,
                vjust="outward") +    ## The label is adjusted outwarded
     scale_x_continuous(name = paste("PC", first_PC)) +
     scale_y_continuous(name = paste("PC", second_PC)) +
     #scale_color_discrete(name = paste(color_variable_name)) +
     #scale_size_continuous(name = paste(size_variable_name)) +
     #scale_shape_discrete(name = paste(shape_variable_name)) +

     theme_bw() +
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())
  }

  if(plottype=="scoreloading"){
    scores <- pca$x %>%
      as.data.frame()
    loadings<- pca$rotation %>%
      as.data.frame()
    for(i in 1:ncol(dataframe)){
      if(is.factor(dataframe[,i])){
        dataframe[,i]<-StatTools::factor_samesequence(dataframe[,i])
      }
    }
    ## A PCA plot for PC1 and PC2
    ggplot(scores,
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
                       yend=loadings[,second_PC]), ## y positions dots to be connected (a vector)
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
      scale_x_continuous(name = paste("PC", first_PC)) +
      scale_y_continuous(name = paste("PC", second_PC)) +
      scale_color_discrete(name = paste(color_variable_name)) +
      scale_size_continuous(name = paste(size_variable_name)) +
      scale_shape_discrete(name = paste(shape_variable_name)) +

      theme_bw() +
      theme(panel.grid.major = element_blank(),  ## remove grid
            panel.grid.minor = element_blank())  ## remove grid

  }

  }

  if(pc_type=="principal"){
    library(psych)
  }
  plot_object<-list()
  plot_object$pca<-pca
  return(plot_object)
}
