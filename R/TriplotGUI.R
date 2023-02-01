#' Produce a triplot with loadings, correlations and risk estimates
#'
#' @param TPObject A TriPlotObject
#' @param first_PC The first PC to plot
#' @param second_PC The second PC to plot
#' @param loadLabels Whether to plot variable loading labels (TRUE; default) or not (FALSE)
#' @param loadCut Loadings below the cut are plotted in light grey and without labels
#' @param loadArrowLength Length of arrow tip (in inces)
#' @param plotLoads Whether to plot loadings (TRUE; default) or suppress them (FALSE)
#' @param loadLim Plot range for loadings
#' @param colCorr Color vector for correlations
#' @param pchCorr Plotting character for correlations
#' @param whichCorr Which correlations to plot (vector of numbers)
#' @param plotCorr Whether to plot correlations (TRUE; default) or suppress them (FALSE)
#' @param corLim Plot range for correlations
#' @param colRisk Color vector for risk estimates
#' @param pchRisk Plotting character for risk estimates
#' @param whichRisk Which risk estimates to plot (vector of numbers)

#' @param riskOR Specify whether to antilog risk layer scale (useful for log:ed risk estimates)
#' @param riskWhisker_percentage default is 1/10 of the confidence interval
#' @param plotRisk Whether to plot risk estimates (TRUE; default) or suppress them (FALSE)
#' @param riskLim Plot range for risks
#' @param plotScores Whether to plot scores (TRUE) or suppress them (FALSE; default)
###@param scoreLabels Whether to plot observation score labels (TRUE) or not (FALSE; default)
#' @param size font size of the name of correlation and risk variables
#' @return A triplot obtained using base R plotting.
#' @export
#'
#' @examples
#' # Make a PCA
#' pca <- prcomp(myData, scale.=TRUE, center=TRUE)
#' # Prepare a TriPlotObject (TPO)
#' tpo <- makeTPO(scores=pca$x, loadings=pca$rotation, compLimit=10)
#' # Make a correlation matrix with TPO scores
#' # NB: This step can be achieved in other ways, e.g. by making partial correlations.
#' corMat <- makeCorr(tpo, corrData)
#' # Add correlations to TPO
#' tpo <- addCorr(tpo, corMat) # More correlations can be added to TPO by further calls to addCorr
#' # Make risk modelling
#' # NB: This step can be achieved in other ways, e.g. by standard logistic correlations or by adjusting for other factors/covariates.
#' risk <- crudeCLR(tpo, caseStat, pairs)
#' # Add risk estimates to TPO
#' tpo <- addRisk(tpo,risk,'crudeOR')
#' # Make a heatmap to check which components may be most interesting to examine
#' checkTPO(tpo)
#' # Produce a triplot
#' triPlot(tpo) # Standard triplot for components 1 and 2.
#' triPlot(tpo, comps=c(3,5)) # Standard triplot for components 3 and 5.
#' # See tutorial at triplot repository for more examples
TriplotGUI <- function(TPObject,
                    first_PC=1,   ## The first PC to map
                    second_PC=2,   ## The first PC to map
                    plotLoads=TRUE,   ##Whether to plot loadings (TRUE; default) or suppress them (FALSE)
                    plotScores=FALSE,   ##Whether to plot scores (TRUE) or suppress them (FALSE; default)
                    plotCorr=TRUE,   ##Whether to plot correlations (TRUE; default) or suppress them (FALSE)
                    plotRisk=TRUE,      ##Whether to plot risk estimates (TRUE; default) or suppress them (FALSE)


                    ##For loadings
                    loadLabels=TRUE,   ###Whether to plot variable loading labels (TRUE; default) or not (FALSE)
                    loadArrowLength=0.02,   ###Length of arrow tip , set it as 0 if you want to remove it
                    loadCut=0,    ###lower limit Loadings below the cut are plotted in light grey and without label
                    loadLim,   ##higher limit,Plot range for loadings

                    ##For correlations
                    colCorr,   ##Color vector for correlations
                    pchCorr=16,   ##Plotting character for correlations
                    whichCorr=NULL,   ##Which correlations to plot (vector of numbers)
                    corLim,     ##Plot range for correlations

                    ##For risks
                    colRisk,    ##Color vector for risk estimates
                    pchRisk=15,    ##Plotting character for risk estimates
                    whichRisk=NULL,  ##Which risk estimates to plot (vector of numbers)
                    riskLim,            ##Plot range for risks
                    riskWhisker_percentage=0.1,  ## whisker length is how many percentage of confidence interval (This is only for the visualization purpose)
                     size=3,

                    riskOR=T  ##Specify whether to antilog risk layer scale (useful for log:ed risk estimates)

                    ## Scores
                     # scoreLabels=FALSE  ##Whether to plot observation score labels (TRUE) or not (FALSE; default)


) {
  library(ggplot2)
  library(ggpubr)
  result<-list()
  ##one thing to not ice that when you increase the margin it shrinks the real images
  ##mai: A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
  ##mar: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1
  ##oma: A vector of the form c(bottom, left, top, right) giving the size of the outer margins in lines of text. When there is multiple figures
  ##new: to open a new plot
  ##mfcol, mfrow: =c(,)


  #############################################################################
  # Plot loadings (if not suppressed)
  if (plotLoads) {  ###Whether to plot loadings (TRUE; default) or suppress them (FALSE)
    ###logical, defaulting to FALSE. If set to TRUE, the next high-level plotting command (actually plot.new) should not clean the frame before drawing as if
    ###it were on a new device. It is an error (ignored with a warning) to try to use new = TRUE on a device that does not currently contain a high-level plot.
    loads <- TPObject$loadings[,c(first_PC,second_PC)] # extract loadings. This is set
    ###loads: rows are variabels, columns are components

    absLoads <- apply(loads,
                      1,
                      function(x) sqrt(sum(x^2))) # calculate absolute length of loadings for each vairable (across components)
    ##absLoads for variables, which means sqrt(loadingsPC1^2+loadingsPC2^2)
    Loadsshow <- absLoads>=loadCut # Select loadings longer than the cut
    ## Loadings below the cut are plotted in light grey and without label

    if(missing(loadLim)) {   ##Plot range for loadings
      rng_loading <- 1.1*loads %>% abs() %>% max()

    } else {
      rng_loading <- loadLim    ##Plot range for loadings
    }

    color=rep("Gray",nrow(loads))
    color[Loadsshow]="Black"
    loadnames=rownames(loads)

    for(i in 1:length(loadnames)){
      if(!Loadsshow[i]){loadnames[i]=""}
    }


    p1<-ggplot()+
      #geom_point() +
      geom_segment(aes(xend=loads[,1],  ## x positions dots to be connected (a vector)
                       yend=loads [,2]), ## y positions dots to be connected (a vector)
                   x=0, ## Starting x position of lines
                   y=0, ## Starting y position of lines
                   color=color,   ## color of the line
                   arrow = arrow(angle=25,
                                 length = unit(loadArrowLength, "npc"),
                                 ends = "last",
                                 type="open"))
    if(loadLabels==T){
      p1<-p1+geom_text_repel(aes(x=loads[,1],
                           y=loads[,2],
                           label=loadnames),   ## Add label
                       size=size,
                       max.overlaps = Inf,
                       fontface = "italic",
                       vjust="outward"           ## The label is adjusted outwarded
      )

    }


    ####get an object with no label, for the final visualization purpose



      p2=p1+geom_vline(xintercept = 0,
                 linetype=2) +
      geom_hline(yintercept = 0,
                 linetype=2)  +
      #geom_text(size = 3, check_overlap = T)  +
      scale_x_continuous(
         name = paste("Component", first_PC, "loadings"),
                         limits=c(-rng_loading,rng_loading)) +
      scale_y_continuous(
                         name = paste("Component", second_PC, "loadings"),
                         limits=c(-rng_loading,rng_loading)) +

      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm'))



      }else{

    loads <- TPObject$loadings[,c(first_PC,second_PC)] # extract loadings. This is set
    absLoads <- apply(loads,
                      1,
                      function(x) sqrt(sum(x^2))) # calculate absolute length of loadings for each vairable (across components)
     Loadsshow <- absLoads>=loadCut # Select loadings longer than the cut

     if(missing(loadLim)) {   ##Plot range for loadings
      rng_loading <- loads %>% abs() %>% max()

    } else {
      rng_loading <- loadLim    ##Plot range for loadings
    }

    color=rep("Gray",nrow(loads))
    color[Loadsshow]="Black"
    loadnames=rownames(loads)

    for(i in 1:length(loadnames)){
      if(!Loadsshow[i]){loadnames[i]=""}
    }
    p1<-ggplot()
    p2=p1+geom_vline(xintercept = 0,
                     linetype=2) +
      geom_hline(yintercept = 0,
                 linetype=2)  +
      #geom_text(size = 3, check_overlap = T)  +
      scale_x_continuous(
                     name = paste("Component", first_PC, "loadings"),
                         limits=c(-rng_loading,rng_loading)) +
      scale_y_continuous(
                          name = paste("Component", second_PC, "loadings"),
                         limits=c(-rng_loading,rng_loading)) +

      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm'))


  }

  p_PCA<-p2

  ########################################################################################################
  ##plot scores
  if (plotScores) {
    scores <- TPObject$scores[,c(first_PC,second_PC)]
    rng_score <- 1.1*scores %>% abs() %>% max()
   cat("\nScores are rescaled and added to the figure with axies.\n")

p2= p2+
  geom_point(aes(x=scores[,1]*rng_loading/rng_score,
                 y=scores[,2]*rng_loading/rng_score))+
  scale_x_continuous(
    name = paste("Component", first_PC, "loadings"),
                               limits =c(-rng_loading,rng_loading),
                               sec.axis = sec_axis(trans=~./(rng_loading/rng_score),
                                                   name =  paste("Component", first_PC, "Scores")  ,
                                                   breaks=waiver()
                                                   #              breaks =seq(-max(abs(loadings[,second_PC])),max(abs(loadings[,second_PC])),10)
                               )

)+
  scale_y_continuous(
   name = paste("Component", second_PC, "loadings"),
                     limits =c(-rng_loading,rng_loading),
                     sec.axis = sec_axis(trans=~./(rng_loading/rng_score),
                                     name =  paste("Component", second_PC, "Scores")  ,
                                         breaks=waiver()
                                         #              breaks =seq(-max(abs(loadings[,second_PC])),max(abs(loadings[,second_PC])),10)
                     )

  )
p_scores<-p2
  }



  ###############################################################################################
  # Plot correlations (if there are any & not suppressed)


  if (TPObject$nCorr>0 & plotCorr) {   ## if there is nCorr and plotCorr is true
    cat("\nCorrelations are rescaled and added to the figure with axies. ")
    if(is.null(whichCorr)){
      whichCorr=1:nrow(TPObject$corrMatrix)
    }

    if(!is.null(whichCorr)){

      if(is.character(whichCorr)){
        if(any(!whichCorr%in%c("all",rownames(TPObject$corrMatrix)))){
          stop("\n Some correlations are not orginally added, please recheck the names of variables")
        }
      }else if(is.numeric(whichCorr)){
        if(any(!whichCorr%in%c(1:nrow(TPObject$corrMatrix)))){
          stop("\n Some correlations are not orginally added, please recheck the names of variables")
        }
      }
    }

    ##rebuild corrMatrix basing on TPObject,

    corrMatrix <- TPObject$corrMatrix[whichCorr,c(first_PC,second_PC)]

    ##If missing,just color them all blue
    if(missing(colCorr)) {
      colCorr <- rep('blue',TPObject$nVar)[whichCorr]
    }else {
      if(length(colCorr)!=length(whichCorr)&length(colCorr)!=1){
        stop("col Corr length must be equal to the number of correlation selected")
      }
      if(length(colCorr)==1) {
        colCorr <- rep(colCorr,nrow(corrMatrix))
      }
    }


    if(missing(pchCorr)) {
      pchCorr <- rep(16,TPObject$nVar)[whichCorr]
    }else {
      if(length(pchCorr)!=length(whichCorr)&length(pchCorr)!=1){
        stop("pch Corr length must be equal to the number of Risk selected")
      }
      if(length(pchCorr)==1) {
        pchCorr <- rep(pchCorr,nrow(corrMatrix))
      }
    }

    ##If missing correlation matrix
    if(missing(corLim)) {
      rng_corr <- 1.1*max(abs(corrMatrix))

    } else {
      rng_corr <- corLim
    }

    p_corr<-ggplot()+geom_point(aes(x=corrMatrix[,1],
                                    y=corrMatrix[,2]),
                                size=size,
                                col=colCorr,
                                shape=pchCorr)+
      geom_text_repel(aes(x=corrMatrix[,1],
                          y=corrMatrix[,2],
                          label=rownames(corrMatrix)),   ## Add label
                      col=colCorr,
                      max.overlaps = Inf,

                      fontface = "italic",
                      size=size ,
                      vjust="outward"           ## The label is adjusted outwarded
      )+
      geom_vline(xintercept = 0,
                 linetype=2) +
      geom_hline(yintercept = 0,
                 linetype=2)  +
      scale_x_continuous(name = paste("Correlation with component", first_PC, "scores"),
                         limits=c(-rng_corr,rng_corr),
                         sec.axis =sec_axis(trans=~./1,
                                            name=paste("Correlation with component", first_PC, "scores")),
                         )+
      scale_y_continuous(name = paste("Correlation with component", second_PC, "scores"),
                         limits=c(-rng_corr,rng_corr),
                         sec.axis =sec_axis(trans=~./1,
                                            name=paste("Correlation with component", second_PC, "scores")),
                         ) +
      labs(x= paste("Correlation with component", first_PC, "scores"),
           y=paste("Correlation with component", second_PC, "scores"))+

      theme_classic()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.length=unit(-0.25, "cm"),
            #axis.ticks.margin=unit(0.5, "cm"),
            axis.title.x=element_text(colour="blue"),
            axis.title.y=element_text(colour="blue"),
            axis.line.y=element_line(colour="blue"),
            axis.line.x=element_line(colour="blue") ,
            axis.text.y =element_text(colour="blue"),
            axis.text.x=element_text(colour="blue"),
            axis.ticks.y = element_line(size=1,color='blue'),
            axis.ticks.x = element_line(size=1,color='blue'),
            plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm')
            )

    p3<-p2+geom_point(aes(x=corrMatrix[,1]*rng_loading/rng_corr ,
                          y=corrMatrix[,2]*rng_loading/rng_corr),
                      size=size,
                      col=colCorr,
                      shape=pchCorr)+
      geom_text_repel(aes(x=corrMatrix[,1]*rng_loading/rng_corr ,
                          y=corrMatrix[,2]*rng_loading/rng_corr,
                          label=rownames(corrMatrix)),   ## Add label
                          col=colCorr,
                          max.overlaps = Inf,
                          #pch=pchCorr,
                      fontface = "italic",
                      size=size ,
                      vjust="outward"           ## The label is adjusted outwarded
      )

    p2<-p3
    library(cowplot)
    corr_x_axis<-get_x_axis(p_corr)
    corr_y_axis<-get_y_axis(p_corr)
    corr_xl_axis <- get_plot_component(p_corr, "xlab-b")
    corr_yl_axis <- get_plot_component(p_corr, "ylab-r")

    empty<-ggplot()+
      theme_minimal()+
      labs(x= "",y="")


    p_final<-plot_grid(
      ggdraw(corr_xl_axis),
      empty,
      empty,
      ggdraw(corr_x_axis),
      empty,
      empty,
      p2,
      ggdraw(corr_y_axis),
      ggdraw(corr_yl_axis),
      align='hv',
      axis='tblr',
      ncol=3,
      rel_heights=c(0.05, 0.05, 0.9),
      rel_widths=c(0.9, 0.05, 0.05)
    )




    if(plotRisk==F|TPObject$nRisk==0){
      result$triplot<-p_final
     p_final
    }

}

  #####################################################################################################################
  # Plot risks (if there are any & not suppressed)
  if (TPObject$nRisk>0 & plotRisk) {  ### when there is nrisk and plotRisk
    cat("\nRisks are rescaled and added to the figure with axies. ")
    if(is.null(whichRisk)){
      whichRisk=1:nrow(TPObject$riskMatrix)
    }
    if(!is.null(whichRisk)){

      if(is.character(whichRisk)){
        if(any(!whichRisk%in%c("all",rownames(TPObject$riskMatrix)))){
          stop("\n Some correlations are not orginally added, please recheck the names of variables")
        }
      }else if(is.numeric(whichRisk)){
        if(any(!whichRisk%in%c(1:nrow(TPObject$riskMatrix)))){
          stop("\n Some correlations are not orginally added, please recheck the names of variables")
        }
      }
    }

    riskMatrix <- TPObject$riskMatrix[whichRisk,c(first_PC,second_PC),drop=F]


    ##When drop is FALSE, the dimensions of the object are kept. And it is not just a cosmetic difference.
    ##drop=F keep the original frame of the matrix, not transform to vector
    riskSE <- TPObject$riskSE[whichRisk,c(first_PC,second_PC),drop=F]
    ##If missing,just color them all blue

    if(riskOR==T){
      ### when riskOR is T, risk MAtrix is changed to OR the original coefficient is stored in riskMatrix_coef
      riskMatrix_coef<-riskMatrix
      riskMatrix<-exp(riskMatrix)
    }

    if(missing(colRisk)) {
      colRisk <- rep('red',TPObject$nVar)[whichRisk]
    }else {
      if(length(colRisk)!=length(whichRisk)&length(colRisk)!=1){
        stop("col Risk length must be equal to the number of Risk selected")
      }
      if(length(colRisk)==1) {
        colRisk <- rep(colRisk,nrow(riskMatrix))
      }
    }


    if(missing(pchRisk)) {
      pchRisk <- rep(15,TPObject$nVar)[whichRisk]
    }else {
      if(length(pchRisk)!=length(whichRisk)&length(pchRisk)!=1){
        stop("pch Risk length must be equal to the number of Risk selected")
      }
      if(length(pchRisk)==1) {
        pchRisk <- rep(pchRisk,nrow(riskMatrix))
      }
    }

    ##If missing correlation matrix
    if(missing(riskLim)) {
      if(riskOR==T){

        #rng_risk <- 1.1*max(abs(exp(riskMatrix_coef-riskSE)-1),
        #                    abs(exp(riskMatrix_coef+riskSE)-1))
        ### I need to make 1 in the middle
        rng_risk_min <- min(exp(riskMatrix_coef-riskSE))  ## the smallest left/bottom side
        rng_risk_max <- max(exp(riskMatrix_coef+riskSE))  ## the biggest right/up side
        rng_risk=1.1*max(rng_risk_max-1,1-rng_risk_min)   ## make 1 the center
      }else{
      rng_risk <- 1.1*max(abs(riskMatrix-riskSE),
                          abs(riskMatrix+riskSE))
      }
    } else {
      rng_risk <- riskLim
    }


   if(riskOR==T){
     ##plot risk matrix

     p_risk<-ggplot()+geom_point(aes(x=(riskMatrix[,1]),
                           y=(riskMatrix[,2])),
                       size=size,
                       col=colRisk,
                      shape=pchRisk)+
       geom_text_repel(aes(x=(riskMatrix[,1]) ,
                           y=(riskMatrix[,2]),
                           label=rownames(riskMatrix)),   ## Add label
                       size=size ,
                       max.overlaps = Inf,

                       col=colRisk,
                       # pch=pchRisk,
                       fontface = "italic",
                       vjust="outward"           ## The label is adjusted outwarded
       )+
       geom_vline(xintercept = 1,
                  linetype=2) +
       geom_hline(yintercept = 1,
                  linetype=2)  +
       scale_x_continuous(name = paste("PCA odds ratio for component", first_PC, "scores"),
                          limits=c(1-rng_risk,1+rng_risk),
                          sec.axis =sec_axis(trans=~./1,
                                             name=paste("PCA odds ratio for component", first_PC, "scores")),

                          )+
       scale_y_continuous(name = paste("PCA odds ratio for component", second_PC, "scores"),
                          limits=c(1-rng_risk,1+rng_risk),
                          sec.axis =sec_axis(trans=~./1,
                                             name=paste("PCA odds ratio for component", second_PC, "scores")),

                         )+


       labs(x= paste("PCA odds ratio for component", first_PC, "scores"),
            y=paste("PCA odds ratio for component", second_PC, "scores"))+

       theme_classic()+
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.ticks.length=unit(-0.25, "cm"),
             #axis.ticks.margin=unit(0.5, "cm"),
             axis.title.x=element_text(colour="red"),
             axis.title.y=element_text(colour="red"),
             axis.line.y=element_line(colour="red"),
             axis.line.x=element_line(colour="red") ,
             axis.text.y =element_text(colour="red"),
             axis.text.x=element_text(colour="red"),
             axis.ticks.y = element_line(size=1,color='red'),
             axis.ticks.x = element_line(size=1,color='red'),
             plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm')
             )+geom_errorbarh(aes(xmin=(exp(riskMatrix_coef[,1]-riskSE[,1])),#*rng_loading/rng_risk,
                                  xmax=(exp(riskMatrix_coef[,1]+riskSE[,1])),#*rng_loading/rng_risk,
                                  y=(riskMatrix[,2])#*rng_loading/rng_risk
                                  #ymin=(riskMatrix[,2]-riskSE[,2])*rng_loading/rng_risk,
                                  #ymax=(riskMatrix[,2]+riskSE[,2])*rng_loading/rng_risk,

             ),
             height=(exp(riskMatrix_coef[,1]+riskSE[,1])-exp(riskMatrix_coef[,1]-riskSE[,1]))*riskWhisker_percentage*rng_loading/rng_risk,
             #position=position_dodge()

             )+
       geom_errorbar(aes(ymin=(exp(riskMatrix_coef[,2]-riskSE[,2])),#*rng_loading/rng_risk,
                         ymax=(exp(riskMatrix_coef[,2]+riskSE[,2])),#*rng_loading/rng_risk,
                         x=(riskMatrix[,1])#*rng_loading/rng_risk
                         #ymin=(riskMatrix[,2]-riskSE[,2])*rng_loading/rng_risk,
                         #ymax=(riskMatrix[,2]+riskSE[,2])*rng_loading/rng_risk,

       ),
       width=(exp(riskMatrix_coef[,2]+riskSE[,2])-exp(riskMatrix_coef[,2]-riskSE[,2]))*riskWhisker_percentage,#*rng_loading/rng_risk,
       #position=position_dodge()
       )


     p3<-p2+geom_point(aes(x=(riskMatrix[,1]-1)*rng_loading/rng_risk ,
                           y=(riskMatrix[,2]-1)*rng_loading/rng_risk),
                       size=size,
                       col=colRisk,
                       shape=pchRisk)+
       geom_text_repel(aes(x=(riskMatrix[,1]-1)*rng_loading/rng_risk ,
                           y=(riskMatrix[,2]-1)*rng_loading/rng_risk,
                           label=rownames(riskMatrix)),   ## Add label
                       size=size ,
                       max.overlaps = Inf,

                       col=colRisk,
                       #pch=pchRisk,
                       fontface = "italic",
                       vjust="outward"           ## The label is adjusted outwarded
       )

   }else{
    ##plot risk matrix
     p_risk<-ggplot()+geom_point(aes(x=riskMatrix[,1] ,
                                     y=riskMatrix[,2]),
                                 size=size,
                                 col=colRisk,
                                 shape=pchRisk)+
       geom_text_repel(aes(x=riskMatrix[,1],
                           y=riskMatrix[,2],
                           label=rownames(riskMatrix)),   ## Add label
                       size=size ,
                       max.overlaps = Inf,

                       col=colRisk,
                       #pch=pchRisk,
                       fontface = "italic",
                       vjust="outward"           ## The label is adjusted outwarded
       )+
       geom_vline(xintercept = 0,
                  linetype=2) +
       geom_hline(yintercept = 0,
                  linetype=2)  +
       scale_x_continuous(name = paste("PCA odds ratio for component", first_PC, "scores"),
                          limits=c(-rng_risk,rng_risk),
                          sec.axis =sec_axis(trans=~./1,
                                             name=paste("PCA odds ratio for component", first_PC, "scores")),

       )+
       scale_y_continuous(name = paste("PCA odds ratio for component", second_PC, "scores"),
                          limits=c(-rng_risk,rng_risk),
                          sec.axis =sec_axis(trans=~./1,
                                             name=paste("PCA odds ratio for component", second_PC, "scores")),

       )+


       labs(x= paste("PCA odds ratio for component", first_PC, "scores"),
            y=paste("PCA odds ratio for component", second_PC, "scores"))+

       theme_classic()+
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.ticks.length=unit(-0.25, "cm"),
             #axis.ticks.margin=unit(0.5, "cm"),
             axis.title.x=element_text(colour="red"),
             axis.title.y=element_text(colour="red"),
             axis.line.y=element_line(colour="red"),
             axis.line.x=element_line(colour="red") ,
             axis.text.y =element_text(colour="red"),
             axis.text.x=element_text(colour="red"),
             axis.ticks.y = element_line(size=1,color='red'),
             axis.ticks.x = element_line(size=1,color='red'),
             plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm')
             )+geom_errorbarh(aes(xmin=(riskMatrix[,1]-riskSE[,1]),#*rng_loading/rng_risk,
                                  xmax=(riskMatrix[,1]+riskSE[,1]),#*rng_loading/rng_risk,
                                  y=(riskMatrix[,2])#*rng_loading/rng_risk
                                  #ymin=(riskMatrix[,2]-riskSE[,2])*rng_loading/rng_risk,
                                  #ymax=(riskMatrix[,2]+riskSE[,2])*rng_loading/rng_risk,

             ),
             height=2*riskSE[,1]*riskWhisker_percentage,
             #position=position_dodge()

             )+
       geom_errorbar(aes(ymin=(riskMatrix[,2]-riskSE[,2]),#*rng_loading/rng_risk,
                         ymax=(riskMatrix[,2]+riskSE[,2]),#*rng_loading/rng_risk,
                         x=(riskMatrix[,1])#*rng_loading/rng_risk
                         #ymin=(riskMatrix[,2]-riskSE[,2])*rng_loading/rng_risk,
                         #ymax=(riskMatrix[,2]+riskSE[,2])*rng_loading/rng_risk,

       ),
       width=2*riskSE[,2]*riskWhisker_percentage,
       #position=position_dodge()
       )



    p3<-p2+geom_point(aes(x=riskMatrix[,1]*rng_loading/rng_risk ,
                          y=riskMatrix[,2]*rng_loading/rng_risk),
                      size=size,
                      col=colRisk,
                      shape=pchRisk)+
      geom_text_repel(aes(x=riskMatrix[,1]*rng_loading/rng_risk ,
                          y=riskMatrix[,2]*rng_loading/rng_risk,
                          label=rownames(riskMatrix)),   ## Add label
                      size=size ,
                      max.overlaps = Inf,
                      col=colRisk,
                      #pch=pchRisk,
                      fontface = "italic",
                      vjust="outward"           ## The label is adjusted outwarded
      )

   }
      p2<-p3

if(riskOR==T){
    ## add error bar
    p3<-p2+geom_errorbarh(aes(xmin=(exp(riskMatrix_coef[,1]-riskSE[,1])-1)*rng_loading/rng_risk,
                          xmax=(exp(riskMatrix_coef[,1]+riskSE[,1])-1)*rng_loading/rng_risk,
                          y=(riskMatrix[,2]-1)*rng_loading/rng_risk
                          #ymin=(riskMatrix[,2]-riskSE[,2])*rng_loading/rng_risk,
                          #ymax=(riskMatrix[,2]+riskSE[,2])*rng_loading/rng_risk,

    ),
    height=(exp(riskMatrix_coef[,1]+riskSE[,1])-exp(riskMatrix_coef[,1]-riskSE[,1]))*riskWhisker_percentage*rng_loading/rng_risk,
    #position=position_dodge()

    )+
      geom_errorbar(aes(ymin=(exp(riskMatrix_coef[,2]-riskSE[,2])-1)*rng_loading/rng_risk,
                         ymax=(exp(riskMatrix_coef[,2]+riskSE[,2])-1)*rng_loading/rng_risk,
                         x=(riskMatrix[,1]-1)*rng_loading/rng_risk
                         #ymin=(riskMatrix[,2]-riskSE[,2])*rng_loading/rng_risk,
                         #ymax=(riskMatrix[,2]+riskSE[,2])*rng_loading/rng_risk,

      ),
      width=(exp(riskMatrix_coef[,2]+riskSE[,2])-exp(riskMatrix_coef[,2]-riskSE[,2]))*riskWhisker_percentage*rng_loading/rng_risk,
      #position=position_dodge()
      )

}else{
  p3<-p2+geom_errorbarh(aes(xmin=(riskMatrix[,1]-riskSE[,1])*rng_loading/rng_risk,
                            xmax=(riskMatrix[,1]+riskSE[,1])*rng_loading/rng_risk,
                            y=(riskMatrix[,2])*rng_loading/rng_risk
                            #ymin=(riskMatrix[,2]-riskSE[,2])*rng_loading/rng_risk,
                            #ymax=(riskMatrix[,2]+riskSE[,2])*rng_loading/rng_risk,

  ),
  height=2*riskSE[,1]*riskWhisker_percentage,
  #position=position_dodge()

  )+
    geom_errorbar(aes(ymin=(riskMatrix[,2]-riskSE[,2])*rng_loading/rng_risk,
                      ymax=(riskMatrix[,2]+riskSE[,2])*rng_loading/rng_risk,
                      x=(riskMatrix[,1])*rng_loading/rng_risk
                      #ymin=(riskMatrix[,2]-riskSE[,2])*rng_loading/rng_risk,
                      #ymax=(riskMatrix[,2]+riskSE[,2])*rng_loading/rng_risk,

    ),
    width=2*riskSE[,2]*riskWhisker_percentage,
    #position=position_dodge()
    )


}
p2<-p3

library(cowplot)
risk_x_axis<-get_x_axis(p_risk)
risk_y_axis<-get_y_axis(p_risk)
risk_xl_axis <- get_plot_component(p_risk, "xlab-b")
risk_yl_axis <- get_plot_component(p_risk, "ylab-r")

if(plotRisk==T|TPObject$nRisk>0){
if(plotCorr==F|TPObject$nCorr==0){
  empty<-ggplot()+
    theme_minimal()+
    labs(x= "",y="")


  p_final<-plot_grid(
    ggdraw(risk_xl_axis),
    empty,
    empty,
    ggdraw(risk_x_axis),
    empty,
    empty,
    p2,
    ggdraw(risk_y_axis),
    ggdraw(risk_yl_axis),
    align='hv',
    axis='tblr',
    ncol=3,
    rel_heights=c(0.05, 0.05, 0.9),
    rel_widths=c(0.9, 0.05, 0.05)
  )
  p_final
  result$triplot<-p_final
} else {
  empty<-ggplot()+
    theme_minimal()+
    labs(x= "",y="")


  p_final<-plot_grid(
    ggdraw(risk_xl_axis),
    empty,
    empty,
    empty,
    empty,
    ggdraw(risk_x_axis),
    empty,
    empty,
    empty,
    empty,
    ggdraw(corr_xl_axis),
    empty,
    empty,
    empty,
    empty,
    ggdraw(corr_x_axis),
    empty,
    empty,
    empty,
    empty,
    p2,
    ggdraw(corr_y_axis),
    ggdraw(corr_yl_axis),
    ggdraw(risk_y_axis),
    ggdraw(risk_yl_axis),
    align='hv',
    axis='tblr',
    ncol=5,
    rel_heights=c(ifelse(plotScores,0.04,0.07),
                  0.04,0.04, 0.04, 0.84),
    rel_widths=c(0.84, 0.04, 0.04,0.04, 0.04),
    greedy=F
  )
  p_final
  result$triplot<-p_final
}
}



}


if(plotCorr==T&TPObject$nCorr>0){
  result$Correlation<-p_corr
}
if(plotRisk==T&TPObject$nRisk>0){
  result$Risk<-p_risk
}
if(plotScores==T){
  result$Scores<-p_scores
}
if(plotLoads){
result$PCA<-p_PCA
}
result$TPObject<-TPObject
return(result)

}




