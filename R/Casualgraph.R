#' Casual diagram
#' @param exposure  A character vector or NULL
#' @param outcome   A character vector or NULL
#' @param mediator These could be metabolomics data (How to map it in detail will be developed)
#' @param covariate_EM  A character vector or NULL
#' @param covariate_EC  A character vector or NULL
#' @param covariate_MC  A character vector or NULL
#' @param effect_modifier  A character vector or NULL
#'
#'

#' @return A TriPlotObject
#' @export
#'
#'
## See example under triPlot()
Showdiagram<-function(exposure=NULL,  ## 1
                      outcome=NULL,   ## 2
                      mediator=NULL,  ## 3
                      covariate_EM=NULL, ## 4
                      covariate_EO=NULL, ## 5
                      covariate_MO=NULL, ## 6
                      effect_modifier=NULL){
  library(DiagrammeR)
  library(diagram)
  a<-menu(c("Yes","No"),
          graphics = T,
          "Are you very clear about which variable is what (exposure, outcome, mediator, confounder or effect modifier)?")
  if(a==2){stop("Please think about which variables are which ")}
  if(is.null(exposure)&is.null(outcome)){

  stop("must have exposure or outcome")
  }
  if(is.null(exposure)){
    exposure_label<-"Exposure"

  } else{exposure_label<-paste(exposure)}

  ########################################################################################3
  library(diagram)
  data <- c(0, 0,0,"'s'","'s'", 0,  ##4->1,5->1,
            "'v'", 0, "'v'",0,"'v'","'v'",          ##5->2,1->2, 6->2,3->2,
           "'a'",  0,0,"'a'",0, "'a'",  # 1->3 4->3 6->3
           0,0,0,0,0,0,              # 4->1 ,4->3
           0,0,0,0,0,0,              # 1->5 , 2->5
           0,0,0,0,0,0            # 6->2, 6->3
            )
  M<- matrix (nrow=6,
              ncol=6,
              byrow = TRUE,
              data=data)
  plot<- plotmat (M,
                  pos=c(2,2,2),
                  name= c( "Exposure",
                           "Outcome",
                           "Mediator",
                           "Covariate EM",
                           "Covariate EO",
                           "Covariate MO"),
                  box.type = "rect",
                  box.size = 0.12,
                  box.prop=0.5,
                  curve=0)
}
  ######################################################################


######another option
DiagrammeR::grViz(
  "digraph casual {

  graph [
       # ranksep = 0.2
       nodestep=1]
  node [shape = plaintext
       fontname=Helvetica
       penwidth=1.0]    ## the frame line width
    exposure    [label = 'Exposure', shape=box]
    outcome    [label = 'Outcome',shape=box]
    mediator  [label = 'Mediator',shape=box]
    effect_modifier [label = 'Effect modifier',shape=box]
    covariate_EM  [label = 'Covariate EM',shape=box]
    covariate_EO  [label = 'Covariate EO', shape = box]
    covariate_MO  [label = 'Covariate MO',shape=box]
  edge [minlen = 2
       arrowhead=diamond]
    exposure->outcome

    exposure->mediator->outcome
    covariate_EO->exposure
    covariate_EO->outcome
   covariate_EM->mediator
   covariate_EM->exposure
   covariate_MO->mediator
   covariate_MO->outcome

  { rank = same; exposure; outcome; mediator }
}
")









