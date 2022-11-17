#' @param exposure
#' @param outcome
#' @param mediator These could be metabolomics data (How to map it in detail will be developed)
#' @param covariate_EM
#' @param covariate_EC
#' @param covariate_MC
#' @param effect_modifier
#'
#'

#' @return A TriPlotObject
#' @export
#'
#'
#' See example under triPlot()
Showdiagram<-function(exposure=NULL,
                      outcome=NULL,
                      mediator=NULL,
                      covariate_EM=NULL,
                      covariate_EC=NULL,
                      covariate_MC=NULL,
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




}


DiagrammeR::grViz("
digraph {

  graph [
       # ranksep = 0.2
       nodestep=1]
  node [shape = plaintext
       fontname=Helvetica
       penwidth=2.0]
    exposure    [label = 'Exposure',shape=box]
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



########################################################################################3
library(diagram)
data <- c(0, "'.47*'", 0,
          0, 0, 0,
          "'.36*'", "'.33* (.16)'", 0)
M<- matrix (nrow=3,
            ncol=3,
            byrow = TRUE,
            data=data)
plot<- plotmat (M, pos=c(1,2),
                name= c( "Math self-efficacy",
                         "Math ability",
                         "Interest in the math major"),
                box.type = "rect",
                box.size = 0.12,
                box.prop=0.5,
                curve=0)

######################################################################









}
