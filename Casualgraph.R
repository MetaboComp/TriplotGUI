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
  a<-menu(c("Yes","No"),
          graphics = T,
          "Are you very clear about which variable is what (exposure, outcome, mediator, confounder or effect modifier)?")
  if(a==2){stop("Please think about which variable ")}
  if(is.null(exposure)&is.null(outcome)){

  stop("must have exposure or outcome")
  }




}


DiagrammeR::grViz("
diagram=digraph {
  graph [ranksep = 0.2]

  node [shape = plaintext]
    exposure    [label = 'Exposure',shape=box]
    outcome    [label = 'Outcome',shape=box]

    mediator  [label = 'Mediator',shape=box]
    effect_modifier [label = 'Effect modifier',shape=box]
    covariate_EM  [label = 'Covariate EM',shape=box]
    covariate_EO  [label = 'Covariate EO', shape = box]
    covaraite_MO  [label = 'Covariate MO',shape=box]
  edge [minlen = 2]
    exposure->outcome

    exposure->mediator->outcome
    covariate_EO->exposure
covariate_EO->outcome
covariate_EM->mediator
covariate_EM->exposure
covariate_MO->mediator
covariate_MO->outcome

  { rank = same; A; Y }
}
")
