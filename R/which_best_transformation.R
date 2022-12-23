#' Scan the data frame by each column and see which one is. transformation and


#' @param Dataframe rows as observations columns as
#' @param Group Group vairable if you want to separate one variable to different groups to examine normality
#' @param p_threshold a p value, usually set as 0.05
#'
#'
#' @export
#' @return best trnsformation type
#'
which_best_transformation<-function(Dataframe,
                                    Group=NULL,
                                    p_threshold=0.05

                                    ){
  none_object<-normal_scan(Dataframe=Dataframe,
                          Group=Group,
                          p_threshold=p_threshold,
                          perform_transform = "none")

  log_object<-normal_scan(Dataframe=Dataframe,
                          Group=Group,
                          p_threshold=p_threshold,
                          perform_transform = "log_all")

  ########################### Do I put scale as one of the option
  scale_object<-normal_scan(Dataframe=Dataframe,
                          Group=Group,
                          p_threshold=p_threshold,
                          perform_transform = "scale_all")
  squareroot_object<-normal_scan(Dataframe=Dataframe,
                            Group=Group,
                            p_threshold=p_threshold,
                            perform_transform = "squareroot_all")
  cubicroot_object<-normal_scan(Dataframe=Dataframe,
                            Group=Group,
                            p_threshold=p_threshold,
                            perform_transform = "cubicroot_all")
  winner= c("Not transforming", "Log transforming", "Scaling",
            "Squareroot root transforming","Subic root transforming")
  winnerlist<-list(none_object,log_object,scale_object,squareroot_object,cubicroot_object)

  place_shapiro<-which.min(c(mean(none_object$padj_shapiro),
            mean(log_object$transformed_padj_shapiro),
            mean(scale_object$transformed_padj_shapiro),
            mean(squareroot_object$transformed_padj_shapiro),
            mean(cubicroot_object$transformed_padj_shapiro)))
  place_kolmogorov<-which.min(c(mean(none_object$padj_kolmogorov),
                           mean(log_object$transformed_padj_kolmogorov),
                           mean(scale_object$transformed_padj_kolmogorov),
                           mean(squareroot_object$transformed_padj_kolmogorov),
                           mean(cubicroot_object$transformed_padj_kolmogorov)))
  result<-list()
  result$shapiro<-winnerlist[[place_shapiro]]
  result$kolmogorov<-winnerlist[[place_kolmogorov]]
  cat("\n",winner[place_shapiro]," gives smallest average p value according to Shapiro test")
  #cat(winner[place_kolmogorov]," gives smallest average p value according to Shapiro test")
  return(result)
}
