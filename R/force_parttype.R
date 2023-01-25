#' To make a dataframe all numeric or all factor,or all character
#'
#' @param Dataframe  A data frame
#' @param transform_part  "numeric","factor","character"
#' @param variable_names   The variable names that you want to do the transformation
#' @return A data frame that is all numeric
#' @export
#'
#' @examples
#' See example under triPlot()
force_parttype <- function(Dataframe,
                       transform_part=c("numeric","factor","character"),
                       variables_names
                       ) {

  if(is.vector(Dataframe)){
    nam<-names(Dataframe)
    Dataframe<-as.data.frame(Dataframe)
    colnames(Dataframe)<-nam
  }
  if(!is.data.frame(Dataframe)){
    stop("The input should be an dataframe")
  }
  if(any(!variables_names%in%colnames(Dataframe))){
    stop("Variable names should be inside column names of the dataframe")
  }
  if(!transform_part%in%c("numeric","factor","character")){

    stop("Wrong option")
  }
  ss<-matrix(NA,nrow=nrow(Dataframe),ncol=0)
  ss<-as.data.frame(ss)
  if(transform_part=="numeric"){
    for(i in 1:ncol(Dataframe)){
      if(class(Dataframe[,i])[1]%in%c("factor","ordinal","logical","integer","numeric")){
        if(colnames(Dataframe)[i]%in%variables_names)
        {a<-as.numeric(Dataframe[,i])
        ss<-cbind(ss,a)
        }else{
          a<-Dataframe[,i]
          ss<-cbind(ss,a)
        }

      }else if(class(Dataframe[,i])[1]%in%c("character")){
        if(colnames(Dataframe)[i]%in%variables_names){
        a<-as.numeric(as.factor(Dataframe[,i]))
        ss<-cbind(ss,a)
        }else{
          a<-Dataframe[,i]
          ss<-cbind(ss,a)
        }

      }

    }
    colnames(ss)<-colnames(Dataframe)
  }

  if(transform_part=="factor"){
    for(i in 1:ncol(Dataframe)){
      if(class(Dataframe[,i])[1]%in%c("factor","ordinal")){
        a<-Dataframe[,i]
        ss<-cbind(ss,a)
      }else if(class(Dataframe[,i])[1]%in%c("logical","integer","numeric","character")){
        if(colnames(Dataframe)[i]%in%variables_names){

         a<-as.factor(Dataframe[,i])
        ss<-cbind(ss,a)
        }else{
          a<-Dataframe[,i]
          ss<-cbind(ss,a)

        }
      }

    }
    colnames(ss)<-colnames(Dataframe)
  }
  if(transform_part=="character"){
    for(i in 1:ncol(Dataframe)){
      if(colnames(Dataframe)[i]%in%variables_names){
      a<-as.character(Dataframe[,i])
      ss<-cbind(ss,a)

      }else{
        a<-Dataframe[,i]
          ss<-cbind(ss,a)
      }
    }
    colnames(ss)<-colnames(Dataframe)
  }
  return(ss)
}
