#' To make a dataframe all numeric or all factor,or all character
#'
#' @param Dataframe  A data frame
#' @param transform_all  "numeric","factor","character"
#' @return A data frame that is all numeric
#' @export
#'
#' @examples
#' See example under triPlot()
force_type <- function(Dataframe,
                        transform_all=c("numeric","factor","character")) {

 # if(is.vector(Dataframe)){
#    nam<-names(Dataframe)
 #   Dataframe<-as.data.frame(Dataframe)
#    colnames(Dataframe)<-nam
 # }else{
    nam<-colnames(Dataframe)
    Dataframe<-as.data.frame(Dataframe)
    colnames(Dataframe)<-nam
#  }
  if(!is.data.frame(Dataframe)){
    stop("The input should be an dataframe")
  }
  if(!transform_all%in%c("numeric","factor","character")){

    stop("Wrong option")
  }
  ss<-matrix(NA,nrow=nrow(Dataframe),ncol=0)
  ss<-as.data.frame(ss)
  if(transform_all=="numeric"){
    for(i in 1:ncol(Dataframe)){
      if(class(Dataframe[,i])[1]%in%c("factor","ordinal","logical","integer","numeric")){
        a<-as.numeric(Dataframe[,i])
        ss<-cbind(ss,a)
      }else if(class(Dataframe[,i])[1]%in%c("character")){
        a<-as.numeric(as.factor(Dataframe[,i]))
        ss<-cbind(ss,a)

      } else if(class(Dataframe[,i])[1]%in%c("tbl_df")){
        kk<-as.data.frame(Dataframe[,i])[,1]
        if(is.character(kk)){
          a<-as.numeric(as.factor(kk))
        }else{
          a<-as.numeric(kk)
        }

        ss<-cbind(ss,a)

      }

    }
    colnames(ss)<-colnames(Dataframe)
  }

  if(transform_all=="factor"){
    for(i in 1:ncol(Dataframe)){
      if(class(Dataframe[,i])[1]%in%c("factor","ordinal")){
        a<-Dataframe[,i]
        ss<-cbind(ss,a)
      }else if(class(Dataframe[,i])[1]%in%c("logical","integer","numeric","character")){
        a<-as.factor(Dataframe[,i])
        ss<-cbind(ss,a)

      }else if(class(Dataframe[,i])[1]%in%c("tbl_df")){
        kk<-as.data.frame(Dataframe[,i])[,1]
        if(is.character(kk)){
          a<-as.factor(kk)
        }else{
          a<-as.factor(kk)
        }

        ss<-cbind(ss,a)
      }

    }
    colnames(ss)<-colnames(Dataframe)
  }
  if(transform_all=="character"){
    for(i in 1:ncol(Dataframe)){
      a<-as.character(as.data.frame(Dataframe[,i])[,1])
      ss<-cbind(ss,a)

    }
    colnames(ss)<-colnames(Dataframe)
  }
  return(ss)
}
