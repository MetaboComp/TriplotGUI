#' rotate around the center, a specific x row or specific y  columns
#'
#' @param Dataframe a dataframe
#' @param rotate_around_x  one x column that the matrix will rotate around, coud be a number or column names
#' @param rotate_around_y  one y column that the matrix will rotate around, coud be a number or column names
#' @return Detailed information and heatmap of correlations and risks in TriPlotObject
#' @export
#'
#'
rotate_around_xory<-function(Dataframe,
                             rotate_around_x=NULL,
                             rotate_around_y=NULL){
  if(is.null(Dataframe)){
    result<-NULL
  }else{
  if(is.null(rotate_around_x)&is.null(rotate_around_y)){
    result=t(Dataframe)
  }
   if(!is.null(rotate_around_x)&is.null(rotate_around_y)){


       if(is.character(rotate_around_x)){
         if(!rotate_around_x%in%rownames(Dataframe)){
           stop("\n We do not have that row in your input")
         }
       }else if(is.numeric(rotate_around_x)){
         if(!rotate_around_x%in%c(1:nrow(Dataframe))){
           stop("\n We do not have that row in your input")
         }
       }

     if(is.character(rotate_around_x)){
       rotate_around_x<-which(rotate_around_x==rownames(Dataframe))
     }

     front<-Dataframe[1:rotate_around_x,]
     back<-Dataframe[-c(1:rotate_around_x),]
     front_new<-matrix(NA,
                       nrow=nrow(front),
                       ncol=ncol(front))
     front_new<-as.data.frame(front_new)
     back_new<-matrix(NA,
                       nrow=nrow(back),
                       ncol=ncol(back))
     back_new<-as.data.frame(back_new)

    for(i in 1:nrow(front)) {
      front_new[i,]<-front[nrow(front)-i+1,]

    }
     rownames(front_new)<-rev(rownames(front))


     for(i in 1:nrow(back)) {
       back_new[i,]<-back[nrow(back)-i+1,]

     }

     rownames(back_new)<-rev(rownames(back))

     result<-rbind(back_new,front_new)

     colnames(result)<-colnames(Dataframe)

   }
if(is.null(rotate_around_x)&!is.null(rotate_around_y)){


  if(is.character(rotate_around_y)){
    if(!rotate_around_y%in%colnames(Dataframe)){
      stop("\n We do not have that row in your input")
    }
  }else if(is.numeric(rotate_around_y)){
    if(!rotate_around_y%in%c(1:ncol(Dataframe))){
      stop("\n We do not have that row in your input")
    }
  }

  if(is.character(rotate_around_y)){
    rotate_around_y<-which(rotate_around_y==colnames(Dataframe))
  }


  front<-as.data.frame(Dataframe[,1:rotate_around_y])
  colnames(front)<-colnames(Dataframe)[1:rotate_around_y]
  back<-as.data.frame(Dataframe[,-c(1:rotate_around_y)])
  colnames(back)<-colnames(Dataframe)[-c(1:rotate_around_y)]
  front_new<-matrix(NA,
                    nrow=nrow(front),
                    ncol=ncol(front))
  front_new<-as.data.frame(front_new)
  back_new<-matrix(NA,
                   nrow=nrow(back),
                   ncol=ncol(back))
  back_new<-as.data.frame(back_new)

  for(i in 1:ncol(front)) {
    front_new[,i]<-front[,ncol(front)-i+1]

  }
  colnames(front_new)<-rev(colnames(front))


  for(i in 1:ncol(back)) {
    back_new[,i]<-back[,ncol(back)-i+1]

  }

  colnames(back_new)<-rev(colnames(back))

  result<-cbind(back_new,front_new)

  rownames(result)<-rownames(Dataframe)




}



if(!is.null(rotate_around_x)&!is.null(rotate_around_y)){

  if(is.character(rotate_around_x)){
    if(!rotate_around_x%in%rownames(Dataframe)){
      stop("\n We do not have that row in your input")
    }
  }else if(is.numeric(rotate_around_x)){
    if(!rotate_around_x%in%c(1:nrow(Dataframe))){
      stop("\n We do not have that row in your input")
    }
  }
  if(is.character(rotate_around_y)){
    if(!rotate_around_y%in%colnames(Dataframe)){
      stop("\n We do not have that row in your input")
    }
  }else if(is.numeric(rotate_around_y)){
    if(!rotate_around_y%in%c(1:ncol(Dataframe))){
      stop("\n We do not have that row in your input")
    }
  }

  if(is.character(rotate_around_x)){
    rotate_around_x<-which(rotate_around_x==rownames(Dataframe))
  }
  if(is.character(rotate_around_y)){
    rotate_around_y<-which(rotate_around_y==colnames(Dataframe))
  }


  ### rotate around x

  front<-Dataframe[1:rotate_around_x,]
  back<-Dataframe[-c(1:rotate_around_x),]
  front_new<-matrix(NA,
                    nrow=nrow(front),
                    ncol=ncol(front))
  front_new<-as.data.frame(front_new)
  back_new<-matrix(NA,
                   nrow=nrow(back),
                   ncol=ncol(back))
  back_new<-as.data.frame(back_new)

  for(i in 1:nrow(front)) {
    front_new[i,]<-front[nrow(front)-i+1,]

  }
  rownames(front_new)<-rev(rownames(front))


  for(i in 1:nrow(back)) {
    back_new[i,]<-back[nrow(back)-i+1,]

  }

  rownames(back_new)<-rev(rownames(back))

  result<-rbind(back_new,front_new)

  colnames(result)<-colnames(Dataframe)
## rename

  Dataframe<-result
#### rotate around y
  front<-as.data.frame(Dataframe[,1:rotate_around_y])
  colnames(front)<-colnames(Dataframe)[1:rotate_around_y]
  back<-as.data.frame(Dataframe[,-c(1:rotate_around_y)])
  colnames(back)<-colnames(Dataframe)[-c(1:rotate_around_y)]
  front_new<-matrix(NA,
                    nrow=nrow(front),
                    ncol=ncol(front))
  front_new<-as.data.frame(front_new)
  back_new<-matrix(NA,
                   nrow=nrow(back),
                   ncol=ncol(back))
  back_new<-as.data.frame(back_new)

  for(i in 1:ncol(front)) {
    front_new[,i]<-front[,ncol(front)-i+1]

  }
  colnames(front_new)<-rev(colnames(front))


  for(i in 1:ncol(back)) {
    back_new[,i]<-back[,ncol(back)-i+1]

  }

  colnames(back_new)<-rev(colnames(back))

  result<-cbind(back_new,front_new)

  rownames(result)<-rownames(Dataframe)


}
  }

  return(result)
}
