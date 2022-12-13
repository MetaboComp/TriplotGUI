#' Scan the data frame by each column and see which one is. transformation and


#' @param Dataframe rows as observations columns as
#' @param Group Group vairable if you want to separate one variable to different groups to examine normality
#' @param perform_transform log, or square root ot cube root, or scale to unit
#' @param figure select figure is T only when there are small number of variables to show
#' @param p_threshold a p value, usually set as 0.05
#' @export
#' @return A vector with adjusted partial correlations
#'


normal_scan<-function(Dataframe,
                      Group=NULL,
                      figure=F,
                      p_threshold=0.05,
                      perform_transform=c("none",
                                          "log_specfic",
                                          "log_all",
                                          "scale_specific",
                                          "scale_all",
                                          "squareroot_all",
                                          "squareroot_specific")
                      ){
  result<-list()


  if(!perform_transform%in%c("none","log_specfic","log_all","scale_specific",
                             "scale_all","squareroot_all","squareroot_specific")){
    stop("This type of transformation not supported")}


  if(is.null(colnames(Dataframe))){
    stop("You must have column names to identify variables")
  }

  library(ggplot2)
  library(ggpubr)
  ########################## check to ensure the variabel type is numeric
  for (i in 1:ncol(Dataframe)){
    if(!class(Dataframe[,i])%in%c("integer","numeric")){

      stop("You must have each column in the input data as numeric")
    }
  }


  ################################################# check the group variable, it should be numeric
  if(!is.null(Group)){
    if(is.factor(Group)==F){
      stop("Group variable should be a factor variable")
    }
  }


  ####################################################
  ### Normality test on the original data frame

  if(is.null(Group)){
    approved_shapiro<-vector()
    approved_kolmogorov<-vector()
    pvalue_shapiro<-vector()
    pvalue_kolmogorov<-vector()
    for(i in 1:ncol(Dataframe)){
      if(length(unique(Dataframe[,i]))!=1){
      test_shapiro<-shapiro.test(Dataframe[,i])
      test_kolmogorov<-ks.test(Dataframe[,i],"pnorm")
      pvalue_shapiro<-c(pvalue_shapiro,
                        test_shapiro$p.value)
      pvalue_kolmogorov<-c(pvalue_kolmogorov,
                           test_kolmogorov$p.value)
      }
    }
    padj_shapiro<-p.adjust(pvalue_shapiro,
                           method = "fdr",
                           n=length(pvalue_shapiro))
    padj_kolmogorov<-p.adjust(pvalue_kolmogorov,
                              method = "fdr",
                              n=length(pvalue_kolmogorov))
    for(i in 1:ncol(Dataframe)){
      if(padj_shapiro[i]>p_threshold){
        approved_shapiro<-c(approved_shapiro,colnames(Dataframe)[i])
      }
      if(padj_kolmogorov[i]>p_threshold){
        approved_kolmogorov<-c(approved_kolmogorov,colnames(Dataframe)[i])
      }
    }
    cat("\n Out of", ncol(Dataframe),"variables in your original data,",
        length(approved_shapiro),"variables pass the Shapiro test that checks normality,\n",
        length(approved_kolmogorov),"variables pass the Kolmogorov- Smirnov test that checks normality,\n")
    result$originaldata<-Dataframe
    result$approved_shapiro<-approved_shapiro
    result$approved_kolmogorov<-approved_kolmogorov
  }



  #############################################################################################
  ###################################################################????????
  if(!is.null(Group)){
    approved_shapiro<-vector()
    approved_kolmogorov<-vector()
    pvalue_shapiro<-matrix(0,
                           nrow=ncol(Dataframe),
                           ncol=length(unique(Group)))
    pvalue_kolmogorov<-matrix(0,
                              nrow=ncol(Dataframe),
                              ncol=length(unique(Group)))
    for(j in 1:length(unique(Group))){
      #ii<-1
      #i<-1
      for( i in 1:ncol(Dataframe)){
      #while(i<ncol(Dataframe)){
        #i<-ii

        #for(m in 1:length(Group)){
        #tryCatch(  {ii<-i
        if(length(unique(Dataframe[Group==unique(Group)[j],i]))!=1){
        test_shapiro<-shapiro.test(Dataframe[Group==unique(Group)[j],i])
        test_kolmogorov<-ks.test(Dataframe[Group==unique(Group)[j],i],"pnorm")

        pvalue_shapiro[i,j]<-test_shapiro$p.value
        pvalue_kolmogorov[i,j]<- test_kolmogorov$p.value
        #ii<-i+1
        }
      }

    }
    padj_shapiro<-pvalue_shapiro
    padj_kolmogorov<-pvalue_kolmogorov
    for(j in 1:length(unique(Group))){
      padj_shapiro[,j]<-p.adjust(pvalue_shapiro[,j],
                                 method = "fdr",
                                 n=length(pvalue_shapiro[,j]))
      padj_kolmogorov[,j]<-p.adjust(pvalue_kolmogorov[,j],
                                    method = "fdr",
                                    n=length(pvalue_kolmogorov[,j]))
    }

    for(i in 1:ncol(Dataframe)){
      if(length(table(padj_shapiro[i,]>p_threshold))==1&names(table(padj_shapiro[i,]>p_threshold))[1]=="TRUE"){
        approved_shapiro<-c(approved_shapiro,colnames(Dataframe)[i])
      }
      if(length(table(padj_kolmogorov[i,]>p_threshold))==1&names(table(padj_kolmogorov[i,]>p_threshold))[1]=="TRUE"){
        approved_kolmogorov<-c(approved_kolmogorov,colnames(Dataframe)[i])
      }
    }
    cat("\n Out of", ncol(Dataframe),"variables in your original data,",
        length(approved_shapiro),"variables pass the Shapiro test that checks normality in all groups,\n",
        length(approved_kolmogorov),"variables pass the Kolmogorov- Smirnov test that checks normality in all groups,\n")
  #  if(length(approved_shapiro)==ncol(Dataframe)){
  #    perform_transform=="none"
  #  }

    result$originaldata<-Dataframe
    result$approved_shapiro<-approved_shapiro
    result$approved_kolmogorov<-approved_kolmogorov
  }
  #############################################################################################
  ###################################################################????????






    if(missing(perform_transform)){
      perform_transform="none"}
    ##############################################
    ################perform transform
    if(perform_transform=="log_all"){
      Dataframe<-log(Dataframe)
      result$transformeddata<-Dataframe
    } else if(perform_transform=="scale_all"){
      Dataframe<-scale(Dataframe)
      result$transformeddata<-Dataframe
    } else if(perform_transform=="squareroot_all"){
      Dataframe<-sqrt(Dataframe)
      result$transformeddata<-Dataframe
    } else if(perform_transform=="log_specific"){
      for(i in nrow(Dataframe)){
        if(!colnames%in%approved_shapiro&!colnames%in%approved_kolmogorov){  ###if it did not pass neither of the test
        Dataframe[,i]<-log(Dataframe[,i])
        }
      }
      result$transformeddata<-Dataframe
    } else if(perform_transform=="scale_specific"){
      for(i in nrow(Dataframe)){
        if(!colnames%in%approved_shapiro&!colnames%in%approved_kolmogorov){
          Dataframe[,i]<-scale(Dataframe[,i])
        }
      }
      result$transformeddata<-Dataframe
    } else if(perform_transform=="squareroot_specific"){
      for(i in nrow(Dataframe)){
        if(!colnames%in%approved_shapiro&!colnames%in%approved_kolmogorov){
          Dataframe[,i]<-sqrt(Dataframe[,i])
        }
      }
      result$transformeddata<-Dataframe
    }




 ####################################################
    ### Normality test on the transformed data
   if(perform_transform!="none"){
    if(is.null(Group)){
      approved_shapiro<-vector()
      approved_kolmogorov<-vector()
      pvalue_shapiro<-vector()
      pvalue_kolmogorov<-vector()
      for(i in 1:ncol(Dataframe)){
        if(length(unique(Dataframe[,i]))!=1){
        test_shapiro<-shapiro.test(Dataframe[,i])
        test_kolmogorov<-ks.test(Dataframe[,i],"pnorm")
        pvalue_shapiro<-c(pvalue_shapiro,
                          test_shapiro$p.value)
        pvalue_kolmogorov<-c(pvalue_kolmogorov,
                             test_kolmogorov$p.value)
        }
      }
      padj_shapiro<-p.adjust(pvalue_shapiro,
                                method = "fdr",
                                n=length(pvalue_shapiro))
      padj_kolmogorov<-p.adjust(pvalue_kolmogorov,
                                method = "fdr",
                                n=length(pvalue_kolmogorov))
      for(i in 1:ncol(Dataframe)){
        if(padj_shapiro[i]>p_threshold){
          approved_shapiro<-c(approved_shapiro,colnames(Dataframe)[i])
          }
        if(padj_kolmogorov[i]>p_threshold){
          approved_kolmogorov<-c(approved_kolmogorov,colnames(Dataframe)[i])
        }
      }
         cat("\n Out of", ncol(Dataframe),"variables in the transformed data,",
        length(approved_shapiro),"variables pass the Shapiro test that checks normality,\n",
        length(approved_kolmogorov),"variables pass the Kolmogorov- Smirnov test that checks normality,\n")
      result$approved_shapiro<-approved_shapiro
      result$approved_kolmogorov<-approved_kolmogorov
    }



#############################################################################################
###################################################################????????
    if(!is.null(Group)){
      approved_shapiro<-vector()
      approved_kolmogorov<-vector()
      pvalue_shapiro<-matrix(0,
                             nrow=ncol(Dataframe),
                             ncol=length(unique(Group)))
      pvalue_kolmogorov<-matrix(0,
                                nrow=ncol(Dataframe),
                                ncol=length(unique(Group)))
     for(j in 1:length(unique(Group))){
       for(i in 1:ncol(Dataframe)){
          #for(m in 1:length(Group)){
         if(length(unique(Dataframe[Group==unique(Group)[j],i]))!=1){
        test_shapiro<-shapiro.test(Dataframe[Group==unique(Group)[j],i])
        test_kolmogorov<-ks.test(Dataframe[Group==unique(Group)[j],i],"pnorm")
        pvalue_shapiro[i,j]<-test_shapiro$p.value
        pvalue_kolmogorov[i,j]<- test_kolmogorov$p.value
        }
        }

     }
      padj_shapiro<-pvalue_shapiro
      padj_kolmogorov<-pvalue_kolmogorov
      for(j in 1:length(unique(Group))){
      padj_shapiro[,j]<-p.adjust(pvalue_shapiro[,j],
                             method = "fdr",
                             n=length(pvalue_shapiro[,j]))
      padj_kolmogorov[,j]<-p.adjust(pvalue_kolmogorov[,j],
                                method = "fdr",
                                n=length(pvalue_kolmogorov[,j]))
      }

      for(i in 1:ncol(Dataframe)){
        if(length(table(padj_shapiro[i,]>p_threshold))==1&names(table(padj_shapiro[i,]>p_threshold))[1]=="TRUE"){
          approved_shapiro<-c(approved_shapiro,colnames(Dataframe)[i])
        }
        if(length(table(padj_kolmogorov[i,]>p_threshold))==1&names(table(padj_kolmogorov[i,]>p_threshold))[1]=="TRUE"){
          approved_kolmogorov<-c(approved_kolmogorov,colnames(Dataframe)[i])
        }
      }
      cat("\n Out of", ncol(Dataframe),"variables in the transformed data,",
          length(approved_shapiro),"variables pass the Shapiro test that checks normality in all groups,\n",
          length(approved_kolmogorov),"variables pass the Kolmogorov- Smirnov test that checks normality in all groups,\n")
      result$approved_shapiro<-approved_shapiro
      result$approved_kolmogorov<-approved_kolmogorov
    }
    #############################################################################################
    ###################################################################????????

   }






  if(figure==T){
  p<-list()
  if(!is.null(Group)){
  for(i in 1:ncol(Dataframe)){
   p[[i]]<-ggplot(data=cbind(Dataframe,Group),
         mapping=aes(x=Dataframe[,i]))+
      geom_density(aes(fill = Group),
                   alpha = 0.5,
                   show.legend=FALSE)+
     scale_x_continuous(name=colnames(Dataframe)[i])
  }
      ggarrange(plotlist=p)

  }

  if(is.null(Group)){
    for(i in 1:ncol(Dataframe)){
      p[[i]]<-ggplot(data=Dataframe,
           mapping=aes(x=Dataframe[,i]))+
      geom_density(aes(fill="red"),
                   alpha = 0.8,
                   show.legend=FALSE)+
        scale_x_continuous(name=colnames(Dataframe)[i])
    }
    ggarrange(plotlist=p)

  }
  names(p)<-colnames(Dataframe)
  result$figure<-p
  }


return(result)


}
