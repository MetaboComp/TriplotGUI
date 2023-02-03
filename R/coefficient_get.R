#' Risk estimate. When the outcomee is binary, odds ratio is calculated.
#' When the outcomee is categorical (n groups >2), the outcomee is onehotencoding first and as n number odds rario is generated
#' When the outcomee is numeric, beta coefficient is calculated.
#'
#' @param TPObject A TriPlotObject
#'
#' @param outcomee  In the format of dataframe, could be binary, categorical (>2 groups) and numeric. It is recommeneded that you put outcomee one at a time
#' @param confounder In the format of dataframe,confounders that need to be adjusted, if it is not null. The colnames must be given
#' @param pair A factor variable Vector with case/control pairs of TPO observations (score rows) Used for the binary case control scenario
#' @param CI Confidence Interval for the risk estimate (defaults to 0.95)
#' @param multinomial logical, do multinomial regression on the categorical variables.
#' If this option is labeled T, the first option of the levels of the factor outcomee variable will be set as reference. So the users need to relevel() to set the reference group before using the function
#' @param partial do confounder adjustment or not
#' @return A list of risk estimation matrix with components in rows and (estimates, margin-of-error and p-values) in columns. The margin-of-error corresponds to half the width of the confidence interval (i.e. z * se).
#' @export
#'
#' @examples
#' See example under triPlot()
coefficient_get <- function(TPObject,
                     outcomee,
                     confounder=NULL,
                     partial=T,
                     multinomial=F,
                     pair=NULL,
                     CI=0.95){
  Risk<-list()
  alpha <- (1 - CI) / 2

  scores <- TPObject$scores   ##row is observations, column is components



  if(class(outcomee)!="data.frame"){
  stop("The outcomee should be put in as a dataframe.")
  }

  if(!is.null(confounder)){
  if(class(confounder)!="data.frame"){
    stop("The confounder should be put in as a dataframe.")

  }
  if(nrow(confounder)!=nrow(outcomee)){
    stop("The number of observations should be equal for the outcomee and confounder")
  }
  }

  if(!is.null(pair)){
  if(length(unique(pair))==length(pair)){
    stop("There must be pairs in the variable pair")
  }
    if(class(pair)!="factor"){
      stop("If pair variable exist, it must be presented as a factor variable")
    }
  }

  library(survival)
  cat("Sometimes if you have a categorical outcomee that one categorial has too few observations, error will occur.")

if(multinomial==F){
  outcomee_o<-onehotencoding(outcomee)  ## every variable is numeric after this. So the orginal factor variables needs to befactor again
  outcomee_t<-matrix(nrow=nrow(outcomee),ncol=0)
  outcomee_t<-as.data.frame(outcomee_t)

  for (i in 1:ncol(outcomee_o)){
    if(length(table(outcomee_o[,i]))==2){

        outcomee_t<-cbind.data.frame(outcomee_t,
                                    factor_samesequence(as.factor(outcomee_o[,i])))
    } else{outcomee_t<-cbind.data.frame(outcomee_t,outcomee_o[,i])}

  }
  colnames(outcomee_t)<-colnames(outcomee_o)

  factorrisk<-matrix(nrow=nrow(outcomee),ncol=0)
  factorrisk<-as.data.frame(factorrisk)
  numericrisk<-matrix(nrow=nrow(outcomee),ncol=0)
  numericrisk<-as.data.frame(numericrisk)
  colnames_factor<-c()
  colnames_numeric<-c()
  for (i in 1:ncol(outcomee_t)){
    if(class(outcomee_t[,i])=="factor"){
      colnames_factor<-c(colnames_factor,colnames(outcomee_t)[i])
      factorrisk<-cbind.data.frame(factorrisk,outcomee_t[,i])

    } else{
      colnames_numeric<-c(colnames_numeric,colnames(outcomee_t)[i])
      numericrisk<-cbind.data.frame(numericrisk,outcomee_t[,i])
    }

  }
  colnames(factorrisk)<-colnames_factor
  colnames(numericrisk)<-colnames_numeric

}

  if(multinomial==T){
    factorrisk<-matrix(nrow=nrow(outcomee),ncol=0)
    factorrisk<-as.data.frame(factorrisk)
    numericrisk<-matrix(nrow=nrow(outcomee),ncol=0)
    numericrisk<-as.data.frame(numericrisk)
    colnames_factor<-c()
    colnames_numeric<-c()
    for (i in 1:ncol(outcomee)){
      if(class(outcomee[,i])=="factor"){
        colnames_factor<-c(colnames_factor,colnames(outcomee)[i])
        factorrisk<-cbind.data.frame(factorrisk,outcomee[,i])

      } else{
        colnames_numeric<-c(colnames_numeric,colnames(outcomee)[i])
        numericrisk<-cbind.data.frame(numericrisk,outcomee[,i])
      }

    }

    colnames(factorrisk)<-colnames_factor
    colnames(numericrisk)<-colnames_numeric
  }
#######################################################################################################################

  if(dim(numericrisk)[2]!=0){
    numericrisk_list<-list()
    z <- abs(qnorm(alpha))   #qnorm is quantile function
    ##########################################################################################
    ###when there is no confounder and there is no paired infor
    if((is.null(confounder)&is.null(pair))|(!is.null(confounder)&is.null(pair)&partial==F)){



    for(i in 1:ncol(numericrisk)){
      numericrisk_list[[i]]<- matrix(nrow=TPObject$nComp,   ##row is component
                     ncol=3)

      colnames( numericrisk_list[[i]]) <- c('est','moe','p')

      rownames( numericrisk_list[[i]]) <- colnames(scores)  ###row is components



      for(j in 1:TPObject$nComp ){
        glm_object<-summary(glm(numericrisk[,i]~scores[,j] ),
                           family="gaussian")
        numericrisk_list[[i]][j,]<-glm_object $coefficients[2,c(1,2,4)]
        numericrisk_list[[i]][j,2] <- z * numericrisk_list[[i]][j,2]
      }

    }
      names(numericrisk_list)<-colnames(numericrisk)




    }

    ##########################################################################################
    ## When there is confounder and there is no paired infor
    if(!is.null(confounder)&is.null(pair)&partial==T){


      for(i in 1:ncol(numericrisk)){

        numericrisk_list[[i]]<- matrix(nrow=TPObject$nComp,   ##row is component
                                       ncol=3)

        colnames( numericrisk_list[[i]]) <- c('est','moe','p')

        rownames( numericrisk_list[[i]]) <- colnames(scores)  ###row is components

        data=cbind(scores,numericrisk,confounder)

        for(j in 1:TPObject$nComp ){


          glm_object<-summary(glm(formula =as.formula(paste(colnames(numericrisk)[i],'~',
                                                    colnames(scores)[j],"+",
                                                    paste(colnames(confounder),collapse="+"))),
            family="gaussian",
            data=data))
          numericrisk_list[[i]][j,]<-glm_object $coefficients[2,c(1,2,4)]
          numericrisk_list[[i]][j,2] <- z * numericrisk_list[[i]][j,2]
        }

      }
      names(numericrisk_list)<-colnames(numericrisk)

    }


      ##########################################################################################
      ###when there is no confounder and there is paired infor
      if((is.null(confounder)&!is.null(pair))|(!is.null(confounder)&!is.null(pair)&partial==F)){


        library(lme4)
        library(lmerTest)

        for(i in 1:ncol(numericrisk)){

          numericrisk_list[[i]]<- matrix(nrow=TPObject$nComp,   ##row is component
                                         ncol=3)

          colnames( numericrisk_list[[i]]) <- c('est','moe','p')

          rownames( numericrisk_list[[i]]) <- colnames(scores)  ###row is components

          data=cbind(scores,numericrisk)

          for(j in 1:TPObject$nComp ){

            ###

            lme_object<-lmer(formula =as.formula(paste(colnames(numericrisk)[i],'~',
                                                       colnames(scores)[j],"+",
                                                       "+","(1|pair)")),
                             data=data)
            lme_object_sum<-summary(lme_object)
            lme_object_ano<-anova(lme_object,test = 'Chisq')


            numericrisk_list[[i]][j,c(1,2)]<-lme_object_sum$coefficients[2,c(1,2)]
            numericrisk_list[[i]][j,3]<-lme_object_ano[1,6]
            ###
            numericrisk_list[[i]][j,2] <- z * numericrisk_list[[i]][j,2]


          }

        }
        names(numericrisk_list)<-colnames(numericrisk)

      }

      ##########################################################################################
      ## When there is confounder and there is paired infor
      if(!is.null(confounder)&!is.null(pair)&partial==T){
        library(lme4)
        library(lmerTest)

        for(i in 1:ncol(numericrisk)){

          numericrisk_list[[i]]<- matrix(nrow=TPObject$nComp,   ##row is component
                                         ncol=3)

          colnames( numericrisk_list[[i]]) <- c('est','moe','p')

          rownames( numericrisk_list[[i]]) <- colnames(scores)  ###row is components

          data=cbind(scores,numericrisk,confounder)

          for(j in 1:TPObject$nComp ){

            ###

            lme_object<-lmer(formula =as.formula(paste(colnames(numericrisk)[i],'~',
                                                             colnames(scores)[j],"+",
                                                             paste(colnames(confounder),collapse="+"),
                                                             "+","(1|pair)")),
                                   data=data)
            lme_object_sum<-summary(lme_object)
            lme_object_ano<-anova(lme_object,test = 'Chisq')


            numericrisk_list[[i]][j,c(1,2)]<-lme_object_sum$coefficients[2,c(1,2)]
            numericrisk_list[[i]][j,3]<-lme_object_ano[1,6]
            ###
            numericrisk_list[[i]][j,2] <- z * numericrisk_list[[i]][j,2]


          }

        }
        names(numericrisk_list)<-colnames(numericrisk)

      }
    if(length(numericrisk_list)!=0){
      Risk$numericrisk_list<-numericrisk_list
    }

  }

  ####add menu to ask outcomee to be in the
  # Have you make the outcomee a factor variable, if outcomee is categorical, and a numeric variable if the outcomee is numeric
  #if outcomee is categorical glm
  # if numeric linear regression
  if(dim(factorrisk)[2]!=0){
    z <- abs(qnorm(alpha))   #qnorm is quantile function

    if(multinomial==F){
      factorrisk_list<-list()
    if((is.null(confounder)&is.null(pair))|(!is.null(confounder)&is.null(pair)&partial==F)){
      for(i in 1:ncol(factorrisk)){
        factorrisk_list[[i]]<- matrix(nrow=TPObject$nComp,   ##row is component
                                       ncol=3)

        colnames(factorrisk_list[[i]]) <- c('est','moe','p')

        rownames(factorrisk_list[[i]]) <- colnames(scores)  ###row is components



        for(j in 1:TPObject$nComp){
          clr <- summary(glm(factorrisk[,i]~scores[,j],
                             family='binomial'))
          factorrisk_list[[i]][j,] <- clr$coefficients[2,c(1,2,4)]

          factorrisk_list[[i]][j,2] <- z * factorrisk_list[[i]][j,2]
        }

      }
      names(factorrisk_list)<-colnames(factorrisk)



    }
    if(!is.null(confounder)&is.null(pair)&partial==T){
      for(i in 1:ncol(factorrisk)){
        factorrisk_list[[i]]<- matrix(nrow=TPObject$nComp,   ##row is component
                                      ncol=3)

        colnames(factorrisk_list[[i]]) <- c('est','moe','p')

        rownames(factorrisk_list[[i]]) <- colnames(scores)  ###row is components

        data=cbind(scores, factorrisk,confounder)

        for(j in 1:TPObject$nComp){
          clr <- summary(glm(formula =as.formula(paste(colnames(factorrisk)[i],'~',
                                     colnames(scores)[j],"+",
                                     paste(colnames(confounder),collapse="+"))),
            family="binomial",
            data=data))
          factorrisk_list[[i]][j,] <- clr$coefficients[2,c(1,2,4)]

          factorrisk_list[[i]][j,2] <- z * factorrisk_list[[i]][j,2]
        }

      }
      names(factorrisk_list)<-colnames(factorrisk)




    }
    if((is.null(confounder)&!is.null(pair))|(!is.null(confounder)&!is.null(pair)&partial==F)){
      for(i in 1:ncol(factorrisk)){
        factorrisk_list[[i]]<- matrix(nrow=TPObject$nComp,   ##row is component
                                      ncol=3)

        colnames(factorrisk_list[[i]]) <- c('est','moe','p')

        rownames(factorrisk_list[[i]]) <- colnames(scores)  ###row is components

        data=cbind(scores, factorrisk,pair)

        for(j in 1:TPObject$nComp){
          clr <- summary(clogit(formula =as.formula(paste("as.numeric(",colnames(factorrisk)[i],")",'~',
                                                          colnames(scores)[j],"+",
                                                          "+","strata(pair)")),

                                data=data))
          factorrisk_list[[i]][j,] <- clr$coefficients[1,c(1,3,5)]

          factorrisk_list[[i]][j,2] <- z * factorrisk_list[[i]][j,2]
        }

      }
      names(factorrisk_list)<-colnames(factorrisk)


    }

    if(!is.null(confounder)&!is.null(pair)&partial==T){
      for(i in 1:ncol(factorrisk)){
        factorrisk_list[[i]]<- matrix(nrow=TPObject$nComp,   ##row is component
                                      ncol=3)

        colnames(factorrisk_list[[i]]) <- c('est','moe','p')

        rownames(factorrisk_list[[i]]) <- colnames(scores)  ###row is components

        data=cbind(scores, factorrisk,confounder,pair)

        for(j in 1:TPObject$nComp){
          clr <- summary(clogit(formula =as.formula(paste("as.numeric(",colnames(factorrisk)[i],")",'~',
                                                       colnames(scores)[j],"+",
                                                       paste(colnames(confounder),collapse="+"),
                                                       "+","strata(pair)")),

                             data=data))
          factorrisk_list[[i]][j,] <- clr$coefficients[1,c(1,3,5)]   ##only coefficient is givn, it is not odds ratio at this stag

          factorrisk_list[[i]][j,2] <- z * factorrisk_list[[i]][j,2]
        }

      }
      names(factorrisk_list)<-colnames(factorrisk)



    }

    }



    if(multinomial==T){
      library(nnet)
      factorrisk_list<-list()

      if((is.null(confounder)&is.null(pair))|(!is.null(confounder)&is.null(pair)&partial==F)
        ){
        for(i in 1:ncol(factorrisk)){

          factorrisk_list[[i]]<-list()
          for(m in 1:(length(levels(factorrisk[,i]))-1)){
            factorrisk_list[[i]][[m]] <- matrix(nrow=TPObject$nComp,   ##row is0
                                        ncol=3)
                    colnames(factorrisk_list[[i]][[m]]) <- c('est','moe','p')

          rownames(factorrisk_list[[i]][[m]]) <- colnames(scores)  ###row is components

          }



          data=cbind(factorrisk,scores)


          for(j in 1:TPObject$nComp){
            multi<- summary(multinom(factorrisk[,i]~scores[,j],
                               data=data))
              new_matrix=cbind(multi$coefficients[1:(length(levels(factorrisk[,i]))-1),2],
                            multi$standard.errors[1:(length(levels(factorrisk[,i]))-1),2])
              zz <- new_matrix[,1]/new_matrix[,2]
              # 2-tailed z test
              p <- (1 - pnorm(abs(zz), 0, 1)) * 2
              new_matrix<-cbind(new_matrix,p)
              new_matrix[,2]<-z*new_matrix[,2]
              rownames(new_matrix)<-paste0(colnames(scores)[j],"_",rownames(new_matrix))
              colnames(new_matrix)<-c('est','moe','p')
            for(m in 1:(length(levels(factorrisk[,i]))-1)){
              factorrisk_list[[i]][[m]][j,]<-new_matrix[m,]
                names(factorrisk_list[[i]][[m]])
            }


          }


            names(factorrisk_list[[i]])<-levels(factorrisk[,i])[-1]


        }
        names(factorrisk_list)<-colnames(factorrisk)


      }
      if(!is.null(confounder)&is.null(pair)&partial==T){
        for(i in 1:ncol(factorrisk)){

          factorrisk_list[[i]]<-list()
          for(m in 1:(length(levels(factorrisk[,i]))-1)){
            factorrisk_list[[i]][[m]] <- matrix(nrow=TPObject$nComp,   ##row is0
                                                ncol=3)
            colnames(factorrisk_list[[i]][[m]]) <- c('est','moe','p')

            rownames(factorrisk_list[[i]][[m]]) <- colnames(scores)  ###row is components

          }



          data=cbind(factorrisk,scores,confounder)


          for(j in 1:TPObject$nComp){
            multi<- summary(multinom(formula =as.formula(paste(colnames(factorrisk)[i],'~',
                                                               colnames(scores)[j],"+",
                                                               paste(colnames(confounder),collapse="+"))),


                                     data=data))

            new_matrix=cbind(multi$coefficients[1:(length(levels(factorrisk[,i]))-1),2],
                             multi$standard.errors[1:(length(levels(factorrisk[,i]))-1),2])
            zz <- new_matrix[,1]/new_matrix[,2]
            # 2-tailed z test
            p <- (1 - pnorm(abs(zz), 0, 1)) * 2
            new_matrix<-cbind(new_matrix,p)
            new_matrix[,2]<-z*new_matrix[,2]
            rownames(new_matrix)<-paste0(colnames(scores)[j],"_",rownames(new_matrix))
            colnames(new_matrix)<-c('est','moe','p')
            for(m in 1:(length(levels(factorrisk[,i]))-1)){
              factorrisk_list[[i]][[m]][j,]<-new_matrix[m,]
              names(factorrisk_list[[i]][[m]])
            }


          }


          names(factorrisk_list[[i]])<-levels(factorrisk[,i])[-1]


        }
        names(factorrisk_list)<-colnames(factorrisk)

      }
      if((is.null(confounder)&!is.null(pair))|(!is.null(confounder)&!is.null(pair)&partial==F)){
        for(i in 1:ncol(factorrisk)){

          factorrisk_list[[i]]<-list()
          for(m in 1:(length(levels(factorrisk[,i]))-1)){
            factorrisk_list[[i]][[m]] <- matrix(nrow=TPObject$nComp,   ##row is0
                                                ncol=3)
            colnames(factorrisk_list[[i]][[m]]) <- c('est','moe','p')

            rownames(factorrisk_list[[i]][[m]]) <- colnames(scores)  ###row is components

          }



          data=cbind(factorrisk,scores)


          for(j in 1:TPObject$nComp){
            multi<-  summary(multinom(factorrisk[,i]~scores[,j],
                                     data=data))
            new_matrix=cbind(multi$coefficients[1:(length(levels(factorrisk[,i]))-1),2],
                             multi$standard.errors[1:(length(levels(factorrisk[,i]))-1),2])
            zz <- new_matrix[,1]/new_matrix[,2]
            # 2-tailed z test
            p <- (1 - pnorm(abs(zz), 0, 1)) * 2
            new_matrix<-cbind(new_matrix,p)
            new_matrix[,2]<-z*new_matrix[,2]
            rownames(new_matrix)<-paste0(colnames(scores)[j],"_",rownames(new_matrix))
            colnames(new_matrix)<-c('est','moe','p')
            for(m in 1:(length(levels(factorrisk[,i]))-1)){
              factorrisk_list[[i]][[m]][j,]<-new_matrix[m,]
              names(factorrisk_list[[i]][[m]])
            }


          }


          names(factorrisk_list[[i]])<-levels(factorrisk[,i])[-1]


        }
        names(factorrisk_list)<-colnames(factorrisk)

      }
      if(!is.null(confounder)&!is.null(pair)&partial==T){
        for(i in 1:ncol(factorrisk)){

          factorrisk_list[[i]]<-list()
          for(m in 1:(length(levels(factorrisk[,i]))-1)){
            factorrisk_list[[i]][[m]] <- matrix(nrow=TPObject$nComp,   ##row is0
                                                ncol=3)
            colnames(factorrisk_list[[i]][[m]]) <- c('est','moe','p')

            rownames(factorrisk_list[[i]][[m]]) <- colnames(scores)  ###row is components

          }



          data=cbind(factorrisk,scores,confounder)


          for(j in 1:TPObject$nComp){
            multi<- summary(multinom(formula =as.formula(paste(colnames(factorrisk)[i],'~',
                                                               colnames(scores)[j],"+",
                                                               paste(colnames(confounder),collapse="+"))),


                                     data=data))

            new_matrix=cbind(multi$coefficients[1:(length(levels(factorrisk[,i]))-1),2],
                             multi$standard.errors[1:(length(levels(factorrisk[,i]))-1),2])
            zz <- new_matrix[,1]/new_matrix[,2]
            # 2-tailed z test
            p <- (1 - pnorm(abs(zz), 0, 1)) * 2
            new_matrix<-cbind(new_matrix,p)
            new_matrix[,2]<-z*new_matrix[,2]
            rownames(new_matrix)<-paste0(colnames(scores)[j],"_",rownames(new_matrix))
            colnames(new_matrix)<-c('est','moe','p')
            for(m in 1:(length(levels(factorrisk[,i]))-1)){
              factorrisk_list[[i]][[m]][j,]<-new_matrix[m,]
              names(factorrisk_list[[i]][[m]])
            }


          }


          names(factorrisk_list[[i]])<-levels(factorrisk[,i])[-1]


        }
        names(factorrisk_list)<-colnames(factorrisk)

      }




    }

      if(length(factorrisk_list)!=0){
        Risk$factorrisk_list<-factorrisk_list
      }



}


return(Risk)

}
