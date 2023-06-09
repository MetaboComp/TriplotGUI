#' This function aims to construct one more flexible pcor()
#' All X, Y C should be numeric
#' @param X  A data frame of X variables (All variables needs to be numeric)
#' @param Y  A dataframe ofY variables (needs to be numeric)
#' @param C A data frame of covariates (All variables needs to be numeric)
#' @param cor_method from cor()
#' @param allnumeric don't know why it has problem logical if True all variables are transformed to numeric
#' @export
#' @return A vector with adjusted partial correlations
#'
#'
#' install.packages(remotes)
#' library(remotes)
#' install_gitlab('CarlBrunius/MUVR@MUVR2')
#' library(MUVR)
#' X=Xotu[,1:4]
#' Y=as.numeric(Yotu)
#' C=Xotu[,5:7]
#'

pCor <- function(X,
                 Y,
                 C,
                 cor_method=c("pearson", "kendall", "spearman"),
                 allnumeric=T
) {  result<-list()

  if(missing(X)){stop("\n Must have a X")}
  if(missing(Y)){Y=X
  cat("\n You don't have a Y, then we let Y = X")
  }
  if(missing(cor_method)){cor_method="spearman"}
  if(cor_method!="pearson"&cor_method!="kendall"&cor_method!="spearman"){
    stop("\n Wrong method, you idiot!")
  }

  if(allnumeric){
  cat("\n In your X, Y and C, if you have variables that are non-numeric, they are transformed to numeric autonomatically.\n")
  cat("\n Be careful if you want this to happen or not")

  }
  if(is.null(dim(X))){
    Xframe=data.frame(X)
    rownamesX<-rownames(Xframe)
    colnamesX<-colnames(Xframe)
    if(allnumeric){
      X=as.numeric(X)}
    X<-data.frame(X)
    colnames(X)<-colnamesX
    rownames(X)<-rownamesX
  }
  if(!is.null(dim(X))){
    colnamesX<-colnames(X)
    rownamesX<-rownames(X)
    if(allnumeric){
    for(i in 1:ncol(X)){
      X[,i]=as.numeric(X[,i])

    }
    }
    X=data.frame(X)
    colnames(X)<-colnamesX
    rownames(X)<-rownamesX
  }


  if(!missing(C)){
    if(is.null(dim(C))){
      Cframe=data.frame(C)
      rownamesC<-rownames(Cframe)
      colnamesC<-colnames(Cframe)
      if(allnumeric){
      C=as.numeric(C)
      }
      C=data.frame(C)
      colnames(C)<-colnamesC
      rownames(C)<-rownamesC
    }
  }


  if(!missing(C)){
    if(!is.null(dim(C))){
      colnamesC<-colnames(C)
      rownamesC<-rownames(C)
      if(allnumeric){
      for(i in 1:ncol(C))
      {C[,i]=as.numeric(C[,i])

      }
      }
      C=data.frame(C)
      colnames(C)<-colnamesC
      rownames(C)<-rownamesC
    }
  }


  if(is.null(dim(Y))){
    Yframe=data.frame(Y)
    colnamesY<-colnames(Yframe)
    rownamesY<-rownames(Yframe)
    if(allnumeric){
    Y=as.numeric(Y)}
    Y=data.frame(Y)
    colnames(Y)<-colnamesY
    rownames(Y)<-rownamesY
  }else{
    colnamesY<-colnames(Y)
    rownamesY<-rownames(Y)
    if(allnumeric){
    for(i in 1:ncol(Y))
    {Y[,i]=as.numeric(Y[,i])

    }
    }
    Y=data.frame(Y)
    colnames(Y)<-colnamesY
    rownames(Y)<-rownamesY
  }


  if(nrow(X)!=nrow(Y)){
    stop("\n X and Y should have same number of observations")
    }





  cor_estimate=matrix(0,ncol(X),ncol(Y))
  cor_pvalue=matrix(0,ncol(X),ncol(Y))
  if(!missing(C)){
    data=data.frame(X,Y,C)

    for (i in 1:ncol(X)){
      for(j in 1:ncol(Y)){
        if(is.numeric(Y[,j])){
      glmX <- glm(formula = as.formula(paste(colnames(X)[i],'~', paste(colnames(C),collapse="+"))),
                  data=data)

      glmY <- glm(formula = as.formula(paste(colnames(Y)[j],'~', paste(colnames(C),collapse="+"))),
                  data=data)
      cor_test<-cor.test(resid(glmX),
                         resid(glmY),
                         method=cor_method)
      cor_estimate[i,j] <-cor_test$estimate
      cor_pvalue[i,j] <- cor_test$p.value

        }
        if(is.factor(Y[,j])){
          modelXY<-lm(formula = as.formula(paste(colnames(X)[i],'~',paste(colnames(Y)[j]),  ## the denpendent variable place must be numeric, the independent varible place not necessary
                                                  '+', paste(colnames(C),collapse="+"))),
                       data=data)
          anovaXY<-anova(modelXY)
          rsqXY<-anovaXY[1,2]/sum(anovaXY[,2])
          cor_estimate[i,j]<-sqrt(rsqXY)
          cor_pvalue[i,j] <- anovaXY[1,5]
        }
      }
    }
    result$C<-C
  }
  if(missing(C)){
    data=data.frame(X,Y)

    for (i in 1:ncol(X)){
      for(j in 1:ncol(Y)){
     if(is.numeric(Y[,j])){
      cor_test<-cor.test(X[,i],Y[,j],
                         method=cor_method)
      cor_estimate[i,j]  <-cor_test$estimate
      cor_pvalue[i,j]  <- cor_test$p.value
      }

      if(is.factor(Y[,j])){
        modelXY<-lm(formula = as.formula(paste(colnames(X)[i],
                                               '~',colnames(Y)[j])),  ## the denpendent variable place must be numeric, the independent varible place not necessary
                    data=data)

        anovaXY<-anova(modelXY)
        rsqXY<-anovaXY[1,2]/sum(anovaXY[,2])

        cor_estimate[i,j]<-sqrt(rsqXY)
        cor_pvalue[i,j] <- anovaXY[1,5]
      }
      }
    }
    cor_pvalue<-data.frame(cor_pvalue)
    rownames(cor_pvalue)<-colnames(X)
    cor_estimate<-data.frame(cor_estimate)
    rownames(cor_estimate)<-colnames(X)
    colnames(cor_estimate)<-colnames(Y)
    colnames(cor_pvalue)<-colnames(Y)

  }

  cor_estimate<-data.frame(cor_estimate)
  cor_pvalue<-data.frame(cor_pvalue)
  rownames(cor_estimate)<-colnames(X)
  rownames(cor_pvalue)<-colnames(X)
  colnames(cor_estimate)<-colnames(Y)
  colnames(cor_pvalue)<-colnames(Y)
  result$cor_estimate<-t(cor_estimate)
  result$cor_pvalue<-t(cor_pvalue)
  result$Y<-Y
  result$X<-X

  return(result)
}
