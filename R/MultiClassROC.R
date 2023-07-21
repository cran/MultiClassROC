#' ROC Curves for Multi-Class Analysis
#'
#' Function 'multiroc' can be used for computing and visualizing Receiver Operating Characteristics (ROC) and Area Under the Curve (AUC) for multi-class classification problems. It supports both one-vs-one and one-vs-all approaches.
#' @param y A string, dependent variable
#' @param x A vector of strings, independent variables
#' @param k The number of categories
#' @param type A string, "OvO" for one-vs-one, "OvA" for one-vs-all approach
#' @param plot A logical, TRUE for the plot of the curves, FALSE for the average AUC
#' @param data A data frame, the dataset to use
#'
#' @return plot with ROC curves using ggroc, pROC (if plot=TRUE) or the average AUC (if plot=FALSE)
#'
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom pROC ggroc
#' @importFrom pROC roc
#' @importFrom stats as.formula
#' @importFrom stats glm
#' @importFrom stats binomial
#' @importFrom stats predict
#'
#' @export
#'
#' @examples multiroc(y="Species",
#'              x=c("Petal.Width","Petal.Length","Sepal.Width","Sepal.Length"),
#'              k=3, type=("OvA"),
#'              plot=TRUE,
#'              data=iris)
#' @examples multiroc(y="Species",
#'              x=c("Petal.Width","Petal.Length","Sepal.Width","Sepal.Length"),
#'              k=3,
#'              type=("OvO"),
#'              plot=FALSE,
#'              data=iris)
#'
#'
#'
multiroc <- function(y, x, k, type=c("OvO","OvA"), plot=TRUE, data) {
  data[[y]] <- as.numeric(factor(data[[y]]))
  data[[y]] <- as.character(data[[y]])
  datasets<-list()
  curves<-list()
  formula <- as.formula(paste(y, "~", paste(x, collapse = "+")))
  if (type=='OvO'){
    n=1
    for(i in 1:(k-1)){
      for(j in (i+1):k){
        datasets[[n]] <- data[data[[y]] %in% c(as.character(i), as.character(j)), ]
        datasets[[n]][[y]] <- as.factor(datasets[[n]][[y]])
        model <- glm(formula, family = binomial(link = "logit"), data = datasets[[n]])
        datasets[[n]]$probs <- predict(model, newdata = datasets[[n]], type = "response")
        datasets[[n]]$predictedy<-0
        datasets[[n]]$predictedy[datasets[[n]]$probs>0.5]<-1
        curves[[paste0("roc",n)]]<-pROC::roc(datasets[[n]][[y]],datasets[[n]]$probs)
        names(curves)[n]<-paste0(i,"vs",j)
        n = n + 1
      }
    }
    if(plot==TRUE){
      roc<-curves$'1vs2'
      line<-0:101
      line<-line[0:101]/100
      curves$roc<-roc
      curves$roc$specificities<-line
      curves$roc$sensitivities<-1-line
      names(curves)[length(curves)] <- "line"
      return(pROC::ggroc(curves ,size=1)+ggplot2::xlab("1-specificity")+ggplot2::ylab("sensitivity")+ggplot2::scale_color_discrete("curves"))
    } else if (plot==FALSE){
      auc<-0
      for (i in 1:k){
        auc<-auc+curves[[i]]$auc
      }
      auc<-auc/k
      return(auc)
    }
  } else if (type=="OvA"){

    for (i in 1:k){
      datasets[[i]] <- data
      unique_categories = unique(datasets[[i]][[y]])
      remaining_categories = unique_categories[unique_categories != as.character(i)]
      datasets[[i]][[y]]<-ifelse(datasets[[i]][[y]] %in% remaining_categories, "Merged", datasets[[i]][[y]])
      datasets[[i]][[y]] = as.factor(datasets[[i]][[y]])
      model <- glm(formula, family = binomial(link = "logit"), data = datasets[[i]])
      datasets[[i]]$probs <- predict(model, newdata = datasets[[i]], type = "response")
      datasets[[i]]$predictedy<-0
      datasets[[i]]$predictedy[datasets[[i]]$probs>0.5]<-1
      curves[[paste0("roc",i)]]<-pROC::roc(datasets[[i]][[y]],datasets[[i]]$probs)
    }
    if(plot==TRUE){
      roc<-curves$roc1
      line<-0:101
      line<-line[0:101]/100
      curves$roc<-roc
      curves$roc$specificities<-line
      curves$roc$sensitivities<-1-line
      for(i in 1:k) {
        names(curves)[i] <- paste0(i,"vsAll")
      }
      names(curves)[length(curves)] <- "line"
      return(pROC::ggroc(curves ,size=1)+ggplot2::xlab("1-specificity")+ggplot2::ylab("sensitivity")+ggplot2::scale_color_discrete("curves"))
    } else if (plot==FALSE){
      auc<-0
      for (i in 1:k){
        auc<-auc+curves[[i]]$auc
      }
      auc<-auc/k
      return(auc)
    }
  }
}


