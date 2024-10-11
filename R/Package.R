#### Representative solver ####

#' Anchored Score-Matching Representative (ASMR) for Logistic Regression
#'
#' This function implements the Anchored Score-Matching Representative (ASMR)
#' method for logistic regression. ASMR constructs representatives for
#' distributed datasets and iteratively updates logistic regression coefficients.
#'
#' @param dat A data frame containing the dataset with covariates and binary
#' response variable.
#' @param names.x A character vector of covariate names.
#' @param names.y The name of the binary response variable.
#' @param iter An integer specifying the number of iterations for ASMR updates
#' (default is 5).
#'
#' @return A matrix of updated regression coefficients for each iteration.
#' The first column corresponds to results from the Mean Representative (MR) method,
#' followed by columns for each iteration of ASMR.
#' @examples
#' # Example usage with simulated data
#' data <- data.frame(node = rep(1:10, each = 100),
#'                    x1 = rnorm(1000),
#'                    x2 = rnorm(1000),
#'                    y = rbinom(1000, 1, 0.5))
#' # Apply ASMR for logistic regression
#' beta_amsr <- rep.amsr.logit(data, names.x = c("x1", "x2"), names.y = "y", iter = 5)
#' @export

rep.amsr.logit <- function(dat, names.x, names.y, iter=5){

  formula.asmr <- formula(paste0(names.y,'~',paste(names.x,collapse = '+')))

  ## MR
  rep.MR <- dat %>% group_by(node) %>%
    summarise(across(everything(), mean), n=n(), .groups = 'drop') %>%
    as.matrix()

  beta.mr <- logistic.reg(rep.MR[,names.x], rep.MR[,names.y],rep.MR[,"n"])
  beta.asmr <- cbind(beta.mr)

  ## asmr
  for(it in seq_len(iter)){
    beta.current <- beta.asmr[,it]

    dat$eta <-  as.matrix(dat[,names.x])%*%beta.current[-1]+beta.current[1]
    dat$h <- dat$y-1/(1+exp(-dat$eta))
    dat$h.sign <- sign(dat$h)

    rep.asmr <- dat %>% group_by(node,h.sign) %>%
      group_map(~ asmr.logit(.x), .groups = 'drop')
    rep.asmr <- do.call(rbind,rep.asmr)
    rep.asmr <- as.matrix(rep.asmr)
    beta.tmp <- logistic.reg(rep.asmr[,names.x], rep.asmr[,names.y],rep.asmr[,"n"],
                             beta=beta.current, max_iter = 1)
    beta.asmr <- cbind(beta.asmr, beta.tmp)
  }
  return(beta.asmr)
}

rep.poisson <- function(dat, names.x, names.y, iter=5){

  formula.asmr <- paste0(names.y,'~',paste(names.x, collapse = '+'))

  # subpartion
  dat <- dat %>% group_by(node) %>%
    mutate(log_y = log2(y + 1),
           partition = cut(log_y,
                           breaks = c(0, 8, 15, 21, 26, 30, 33, 35:100, Inf),  # Custom breakpoints based on your intervals
                           labels = FALSE, right = FALSE)) %>%
    mutate(node = paste(node, partition, sep = "_")) %>%
    select(-log_y, -partition)

  # TMR
  rep.mr <- dat %>% group_by(node) %>%
    summarise(
      y=exp(mean(log(y + 1)))-1,
      across(!c(y), mean),  # Mean of all variables except 'node' and 'log_y'
      n = n(),  # Count the number of rows in each partition
      .groups = 'drop'  # Drop the grouping after summarization
    ) %>%
    select(-node)   # Arrange by node and partition

  beta.asmr <- cbind(glm(formula=formula.asmr,
                         family = poisson(link="log"),
                         data=rep.mr,
                         weights=n)$coefficients)

  ## asmr

  for(it in seq_len(iter)){
    beta.current <- beta.asmr[,it]

    dat$eta <-  as.matrix(dat[,names.x])%*%beta.current[-1]+beta.current[1]
    dat$h <- dat$y-exp(dat$eta)
    dat$h.sign <- sign(dat$h)

    rep.asmr <- dat %>% group_by(node,h.sign) %>%
      group_map(~ asmr.poisson(.x), .groups = 'drop') %>%
      do.call(rbind,.) %>%
      as.data.frame()
    beta.tmp <- glm(formula=formula.asmr,family = poisson(link="log"), data=rep.asmr,
                    weights=n, start = beta.current, maxit = 1)$coefficients
    beta.asmr <- cbind(beta.asmr, beta.tmp)
  }
  return(beta.asmr)
}

#### Representative construction ####

mr.generic <- function(dat.node){
  dat.node <- as.matrix(dat.node, drop=FALSE)
  n <- nrow(dat.node)
  if(n==0){
    return(NULL)
  }
  results <- c(colMeans(dat.node), n=n)
  return(results)
}


tmr.poisson <- function(dat.node){
  dat.node <- as.matrix(dat.node, drop=FALSE)
  pp <- ncol(dat.node)-3
  n <- nrow(dat.node)
  if(n==0){
    return(NULL)
  }
  y.node <- dat.node[,1]
  X.node <- dat.node[,2:(pp+1)]
  y.tmr <- exp(mean(log(y.node + 1)))-1
  x.tmr <- colMeans(X.node)
  results <- c(y=y.tmr, x.tmr, n=n)
  return(results)
}

asmr.logit <- function(dat.node){
  ## asmr per node
  ## input can be matrix, data.frame, or data.table

  dat.node <- as.matrix(dat.node, drop=FALSE)
  pp <- ncol(dat.node)-3
  n <- nrow(dat.node)
  if(n==0){
    return(NULL)
  }
  x.labels <- colnames(dat.node)[2:(pp+1)]

  ## all record duplicates
  if(nrow(unique(dat.node))==1){
    results <- c(dat.node[1,1:(pp+1)],n=n)
    return(results)
  }

  eta.node <- dat.node[,pp+2]
  h.node <- dat.node[,pp+3]
  y.node <- dat.node[,1]
  X.node <- dat.node[,2:(pp+1)]

  ## all record duplicates in terms of covariates
  if(nrow(unique(X.node))==1){
    results <- c(y=mean(y.node), X.node[1,], n=n)
    return(results)
  }

  if(sum(h.node)==0){
    results <- c(y=mean(y.node), colMeans(X.node), n=n)
    return(results)
  }

  eta.asmr <- sum(h.node*eta.node)/sum(h.node)
  x.asmr <- colSums(X.node*h.node)/sum(h.node)
  y.asmr <- 1/(1+exp(-eta.asmr))+mean(h.node)
  y.asmr <- max(min(y.asmr,1),0)
  results <- c(y=y.asmr, x.asmr, n=n)
  return(results)
}

asmr.poisson <- function(dat.node){
  ## asmr per node
  ## input can be matrix, data.frame, or data.table
  # family <- poisson(link = "log")
  # link_inverse <- function(eta) family$linkinv(eta)
  # link_dtheta_deta <- function(eta) family$dtheta_deta(eta)

  # A.type <- function(x) 1
  # F.type <- function(x) exp(x)

  dat.node <- as.matrix(dat.node, drop=FALSE)
  pp <- ncol(dat.node)-3
  n <- nrow(dat.node)
  if(n==0){
    return(NULL)
  }
  x.labels <- colnames(dat.node)[2:(pp+1)]

  ## all record duplicates
  if(nrow(unique(dat.node))==1){
    results <- c(dat.node[1,1:(pp+1)],n=n)
    return(results)
  }

  eta.node <- dat.node[,pp+2]
  h.node <- dat.node[,pp+3]
  y.node <- dat.node[,1]
  X.node <- dat.node[,2:(pp+1)]

  ## all record duplicates in terms of covariates
  if(nrow(unique(X.node))==1){
    results <- c(y=mean(y.node), X.node[1,], n=n)
    return(results)
  }

  if(sum(h.node)==0){
    results <- c(y=mean(y.node), colMeans(X.node), n=n)
    return(results)
  }

  eta.asmr <- sum(h.node*eta.node)/sum(h.node)
  x.asmr <- colSums(X.node*h.node)/sum(h.node)
  y.asmr <- exp(eta.asmr)+mean(h.node)
  y.asmr <- max(y.asmr,0)

  results <- c(y=y.asmr, x.asmr, n=n)
  return(results)
}


asmr.linear <- function(dat.node){
  ## asmr per node
  ## input can be matrix, data.frame, or data.table

  dat.node <- as.matrix(dat.node, drop=FALSE)
  pp <- ncol(dat.node)-3
  n <- nrow(dat.node)
  if(n==0){
    return(NULL)
  }
  x.labels <- colnames(dat.node)[2:(pp+1)]

  ## all record duplicates
  if(nrow(unique(dat.node))==1){
    results <- c(dat.node[1,1:(pp+1)],n=n)
    return(results)
  }

  eta.node <- dat.node[,pp+2]
  h.node <- dat.node[,pp+3]
  y.node <- dat.node[,1]
  X.node <- dat.node[,2:(pp+1)]

  ## all record duplicates in terms of covariates
  if(nrow(unique(X.node))==1){
    results <- c(y=mean(y.node), X.node[1,], n=n)
    return(results)
  }

  if(sum(h.node)==0){
    results <- c(y=mean(y.node), colMeans(X.node), n=n)
    return(results)
  }

  eta.asmr <- sum(h.node*eta.node)/sum(h.node)
  x.asmr <- colSums(X.node*h.node)/sum(h.node)
  y.asmr <- eta.asmr+mean(h.node)

  results <- c(y=y.asmr, x.asmr, n=n)
  return(results)
}

asmr.generic <- function(data, family){
  # browser()
  ## data must be data.table

  link_inverse <- function(eta) family$linkinv(eta)
  link_dtheta_deta <- function(eta) family$dtheta_deta(eta)

  dat.node <- as.matrix(dat.node, drop=FALSE)
  pp <- ncol(dat.node)-3
  n <- nrow(dat.node)
  if(n==0){
    return(NULL)
  }
  x.labels <- colnames(dat.node)[2:(pp+1)]

  ## all record duplicates
  if(nrow(unique(dat.node))==1){
    results <- c(dat.node[1,1:(pp+1)],n=n)
    return(results)
  }

  eta.node <- dat.node[,pp+2]
  h.node <- dat.node[,pp+3]
  y.node <- dat.node[,1]
  X.node <- dat.node[,2:(pp+1)]

  ## all record duplicates in terms of covariates
  if(nrow(unique(X.node))==1){
    results <- c(y=mean(y.node), X.node[1,], n=n)
    return(results)
  }

  eta.asmr <- sum(h.node*eta.node)/sum(h.node)
  x.asmr <- colSums(X.node*h.node)/sum(h.node)
  y.asmr <- link_inverse(eta.asmr)+mean(h.node)/link_dtheta_deta()

  results <- c(y=y.asmr, x.asmr, n=n)
  return(results)
}
