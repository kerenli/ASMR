Representative.generic <- function(dat, names.x, names.y, link="binomial", iter=5){
  
  dat <- dat[,c(names.y, names.x, "node")]
  
  formula.ismr <- paste0(names.y,'~',paste(names.x,collapse = '+'))
  
  # nu.fun <- function(x) 1
  # G.fun <- function(x) 1/(1+exp(-x))
  
  ## MR
  
  rep.MR <- dat %>% group_by(node) %>% 
    summarise(across(everything(), mean), n=n(), .groups = 'drop') %>%
    as.data.table()
  
  beta.smr <- cbind(glm(formula=formula.ismr, family = link, data=rep.MR, weights=n)$coefficients)
  
  ## ISMR
  beta.ismr <-  beta.smr[,1,drop=FALSE]
  it=1
  for(it in seq_len(iter)){
    dat$eta <-  as.matrix(dat[,..names.x])%*%beta.ismr[-1,it]+beta.ismr[1,it]
    dat$h <- nu.fun(dat$eta)*(dat$y-G.fun(dat$eta))
    dat[,h.sign:=sign(h)]
    
    rep.ismr <- dat %>% group_by(node,h.sign) %>% 
      group_map(~ ismr.logit(.x, nu.fun, G.fun), .groups = 'drop') 
    rep.ismr <- do.call(rbind,rep.ismr)
    rep.ismr <- data.table(rep.ismr)
    
    beta.ismr <- cbind(beta.ismr, glm(formula=formula.ismr, family = link, data=rep.ismr, weights=n)$coefficients)
  }
  return(beta.ismr)
}

#### ISMR solver ####

ismr.generic <- function(data, nu.fun, G.fun){
  # browser()
  ## data must be data.table

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
  
  eta.ismr <- sum(h.node*eta.node)/sum(h.node)
  x.ismr <- colSums(X.node*h.node)/sum(h.node)
  y.ismr <- G.fun(eta.ismr)+mean(h.node)/nu.fun()
  
  results <- c(y=y.ismr, x.ismr, n=n)
  return(results)
}
