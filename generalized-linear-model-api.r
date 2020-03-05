glm.getGLMsum <- function(dataset, ycol, xcols) 
{
  
  xnam <- paste0("dataset[,xcols[", 1:length(xcols), "]]")
  
  fmla <- as.formula(paste0("dataset[,ycol] ~ ", paste0(xnam, collapse= "+")))
  
  glminfo <- glm(fmla, data=dataset, family=binomial())
  
  sum <- summary(glminfo)
  
  return(sum)
  
  
}



glm.getGLMcon <- function(dataset, ycol, xcols, io.json = FALSE) 
{
  if(io.json == TRUE)
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  xnam <- paste0("dataset[,xcols[", 1:length(xcols), "]]")
  
  fmla <- as.formula(paste0("dataset[,ycol] ~ ", paste0(xnam, collapse= "+")))
  
  glminfo <- glm(fmla,data=dataset,family=binomial())
  
  con <- confint(glminfo)
  
  
  if(io.json == TRUE)
  {
    return(toJSON(con))
    
    
  }
  else{
    return(con)
    
  }
  
}

glm.getGLMcoeff <- function(dataset, ycol, xcols, io.json = FALSE) 
{
  if(io.json == TRUE)
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  xnam <- paste0("dataset[,xcols[", 1:length(xcols), "]]")
  
  fmla <- as.formula(paste0("dataset[,ycol] ~ ", paste0(xnam, collapse= "+")))
  
  glminfo <- glm(fmla,data=dataset,family=binomial())
  
  c <- exp(coef(glminfo))
  
  
  if(io.json == TRUE)
  {
    return(toJSON(c))
    
    
  }
  else{
    return(c)
    
  }
  
}

glm.getGLMconfit <- function(dataset, ycol, xcols, io.json = FALSE) 
{
  if(io.json == TRUE)
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  xnam <- paste0("dataset[,xcols[", 1:length(xcols), "]]")
  
  fmla <- as.formula(paste0("dataset[,ycol] ~ ", paste0(xnam, collapse= "+")))
  
  glminfo <- glm(fmla,data=dataset,family=binomial())
  
  c <- exp(confint(glminfo))
  
  
  if(io.json == TRUE)
  {
    return(toJSON(c))
    
    
  }
  else{
    return(c)
    
  }
  
}

glm.getGLMpre <- function(dataset, ycol, xcols, io.json = FALSE) 
{
  if(io.json == TRUE)
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  xnam <- paste0("dataset[,xcols[", 1:length(xcols), "]]")
  
  fmla <- as.formula(paste0("dataset[,ycol] ~ ", paste0(xnam, collapse= "+")))
  
  glminfo <- glm(fmla,data=dataset,family=binomial())
  
  c <- predict(glminfo, type="response")
  
  
  if(io.json == TRUE)
  {
    return(toJSON(c))
    
    
  }
  else{
    return(c)
    
  }
  
}


glm.getGLMres <- function(dataset, ycol, xcols, io.json = FALSE) 
{
  if(io.json == TRUE)
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  xnam <- paste0("dataset[,xcols[", 1:length(xcols), "]]")
  
  fmla <- as.formula(paste0("dataset[,ycol] ~ ", paste0(xnam, collapse= "+")))
  
  glminfo <- glm(fmla,data=dataset,family=binomial())
  
  c <- residuals(glminfo, type="deviance")
  
  
  if(io.json == TRUE)
  {
    return(toJSON(c))
    
    
  }
  else{
    return(c)
    
  }
  
}


glm.getModel <- function(dataset, ycol, xcols, io.json = FALSE)
{
  
  if(io.json == TRUE)
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  
  sum<-glm.getGLMsum(dataset, ycol, xcols)
  con<-glm.getGLMcon(dataset, ycol, xcols, io.json = FALSE)
  coeff<-glm.getGLMcoeff(dataset, ycol, xcols, io.json = FALSE)
  confit<-glm.getGLMconfit(dataset, ycol, xcols, io.json = FALSE)
  pre<-glm.getGLMpre(dataset, ycol, xcols, io.json = FALSE)
  res<-glm.getGLMres(dataset, ycol, xcols, io.json = FALSE)
  
  
  if(io.json == TRUE)
  {
    results <- list("con" = con, "coeff" = coeff, "confit" = confit, "pre" = pre, "res" = res)
    return(toJSON(results))
  }
  else
  {
    results <- list("sum" = sum,"con" = con, "coeff" = coeff, "confit" = confit, "pre" = pre, "res" = res)
    return(results)
  }
  
}