lm.getCoeff <- function(dataset, ycol, xcols, io.json = FALSE) 
{
  if(io.json == TRUE)
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  
  xnam <- paste0("dataset[,xcols[", 1:length(xcols), "]]")
  
  fmla <- as.formula(paste0("dataset[,ycol] ~ ", paste0(xnam, collapse= "+")))
  
  lminfo <- lm(fmla)
  
  coeff.lm <- lminfo$coefficients
  
  if(io.json == TRUE)
  {
    return(toJSON(coeff.lm))
    
  }
  else{
    return(coeff.lm)
  }
}

lm.getRes <- function(dataset, ycol, xcols, io.json = FALSE) 
{
  if(io.json == TRUE)
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  xnam <- paste0("dataset[,xcols[", 1:length(xcols), "]]")
  
  fmla <- as.formula(paste0("dataset[,ycol] ~ ", paste0(xnam, collapse= "+")))
  
  lminfo <- lm(fmla)
  
  res.lm <- lminfo$residuals
  
  if(io.json == TRUE)
  {
    return(toJSON(res.lm))
    
  }
  else{
    return(res.lm)
  }
}

lm.getEff <- function(dataset, ycol, xcols, io.json = FALSE) 
{
  if(io.json == TRUE)
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  xnam <- paste0("dataset[,xcols[", 1:length(xcols), "]]")
  
  fmla <- as.formula(paste0("dataset[,ycol] ~ ", paste0(xnam, collapse= "+")))
  
  lminfo <- lm(fmla)
  
  eff.lm <- lminfo$effects
  
  if(io.json == TRUE)
  {
    return(toJSON(eff.lm))
    
  }
  else{
    return(eff.lm)
  }
}

lm.getFit <- function(dataset, ycol, xcols, io.json = FALSE) 
{
  if(io.json == TRUE)
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  xnam <- paste0("dataset[,xcols[", 1:length(xcols), "]]")
  
  fmla <- as.formula(paste0("dataset[,ycol] ~ ", paste0(xnam, collapse= "+")))
  
  lminfo <- lm(fmla)
  
  fitval.lm <- lminfo$fitted.values
  
  if(io.json == TRUE)
  {
    return(toJSON(fitval.lm))
    
  }
  else{
    return(fitval.lm)
  }
}

lm.getRank <- function(dataset, ycol, xcols, io.json = FALSE) 
{
  if(io.json == TRUE)
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  xnam <- paste0("dataset[,xcols[", 1:length(xcols), "]]")
  
  fmla <- as.formula(paste0("dataset[,ycol] ~ ", paste0(xnam, collapse= "+")))
  
  lminfo <- lm(fmla)
  
  rank.lm <- lminfo$rank
  
  if(io.json == TRUE)
  {
    return(toJSON(rank.lm))
    
    
  }
  else{
    return(rank.lm)
    
  }
  
}

lm.getModel <- function(dataset, ycol, xcols, io.json = FALSE)
{
  
  if(io.json == TRUE)
    
  {
    xcols <- fromJSON(xcolsJ)
    ycol <- fromJSON(ycolJ)
    dataset <- fromJSON(dataset)
  }
  c <- lm.getCoeff(dataset, ycol, xcols, io.json = FALSE)
  res <- lm.getRes(dataset, ycol, xcols, io.json = FALSE)
  e <- lm.getEff(dataset, ycol, xcols, io.json = FALSE)
  f <- lm.getFit(dataset, ycol, xcols, io.json = FALSE)
  rank <- lm.getRank(dataset, ycol, xcols, io.json = FALSE)
  
  results <- list("eff" = e, "fit" = f, "Coeff" = c, "rank" = rank, "res" = res)
  
  if(io.json == TRUE)
  {
    return(toJSON(results))
  }
  else
  {
    return(results)
  }
  
}