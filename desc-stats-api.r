desc.stats.getmode <- function(v, io.json = FALSE) 
{
  if(io.json == TRUE)
  {
    v <- fromJSON(v)
  }
  uniqv <- unique(v)
  mode.val <- uniqv[which.max(tabulate(match(v, uniqv)))]
  
  if(io.json == TRUE)
  {
    return(toJSON(mode.val))
  }
  else
  {
    return(mode.val)
  }
}

desc.stats.getSD <- function(v, io.json = FALSE)
{
  if(io.json == TRUE)
  {
    v <- fromJSON(v)
  }
  sd.val <- sd(v)
  if(io.json == TRUE)
  {
    return(toJSON(sd.val))
  }
  else
  {
    return(sd.val)
  }
}

desc.stats.getMax <- function(v, io.json = FALSE)
{
  if(io.json == TRUE)
  {
    v <- fromJSON(v)
  }
  max.val <- max(v)
  
  if(io.json == TRUE)
  {
    return(toJSON(max.val))
  }
  else
  {
    return(max.val)
  }
}

desc.stats.getMin <- function(v, io.json = FALSE)
{
  if(io.json == TRUE)
  {
    v <- fromJSON(v)
  }
  min.val <- min(v)
  if(io.json == TRUE)
  {
    return(toJSON(min.val))
  }
  else
  {
    return(min.val)
  }
}

desc.stats.getMedian <- function(v, io.json = FALSE)
{
  if(io.json == TRUE)
  {
    v <- fromJSON(v)
  }
  med.val <- median(v)
  
  if(io.json == TRUE)
  {
    return(toJSON(med.val))
  }
  else
  {
    return(med.val)
  }
}

desc.stats.getRange <- function(v, io.json = FALSE)
{
  if(io.json == TRUE)
  {
    v <- fromJSON(v)
  }
  range.val <- range(v)
  if(io.json == TRUE)
  {
    return(toJSON(range.val))
  }
  else{
    return(range.val)
  }
}

desc.stats.getQuantile <- function(v, io.json = FALSE)
{
  if(io.json == TRUE)
  {
    v <- fromJSON(v)
  }
  quan.val <- quantile(v)
  if(io.json == TRUE)
  {
    return(toJSON(quan.val))
  }
  else
  {
    return(quan.val)
  }
}