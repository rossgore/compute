library(jsonlite)
library(tidyverse)

source("compute-api.r")

v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
v.JSON <- toJSON(v)

# descriptive statistics tests

# with JSON
mode <- desc.stats.getmode(v.JSON, io.json = TRUE)
mode
validate(mode)

standardDev <- desc.stats.getSD(v.JSON, io.json = TRUE)
standardDev
validate(standardDev)

max.val <- desc.stats.getMax(v.JSON, io.json = TRUE)
max.val
validate(max.val)

min.val <- desc.stats.getMin(v.JSON, io.json = TRUE)
min.val
validate(min.val)

med.val <- desc.stats.getMedian(v.JSON, io.json = TRUE)
med.val
validate(med.val)

range.val <- desc.stats.getRange(v.JSON, io.json = TRUE)
range.val
validate(range.val)

quan.val <- desc.stats.getQuantile(v.JSON, io.json = TRUE)
quan.val
validate(quan.val)

# without JSON
mode <- desc.stats.getmode(v)
mode

standardDev <- desc.stats.getSD(v)
standardDev

max.val <- desc.stats.getMax(v)
max.val

min.val <- desc.stats.getMin(v)
min.val

med.val <- desc.stats.getMedian(v)
med.val

range.val <- desc.stats.getRange(v)
range.val

quan.val <- desc.stats.getQuantile(v)
quan.val


# linear regression tests
head(mtcars)
dataset <- mtcars
datasetJ <- toJSON(dataset)

ycol <- 1
xcols <- c(1,3,4,5,8)

ycolJ <- toJSON(ycol)
xcolsJ <- toJSON(xcols)


# JSON test for linear regression
c.JSON <- lm.getCoeff(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
res.JSON <- lm.getRes(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
e.JSON <- lm.getEff(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
f.JSON <- lm.getFit(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
rank.j <- lm.getRank(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
results.JSON <- lm.getModel(datasetJ, ycolJ, xcolsJ, io.json = TRUE)

# without JSON tests for linear regression
c <- lm.getCoeff(dataset, ycol, xcols, io.json = FALSE)
res <- lm.getRes(dataset, ycol, xcols, io.json = FALSE)
e <- lm.getEff(dataset, ycol, xcols, io.json = FALSE)
f <- lm.getFit(dataset, ycol, xcols, io.json = FALSE)
rank <- lm.getRank(dataset, ycol, xcols, io.json = FALSE)
results <- lm.getModel(dataset, ycol, xcols, io.json = FALSE)



# JSON test for generalized linear regression
dataset <- mtcars
datasetJ <- toJSON(dataset)

ycol <- 8
xcols <- c(3, 5)

ycolJ <- toJSON(ycol)
xcolsJ <- toJSON(xcols)


# JSON test for generalized linear regression
con.J <- glm.getGLMcon(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
coeff.J <- glm.getGLMcoeff(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
confit.J <- glm.getGLMconfit(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
pre.J <- glm.getGLMpre(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
res.J <- glm.getGLMres(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
resultsglm.JSON <- glm.getModel(datasetJ, ycolJ, xcolsJ, io.json = TRUE)

# without JSON tests for generalized linear regression
con <-glm.getGLMcon(dataset, ycol, xcols, io.json = FALSE)
coeff <- glm.getGLMcoeff(dataset, ycol, xcols, io.json = FALSE)
confit <- glm.getGLMconfit(dataset, ycol, xcols, io.json = FALSE)
pre <- glm.getGLMpre(dataset, ycol, xcols, io.json = FALSE)
res <- glm.getGLMres(dataset, ycol, xcols, io.json = FALSE)
resultsglm <- glm.getModel(dataset, ycol, xcols, io.json = FALSE)

print("if no errors occurred then the tests were passed.")

