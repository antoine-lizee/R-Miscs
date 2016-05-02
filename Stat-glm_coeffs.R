## Logisitcs Regressions

#' ---
#' title: "Logisitic Regressions and regularization"
#' author: "Antoine Lizee"
#' date: "April 21, 2016"
#' ---
#' 
#' 

#+ setup, echo = FALSE, warning = FALSE, message = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE, fig.width=4, fig.height=8)

library(dplyr)
set.seed(123456)

#' ## Prepare data
# load data ---------------------------------------------------------------

df <- ggplot2::diamonds %>% 
  mutate(priceBinary = price > median(price)) %>% 
  select(-(cut:clarity))
str(df)

sds <- df %>% sapply(sd)
mus <- df %>% sapply(mean)

dfScale <- df %>% mutate_each(funs(scale), -price, -priceBinary)

#' ## `glm` from stats
# classic glms -------------------------------------------------------------

#' ### Normal "full" logisitc regression
lr1 <- glm(priceBinary ~ depth + carat  + x + y + z, data = df, family = binomial)
summary(lr1)
# Natural coefficients
coeff_lr1 <- coef(lr1)
# Get the standardized coefficients:
# Multiply the coefficients by the sds: (except the intercept)
std_coeff_lr1 <- coeff_lr1 * c(1, c(sds[names(coeff_lr1)][-1]))
# transform the intercept:
std_coeff_lr1[1] <- coeff_lr1[1] + sum(coeff_lr1[-1] * mus[names(coeff_lr1)][-1])

#' ### logisitc regression on standardized data
std_lr1 <- glm(priceBinary ~ depth + carat  + x + y + z, data = dfScale, family = binomial)
summary(std_lr1)
# Natural coefficients
coeff_std_lr1 <- coef(std_lr1)

#' ### Compare -> they are the same.
print(cbind(std_coeff_lr1, coeff_std_lr1))
print(std_coeff_lr1 - coeff_std_lr1)


#' ## Using the glmnet package
# glmnet ------------------------------------------------------------------

#' Compute all models --  **SLOW** (because we directly compute the cross-validated optimums)
# default with standardization & coefficients reported in the feature units
set.seed(123)
cvlr2 <- glmnet::cv.glmnet(x = data.matrix(df[c("depth", "carat", "x", "y", "z")]), 
                           y = df$priceBinary,
                           family = "binomial")
lr2 <- cvlr2$glmnet.fit
# no standardization
set.seed(123)
cvlr2_raw <- glmnet::cv.glmnet(x = data.matrix(df[c("depth", "carat", "x", "y", "z")]), 
                               y = df$priceBinary,
                               family = "binomial", standardize = FALSE)
lr2_raw <- cvlr2_raw$glmnet.fit
# inputting standardized data
set.seed(123)
std_cvlr2 <- glmnet::cv.glmnet(x = data.matrix(dfScale[c("depth", "carat", "x", "y", "z")]), 
                               y = df$priceBinary,
                               family = "binomial")
std_lr2 <- std_cvlr2$glmnet.fit

#' ### Comparison of glmnet fits
#' We can see that the standaradized and non-standardized fits
#' lead to completely different results.
layout(matrix(1:2))
plot(lr2)
plot(lr2_raw)

#' On the other hand, relying on the scaling from glmnet or
#' scaling the features beforehand leads to the same model
layout(matrix(1:2))
plot(lr2, xvar = "lambda")
plot(std_lr2, xvar = "lambda")

#' RQ: looking in the "norm" space for the coefficient variation will
#' show different graphs because the amount of regularisation of the coeffs 
#' is also affected by their value. Try:
#plot(lr2)
#plot(std_lr2)

#' ### Comparison of glmnet optima from the CV

layout(matrix(1:2))
plot(cvlr2, xlim = c(-7, -5), ylim = c(0.2, 0.3))
plot(std_cvlr2, xlim = c(-7, -5), ylim = c(0.2, 0.3))

#' Of course, the optimal lambda would be quite different
#' if the randomness in the fold generation for the CVs was not controlled
#' as we have done here.
#' Here, the optimal lambdas and resulting coefficients are the same:
# Lambdas:
c(cvlr2$lambda.1se, std_cvlr2$lambda.1se)
# Coeffciients:
coeff_lr2 <- coef(cvlr2, s = "lambda.1se")[,1]
coeff_std_lr2 <- coef(std_cvlr2, s = "lambda.1se")[,1]
# Same transformations as above:
std_coeff_lr2 <- coeff_lr2 * c(1, c(sds[names(coeff_lr2)][-1]))
std_coeff_lr2[1] <- coeff_lr2[1] + sum(coeff_lr2[-1] * mus[names(coeff_lr2)][-1])
cbind(coeff_std_lr2, std_coeff_lr2)


