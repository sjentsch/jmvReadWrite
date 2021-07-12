## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      comment=NA,message=FALSE,
                      warning=FALSE,
                      fig.height=5,
                      fig.width=7,
                      fig.align="center")

## ---- eval=FALSE--------------------------------------------------------------
#  if(!require(devtools)) install.packages("devtools")
#  devtools::install_github("sjentsch/jmvReadWrite")

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("jmvReadWrite")

## ---- echo=TRUE---------------------------------------------------------------
library(jmvReadWrite)
library(jmv)

data = read_jmv(fleNme = system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite"))
jmv::ANOVA(
    formula = len ~ supp + dose + supp:dose,
    data = data,
    effectSize = c("omega"),
    modelTest = TRUE,
    homo = TRUE,
    norm = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  library(jmvReadWrite)
#  
#  write_jmv(dtaFrm = data, fleNme = 'Trial.omv')

