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

## ---- echo=TRUE---------------------------------------------------------------
library(jmvReadWrite)

data = read_jmv(fleNme = system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite"), getSyn = TRUE)
# shows the syntax of the analyses from the .omv-file
attr(data, 'syntax')
# runs the command of the first analysis
eval(parse(text=attr(data, 'syntax')[[1]]))
# runs the command of the second analysis and assigns the output from that analysis to the variable result2
eval(parse(text=paste0('result2 = ', attr(data, 'syntax')[[2]])))
names(result2)
# → "main"      "assump"    "contrasts" "postHoc"   "emm" (the names of the five output tables)

## ---- eval=FALSE--------------------------------------------------------------
#  library(jmvReadWrite)
#  
#  write_jmv(dtaFrm = data, fleNme = 'Trial.omv')

