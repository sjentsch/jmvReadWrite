# binds the variable jamovi.coms.AnalysisResponse locally to the function, otherwise devtools::check() - required for submitting to CRAN - throws an error
if(getRversion() >= "2.15.1")  utils::globalVariables(c("jamovi.coms.AnalysisResponse"));
