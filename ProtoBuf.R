savehistory(file="~/.Rhistory")
savehistory(file="~/.Rhistory")
exists("jamovi.coms.Status")
synPkg <- c("RProtoBuf", "jmvcore")
synPkg
flePtB <- system.file("jamovi.proto", package = "jmvcore")
flePtB
requireNamespace("RProtoBuf", quietly = TRUE)
jamovi.coms
jmvReadWrite:::jmvPtB()

results <- jmv::ANOVA(len ~ supp * dose, data = jmv::ToothGrowth)
results <- jmv::ANOVA(len ~ supp * dose, data = jmv:::ToothGrowth)
results <- jmv::ANOVA(len ~ supp * dose, ToothGrowth)
results <- jmv::ANOVA(formula = len ~ supp * dose, data = ToothGrowth)
results
results$parent
X <- results$parent
str(X)
X$asProtoBuf
PB <- X$asProtoBuf()
install.packages("RProtoBuf")
install.packages("RProtoBuf")
PB <- X$asProtoBuf()
history()

pb$options
pb$options$names
pb$options$names == "data"
which(pb$options$names == "data")
pb$options[28]
pb$options[[28]]
pb$options$options[[2]]
pb$options$options[[28]]
pb$options$options[[28]]$toString()
pb$options$options[[28]]$o
formalArgs(jmv::ANOVA)
formals(jmv::ANOVA)
pb$options$names
setdiff(pb$options$names, formalArgs(jmv::ANOVA))
setdiff(formalArgs(jmv::ANOVA), pb$options$names)

results <- jmv::ANOVA(formula = len ~ supp * dose, data = ToothGrowth)
analysis <- results$parent
analysis$asProtoBuf()

analysis$.__enclos_env__$private$.analysisId <- 0
analysis$asProtoBuf()

ptB <- analysis$asProtoBuf()
ptB
ptB$toString()
cat(ptB$toString())
system.file("analysis.R", package = "jmvcore")
system.file("R/analysis.R", package = "jmvcore")
jmvcore::Analysis
jmvcore::Analysis$.asArgs()
jmvcore::Analysis$private$.asArgs()
jmvcore::Analysis$private$.asArgs
jmvReadWrite:::jmvPtB()
jmv::ANOVA
jmvReadWrite:::var2PB
pb <- RProtoBuf::read(jamovi.coms.AnalysisResponse, file('~/analysisPB', open = 'rb', raw = TRUE))
pb
str(pb)
cat(pb$toString())
pb$arbitraryCode
pb$name
pb$ns

pb$asString()

# jmvcore::create
function (ns, name, optionsPB, datasetId, analysisId, revision) 
{
    analysis <- tryStack({
        namespace <- base::getNamespace(ns)
        optionsClass <- namespace[[paste0(name, "Options")]]
        analysisClass <- namespace[[paste0(name, "Class")]]
        options <- optionsClass$new()
        options$read(optionsPB)
        analysis <- analysisClass$new(options = options, datasetId = datasetId, 
            analysisId = analysisId, revision = revision)
    })
    if (isError(analysis)) 
        analysis <- createErrorAnalysis(format("Could not create analysis: {}::{}", 
            ns, name), attr(analysis, "stack"), ns, name, datasetId, 
            analysisId, revision)
    analysis
}

