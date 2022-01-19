# binds the variable jamovi.coms.AnalysisResponse locally to the function,
# otherwise devtools::check() - required before submitting to CRAN - throws an error
if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("jamovi.coms.AnalysisResponse"));
}

# =================================================================================================
# the next lines store the currently supported versions (stored in meta / MANIFEST.MF)
# and the string that precedes the version number
lstMnf <- list(mnfVer = c("Manifest-Version",        "1",  "0"),
               datVer = c("Data-Archive-Version",    "1",  "0", "2"),
               jmvVer = c("jamovi-Archive-Version",  "11", "0"),
               crtStr = c("Created-By"));

# the next lines are dealing with storing the global and the data column attributes (that go into
# metadata.json inside the .omv-file; the currently defined defaults are in accordance with
# jamovi-Archive-Version: 11.0 (from jamovi 1.8)
mtaGlb <- list(rowCount = NA, columnCount = NA, removedRows = list(), addedRows = list(), fields = list(), transforms = list());
mtaFld <- list(name = "", id = NA, columnType = "Data", dataType = "Integer", measureType = "Nominal", formula = "", formulaMessage = "",
               parentId = 0, width = 100, type = "number", outputAnalysisId = NA, outputOptionName = "", outputName = "",
               outputDesiredColumnName = "", outputAssignedColumnName = "", importName = "", description = "", transform = 0,
               edits = list(), missingValues = list(), trimLevels = TRUE, filterNo = NA, active = FALSE)
grpMta <- paste0("^", paste(c(names(mtaGlb), names(mtaFld)), collapse = "$|^"), "$");

# =================================================================================================
# functions for handling setting and storing metadata-information

setAtt <- function(attLst = c(), inpObj = NULL, outObj = NULL) {
    for (attNme in attLst) {
        # if the output object is the mtaDta-variable, the input object must be the data frame
        # which contains the attribute in attNme (chkAtt), that are then stored in the mtaDta-
        # variable; the attribute might be empty (chkAtt == FALSE), and then the default is kept
        if        (is.list(outObj) && chkAtt(inpObj, attNme)) {
            outObj[[attNme]] <- attr(inpObj, attNme);
        # if the input object is the mtaDta-variable (which is a list), and the attribute is set in
        # the output object unless the attribute already exists in the ouput object (!chkAtt), then
        # it shouldn't be overwritten
        } else if (is.list(inpObj) && !chkAtt(outObj, attNme)) {
            attr(outObj, attNme) <- inpObj[[attNme]];
        # the case which is critical is if both input and output objects are lists (then the first
        # part of the if-conditions above - is.list - wouldn't work); the problem is that data
        # frames are both lists and data frames, and therefore an error is thrown if BOTH input
        # and output objects are lists but not data frames
        } else if (is.list(inpObj) && !is.data.frame(inpObj) && is.list(outObj) && !is.data.frame(outObj)) {
            cat(attNme);
            cat(attLst);
            cat(utils::str(inpObj));
            cat(utils::str(outObj));
            stop("Error when storing or accessing meta-data information. Please send the file causing the error to sebastian.jentschke@uib.no");
        }
    }
    rm(attNme, inpObj);
    outObj
}

chkAtt <- function(attObj = NULL, attNme = "", attVal = NULL) {
   (!is.null(attr(attObj, attNme)) && length(attr(attObj, attNme)) > 0 && ifelse(!is.null(attVal), grepl(attVal, attr(attObj, attNme)), TRUE))
}

chkFld <- function(fldObj = NULL, fldNme = "", fldVal = NULL) {
   ((fldNme %in% names(fldObj))    && length(fldObj[[fldNme]])     > 0 && ifelse(!is.null(fldVal), grepl(fldVal, fldObj[[fldNme]]),     TRUE))

# =================================================================================================

# Definitions: SPSS commands (copied from the left panel in https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-introduction-guide-command-syntax, replace "-" with "\n",
# restore "T-TEST" manually, find commands with a ":" and delete them, sort the commands, and replace "\n" with "", "", manually added "ELSE", "ELSE IF" and "END IF"as well as the
# command terminator "." after "CACHE", "ELSE", all "END ..."-commands, "EXECUTE", "NEW FILE", "PRESERVE", "RESTORE")
cmdSPS = c("2SLS", "ACF", "ADD DOCUMENT", "ADD FILES", "ADD VALUE LABELS", "ADP", "AGGREGATE", "AIM", "ALSCAL", "ALTER TYPE", "ANACOR", "ANOVA", "APPLY DICTIONARY", "AREG", "ARIMA", "AUTORECODE",
           "BAYES ANOVA", "BAYES CORRELATION", "BAYES INDEPENDENT", "BAYES LOGLINEAR", "BAYES ONESAMPLE", "BAYES REGRESSION", "BAYES RELATED", "BAYES REPEATED", "BEGIN DATA", "BEGIN EXPR",
           "BEGIN GPL", "BEGIN PROGRAM", "BOOTSTRAP", "BREAK", "CACHE.", "CASEPLOT", "CASESTOVARS", "CATPCA", "CATREG", "CCF", "CD", "CLEAR TIME PROGRAM", "CLEAR TRANSFORMATIONS", "CLUSTER",
           "CODEBOOK", "COMMENT", "COMPARE DATASETS", "COMPUTE", "CONJOINT", "CORRELATIONS", "CORRESPONDENCE", "COUNT", "COXREG", "CREATE", "CROSSTABS", "CSCOXREG", "CSDESCRIPTIVES", "CSGLM",
           "CSLOGISTIC", "CSORDINAL", "CSPLAN", "CSSELECT", "CSTABULATE", "CTABLES", "CURVEFIT", "DATAFILE ATTRIBUTE", "DATA LIST", "DATASET ACTIVATE", "DATASET CLOSE", "DATASET COPY",
           "DATASET DECLARE", "DATASET DISPLAY", "DATASET NAME", "DATE", "DEFINE", "DELETE VARIABLES", "DESCRIPTIVES", "DETECTANOMALY", "DISCRIMINANT", "DISPLAY", "DMCLUSTER", "DMLOGISTIC",
           "DMROC", "DMTABLES", "DMTREE", "DOCUMENT", "DO IF", "DO REPEAT", "DROP DOCUMENTS", "ECHO", "ELSE.", "ELSE IF", "END CASE.", "END DATA.", "!ENDDEFINE.", "END EXPR.", "END FILE.",
           "END FILE TYPE.", "END GPL.", "END IF.", "END INPUT PROGRAM.", "END LOOP.", "END MATRIX.", "END PROGRAM.", "END REPEAT.", "ERASE", "EXAMINE", "EXECUTE.", "EXPORT", "EXSMOOTH", "EXTENSION",
           "FACTOR", "FILE HANDLE", "FILE LABEL", "FILE TYPE", "FILTER", "FINISH", "FIT", "FLEISS MULTIRATER KAPPA", "FLIP", "FORMATS", "FREQUENCIES", "GENLIN", "GENLINMIXED", "GENLOG", "GET",
           "GET CAPTURE", "GETCOGNOS", "GET DATA", "GET SAS", "GET STATA", "GETTM1", "GET TRANSLATE", "GGRAPH", "GLM", "GRAPH", "HILOGLINEAR", "HOMALS", "HOST", "IF", "IGRAPH", "IMPORT",
           "INCLUDE", "INFO", "INPUT PROGRAM", "INSERT", "INSERT EXEC", "INSERT HIDDEN", "KEYED DATA LIST", "KM", "KNN", "LEAVE", "LINEAR", "LIST", "LOGISTIC REGRESSION", "LOGLINEAR", "LOOP",
           "MANOVA", "MATCH FILES", "MATRIX", "MATRIX DATA", "MCONVERT", "MEANS", "META BINARY", "META CONTINUOUS", "META ES BINARY", "META ES CONTINUOUS", "META REGRESSION", "MISSING VALUES",
           "MIXED", "MLP", "MODEL CLOSE", "MODEL HANDLE", "MODEL LIST", "MODEL NAME", "MRSETS", "MULTIPLE CORRESPONDENCE", "MULTIPLE IMPUTATION", "MULT RESPONSE", "MVA", "NAIVEBAYES", 
           "NEW FILE.", "NLR", "N OF CASES", "NOMREG", "NONPAR CORR", "NPAR TESTS", "NPTESTS", "NUMERIC", "OLAP CUBES", "OMS", "OMSEND", "OMSINFO", "OMSLOG", "ONEWAY", "OPTIMAL BINNING",
           "ORTHOPLAN", "OUTPUT ACTIVATE", "OUTPUT CLOSE", "OUTPUT DISPLAY", "OUTPUT EXPORT", "OUTPUT MODIFY", "OUTPUT NAME", "OUTPUT NEW", "OUTPUT OPEN", "OUTPUT SAVE", "OVERALS", "PACF",
           "PARTIAL CORR", "PERMISSIONS", "PLANCARDS", "PLS", "PLUM", "POINT", "POWER MEANS INDEPENDENT", "POWER MEANS ONESAMPLE", "POWER MEANS RELATED", "POWER ONEWAY ANOVA",
           "POWER PARTIALCORR", "POWER PEARSON ONESAMPLE", "POWER PROPORTIONS INDEPENDENT", "POWER PROPORTIONS ONESAMPLE", "POWER PROPORTIONS RELATED", "POWER SPEARMAN ONESAMPLE",
           "POWER UNIVARIATE LINEAR", "PPLOT", "PREDICT", "PREFSCAL", "PRESERVE.", "PRINCALS", "PRINT", "PRINT EJECT", "PRINT FORMATS", "PRINT SPACE", "PROBIT", "PROCEDURE OUTPUT",
           "PROPORTIONS", "PROXIMITIES", "PROXSCAL", "QUANTILE REGRESSION", "QUICK CLUSTER", "RANK", "RATIO STATISTICS", "RBF", "READ MODEL", "RECODE", "RECORD TYPE", "REFORMAT", "REGRESSION",
           "RELATIONSHIP MAP", "RELIABILITY", "RENAME VARIABLES", "REPEATING DATA", "REPORT", "REPOSITORY ATTRIBUTES", "REPOSITORY CONNECT", "REPOSITORY COPY", "REREAD", "RESPONSE RATE",
           "RESTORE.", "RMV", "ROC", "ROC ANALYSIS", "SAMPLE", "SAVE", "SAVE CODEPAGE", "SAVE DATA COLLECTION", "SAVE MODEL", "SAVETM1", "SAVE TRANSLATE", "SCRIPT", "SEASON", "SELECT IF",
           "SELECTPRED", "SET", "SHIFT VALUES", "SHOW", "SIMPLAN", "SIMPREP BEGIN", "SIMPREP END", "SIMRUN", "SORT CASES", "SORT VARIABLES", "SPATIAL ASSOCIATION RULES", "SPATIAL MAPSPEC",
           "SPATIAL TEMPORAL PREDICTION", "SPCHART", "SPECTRA", "SPLIT FILE", "STAR JOIN", "STRING", "SUBTITLE", "SUMMARIZE", "SURVIVAL", "SYSFILE INFO", "TABLES", "TCM ANALYSIS", "TCM APPLY",
           "TCM MODEL", "TDISPLAY", "TEMPORARY", "TIME PROGRAM", "TITLE", "TMS BEGIN", "TMS END", "TMS IMPORT", "TMS MERGE", "TREE", "TSAPPLY", "TSET", "TSHOW", "TSMODEL", "TSPLOT", "T-TEST",
           "TWOSTEP CLUSTER", "UNIANOVA", "UPDATE", "USE", "VALIDATEDATA", "VALUE LABELS", "VARCOMP", "VARIABLE ALIGNMENT", "VARIABLE ATTRIBUTE", "VARIABLE LABELS", "VARIABLE LEVEL",
           "VARIABLE ROLE", "VARIABLE WIDTH", "VARSTOCASES", "VECTOR", "VERIFY", "WEIGHT", "WEIGHTED KAPPA", "WLS", "WRITE", "WRITE FORMATS", "XGRAPH", "XSAVE");
# only the following commands / analyses are covered (included) in the conversion ...
cmdAnl = c("ANOVA", "CORRELATIONS", "CROSSTABS", "CTABLES", "DESCRIPTIVES", "EXAMINE", "FACTOR", "FREQUENCIES", "GLM", "GRAPH", "LOGISTIC REGRESSION", "LOGLINEAR", "MANOVA", "MEANS",
           "NONPAR CORR", "NPAR TESTS", "NPTESTS", "ONEWAY", "PARTIAL CORR", "REGRESSION", "RELIABILITY", "SUMMARIZE", "T-TEST", "UNIANOVA");
# the following commands are used to modify the data set (the difference is that for the commands under "cmdAnl" extracting the variables is necessary) ...
cmdMod = c("ADD VALUE LABELS", "ALTER TYPE", "COMMENT", "COMPUTE", "DELETE VARIABLES", "FILTER", "NUMERIC", "RANK", "RECODE", "RENAME VARIABLES", "SAMPLE", "SORT CASES", "SORT VARIABLES", "STRING",
           "USE", "VALUE LABELS", "VARIABLE LABELS", "VARIABLE LEVEL");
# and the following commands are used to load or save data sets
cmdDta = c("SAVE OUTFILE", "GET FILE", "GET DATA", "NEW FILE.")
# grep: match complete commands - assumes that the command is followed by one or more white spaces (or no whitespace if it ends in ".")
grcSPS = gsub("\\.\\\\s+", "\\\\.", paste0(paste0(paste0("^", cmdSPS), collapse="\\s+|"), "\\s+"));
grpAnl = gsub("\\.\\\\s+", "\\\\.", paste0(paste0(paste0("^", cmdAnl), collapse="\\s+|"), "\\s+"));
grpMod = gsub("\\.\\\\s+", "\\\\.", paste0(paste0(paste0("^", cmdMod), collapse="\\s+|"), "\\s+"));
grpDta = gsub("\\.\\\\s+", "\\\\.", paste0(paste0(paste0("^", cmdMod), collapse="\\s+|"), "\\s+"));
# grep: match abbreviated commands (SPSS permits that commands only contain of the first three characters [or more if the first 3 can"t be resolved without ambiguity])
# adding a "\\s+" (one or more whitespaces) is for convenience, for the other matching condition (end of string), "\\s+" is replaced with "$" (see lneCmd = ...)
graSPS <- "";
for (cmdCnt in seq_along(cmdSPS)) {
    if (! grepl(" ", cmdSPS[cmdCnt])) {
        cmdLng = 3; while (length(grep(paste0("^", substr(cmdSPS[cmdCnt], 1, cmdLng)), cmdSPS)) > 1 && cmdLng < nchar(cmdSPS[cmdCnt])) { cmdLng = cmdLng + 1 };
        graSPS <- paste0(graSPS, "^", gsub("\\.$", "", substr(cmdSPS[[cmdCnt]], 1, cmdLng)), ifelse(nchar(gsub("\\.$", "", cmdSPS[[cmdCnt]])) > cmdLng, "[A-Z]*", ""));
    } else {
        splSPS = strsplit(cmdSPS[[cmdCnt]], " ")[[1]];
        for (splCnt in seq_along(splSPS)) {
            graSPS <- paste0(graSPS, ifelse(splCnt > 1, " ", "^"), gsub("\\.$", "", substr(splSPS[splCnt], 1, 3)), ifelse(nchar(gsub("\\.$", "", splSPS[splCnt])) > 3, "[A-Z]*", ""));
        }
    }
    graSPS <- paste0(graSPS, ifelse(grepl("\\.$", cmdSPS[[cmdCnt]]), "\\.", "\\s+"), ifelse(cmdCnt < length(cmdSPS), "|", ""));
}
rm("cmdCnt", "cmdLng", "splSPS", "splCnt");

# define unicode-characters and their respective replacements
lstRpl <- list("\x84" = "\"", "\x93" = "\"", "\xc4" = "Ae", "\xd6" = "Oe", "\xdc" = "Ue", "\xdf" = "ss", "\xe4" = "ae", "\xf6" = "oe", "\xfc" = "ue")

# =================================================================================================
