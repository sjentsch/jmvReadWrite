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
}

# =================================================================================================
