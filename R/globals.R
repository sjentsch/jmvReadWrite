# binds the variable jamovi.coms.AnalysisResponse locally to the function,
# otherwise devtools::check() - required before submitting to CRAN - throws an error
if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("jamovi.coms.AnalysisResponse"));
}

# =================================================================================================
# define unicode-characters and their respective replacements
lstRpl <- list("\x84" = "\"", "\x93" = "\"", "\xc4" = "Ae", "\xd6" = "Oe", "\xdc" = "Ue", "\xdf" = "ss", "\xe4" = "ae", "\xf6" = "oe", "\xfc" = "ue");

lstRpl <- rbind(c("\x84", "\x93", "\xc4", "\xd6", "\xdc", "\xdf", "\xe4", "\xf6", "\xfc"),
                c(  "\"",   "\"",   "Ae",   "Oe",   "Ue",   "ss",   "ae",   "oe",   "ue"));
                
# =================================================================================================
# the next lines store the currently supported versions (stored in meta / MANIFEST.MF)
# and the string that precedes the version number
lstMnf <- list(mnfVer = c("Manifest-Version",        "1.0"),
               datVer = c("Data-Archive-Version",    "1.0.2"),
               jmvVer = c("jamovi-Archive-Version",  "11.0"),
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
# functions for checking parameters (file and directory existence, correct file extension, correct
# dimensions and existence of data frames

chkFle <- function(fleNme = "", fleCnt = "", isZIP = FALSE) {
    if (! file_test("-f", fleNme)) {
        if (nchar(fleCnt) > 0) {
            stop(sprintf("File \"%s\" doesn't contain the file \"%s\".", fleCnt, fleNme));
        } else {
            stop(sprintf("File \"%s\" not found.", fleNme));
        }
    } else if (isZIP) {
        hdrStr <- readBin(tmpHdl <- file(fleNme, "rb"), "character"); close(tmpHdl); rm(tmpHdl);
        # only "PK\003\004" is considered, not "PK\005\006" (empty ZIP) or "PK\007\008" (spanned [over several files])
        if (! hdrStr == "PK\003\004\024") {
            stop(sprintf("File \"%s\" has not the correct file format (is not a ZIP archive).", basename(fleNme)));
        }
        rm(hdrStr);
    }
    TRUE
}

# REMEMBER: requires the full file name, NOT the directory
chkDir <- function(fleNme = "") {
    if (! file_test("-d", dirname(fleNme))) {
        stop(sprintf("Directory (%s) doesn't exist.", dirname(fleNme)));
    }
    TRUE
}

chkDtF <- function(dtaFrm = NULL, minSze = c(1, 1)) {
    if (length(minSze != 2)) minSze <- rep(minSze[1], 2)
    if (is.null(dtaFrm) || ! is.data.frame(dtaFrm) || length(dim(dtaFrm)) != 2) {
        stop("Input data frame is not a data frame or it has either only one or more than two dimensions.");
    } else if (any(dim(dtaFrm) < minSze)) {
        stop(sprintf("The %s dimension of the input data frame has not the required size (%d < %d).",
                     ifelse(which(dim(data) < minSze)[1] == 1, "first", "second"), dim(data)[dim(data) < minSze][1], minSze[dim(data) < minSze][1]));
    }
    TRUE
}

chkExt <- function(fleNme = "", extNme = c("")) {
    if (! hasExt(fleNme, extNme)) {
        stop(sprintf("File name (%s) contains an unsupported file extension (%s).", basename(fleNme), paste(paste0(".", extNme[tools::file_ext(fleNme) != extNme]), collapse = ", ")));
    }
}

hasExt <- function(fleNme = "", extNme = c("")) {
    any(tolower(tools::file_ext(fleNme)) == tolower(extNme))
}

hasPkg <- function(usePkg = c()) {
    all(sapply(usePkg, function(X) nzchar(system.file(package = X))))
}


# =================================================================================================
# get function arguments

fcnArg <- function(fncNme = "") {
    eval(parse(text = paste0("formals(", fncNme, ")")))
}


