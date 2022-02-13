# binds the variable jamovi.coms.AnalysisResponse locally to the function,
# otherwise devtools::check() - required before submitting to CRAN - throws an error
if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("jamovi.coms.AnalysisResponse"));
}

# =================================================================================================
# define unicode-characters and their respective replacements
lstRpl <- rbind(c("\x84", "\x93", "\xc4", "\xd6", "\xdc", "\xdf", "\xe4", "\xf6", "\xfc"),
                c("\"",   "\"",   "Ae",   "Oe",   "Ue",   "ss",   "ae",   "oe",   "ue"));
                
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
# functions for checking parameters (file and directory existence, correct file extension, correct
# dimensions and existence of data frames) and normalizing the file name

#            jamovi  CSV   TSV     Rdata           RDS   SPSS           Stata  SAS
vldExt <- c("omv",  "csv", "tsv", "rdata", "rda", "rds", "sav", "zsav", "dta", "sas7bdat", "sd2", "sd7", "xpt", "stx", "stc");

# REMEMBER: requires the full file name, NOT the directory
chkDir <- function(fleNme = "", wrtPrm = TRUE) {
    if (! utils::file_test("-d", dirname(fleNme))) {
        stop(sprintf("Directory (%s) doesn\'t exist.", dirname(fleNme)));
    }
    if (file.access(dirname(fleNme), mode = 2) != 0) {
        stop(sprintf("The directory (%s) exists, but you don\'t have writing permissions in that directory.", dirname(fleNme)));
    }
    TRUE
}

chkDtF <- function(dtaFrm = NULL, minSze = c(1, 1)) {
    if (length(minSze) != 2) minSze <- rep(minSze[1], 2)
    if (is.null(dtaFrm) || ! is.data.frame(dtaFrm) || length(dim(dtaFrm)) != 2) {
        stop("Input data are either not a data frame or have incorrect (only one or more than two) dimensions.");
    } else if (any(dim(dtaFrm) < minSze)) {
        stop(sprintf("The %s dimension of the input data frame has not the required size (%d < %d).",
                     ifelse(which(dim(dtaFrm) < minSze)[1] == 1, "first", "second"), dim(dtaFrm)[dim(dtaFrm) < minSze][1], minSze[dim(dtaFrm) < minSze][1]));
    }
    TRUE
}

chkExt <- function(fleNme = "", extNme = c("")) {
    if (! hasExt(fleNme, extNme)) {
        stop(sprintf("File name (%s) contains an unsupported file extension (%s).", basename(fleNme), paste(paste0(".", extNme[tools::file_ext(fleNme) != extNme]), collapse = ", ")));
    }
    TRUE
}

chkFle <- function(fleNme = "", fleCnt = "", isZIP = FALSE) {
    if (! utils::file_test("-f", fleNme)) {
        if (nchar(fleCnt) > 0) {
            stop(sprintf("File \"%s\" doesn\'t contain the file \"%s\".", fleCnt, fleNme));
        } else {
            stop(sprintf("File \"%s\" not found.", fleNme));
        }
    } else if (isZIP) {
        hdrStr <- readBin(tmpHdl <- file(fleNme, "rb"), "character"); close(tmpHdl);
        # only "PK\003\004" is considered, not "PK\005\006" (empty ZIP) or "PK\007\008" (spanned [over several files])
        if (! hdrStr == "PK\003\004\024") {
            stop(sprintf("File \"%s\" has not the correct file format (is not a ZIP archive).", basename(fleNme)));
        }
    }
    TRUE
}

chkVar <- function(dtaFrm = NULL, varNme = c()) {
    if (is.null(varNme) || length(varNme) == 0 || !all(nzchar(varNme))) return(FALSE);
    if (!all(varNme %in% names(dtaFrm))) {
        stop(sprintf("The variable(s) %s are not contained in the current data set.", paste(varNme[! (varNme %in% names(dtaFrm))], collapse = ", ")));
    }
    TRUE
}

hasExt <- function(fleNme = "", extNme = c("")) {
    any(tolower(tools::file_ext(fleNme)) == tolower(extNme))
}

hasPkg <- function(usePkg = c()) {
    all(sapply(usePkg, function(X) nzchar(system.file(package = X))))
}

nrmFle <- function(fleNme = "") {
    file.path(normalizePath(dirname(fleNme)), basename(fleNme))
}

fmtFlI <- function(fleInp = c(), minLng = 1, maxLng = Inf, excExt = "") {
    # normalize the path of the input file and then check whether the file exists and whether it is of a supported file type
    # ("omv" / "jamovi, " are excluded since it makes little sense to convert from jamovi to jamovi-files)
    # if fleOut is empty, fleInp is used with its file extension replaced with ".omv"
    if (length(fleInp) < minLng || length(fleInp) > maxLng) {
        stop(sprintf("The fleInp-argument is supposed to be a character vector with a minimal length of %.0f and a maximal length of %.0f (current length is %.0f).%s",
                     minLng, maxLng, length(fleInp), ifelse(length(fleInp) > maxLng, "\n  If you would like to process several files, call the function individually for each.", "")));
    }
    fleInp <- unname(sapply(fleInp, nrmFle));
    all(sapply(fleInp, chkFle));
    all(sapply(fleInp, chkExt, setdiff(vldExt, excExt)));
    fleInp
}

fmtFlO <- function(fleOut = "", fleInp = "", rplExt = "") {
    if (nzchar(fleOut)) {
        nrmFle(fleOut)
    } else if (length(fleInp) == 1 && nzchar(fleInp[1])) {
        sub(paste0(".", tools::file_ext(fleInp[1])), rplExt, fleInp[1])
    } else {
        stop(paste0("Either fleOut needs to be given as a valid non-empty file name or a single entry in fleInp where the extension is replaced with: \"", rplExt, "\"."));
    }
}

# =================================================================================================
# get function arguments and adjust them / select those valid for the current function call

adjArg <- function(fcnNme = c(), dflArg = list(), varArg = list(), fxdArg = c()) {
    chgArg <- setdiff(intersect(fcnArg(fcnNme), names(varArg)), fxdArg);
    c(dflArg[setdiff(names(dflArg), chgArg)], varArg[chgArg])
}

fcnArg <- function(fcnNme = c()) {
    if        (is.character(fcnNme) && length(fcnNme) == 1) {
        eval(parse(text = paste0("formalArgs(", fcnNme, ")")))
    } else if (is.character(fcnNme) && length(fcnNme) == 2) {
        eval(parse(text = paste0("formalArgs(getS3method(\"", fcnNme[1], "\", \"", fcnNme[2], "\"))")))
    } else {
        stop("The argument to fcnArg must be a character (vector) with 1 or 2 elements.");
    }
}


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
    outObj
}

rmvAtt <- function(attObj = NULL) {
    for (crrAtt in setdiff(names(attributes(attObj)), c("class", "comment", "dim", "jmv-id", "jmv-desc", "levels", "names", "row.names", "values"))) {
        attr(attObj, crrAtt) <- NULL;
    }
    attObj
}

chkAtt <- function(attObj = NULL, attNme = "", attVal = NULL) {
   (!is.null(attr(attObj, attNme)) && length(attr(attObj, attNme)) > 0 && ifelse(!is.null(attVal), grepl(attVal, attr(attObj, attNme)), TRUE))
}

chkFld <- function(fldObj = NULL, fldNme = "", fldVal = NULL) {
   ((fldNme %in% names(fldObj))    && length(fldObj[[fldNme]])     > 0 && ifelse(!is.null(fldVal), grepl(fldVal, fldObj[[fldNme]]),     TRUE))
}
