#' Converts .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org) from long to wide format
#'
#' @param fleInp Name (including the path, if required) of the data file to be read ('FILENAME.omv'; default: '')
#' @param fleOut Name (including the path, if required) of the data file to be written ('FILENAME.omv'; default: ''); if empty, FILENAME from fleInp is extended with '_wide'
#' @param varTme Name of the variable that differentiates multiple records from the same group / individual (default: '')
#' @param varID  Names of one or more variables that identify the same group / individual (default: c())
#' @param varTgt Names of one or more variables to be transformed / reshaped (other variables are excluded, if empty(c()) all variables except varTme and varID are included; default: c())
#' @param varSep Separator character when concatenating the fixed and time-varying part of the variable name ('VAR1.1', 'VAR1.2'; default: '.')
#' @param varOrd How variables / columns are organized: for 'times' (default) the steps of the time varying variable are adjacent, for 'vars' the steps of the original columns in the long dataset
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the order returned from reshape is kept; default: c())
#' @param usePkg Name of the package: 'haven' or 'foreign' that shall be used to read SPSS, Stata and SAS files; 'haven' is the default (it is more comprehensive), but with problems you may try 'foreign'
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @details
#' The ellipsis-parameter (...) can be used to submit arguments / parameters to the functions that are used for transforming or reading the data. The transformation uses 'reshape'. When reading the
#' data, the functions are: 'read_omv' (for jamovi-files), 'read.table' (for CSV / TSV files; using similar defaults as 'read.csv' for CSV and 'read.delim' for TSV which both are based upon
#' 'read.table' but with adjusted defaults for the respective file types), 'readRDS' (for rds-files), 'read_sav' (needs R-package 'haven') or 'read.spss' (needs R-package 'foreign') for SPSS-files,
#' 'read_dta' ('haven') / 'read.dta' ('foreign') for Stata-files, 'read_sas' ('haven') for SAS-data-files, and 'read_xpt' ('haven') / 'read.xport' ('foreign') for SAS-transport-files.
#' Please note that the R-packages 'haven' and 'foreign' are not marked as 'Imports' (i.e., they are not installed by default). If you wish to convert files from SPSS, SAS or Stata and haven't installed
#' them yet, please install them manually (e.g., `install.packages('haven', dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);

#' }
#'
#' @export long2wide_omv
#'
long2wide_omv <- function(fleInp = "", fleOut = "", varID = "", varTme = "", varTgt = c(), varSep = ".", varOrd = c("times", "vars"), varSrt = c(), usePkg = c("haven", "foreign"), selSet = "", ...) {

    # check and format input and output files
    fleInp <- fmtFlI(fleInp, maxLng = 1);
    fleOut <- fmtFlO(fleOut, fleInp, rplExt = "_wide.omv");

    # handle / check further input arguments
    # check varID (can be several) and varTme (must be one), neither can be empty
    if (!all(nzchar(c(varID, varTme)))) {
        stop("Using the arguments varID and varTme is mandatory (i.e., they can't be empty).");
    } else if (length(varID) < 1 || length(varTme) != 1) {
        stop("The argument varID must at least contain one variable, and varTme exactly one variable.");
    }
    varOrd <- match.arg(varOrd);
    usePkg <- match.arg(usePkg);
    varArg <- list(...);

    # read file
    dtaFrm <- read_all(fleInp, usePkg, selSet, varArg);

    # transform data set
    # [1] check whether varID, varTme and varTgt are not empty and exist in the data set
    chkVar(dtaFrm, varID);
    chkVar(dtaFrm, varTme);
    if (!chkVar(dtaFrm, varTgt)) {
        varTgt <- setdiff(names(dtaFrm), c(varID, varTme));
    }
    # [2] store the original variable labels and the steps of the time-varying variable in crrLnT
    crrLnT <- getLbl(dtaFrm, varTme);
    # [3] call "reshape" with having the variable arguments limited to those valid when calling the function
    crrArg <- list(data = dtaFrm[, c(varID, varTme, varTgt)], direction = "wide", idvar = varID, timevar = varTme, sep = varSep);
    dtaFrm <- do.call(stats::reshape, adjArg("reshape", crrArg, varArg, c("data", "direction", "idvar", "timevar")));

    # change the order of column (if requested)
    if (varOrd == "vars") dtaFrm <- chgVrO(dtaFrm);

    # restore the original labels
    dtaFrm <- rstLbl(dtaFrm, crrLnT);

    # sort data set (if varSrt is not empty)
    dtaFrm <- srtFrm(dtaFrm, varSrt);

    # write file
    write_omv(dtaFrm, fleOut)
}

chgVrO <- function(dtaFrm = NULL) {
    varVry <- attr(dtaFrm, "reshapeWide")$varying;
    varLst <- setdiff(names(dtaFrm), varVry);
    for (i in seq_len(dim(varVry)[1])) {
        varLst <- c(varLst, varVry[i, ]);
    }

    dtaFrm[, varLst]
}

getLbl <- function(dtaFrm = NULL, varTme = "") {
    # if only one data frame is given (not a list of them) it needs to be wrapped as list
    if (!is.null(dim(dtaFrm))) dtaFrm <- list(dtaFrm);
    lblLst <- tmeLst <- NULL;
    for (i in seq_along(dtaFrm)) {
        lblLst <- c(lblLst, sapply(dtaFrm[[i]], attr, "jmv-desc"), sapply(dtaFrm[[i]], attr, "label"));
        tmeLst <- unique(c(tmeLst, dtaFrm[[i]][[varTme]]));
    }
    if (all(tmeLst == seq_along(tmeLst))) tmeLst <- NULL;

    list(label = lblLst[! sapply(lblLst, is.null)], times = tmeLst)
}

rstLbl <- function(dtaFrm = NULL, crrLnT = list()) {
    for (i in seq_along(dtaFrm)) {
        varNme <- names(dtaFrm);
        for (crrNme in names(crrLnT$label)) {
            crrLbl <- crrLnT$label[[crrNme]];
            crrCol <- grep(paste0("^", crrNme), varNme);
            for (i in seq_along(crrCol)) {
                attr(dtaFrm[[crrCol[i]]], "jmv-desc") <- ifelse(is.null(crrLnT$times), crrLbl, paste0(crrLbl, " (", as.character(crrLnT$times[i]), ")"));
            }
        }
    }

    dtaFrm
}
