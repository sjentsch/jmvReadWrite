#' Converts .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org) from long to wide format
#'
#' @param fleInp Name (including the path, if required) of the data file to be read (e.g., "FILE_IN.omv"; default: ""); can be any supported file type, see Details below
#' @param fleOut Name (including the path, if required) of the data file to be written (e.g., "FILE_OUT.omv"; default: ""); if empty, FILE_IN from fleInp is extended with "_wide(file extension -> .omv)"
#' @param varTme Name of the variable that differentiates multiple records from the same group / individual (default: "")
#' @param varID  Names of one or more variables that identify the same group / individual (default: c())
#' @param varTgt Names of one or more variables to be transformed / reshaped (other variables are excluded, if empty(c()) all variables except varTme and varID are included; default: c())
#' @param varSep Separator character when concatenating the fixed and time-varying part of the variable name ("VAR1.1", "VAR1.2"; default: ".")
#' @param varOrd How variables / columns are organized: for "times" (default) the steps of the time varying variable are adjacent, for "vars" the steps of the original columns in the long dataset
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the order returned from reshape is kept; default: c())
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @details
#' The ellipsis-parameter (...) can be used to submit arguments / parameters to the functions that are used for transforming or reading the data. The transformation uses `reshape`. When reading the
#' data, the functions are: `read_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV which both are based upon
#' `read.table` but with adjusted defaults for the respective file types), `readRDS` (for rds-files), `read_sav` (needs R-package "haven") or `read.spss` (needs R-package "foreign") for SPSS-files,
#' `read_dta` ("haven") / `read.dta` ("foreign") for Stata-files, `read_sas` ("haven") for SAS-data-files, and `read_xpt` ("haven") / `read.xport` ("foreign") for SAS-transport-files. If you would
#' like to use "haven", it may be needed to install it manually (i.e., `install.packages("haven", dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' # generate a test dataframe with 100 (imaginary) participants / units of
#' #  observation (ID), 8 measurement (Meas) of one variable (X)
#' dtaInp <- data.frame(ID = sort(rep(seq(1:100), 8)),
#'                      Meas = rep(seq(1, 8), 100),
#'                      X = runif(800, -10, 10));
#' cat(str(dtaInp));
#' # the output should look like this
#' # 'data.frame': 800 obs. of  3 variables:
#' #  $ ID  : int  1 1 1 1 1 1 1 1 2 2 ...
#' #  $ Meas: int  1 2 3 4 5 6 7 8 1 2 ...
#' #  $ X   : num  ...
#' # this data set is stored as (temporary) RDS-file and later processed by long2wide
#' nmeInp <- paste0(tempfile(), ".rds");
#' nmeOut <- paste0(tempfile(), ".omv");
#' saveRDS(dtaInp, nmeInp);
#' long2wide_omv(fleInp = nmeInp, fleOut = nmeOut, varID = "ID", varTme = "Meas", varTgt = "X");
#' # it is required to give at least the arguments fleInp, varID and varTme
#' # check whether the file was created and its size
#' cat(list.files(dirname(nmeOut), basename(nmeOut)));
#' # -> "file[...].omv" ([...] contains a random combination of numbers / characters
#' cat(file.info(nmeOut)$size);
#' # -> 6200 (approximate size; size may differ in every run [in dependence of
#' #          how well the generated random data can be compressed])
#' cat(str(read_omv(nmeOut, sveAtt = FALSE)));
#' # the data set is now transformed into wide (and each the measurements is now
#' # indicated as a suffix to X; X.1, X.2, ...)
#' # 'data.frame':	100 obs. of  9 variables:
#' #  $ ID : int  1 2 3 4 5 6 7 8 9 10 ...
#' #   ..- attr(*, "jmv-id")= logi TRUE
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X.1: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X.2: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X.3: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X.4: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X.5: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X.6: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X.7: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X.8: num  ...
#' #   ..- attr(*, "missingValues")= list()
#'
#' unlink(nmeInp);
#' unlink(nmeOut);
#' }
#'
#' @export long2wide_omv
#'
long2wide_omv <- function(fleInp = "", fleOut = "", varID = "", varTme = "", varTgt = c(), varSep = ".", varOrd = c("times", "vars"), varSrt = c(), usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and format input and output files
    fleInp <- fmtFlI(fleInp, maxLng = 1);
    fleOut <- fmtFlO(fleOut, fleInp, rplExt = "_wide.omv");

    # handle / check further input arguments
    # check varID (can be several) and varTme (must be one), neither can be empty
    if (!all(nzchar(c(varID, varTme)))) {
        stop("Using the arguments varID and varTme is mandatory (i.e., they can\'t be empty).");
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
