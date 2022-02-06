#' Merges two .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org) by adding the content of the second file (fleIn2) as rows to the first file (fleIn1)
#'
#' @param fleInp vector with file names (including the path, if required) of the data files to be read (c("FILE1.omv", "FILE2.omv"); default: c())
#' @param fleOut name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the data frame with the added columns is returned as variable (but not written)
#' @param typMrg type of merging operation ("all" [default], "common")
#' @param colInd add a column with an indicator (the basename of the file minus the extension) marking from which input data set the respective rows are coming (default: FALSE)
#' @param rstRwN reset row names (i.e., do not keep the row names of the original input data sets but number them consecutively - one to the row number of all input data sets added up; default: TRUE)
#' @param rmvDpl remove duplicated rows (i.e., rows with the same content as a previous row in all columns; default: FALSE)
#' @param usePkg name of the package ("haven" or "foreign") that shall be used to read SPSS, Stata and SAS files; "haven" is the default (it is more comprehensive), but with problems you may try "foreign"
#' @param selSet name of the data set that is to be selected from the workspace (only applies when reading .Rdata-files)
#' @param ...
#' @return a data frame (if fleOut is empty) with where the rows of all input data sets (i.e., the files given in the fleInp-argument) are concatenated
#'
#' @details
#' The different types of merging operations: "all" keeps all existing variables / columns that are contained in any of the input data sets and fills them up with NA where the variable / column doesn't
#' exist in a input data set. "common" only keeps the variables / columns that are common to all input data sets (i.e., that are contained in all data sets).
#' The ellipsis-parameter can be used to submit arguments / parameters to the functions that are used for merging or reading the data. The merging operation uses "rbind". When reading the data, the
#' functions are: "read_omv" (for jamovi-files), "read.table" (for CSV / TSV files; using similar defaults as "read.csv" for CSV and "read.delim" for TSV which both are based upon "read.table" but with
#' adjusted defaults for the respective file types), "readRDS" (for rds-files), "read_sav" (needs R-package "haven") or "read.spss" (needs R-package "foreign") for SPSS-files, read_dta ("haven") /
#' read.dta ("foreign") for Stata-files, read_sas ("haven") for SAS-data-files, and read_xpt ("haven") / read.xport ("foreign") for SAS-transport-files.
#' Please note that if the columns / variables have attributes (e.g., labels), the attributes of the first input data set (i.e., those contained in the first file given in the fleInp-argument) determine
#' the attributes of the output data set (i.e., the attributes of the second to the last input data set are disregarded).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' }
#'
#' @export merge_rows_omv
#'
merge_rows_omv <- function(fleInp = c(), fleOut = "", typMrg = c("all", "common"), colInd = FALSE, rstRwN = TRUE, rmvDpl = FALSE, usePkg = c("haven", "foreign"), selSet = "", ...) {

    # normalize the path of the input files and then check whether the files exist and whether they are of a supported file type
    if (length(fleInp) < 2) {
        stop("A character vector that contains at least two file names is required to be given as fleInp-argument.")
    }
    fleInp <- sapply(fleInp, nrmFle, USE.NAMES = FALSE);
    all(sapply(fleInp, chkFle));
    all(sapply(fleInp, chkExt, vldExt));

    # handle / check further input arguments
    typMrg <- match.arg(typMrg);
    usePkg <- match.arg(usePkg);
    varArg <- list(...);

    # read files
    dtaInp <- vector(mode = "list", length = length(fleInp));
    for (i in seq_along(fleInp)) {
        dtaInp[[i]] <- addInd(read_all(fleInp[i], usePkg, selSet, varArg), ifelse(colInd, fleInp[i], ""));
    }

    # merge files - the additional arguments are the same in either case
    crrArg <- adjArg(c("rbind", "data.frame"), list(), varArg, c());
    # keeping all existing variables, filling the void columns with NA
    if      (typMrg == "all") {
        varNme <- unlist(sapply(dtaInp, names));
        varNme <- varNme[!duplicated(varNme)];
        dtaOut <- addCol(dtaInp[[1]], varNme);
        for (i in setdiff(seq_along(fleInp), 1)) {
            dtaOut <- do.call(rbind, c(list(dtaOut, addCol(dtaInp[[i]], varNme)), crrArg));
        }
    # keeping only variables that are common to all input data sets
    } else if (typMrg == "common") {
        varNme <- Reduce(intersect, sapply(dtaInp, names));
        if (identical(varNme, character(0))) {
            stop(paste("The data sets in the files that were given as fleInp-argument do not contain variables that are overlapping (i.e., contained in all data sets).",
                       "You can either reduce the number of data sets given to fleInp or use \"outer\" as argument for \"typMrg\" (see Details in the help for this function)."));
        }
        dtaOut <- dtaInp[[1]][, varNme];
        for (i in setdiff(seq_along(fleInp), 1)) {
            dtaOut <- do.call(rbind, c(list(dtaOut, dtaInp[[i]][, varNme]), crrArg));
        }
    }

    # remove duplicate rows
    if (rstRwN == TRUE) {
        rownames(dtaOut) <- NULL;
    }

    # remove duplicate rows
    if (rmvDpl == TRUE) {
        dtaOut <- dtaOut[!duplicated(dtaOut), ];
    }

    # write files (if fleOut is not empty) or return resulting data frame
    if (nzchar(fleOut)) {
        write_omv(dtaOut, nrmFle(fleOut));
    } else {
        dtaOut
    }
}

addCol <- function(dtaFrm = NULL, varNme = c()) {
    varDff <- setdiff(varNme, names(dtaFrm));
    if (length(varDff) > 0) {
        cbind(dtaFrm, setNames(data.frame(as.list(NA[seq_along(varDff)])), varDff))[, varNme]
    } else {
        dtaFrm
    }
}

addInd <- function(dtaFrm = NULL, fleNme = "") {
    if (fleNme == "") {
        dtaFrm
    } else {
        cbind(list(fleInd = rep(gsub(paste0(".", tools::file_ext(fleNme)), "", basename(fleNme)), dim(dtaFrm)[1])), dtaFrm);
    }
}
