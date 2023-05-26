#' Merges two .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>) by adding the content of the second, etc.  file(s) as rows to the first file
#'
#' @param fleInp Vector with file names (including the path, if required) of the data files to be read (c("FILE1.omv", "FILE2.omv"); default: c()); can be any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the data frame with the added columns is returned as variable (but not written)
#' @param typMrg Type of merging operation: "all" (default) or  "common"; see also Details
#' @param colInd Add a column with an indicator (the basename of the file minus the extension) marking from which input data set the respective rows are coming (default: FALSE)
#' @param rstRwN Reset row names (i.e., do not keep the row names of the original input data sets but number them consecutively - one to the row number of all input data sets added up; default: TRUE)
#' @param rmvDpl Remove duplicated rows (i.e., rows with the same content as a previous row in all columns; default: FALSE)
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the order after merging is kept; default: c())
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (if fleOut is empty) with where the rows of all input data sets (i.e., the files given in the fleInp-argument) are concatenated
#'
#' @details
#' The different types of merging operations: "all" keeps all existing variables / columns that are contained in any of the input data sets and fills them up with NA where the variable / column doesn't
#' exist in a input data set. "common" only keeps the variables / columns that are common to all input data sets (i.e., that are contained in all data sets).
#' The ellipsis-parameter can be used to submit arguments / parameters to the functions that are used for merging or reading the data. The merging operation uses `rbind`. When reading the data, the
#' functions are: `read_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV which both are based upon `read.table` but with
#' adjusted defaults for the respective file types), `readRDS` (for rds-files), `read_sav` (needs R-package "haven") or `read.spss` (needs R-package "foreign") for SPSS-files, read_dta ("haven") /
#' read.dta ("foreign") for Stata-files, read_sas ("haven") for SAS-data-files, and read_xpt ("haven") / read.xport ("foreign") for SAS-transport-files. If you would like to use "haven", it may be needed
#' to install it manually (i.e., `install.packages("haven", dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite)
#' dtaInp <- bfi_sample2
#' nmeInp <- paste0(tempfile(), "_", 1:3, ".rds")
#' nmeOut <- paste0(tempfile(), ".omv")
#' for (i in seq_along(nmeInp)) saveRDS(dtaInp[-i - 1], nmeInp[i])
#' # save dtaInp three times (i.e., the length of nmeInp), removing one data columns in
#' # each data set (for demonstration purposes, A1 in the first, A2 in the second, ...)
#' merge_rows_omv(fleInp = nmeInp, fleOut = nmeOut, colInd = TRUE)
#' cat(file.info(nmeOut)$size)
#' # -> 10767 (size may differ on different OSes)
#' dtaOut <- read_omv(nmeOut, sveAtt = FALSE)
#' # read the data set where the three original datasets were added as rows and show
#' # the variable names
#' cat(names(dtaInp))
#' cat(names(dtaOut))
#' # compared to the input data set, we have the same variable names; fleInd (switched
#' # on by colInd = TRUE and showing from which data set the rows are coming from) is
#' # new and A1 is moved to the end of the list (the "original" order of variables may
#' # not always be preserved and columns missing from at least one of the input data
#' # sets may be added at the end)
#' cat(dim(dtaInp), dim(dtaOut))
#' # the first dimension of the data sets (rows) is now three times of that of the input
#' # data set (250 -> 750), the second dimension (columns / variables) is increased by 1
#' # (for "fleInd")
#'
#' merge_rows_omv(fleInp = nmeInp, fleOut = nmeOut, typMrg = "common")
#' # the argument typMrg = "common" removes the columns that are not present in all of
#' # the input data sets (i.e., A1, A2, A3)
#' dtaOut <- read_omv(nmeOut, sveAtt = FALSE)
#' # read the data set where the three original datasets were added as rows and show
#' # the variable names
#' cat(names(dtaInp))
#' cat(names(dtaOut))
#' # compared to the input data set, the variables that were missing in at least one
#' # data set (i.e., "A1", "A2" and "A3") are removed
#' cat(dim(dtaInp), dim(dtaOut))
#' # the first dimension of the data sets (rows) is now three times of that of the
#' # input data set (250 -> 750), the second dimension (columns / variables) is
#' # reduced by 3 (i.e., "A1", "A2", "A3")
#'
#' unlink(nmeInp)
#' unlink(nmeOut)
#' }
#'
#' @export merge_rows_omv
#'
merge_rows_omv <- function(fleInp = c(), fleOut = "", typMrg = c("all", "common"), colInd = FALSE, rstRwN = TRUE, rmvDpl = FALSE, varSrt = c(), usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and format input file names and handle / check further input arguments
    fleInp <- fmtFlI(fleInp, minLng = 2)
    typMrg <- match.arg(typMrg)
    usePkg <- match.arg(usePkg)
    varArg <- list(...)

    # read files
    dtaInp <- vector(mode = "list", length = length(fleInp))
    for (i in seq_along(fleInp)) {
        dtaInp[[i]] <- addInd(read_all(fleInp[i], usePkg, selSet, varArg), ifelse(colInd, fleInp[i], ""))
    }

    # merge files - the additional arguments are the same in either case
    crrArg <- adjArg(c("rbind", "data.frame"), list(), varArg, c())
    # keeping all existing variables, filling the void columns with NA
    if      (typMrg == "all") {
        # determine the variable names and types in all input data sets and remove
        # duplicates(order ensures that the data set that contains most variables
        # is prioritized, but yet, it is still impossible to preserve the whole
        # order - i.e., variables missing in one data set may end up at the end)
        varNme <- unlist(sapply(dtaInp[order(-sapply(sapply(dtaInp, dim, simplify = FALSE), "[[", 2))], names,                                                  simplify = FALSE))
        varTyp <- unlist(sapply(dtaInp[order(-sapply(sapply(dtaInp, dim, simplify = FALSE), "[[", 2))], function(D) unlist(sapply(D, function(C) class(C)[1])), simplify = FALSE))
        varNme <- varNme[!duplicated(varNme)]
        for (crrNme in varNme) {
            if (sum(names(varTyp) == crrNme) <= 1) next
            if (all(duplicated(varTyp[names(varTyp) == crrNme])[-1])) {
                varTyp <- varTyp[-which(names(varTyp) == crrNme)[-1]]
            } else {
                stop(sprintf("Variable %s has different types:\n%s\n", crrNme, paste(paste0(basename(fleInp), rep(": ", sum(names(varTyp) == crrNme)), varTyp[names(varTyp) == crrNme]), collapse = "\n")))
            }
        }
        if (length(varNme) != length(varTyp)) stop("Something went wrong when comparing the variable types of the input data files. Please send the data files to sebastian.jentschke@uib.no for debugging.")
        dtaOut <- addCol(dtaInp[[1]], varNme, varTyp)
        for (i in setdiff(seq_along(fleInp), 1)) {
            crrInp <- addCol(dtaInp[[i]], varNme, varTyp)
            dtaOut <- do.call(rbind, c(list(dtaOut, crrInp), crrArg))
        }
    # keeping only variables that are common to all input data sets
    } else if (typMrg == "common") {
        varNme <- Reduce(intersect, sapply(dtaInp, names, simplify = FALSE))
        if (identical(varNme, character(0))) {
            stop(paste("The data sets in the files that were given as fleInp-argument do not contain variables that are overlapping (i.e., contained in all data sets).",
                       "You can either reduce the number of data sets given to fleInp or use \"outer\" as argument for \"typMrg\" (see Details in the help for this function)."))
        }
        dtaOut <- dtaInp[[1]][, varNme]
        for (i in setdiff(seq_along(fleInp), 1)) {
            dtaOut <- do.call(rbind, c(list(dtaOut, dtaInp[[i]][, varNme]), crrArg))
        }
    }

    # remove row names
    if (rstRwN == TRUE) {
        rownames(dtaOut) <- NULL
    }

    # remove duplicate rows
    if (rmvDpl == TRUE) {
        dtaOut <- dtaOut[!duplicated(dtaOut[, setdiff(varNme, "fleInd")]), ]
    }

    # sort data frame (if varSrt not empty)
    dtaOut <- srtFrm(dtaOut, varSrt)

    # write files (if fleOut is not empty) or return resulting data frame
    if (nzchar(fleOut)) {
        write_omv(dtaOut, nrmFle(fleOut))
    } else {
        dtaOut
    }
}

addCol <- function(dtaFrm = NULL, varNme = c(), varTyp = c()) {
    varDff <- setdiff(varNme, names(dtaFrm))
    for (i in seq_along(varDff)) {
        eval(parse(text = paste0("dtaFrm[varDff[i]] <- as.", varTyp[[varDff[i]]], "(NA)")))
    }
    dtaFrm
}

addInd <- function(dtaFrm = NULL, fleNme = "") {
    if (fleNme == "") {
        dtaFrm
    } else {
        cbind(list(fleInd = rep(gsub(paste0(".", tools::file_ext(fleNme)), "", basename(fleNme)), dim(dtaFrm)[1])), dtaFrm)
    }
}
