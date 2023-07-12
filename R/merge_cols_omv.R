#' Merges two or more data files by adding the content of other input files as columns to the first input file and outputs them as files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame (with the attribute "fleInp" containing the files to merge) or vector with the names of the input files (including the path, if required)
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is returned instead
#' @param typMrg Type of merging operation: "outer" (default), "inner", "left" or "right"; see Details below
#' @param varBy  Name of the variable by which the data sets are matched, can either be a string, a character or a list (see Details below; default: list())
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the order after merging is kept; default: c())
#' @param psvAnl Whether analyses that are contained in the input file shall be transferred to the output file (TRUE / FALSE; default: FALSE)
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (if fleOut is empty) where the columns of all input data sets (given in the dtaInp-argument) are concatenated
#'
#' @details
#' Using data frames with the input parameter dtaInp is primarily thought to be used when calling merge_cols_omv from the jamovi-module jTransform. For the use in R, it is strongly recommended to use a
#' character vector with the file names instead.
#' There are four different types of merging operations: "outer" keeps all cases (but columns in the resulting data set may be empty if they did not contain values in same input data sets), "inner" keeps
#' only those cases where all datasets contain the same value in the matching variable, for "left" all cases from the first data set in dtaInp are kept (whereas cases that are only contained in input data
#' set two or higher are dropped), for "right" all cases from the second (or any higher) data set in dtaInp are kept. The behaviour of "left" and "right" may be somewhat difficult to predict in case of
#' merging several data sets, therefore "outer" might be a safer choice if several data sets are merged.
#' The variable that is used for matching (varBy) can either be a string (if all datasets contain a matching variable with the same name), a character vector (containing several matching variables that
#' are the same for all data sets) or a list with the same length as dtaInp. In the latter case, each cell of that list can again contain either a string (one matching variable for each data set in dtaInp)
#' or a character vector (several matching variables for each data set in dtaInp; NB: all character vectors in the cells of the list must have the same length as it is necessary to always use the same
#' number of matching variables when merging).
#' The ellipsis-parameter (...) can be used to submit arguments / parameters to the functions that are used for merging or reading the data. Adding columns uses `merge`. When reading the data, the
#' functions are: `read_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV which both are based upon `read.table` but
#' with adjusted defaults for the respective file types), `readRDS` (for rds-files), `read_sav` (needs R-package "haven") or `read.spss` (needs R-package "foreign") for SPSS-files, `read_dta`
#' ("haven") / `read.dta` ("foreign") for Stata-files, `read_sas` ("haven") for SAS-data-files, and `read_xpt` ("haven") / `read.xport` ("foreign") for SAS-transport-files. If you would like to use
#' "haven", it may be needed to install it manually (i.e., `install.packages("haven", dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite)
#' dtaInp <- bfi_sample2
#' nmeInp <- paste0(tempfile(), "_", 1:3, ".rds")
#' nmeOut <- paste0(tempfile(), ".omv")
#' for (i in seq_along(nmeInp)) {
#'     saveRDS(stats::setNames(dtaInp, c("ID", paste0(names(dtaInp)[-1], "_", i))), nmeInp[i])
#' }
#' # save dtaInp three times (i.e., the length of nmeInp), adding "_" + 1 ... 3 as index
#' # to the data variables (A1 ... O5, gender, age → A1_1, ...)
#' merge_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varBy = "ID")
#' cat(file.info(nmeOut)$size)
#' # -> 17731 (size may differ on different OSes)
#' dtaOut <- read_omv(nmeOut, sveAtt = FALSE)
#' # read the data set where the three original datasets were added as columns and show
#' # the variable names
#' cat(names(dtaOut))
#' cat(names(dtaInp))
#' # compared to the input data set, we have the same names (expect for "ID" which was
#' # used for matching and that each variable had added an indicator from which data
#' # set they came)
#' cat(dim(dtaInp), dim(dtaOut))
#' # the first dimension of the data sets (rows) stayed the same (250), whereas the
#' # second dimension is now approx. three times as large (28 -> 82):
#' # 28 - 1 (for "ID") = 27 * 3 + 1 (for "ID") = 82
#' cat(colMeans(dtaInp[2:11]))
#' cat(colMeans(dtaOut[2:11]))
#' # it's therefore not much surprise that the values of the column means for the first
#' # 10 variables of dtaInp and dtaOut are the same too
#'
#' unlink(nmeInp)
#' unlink(nmeOut)
#' }
#'
#' @export merge_cols_omv
#'
merge_cols_omv <- function(dtaInp = NULL, fleOut = "", typMrg = c("outer", "inner", "left", "right"), varBy = list(), varSrt = c(), psvAnl = FALSE, usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, fleOut = fleOut, minDF = 2, maxDF = Inf, usePkg = usePkg, selSet = selSet, ...)
    fleOut <- attr(dtaFrm[[1]], "fleOut")

    # store attributes
    attCol <- list()
    for (i in seq_along(dtaFrm)) attCol <- c(attCol, sapply(dtaFrm[[i]][, setdiff(names(dtaFrm[[i]]), names(attCol))], attributes))
    attDF <- attributes(dtaFrm[[1]])

    # check the matching variable(s)
    varBy <- chkByV(varBy, dtaFrm)

    # merge files ([1] determine arguments, [2] merge using a temporary variable which afterwards is written back)
    typMrg <- match.arg(typMrg)
    crrArg <- list(x = NULL, y = NULL, by.x = "", by.y = "", all.x = ifelse(any(typMrg %in% c("outer", "left")), TRUE, FALSE), all.y = ifelse(any(typMrg %in% c("outer", "right")), TRUE, FALSE))
    crrArg <- adjArg(c("merge", "data.frame"), crrArg, list(...), c("x", "y", "by.x", "by.y", "all.x", "all.y"))
    tmpMrg <- dtaFrm[[1]]
    for (i in setdiff(seq_along(dtaFrm), 1)) {
        tmpMrg <- do.call(merge, c(list(x = tmpMrg, y = dtaFrm[[i]], by.x = varBy[[1]], by.y = varBy[[i]]), crrArg[!grepl("^x$|^y$|^by.x$|^by.y$", names(crrArg))]))
    }
    dtaFrm <- tmpMrg

    # sort data frame (if varSrt not empty)
    dtaFrm <- srtFrm(dtaFrm, varSrt)

    # restore attributes
    for (crrAtt in setdiff(names(attDF), c("names", "row.names", "class"))) attr(dtaFrm, crrAtt) <- attDF[[crrAtt]]
    for (crrNme in names(dtaFrm)) {
        if (!is.null(attCol[[crrNme]])) {
            dtaFrm[crrNme] <- setAtt(setdiff(names(attCol[[crrNme]]), names(attributes(dtaFrm[crrNme]))), attCol[[crrNme]], dtaFrm[crrNme])
        }
    }

    # write the resulting data frame to the output file or, if no output file
    # name was given, return the data frame
    if (!is.null(fleOut) && nzchar(fleOut)) {
        write_omv(dtaFrm, fleOut)
        # transfer analyses from input to output file
        if (psvAnl) {
            if (is.character(dtaInp)) {
                xfrAnl(dtaInp[[1]], fleOut)
            } else {
                warning("psvAnl is only possible if dtaInp is a file name (analyses are not stored in data frames, only in the jamovi files).")
            }
        }
        return(invisible(NULL))
    } else {
        if (psvAnl) warning("psvAnl is only possible if fleOut is a file name (analyses are not stored in data frames, only in the jamovi files).")
        dtaFrm
    }
}

chkByV <- function(varBy = list(), dtaFrm = NULL) {
    # varBy is empty
    if ((is.list(varBy)      && length(varBy) == 0) ||
        (is.vector(varBy)    && length(varBy) == 0) ||
        (is.character(varBy) && !nzchar(varBy)) ||
        is.null(varBy)) {
        return(rep(list(mtcVar(dtaFrm)), length(dtaFrm)))
    # varBy is a list with the same length as dtaFrm
    } else if (is.list(varBy) && length(varBy) == length(dtaFrm)) {
        if (all(sapply(seq_along(dtaFrm), function(i) all(varBy[[i]] %in% names(dtaFrm[[i]]))))) {
            return(varBy)
        } else {
            stop("Not all data sets given in dtaInp contain the variable(s) / column(s) that shall be used for matching.")
        }
    # varBy is a character vector (without empty elements) or a string
    } else if ((is.vector(varBy) && !is.list(varBy) && length(varBy) >= 1 && all(nzchar(varBy))) || is.character(varBy)) {
        if (all(sapply(dtaFrm, function(x) all(varBy %in% names(x))))) {
            return(rep(list(varBy), length(dtaFrm)))
        } else {
            stop("Not all data sets given in dtaInp contain the variable(s) / column(s) that shall be used for matching.")
        }
    } else {
        stop("varBy must be either a list (with the same length as dtaInp), a character vector, or a string.")
    }
}

mtcVar <- function(dtaFrm = NULL) {
    varCmm <- names(dtaFrm[[1]])
    for (i in setdiff(seq_along(dtaFrm), 1)) {
        varCmm <- intersect(varCmm, names(dtaFrm[[i]]))
    }
    varCmm
}
