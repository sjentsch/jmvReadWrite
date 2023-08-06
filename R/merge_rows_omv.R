#' Merges two .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>) by adding the content of the second, etc.  file(s) as rows to the
#' first file
#'
#' @param dtaInp Either a data frame (with the attribute "fleInp" containing the files to merge) or vector with the names of the input files (including the
#'               path, if required; "FILENAME.ext"; default: NULL); files can be of any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param typMrg Type of merging operation: "all" (default) or  "common"; see also Details
#' @param colInd Add a column with an indicator (the basename of the file minus the extension) marking from which input data set the respective rows are coming
#'               (default: FALSE)
#' @param rstRwN Reset row names (i.e., do not keep the row names of the original input data sets but number them consecutively - one to the row number of all
#'               input data sets added up; default: TRUE)
#' @param rmvDpl Remove duplicated rows (i.e., rows with the same content as a previous row in all columns; default: FALSE)
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the order after merging is kept; default: c())
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
#'               base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) where the rows of all input data sets (given in the `dtaInp`-argument) are concatenated
#'
#' @details
#' * Using data frames with the input parameter `dtaInp` is primarily thought to be used when calling `merge_cols_omv` from the jamovi-modules `jTransform` and
#'   `Rj`. For the use in R, it is strongly recommended to use a character vector with the file names instead.
#' * There are four different types of merging operations (defined via `typMrg`): "all" keeps all existing variables / columns that are contained in any of the
#'   input data sets and fills them up with NA where the variable / column doesn't exist in an input data set. "common" only keeps the variables / columns that
#'   are common to all input data sets (i.e., that are contained in all data sets).
#' * `varSrt` can be either a character or a character vector (with one or more variables respectively). The sorting order for a particular variable can be
#'   inverted with preceding the variable name with "-". Please note that this doesn't make sense and hence throws a warning for certain variable types (e.g.,
#'   factors).
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for merging or reading the data. By clicking on
#'   the respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take.
#' * Adding columns uses `rbind` (with some further operation, adding missing columns (filled with NAs), if `typMrg` is "all").
#' * The functions for reading and writing the data are: `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar
#'   defaults as `read.csv` for CSV and `read.delim` for TSV which both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files),
#'   `read_sav` (needs R-package `haven`) or `read.spss` (needs R-package `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for
#'   Stata-files, `read_sas` (`haven`) for SAS-data-files, and `read_xpt` (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you would like to
#'   use `haven`, you may need to install it using `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `merge_rows_omv` internally uses the following functions: Adding columns uses [rbind()]. For reading and writing data files in different formats:
#'   [jmvReadWrite::read_omv()] and [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files,
#'   [readRDS()] for .rds-files, [haven::read_sav()] or [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for Stata-files,
#'   [haven::read_sas()] for SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite)
#' dtaInp <- bfi_sample2
#' nmeInp <- paste0(tempfile(), "_", 1:3, ".rds")
#' nmeOut <- tempfile(fileext = ".omv")
#' for (i in seq_along(nmeInp)) saveRDS(dtaInp[-i - 1], nmeInp[i])
#' # save dtaInp three times (i.e., the length of nmeInp), removing one data columns in
#' # each data set (for demonstration purposes, A1 in the first, A2 in the second, ...)
#' merge_rows_omv(dtaInp = nmeInp, fleOut = nmeOut, colInd = TRUE)
#' cat(file.info(nmeOut)$size)
#' # -> 10767 (size may differ on different OSes)
#' dtaOut <- read_omv(nmeOut, sveAtt = FALSE)
#' unlink(nmeOut)
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
#' merge_rows_omv(dtaInp = nmeInp, fleOut = nmeOut, typMrg = "common")
#' # the argument typMrg = "common" removes the columns that are not present in all of
#' # the input data sets (i.e., A1, A2, A3)
#' dtaOut <- read_omv(nmeOut, sveAtt = FALSE)
#' unlink(nmeOut)
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
#' }
#'
#' @export merge_rows_omv
#'
merge_rows_omv <- function(dtaInp = NULL, fleOut = "", typMrg = c("all", "common"), colInd = FALSE, rstRwN = TRUE, rmvDpl = FALSE, varSrt = c(), usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, minDF = 2, maxDF = Inf, usePkg = usePkg, selSet = selSet, ...)
    if (is.character(dtaInp)) fleInp <- dtaInp else fleInp <- c("input data frame", attr(dtaInp, "fleInp"))
    if (colInd) dtaFrm <- lapply(seq_along(dtaFrm), function(i) addIdx(dtaFrm[[i]], fleInp[i]))

    # merge files - the additional arguments are the same in either case
    typMrg <- match.arg(typMrg)
    crrArg <- adjArg(c("rbind", "data.frame"), list(), list(...), c())
    # keeping all existing variables, filling the void columns with NA
    if      (typMrg == "all") {
        # determine the variable names and types in all input data sets and remove
        # duplicates(order ensures that the data set that contains most variables
        # is prioritized, but yet, it is still impossible to preserve the whole
        # order - i.e., variables missing in one data set may end up at the end)
        varNme <- unlist(sapply(dtaFrm[order(-sapply(sapply(dtaFrm, dim, simplify = FALSE), "[[", 2))], names,                                                  simplify = FALSE))
        varTyp <- unlist(sapply(dtaFrm[order(-sapply(sapply(dtaFrm, dim, simplify = FALSE), "[[", 2))], function(D) unlist(sapply(D, function(C) class(C)[1])), simplify = FALSE))
        varNme <- varNme[!duplicated(varNme)]
        for (crrNme in varNme) {
            if (sum(names(varTyp) == crrNme) <= 1) next
            if (all(duplicated(varTyp[names(varTyp) == crrNme])[-1])) {
                varTyp <- varTyp[-which(names(varTyp) == crrNme)[-1]]
            } else {
                stop(sprintf("Variable %s has different types:\n%s\n", crrNme, paste(paste0(basename(fleInp),
                  rep(": ", sum(names(varTyp) == crrNme)), varTyp[names(varTyp) == crrNme]), collapse = "\n")))
            }
        }
        if (length(varNme) != length(varTyp)) stop("Something went wrong when comparing the variable types of the input data files. Please send the data files to sebastian.jentschke@uib.no for debugging.")
        tmpMrg <- addCol(dtaFrm[[1]], varNme, varTyp)
        for (i in setdiff(seq_along(dtaFrm), 1)) {
            crrInp <- addCol(dtaFrm[[i]], varNme, varTyp)
            tmpMrg <- do.call(rbind, c(list(tmpMrg, crrInp), crrArg))
        }
        dtaFrm <- tmpMrg
    # keeping only variables that are common to all input data sets
    } else if (typMrg == "common") {
        varNme <- Reduce(intersect, sapply(dtaFrm, names, simplify = FALSE))
        if (identical(varNme, character(0))) {
            stop(paste("The data sets in the files that were given as dtaInp-argument do not contain variables that are overlapping (i.e., contained in all data sets).",
                       "You can either reduce the number of data sets given to dtaInp or use \"outer\" as argument for \"typMrg\" (see Details in the help for this function)."))
        }
        tmpMrg <- dtaFrm[[1]][, varNme]
        for (i in setdiff(seq_along(dtaFrm), 1)) {
            tmpMrg <- do.call(rbind, c(list(tmpMrg, dtaFrm[[i]][, varNme]), crrArg))
        }
        dtaFrm <- tmpMrg
    }

    # remove row names
    if (rstRwN == TRUE) {
        rownames(dtaFrm) <- NULL
    }

    # remove duplicate rows
    if (rmvDpl == TRUE) {
        dtaFrm <- dtaFrm[!duplicated(dtaFrm[, setdiff(varNme, "fleInd")]), ]
    }

    # sort data frame (if varSrt not empty)
    dtaFrm <- srtFrm(dtaFrm, varSrt)

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, sfxTtl = "_mrg_rows", ...)
}

addCol <- function(dtaFrm = NULL, varNme = c(), varTyp = c()) {
    varDff <- setdiff(varNme, names(dtaFrm))
    for (i in seq_along(varDff)) {
        eval(parse(text = paste0("dtaFrm[varDff[i]] <- as.", varTyp[[varDff[i]]], "(NA)")))
    }
    dtaFrm
}

addIdx <- function(dtaFrm = NULL, fleNme = "") {
    if (nzchar(fleNme)) {
        cbind(list(fleInd = rep(gsub(paste0("\\.", tools::file_ext(fleNme)), "", basename(fleNme)), dim(dtaFrm)[1])), dtaFrm)
    } else {
        dtaFrm
    }
}
