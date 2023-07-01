#' Transpose .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name (including the path, if required) of a data file to be read ("FILENAME.ext"; default: NULL); files can be of any supported file type, see Details below
#' @param fleOut Name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, and a file name is given as dtaInp, it is appended with "_xpsd.omv"
#' @param nmeVar Name of the variables in the output data frame; see Details below
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @details
#' If nmeVar empty, the row names of the input data set are used (preceded by "V_" if all row names are numbers); if nmeVar has the length 1 then it is supposed to point to a variable in the input
#' data frame; if nmeVar has the same length as the number of rows in the input data frame, then the values in nmeVar are assigned as column names to the output data frame.
#' The ellipsis-parameter can be used to submit arguments / parameters to the functions that are used for reading the data. These are: `read_omv` (for jamovi-files), `read.table` (for CSV / TSV
#' files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV which both are based upon `read.table` but with adjusted defaults for the respective file types), `readRDS` (for
#' rds-files), `read_sav` (needs R-package "haven") or `read.spss` (needs R-package "foreign") for SPSS-files, `read_dta` ("haven") / `read.dta` ("foreign") for Stata-files, `read_sas` ("haven") for
#' SAS-data-files, and `read_xpt` ("haven") / `read.xport` ("foreign") for SAS-transport-files. If you would like to use "haven", it may be needed to install it manually
#' (i.e., `install.packages("haven", dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite)
#' set.seed(1)
#' tmpDF <- stats::setNames(as.data.frame(matrix(sample(6, 1200, replace = TRUE), nrow = 16)),
#'                          sprintf("sbj_%03d", seq(75)))
#' str(tmpDF)
#' # Data sets that were extracted, e.g., from PsychoPy, may look like this (trials as rows
#' # and participants as columns, one for each participant, manually assmebled / copy-and-pasted).
#' # However, for analyses, one wants the data set transposed (units / participants as columns)...
#' fleTmp <- paste0(tempfile(), ".omv")
#' transpose_omv(dtaInp = tmpDF, fleOut = fleTmp)
#' dtaFrm <- read_omv(fleTmp)
#' str(dtaFrm)
#' # if no nmeVar-parameter is given, generic variable names are created (V_...)
#' transpose_omv(dtaInp = tmpDF, fleOut = fleTmp, nmeVar = sprintf("Trl_%02d", seq(16)))
#' dtaFrm <- read_omv(fleTmp)
#' str(dtaFrm)
#' # alternatively, the character vector with the desired variable names (of the same length as
#' # the number of rows in tmpDF) may be given, "Trl" can easily be exchanged by the name of your
#' # questionnaire, experimental conditions, etc.
#' unlink(fleTmp)
#' }
#'
#' @export transpose_omv
#'
transpose_omv <- function(dtaInp = NULL, fleOut = "", nmeVar = "", usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and import input data set (either as data frame or from a file)
    dtaFrm <- inp2DF(dtaInp, fleOut, "_xpsd.omv", usePkg, selSet, list(...))
    fleOut <- attr(dtaFrm, "fleOut")

    # create variable names for the output data frame: if nmeVar is empty (default), then the row names of the
    # original data frame are used (preceded by "V_" if they contain only numbers); if nmeVar has the length 1
    # then it is assumed that it points to
    if (is.character(nmeVar)) {
        # nmeVar is empty, use row names of the input data frame, if all names are numbers, precede them with "V_"
        if        (length(nmeVar) == 1 && !nzchar(nmeVar)) {
            varOut <- paste0(ifelse(all(grepl("^[0-9]*$", row.names(dtaFrm))), "V_", ""), row.names(dtaFrm))
        # nmeVar has length 1, use the content of respective variable as column names
        } else if (length(nmeVar) == 1 &&  nzchar(nmeVar)) {
            selVar <- (names(dtaFrm) %in% nmeVar)
            if (any(selVar)) {
               varOut <- dtaFrm[,  selVar]
               dtaFrm <- dtaFrm[, !selVar]
            } else {
               stop(sprintf("%s (nmeVar) not contained in the input data frame.", nmeVar))
            }
        # nmeVar has the same length as there are rows in the input data frame
        } else if (length(nmeVar) == dim(dtaFrm)[1]) {
            varOut <- nmeVar
        } else {
            stop("nmeVar must either be empty, have one element or as many elements as there are rows in the input data frame.")
        }
    } else {
        stop("nmeVar must be a character variable.")
    }

    # transpose data frame
    dtaFrm <- stats::setNames(as.data.frame(t(dtaFrm)), varOut)

    # transfer the former variable / column names into a variable called "ID", and reset
    # the row.names to numbers (those wouldn't be written to the output data set anyway)
    dtaFrm <- cbind(list(ID = as.character(row.names(dtaFrm))), dtaFrm)
    attr(dtaFrm[, "ID"], "jmv-id") <- TRUE
    row.names(dtaFrm) <- NULL

    # write file
    write_omv(dtaFrm, fleOut)
}
