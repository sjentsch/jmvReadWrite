#' Transpose .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of
#'               any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param varNme Name of the variables in the output data frame; see Details below
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
#'               base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) where the input data set is transposed
#'
#' @details
#' * If `varNme` empty, the row names of the input data set are used (preceded by "V_" if all row names are numbers); if `varNme` has length 1, then it is
#'   supposed to point to a variable in the input data frame; if `varNme` has the same length as the number of rows in the input data frame, then the values
#'   in `varNme` are assigned as column names to the output data frame.
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for reading and writing the data. By clicking
#'   on the respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take. The functions are:
#'   `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV
#'   which both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files), `read_sav` (needs the R-package `haven`) or `read.spss`
#'   (needs the R-package `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas` (`haven`) for SAS-data-files,
#'   and `read_xpt` (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you would like to use `haven`, you may need to install it using
#'   `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `transpose_omv` internally uses the following functions for reading and writing data files in different formats: [jmvReadWrite::read_omv()] and
#'   [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files, [readRDS()] for .rds-files,
#'   [haven::read_sav()] or [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for
#'   SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' tmpDF <- stats::setNames(as.data.frame(matrix(sample(6, 1200, replace = TRUE), nrow = 16)),
#'                          sprintf("sbj_%03d", seq(75)))
#' str(tmpDF)
#' # Data sets that were extracted, e.g., from PsychoPy, may look like this (trials as rows
#' # and participants as columns, one for each participant, manually assembled / copy-and-pasted).
#' # However, for analyses, one wants the data set transposed (units / participants as columns)...
#' nmeOut <- tempfile(fileext = ".omv")
#' jmvReadWrite::transpose_omv(dtaInp = tmpDF, fleOut = nmeOut)
#' dtaFrm <- jmvReadWrite::read_omv(nmeOut)
#' unlink(nmeOut)
#' str(dtaFrm)
#' # if no varNme-parameter is given, generic variable names are created (V_...)
#' jmvReadWrite::transpose_omv(dtaInp = tmpDF, fleOut = nmeOut, varNme = sprintf("Trl_%02d", seq(16)))
#' dtaFrm <- jmvReadWrite::read_omv(nmeOut)
#' unlink(nmeOut)
#' str(dtaFrm)
#' # alternatively, the character vector with the desired variable names (of the same length as
#' # the number of rows in tmpDF) may be given, "Trl" can easily be exchanged by the name of your
#' # questionnaire, experimental conditions, etc.
#' }
#'
#' @export transpose_omv
#'
transpose_omv <- function(dtaInp = NULL, fleOut = "", varNme = "", usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, rmvEmp = TRUE, usePkg = usePkg, selSet = selSet, ...)

    # create variable names for the output data frame: if varNme is empty (default), then the row names of the
    # original data frame are used (preceded by "V_" if they contain only numbers); if varNme has the length 1
    # then it is assumed that it points to
    if (is.character(varNme)) {
        # varNme is empty, use row names of the input data frame, if all names are numbers, precede them with "V_"
        if        (length(varNme) == 1 && !nzchar(varNme)) {
            varOut <- paste0(ifelse(all(grepl("^[0-9]*$", row.names(dtaFrm))), "V_", ""), row.names(dtaFrm))
        # varNme has length 1, use the content of respective variable as column names
        } else if (length(varNme) == 1 &&  nzchar(varNme)) {
            selVar <- (names(dtaFrm) %in% varNme)
            if (any(selVar)) {
               varOut <- dtaFrm[,  selVar]
               dtaFrm <- dtaFrm[, !selVar]
            } else {
               stop(sprintf("%s (varNme) not contained in the input data frame.", varNme))
            }
        # varNme has the same length as there are rows in the input data frame
        } else if (length(varNme) == dim(dtaFrm)[1]) {
            varOut <- varNme
        } else {
            stop("varNme must either be empty, have one element or as many elements as there are rows in the input data frame.")
        }
    } else {
        stop("varNme must be a character variable.")
    }

    # transpose data frame
    dtaFrm <- stats::setNames(as.data.frame(t(dtaFrm)), varOut)

    # transfer the former variable / column names into a variable called "ID", and reset
    # the row.names to numbers (those wouldn't be written to the output data set anyway)
    dtaFrm <- cbind(list(ID = as.character(row.names(dtaFrm))), dtaFrm)
    attr(dtaFrm[, "ID"], "jmv-id") <- TRUE
    row.names(dtaFrm) <- NULL

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, dtaTtl = jmvTtl("_xpsd"), ...)
}
