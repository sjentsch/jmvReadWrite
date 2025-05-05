#' Combines pairs of columns from a raw data matrix in .omv-files for the statistical spreadsheet
#' 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path,
#'               if required; "FILENAME.ext"; default: NULL); files can be of any supported file
#'               type, see Details below.
#' @param fleOut Name of the data file to be written (including the path, if required;
#'               "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is returned
#'               instead.
#' @param varPrs Variable (default: list()) containing a list of lists where each nested list
#'               contains the names of pairs of variables to be combined.
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata,
#'               and SAS files; "foreign" is the default (it comes with base R), but "haven" is
#'               newer and more comprehensive.
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when
#'               reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below.
#'
#' @return a data frame containing the column pairs given in `varPrs` combined and the original
#'         columns removed
#'
#' @details
#' * 
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions
#'   that are used for reading and writing the data. By clicking on the respective function under
#'   “See also”, you can get a more detailed overview over which parameters each of those functions
#'   take. The functions are: `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV
#'   / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV which both
#'   are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files),
#'   `read_sav` (needs the R-package `haven`) or `read.spss` (needs the R-package `foreign`) for
#'   SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas`
#'   (`haven`) for SAS-data-files, and `read_xpt` (`haven`) / `read.xport` (`foreign`) for
#'   SAS-transport-files. If you would like to use `haven`, you may need to install it using
#'   `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `combine_cols_omv` uses the following functions for reading and writing data files in
#'   different formats: [jmvReadWrite::read_omv()] and [jmvReadWrite::write_omv()] for
#'   jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files,
#'   [readRDS()] for .rds-files, [haven::read_sav()] or [foreign::read.spss()] for SPSS-files,
#'   [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for
#'   SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export combine_cols_omv
#'
combine_cols_omv <- function(dtaInp = NULL, fleOut = "", varPrs = list(), usePkg = c("foreign", "haven"),
                             selSet = "", ...) {

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, usePkg = usePkg, selSet = selSet, ...)

    if (!all(nzchar(varDst)) || length(intersect(varDst, names(dtaFrm))) < 2) {
        stop("Calling distances_omv requires giving at least two (valid) variables to calculate distances between.")
    }


}

match_col <- function() {

}
