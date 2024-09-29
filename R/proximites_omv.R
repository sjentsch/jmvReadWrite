#' Calculates proximities (returns symmetric matrix) from a raw data matrix in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of
#'               any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param varPxm Variable (default: c()) containing a character vector with the names of the variables for which proximities are to be calculated. See Details
#'               for more information.
#' @param nmePxm Variable (default: "euclidean") containing a character indicating which proximity measure is to be calculated calculated. See Details for more
#'               information.
#' @param clmPxm Whether the proximities shall be calculated between columns (TRUE) or rows (FALSE; default: TRUE). See Details for more information.
#' @param mtxSps Whether the symmetric matrix to be returned should be sparse (default: FALSE)
#' @param mtxTrL Whether the symmetric matrix to be returned should only contain the lower triangular (default: FALSE)
#' @param mtxDgn Whether the symmetric matrix to be returned should retain the values in the main diagonal (default: TRUE)
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
#'               base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame containing a symmetric matrix (only returned if `fleOut` is empty) where the order of variables / columns of the input data set is
#'         re-arranged
#'
#' @details
#' * `varPxm` must a character vector containing the variables to calculated proximities over. If `clmPxm` is set to TRUE, proximities are calculated between all
#'   possible variable pairs and over subjects / rows in the original data frame. If `clmPxm` is set to FALSE, proximities are calculated between participants
#'   and over all variables given in `varPxm`.
#' * `nmePxm` can be one of the following proximity measures: ``
#' * If `clmPxm` is set to `TRUE`, the symmetric matrix that is returned has the size V x V (V being the number of variables in varPxm; if `mtxSps` is set to
#'   `TRUE`, the size is V - 1 x V - 1, see below); if `clmPxm` is set to `FALSE`, the symmetric matrix that is returned has the size R x R (R being the number
#'   of rows in the original dataset; it is if `mtxSps` is set to `TRUE`, the size is R - 1 x R - 1, see below).
#' * If `mtxSps` is set, a sparse matrix is returned. Those matrices are similar to the format one often finds for correlation matrices. The values are only
#'   retained in the lower triangular, the columns range from the first to the variable that is second to the last in `varPxm` (or respectively, the columns
#'   contain the first to the second to the last row of the original dataset when `clmPxm` is set to `FALSE`), and the rows contain the second to the last
#'   variable in `varPxm` (or respectively, the rows contain the second to the last row of the original dataset when `clmPxm` is set to `FALSE`).
#' * By default, a full symmetric matrix is returned (i.e., a matrix that has no NAs in any cell). This behaviour can be changed with setting `mtxTrL` and
#'   `mtxDgn`: If `mtxTrL` is set to `TRUE`, the values from the upper triangular matrix are removed / replaced with NAs; if `mtxDgn` is set to `FALSE`, the
#'   values from the main diagonal are removed / replaced with NAs.
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for reading and writing the data. By clicking
#'   on the respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take. The functions are:
#'   `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV
#'   which both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files), `read_sav` (needs the R-package `haven`) or `read.spss`
#'   (needs the R-package `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas` (`haven`) for SAS-data-files,
#'   and `read_xpt` (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you would like to use `haven`, you may need to install it using
#'   `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `proximities_omv` internally uses the following functions for reading and writing data files in different formats: [jmvReadWrite::read_omv()] and
#'   [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files, [readRDS()] for .rds-files,
#'   [haven::read_sav()] or [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for
#'   SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export proximities_omv
#'
proximities_omv <- function(dtaInp = NULL, fleOut = "", varPxm = c(), nmePxm = c("pearson"), clmPxm = TRUE, mtxSps = FALSE, mtxTrL = FALSE, mtxDgn = TRUE,
                          usePkg = c("foreign", "haven"), selSet = "", ...) {
# TBA
}
