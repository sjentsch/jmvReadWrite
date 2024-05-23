#' Convert data files (CSV, R, other statistics packages) into .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param fleInp Name (including the path, if required) of the data file to be read ("FILENAME.ext"; default: ""); supports CSV and R-files natively, or other
#'               file types if "foreign" or "haven" are installed, see Details below
#' @param fleOut Name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, the extension of fleInp is
#'               replaced with ".omv"
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the row order of the input file is kept; default: c())
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with base
#'               R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return the function doesn't have a return value (it returns NULL)
#'
#' @details
#' * In difference to the remaining helper functions, `convert_to_omv` doesn't accept a data frame as input and it neither does return a data frame if `fleOut`
#'   is left empty: If you want to write a data frame, use `write_omv`. If you want to have a data frame returned use `read_omv` for jamovi-files or any of the
#'   functions listed in the bullet point below for any other file type.
#' * `varSrt` can be either a character or a character vector (with one or more variables respectively). The sorting order for a particular variable can be
#'   inverted with preceding the variable name with "-". Please note that this doesn't make sense and hence throws a warning for certain variable types (e.g.,
#'   factors).
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for reading and writing the data. By clicking
#'   on the respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take. The functions are:
#'   `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV
#'   which both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files), `read_sav` (needs the R-package `haven`) or `read.spss`
#'   (needs the R-package `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas` (`haven`) for SAS-data-files,
#'   and `read_xpt` (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you would like to use `haven`, you may need to install it using
#'   `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `convert_to_omv` internally uses the following functions for reading and writing data files in different formats: [jmvReadWrite::read_omv()] and
#'   [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files, [readRDS()] for .rds-files,
#'   [haven::read_sav()] or [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for
#'   SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' # Example 1: Convert from RDS
#' # (use ToothGrowth as example, save it as RDS)
#' nmeInp <- tempfile(fileext = ".rds")
#' nmeOut <- tempfile(fileext = ".omv")
#' saveRDS(jmvReadWrite::ToothGrowth, nmeInp)
#' jmvReadWrite::convert_to_omv(fleInp = nmeInp, fleOut = nmeOut)
#' cat(list.files(dirname(nmeOut), basename(nmeOut)))
#' # -> "file[...].omv" ([...] contains a random combination of numbers / characters
#' cat(file.info(nmeOut)$size)
#' # -> 2448 (size may differ on different OSes)
#' cat(str(jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)))
#' # gives a overview of the dataframe (all columns and some attributes,
#' # sveAtt is intentionally set to FALSE to make the output not too overwhelming)
#' unlink(nmeInp)
#' unlink(nmeOut)
#'
#' # Example 2: Convert from CSV
#' # (use ToothGrowth again as example, this time save it as CSV)
#' nmeInp <- tempfile(fileext = ".csv")
#' nmeOut <- tempfile(fileext = ".omv")
#' write.csv(jmvReadWrite::ToothGrowth, nmeInp)
#' jmvReadWrite::convert_to_omv(fleInp = nmeInp, fleOut = nmeOut)
#' cat(list.files(dirname(nmeOut), basename(nmeOut)))
#' cat(file.info(nmeOut)$size)
#' # -> 2104 (size may differ acc. to OS; the size is smaller than for the RDS-file
#' # because CSV can store fewer attributes, e.g., labels)
#' cat(str(jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)))
#' # gives a overview of the dataframe (all columns and some attributes)
#' unlink(nmeInp)
#' unlink(nmeOut)
#' }
#'
#' @export convert_to_omv
#'
convert_to_omv <- function(fleInp = "", fleOut = "", varSrt = c(), usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and format input and output files and handle / check further input arguments
    fleInp <- fmtFlI(fleInp, maxLng = 1)
    fleOut <- ifelse(nzchar(fleOut), fmtFlO(fleOut), fmtFlO(sub(paste0("\\.", tools::file_ext(fleInp)), ".omv", fleInp)))

    # read file and sort it (if varSrt is not empty)
    dtaFrm <- read_all(fleInp = fleInp, usePkg = usePkg, selSet = selSet, ...)
    dtaFrm <- srtFrm(dtaFrm, varSrt)

    # write file
    write_omv(dtaFrm = dtaFrm, fleOut = fleOut, ...)

    return(invisible(NULL))
}
