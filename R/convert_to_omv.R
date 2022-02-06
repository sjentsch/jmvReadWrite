#' Convert data files (CSV, R, other statistics packages) into .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org)
#'
#' @param fleInp name (including the path, if required) of the data file to be read ("FILENAME.ext"; default: ""); supports CSV and R-files natively, or other file types if "haven" or "foreign" are installed
#' @param fleOut name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, the extension of fleInp is replaced with ".omv'
#' @param usePkg name of the package ("haven" or "foreign") that shall be used to read SPSS, Stata and SAS files; "haven" is the default (it is more comprehensive), but with problems you may try "foreign"
#' @param selSet name of the data set that is to be selected from the workspace (only applies when reading .Rdata-files)
#' @param ...
#'
#' @details
#' The ellipsis-parameter can be used to submit arguments / parameters to the functions that are actually used for reading the data. These are: read_omv (for jamovi-files), read.table (for CSV / TSV
#' files), readRDS (for rds-files), read_sav (needs R-package "haven") or read.spss (needs R-package "foreign") for SPSS-files,  read_dta ("haven") / read.dta ("foreign") for Stata-files, read_sas
#' ("haven") for SAS-data-files, and read_xpt ("haven") / read.xport ("foreign") for SAS-transport-files. For reading CSV / TSV files, "convert_to_omv" uses similar defaults as "read.csv" (CSV) and
#' "read.delim" (TSV) which both are based upon "read.table" but with setting reasonable defaults for the respective file types.
#' Please note that the R-packages "haven" and "foreign" are not marked as "Imports" (i.e., they are not installed by default). If you wish to convert files from SPSS, SAS or Stata and haven't installed
#' them yet, please install them manually (e.g., `install.packages("haven", dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' }
#'
#' @export convert_to_omv
#'
convert_to_omv <- function(fleInp = "", fleOut = "", usePkg = c("haven", "foreign"), selSet = "", ...) {

    # normalize the path of the input file and then check whether the file exists and whether it is of a supported file type
    # ("omv" / "jamovi, " are excluded since it makes little sense to convert from jamovi to jamovi-files)
    # if fleOut is empty, fleInp is used with its file extension replaced with ".omv"
    if (length(fleInp) > 1) {
        stop("The fleInp-argument is not supposed to be a character vector with a length of more than one (if you would like to convert several files, call \"convert_to_omv\" individually).")
    }
    fleInp <- nrmFle(fleInp);
    chkFle(fleInp)
    chkExt(fleInp, setdiff(vldExt, "omv"));
    fleOut <- ifelse(nzchar(fleOut), nrmFle(fleOut), sub(tools::file_ext(fleInp), "omv", fleInp));

    # handle / check further input arguments
    varArg <- list(...);
    usePkg <- match.arg(usePkg);

    # read file
    dtaFrm <- read_all(fleInp, usePkg, selSet, varArg);

    # write file
    write_omv(dtaFrm, fleOut)
}
