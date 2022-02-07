#' Sort data (using one or more variables) in .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org)
#'
#' @param fleInp Name (including the path, if required) of the data file to be read ("FILENAME.ext"; default: ""); supports CSV and R-files natively, or other file types if "haven" or "foreign" are installed
#' @param fleOut Name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, the extension of fleInp is replaced with "_sorted.omv"
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; default: c())
#' @param usePkg Name of the package: "haven" or "foreign" that shall be used to read SPSS, Stata and SAS files; "haven" is the default (it is more comprehensive), but with problems you may try "foreign"
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .Rdata-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @details
#' The ellipsis-parameter (...) can be used to submit arguments / parameters to the functions that are actually used for reading the data. These are: "read_omv" (for jamovi-files), "read.table" (for
#' CSV / TSV files), "readRDS" (for rds-files), "read_sav" (needs R-package "haven") or "read.spss" (needs R-package "foreign") for SPSS-files, "read_dta" ("haven") / "read.dta" ("foreign") for
#' Stata-files, "read_sas" ("haven") for SAS-data-files, and "read_xpt" ("haven") / "read.xport" ("foreign") for SAS-transport-files. For reading CSV / TSV files, "convert_to_omv" uses similar
#' defaults as "read.csv" (CSV) and "read.delim" (TSV) which both are based upon "read.table" but with setting reasonable defaults for the respective file types.
#' Please note that the R-packages "haven" and "foreign" are not marked as "Imports" (i.e., they are not installed by default). If you wish to convert files from SPSS, SAS or Stata and haven't installed
#' them yet, please install them manually (e.g., `install.packages("haven", dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' }
#'
#' @export sort_omv
#'
sort_omv <- function(fleInp = c(), fleOut = "", varSrt = c(), usePkg = c("haven", "foreign"), selSet = "", ...) {
    if (length(varSrt) == 0 || !all(nzchar(varSrt))) {
        stop("Calling sort_omv requires giving at least one variable to sort after.");
    }

    # check and format input and output files, handle / check further input arguments
    fleInp <- fmtFlI(fleInp, maxLng = 1);
    fleOut <- fmtFlO(fleOut, fleInp, "_sort.omv");
    varArg <- list(...);
    usePkg <- match.arg(usePkg);

    # read file and sort it
    dtaFrm <- read_all(fleInp, usePkg, selSet, varArg)
    dtaFrm <- srtFrm(dtaFrm, varSrt);

    # write file
    write_omv(dtaFrm, fleOut)
}

srtFrm <- function(dtaFrm = NULL, varSrt = c()) {
    if (chkVar(dtaFrm, gsub("^-", "", varSrt))) {
        dtaFrm[eval(parse(text = paste0("order(", gsub("dtaFrm[[\"-", "-dtaFrm[[\"", paste0("dtaFrm[[\"", varSrt, "\"]]", collapse = ", "), fixed = TRUE), ")"))), ]
    } else {
        dtaFrm
    }    
}
