#' Convert data files (CSV, R, other statistics packages) into .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org)
#'
#' @param fleInp Name (including the path, if required) of the data file to be read ("FILENAME.ext"; default: ""); supports CSV and R-files natively, or other file types if "foreign" or "haven" are installed
#' @param fleOut Name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, the extension of fleInp is replaced with ".omv"
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the row order of the input file is kept; default: c())
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @details
#' The ellipsis-parameter (...) can be used to submit arguments / parameters to the functions that are used for reading the data. These are: `read_omv` (for jamovi-files), `read.table` (for
#' CSV / TSV files), `readRDS` (for rds-files), `read_sav` (needs R-package "haven") or `read.spss` (needs R-package "foreign") for SPSS-files,  read_dta ("haven") / read.dta ("foreign") for
#' Stata-files, `read_sas` ("haven") for SAS-data-files, and `read_xpt` ("haven") / `read.xport` ("foreign") for SAS-transport-files. For reading CSV / TSV files, similar defaults as `read.csv` (CSV)
#' and `read.delim` (TSV) are used; both are based upon `read.table` but with setting reasonable defaults for the respective file types. If you would like to use "haven", it may be needed to install
#' it manually (i.e., `install.packages("haven", dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#'
#' # Example 1: Convert from RDS
#' # (use ToothGrowth as example, save it as RDS)
#' nmeInp <- paste0(tempfile(), ".rds");
#' nmeOut <- paste0(tempfile(), ".omv");
#' saveRDS(jmvReadWrite::ToothGrowth, nmeInp);
#' convert_to_omv(fleInp = nmeInp, fleOut = nmeOut);
#' cat(list.files(dirname(nmeOut), basename(nmeOut)));
#' # -> "file[...].omv" ([...] contains a random combination of numbers / characters
#' cat(file.info(nmeOut)$size);
#' # -> 2199 (size may differ on different OSes)
#' cat(str(read_omv(nmeOut, sveAtt = FALSE)));
#' # gives a overview of the dataframe (all columns and some attributes,
#' # sveAtt is intentionally set to FALSE to make the output not too overwhelming)
#' unlink(nmeInp);
#' unlink(nmeOut);
#'
#' # Example 2: Convert from CSV
#' # (use ToothGrowth again as example, this time save it as CSV)
#' nmeInp <- paste0(tempfile(), ".csv");
#' nmeOut <- paste0(tempfile(), ".omv");
#' write.csv(jmvReadWrite::ToothGrowth, nmeInp);
#' convert_to_omv(fleInp = nmeInp, fleOut = nmeOut);
#' cat(list.files(dirname(nmeOut), basename(nmeOut)));
#' cat(file.info(nmeOut)$size);
#' # -> 2107 (size may differ acc. to OS; the size is smaller than for the RDS-file
#' # because CSV can store fewer attributes, e.g., labels)
#' cat(str(read_omv(nmeOut, sveAtt = FALSE)));
#' # gives a overview of the dataframe (all columns and some attributes)
#' unlink(nmeInp);
#' unlink(nmeOut);
#' }
#'
#' @export convert_to_omv
#'
convert_to_omv <- function(fleInp = "", fleOut = "", varSrt = c(), usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and format input and output files and handle / check further input arguments
    fleInp <- fmtFlI(fleInp, maxLng = 1);
    fleOut <- fmtFlO(fleOut, fleInp, rplExt = ".omv");
    varArg <- list(...);
    usePkg <- match.arg(usePkg);

    # read file and sort it (if varSrt is not empty)
    dtaFrm <- read_all(fleInp, usePkg, selSet, varArg)
    dtaFrm <- srtFrm(dtaFrm, varSrt);

    # write file
    write_omv(dtaFrm, fleOut)
}
