#' Convert data files (CSV, R, other statistics packages) into .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org)
#'
#' @param fleInp Name (including the path, if required) of the data file to be read ('FILENAME.ext'; default: ''); supports CSV and R-files natively, or other file types if 'haven' or 'foreign' are installed
#' @param fleOut Name (including the path, if required) of the data file to be written ('FILENAME.omv'; default: ''); if empty, the extension of fleInp is replaced with '.omv'
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the row order of the input file is kept; default: c())
#' @param usePkg Name of the package: 'haven' or 'foreign' that shall be used to read SPSS, Stata and SAS files; 'haven' is the default (it is more comprehensive), but with problems you may try 'foreign'
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @details
#' The ellipsis-parameter (...) can be used to submit arguments / parameters to the functions that are used for reading the data. These are: 'read_omv' (for jamovi-files), 'read.table' (for
#' CSV / TSV files), 'readRDS' (for rds-files), 'read_sav' (needs R-package 'haven') or 'read.spss' (needs R-package 'foreign') for SPSS-files,  read_dta ('haven') / read.dta ('foreign') for
#' Stata-files, 'read_sas' ('haven') for SAS-data-files, and 'read_xpt' ('haven') / 'read.xport' ('foreign') for SAS-transport-files. For reading CSV / TSV files, similar defaults as 'read.csv' (CSV)
#' and 'read.delim' (TSV) are used; both are based upon 'read.table' but with setting reasonable defaults for the respective file types.
#' Please note that the R-packages 'haven' and 'foreign' are not marked as 'Imports' (i.e., they are not installed by default). If you wish to convert files from SPSS, SAS or Stata and haven't installed
#' them yet, please install them manually (e.g., `install.packages('haven', dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' fleRDS <- paste0(tempfile(), ".rds");
#' fleOMV <- paste0(tempfile(), ".omv");
#' saveRDS(jmvReadWrite::ToothGrowth, fleRDS);
#' convert_to_omv(fleInp = fleRDS, fleOut = fleOMV);
#' cat(list.files(dirname(fleOMV), basename(fleOMV)));
#' # -> "file[...].omv" ([...] contains a random combination of numbers / characters
#' cat(file.info(fleOMV)$size);
#' # -> 2199 (size may differ on different OSes)
#' cat(str(read_omv(fleOMV, sveAtt = FALSE)));
#' # gives a overview of the dataframe (all columns and some attributes,
#' # sveAtt is intentionally set to FALSE to make the output not too overwhelming)
#' unlink(fleOMV);
#' }
#'
#' @export convert_to_omv
#'
convert_to_omv <- function(fleInp = "", fleOut = "", varSrt = c(), usePkg = c("haven", "foreign"), selSet = "", ...) {

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
