#' Sort data (using one or more variables) in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param fleInp Name (including the path, if required) of the data file to be read ("FILENAME.ext"; default: ""); can be any supported file type, see Details below
#' @param fleOut Name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, the extension of fleInp is replaced with "_sorted(file extension -> .omv)"
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; default: c())
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @details
#' varSrt can be either a character or a character vector (with one or more variables respectively). The sorting order for a particular variable can be inverted with preceding the variable name with
#' "-". Please note that this doesn't make sense and hence throws a warning for certain variable types (e.g., factors).
#' The ellipsis-parameter can be used to submit arguments / parameters to the functions that are used for reading the data. These are: `read_omv` (for jamovi-files), `read.table` (for CSV / TSV
#' files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV which both are based upon `read.table` but with adjusted defaults for the respective file types), `readRDS` (for
#' rds-files), `read_sav` (needs R-package "haven") or `read.spss` (needs R-package "foreign") for SPSS-files, `read_dta` ("haven") / `read.dta` ("foreign") for Stata-files, `read_sas` ("haven") for
#' SAS-data-files, and `read_xpt` ("haven") / `read.xport` ("foreign") for SAS-transport-files. If you would like to use "haven", it may be needed to install it manually
#' (i.e., `install.packages("haven", dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite)
#' fleOMV <- system.file("extdata", "AlbumSales.omv", package = "jmvReadWrite")
#' fleTmp <- paste0(tempfile(), ".omv")
#' sort_omv(fleInp = fleOMV, fleOut = fleTmp, varSrt = "Image")
#' dtaFrm <- read_omv(fleInp = fleTmp)
#' cat(dtaFrm$Image)
#' # shows that the variable "Image" is sorted in ascending order
#' cat(is.unsorted(dtaFrm$Image))
#' # is.unsorted (which checks for whether the variable is NOT sorted) returns FALSE
#' sort_omv(fleInp = fleOMV, fleOut = fleTmp, varSrt = "-Image")
#' # variables can also be sorted in descending order by preceding them with "-"
#' dtaFrm <- read_omv(fleInp = fleTmp)
#' cat(dtaFrm$Image)
#' # shows that the variable "Image" is now sorted in descending order
#' cat(is.unsorted(dtaFrm$Image))
#' # this first returns TRUE (the variable is not in ascending order, i.e., unsorted)
#' cat(is.unsorted(-dtaFrm$Image))
#' # if the sign of the variable is changed, it returns FALSE (i.e., the variable is
#' # NOT unsorted)
#' unlink(fleTmp)
#' }
#'
#' @export sort_omv
#'
sort_omv <- function(fleInp = c(), fleOut = "", varSrt = c(), usePkg = c("foreign", "haven"), selSet = "", ...) {
    if (length(varSrt) == 0 || !all(nzchar(varSrt))) {
        stop("Calling sort_omv requires giving at least one variable to sort after.")
    }

    # check and format input and output files, handle / check further input arguments
    fleInp <- fmtFlI(fleInp, maxLng = 1)
    fleOut <- fmtFlO(fleOut, fleInp, "_sort.omv")
    varArg <- list(...)
    usePkg <- match.arg(usePkg)

    # read file and sort it
    dtaFrm <- read_all(fleInp, usePkg, selSet, varArg)
    dtaFrm <- srtFrm(dtaFrm, varSrt)

    # write file
    write_omv(dtaFrm, fleOut)
}

srtFrm <- function(dtaFrm = NULL, varSrt = c()) {
    # if the sorting variable(s) are found, generate an order according to them and afterwards remove / reset the rownames
    if (chkVar(dtaFrm, gsub("^-", "", varSrt))) {
#       srtOrd <- eval(parse(text = paste0("order(", paste0(gsub("dtaFrm[[\"-", "-dtaFrm[[\"", paste0("dtaFrm[[\"", varSrt, "\"]]"), fixed = TRUE), collapse = ", "), ")")))
        srtOrd <- eval(parse(text = paste0("order(", paste0(sapply(varSrt, function(x) {
            s <- ifelse(grepl("^-", x), "-", "")
            ifelse(!any(is.na(suppressWarnings(as.numeric(dtaFrm[[x]])))),
                paste0(s, "as.numeric(dtaFrm[[\"", sub("^-", "", x), "\"]])"),
                paste0(s, "dtaFrm[[\"", sub("^-", "", x), "\"]]"))
            }), collapse = ", "), ")")))
        # sorting makes the data.frame lose it's attributes which are therefore stored and later restored
        attMem <- sapply(dtaFrm, attributes)
        dtaFrm <- dtaFrm[srtOrd, ]
        rownames(dtaFrm) <- NULL
        for (n in names(attMem)[!sapply(attMem, is.null)]) attributes(dtaFrm[[n]]) <- attMem[[n]]
    }

    dtaFrm
}
