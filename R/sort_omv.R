#' Sort data (using one or more variables) in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is returned instead
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; default: c())
#' @param psvAnl Whether analyses that are contained in the input file shall be transferred to the output file (TRUE / FALSE; default: FALSE)
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if fleOut is empty) where the input data set is sorted (according to the variables in varSrt)
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
#' sort_omv(dtaInp = fleOMV, fleOut = fleTmp, varSrt = "Image")
#' dtaFrm <- read_omv(fleTmp)
#' cat(dtaFrm$Image)
#' # shows that the variable "Image" is sorted in ascending order
#' cat(is.unsorted(dtaFrm$Image))
#' # is.unsorted (which checks for whether the variable is NOT sorted) returns FALSE
#' sort_omv(dtaInp = fleOMV, fleOut = fleTmp, varSrt = "-Image")
#' # variables can also be sorted in descending order by preceding them with "-"
#' dtaFrm <- read_omv(fleTmp)
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
sort_omv <- function(dtaInp = NULL, fleOut = "", varSrt = c(), psvAnl = FALSE, usePkg = c("foreign", "haven"), selSet = "", ...) {
    if (length(varSrt) == 0 || !all(nzchar(varSrt))) {
        stop("Calling sort_omv requires giving at least one variable to sort after.")
    }

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, fleOut = fleOut, usePkg = usePkg, selSet = selSet, ...)
    fleOut <- attr(dtaFrm, "fleOut")

    # sort data set
    dtaFrm <- srtFrm(dtaFrm, varSrt)

    # write the resulting data frame to the output file or, if no output file
    # name was given, return the data frame
    if (!is.null(fleOut) && nzchar(fleOut)) {
        write_omv(dtaFrm, fleOut)
        # transfer analyses from input to output file
        if (psvAnl) {
            if (is.character(dtaInp)) {
                xfrAnl(dtaInp, fleOut)
            } else {
                warning("psvAnl is only possible if dtaInp is a file name (analyses are not stored in data frames, only in the jamovi files).")
            }
        }
        return(invisible(NULL))
    } else {
        if (psvAnl) warning("psvAnl is only possible if fleOut is a file name (analyses are not stored in data frames, only in the jamovi files).")
        dtaFrm
    }
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
