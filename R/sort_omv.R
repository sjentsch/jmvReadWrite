#' Sort data (using one or more variables) in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of
#'               any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; default: c())
#' @param psvAnl Whether analyses that are contained in the input file shall be transferred to the output file (TRUE / FALSE; default: FALSE)
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
#'               base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) where the input data set is sorted (according to the variables in `varSrt`)
#'
#' @details
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
#' @seealso `sort_omv` internally uses the following functions for reading and writing data files in different formats: [jmvReadWrite::read_omv()] and
#'   [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files, [readRDS()] for .rds-files,
#'   [haven::read_sav()] or [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for
#'   SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite)
#' nmeInp <- system.file("extdata", "AlbumSales.omv", package = "jmvReadWrite")
#' nmeOut <- tempfile(fileext = ".omv")
#' sort_omv(dtaInp = nmeInp, fleOut = nmeOut, varSrt = "Image")
#' dtaFrm <- read_omv(nmeOut)
#' unlink(nmeOut)
#' cat(dtaFrm$Image)
#' # shows that the variable "Image" is sorted in ascending order
#' cat(is.unsorted(dtaFrm$Image))
#' # is.unsorted (which checks for whether the variable is NOT sorted) returns FALSE
#' sort_omv(dtaInp = nmeInp, fleOut = nmeOut, varSrt = "-Image")
#' # variables can also be sorted in descending order by preceding them with "-"
#' dtaFrm <- read_omv(nmeOut)
#' unlink(nmeOut)
#' cat(dtaFrm$Image)
#' # shows that the variable "Image" is now sorted in descending order
#' cat(is.unsorted(dtaFrm$Image))
#' # this first returns TRUE (the variable is not in ascending order, i.e., unsorted)
#' cat(is.unsorted(-dtaFrm$Image))
#' # if the sign of the variable is changed, it returns FALSE (i.e., the variable is
#' # NOT unsorted)
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
    dtaFrm <- inp2DF(dtaInp = dtaInp, usePkg = usePkg, selSet = selSet, ...)

    # sort data set
    dtaFrm <- srtFrm(dtaFrm, varSrt)

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, sfxTtl = "_sort", psvAnl = psvAnl, dtaInp = dtaInp, ...)
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
        dtaFrm <- dtaFrm[srtOrd, , drop = FALSE]
        rownames(dtaFrm) <- NULL
        for (n in names(attMem)[!sapply(attMem, is.null)]) attributes(dtaFrm[[n]]) <- attMem[[n]]
    }

    dtaFrm
}
