#' Label columns / variables in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of
#'               any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param varLbl Variable (default: NULL) containing either a character (a file name; the file must contain two columns one with variable names, the other with
#'               the labels), a data frame (one column the variable names, the other the labels), or a character vector (with the same length as the data set,
#'               containing the variable labels). See Details for more information.
#' @param psvAnl Whether analyses that are contained in the input file shall be transferred to the output file (default: FALSE)
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
#'               base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) where the order of variables / columns of the input data set is re-arranged
#'
#' @details
#' * `varLbl` can be either (1) a character with a file name to read (the file must contain to columns, one with the variable names, the other with the
#'   variable labels); (2) a data frame with two columns (one with the variable names, the other with the variable labels), or (3) a character vector
#'   containing the variable labels (with a length equal to the number of variables in the input data set).
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for reading and writing the data. By clicking
#'   on the respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take. The functions are:
#'   `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV
#'   which both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files), `read_sav` (needs the R-package `haven`) or `read.spss`
#'   (needs the R-package `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas` (`haven`) for SAS-data-files,
#'   and `read_xpt` (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you would like to use `haven`, you may need to install it using
#'   `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `label_vars_omv` internally uses the following functions for reading and writing data files in different formats: [jmvReadWrite::read_omv()] and
#'   [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files, [readRDS()] for .rds-files,
#'   [haven::read_sav()] or [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for
#'   SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' # use one of the data files included in the package, but only the first 28 columns
#' # (the latter columns contain data for testing calculations, etc.)
#' nmeInp <- system.file("extdata", "bfi_sample.omv", package = "jmvReadWrite")
#' dtaInp <- jmvReadWrite::read_omv(nmeInp)[1:28]
#' nmeOut <- tempfile(fileext = ".omv")
#' # in the original file, the variable labels – attr(*, "jmv-desc") - are empty
#' lapply(dtaInp, attr, "jmv-desc")
#' # the definition of the variable labels can be read from a file with two columns,
#' # the first containing the variable name, the second the variable labels
#' # you can easily create such a file in Excel and save it as CSV
#' # if your CSV contains column names (e.g., varNme and varLbl) in the first row are they ignored
#' lblFle <- system.file("extdata", "label_example.csv", package = "jmvReadWrite")
#' lblDtF <- utils::read.csv(lblFle, header = FALSE)
#' str(lblDtF)
#'
#' # there are three options to give the varLbl parameter:
#' # (1) as file name, ...
#' jmvReadWrite::label_vars_omv(dtaInp = dtaInp, fleOut = nmeOut, varLbl = lblFle)
#' lapply(jmvReadWrite::read_omv(nmeOut), attr, "jmv-desc")
#' unlink(nmeOut)
#'
#' # (2) as data frame (using lblDtF from above), or ...
#' jmvReadWrite::label_vars_omv(dtaInp = dtaInp, fleOut = nmeOut, varLbl = lblDtF)
#' lapply(jmvReadWrite::read_omv(nmeOut), attr, "jmv-desc")
#' unlink(nmeOut)
#'
#' # (3) as character vector (with the same length as there are columns in the input data set)
#' lblChr <- lblDtF[[2]]
#' head(lblChr)
#' jmvReadWrite::label_vars_omv(dtaInp = dtaInp, fleOut = nmeOut, varLbl = lblChr)
#' lapply(jmvReadWrite::read_omv(nmeOut), attr, "jmv-desc")
#' unlink(nmeOut)
#' }
#'
#' @export label_vars_omv
#'
label_vars_omv <- function(dtaInp = NULL, fleOut = "", varLbl = NULL, psvAnl = FALSE, usePkg = c("foreign", "haven"), selSet = "", ...) {
    # check the input parameter: varLbl needs to be given
    if (length(varLbl) < 1) {
        stop("Calling label_vars_omv requires the parameter varLbl, using the correct format (see Details in help).")
    }

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, usePkg = usePkg, selSet = selSet, ...)

    # if varLbl is a character (a file name) or a character vector (with labels), transform it into a data frame with two columns (variable names and labels)
    if (is.character(varLbl)) {
        if (length(varLbl) == 1 && file.exists(varLbl)) {
            varLbl <- utils::read.csv(varLbl, header = FALSE)
        } else if (length(varLbl) == length(dtaFrm)) {
            varLbl <- data.frame(varNme = names(dtaFrm), varLbl = varLbl)
        } else {
            stop(sprintf(paste0("If the parameter varLbl is a character, it eiter needs to be the name of an (exisiting) file or a vector with labels that",
                                "has the same length as the number of variables in the input data set (%d)."), length(dtaFrm)))
        }
    }

    # assign the labels to the data frame
    if (is.data.frame(varLbl) && length(varLbl) == 2) {
        # determine which column of the data frame contains the variable names
        nmeClm <- which(vapply(varLbl, function(c) all(c %in% names(dtaFrm)), logical(1), USE.NAMES = FALSE))
        # ensure that there is exactly one such column
        if (length(nmeClm) == 1) {
            # iterate through the variable of the input data set
            nmeDtF <- names(dtaFrm)
            for (n in seq_along(nmeDtF)) {
                # if the variable name is found, set the variable label
                selRow <- varLbl[[nmeClm]] %in% nmeDtF[n]
                if (any(selRow)) attr(dtaFrm[, n], "jmv-desc") <- varLbl[selRow, -nmeClm]
            }
        } else {
            stop(sprintf(paste0("There must be exactly one column with the variable names (currently: %d).\n",
                                "All variable names in the label definition must be contained in the input data set.\n%s\n%s"),
                                length(nmeClm), paste0(varLbl[[1]], collapse = ", "), paste0(names(dtaFrm), collapse = ", ")))
        }
    } else {
        stop(sprintf("varLbl was either not a data frame or could not be converted into one.\n%s", utils::capture.output(utils::str(varLbl))))
    }

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, dtaTtl = jmvTtl("_label_vars"), psvAnl = psvAnl, dtaInp = dtaInp, ...)
}
