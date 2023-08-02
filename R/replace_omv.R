#' Search values in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a jamovi data file to be read (including the path, if required; "FILENAME.omv"; default: NULL)
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param rplLst A list where each entry is a vector (with length 2) containing the original value and the to-replace-value (default: list())
#' @param whlTrm Whether the search term (first entry in the vectors) must be found exactly (TRUE) or whether a partial match is sufficient (FALSE; default:
#'               TRUE)
#' @param incNum Whether to include continuous variables in the search (default: TRUE)
#' @param incOrd Whether to include ordinal variables in the search (default: TRUE)
#' @param incNom Whether to include nominal variables in the search (default: TRUE)
#' @param incID  Whether to include ID variables in the search (default: TRUE)
#' @param incCmp Whether to include Computed variables in the search (default: TRUE)
#' @param incRcd Whether to include Recoded variables in the search (default: TRUE)
#' @param psvAnl Whether analyses that are contained in the input file shall be transferred to the output file (default: FALSE)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) with the replaced values
#'
#' @details
#' * `rplLst` is a list. Each list entry contains a vector (with length 2), where the first entry is the original value, and the second entry is the value the
#'   original value is to be replaced with.
#' * ``
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the function that is used for writing the data. Clicking on the respective
#'   function under “See also”, you can get a more detailed overview over which parameters each of those functions take. The functions are:
#'   `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV
#'   which both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files), `read_sav` (needs the R-package `haven`) or `read.spss`
#'   (needs the R-package `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas` (`haven`) for SAS-data-files,
#'   and `read_xpt` (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you would like to use `haven`, you may need to install it using
#'   `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `replace_omv` uses [jmvReadWrite::write_omv()] for writing jamovi-files.
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite)
#' rplLst = list(c(), c())
#' }
#'
#' @export replace_omv
#'
replace_omv <- function(dtaInp = NULL, fleOut = "", rplLst = list(), whlTrm = TRUE, incNum = TRUE, incOrd = TRUE, incNom = TRUE, incID = TRUE, incCmp = TRUE, incRcd = TRUE, psvAnl = FALSE, ...) {

    # check the input parameter:
    if (length(rplLst) < 1 || !is.list(rplLst) || !all(sapply(rplLst, length) == 2)) {
        stop("Calling replace_omv requires the parameter rplLSt (a list where each entry is a vector with length 2; see Details in help).")
    }

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, ...)
    dtaFrm <- jmvAtt(dtaFrm)

    incClT <- c("Data", rep("Computed", incCmp), rep("Recoded", incRcd))
    incMsT <- c(rep("ID", incID), rep("Nominal", incNom), rep("Ordinal", incOrd), rep("Continuous", incNum))
    srcNme <- names(dtaFrm)
    srcNme <- srcNme[sapply(dtaFrm[srcNme], function(x) is.null(attr(x,  "columnType")) || any(attr(x,  "columnType") == incClT))]
    srcNme <- srcNme[sapply(dtaFrm[srcNme], function(x) is.null(attr(x, "measureType")) || any(attr(x, "measureType") == incMsT))]
    for (i in seq_along(srcNme)) {
        typClm <- class(dtaFrm[[srcNme[i]]])
        for (j in seq_along(rplLst)) {
            if (is.factor(dtaFrm[[srcNme[i]]])) {
                if (rplLst[[j]][1] %in% levels(dtaFrm[[srcNme[i]]])) levels(dtaFrm[[srcNme[i]]]) <- gsub(rplLst[[j]][1], rplLst[[j]][2], levels(dtaFrm[[srcNme[i]]]))
            } else if (is.numeric(dtaFrm[[srcNme[i]]]) || is.character(dtaFrm[[srcNme[i]]])) {
                srcSel <- srcClm(dtaFrm[[srcNme[i]]], as.character(rplLst[[j]][1]), whlTrm)
                if (any(srcSel)) dtaFrm[srcSel, srcNme[i]] <- as(rplLst[[j]][2], typClm)
            }
            # other variable types are already caught by jmvAtt() above
        }
    }

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, sfxTtl = "_rplc", psvAnl = psvAnl, dtaInp = dtaInp, ...)
}
