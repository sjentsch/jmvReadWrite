#' Search values in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @inheritParams aggregate_omv dtaInp fleOut usePkg selSet
#' @param rplLst A list where each entry is a vector (with length 2) containing the original value and the
#'               to-replace-value (default: list())
#' @param whlTrm Whether the search term (first entry in the vectors) must be found exactly (TRUE) or whether a partial
#'               match is sufficient (FALSE; default: TRUE)
#' @param varInc Names of variables (character vector) to be included in the replacement (default: NULL)
#' @param varExc Names of variables (character vector) to be excluded from the replacement (default: NULL)
#' @param incNum Whether to include continuous variables in the replacement (default: TRUE)
#' @param incOrd Whether to include ordinal variables in the replacement (default: TRUE)
#' @param incNom Whether to include nominal variables in the replacement (default: TRUE)
#' @param incID  Whether to include ID variables in the replacement (default: TRUE)
#' @param incCmp Whether to include Computed variables in the replacement (default: TRUE)
#' @param incRcd Whether to include Recoded variables in the replacement (default: TRUE)
#' @param psvAnl Whether analyses that are contained in the input file shall be transferred to the output file (TRUE /
#'               FALSE; default: FALSE)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) with the replaced values
#'
#' @details
#' * `rplLst` is a list. Each list entry contains a vector (with length 2), where the first entry is the original
#'   value, and the second entry is the value the original value is to be replaced with.
#' * `whlTrm` indicates whether partial matches of the original value(s) shall replaced (e.g., for original: 24 and
#'   replacement: 34, 241 will be changed into 341).
#' * `varInc` and `varExc` determine which variables are included or excluded from the replacement. If both are given,
#'   a warning is issued and `varInc` takes precedence. `varInc` makes that only in these variables, the replacement
#'   requested by `rplLst` is carried out, if `varExc` is given, for all variables of the input data set, except those
#'   defined in `varExc`, the replacement is carried out.
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for
#'   reading the data. By clicking on the respective function under “See also”, you can get a more detailed overview
#'   over which parameters each of those functions take.
#'
#' @seealso
#' `replace_omv` internally uses the following functions for reading and writing data files in different formats:
#' [jmvReadWrite::read_omv()] and [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV
#' files, [load()] for reading .RData-files, [readRDS()] for .rds-files, [haven::read_sav()] or [foreign::read.spss()]
#' for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for
#' SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' bfi_sample <- jmvReadWrite::bfi_sample
#' # the gender in the original data file is plural...
#' table(bfi_sample$gender)
#' # and shall be converted to singular
#' rplDF <- jmvReadWrite::replace_omv(dtaInp = bfi_sample,
#'            rplLst = list(c("Females", "Female"), c("Males", "Male")))
#' table(rplDF$gender)
#' # with giving an output file name, the data set is written
#' nmeOut <- tempfile(fileext = ".omv")
#' jmvReadWrite::replace_omv(bfi_sample, fleOut = nmeOut,
#'   rplLst = list(c("Females", "Female"), c("Males", "Male")))
#' file.exists(nmeOut)
#' rplDF <- jmvReadWrite::read_omv(nmeOut)
#' table(rplDF$gender)
#' unlink(nmeOut)
#' # it is sensible to check / search for the original values before running replace_omv
#' jmvReadWrite::search_omv(bfi_sample, 24, whlTrm = TRUE)
#' rplDF <- jmvReadWrite::replace_omv(bfi_sample, rplLst = list(c(24, NA)))
#' table(rplDF$age)
#' }
#'
#' @export replace_omv
#'
replace_omv <- function(dtaInp = NULL, fleOut = "", rplLst = list(), whlTrm = TRUE, varInc = NULL, varExc = NULL,
                        incNum = TRUE, incOrd = TRUE, incNom = TRUE, incID = TRUE, incCmp = TRUE, incRcd = TRUE,
                        psvAnl = FALSE, usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check the input parameter:
    if (length(rplLst) < 1 || !is.list(rplLst) || !all(vapply(rplLst, length, integer(1)) == 2)) {
        stop("Calling replace_omv requires the parameter rplLst (a list where each entry is a vector with length 2; see Details in help).")
    }

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, ...)
    dtaFrm <- jmvAtt(dtaFrm)

    srcNme <- chkInE(dtaFrm, varInc, varExc, incNum, incOrd, incNom, incID, incCmp, incRcd)
    for (i in seq_along(srcNme)) {
        for (j in seq_along(rplLst)) {
            dtaFrm <- rplVal(dtaFrm, srcNme[i], rplLst[[j]], whlTrm)
        }
    }

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, dtaTtl = jmvTtl("_rplc"), psvAnl = psvAnl, dtaInp = dtaInp, ...)
}

chkInE <- function(dtaFrm = NULL, varInc = NULL, varExc = NULL, incNum = TRUE, incOrd = TRUE, incNom = TRUE,
                   incID = TRUE, incCmp = TRUE, incRcd = TRUE) {

    if (!is.null(varInc) && length(varInc) > 0 && !is.null(varExc) && length(varExc) > 0)
        warning("Both varInc and varExc are given, varInc takes precedence.")
    if (!is.null(varInc) && length(varInc) > 0 && all(nzchar(varInc))) {
        if (!all(varInc %in% names(dtaFrm))) {
            stop("All variables in varInc must be contained in the original data set (",
                 paste(varInc[!(varInc %in% names(dtaFrm))], collapse = ", "), " are not).")
        }
        srcNme <- varInc
    } else if (!is.null(varExc) && length(varExc) > 0 && all(nzchar(varExc))) {
        if (!all(varExc %in% names(dtaFrm))) {
            stop("All variables in varExc must be contained in the original data set (",
                 paste(varExc[!(varExc %in% names(dtaFrm))], collapse = ", "), " are not).")
        }
        srcNme <- setdiff(names(dtaFrm), varExc)
    } else {
        srcNme <- names(dtaFrm)
    }

    incClT <- c("Data", rep("Computed", incCmp), rep("Recoded", incRcd))
    incMsT <- c(rep("ID", incID), rep("Nominal", incNom), rep("Ordinal", incOrd), rep("Continuous", incNum))
    srcNme <- srcNme[vapply(dtaFrm[srcNme], function(x) is.null(attr(x,  "columnType")) || any(attr(x,  "columnType") == incClT), logical(1))]
    srcNme <- srcNme[vapply(dtaFrm[srcNme], function(x) is.null(attr(x, "measureType")) || any(attr(x, "measureType") == incMsT), logical(1))]

    srcNme
}

rplVal <- function(dtaFrm = NULL, crrCll = "", crrRpl = NULL, whlTrm = TRUE) {
    if (is.factor(dtaFrm[[crrCll]])) {
        if (crrRpl[1] %in% levels(dtaFrm[[crrCll]])) levels(dtaFrm[[crrCll]]) <- gsub(crrRpl[1], crrRpl[2], levels(dtaFrm[[crrCll]]))
    } else if (is.numeric(dtaFrm[[crrCll]])) {
        srcSel <- srcClm(dtaFrm[[crrCll]], gsub("\\.", "\\\\.", as.character(crrRpl[1])), whlTrm)
        if (any(srcSel)) dtaFrm[srcSel, crrCll] <- dtaFrm[srcSel, crrCll] + diff(as.numeric(crrRpl))
    } else if (is.character(dtaFrm[[crrCll]])) {
        srcSel <- srcClm(dtaFrm[[crrCll]], gsub("\\.", "\\\\.", as.character(crrRpl[1])), whlTrm)
        if (any(srcSel)) dtaFrm[srcSel, crrCll] <- crrRpl[2]
    }
    # other variable types are already caught by jmvAtt() above

    dtaFrm
}
