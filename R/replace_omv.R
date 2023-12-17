#' Search values in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a jamovi data file to be read (including the path, if required; "FILENAME.omv"; default: NULL)
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param rplLst A list where each entry is a vector (with length 2) containing the original value and the to-replace-value (default: list())
#' @param whlTrm Whether the search term (first entry in the vectors) must be found exactly (TRUE) or whether a partial match is sufficient (FALSE; default:
#'               TRUE)
#' @param varInc Names of variables (character vector) to be included in the replacement (default: c())
#' @param varExc Names of variables (character vector) to be excluded from the replacement (default: c())
#' @param incNum Whether to include continuous variables in the replacement (default: TRUE)
#' @param incOrd Whether to include ordinal variables in the replacement (default: TRUE)
#' @param incNom Whether to include nominal variables in the replacement (default: TRUE)
#' @param incID  Whether to include ID variables in the replacement (default: TRUE)
#' @param incCmp Whether to include Computed variables in the replacement (default: TRUE)
#' @param incRcd Whether to include Recoded variables in the replacement (default: TRUE)
#' @param psvAnl Whether analyses that are contained in the input file shall be transferred to the output file (default: FALSE)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) with the replaced values
#'
#' @details
#' * `rplLst` is a list. Each list entry contains a vector (with length 2), where the first entry is the original value, and the second entry is the value the
#'   original value is to be replaced with.
#' * `whlTrm` indicates whether partial matches of the original value(s) shall replaced (e.g., for original: 24 and replacement: 34, 241 will be changed into
#'   341).
#' * `varInc` and `varExc` determine which variables are included or excluded from the replacement. If both are given, a warning is issued and `varInc` takes
#'   precedence. `varInc` makes that only in these variables, the replacement requested by `rplLst` is carried out, if `varExc` is given, for all variables of
#'   the input data set, except those defined in `varExc`, the replacement is carried out.
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the function that is used for reading and writing the data. Clicking on the
#'   respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take. The functions are:
#'   `read_omv` and `write_omv` (for jamovi-files).
#'
#' @seealso `replace_omv` uses [jmvReadWrite::read_omv()] and [jmvReadWrite::write_omv()] for reading and writing jamovi-files.
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
replace_omv <- function(dtaInp = NULL, fleOut = "", rplLst = list(), whlTrm = TRUE, varInc = c(), varExc = c(), incNum = TRUE, incOrd = TRUE, incNom = TRUE,
                        incID = TRUE, incCmp = TRUE, incRcd = TRUE, psvAnl = FALSE, ...) {

    # check the input parameter:
    if (length(rplLst) < 1 || !is.list(rplLst) || !all(vapply(rplLst, length, integer(1)) == 2)) {
        stop("Calling replace_omv requires the parameter rplLst (a list where each entry is a vector with length 2; see Details in help).")
    }

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, ...)
    dtaFrm <- jmvAtt(dtaFrm)

    incClT <- c("Data", rep("Computed", incCmp), rep("Recoded", incRcd))
    incMsT <- c(rep("ID", incID), rep("Nominal", incNom), rep("Ordinal", incOrd), rep("Continuous", incNum))

    srcNme <- chkInE(varInc, varExc, dtaFrm)
    srcNme <- srcNme[vapply(dtaFrm[srcNme], function(x) is.null(attr(x,  "columnType")) || any(attr(x,  "columnType") == incClT), logical(1))]
    srcNme <- srcNme[vapply(dtaFrm[srcNme], function(x) is.null(attr(x, "measureType")) || any(attr(x, "measureType") == incMsT), logical(1))]
    for (i in seq_along(srcNme)) {
        for (j in seq_along(rplLst)) {
            if (is.factor(dtaFrm[[srcNme[i]]])) {
                if (rplLst[[j]][1] %in% levels(dtaFrm[[srcNme[i]]])) levels(dtaFrm[[srcNme[i]]]) <- gsub(rplLst[[j]][1], rplLst[[j]][2], levels(dtaFrm[[srcNme[i]]]))
            } else if (is.numeric(dtaFrm[[srcNme[i]]])) {
                srcSel <- srcClm(dtaFrm[[srcNme[i]]], gsub("\\.", "\\\\.", as.character(rplLst[[j]][1])), whlTrm)
                if (any(srcSel)) dtaFrm[srcSel, srcNme[i]] <- dtaFrm[srcSel, srcNme[i]] + diff(as.numeric(rplLst[[j]]))
            } else if (is.character(dtaFrm[[srcNme[i]]])) {
                srcSel <- srcClm(dtaFrm[[srcNme[i]]], gsub("\\.", "\\\\.", as.character(rplLst[[j]][1])), whlTrm)
                if (any(srcSel)) dtaFrm[srcSel, srcNme[i]] <- rplLst[[j]][2]
            }
            # other variable types are already caught by jmvAtt() above
        }
    }

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, dtaTtl = jmvTtl("_rplc"), psvAnl = psvAnl, dtaInp = dtaInp, ...)
}

chkInE <- function(varInc = c(), varExc = c(), dtaFrm = NULL) {
    if (!is.null(varInc) && length(varInc) > 0 && !is.null(varExc) && length(varExc) > 0) warning("Both varInc and varExc are given, varInc takes precedence.")
    if        (!is.null(varInc) && length(varInc) > 0 && all(nzchar(varInc))) {
        if (!all(varInc %in% names(dtaFrm))) {
            stop(sprintf("All variables in varInc must be contained in the original data set (%s are not).", paste(varInc[!(varInc %in% names(dtaFrm))], collapse = ", ")))
        }
        srcNme <- varInc
    } else if (!is.null(varExc) && length(varExc) > 0 && all(nzchar(varExc))) {
        if (!all(varExc %in% names(dtaFrm))) {
            stop(sprintf("All variables in varExc must be contained in the original data set (%s are not).", paste(varExc[!(varExc %in% names(dtaFrm))], collapse = ", ")))
        }
        srcNme <- setdiff(names(dtaFrm), varExc)
    } else {
        srcNme <- names(dtaFrm)
    }

    srcNme
}
