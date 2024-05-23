#' Search values in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a jamovi data file to be read (including the path, if required; "FILENAME.omv"; default: NULL)
#' @param srcTrm (Character or numeric) Vector (with length = 1) with a search term to be found in the data frame (default: c())
#' @param whlTrm Whether the exact search term shall be found (TRUE) or whether a partial match is sufficient (FALSE; default: FALSE)
#' @param ignCse Whether to ignore the case of the search term (default: FALSE)
#' @param incNum Whether to include continuous variables in the search (default: TRUE)
#' @param incOrd Whether to include ordinal variables in the search (default: TRUE)
#' @param incNom Whether to include nominal variables in the search (default: TRUE)
#' @param incID  Whether to include ID variables in the search (default: TRUE)
#' @param incCmp Whether to include Computed variables in the search (default: TRUE)
#' @param incRcd Whether to include Recoded variables in the search (default: TRUE)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a named list with the places where the search term was found: names in the list are the variables / columns, the entries the respective row names
#'         within that variable / column (row names are used for being tolerant to filtered-out cases in jamovi, if a filter is used, row numbers would be
#'         incorrect)
#'
#' @details
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the function that is used for reading and writing the data. Clicking on the
#'   respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take. The functions are:
#'   `read_omv` and `write_omv` (for jamovi-files).
#'
#' @seealso `replace_omv` uses [jmvReadWrite::read_omv()] and [jmvReadWrite::write_omv()] for reading and writing jamovi-files.
#'
#' @examples
#' \dontrun{
#' # the exact value 24 appears 13 times in age
#' bfi_sample <- jmvReadWrite::bfi_sample
#' jmvReadWrite::search_omv(bfi_sample, 24, whlTrm = TRUE)
#' # taking the fifth entry from the search results
#' bfi_sample["61", "age"]
#' # with the following search, both Males and Females are found
#' # (the M of Males, wouldn't be matched if ignCse were FALSE and males is
#' #  only a partial match within Females, thus whlTrm must be set to FALSE)
#' jmvReadWrite::search_omv(bfi_sample, "males", whlTrm = FALSE, ignCse = TRUE)
#' # the first entry is a female, the first entry is a male
#' bfi_sample["1", "gender"] # Females
#' bfi_sample["6", "gender"] # Males
#' # using the search results assigned to a variable
#' srcRes <- jmvReadWrite::search_omv(bfi_sample, "males", whlTrm = FALSE, ignCse = TRUE)
#' bfi_sample[srcRes[[1]][1], names(srcRes[1])] # Females
#' bfi_sample[srcRes[[1]][6], names(srcRes[1])] # Males
#' }
#'
#' @export search_omv
#'
search_omv <- function(dtaInp = NULL, srcTrm = c(), whlTrm = FALSE, ignCse = FALSE, incNum = TRUE, incOrd = TRUE, incNom = TRUE, incID = TRUE, incCmp = TRUE, incRcd = TRUE, ...) {

    # check the input parameter: the search term needs to be a non-empty character vector with length 1
    if (!is.character(srcTrm) && is.vector(srcTrm)) srcTrm <- as.character(srcTrm)
    if (length(srcTrm) != 1 || !nzchar(srcTrm)) {
        stop("Calling search_omv requires the parameter srcTrm (a character vector with length 1).")
    }
    srcTrm <- gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", gsub("\\.", "\\\\.", srcTrm)))

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, ...)
    dtaFrm <- jmvAtt(dtaFrm)

    incClT <- c("Data", rep("Computed", incCmp), rep("Recoded", incRcd))
    incMsT <- c(rep("ID", incID), rep("Nominal", incNom), rep("Ordinal", incOrd), rep("Continuous", incNum))
    srcNme <- names(dtaFrm)
    srcNme <- srcNme[vapply(dtaFrm[srcNme], function(x) is.null(attr(x,  "columnType")) || any(attr(x,  "columnType") == incClT), logical(1))]
    srcNme <- srcNme[vapply(dtaFrm[srcNme], function(x) is.null(attr(x, "measureType")) || any(attr(x, "measureType") == incMsT), logical(1))]
    srcRes <- stats::setNames(rep(list(NULL), length(srcNme)), srcNme)
    nmeRow <- row.names(dtaFrm)
    for (i in seq_along(srcRes)) {
        srcRes[[i]] <- nmeRow[srcClm(dtaFrm[[srcNme[i]]], srcTrm, whlTrm, ignCse)]
    }
    return(srcRes[vapply(srcRes, length, integer(1)) > 0])
}

srcClm <- function(inpClm = NULL, srcTrm = "", whlTrm = FALSE, ignCse = FALSE) {
    if (!is.na(srcTrm)) {
        return(grepl(paste0(rep("^", whlTrm), srcTrm, rep("$", whlTrm)), as.character(inpClm), ignore.case = ignCse))
    } else if (is.na(srcTrm)) {
        return(is.na(inpClm))
    }
}
