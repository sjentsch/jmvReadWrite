#' Search values in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a jamovi data file to be read (including the path, if required; "FILENAME.omv"; default: NULL)
#' @param srcTrm (Character or numeric) Vector (with length = 1) with a search term to be found in the data frame (default: c())
#' @param whlTrm Whether the exact search term shall be found (TRUE) or whether a partial match is sufficient (FALSE; default: FALSE)
#' @param incNum Whether to include continuous variables in the search (default: TRUE)
#' @param incOrd Whether to include ordinal variables in the search (default: TRUE)
#' @param incNom Whether to include nominal variables in the search (default: TRUE)
#' @param incID  Whether to include ID variables in the search (default: TRUE)
#' @param incCmp Whether to include Computed variables in the search (default: TRUE)
#' @param incRcd Whether to include Recoded variables in the search (default: TRUE)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a named list with the places where the search term was found (names in the list are the variables / columns, the entries the respective row
#'         numbers within that variable / column)
#'
#'
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite)
#' nmeInp <- system.file("extdata", "AlbumSales.omv", package = "jmvReadWrite")
#' }
#'
#' @export search_omv
#'
search_omv <- function(dtaInp = NULL, srcTrm = c(), whlTrm = FALSE, incNum = TRUE, incOrd = TRUE, incNom = TRUE, incID = TRUE, incCmp = TRUE, incRcd = TRUE, ...) {

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
    srcNme <- srcNme[sapply(dtaFrm[srcNme], function(x) is.null(attr(x,  "columnType")) || any(attr(x,  "columnType") == incClT))]
    srcNme <- srcNme[sapply(dtaFrm[srcNme], function(x) is.null(attr(x, "measureType")) || any(attr(x, "measureType") == incMsT))]
    srcRes <- stats::setNames(rep(list(NULL), length(srcNme)), srcNme)
    nmeRow <- row.names(dtaFrm)
    for (i in seq_along(srcRes)) {
        srcRes[[i]] <- nmeRow[srcClm(dtaFrm[[srcNme[i]]], srcTrm, whlTrm)]
    }
    return(srcRes[sapply(srcRes, length) > 0])
}

srcClm <- function(inpClm = NULL, srcTrm = "", whlTrm = FALSE) {
    if (!is.na(srcTrm)) {
        return(grepl(paste0(rep("^", whlTrm), srcTrm, rep("$", whlTrm)), as.character(inpClm)))
    } else if (is.na(srcTrm)) {
        return(is.na(inpClm))
    }
}
