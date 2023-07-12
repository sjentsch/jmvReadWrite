#' Re-arrange columns / variables in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is returned instead
#' @param varOrd Character vector with the desired order of variable(s) in the data frame (see Details; default: c())
#' @param varMve Named list defining to how much a particular variable (name of a list entry) should be moved up (neg. value of a list entry) or down (pos. value) in the data frame (see Details; default: c())
#' @param psvAnl Whether analyses that are contained in the input file shall be transferred to the output file (TRUE / FALSE; default: FALSE)
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if fleOut is empty) where the variables / columns of the input data set are re-arranged
#'
#' @details
#' varOrd is a character vector. If not all variables of the original data set are contained in varOrd, a warning is issued but otherwise the list of variables defined in varOrd is used (removing
#' variables not contained in varOrd).
#' varMve is a named list. For example would list(VARNAME = -3) move the variable VARNAME three positions up in the list of variables (towards the first column), and list(VARNAME = 3) would move
#' it three positions down (towards the last column). If how much the variable is to be moved leads to the position being lower than the first or higher than the total number of variables in the
#' data set, an error message is issued. Please note that the list entries are processed one after another, that is, you have for a second list entry to consider how the first list entry may have
#' changed to order of variables.
#' Generally, using varOrd makes more sense if several variables shall change their position whereas using varMve makes more sense for one variable. If both parameters are given, a warning is
#' issued and varOrd takes precedence.
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
#' # the original file has the variables in the order: "Adverts", "Airplay", "Image", "Sales"
#' names(read_omv(fleOMV))
#' # first, we move the variable "Sales" to the first place using the varOrd-parameter
#' arrange_cols_omv(dtaInp = fleOMV, fleOut = fleTmp,
#'   varOrd = c("Sales", "Adverts", "Airplay", "Image"))
#' names(read_omv(fleTmp))
#' # now, we move the variable "Sales" to the first place using the varMve-parameter
#' arrange_cols_omv(dtaInp = fleOMV, fleOut = fleTmp, varMve = list(Sales = -3))
#' names(read_omv(fleTmp))
#' unlink(fleTmp)
#' }
#'
#' @export arrange_cols_omv
#'
arrange_cols_omv <- function(dtaInp = "", fleOut = "", varOrd = c(), varMve = list(), psvAnl = FALSE, usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check the input parameters: either varOrd or varMve need to be given; if varOrd is given, the given character vectore and all of its elements need to be not empty;
    # if varMve is given, it needs to be a list, the names can't be empty, and the values need to be integers and can't be zero
    if ((length(varOrd) < 1 || !is.character(varOrd) || !all(nzchar(varOrd))) &&
        (length(varMve) < 1 || !is.list(varMve) || !is.character(names(varMve)) || !is.numeric(unlist(varMve)) || !all(unlist(varMve) != 0) || !all(unlist(varMve) %% 1 == 0))) {
        stop("Calling arrange_cols_omv requires either the parameter varOrd (a character vector) or the parameter varMve (a named list), using the correct format (see Details in help).")
    }

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, fleOut = fleOut, usePkg = usePkg, selSet = selSet, ...)
    fleOut <- attr(dtaFrm, "fleOut")

    # re-arrange the order of variables in the data set (varOrd)
    if (length(varOrd) > 0) {
        # [1] check whether all variables in varOrd are not empty and exist in the data set
        chkVar(dtaFrm, varOrd)
        # [2] check whether any variable of the original data set is not contained in varOrd, if so issue a warning but proceed
        if (!all(names(dtaFrm) %in% varOrd)) {
            warning(sprintf("The following variable(s) from the original data set are not contained in varOrd: %s", paste(setdiff(names(dtaFrm), varOrd), collapse = ", ")))
        }
    }
    if (length(varMve) > 0 && length(varOrd) == 0) {
        # [1] assign the original order of variables to varOrd
        varOrd <- names(dtaFrm)
        # [2] check whether all variables in varMve are not empty and exist in the data set
        chkVar(dtaFrm, names(varMve))
        for (crrVar in names(varMve)) {
            crrPos <- which(varOrd == crrVar)
            if (crrPos + varMve[[crrVar]] < 1 || crrPos + varMve[[crrVar]] > dim(dtaFrm)[2]) {
                stop("The value given in varMve must be chosen so that the element isn't moved before the first or after the last column.")
            }
            allPos <- seq(dim(dtaFrm)[2])
            if (varMve[[crrVar]] < 0) {
                rplPos <- seq(crrPos + varMve[[crrVar]], crrPos)
                allPos[allPos %in% rplPos] <- c(crrPos, setdiff(rplPos, crrPos))
            } else {
                rplPos <- seq(crrPos, crrPos + varMve[[crrVar]])
                allPos[allPos %in% rplPos] <- c(setdiff(rplPos, crrPos), crrPos)
            }
            varOrd <- varOrd[allPos]
        }
    } else if (length(varMve) > 0 && length(varOrd) > 0) {
        warning("Both, varOrd and varMve given as input parameters. varOrd takes precedence.")
    }

    # re-arrange to order of variables, while storing and restoring the attributes attached to the whole data frame (column attributes are not affected)
    attMem <- attributes(dtaFrm)
    dtaFrm <- dtaFrm[, varOrd]
    dtaFrm <- setAtt(setdiff(names(attMem), c("names", "row.names", "class", "fltLst", "fleOut")), attMem, dtaFrm)

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
