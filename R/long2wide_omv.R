#' Converts .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>) from long to wide format
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of
#'               any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param varTgt Names of one or more variables to be transformed / reshaped (other variables are excluded, if empty(c()) all variables except `varTme`,
#'               `varID` and `varExc` are included; default: c())
#' @param varExc Name of the variable(s) should be excluded from the transformation, typically this will be between-subject-variable(s) (default: c())
#' @param varID  Names of one or more variables that identify the same group / individual (default: c())
#' @param varTme Name of the variable(s) that differentiates multiple records from the same group / individual (default: c())
#' @param varSep Separator character when concatenating the fixed and time-varying part of the variable name ("VAR1_1", "VAR1_2"; default: "_")
#' @param varOrd How variables / columns are organized: for "times" (default) the steps of the time varying variable are adjacent, for "vars" the steps of
#'               the original columns in the long dataset
#' @param varAgg How multiple occurrences of particular combinations of time varying variables are aggregated: either "mean" (calculate the mean over
#'               occurrences), or "first" (take the first occurrence)
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the order returned from reshape is kept; default: c())
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
#'               base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) where the input data set is converted from long to wide format
#'
#' @details
#' * If `varTgt` is empty, it is tried to generate it using all variables in the data frame except those defined by `varID`, `varTme` and `varExc`. The
#'   variable(s) in `varID` need to be unique identifiers (in the original dataset), those in `varExc` don't have this requirement. It is generally recommended
#'   that the variable names in `varExc` and `varID` should not contain the variable separator (defined in `varSep`; default: "_").
#' * `varSrt` can be either a character or a character vector (with one or more variables respectively). The sorting order for a particular variable can be
#'   inverted with preceding the variable name with "-". Please note that this doesn't make sense and hence throws a warning for certain variable types (e.g.,
#'   factors).
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for transforming, reading or writing the data.
#'   By clicking on the respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take.
#' * The transformation from long to wide uses `reshape`. `varTgt` matches (~) `v.names` in `reshape`, `varID` ~ `idvar`, `varTme` ~ `timevar`, and `varSep` ~
#'   `sep`. The help for `reshape` is very explanatory, click on the link under “See also” to access it, particularly what is explained under “Details”.
#' * The functions for reading and writing the data are: `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar
#'   defaults as `read.csv` for CSV and `read.delim` for TSV which both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files),
#'   `read_sav` (needs R-package `haven`) or `read.spss` (needs R-package `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for
#'   Stata-files, `read_sas` (`haven`) for SAS-data-files, and `read_xpt` (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you would like to
#'   use `haven`, you may need to install it using `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `long2wide_omv` internally uses the following functions: The transformation from long to wide uses [stats::reshape()]. For reading and writing data
#'   files in different formats: [jmvReadWrite::read_omv()] and [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files,
#'   [load()] for reading .RData-files, [readRDS()] for .rds-files, [haven::read_sav()] or [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or
#'   [foreign::read.dta()] for Stata-files, [haven::read_sas()] for SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' # generate a test dataframe with 100 (imaginary) participants / units of
#' #  observation (ID), 8 measurement (measure) of one variable (X)
#' dtaInp <- data.frame(ID = rep(as.character(seq(1, 100)), each = 8),
#'                      measure = rep(seq(1, 8), times = 100),
#'                      X = runif(800, -10, 10))
#' cat(str(dtaInp))
#' # the output should look like this
#' # 'data.frame': 800 obs. of  3 variables:
#' #  $ ID     : chr  "1" "1" "1" "1" ...
#' #  $ measure: int  1 2 3 4 5 6 7 8 1 2 ...
#' #  $ X      : num  ...
#' # this data set is stored as (temporary) RDS-file and later processed by long2wide
#' nmeInp <- tempfile(fileext = ".rds")
#' nmeOut <- tempfile(fileext = ".omv")
#' saveRDS(dtaInp, nmeInp)
#' jmvReadWrite::long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varTgt = "X", varID = "ID",
#'   varTme = "measure")
#' # it is required to give at least the arguments dtaInp, varID and varTme
#' # check whether the file was created and its size
#' cat(list.files(dirname(nmeOut), basename(nmeOut)))
#' # -> "file[...].omv" ([...] contains a random combination of numbers / characters
#' cat(file.info(nmeOut)$size)
#' # -> 6851 (approximate size; size may differ in every run [in dependence of
#' #          how well the generated random data can be compressed])
#' cat(str(jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)))
#' # the data set is now transformed into wide (and each the measurements is now
#' # indicated as a suffix to X; X_1, X_2, ...)
#' # 'data.frame':	100 obs. of  9 variables:
#' #  $ ID : chr  "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" ...
#' #   ..- attr(*, "jmv-id")= logi TRUE
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X_1: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X_2: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X_3: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X_4: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X_5: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X_6: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X_7: num  ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X_8: num  ...
#' #   ..- attr(*, "missingValues")= list()
#'
#' unlink(nmeInp)
#' unlink(nmeOut)
#' }
#'
#' @export long2wide_omv
#'
long2wide_omv <- function(dtaInp = NULL, fleOut = "", varTgt = c(), varExc = c(), varID = "ID", varTme = c(), varSep = "_", varOrd = c("times", "vars"),
                          varAgg = c("mean", "first"), varSrt = c(), usePkg = c("foreign", "haven"), selSet = "", ...) {

    # handle / check further input arguments
    # check varID (can be several) and varTme (must be one), neither can be empty
    if (!all(nzchar(c(varID, varTme)))) {
        stop("Using the arguments varID and varTme is mandatory (i.e., they can\'t be empty).")
    }
    varOrd <- match.arg(varOrd)
    varAgg <- match.arg(varAgg)

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, usePkg = usePkg, selSet = selSet, ...)

    # transform data set
    # [a] check whether varID, varTme and varTgt are not empty and exist in the data set
    chkVar(dtaFrm, varID)
    chkVar(dtaFrm, varTme)
    chkVar(dtaFrm, varExc)
    if (!chkVar(dtaFrm, varTgt)) {
        varTgt <- setdiff(names(dtaFrm), c(varID, varTme, varExc))
    }
    # [b] store the original variable labels, the original time-varying / target variable,
    # and an empty vector for storing labels
    lstLbl <- list(orgLbl = sapply(dtaFrm, attr, "jmv-desc"), orgTgt = varTgt)

    # [c] there might be several occurrences for each combination of varID and varTme; aggregate them
    dtaFrm <- aggDta(dtaFrm = dtaFrm, varAgg = varAgg, varID = varID, varTme = varTme, varExc = varExc, varTgt = varTgt)

    # do the actual work, iterating through the different time-varying variables in varTme
    for (i in seq_along(varTme)) {
        # [d] call "reshape" with having the variable arguments limited to those valid when calling the function
        crrArg <- list(data = dtaFrm, direction = "wide", v.names = varTgt, idvar = c(varID, varTme[seq_along(varTme)[seq_along(varTme) > i]]), timevar = varTme[i], sep = varSep)
        dtaFrm <- do.call(stats::reshape, adjArg("reshape", crrArg, list(), c("data", "direction", "v.names", "idvar", "timevar")))
        # [1] varVry contains the variable names with varTgt / v.names as rows, and the steps of varTme[i] / timevar
        # as columns; [2] when generating an updated varTgt for the next step, this matrix is transformed to a vector
        # either not transposed (in this case, the original variables are adjacent), or transposed (in this case, the
        # different steps of the time-varying variable are adjacent); [3] afterwards the reshapeWide-attribute is removed
        varVry <- attr(dtaFrm, "reshapeWide")$varying
        varTgt <- eval(parse(text = paste0("as.vector(", ifelse(varOrd == "times", "t(varVry)", "varVry"), ")")))
        attr(dtaFrm, "reshapeWide") <- NULL
    }
    if (varOrd == "times") dtaFrm <- dtaFrm[, c(setdiff(names(dtaFrm), as.vector(varVry)), varTgt)]

    # select all variable(s) except those defined by varID and varExc and remove the prefix "measure", if present
#   selVrN <- !grepl(paste0(c(varID, varExc), collapse = "|"), names(dtaFrm))
#   if (all(grepl(paste0("^measure", varSep), names(dtaFrm)[selVrN]))) names(dtaFrm)[selVrN] <- gsub(paste0("^measure", varSep), "", names(dtaFrm)[selVrN])

    # restore the original labels
    dtaFrm <- rstLbl(dtaFrm, lstLbl, varTgt, varTme, varSep)

    # sort data set (if varSrt is not empty)
    dtaFrm <- srtFrm(dtaFrm, varSrt)

    # if varID is unique, set it's measureType to ID
    if (!any(duplicated(dtaFrm[[varID]]))) attr(dtaFrm[[varID]], "jmv-id") <- TRUE

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, dtaTtl = jmvTtl("_wide"), ...)
}

aggDta <- function(dtaFrm = NULL, varAgg = "", varID = c(), varTme = c(), varExc = c(), varTgt = c()) {
    # if there exists only one occurence of each possible combination of the variables in varID and
    # varTme, the data don't need to be aggregated, just return the data frame with the relevant
    # columns selected
    if (!any(stats::aggregate(dtaFrm[, varTgt[1]], by = dtaFrm[, c(varID, varTme)], FUN = length)[["x"]] > 1)) {
        dtaFrm[, c(varID, varTme, varExc, varTgt)]
    # otherwise (with more than one occurence), values are aggregate at each possible combination of the
    # variables in varID and varTme
    } else if (varAgg == "first") {
        # [1] if "first" is chosen as aggregation function, the first occurence at each step is returned
        stats::aggregate(x = dtaFrm[, c(varTgt, varExc), drop = FALSE], by = dtaFrm[, c(varID, varTme), drop = FALSE], FUN = "[[", 1)
    } else if (varAgg == "mean")  {
        # [2] if "mean" is chosen as aggregation function, it becomes (a little) more complicated
        # [a] the target variables (for which the mean is calculated) should be numeric
        if (!all(sapply(dtaFrm[, varTgt], is.numeric))) {
            stop(paste("In order to calculate the mean when aggregating the data, all target variables (varTgt) need to be numeric. Use varAgg = \"first\" instead",
                       "(to use the first occuring value) or convert the target variables to numeric."))
        }
        # [b] if there are both target and “excluded” variables, the mean is calculated for the target
        # variables at each possible combination of the variables varID and varTme (first aggregate
        # within merge); afterwards, for the “excluded” variables (i.e., variables not to be
        # transformed to wide, the first occurrence is chosen (second aggregate; variables in
        # varExc, e.g. sex, should be the same for each step of the ID variables, e.g., each
        # participant [ID]); finally the results from the two aggregate-functions are merged again
        # to return the complete data set
        if (length(varExc) > 0) {
            merge(stats::aggregate(x = dtaFrm[, c(varTgt), drop = FALSE], by = dtaFrm[, c(varID, varTme), drop = FALSE], FUN = mean),
                  stats::aggregate(x = dtaFrm[, c(varExc), drop = FALSE], by = dtaFrm[, c(varID, varTme), drop = FALSE], FUN = "[[", 1))
        # [c] if there is no “excluded” variable, the mean is calculated for the target variables
        # at each possible combination of the variables varID and varTme
        } else {
            stats::aggregate(x = dtaFrm[, c(varTgt), drop = FALSE], by = dtaFrm[, c(varID, varTme), drop = FALSE], FUN = mean)
        }
    }
}

rstLbl <- function(dtaFrm = NULL, lstLbl = list(), varTgt = c(), varTme = c(), varSep = "_") {
    for (crrNme in names(lstLbl$orgLbl)) {
        if (crrNme %in% names(dtaFrm)) {
            attr(dtaFrm[[crrNme]], "jmv-desc") <- lstLbl$orgLbl[[crrNme]]
        } else if (crrNme %in% lstLbl$orgTgt) {
            splTgt <- strsplit(varTgt, varSep)
            for (i in seq_along(splTgt)) {
                if (crrNme %in% splTgt[[i]]) {
                    attr(dtaFrm[[varTgt[i]]], "jmv-desc") <-
                      sprintf("%s (%s)", lstLbl$orgLbl[[crrNme]], paste0(apply(rbind(varTme, splTgt[[i]][-1]), 2, paste0, collapse = ": "), collapse = ", "))
                }
            }
        }
    }

    dtaFrm
}
