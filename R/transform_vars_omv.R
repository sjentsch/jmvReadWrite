#' Transform skewed variables (aiming at they conform to a normal distribution) in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of
#'               any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param varXfm Named list variable where the name indicates which transformation is to be carried out and where each list entry points to one or more
#'               variables to be transformed using this transformation. See Details for more information.
#' @param psvAnl Whether analyses that are contained in the input file shall be transferred to the output file (default: FALSE)
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
#'               base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) where the order of variables / columns of the input data set is re-arranged
#'
#' @details
#' * `varXfm` has to be a named list variable where the names can either indicate the type of transformation or the kind and degree of skewness that shall be
#'   corrected. For the type of transformation, the following names are valid: `posSqr`, `negSqr`, `posLog`, `negLog`, `posInv`, `negInv`; where the second
#'   part of the name indicates the transformation to be carried out: `...Sqr` - square root, `...Log` - logarithm to the basis 10, `...Inv` - inversion, i.e.,
#'   1 / original value), and where the first part of the name indicates whether the original value is used (`pos...`) or whether the original value is
#'   subtracted from the maximum value of that variable (`neg...`; a constant of 1 is added to the maximum value for `...Log` and `...Inv` transformations).
#'   For the degree and kind of skewness, the following names are valid: `mdrPos`, `strPos`, `svrPos`, `mdrNeg`, `strNeg`, `svrNeg` (degree: moderate, strong,
#'   severe; kind: positive or negative).
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for reading and writing the data. By clicking
#'   on the respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take. The functions are:
#'   `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV
#'   which both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files), `read_sav` (needs the R-package `haven`) or `read.spss`
#'   (needs the R-package `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas` (`haven`) for SAS-data-files,
#'   and `read_xpt` (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you would like to use `haven`, you may need to install it using
#'   `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `transform_vars_omv` internally uses the following functions for reading and writing data files in different formats: [jmvReadWrite::read_omv()] and
#'   [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files, [readRDS()] for .rds-files,
#'   [haven::read_sav()] or [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for
#'   SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' # generate skewed variables
#' set.seed(335)
#' dtaInp <- data.frame(MP = rnorm(1000) * 1e-1 + rexp(1000, 2) * (1 - 1e-1),
#'                      MN = rnorm(1000) * 1e-1 - rexp(1000, 2) * (1 - 1e-1),
#'                      SP = rnorm(1000) * 1e-2 + rexp(1000, 2) * (1 - 1e-2),
#'                      SN = rnorm(1000) * 1e-2 - rexp(1000, 2) * (1 - 1e-2),
#'                      EP = rnorm(1000) * 1e-4 + rexp(1000, 2) * (1 - 1e-4),
#'                      EN = rnorm(1000) * 1e-4 - rexp(1000, 2) * (1 - 1e-4))
#' jmv::descriptives(data = dtaInp, skew = TRUE, sw = TRUE)
#'
#' crrXfm <- list(posSqr = c("MP"), negSqr = c("MN"), posLog = c("MP", "SP"), negLog = c("SN"),
#'                posInv = c("MP", "SP", "EP"), negInv = c("EN"))
#' dtaOut <- jmvReadWrite::transform_vars_omv(dtaInp = dtaInp, varXfm = crrXfm)
#' jmv::descriptives(data = dtaOut, skew = TRUE, sw = TRUE)
#'
#' crrXfm <- list(mdrPos = c("MP"), mdrNeg = c("MN"), strPos = c("SP"), strNeg = c("SN"),
#'                svrPos = c("EP"), svrNeg = c("EN"))
#' dtaOut <- jmvReadWrite::transform_vars_omv(dtaInp = dtaInp, varXfm = crrXfm)
#' jmv::descriptives(data = dtaOut, skew = TRUE, sw = TRUE)
#'
#' }
#'
#' @export transform_vars_omv
#'
transform_vars_omv <- function(dtaInp = NULL, fleOut = "", varXfm = NULL, psvAnl = FALSE, usePkg = c("foreign", "haven"), selSet = "", ...) {
    # check the input parameter: varXfm needs to be given
    if (length(varXfm) < 1 || !is.list(varXfm)) {
        stop("Calling transform_vars_omv requires the parameter varXfm, using the correct format (see Details in help).")
    }
    names(varXfm) <- gsub("mdrPos", "posSqr", gsub("strPos", "posLog", gsub("svrPos", "posInv",
                     gsub("mdrNeg", "negSqr", gsub("strNeg", "negLog", gsub("svrNeg", "negInv", names(varXfm)))))))
    if (!all(names(varXfm) %in% c("posSqr", "negSqr", "posLog", "negLog", "posInv", "negInv"))) {
        stop("The parameter varXfm has an invalid entry (wrong name), please use the correct format (see Details in help).")
    }

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, usePkg = usePkg, selSet = selSet, ...)

    # check whether all variables in varXfm are also contained in the input data frame
    if (!all(unique(unlist(varXfm, use.names = FALSE)) %in% names(dtaInp))) {
        stop(sprintf("All columns / variables given in the parameter varXfm need to be contained in the input data frame (dtaInp), but variable(s) %s are missing.",
          paste(setdiff(unique(unlist(varXfm, use.names = FALSE)), names(dtaInp)), collapse = ", ")))
    }

    # TO-DO: replace the functionality underneath with compute_omv once implemented
    for (crrNme in names(varXfm)) {
        crrSfx <- toupper(substr(crrNme, 4, 6))
        cmdJmv <- ifelse(crrSfx == "INV", "1 / (RPL_VAR)", sprintf("%s(RPL_VAR)", gsub("SQR", "SQRT", gsub("LOG", "LOG10", crrSfx))))
        for (crrVar in varXfm[[crrNme]]) {
            if        (substr(crrNme, 1, 3) == "pos") {
                rplVar <- paste0("RPL_VAR", ifelse(substr(crrNme, 4, 6) != "Sqr" && min(dtaFrm[, crrVar]) < 1, " - VMIN(RPL_VAR) + 1",
                                            ifelse(min(dtaFrm[, crrVar]) < 0, " - VMIN(RPL_VAR)", "")))
            } else if (substr(crrNme, 1, 3) == "neg") {
                rplVar <- paste0("VMAX(RPL_VAR)", ifelse(substr(crrNme, 4, 6) != "Sqr", " + 1", ""), " - RPL_VAR")
            }
            tgtVar <- paste0(crrVar, "_", crrSfx)
            tgtAtt <- list(columnType = "Computed", formula = gsub("RPL_VAR", gsub("RPL_VAR", crrVar, rplVar), cmdJmv))
            dtaFrm[, tgtVar] <- eval(parse(text = gsub("SQRT", "sqrt", gsub("LOG10", "log10", gsub("RPL_VAR",
              gsub("RPL_VAR", paste0("dtaFrm[, \"", crrVar, "\"]"), gsub("VMIN", "min", gsub("VMAX", "max", rplVar))), cmdJmv)))))
            dtaFrm[tgtVar]   <- setAtt(names(tgtAtt), inpObj = tgtAtt, outObj = dtaFrm[tgtVar])
        }
    }

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, dtaTtl = jmvTtl("_transform_vars"), psvAnl = psvAnl, dtaInp = dtaInp, ...)
}
