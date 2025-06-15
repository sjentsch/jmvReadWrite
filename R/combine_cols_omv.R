#' Combines pairs of columns from a raw data matrix in .omv-files for the statistical spreadsheet
#' 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path,
#'               if required; "FILENAME.ext"; default: NULL); files can be of any supported file
#'               type, see Details below.
#' @param fleOut Name of the data file to be written (including the path, if required;
#'               "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is returned
#'               instead.
#' @param varPrs Definition of variable pairs; a list containing either list(s) or character
#'               vector(s) with the names of pairs of variables to be combined (default: list()).
#' @param mdeCmb Mode of combining the variables when conflicting values occur, either "none",
#'               "first", or "second" (default: "none"), see Details below.
#' @param psvAnl Whether analyses that are contained in the input file shall be transferred to the
#'               output file (TRUE / FALSE; default: FALSE)
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata,
#'               and SAS files; "foreign" is the default (it comes with base R), but "haven" is
#'               newer and more comprehensive.
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when
#'               reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below.
#'
#' @return a data frame containing the column pairs given in `varPrs` combined and the original
#'         columns removed
#'
#' @details
#' * The need to combine two columns into one is quite common after merging columns or rows (using,
#'   e.g., merge_cols_omv or merge_rows_omv). `varPrs` defines the variable pairs to be combined.
#'   It is a list containing the pairs of variables to be combined, either as list or as character
#'   vector; e.g., list(c("A", "B"), c("C", "D")) or list(list("A", "B"), list("C", "D")). `mdeCmb`
#'   defines what to to if values in the first and the second variable of a variable pair contain
#'   conflicting / different values: "none" does not merge the variables (and instead throws an
#'   error), "first" makes that the values from the first variable of each pair are taken if the
#'   values are conflicting, and "second" use the values from the second variable of each pair in
#'   case of conflicts.
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions
#'   that are used for reading and writing the data. By clicking on the respective function under
#'   “See also”, you can get a more detailed overview over which parameters each of those functions
#'   take. The functions are: `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV
#'   / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV which both
#'   are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files),
#'   `read_sav` (needs the R-package `haven`) or `read.spss` (needs the R-package `foreign`) for
#'   SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas`
#'   (`haven`) for SAS-data-files, and `read_xpt` (`haven`) / `read.xport` (`foreign`) for
#'   SAS-transport-files. If you would like to use `haven`, you may need to install it using
#'   `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `combine_cols_omv` uses the following functions for reading and writing data files in
#'   different formats: [jmvReadWrite::read_omv()] and [jmvReadWrite::write_omv()] for
#'   jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files,
#'   [readRDS()] for .rds-files, [haven::read_sav()] or [foreign::read.spss()] for SPSS-files,
#'   [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for
#'   SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' dtaInp <- jmvReadWrite::bfi_sample2
#' # create a new column (A1_1) containing a subset of the values in the original variable
#' # whereas those lines are replaced with NAs
#' set.seed(1)
#' selRow <- rnorm(nrow(dtaInp)) < 0
#' dtaInp[selRow,  "A1_1"] <- dtaInp[selRow, "A1"]
#' dtaInp[selRow,  "A1"]   <- NA
#' head(dtaInp[, c("A1", "A1_1")])
#' dtaOut <- combine_cols_omv(dtaInp, varPrs = list(c("A1", "A1_1")))
#' # show the differences before and after combining the values in the columns and ensure
#' # that all values are the same as in the original data set
#' dtaInp[, "A1"]
#' dtaOut[, "A1"]
#' all(dtaOut[, "A1"] == jmvReadWrite::bfi_sample2[, "A1"])
#'
#' # create a new column, containing values that are different from the original variable
#' dtaInp <- jmvReadWrite::bfi_sample2
#' dtaInp[selRow,  "A1_1"] <- dtaInp[selRow,  "A1"] + 1
#' # [1] if mdeCmb is "none" (or if mdeCmb is not given - "none" is the default) an error would be
#' # thrown (therefore the next line is commented out)
#' # dtaOut <- combine_cols_omv(dtaInp, varPrs = list(c("A1", "A1_1")), mdeCmb = "none")
#' # [2] if mdeCmb is "first", missing values are replaced and values from the first column ("A1")
#' # take precedence if the values are unequal
#' dtaOut <- combine_cols_omv(dtaInp, varPrs = list(c("A1", "A1_1")), mdeCmb = "first")
#' head(cbind(dtaOut[, "A1"], dtaInp[, c("A1", "A1_1")]))
#' # [3] if mdeCmb is "second", missing values are replaced and values from the second column
#' # ("A1_1") take precedence if the values are unequal
#' dtaOut <- combine_cols_omv(dtaInp, varPrs = list(c("A1", "A1_1")), mdeCmb = "second")
#' head(cbind(dtaOut[, "A1"], dtaInp[, c("A1", "A1_1")]))
#' }
#'
#' @export combine_cols_omv
#'
combine_cols_omv <- function(dtaInp = NULL, fleOut = "", varPrs = list(), mdeCmb = c("none", "first", "second"),
                             psvAnl = FALSE, usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    mdeCmb <- match.arg(mdeCmb)
    dtaFrm <- inp2DF(dtaInp = dtaInp, usePkg = usePkg, selSet = selSet, ...)

    if (!all(is.character(unlist(varPrs))) || !all(nzchar(unlist(varPrs))) ||
        !all(unlist(varPrs) %in% names(dtaInp)) || !all(vapply(varPrs, length, numeric(1)) == 2) ||
        !all(vapply(varPrs, function(v) isa(v, "character") || isa(v, "list"), logical(1)))) {
        stop("The parameter varPrs needs to be a list with at least one (valid) variable pair to combine.")
    }

    for (crrPrs in varPrs) {
        if (is.list(crrPrs)) crrPrs <- as.character(crrPrs)
        # all values are equal (or NA) -> replace NAs in variable 1 with the respective rows from variable 2
        if (all(dtaFrm[, crrPrs[1]] == dtaFrm[, crrPrs[2]], na.rm = TRUE) || mdeCmb == "first") {
            dtaFrm[is.na(dtaFrm[, crrPrs[1]]), crrPrs[1]] <- dtaFrm[is.na(dtaFrm[, crrPrs[1]]), crrPrs[2]]
        } else if (mdeCmb == "second") {
            dtaFrm[!is.na(dtaFrm[, crrPrs[2]]), crrPrs[1]] <- dtaFrm[!is.na(dtaFrm[, crrPrs[2]]), crrPrs[2]]
        } else if (mdeCmb == "none") {
            stop(sprintf("Mismatching values in the variable pair %s.",  paste(crrPrs, collapse = " - ")))
        }
        dtaFrm <- dtaFrm[, setdiff(names(dtaFrm), crrPrs[2])]
    }

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, dtaTtl = jmvTtl("_cmb_cols"), psvAnl = psvAnl, dtaInp = dtaInp, ...)
}
