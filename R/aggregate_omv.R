#' Aggregates data from a data set / frame (in long format) and returns them as a .omv-file for the
#' statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path,
#'               if required; "FILENAME.ext"; default: NULL); files can be of any supported file
#'               type, see Details below.
#' @param fleOut Name of the aggregated data set / file to be written (including the path, if
#'               required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead.
#' @param varAgg A character vector (default: c()) with the names of the variables which shall be
#'               aggregated.
#' @param grpAgg A character vector (default: c()) with the variables to group the aggregation
#'               variable (`varAgg`) by. If no grouping variable is given, the aggregation happens
#'               over the whole data set. If several grouping variables are given, the aggregation
#'               happens for each step / possible combination of these variables. See Details for
#'               more information.
#' @param clcN   If TRUE, counts the number of valid values for each step / combination of values
#'               in the grouping variable. The suffix appended `"_N"´ is appended to each variable
#'               in the resulting data set.
#' @param clcMss If TRUE, counts the number of missing values for each step / combination of values
#'               in the grouping variable. The suffix appended `"_Mss"` is appended to each
#'               variable in the resulting data set.
#' @param clcMn  If TRUE, calculates the mean for each step / combination of values in the grouping
#'               variable. The suffix appended `"_Mn"` is appended to each variable in the
#'               resulting data set.
#' @param clcMdn If TRUE, calculates the median for each step / combination of values in the
#'               grouping variable. The suffix appended `"_Mdn"` is appended to each variable in
#'               the resulting data set.
#' @param clcMde If TRUE, calculates the mode for each step / combination of values in the grouping
#'               variable. The suffix appended `"_Mde"` is appended to each variable in the
#'               resulting data set.
#' @param clcSum If TRUE, calculates the sum for each step / combination of values in the grouping
#'               variable. The suffix appended `"_Sum"` is appended to each variable in the
#'               resulting data set.
#' @param clcSD  If TRUE, calculates the std. deviation for each step / combination of values in
#'               the grouping variable. The suffix appended `"_SD"` is appended to each variable in
#'               the resulting data set.
#' @param clcVar If TRUE, calculates the variance for each step / combination of values in the
#'               grouping variable. The suffix appended `"_Var"` is appended to each variable in
#'               the resulting data set.
#' @param clcRng If TRUE, calculates the range for each step / combination of values in the
#'               grouping variable. The suffix appended `"_Rng"` is appended to each variable in
#'               the resulting data set.
#' @param clcMin If TRUE, determines the minimum at each step / combination of values in the
#'               grouping variable. The suffix appended `"_Min"` is appended to each variable in
#'               the resulting data set.
#' @param clcMax If TRUE, determines the maximum at each step / combination of values in the
#'               grouping variable. The suffix appended `"_Max"` is appended to each variable in
#'               the resulting data set.
#' @param clcIQR If TRUE, calculates the IQR for each step / combination of values in the grouping
#'               variable. The suffix appended `"_IQR"` is appended to each variable in the
#'               resulting data set.
#' @param drpNA  If TRUE (default: TRUE), NA values are removed before the aggregation, and the
#'               aggregation is calculated using the valid values. If FALSE, the result would be NA
#'               for any step / combination of values in the grouping variable that contains a NA
#'               value.
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata,
#'               and SAS files; "foreign" is the default (it comes with base R), but "haven" is
#'               newer and more comprehensive.
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when
#'               reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below.
#'
#' @return a data frame containing a symmetric matrix (only returned if `fleOut` is empty)
#'         containing the distances between the variables / columns (clmDst == TRUE) or rows
#'         (clmDst == FALSE)
#'
#' @details
#' * `varAgg` must be a character vector containing the variables to be aggregated. From these
#'   variables, a new data set is created where the values in the original data set are aggregated
#'   for each possible combination of steps in the grouping variable(s) given in `grpAgg`. For
#'   example, if one grouping variable were a participant ID, and another grouping variable were
#'   measurement time points, the resulting new dataset would contain as many rows as the number of
#'   participants multiplied by the number of measurement time points. For each variable in
#'   `varAgg`, and each possible aggregation (`clcN`, `clcMn`, etc. if set to TRUE), a new column
#'   would be generated in the resulting new data set.
#' * `drpNA` determines whether NA should be dropped / removed before calculating an aggregation.
#'   If set to FALSE, the result for a combination of steps in the grouping variable(s) where at
#'   least one value is NA would be NA. If set to TRUE, only the values for any given combination
#'   of steps in the grouping variable(s) that are not NA are considered for the calculation. For
#'   `clcN` and `clcMss`, `drpNA` has no effect.
#'   NB: If `drpNA` is set to TRUE, any row where any of the grouping variable(s) has the value NA
#'   is excluded from the aggregation, if `drpNA` is set to FALSE and there is any row where any of
#'   the group variables has the value NA, an error is thrown and no aggregation is carried out.
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions
#'   that are used for reading and writing the data. By clicking on the respective function under
#'   “See also”, you can get a more detailed overview over which parameters each of those functions
#'   take. The functions are: `read_omv` and `write_omv` (for jamovi-files), `read.table` (for
#'   CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV which
#'   both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files),
#'   `read_sav` (needs the R-package `haven`) or `read.spss` (needs the R-package `foreign`) for
#'   SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas`
#'   (`haven`) for SAS-data-files, and `read_xpt` (`haven`) / `read.xport` (`foreign`) for
#'   SAS-transport-files. If you would like to use `haven`, you may need to install it using
#'   `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `aggregate_omv` internally uses [stats::aggregate()] as function for the aggregation,
#'   and [base::is.na()], [base::mean()], [stats::median()], [base::sum()], [stats::sd()],
#'   [stats::var()], [base::range()], [base::min()], [base::max()], and
#'   [stats::quantile()] for calculation. It furthermore uses the following functions for reading
#'   and writing data files in different formats: [jmvReadWrite::read_omv()] and
#'   [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files,
#'   [load()] for reading .RData-files, [readRDS()] for .rds-files, [haven::read_sav()] or
#'   [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for
#'   Stata-files, [haven::read_sas()] for SAS-data-files, and [haven::read_xpt()] or
#'   [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' # generate a test dataframe with 100 (imaginary) participants / units of
#' #  observation (ID), 10 measurement (measure) of two variables (V1, V2)
#' dtaInp <- data.frame(ID = rep(as.character(seq(1, 100)), each = 10),
#'                      Measure = rep(seq(1, 10), times = 100),
#'                      V1 = runif(1000,   0, 100),
#'                      V2 = round(rnorm(1000, 100,  15)))
#' cat(str(dtaInp))
#' # the output should look like this
#' # 'data.frame': 1000 obs. of  4 variables:
#' #  $ ID     : chr  "1" "1" "1" "1" ...
#' #  $ Measure: int  1 2 3 4 5 6 7 8 9 10 ...
#' #  $ V1     : num  ...
#' #  $ V2     : num  ...
#' # this data set is stored as (temporary) RDS-file and later processed by long2wide
#' nmeInp <- tempfile(fileext = ".rds")
#' nmeOut <- tempfile(fileext = ".omv")
#' saveRDS(dtaInp, nmeInp)
#' jmvReadWrite::aggregate_omv(dtaInp = nmeInp, fleOut = nmeOut, varAgg = c("V1", "V2"),
#'                             grpAgg = "ID", clcN = TRUE, clcMn = TRUE, clcSD = TRUE)
#' # it is required to give at least the arguments dtaInp, varAgg and grpAgg, each of
#' # the different switches (clc...) requests a aggregation measure (e.g., mean, median,
#' # SD, IQR, etc.) to be calculated
#' # check whether the file was created and its size
#' cat(list.files(dirname(nmeOut), basename(nmeOut)))
#' # -> "file[...].omv" ([...] contains a random combination of numbers / characters
#' cat(file.info(nmeOut)$size)
#' # -> 4898 (approximate size; size may differ in every run [in dependence of
#' #          how well the generated random data can be compressed])
#' cat(str(jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)))
#' # the data set contains now the ID variable identifying the different steps of
#' # aggregation and one column for each combination of aggregation variable (V1 / V2)
#' # and which calculation was requested (N, mean and SD)
#' # 'data.frame':	100 obs. of  7 variables:
#' #  $ ID   : chr  "1" "10" "100" "11" ...
#' #   ..- attr(*, "jmv-id")= logi TRUE
#' #   ..- attr(*, "missingValues")= list()
#' #  $ V1_N : int  10 10 10 10 10 10 10 10 10 10 ...
#' #   ..- attr(*, "jmv-desc")= chr "V1 (N)"
#' #   ..- attr(*, "missingValues")= list()
#' #  $ V1_Mn: num  45.4 51.9 49.4 54.8 47.2 ...
#' #   ..- attr(*, "jmv-desc")= chr "V1 (Mean)"
#' #   ..- attr(*, "missingValues")= list()
#' #  $ V1_SD: num  31.7 31.4 26.5 20.2 29.1 ...
#' #   ..- attr(*, "jmv-desc")= chr "V1 (SD)"
#' #   ..- attr(*, "missingValues")= list()
#' #  $ V2_N : int  10 10 10 10 10 10 10 10 10 10 ...
#' #   ..- attr(*, "jmv-desc")= chr "V2 (N)"
#' #   ..- attr(*, "missingValues")= list()
#' #  $ V2_Mn: num  96.4 102.3 101.6 104.6 108.7 ...
#' #   ..- attr(*, "jmv-desc")= chr "V2 (Mean)"
#' #   ..- attr(*, "missingValues")= list()
#' #  $ V2_SD: num  14.8 18.4 11.2 10.1 14.3 ...
#' #   ..- attr(*, "jmv-desc")= chr "V2 (SD)"
#' #   ..- attr(*, "missingValues")= list()
#'
#' unlink(nmeInp)
#' unlink(nmeOut)
#'
#'
#' @export aggregate_omv
#'
aggregate_omv <- function(dtaInp = NULL, fleOut = "", varAgg = c(), grpAgg = c(), clcN = FALSE,
                          clcMss = FALSE, clcMn = FALSE, clcMdn = FALSE, clcMde = FALSE,
                          clcSum = FALSE, clcSD = FALSE, clcVar = FALSE, clcRng = FALSE,
                          clcMin = FALSE, clcMax = FALSE, clcIQR = FALSE, drpNA = TRUE,
                          usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    frmInp <- inp2DF(dtaInp = dtaInp, usePkg = usePkg, selSet = selSet, ...)

    if (!all(nzchar(varAgg)) || length(intersect(varAgg, names(frmInp))) < 1 ||
        !all(nzchar(grpAgg)) || length(intersect(grpAgg, names(frmInp))) < length(grpAgg)) {
        stop(paste("Calling aggregate_omv requires giving at least one (valid) variable to",
                   "aggregate, and the grouping variable(s) needs to be empty or valid."))
    }

    if (length(grpAgg) >= 1) {
        grpNA <- rowSums(is.na(frmInp[, grpAgg, drop = FALSE])) > 0
        if (!drpNA && any(grpNA))
            stop("The grouping variables must not contain empty or NA values (if drpNA is set to FALSE).")
    } else {
        grpNA <- rep(FALSE, nrow(frmInp))
    }

    clcSel <- c(clcN, clcMss,  clcMn,  clcMdn,   clcMde, clcSum, clcSD, clcVar,     clcRng,  clcMin, clcMax, clcIQR)
    clcStr <- c("N",  "Mss",   "Mn",   "Mdn",    "Mde",  "Sum",  "SD",  "Var",      "Rng",   "Min",  "Max",  "IQR")
    clcDsc <- c("N",  "Miss.", "Mean", "Median", "Mode", "Sum",  "SD",  "Variance", "Range", "Min.", "Max.", "IQR")
    if (!any(clcSel))
        stop("At least one aggregation calculation (clc...) needs to be set to TRUE.")

    frmOut <- NULL
    dscOut <- list()
    for (crrVar in varAgg) {
        crrDsc <- c(attr(frmInp[, crrVar], "description"), attr(frmInp[, crrVar], "jmv-desc"), crrVar)

        if (!is.numeric(frmInp[, crrVar]))
            frmInp[, crrVar] <- cnvCol(frmInp[, crrVar], "numeric")

        for (i in which(clcSel)) {
            crrNme <- sprintf("%s_%s", crrVar, clcStr[i])
            crrRes <- do.call(stats::aggregate,
                              c(list(x = frmInp[!grpNA, crrVar], by = frmInp[!grpNA, grpAgg, drop = FALSE], FUN = getFnc(clcStr[i], drpNA)),
                                rep(list(na.rm = drpNA), !(clcStr[i] %in% c("N", "Mss", "Mde", "Rng", "IQR")))))
            names(crrRes)[ncol(crrRes)] <- crrNme
            dscOut[crrNme] <- sprintf("%s (%s)", crrDsc[nzchar(crrDsc)][1], clcDsc[i])
            if (is.null(frmOut)) {
                frmOut <- crrRes
            } else {
                frmOut <- merge(frmOut, crrRes, by = grpAgg, all = TRUE)
            }
        }
    }

    for (crrCol in names(dscOut)) attr(frmOut[, crrCol], "description") <- dscOut[[crrCol]]

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi
    # session or return it)
    rtnDta(dtaFrm = frmOut, fleOut = fleOut, dtaTtl = jmvTtl("_agg"), ...)
}

getFnc <- function(crrClc = "", drpNA = TRUE) {
    if        (crrClc == "N")   {
        eval(parse(text = "function(x) sum(!is.na(x))"))
    } else if (crrClc == "Mss") {
        eval(parse(text = "function(x) sum(is.na(x))"))
    } else if (crrClc == "Mn")  {
        mean
    } else if (crrClc == "Mdn") {
        stats::median
    } else if (crrClc == "Mde") {
        eval(parse(text = sprintf("function(x) ifelse(%s && any(is.na(x)), NA, as.numeric(names(sort(-table(x))))[1])", !drpNA)))
    } else if (crrClc == "Sum") {
        sum
    } else if (crrClc == "SD")  {
        stats::sd
    } else if (crrClc == "Var") {
        stats::var
    } else if (crrClc == "Rng") {
        eval(parse(text = sprintf("function(x) diff(range(x, na.rm = %s))", drpNA)))
    } else if (crrClc == "Min") {
        min
    } else if (crrClc == "Max") {
        max
    } else if (crrClc == "IQR") {
        eval(parse(text = sprintf(paste("function(x) ifelse(%s && any(is.na(x)), NA,",
                                        "stats::quantile(x, 0.75, na.rm = TRUE) - stats::quantile(x, 0.25, na.rm = TRUE))"), !drpNA)))
    }
}
