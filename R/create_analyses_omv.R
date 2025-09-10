#' Creates analyses within a data set stored as .omv-file for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required;
#'               "FILENAME.ext"; default: NULL); files can be of any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param vecSyn Character vector with one or more analyses to be added (see Details; default: c())
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
#'               base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty)
#'
#' @details
#' * The aim of this function is to add analyses to jamovi data files. NB: The data set should not contain any existing analyses. These will be
#'   overwritten (a  warning is issued informing you about that).
#' * `vecSyn` is a character vector, containing one or more jamovi commands.
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for reading and writing the data.
#'   By clicking on the respective function under “See also”, you can get a more detailed overview over which parameters each of those functions
#'   take. The functions are: `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv`
#'   for CSV and `read.delim` for TSV which both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files), `read_sav`
#'   (needs the R-package `haven`) or `read.spss` (needs the R-package `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`)
#'   for Stata-files, `read_sas` (`haven`) for SAS-data-files, and `read_xpt` (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you
#'   would like to use `haven`, you may need to install it using `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `create_analyses_omv` internally uses the following functions for reading and writing data files in different formats: #'   [jmvReadWrite::read_omv()] and [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading
#'   .RData-files, [readRDS()] for .rds-files, [haven::read_sav()] or [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or
#'   [foreign::read.dta()] for Stata-files, [haven::read_sas()] for SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for
#'   SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' dtaFrm <- jmvReadWrite::ToothGrowth[, c("len", "supp", "dose")]
#' nmeOut <- tempfile(fileext = ".omv")
#'
#' }
#'
#' @export create_analyses_omv
#'
create_analyses_omv <- function(dtaInp = NULL, fleOut = "", vecSyn = c(), usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check the input parameters: vecSyn needs to be given (and in the correct format)
    if (!is.character(vecSyn) || length(vecSyn) < 1 || !nzchar(vecSyn)) {
        stop("Calling create_analyses_omv requires the parameter vecSyn (character vector).")
    }

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, getSyn = TRUE, getHTM = TRUE, usePkg = usePkg, selSet = selSet, ...)

    if (utils::packageVersion("jmvcore") < "2.4.3") {
        warning("jmvcore version 2.4.3 (or higher) is required for using create_analyses_omv.\n\n")
        return(invisible(NULL))
    }
    if (!jmvPtB()) stop("The R-packages RProtoBuf and jmvcore must be installed for using create_analyses_omv (see warnings() for further details).")

    # check whether the data set contains analyses and, if so warn that they will be overwritten
    if (!is.null(attr(dtaFrm, "syntax")) && length(attr(dtaFrm, "syntax")) > 0) {
        warning("The data set contains analyses. Those will be overwritten.")
        if (is.character(dtaInp) && identical(dtaInp, fleOut)) return(invisible(NULL))
        attr(dtaFrm, "syntax")   <- NULL
        attr(dtaFrm, "protobuf") <- NULL
        attr(dtaFrm, "HTML")     <- htmTxt()
    }

    # check whether the data set contains the attribute HTML, if not, add text of an empty index.html
    if (is.null(attr(dtaFrm, "HTML"))) attr(dtaFrm, "HTML") <- htmTxt()

    attr(dtaFrm, "protobuf")[["01 empty/analysis"]] <-
      RProtoBuf::new(jamovi.coms.AnalysisResponse, analysisId = 1, name = "empty", ns = "jmv", options = optPtB,
        results = RProtoBuf::new(jamovi.coms.ResultsElement, title = "Results", status = 3, group = RProtoBuf::new(jamovi.coms.ResultsGroup)),
        status = 3, index = 1, title = "Results", hasTitle = TRUE)

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, dtaTtl = jmvTtl("_anl"), wrtPtB = TRUE, ...)
}

options <- jmv:::anovaOptions$new()
options <- eval(parse(text = "jmv:::anovaOptions$new()"))

o <- options$option('dep')
o$value <- 'len'
options$.__enclos_env__$private$.env$dep <- 'len'

o <- options$option('factors')
o$value <- list('supp', 'dose')
options$.addOption(o)

analysis <- jmv:::anovaClass$new(
    options = options,
    data = ToothGrowth)

analysis$.__enclos_env__$private$.analysisId <- 0

analysis$init()
analysis$run()
analysis$results

anaPB <- analysis$asProtoBuf(final=TRUE)
serialize(connection=NULL, anaPB)

