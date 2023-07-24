#' Adds a title and a description for a data set stored as .omv-file for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of
#'               any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param dtaTtl Character vector with the title to be added to the data set (see Details; default: "")
#' @param dtaDsc HTML-formatted description of the data set (see Details; default: "")
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
#'               base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) where the order of variables / columns of the input data set is re-arranged
#'
#' @details
#' * The aim of this function is to help 
#'   The main use of it is likely to help creating data sets to be used in teaching (or provide ”properly described“ data when publishing in a repository
#'   such as the OSF).
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for reading the data. By clicking on the
#'   respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take. The functions are:
#'   `read_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV which both are
#'   based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files), `read_sav` (needs R-package `haven`) or `read.spss` (needs R-package
#'   `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas` (`haven`) for SAS-data-files, and `read_xpt`
#'   (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you would like to use `haven`, you may need to install it manually (i.e.,
#'   `install.packages("haven", dep = TRUE)`).
#'
#' @seealso `annotate_omv` internally uses the following functions to read data files in different formats: [jmvReadWrite::read_omv()] for jamovi-files,
#'   [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files, [readRDS()] for .rds-files, [haven::read_sav()] or [foreign::read.spss()]
#'   for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for SAS-data-files, and [haven::read_xpt()] or
#'   [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite)
#' fleOMV <- system.file("extdata", "AlbumSales.omv", package = "jmvReadWrite")
#' fleTmp <- paste0(tempfile(), ".omv")
#' unlink(fleTmp)
#' }
#'
#' @export describe_omv
#'
describe_omv <- function(dtaInp = NULL, fleOut = "", dtaTtl = c(), dtaDsc = c(), usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check the input parameters: either dtaTtl or dtaDsc need to be given
    if ((length(dtaTtl) < 1 || !is.character(varOrd) || !all(nzchar(varOrd))) &&
        (length(dtaDsc) < 1 || !is.list(varMve) || !is.character(names(varMve)) || !is.numeric(unlist(varMve)) || !all(unlist(varMve) != 0) || !all(unlist(varMve) %% 1 == 0))) {
        stop("Calling annotate_omv requires either the parameter varOrd (a character vector) or the parameter varMve (a named list), using the correct format (see Details in help).")
    }

# name has to be given

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, getSyn = TRUE, getHTM = TRUE, usePkg = usePkg, selSet = selSet, ...)

    # check whether there are any 

    # check whether the data set contains the attribute HTML, otherwise add it
    if (!is.null(attr(dtaFrm, "HTML"))) {
            
    }

    # add title and description
    # in HMTL
#    <h1 contenteditable=\"\" spellcheck=\"false\">Results</h1>
#    <p style=\"text-align:left;padding:0px 0px 0px 0px;\"></p>
# ->
#    <h1 contenteditable=\"\" spellcheck=\"false\">Album Sales</h1>
#    <div class="note"><p><strong>Description:</strong></p><p>Imagine that you worked for a record company and that your boss was interested in predicting album sales from advertising. This data file has 200 rows, each one representing a different album.</p><p>&nbsp;</p><p><strong>Variables:</strong></p><ul><li><strong><em>Adverts</em></strong>: amount (in thousands of pounds) spent promoting the album before release</li><li><strong><em>Airplay</em></strong>: how many times songs from the album were played on a prominent national radio station in the week before release</li><li><strong><em>Image</em></strong>: how attractive people found the band&#x27;s image (out of 10)</li><li><strong><em>Sales</em></strong>: sales (in thousands) of each album in the week after release</li></ul><p>&nbsp;</p><p><strong>Reference:</strong></p><p>Field, A. P. (2017). <em>Discovering Statistics Using IBM SPSS Statistics</em> (5th ed.). Sage. [Fictional data set]</p><p><em>The data set was constructed by Andy Field who therefore owns the copyright. Andy Field generously agreed that we can include the data set in the jamovi Data Library. This data set is also publicly available on the website that accompanies Andy Field`s book, https:&#x2F;&#x2F;edge.sagepub.com&#x2F;field5e. Without Andy Field`s explicit consent, this data set may not be distributed for commercial purposes, this data set may not be edited, and this data set may not be presented without acknowledging its source (i.e., the terms of a CC BY-NC-ND license).</em></p> </div>
    # as protobuf
    htmPtB <- list()
    dscPtB <- RProtoBuf::new(jamovi.coms.AnalysisOption, c = RProtoBuf::new(jamovi.coms.AnalysisOptions, options =
                  RProtoBuf::new(jamovi.coms.AnalysisOption, c = RProtoBuf::new(jamovi.coms.AnalysisOptions, options = htmPtB)),
                hasNames = TRUE, names = "ops"))
    ttlPtB <- RProtoBuf::new(jamovi.coms.AnalysisOption, s = dtaTtl)
    optPtB <- RProtoBuf::new(jamovi.coms.AnalysisOptions, options = list(dscPtB, ttlPtB), hasNames = TRUE,
      names = c("results//topText", "results//heading"))
    attr(dtaFrm, "protobuf")[["01 empty/analysis"]] <-
      RProtoBuf::new(jamovi.coms.AnalysisResponse, analysisId = 1, name = "empty", ns = "jmv", options = O, status = 3)

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
    dtaFrm <- setAtt(setdiff(names(attMem), c("names", "row.names", "class", "fltLst")), attMem, dtaFrm)

    # write the resulting data frame to the output file or, if no output file
    # name was given, return the data frame
    if (!is.null(fleOut) && nzchar(fleOut)) {
        fleOut <- fmtFlO(fleOut)
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
