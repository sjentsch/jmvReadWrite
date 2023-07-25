#' Adds a title and a description for a data set stored as .omv-file for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of
#'               any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param dtaTtl Character vector with the title to be added to the data set (see Details; default: "")
#' @param dtaDsc Character vector with a HTML-formatted description of the data set (see Details; default: "")
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
    if ((length(dtaTtl) < 1 || !nzchar(dtaTtl)) && (length(dtaDsc) < 1 || !nzchar(dtaDsc))) {
        stop("Calling describe_omv requires either the parameter dtaTtl or the parameter dtaDsc (both are character vector).")
    }

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, getSyn = TRUE, getHTM = TRUE, usePkg = usePkg, selSet = selSet, ...)

    # check whether the data set contains the attribute HTML, if so, check whether
    # it contains analyses; if not, add it
    if (!is.null(attr(dtaFrm, "HTML"))) {

    } else {
        attr(dtaFrm, "HTML") <- htmTxt()
    }

    # add title and description
    # in HMTL
    if (nzchar(dtaDsc)) {
    }
    if (nzchar(dtaDsc)) {
    }
    
#    <h1 contenteditable=\"\" spellcheck=\"false\">Results</h1>
#    <p style=\"text-align:left;padding:0px 0px 0px 0px;\"></p>
# ->
#    <h1 contenteditable=\"\" spellcheck=\"false\">Album Sales</h1>
#    <div class="note">[dtaDsc]</div>

    # as protobuf
    if (nzchar(dtaTtl)) {
        ttlPtB <- RProtoBuf::new(jamovi.coms.AnalysisOption, s = dtaTtl)
    } else {
        ttlPtB <- NULL
    }
    if (nzchar(dtaDsc)) {
        splDsc <- gsub("^\\s+\\n|\\n\\s+$", "\n", strsplit(gsub("^<p>|</p>$", "", gsub("</p>|<br\\s*/>|<br>", "\n", dtaDsc)), "(?=[<>])", perl = TRUE)[[1]])
        rplDsc <- grep("<", splDsc)
        if (!all(splDsc[rplDsc + 2] == ">")) stop("Error when decoding HTML. Please send the HTML-string the you used for dtaDsc to sebastian.jentschke@uib.no")
        splDsc[rplDsc + 1] <- paste0("<", splDsc[rplDsc + 1], ">")
        splDsc <- splDsc[-c(rplDsc, rplDsc + 2)]
        tgtDsc <- which(!grepl("<.*?>", splDsc))
        attDsc <- list()
        htmPtB <- list()
        for (i in seq_along(splDsc)) {
            if (i %in% tgtDsc) {
# TO-DO: convert unicode
                if (length(attDsc) > 0) {
                    htmPtB[[which(i == tgtDsc)]] <- var2PB(list(attributes = attDsc, insert = splDsc[i]))
                } else {
                    htmPtB[[which(i == tgtDsc)]] <- var2PB(list(insert = splDsc[i]))
                }
            } else {
                attDsc <- getAtt(splDsc, i, attDsc)
				# formula
				if (grepl("class=\".*?ql-formula", splDsc[i])) {
					for (j in intersect(seq(i + 1, nxtAtt(splDsc, i, "class=") - 1), tgtDsc)) {
						splDsc[j] <- list(formula = splDsc[j])
					}
					splDsc[nxtAtt(splDsc, i, "class=")] <- ""
				}

			}                        
        }
        
        dscPtB <- RProtoBuf::new(jamovi.coms.AnalysisOption, c = RProtoBuf::new(jamovi.coms.AnalysisOptions, options =
                      RProtoBuf::new(jamovi.coms.AnalysisOption, c = RProtoBuf::new(jamovi.coms.AnalysisOptions, options = htmPtB)),
                    hasNames = TRUE, names = "ops"))
    } else {
        dscPtB <- NULL
    }
    optPtB <- RProtoBuf::new(jamovi.coms.AnalysisOptions, options = list(dscPtB, ttlPtB), hasNames = TRUE,
      names = c("results//topText", "results//heading"))
    attr(dtaFrm, "protobuf")[["01 empty/analysis"]] <-
      RProtoBuf::new(jamovi.coms.AnalysisResponse, analysisId = 1, name = "empty", ns = "jmv", options = O, status = 3)

    # write the resulting data frame to the output file or, if no output file
    # name was given, return the data frame
    if (!is.null(fleOut) && nzchar(fleOut)) {
        fleOut <- fmtFlO(fleOut)
        write_omv(dtaFrm, fleOut)
        return(invisible(NULL))
    } else {
        dtaFrm
    }
}

nxtAtt <- function(vecChr = c(), vecPos = NA, addTrm = c()) {
    grep(paste(c(paste0(c("</", "<"), gsub("<(\\w+)\\s+.*", "\\1", vecChr[vecPos])), addTrm), collapse = "|"),
      vecChr[seq(vecPos + 1, length(vecChr))])[1] + vecPos
}

getAtt <- function(vecChr = c(), vecPos = NA, lstAtt = list()) {
    if (!nzchar(vecChr[vecPos])) return(lstAtt)
    prvAtt <- lstAtt
    
    # bold
    if (grepl("<strong>|<b>",   vecChr[vecPos])) lstAtt[["bold"]] <- TRUE
    if (grepl("</strong>|</b>", vecChr[vecPos])) lstAtt[["bold"]] <- NULL
    # italic
    if (grepl("<em>|<i>",       vecChr[vecPos])) lstAtt[["italic"]] <- TRUE
    if (grepl("</em>|</i>",     vecChr[vecPos])) lstAtt[["italic"]] <- NULL
    # underline
    if (grepl("<u>",            vecChr[vecPos])) lstAtt[["underline"]] <- TRUE
    if (grepl("</u>",           vecChr[vecPos])) lstAtt[["underline"]] <- NULL
    # strikethrough
    if (grepl("<s>",            vecChr[vecPos])) lstAtt[["strike"]] <- TRUE
    if (grepl("</s>",           vecChr[vecPos])) lstAtt[["strike"]] <- NULL
    # preformatted / code-block
    if (grepl("<pre.*?>",       vecChr[vecPos])) lstAtt[["code-block"]] <- TRUE
    if (grepl("</pre>",         vecChr[vecPos])) lstAtt[["code-block"]] <- NULL
    # superscript
    if (grepl("<sup>",          vecChr[vecPos])) lstAtt[["script"]] <- "super"
    if (grepl("</sup>",         vecChr[vecPos])) lstAtt[["script"]] <- NULL
    # subscript
    if (grepl("<sub>",          vecChr[vecPos])) lstAtt[["script"]] <- "sub"
    if (grepl("</sub>",         vecChr[vecPos])) lstAtt[["script"]] <- NULL
    # heading
    if (grepl("<h\\d.*?>",      vecChr[vecPos])) lstAtt[["header"]] <- as.integer(gsub("<h(\\d).*?>", "\\1", vecChr[vecPos]))
    if (grepl("</h\\d>",        vecChr[vecPos])) lstAtt[["header"]] <- NULL
    # links
    if (grepl("<a href=.*?>",   vecChr[vecPos])) lstAtt[["link"]] <- gsub("<a href=\"(.*)\"\\s+.*?>", "\\1", vecChr[vecPos])
    if (grepl("</a>",           vecChr[vecPos])) lstAtt[["link"]] <- NULL
    # ordered list
    if (grepl("<ol>",           vecChr[vecPos])) lstAtt[["list"]] <- "ordered"
    if (grepl("</ol>",          vecChr[vecPos])) lstAtt[["list"]] <- NULL
    # bullet list
    if (grepl("<ul>",           vecChr[vecPos])) lstAtt[["list"]] <- "bullet"
    if (grepl("</ul>",          vecChr[vecPos])) lstAtt[["list"]] <- NULL
    # alignment
	if (grepl("class=\".*?ql-align-\\w+", vecChr[vecPos])) {
        lstAtt[["align"]] <- gsub(".*?ql-align-(\\w+).*", "\\1", vecChr[vecPos])
        attr(lstAtt[["align"]], "endPos") <- nxtAtt(vecChr, vecPos, "class=")
    } else if (chkAtt(lstAtt[["align"]], "endPos") && attr(lstAtt[["align"]], "endPos") <= vecPos) {
        lstAtt[["align"]] <- NULL
    }
    # indent
	if (grepl("class=\".*?ql-indent-\\d", vecChr[vecPos])) {
        lstAtt[["indent"]] <- as.integer(gsub(".*?ql-indent-(\\d+).*", "\\1", vecChr[vecPos]))
        attr(lstAtt[["indent"]], "endPos") <- nxtAtt(vecChr, vecPos, "class=")
    } else if (chkAtt(lstAtt[["indent"]], "endPos") && attr(lstAtt[["indent"]], "endPos") <= vecPos) {
        lstAtt[["indent"]] <- NULL
    }
    # colours
	if (grepl("style=\".*?background-color:", vecChr[vecPos])) {
        lstAtt[["background"]] <- gsub(".*?background-color:.*(\\#[0-9,a-f]+).*", "\\1", vecChr[vecPos])
        attr(lstAtt[["background"]], "endPos") <- nxtAtt(vecChr, vecPos, "style=")
    } else if (chkAtt(lstAtt[["background"]], "endPos") && attr(lstAtt[["background"]], "endPos") <= vecPos) {
        lstAtt[["background"]] <- NULL
    }
	if (grepl("style=\".*?color:", vecChr[vecPos]) && !grepl("style=\".*?background-color:", vecChr[vecPos])) {
        lstAtt[["color"]] <- gsub(".*?color:.*(\\#[0-9,a-f]+).*", "\\1", vecChr[vecPos])
        attr(lstAtt[["color"]], "endPos") <- nxtAtt(vecChr, vecPos, "style=")
    } else if (chkAtt(lstAtt[["color"]], "endPos") && attr(lstAtt[["color"]], "endPos") <= vecPos) {
        lstAtt[["color"]] <- NULL
    }

    if (identical(prvAtt, lstAtt)) {
        cat(sprintf("Attributes unchanged - entry %d\n", vecPos))
    }
    
    return(lstAtt)
}
