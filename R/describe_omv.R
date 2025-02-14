#' Adds a title and a description for a data set stored as .omv-file for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of
#'               any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param dtaTtl Character vector with a title to be added to the data set (see Details; default: "")
#' @param dtaDsc Description of the data set, either as character vector (HTML-formatted) or as named list with the entries "description", "variables",
#'               "references", and "license" (see Details; default: "")
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
#'               base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) where the order of variables / columns of the input data set is re-arranged
#'
#' @details
#' * The aim of this function is to add a title and a data set description to jamovi data files. Two typical use cases would be (1) to help creating data sets
#'   to be used in teaching (i.e., either creating or using data sets in R, and afterwards adding a description to those), and (2) to provide ”properly
#'   described“ data when publishing in a repository such as the OSF).
#' * NB: The data set should not contain any existing analyses. These will be overwritten (a  warning is issued informing you about that).
#' * `dtaTtl` is a title for the dataset (at the top of the results output, i.e., that title which initially is “Results” when you create a new data set in
#'   jamovi).
#' * `dtaDsc` can either be a character vector (with length = 1) containing HTML-formatted text that describes the data set (see `chrDsc` in the examples for
#'   HTML tags that are currently implemented; putting “unformatted” text is not a problem, but then the result is just plain text without formatting).
#'   Alternatively, `dtaDcs` can be a named list with the entries `description`, `variables`, `references`, `license`. All entries except from `variables`
#'   contain character vectors (length = 1); `variables` shall be a named list with the variable name as name and a description what the variable contains as
#'   entry. `description` and `variables` must be given, `references` and `license` can be left blank (""; but the names must be present in the list). An
#'   example for both a named list with a description (`lstDsc`), as well as a character vector with all HTML tags that are implemented (`chrDsc`) can be found
#'   in the examples below.
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for reading and writing the data. By clicking
#'   on the respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take. The functions are:
#'   `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV
#'   which both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files), `read_sav` (needs the R-package `haven`) or `read.spss`
#'   (needs the R-package `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas` (`haven`) for SAS-data-files,
#'   and `read_xpt` (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you would like to use `haven`, you may need to install it using
#'   `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `describe_omv` internally uses the following functions for reading and writing data files in different formats: [jmvReadWrite::read_omv()] and
#'   [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files, [readRDS()] for .rds-files,
#'   [haven::read_sav()] or [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for
#'   SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' dtaFrm <- jmvReadWrite::ToothGrowth[, c("len", "supp", "dose")]
#' nmeOut <- tempfile(fileext = ".omv")
#'
#' # the paste's underneath are only for readability (without them, the vignettes are misformatted)
#' lstDsc <- list(description = paste("The response is the length of odontoblasts (cells responsible",
#'                                    "for tooth growth) in 60 guinea pigs. Each animal received one",
#'                                    "of three dose levels of vitamin C (0.5, 1, and 2 mg / day) by",
#'                                    "one of two delivery methods, orange juice or ascorbic acid (a",
#'                                    "form of vitamin C and coded as VC)."),
#'                variables = list(len  = "Tooth length",
#'                                 supp = "Supplement type (VC or OJ)",
#'                                 dose = "Dose (in milligrams / day"),
#'                references = paste("Crampton, E. W. (1947). The growth of the odontoblast of the",
#'                                   "incisor teeth as a criterion of vitamin C intake of the guinea",
#'                                   "pig. <em>The Journal of Nutrition, 33</em>(5), 491-504.",
#'                                   "https://doi.org/10.1093/jn/33.5.491"),
#'                license = "")
#' jmvReadWrite::describe_omv(dtaInp = dtaFrm, fleOut = nmeOut, dtaTtl = "ToothGrowth",
#'   dtaDsc = lstDsc)
#' # don't include the unlink, if you copy the code and want to look at the resulting output file
#' unlink(nmeOut)
#'
#' # the code underneath should cover all formatting options jamovi is able to use (paste0 is only
#' # for readability)
#' chrDsc <- paste0("<p><strong>Trial – all formattings:</strong><br/><strong>bold</strong><br/>",
#'                  "<strong><em>bold, italics</em></strong><br/><em>italics</em><br/><u>underlined",
#'                  "</u><br/>link:<a href=\"https://jamovi.org﻿﻿﻿\" target=\"_blank\">https://",
#'                  "jamovi.org﻿﻿﻿</a><br/><s>strikethrough</s><br/>C<sub>2</sub>H<sub>5</sub>",
#'                  "OH<br/>R<sup>2</sup><br/><span style=\"background-color:#e60000\">background ",
#'                  "colour: red</span><br/><span style=\"color:#e60000\">foreground color: red",
#'                  "</span></p><p class=\"ql-align-center\">centered</p><p class=\"ql-align-right\">",
#'                  "right</p><p class=\"ql-align-justify\">justify justify justify justify justify ",
#'                  "justify justify justify justify justify justify justify justify justify justify ",
#'                  "justify justify justify justify justify justify justify justify justify justify",
#'                  "</p><p><br/></p><ol><li>numbered list</li><li>numbered list</li></ol><p><br/>",
#'                  "</p><ul><li>bullet point</li><li>bullet point</li></ul><p class=\"ql-indent-1\">",
#'                  "indented once</p><p class=\"ql-indent-2\">indented twice</p><p ",
#'                  "class=\"ql-indent-1\">indented once</p><p>Formula: <span class=\"ql-formula\">",
#'                  "e=mc^2</span></p><pre>Preformatted</pre><p>normal again</p><h2>Heading</h2>")
#' jmvReadWrite::describe_omv(dtaInp = dtaFrm, fleOut = nmeOut, dtaTtl = "ToothGrowth",
#'   dtaDsc = chrDsc)
#' unlink(nmeOut)
#' }
#'
#' @export describe_omv
#'
describe_omv <- function(dtaInp = NULL, fleOut = "", dtaTtl = c(), dtaDsc = c(), usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check the input parameters: either dtaTtl or dtaDsc need to be given (and in the correct format)
    dscPrm(dtaTtl, dtaDsc)

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, getSyn = TRUE, getHTM = TRUE, usePkg = usePkg, selSet = selSet, ...)

    if (utils::packageVersion("jmvcore") < "2.4.3") {
        warning("jmvcore version 2.4.3 (or higher) is required for using describe_omv.\n\n")
        return(invisible(NULL))
    }
    if (!jmvPtB()) stop("The R-packages RProtoBuf and jmvcore must be installed for using describe_omv (see warnings() for further details).")

    # check whether dtaDsc is a list, and if so, convert it to a HTML desription
    if (is.list(dtaDsc)) {
        dtaDsc <- crtHTM(dtaDsc)
    }

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

    # add title and description in HMTL
    attr(dtaFrm, "HTML") <- addHTM(attr(dtaFrm, "HTML"), dtaTtl, dtaDsc)

    # add title and description as protobuffer (calls optPtB to create protobuffer options)
    attr(dtaFrm, "protobuf")[["01 empty/analysis"]] <-
      RProtoBuf::new(jamovi.coms.AnalysisResponse, analysisId = 1, name = "empty", ns = "jmv", options = optPtB(dtaTtl, dtaDsc),
        results = RProtoBuf::new(jamovi.coms.ResultsElement, title = "Results", status = 3, group = RProtoBuf::new(jamovi.coms.ResultsGroup)),
        status = 3, index = 1, title = "Results", hasTitle = TRUE)

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, dtaTtl = jmvTtl("_desc"), wrtPtB = TRUE, ...)
}

# =================================================================================================
# helper functions (in alphabetical order)
# =================================================================================================

# add / replace HTML for title and description within index.html
addHTM <- function(crrHTM = "", dtaTtl = c(), dtaDsc = c()) {
    vldHTM <- which(!grepl("^\\s*$", crrHTM))
    fndTtl <- grep("<h1.*?>", crrHTM[vldHTM])[1]
    if (length(dtaTtl) > 0 && nzchar(dtaTtl)) {
        splLne <- splHTM(crrHTM[vldHTM[fndTtl + 0]])
        splLne[grep("<h1.*?>", splLne) + 1] <- dtaTtl
        crrHTM[vldHTM[fndTtl + 0]] <- paste0(splLne, collapse = "")
    }
    if (length(dtaDsc) > 0 && nzchar(dtaDsc)) {
        # issue warning if the target line hasn't the expected format
        if (!grepl("^\\s*<p.*?></p>\\s*$", crrHTM[vldHTM[fndTtl + 1]])) {
            warning(sprintf("Unexpected HTML to be replaced with description: %s\n\n", crrHTM[vldHTM[fndTtl + 1]]))
        }
        # assign description embedded in <div> / </div> tags
        crrHTM[vldHTM[fndTtl + 1]] <- paste0("<div class=\"note\">", clnHTM(dtaDsc, toHTM = TRUE), "</div>")
    }

    return(crrHTM)
}

# ensure that the input line is concatened (if required) and remove leading and trailing <p> / </p> tags,
# then split the input (into HTML tags and content) and replace characters that have a HTML-function (e.g., "/")
# in the content
clnHTM <- function(inpLne = c(), toHTM = FALSE) {
    inpLne <- vapply(inpLne, function(x) paste0(ifelse(grepl("^\\s*<p.*?>|^\\s*<li.*?>", x), "", "<p>"), x, ifelse(grepl("</p.*?>\\s*$|</li.*?>\\s*$", x), "", "</p>")),
                character(1), USE.NAMES = FALSE)
    splLne <- splHTM(paste0(inpLne, collapse = ""))
    selCnt <- !grepl("<.*?>", splLne)
    splLne[selCnt] <- rplHTM(splLne[selCnt], toHTM)
    return(paste0(splLne, collapse = ""))
}

# create HTML from the list-version of dtaDsc
crtHTM <- function(inpDsc = NULL) {
    outDsc <- c()
    outDsc <- paste0(outDsc, "<p><strong>Description:</strong></p>", clnHTM(inpDsc[["description"]]))
    outDsc <- paste0(outDsc, "<p><br/><strong>Variables:</strong></p><ul>")
    for (varNme in names(inpDsc[["variables"]])) {
         outDsc <- paste0(outDsc, clnHTM(paste0("<li><strong><em>", varNme, ":</em></strong> ", inpDsc[["variables"]][[varNme]], "</li>")))
    }
    outDsc <- paste0(outDsc, "</ul>")
    if (length(inpDsc[["references"]]) > 0 && all(nzchar(inpDsc[["references"]]))) {
        outDsc <- paste0(outDsc, "<p><br/><strong>References:</strong></p>", clnHTM(inpDsc[["references"]]))
    }
    if (length(inpDsc[["license"]]) > 0    && all(nzchar(inpDsc[["license"]]))) {
        outDsc <- paste0(outDsc, "<p><br/>", clnHTM(paste0("<em>", inpDsc[["license"]], "</em>")))
    }

    return(outDsc)
}

# check input parameters
dscPrm <- function(dtaTtl = c(), dtaDsc = NULL) {
    if ((!is.character(dtaTtl) || length(dtaTtl) != 1 || !nzchar(dtaTtl)) &&
        (((!is.character(dtaDsc) || length(dtaDsc) != 1 || !nzchar(dtaDsc)) &&
          (!is.list(dtaDsc) || !all(c("description", "variables", "references", "license") %in% names(dtaDsc)))))) {
        stop("Calling describe_omv requires either the parameter dtaTtl (character vector) or the parameter dtaDsc (character vector or named list).")
    }
}

# transform HTML tags into attributes (bold, italic, heading, etc.)
getAtt <- function(vecChr = c(), vecPos = NA, lstAtt = list()) {
    if (grepl("<p>|</p>", vecChr[vecPos])) return(lstAtt)
    prvAtt <- lstAtt

    # bold
    if (grepl("<strong>|<b>",       vecChr[vecPos])) lstAtt[["bold"]] <- TRUE
    if (grepl("</strong>|</b>",     vecChr[vecPos])) lstAtt[["bold"]] <- NULL
    # italic
    if (grepl("<em>|<i>",           vecChr[vecPos])) lstAtt[["italic"]] <- TRUE
    if (grepl("</em>|</i>",         vecChr[vecPos])) lstAtt[["italic"]] <- NULL
    # underline
    if (grepl("<u>",                vecChr[vecPos])) lstAtt[["underline"]] <- TRUE
    if (grepl("</u>",               vecChr[vecPos])) lstAtt[["underline"]] <- NULL
    # strikethrough
    if (grepl("<s>",                vecChr[vecPos])) lstAtt[["strike"]] <- TRUE
    if (grepl("</s>",               vecChr[vecPos])) lstAtt[["strike"]] <- NULL
    # preformatted / code-block
    if (grepl("<pre.*?>|<code.*?>", vecChr[vecPos])) lstAtt[["code-block"]] <- TRUE
    if (grepl("</pre>|</code>",     vecChr[vecPos])) lstAtt[["code-block"]] <- NULL
    # superscript
    if (grepl("<sup>",              vecChr[vecPos])) lstAtt[["script"]] <- "super"
    if (grepl("</sup>",             vecChr[vecPos])) lstAtt[["script"]] <- NULL
    # subscript
    if (grepl("<sub>",              vecChr[vecPos])) lstAtt[["script"]] <- "sub"
    if (grepl("</sub>",             vecChr[vecPos])) lstAtt[["script"]] <- NULL
    # heading
    if (grepl("<h\\d.*?>",          vecChr[vecPos])) lstAtt[["header"]] <- as.integer(gsub("<h(\\d).*?>", "\\1", vecChr[vecPos]))
    if (grepl("</h\\d>",            vecChr[vecPos])) lstAtt[["header"]] <- NULL
    # links
    if (grepl("<a href=.*?>",       vecChr[vecPos])) lstAtt[["link"]] <- gsub("<a href=\"(.*)\"\\s+.*?>", "\\1", vecChr[vecPos])
    if (grepl("</a>",               vecChr[vecPos])) lstAtt[["link"]] <- NULL
    # ordered list
    if (grepl("<ol>",               vecChr[vecPos])) lstAtt[["list"]] <- "ordered"
    if (grepl("</ol>",              vecChr[vecPos])) lstAtt[["list"]] <- NULL
    # bullet list
    if (grepl("<ul>",               vecChr[vecPos])) lstAtt[["list"]] <- "bullet"
    if (grepl("</ul>",              vecChr[vecPos])) lstAtt[["list"]] <- NULL
    # list entries
    if (grepl("<li>",               vecChr[vecPos])) attr(lstAtt[["list"]], "lstEnt") <- TRUE
    if (grepl("</li>",              vecChr[vecPos])) attr(lstAtt[["list"]], "lstEnt") <- FALSE
    # alignment
    if (grepl("class=\".*?ql-align-\\w+|style=\".*?text-align:\\s*\\w+", vecChr[vecPos])) {
        lstAtt[["align"]] <- gsub(".*?ql-align-(\\w+).*", "\\1", gsub(".*?text-align:\\s*(\\w+).*", "\\1", vecChr[vecPos]))
        attr(lstAtt[["align"]], "endPos") <- nxtAtt(vecChr, vecPos, "class=|style=")
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
    # formula
    if (grepl("class=\".*?ql-formula", vecChr[vecPos])) {
        lstAtt[["formula"]] <- TRUE
        attr(lstAtt[["formula"]], "endPos") <- nxtAtt(vecChr, vecPos, "class=")
    } else if (chkAtt(lstAtt[["formula"]], "endPos") && attr(lstAtt[["formula"]], "endPos") <= vecPos) {
        lstAtt[["formula"]] <- NULL
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
        warning(sprintf("The HTML tag \"%s\" isn't implemented, and hence the jamovi formatting attributes remain unchanged.\n", vecChr[vecPos]))
    }

    return(lstAtt)
}

# find the next HTML tag of a particular type
nxtAtt <- function(vecChr = c(), vecPos = NA, addTrm = c()) {
    grep(paste(c(paste0(c("</", "<"), gsub("<(\\w+)\\s+.*", "\\1", vecChr[vecPos])), addTrm), collapse = "|"),
      vecChr[seq(vecPos + 1, length(vecChr))])[1] + vecPos
}

# store title and description as protobuffer
optPtB <- function(dtaTtl = "", dtaDsc = c()) {
    # create protobuffer for the title
    if (length(dtaTtl) > 0 && nzchar(dtaTtl)) {
        ttlPtB <- var2PB(dtaTtl)
    } else {
        ttlPtB <- NULL
    }
    # create protobuffer for the description
    if (length(dtaDsc) > 0 && nzchar(dtaDsc)) {
        # gsub("^\\s*<p.*?>|</p>\\s*$", "",
        splDsc <- splHTM(gsub("(</h\\d>|</p>|</pre>|</code>)", "\n\\1", gsub("</li>", "</li>\n", gsub("<br\\s*/>|<br>", "\n", rplHTM(dtaDsc)))))
        tgtDsc <- !grepl("<.*?>", splDsc)
        attDsc <- htmPtB <- list()
        for (i in seq_along(splDsc)) {
            if (tgtDsc[i]) {
                if (is.null(attDsc[["formula"]]) || attDsc[["formula"]] == FALSE) crrIns <- splDsc[i] else crrIns <- list(formula = splDsc[i])
                htmPtB[[sum(tgtDsc[seq(i)])]] <- var2PB(c(prpAtt(attDsc), list(insert = crrIns)))
            } else {
                attDsc <- getAtt(splDsc, i, attDsc)
            }
        }

        dscPtB <- RProtoBuf::new(jamovi.coms.AnalysisOption, c = RProtoBuf::new(jamovi.coms.AnalysisOptions, options =
                      RProtoBuf::new(jamovi.coms.AnalysisOption, c = RProtoBuf::new(jamovi.coms.AnalysisOptions, options = htmPtB)),
                    hasNames = TRUE, names = "ops"))
    } else {
        dscPtB <- NULL
    }

    # assemble the two protobuffers (for title, and description) into a AnalysisOptions protobuffer
    RProtoBuf::new(jamovi.coms.AnalysisOptions, options = c(dscPtB, ttlPtB), hasNames = TRUE,
      names = c(rep("results//topText", !is.null(dscPtB)), rep("results//heading", !is.null(ttlPtB))))
}

prpAtt <- function(lstAtt = NULL) {
    # for list entries (<li>), the attribute "list" needs to be removed (it is only attached to the "\n" at
    # the end), and formulas are encoded by wrapping the "insert" (i.e., the attribute itself must not be
    # transformed: [1] assign lstAtt to a temporary variable, [2] remove the "list" entry if inside a list,
    # [3] remove the formula attribute (if present), [4] wrap the arguments in a named list (under the name
    # "attributes"), if crrArg is empty, nothing is added in c(crrAtt, ...)
    crrAtt <- lstAtt
    if (utils::hasName(lstAtt, "list") && identical(attr(lstAtt[["list"]], "lstEnt"), TRUE)) crrAtt[["list"]] <- NULL
    if (utils::hasName(lstAtt, "formula")) crrAtt[["formula"]] <- NULL
    if (length(crrAtt) > 0) return(list(attributes = crrAtt)) else return(invisible(NULL))
}

rplHTM <- function(inpVec = c(), toHTM = FALSE) {
    if (toHTM) {
        return(gsub("\\/", "&#x2F;", inpVec))
    } else {
        return(gsub("&nbsp;", " ", gsub("&#x2F;", "/",   inpVec)))
    }
}

# split HTML into constituents (i.e., tags and content)
splHTM <- function(vecChr = "") {
        splChr <- gsub("^\\s+\\n|\\n\\s+$", "\n", strsplit(vecChr, "(?=[<>])", perl = TRUE)[[1]])
        rplDsc <- grep("<", splChr)
        # has HTML tags
        if (length(rplDsc) > 0) {
            if (!all(splChr[rplDsc + 2] == ">")) {
                stop(paste("Error when decoding HTML. Please check whether the HTML-string the you used for the description is valid (i.e., no typos, etc.).",
                           "If it is valid, send it to sebastian.jentschke@uib.no"))
            }
            splChr[rplDsc + 1] <- paste0("<", splChr[rplDsc + 1], ">")
            return(splChr[-c(rplDsc, rplDsc + 2)])
        } else {
            return(splChr)
        }
}
