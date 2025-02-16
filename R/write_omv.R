#' Write files to be used with the statistical spreadsheet 'jamovi'
#' (<https://www.jamovi.org>)
#'
#' @param dtaFrm Data frame to be exported (default: NULL)
#' @param fleOut Name / position of the output file to be generated
#'               ("FILENAME.omv"; default: "")
#' @param wrtPtB Whether to write protocol buffers (see Details; default: FALSE)
#' @param frcWrt Whether to overwrite existing files with the same name (see Details; default: FALSE)
#' @param retDbg Whether to return a list with debugging information (see
#'               Value; default: FALSE)
#'
#' @return a list (if retDbg == TRUE), containing the meta data (mtaDta,
#'         metadata.json in the OMV-file), the extended data (xtdDta,
#'         xdata.json in the OMV-file) and the original data frame (dtaFrm)
#'
#' @details
#' * jamovi has a specific measurement level / type "ID" (in addition to the
#'   "standard" ones "Nominal", "Ordinal", and "Continuous"). "ID" is used for
#'   columns that contain some form of ID (e.g., a participant code). In order
#'   to set a variable of your data frame to "ID", you have to set the
#'   attribute `jmv-id` (e.g., `attr(dtaFrm$column, "jmv-id") = TRUE`).
#' * CAUTION: Setting wrtPtB to TRUE currently overwrites analyses that already
#'   exist in a data file. It is meant to be used for `describe_omv` only. If
#'   you set wrtPtB to TRUE, ensure to use an output file name that isn't would
#'   not overwrite any existing file. Protocol buffers are used to exchange
#'   data between the different parts of jamovi (the server and the client) and
#'   also the format in which analyses are stored in the jamovi data files.
#' * `write_omv` checks whether the output file already exists and throws an
#'   error if this is the case. frcWrt permits you to overwrite the existing
#'   file.
#'
#' @examples
#' \dontrun{
#' # use the data set "ToothGrowth" and, if it exists, write it as
#' # jamovi-file using write_omv()
#' jmvReadWrite::ToothGrowth
#' nmeOut <- tempfile(fileext = ".omv")
#' # typically, one would use a "real" file name instead of tempfile(),
#' # e.g., "Data1.omv"
#' dtaDbg = jmvReadWrite::write_omv(dtaFrm = ToothGrowth, fleOut = nmeOut, retDbg = TRUE)
#' print(names(dtaDbg))
#' # the print-function is only used to force devtools::run_examples()
#' # to show output
#' # -> "mtaDta" "xtdDta" "dtaFrm"
#' # returns a list with the metadata (mtaDta, e.g., column and data type),
#' # the extended data (xtdDta, e.g., variable lables), and the data frame
#' # (dtaFrm) the purpose of these variables is merely for checking (under-
#' # standing the file format) and debugging
#'
#' # check whether the file was written to the disk, get the file informa-
#' # tion (size, etc.) and delete the file afterwards
#' print(list.files(dirname(nmeOut), basename(nmeOut)))
#' # -> "file[...].omv" ([...] is a combination of random numbers / characters
#' print(file.info(nmeOut)$size)
#' # -> approx. 2600 (size may differ on different OSes)
#' unlink(nmeOut)
#' }
#'
#' @export write_omv

write_omv <- function(dtaFrm = NULL, fleOut = "", wrtPtB = FALSE, frcWrt = FALSE, retDbg = FALSE) {
    if (is.null(dtaFrm)) stop("The data frame to be written needs to be given as parameter (dtaFrm = ...).")
    if (!nzchar(fleOut)) stop("Output file name needs to be given as parameter (fleOut = ...).")

    # check that the file name isn't empty, that the destination directory exists and that it ends in .omv
    fleOut <- nrmFle(fleOut)
    chkDir(fleOut)
    chkExt(fleOut, c("omv", "omt"))
    fleExs(fleOut, frcWrt)

    # [1] handle the attributes "variable.labels" and "value.labels" in the format provided by the R-package "foreign"
    #     the attribute "variable.labels" (attached to the data frame) is converted them to the format used by jamovi ("jmv-desc" attached to the data column)
    # [2] clean tibble attributes, particularly convert haven_labelled to either factors or numeric / integer
    if (chkAtt(dtaFrm, "variable.labels"))
        dtaFrm <- clnFgn(dtaFrm)
    if (methods::is(dtaFrm, "tbl_df") || any(vapply(dtaFrm, function(C) methods::is(C, "haven_labelled"), logical(1))))
        dtaFrm <- clnTbb(dtaFrm, c("format.sas", "format.spss", "format.stata", "display_width"), jmvLbl = TRUE)
    if (chkAtt(dtaFrm, "label.table"))
        stop("R-foreign-style value labels need to be implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no")

    # check whether dtaFrm is a data frame, remove tibble attributes (if necessary) and attach dataType and measureType attributes
    dtaFrm <- jmvAtt(dtaFrm, blnChC = TRUE)

    # initialize metadata.json
    mtaDta <- mtaGlb
    # use the attributes stored in the data frame (for the whole data set) to
    # update the metadata; jmv-weights-name are stored as weights, jmv-weights
    # needs to be dropped
    if (chkAtt(dtaFrm, "jmv-weights-name")) {
        attr(dtaFrm, "weights") <- attr(dtaFrm, "jmv-weights-name")
        attr(dtaFrm, "jmv-weights-name") <- NULL
        attr(dtaFrm, "jmv-weights")      <- NULL
    }
    mtaDta <- setAtt(names(mtaDta), dtaFrm, mtaDta)
    # the number of rows and columns has to be adjusted to the current data set
    mtaDta$rowCount    <- ifelse(hasExt(fleOut, "omt"), 0, nrow(dtaFrm))
    mtaDta$columnCount <- ncol(dtaFrm)
    # create the entries for storing the column specific information
    mtaDta$fields      <- rep(list(mtaFld), mtaDta$columnCount)

    # initialize xdata.json
    xtdDta <- list()

    # create data.bin
    binHdl <- file(description = file.path(tempdir(), "data.bin"),    open = "wb")

    # create strings.bin
    strHdl <- file(description = file.path(tempdir(), "strings.bin"), open = "wb")
    strPos <- 0

    for (i in seq_along(dtaFrm)) {
        # assign the jamovi-specific-attributes that are stored in data frame for this data column (if available)
        mtaDta$fields[[i]] <- setAtt(names(mtaDta$fields[[i]]), dtaFrm[i], mtaDta$fields[[i]])

        # name
        crrNme <- names(dtaFrm[i])
        mtaDta$fields[[i]][["name"]] <- crrNme

        # variable label: if available, choose "jmv-desc", "label", ...
        # the attributes are concatenated if available (otherwise they will be NULL and dropped), if several are available,
        # the first ("jmv-desc") takes precedence, if all are NULL, the content of mtaDta$fields serves as fallback-option
        mtaDta$fields[[i]][["description"]] <- c(attr(dtaFrm[[i]], "jmv-desc"), mtaDta$fields[[i]][["description"]], attr(dtaFrm[[i]], "label"))[1]

        # remove atrributes that are only used with specific columnTypes
        mtaDta$fields[[i]] <- rmvMta(mtaDta$fields[[i]], dtaFrm[[i]])

        # assign column from the original data frame to crrCol (so that modifications don't affect the original)
        crrCol <- dtaFrm[[i]]

        # factors: store labels / values in xdata.json and convert factors to integer for saving
        if (is.factor(crrCol)) {
            xtdDta[[crrNme]] <- xtdCol(crrCol, crrNme)
            crrCol <- cnvCol(crrCol, "integer")
        }

        # keep value from "type" if it exists, otherwise determine and set "type" based upon whether the variable is an ID,
        # and upon "dataType"
        mtaDta$fields[[i]][["type"]] <- ifelse(chkAtt(crrCol, "type"), attr(crrCol, "type"),
                                               gsub("decimal", "number",
                                               gsub("text", ifelse(isID(crrCol), "string", "integer"), tolower(attr(crrCol, "dataType")))))

        # for debugging: check that dataType, and measureType are set accordingly to type (attribute and column in the data frame)
        #cat(do.call(sprintf, c(fmt = "%02d: %s - %s - %s - %s\n", c(i, mtaDta$fields[[i]][c("name", "type", "dataType", "measureType")]))))

        # write to data.bin according to type
        if (!hasExt(fleOut, "omt")) {
            if        (chkFld(mtaDta$fields[[i]], "type", "integer")) {
                wrtCol <- as.integer(crrCol)
            } else if (chkFld(mtaDta$fields[[i]], "type", "number"))  {
                wrtCol <- as.double(crrCol)
            } else if (chkFld(mtaDta$fields[[i]], "type", "string"))  {
                wrtCol <- cnvStr(crrCol, strHdl, strPos)
                strPos <- attr(wrtCol, "strPos")
                wrtCol <- as.integer(wrtCol)
            }
            writeBin(wrtCol, binHdl, endian = "little")
            # remove temporary variables for writing the current column
            rm(wrtCol)
        }

        # remove temporary variables for modifying the current column from the data set
        rm(crrCol)
    }

    # ensure that the column-ID is not empty(NA) and unique
    mtaDta$fields <- unqID(mtaDta$fields)

    # compress data.bin and discard the temporary file
    add2ZIP(fleOut, crrHdl = binHdl)

    # compress strings.bin (only if it contains data) and discard the temporary file
    add2ZIP(fleOut, crrHdl = strHdl, incZIP = (strPos > 0))
    rm(strPos)

    # create meta, write it and add it to ZIP file
    add2ZIP(fleOut, crrFle = c("meta", "wb"), txtOut = mnfTxt())

    # write metadata.json
    add2ZIP(fleOut, crrFle = "metadata.json", txtOut = fmtJSON(list(dataSet = mtaDta)))

    # write xdata.json
    add2ZIP(fleOut, crrFle = "xdata.json",    txtOut = fmtJSON(xtdDta))

    # if "HTML" attribute exists (e.g., because functions such as describe_omv
    # did modify it), this attribute is written; otherwise, htmTxt (a clean
    # "index.html", i.e., without any results) is written
    if (!is.null(attr(dtaFrm, "HTML"))) {
        add2ZIP(fleOut, crrFle = "index.html", txtOut = attr(dtaFrm, "HTML"))
    } else {
        add2ZIP(fleOut, crrFle = "index.html", txtOut = htmTxt())
    }

    # write ProtoBuffers: if the argument wrtPtB is set and the data set contains no
    # protobuffers, an error message is thrown; for templates, protobuffers are written
    # (if they exist), but their non-existence doesn't result in an error message
    if ((wrtPtB || hasExt(fleOut, "omt")) && hasPtB(dtaFrm)) {
        add2ZIP(fleOut, crrFle = c(names(attr(dtaFrm, "protobuf")[1]), "wb"), ptbOut = attr(dtaFrm, "protobuf")[[1]])
    } else if (wrtPtB && !hasPtB(dtaFrm)) {
        unlink(fleOut)
        stop("The data frame (dtaFrm) must contain the attribute \"protobuf\", there has to be at least one of them, and it has to be of the correct type (a RProtoBuf).")
    }

    # handle weights
    if (chkAtt(dtaFrm, "weights", "\\w+")) {
        # TO-DO: this likely requires creating protobuffers
        warning("Handling of weights not yet implemented.")
    }

    if (retDbg) {
        list(mtaDta = mtaDta, xtdDta = xtdDta, dtaFrm = dtaFrm)
    }
}

jmvAtt <- function(dtaFrm = NULL, blnChC = FALSE) {
    chkDtF(dtaFrm)

    for (i in seq_along(dtaFrm)) {
        crrNme <- names(dtaFrm)[i]
        # (a) jmv-id
        # ID variables represent a special case and are therefore treated first
        # if the jmv-id marker is set or if the measureType is set to "ID" in the original data, or if it is the first column that hasn't the attribute
        # measureType attached, has values that are unique and not NA, is either a factor or an integer (rounded equals the original value) and has a
        # name that (as lower-case) matches "id", "name" or "subject"
        if (isID(dtaFrm[[i]], i, crrNme)) {
            attr(dtaFrm[[i]], "jmv-id")      <- TRUE
            attr(dtaFrm[[i]], "measureType") <- "ID"
            attr(dtaFrm[[i]], "dataType")    <- "Text"
            if (blnChC) dtaFrm[[i]] <- cnvCol(dtaFrm[[i]], "character")
        # (b) date and time - jamovi doesn't support it natively, thus the transformation to integer;
        # back-transformation in R: as.Date(..., origin = "1970-01-01") / hms::as_hms(...)
        # NB: must come before the numerical variables since date values are an integer since R 4.5
        } else if (isDnT(dtaFrm[[i]])) {
            attr(dtaFrm[[i]], "measureType") <- "Continuous"
            attr(dtaFrm[[i]], "dataType")    <- "Integer"
            if (blnChC) {
                # assign the description before the conversion (description depends on the original type)
                attr(dtaFrm[[i]], "description") <- dscDnT(dtaFrm[[i]], crrNme)
                dtaFrm[[i]] <- cnvCol(dtaFrm[[i]], "integer")
            }
        # (c) numerical variables, determine first whether the variable can be integer, if not, use / keep it numeric / float
        } else if (is.numeric(dtaFrm[[i]])) {
            attr(dtaFrm[[i]], "measureType") <- "Continuous"
            attr(dtaFrm[[i]], "dataType")    <- ifelse(is.integer(dtaFrm[[i]]) || detInt(dtaFrm[[i]]), "Integer", "Decimal")
        # (d) factors
        } else if (is.factor(dtaFrm[[i]])) {
            attr(dtaFrm[[i]], "measureType") <- ifelse(is.ordered(dtaFrm[[i]]), "Ordinal", "Nominal")
            attr(dtaFrm[[i]], "dataType")    <- ifelse(!is.null(attr(dtaFrm[[i]], "values")) || intFnC(dtaFrm[[i]]), "Integer", "Text")
        # (e) logical and character are converted to factor (if blnChC)
        } else if (is.logical(dtaFrm[[i]]) || is.character(dtaFrm[[i]])) {
            attr(dtaFrm[[i]], "measureType") <- "Nominal"
            attr(dtaFrm[[i]], "dataType")    <- ifelse(is.logical(dtaFrm[[i]]) || intFnC(dtaFrm[[i]]), "Integer", "Text")
            if (blnChC) dtaFrm[[i]] <- cnvCol(dtaFrm[[i]], "factor")
        # variable type is not implemented
        } else {
            clsRmv()
            stop(sprintf("\n\n%s: Variable type %s not implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no",
              names(dtaFrm)[i], class(dtaFrm[[i]])))
        }

    }

    dtaFrm
}

# determine whether a column is (i.e., can become) integer without loosing data
detInt <- function(crrCol = NULL) {
    is.numeric(crrCol) && !all(is.na(crrCol)) &&
    max(abs(crrCol), na.rm = TRUE) <= .Machine$integer.max &&
    all(abs(crrCol - round(crrCol)) < sqrt(.Machine$double.eps), na.rm = TRUE)
}

# determine whether a column likely should get the measureType "Continuous", requiring
# that there are enough different values and a high value range and variability (sd)
#detCnt <- function(crrCol = NULL) {
#    length(unique(crrCol)) > length(crrCol) / 5 &&
#    length(unique(crrCol)) > diff(range(crrCol, na.rm = TRUE)) / 5 &&
#    stats::sd(crrCol, na.rm = TRUE) > diff(range(crrCol, na.rm = TRUE)) / 10
#}

isDnT  <- function(crrCol) {
    methods::is(crrCol, "Date") || methods::is(crrCol, "POSIXct") || methods::is(crrCol, "POSIXlt") || methods::is(crrCol, "difftime")
}

dscDnT <- function(crrCol = NULL, crrNme = "") {
    paste(ifelse(chkAtt(crrCol, "description"), attr(crrCol, "description"), crrNme),
          ifelse(methods::is(crrCol, "difftime"), "(time converted to integer; sec since 00:00)", "(date converted to integer; days since 1970-01-01)"))
}

xtdCol <- function(crrCol = NULL, crrNme = "") {
    crrLvl <- levels(crrCol)
    # ensure that the "values" attribute is correct
    if (chkAtt(crrCol, "values") && all(grepl("^\\d+$", crrLvl)) &&
      !identical(attr(crrCol, "values"), as.integer(attr(crrCol, "levels")))) {
        clsRmv()
        stop(sprintf(paste("\"values\"-attribute with unexpected values found for column \"%s\".",
                           "Please send the file to sebastian.jentschke@uib.no for debugging."), crrNme))
    }

    # columns that have previously been logical or where all factor levels can be converted to integer
    if        (chkAtt(crrCol, "dataType", "Integer") && identical(crrLvl, c("FALSE", "TRUE"))) {
        return(list(labels = lapply(c(TRUE, FALSE),    function(l) list(as.integer(l), as.character(l), as.character(l), FALSE))))
    } else if (chkAtt(crrCol, "dataType", "Integer") && intFnC(crrLvl)) {
        return(list(labels = lapply(crrLvl,            function(l) list(as.integer(l),              l,               l,  FALSE))))
    # columns that           or where not all factor levels can be converted to integer
    } else if (chkAtt(crrCol, "dataType", "Text")) {
        return(list(labels = lapply(seq_along(crrLvl), function(l) list(l - 1,               crrLvl[l],       crrLvl[l], FALSE))))
    }
}

rmvMta <- function(crrFld = NULL, dtaCol = NULL) {
    if (chkFld(crrFld, "columnType", "Filter")) {
        # if the variable is a filter, trimLevels is removed in any case
        crrFld[["trimLevels"]]               <- NULL
    } else {
        # if the variable isn't a filter, trimLevels is only removed
        # if the original variable was a factor / logical
        if (all(class(dtaCol) != c("logical", "factor"))) {
            crrFld[["trimLevels"]]           <- NULL
        }
        crrFld[["filterNo"]]                 <- NULL
        crrFld[["active"]]                   <- NULL
    }
    if (!chkFld(crrFld, "columnType", "Output")) {
        crrFld[["outputAnalysisId"]]         <- NULL
        crrFld[["outputOptionName"]]         <- NULL
        crrFld[["outputName"]]               <- NULL
        crrFld[["outputDesiredColumnName"]]  <- NULL
        crrFld[["outputAssignedColumnName"]] <- NULL
    }

    crrFld
}

cnvStr <- function(crrCol = NULL, strHdl = NULL, strPos = NA) {
    crrCol[is.na(crrCol)] <- ""
    wrtCol <- rep(NA, length(crrCol))
    for (j in seq_along(crrCol)) {
        if (crrCol[j] == "") next
        writeBin(crrCol[j], strHdl)
        wrtCol[j] <- strPos
        strPos <- strPos + length(charToRaw(crrCol[j])) + 1
    }

    attr(wrtCol, "strPos") <- strPos
    wrtCol
}

unqID <- function(mtaFld = NULL) {
    id_Lst <- unlist(lapply(seq_along(mtaFld), function(i) mtaFld[[i]][["id"]]))
    if (any(is.na(id_Lst)) || any(duplicated(id_Lst))) {
        for (i in seq_along(mtaFld)) mtaFld[[i]][["id"]] <- i
    }

    mtaFld
}

fleExs <- function(fleOut = c(), frcWrt = FALSE) {
    if (file.exists(fleOut)) {
        if (frcWrt) {
            unlink(fleOut)
        } else {
            stop(sprintf("The output file %s already exists. Either remove the file or set the parameter frcWrt to TRUE.", basename(fleOut)))
        }
    }
}

isID <- function(crrCol = NULL, i = NA, crrNme = "") {
    chkAtt(crrCol, "jmv-id", TRUE) || chkAtt(crrCol, "measureType", "ID") ||
           (i == 1 && !chkAtt(crrCol, "jmv-id") && !chkAtt(crrCol, "measureType") && !any(duplicated(crrCol) | is.na(crrCol)) &&
           (is.character(crrCol) || is.factor(crrCol) || (is.numeric(crrCol) && all(crrCol %% 1 == 0))) &&
           (tolower(crrNme) %in% c("id", "name", "subject")))
}

hasPtB <- function(dtaFrm = NULL) {
    chkAtt(dtaFrm, "protobuf") && inherits(attr(dtaFrm, "protobuf")[[1]], "Message")
}

# convert to JSON and do some beatifying (adding spaces for increased legibility)
fmtJSON <- function(txtJSON = "") {
    gsub("\"weights\": \\{\\}", "\"weights\": null", gsub("00: 00", "00:00", gsub("  ", " ", gsub(":", ": ", gsub(",", ", ",
      jsonlite::toJSON(txtJSON, auto_unbox = TRUE))))))
}

# generates an “empty” index.html (i.e., an index.html not containing any results output)
htmTxt <- function() {
    c("<!DOCTYPE html>",
      "<html>",
      "<head>",
      "    <meta charset=\"utf-8\" />",
      "    <title>Results</title>",
      "    <style>\n",
      "    body {",
      "        font-family: \"Segoe UI\",Roboto,Helvetica,Arial,sans-serif,\"Segoe UI Emoji\",\"Segoe UI Symbol\" ;",
      "        color: #333333 ;",
      "        cursor: default ;",
      "        margin: 24px;",
      "        font-size: 12px ;",
      "    }\n",
      "    h1 {",
      "        font-size: 160% ;",
      "        color: #3E6DA9 ;",
      "        margin-bottom: 12px ;",
      "        white-space: nowrap ;",
      "    }\n",
      "    h2 {",
      "        font-size: 130% ;",
      "        margin-bottom: 12px ;",
      "        color: #3E6DA9 ;",
      "    }\n",
      "    h3, h4, h5 {",
      "        font-size: 110% ;",
      "        margin-bottom: 12px ;",
      "    }\n",
      "    table {",
      "        border-spacing: 0 ;",
      "        page-break-inside: avoid;",
      "    }\n",
      "    table tr td, table tr th {",
      "        page-break-inside: avoid;",
      "        font-size: 12px ;",
      "    }\n",
      "    .ql-align-center {\n        text-align: center;\n    }\n",
      "    .ql-align-right {\n        text-align: right;\n    }\n",
      "    .ql-align-justify {\n        text-align: justify;\n    }\n",
      "    .ql-indent-1 {\n        padding-left: 3em;\n    }\n",
      "    .ql-indent-2 {\n        padding-left: 6em;\n    }\n",
      "    .ql-indent-3 {\n        padding-left: 9em;\n    }\n",
      "    .ql-indent-4 {\n        padding-left: 12em;\n    }\n",
      "    .ql-indent-5 {\n        padding-left: 15em;\n    }\n",
      "    .note {\n        margin: 5px 0px;\n    }\n",
      "    </style>",
      "</head>",
      "<body>\n",
      "    <h1 contenteditable=\"\" spellcheck=\"false\">Results</h1>\n",
      "    <p style=\"text-align:left;padding:0px 0px 0px 0px;\"></p>\n",
      "</body>",
      "</html>")
}

# creates the text output for the manifest file
mnfTxt <- function() {
    crrTxt <- paste0(vapply(lstMnf, "[[", character(1), 1), ":")
    for (i in seq_along(lstMnf)) {
        if (crrTxt[i] == "Created-By:") {
            crrTxt[i] <- paste(crrTxt[i], "jmvReadWrite", utils::packageVersion("jmvReadWrite"))
        } else {
            crrTxt[i] <- paste(crrTxt[i], paste(lstMnf[[i]][2:length(lstMnf[[i]])], collapse = "."))
        }
    }
    enc2utf8(crrTxt)
}

# adds an input file to the .omv-file (which is a ZIP-archive)
add2ZIP <- function(fleZIP = "", crrHdl = NULL, crrFle = c(), txtOut = "", ptbOut = NULL, incZIP = TRUE) {
    if ((!is.character(fleZIP) || length(fleZIP) < 1 || !nzchar(fleZIP)) ||
        ((is.null(crrHdl) || length(crrHdl) < 1) && (!is.character(crrFle) || length(crrFle) < 1 || !all(nzchar(crrFle))))) {
        clsRmv()
        stop("fleZIP (a character with a file name), and either crrHdl (with a connection) or crrFle (with a file name and [optionally] a writing mode) need to be given as arguments.")
    }

    # if a file handle is given, determine the file name, close the handle and remove it (from the calling environment)
    if (!is.null(crrHdl)) {
        if (!all(class(crrHdl) == c("file", "connection"))) {
            clsRmv()
            stop(sprintf("Parameter isn\'t a file handle pointing to a file to be zipped:\n%s", trimws(utils::capture.output(utils::str(crrHdl)))))
        }
        crrFle <- rmvTmp(summary(crrHdl)[["description"]])
        close(crrHdl)
        rm(list = deparse(substitute(crrHdl)), envir = sys.frame(-1))
    # if a file name is given, open a connection, write whatever is required
    } else if (is.character(crrFle)) {
        crrFle[1] <- rmvTmp(crrFle[1])
        crtDir(crrFle)
        crrHdl <- file(file.path(tempdir(), crrFle[1]), open = ifelse(length(crrFle) > 1, crrFle[2], "w"))
        if        (all(nzchar(txtOut))) {
            # writing the manifest requires a bit of special handling because of \r\n on Windows vs. \n on Mac / Linux
            # for the other files (metadata.json, xdata.json, index.html), it doesn't matter, therefore sep = "\n" is added for all
            writeLines(txtOut, crrHdl, sep = "\n")
        } else if (!is.null(ptbOut)) {
            ptbOut$serialize(crrHdl)
        }
        close(crrHdl)
    }

    if (incZIP) {
        crrPrm <- list(zipfile = fleZIP, files = crrFle[1], root = tempdir())
        if (file.exists(fleZIP)) do.call(zip::zip_append, crrPrm) else do.call(zip::zip, crrPrm)
    }

    delTmp(crrFle)
    rm(crrFle, crrHdl)

    return(TRUE)
}

crtDir <- function(crrFle = c()) {
    if (dirname(crrFle[1]) != "." && !dir.exists(file.path(tempdir(), dirname(crrFle[1])))) dir.create(file.path(tempdir(), dirname(crrFle[1])))

    return(invisible(NULL))
}

delTmp <- function(crrFle = c()) {
    if (dirname(crrFle[1]) == ".") unlink(file.path(tempdir(), crrFle[1])) else unlink(file.path(tempdir(), dirname(crrFle[1])), recursive = TRUE)

    return(invisible(NULL))
}

rmvTmp <- function(fleNme = "") {
    return(sub("^/|^\\\\", "", sub(tempdir(), "", fleNme, fixed = TRUE)))
}
