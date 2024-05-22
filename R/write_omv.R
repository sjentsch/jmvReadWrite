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
    chkExt(fleOut, "omv")
    fleExs(fleOut, frcWrt)

    # check whether dtaFrm is a data frame
    # attach dataType and measureType attributes when inside jamovi
    chkDtF(dtaFrm)
    if (isJmv()) dtaFrm <- jmvAtt(dtaFrm)

    # handle the attributes "variable.labels" and "value.labels" in the format provided bymtaDta <- mtaGlb the R-package "foreign"
    # the attribute "variable.labels" (attached to the data frame) is converted them to the format used by jamovi ("jmv-desc" attached to the data column)
    if (chkAtt(dtaFrm, "variable.labels")) dtaFrm <- fgnLbl(dtaFrm)
    if (chkAtt(dtaFrm, "label.table")) stop("R-foreign-style value labels need to be implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no")

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
    mtaDta$rowCount    <- nrow(dtaFrm)
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

        # assign column from the original data frame to crrCol (so that modifications don't affect the original)
        crrCol <- dtaFrm[[i]]

        # ID variables represent a special case and are therefore treated first
        # if the jmv-id marker is set or if the measureType is set to "ID" in the original data, or if it is the first column that hasn't the attribute
        # measureType attached, has values that are unique and not NA and is either a factor or an integer (rounded equals the original value)
        if (isID(crrCol, i)) {
            mtaDta$fields[[i]][["measureType"]] <- "ID"
            mtaDta$fields[[i]][["dataType"]]    <- ifelse(chkAtt(dtaFrm[[i]], "dataType"), attr(dtaFrm[[i]], "dataType"), ifelse(is.numeric(crrCol), "Integer", "Text"))
            mtaDta$fields[[i]][["type"]]        <- ifelse(is.numeric(crrCol), "integer", "string")
        # afterwards, the different variable types for each column of the original data frame are tested
        # an overview about how jamovi treats variable types internally and as which types they are written
        # can be found in the function jmvAtt under globals.R
        # [a] logical
        } else if (is.logical(crrCol)) {
            crrCol <- as.integer(crrCol)
            mtaDta$fields[[i]][["dataType"]]    <- ifelse(chkAtt(dtaFrm[[i]], "dataType"), attr(dtaFrm[[i]], "dataType"), "Integer")
            mtaDta$fields[[i]][["type"]]        <- "integer"
            # measureType not set as the correct type ("Nominal") is already the default
            xtdDta[[crrNme]] <- list(labels = lapply(0:1, function(i) list(i, as.character(i), as.character(i), FALSE)))
            # NB: If jamovi imports RData / RDS-files, logical variables, labels are assigned as (0, "FALSE", "0", FALSE) and (1, "TRUE", "1", false)
            #     however, using "0" and "1" instead of "FALSE" and "TRUE" seems to make more sense
        # [b] factors or characters / strings
        } else if (is.factor(crrCol) || is.character(crrCol)) {
            crrCol <- prcFnC(crrCol, mtaDta$fields[[i]], dtaFrm[[i]], crrNme)
            mtaDta$fields[[i]] <- attr(crrCol, "crrFld")
            xtdDta[[crrNme]]   <- attr(crrCol, "crrLbl")
        # [c] numerical (integer / decimals)
        } else if (is.numeric(crrCol)) {
            crrCol <- prcNum(crrCol, mtaDta$fields[[i]])
            mtaDta$fields[[i]] <- attr(crrCol, "crrFld")
        # [d] dates / times - jamovi actually doesn't support it but i perhaps makes most sense to implement it as numeric
        # can be transformed back in R using - as.Date(..., origin = "1970-01-01") and hms::as_hms(...)
        } else if (inherits(crrCol, c("Date", "POSIXt"))) {
            crrCol <- as.numeric(crrCol)
            mtaDta$fields[[i]][["type"]]        <- "number"
            mtaDta$fields[[i]][["dataType"]]    <- "Decimal"
            mtaDta$fields[[i]][["measureType"]] <- "Continuous"
            mtaDta$fields[[i]][["description"]] <- paste(c(ifelse(nzchar(mtaDta$fields[[i]][["description"]]), mtaDta$fields[[i]][["description"]], crrNme),
                                                         "(date converted to numeric; days since 1970-01-01)"), collapse = " ")
        } else if (inherits(crrCol, c("difftime"))) {
            crrCol <- as.numeric(crrCol)
            mtaDta$fields[[i]][["type"]]        <- "number"
            mtaDta$fields[[i]][["dataType"]]    <- "Decimal"
            mtaDta$fields[[i]][["measureType"]] <- "Continuous"
            mtaDta$fields[[i]][["description"]] <- paste(c(ifelse(nzchar(mtaDta$fields[[i]][["description"]]), mtaDta$fields[[i]][["description"]], crrNme),
                                                         "(time converted to numeric; sec since 00:00)"), collapse = " ")
        } else {
            clsRmv()
            stop(sprintf("Variable type %s not implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no", class(crrCol)))
        }

        # remove atrributes that are only used with specific columnTypes
        mtaDta$fields[[i]] <- rmvMta(mtaDta$fields[[i]], dtaFrm[[i]])

        # check that dataType, and measureType are set accordingly to type (attribute and column in the data frame)
        # dataType: Text, Integer, Decimal
        # cat(sprintf("%02d: %s - %s - %s - %s\n", i, mtaDta$fields[[i]][["name"]], mtaDta$fields[[i]][["type"]], mtaDta$fields[[i]][["dataType"]], mtaDta$fields[[i]][["measureType"]]))

        # write to data.bin according to type
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

        # remove temporary variables for modifying and storing the current column from the data set
        rm(crrCol, wrtCol)
    }

    # double check whether ID is unique
    if (unqID(mtaDta$fields)) {
        for (i in seq_along(mtaDta$fields)) mtaDta$fields[[i]][["id"]] <- i
    }

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

    # write ProtoBuffers
    if (wrtPtB && chkPtB(dtaFrm, fleOut)) {
        add2ZIP(fleOut, crrFle = c(names(attr(dtaFrm, "protobuf")[1]), "wb"), ptbOut = attr(dtaFrm, "protobuf")[[1]])
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

prcFnC <- function(crrCol = NULL, crrFld = NULL, dtaCol = NULL, crrNme = c()) {
    if (is.character(crrCol)) {
        # if "dataType" is already stored in the data frame, keep it, otherwise determine whether the factor levels are more likely to be "Integer" or "Text"
        crrFld[["dataType"]] <- ifelse(chkAtt(dtaCol, "dataType"), attr(dtaCol, "dataType"),
                                              ifelse(!any(is.na(suppressWarnings(as.numeric(crrCol)))), "Integer", "Text"))
        crrCol <- factor(trimws(crrCol), eval(parse(text = ifelse(crrFld[["dataType"]] == "Integer",
                                                                  "as.character(sort(as.numeric(unique(trimws(crrCol)))))",
                                                                  "sort(unique(trimws(crrCol)))"))))
    }
    if (chkAtt(dtaCol, "values") && !identical(attr(dtaCol, "values"), as.integer(attr(dtaCol, "levels")))) {
        clsRmv()
        stop(sprintf(paste("\"values\"-attribute with unexpected values found for column \"%s\".",
                           "Please send the file to sebastian.jentschke@uib.no for debugging."), crrNme))
    }
    # NB: If jamovi imports RData / RDS-files, character variables are given "ID" (measureType) / "Text" (dataType)
    #     however, converting them to factors and exporting those seems to make more sense
    facLvl <- attr(crrCol, "levels")
    # above must be kept at crrCol as the original column might be character and was converted above
    facOrd <- is.ordered(dtaCol)
    crrLbl <- NULL
    if (chkAtt(dtaCol, "values")) {
        crrCol <- attr(dtaCol, "values")[as.vector.factor(crrCol, mode = "integer")]
        if (length(facLvl) > 0) {
            crrLbl <- list(labels = lapply(seq_along(facLvl), function(j) list(attr(dtaCol, "values")[j], facLvl[[j]], facLvl[[j]], FALSE)))
        }
    } else {
        crrCol <- as.vector.factor(crrCol, mode = "integer") - 1
        if (length(facLvl) > 0) {
            crrLbl <- list(labels = lapply(seq_along(facLvl), function(j) list(j - 1,                     facLvl[[j]], facLvl[[j]], FALSE)))
        }
    }
    # if "dataType" is already stored in the data frame, keep it, otherwise determine whether the factor levels are more likely to be "Integer" or "Text"
    crrFld[["dataType"]] <- ifelse(chkAtt(dtaCol, "dataType"), attr(dtaCol, "dataType"),
        ifelse(chkAtt(dtaCol, "values"), "Integer",
        ifelse(all(!is.na(suppressWarnings(as.integer(facLvl)))) && all(as.character(as.integer(facLvl)) == facLvl), "Integer", "Text")))
    crrFld[["type"]]     <- "integer"
    # if "measureType" is already stored in the data frame, keep it, otherwise set it to "Ordinal" if the properties indicate it to be likely
    # ("Nominal" is already the default)
    if (facOrd) crrFld[["measureType"]] <- ifelse(chkAtt(dtaCol, "measureType"), attr(dtaCol, "measureType"), "Ordinal")
    # the code below permitted to "guess" whether a factor likely was ordered, but this lead to some problems when storing reshaped data
#           if (facOrd || (chkFld(crrFld, "dataType", "Integer") && length(facLvl) > 5 && !any(is.na(c(as.integer(facLvl), crrCol))) &&
#                          stats::sd(diff(as.integer(facLvl))) < diff(range(crrCol)) / 10)) {
#               crrFld[["measureType"]] <- ifelse(chkAtt(dtaCol, "measureType"), attr(dtaCol, "measureType"), "Ordinal")
#           }

     attr(crrCol, "crrFld") <- crrFld
     attr(crrCol, "crrLbl") <- crrLbl

     crrCol
}

prcNum <- function(crrCol = NULL, crrFld = NULL) {
    # determine type (how values are stored) and dataType (used by jamovi ~ R: Decimal ~ numeric, Integer ~ integer)
    if        (chkAtt(crrCol, "dataType")) {
        crrFld[["type"]]     <- gsub("decimal", "number", tolower(attr(crrCol, "dataType")))
        crrFld[["dataType"]] <- attr(crrCol, "dataType")
    } else if (detInt(crrCol)) {
        crrFld[["type"]]     <- "integer"
        crrFld[["dataType"]] <- "Integer"
    } else {
        crrFld[["type"]]     <- "number"
        crrFld[["dataType"]] <- "Decimal"
    }
    # if "measureType" is already stored in the data frame, keep it; otherwise assign "Continuous" if the dataType is Decimal" or
    crrFld[["measureType"]] <- ifelse(chkAtt(crrCol, "measureType"), attr(crrCol, "measureType"),
                                        ifelse(crrFld[["dataType"]] == "Decimal" || detCnt(crrCol), "Continuous", crrFld[["measureType"]]))

    attr(crrCol, "crrFld") <- crrFld
    crrCol
}

# determine whether a column is (i.e., can become) integer without loosing data
detInt <- function(crrCol = NULL) {
    !all(is.na(crrCol)) &&
    max(abs(crrCol), na.rm = TRUE) <= .Machine$integer.max &&
    all(abs(crrCol - round(crrCol)) < sqrt(.Machine$double.eps), na.rm = TRUE)
}

# determine whether a column likely should get the measureType "Continuous", requiring
# that there are enough different values and a high value range and variability (sd)
detCnt <- function(crrCol = NULL) {
    length(unique(crrCol)) > length(crrCol) / 5 &&
    length(unique(crrCol)) > diff(range(crrCol, na.rm = TRUE)) / 5 &&
    stats::sd(crrCol, na.rm = TRUE) > diff(range(crrCol, na.rm = TRUE)) / 10
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
    if (!is.character(crrCol)) crrCol <- as.character(crrCol)
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

unqID <- function(allFld = NULL) {
    id_Lst <- unlist(lapply(seq_along(allFld), function(i) allFld[[i]][["id"]]))
    any(is.na(id_Lst)) || any(duplicated(id_Lst))
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

isID <- function(crrCol = NULL, i = NA) {
    chkAtt(crrCol, "jmv-id", TRUE) || chkAtt(crrCol, "measureType", "ID") ||
           (i == 1 && !chkAtt(crrCol, "measureType") && !any(duplicated(crrCol) | is.na(crrCol)) &&
           (is.character(crrCol) || is.factor(crrCol) || (is.numeric(crrCol) && all(crrCol %% 1 == 0))))
}

chkPtB <- function(dtaFrm = NULL, fleOut = c()) {
    if (is.null(attr(dtaFrm, "protobuf")) || length(attr(dtaFrm, "protobuf")) < 1 || !inherits(attr(dtaFrm, "protobuf")[[1]], "Message")) {
        unlink(fleOut)
        stop("The data frame (dtaFrm) must contain the attribute \"protobuf\", there has to be at least one of them, and it has to be of the correct type (a RProtoBuf).")
    }

    TRUE
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
        if (dirname(crrFle[1]) != "." && !dir.exists(file.path(tempdir(), dirname(crrFle[1])))) dir.create(file.path(tempdir(), dirname(crrFle[1])))
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

    if (dirname(crrFle[1]) == ".") unlink(file.path(tempdir(), crrFle[1])) else unlink(file.path(tempdir(), dirname(crrFle[1])), recursive = TRUE)
    rm(crrFle, crrHdl)

    return(TRUE)
}

rmvTmp <- function(fleNme = "") {
    return(sub("^/|^\\\\", "", sub(tempdir(), "", fleNme, fixed = TRUE)))
}
