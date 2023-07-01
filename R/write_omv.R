#' Write files to be used with the statistical spreadsheet 'jamovi'
#' (<https://www.jamovi.org>)
#'
#' @param dtaFrm Data frame to be exported (default: NULL)
#' @param fleOut Name / position of the output file to be generated ("FILENAME.omv"; default: "")
#' @param retDbg Whether to return a list with debugging information (see Value; default: FALSE)
#'
#' @return a list (if retDbg == TRUE), containing the meta data (mtaDta, metadata.json in the OMV-file), the extended data (xtdDta, xdata.json in the OMV-file) and the original data frame (dtaFrm)
#'
#' @details
#' jamovi has a specific measurement level / type "ID" (in addition to the "standard" ones "Nominal", "Ordinal", and "Continuous"). "ID" is used for columns that contain some form of ID (e.g., a
#' participant code). In order to set a variable of your data frame to "ID", you have to manually set an attribute \code{jmv-id} (e.g., \code{attr(dtaFrm$column, "jmv-id") = TRUE}).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite)
#'
#' # use the data set "ToothGrowth" and, if it exists, write it as
#' # jamovi-file using write_omv()
#' data("ToothGrowth")
#' fleOMV <- paste0(tempfile(), ".omv")
#' # typically, one would use a "real" file name instead of tempfile(),
#' # e.g., "Data1.omv"
#' dtaDbg = write_omv(ToothGrowth, fleOMV, retDbg = TRUE)
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
#' print(list.files(dirname(fleOMV), basename(fleOMV)))
#' # -> "file[...].omv" ([...] is a combination of random numbers / characters
#' print(file.info(fleOMV)$size)
#' # -> approx. 2500 (size may differ on different OSes)
#' unlink(fleOMV)
#' }
#'
#' @export write_omv

write_omv <- function(dtaFrm = NULL, fleOut = "", retDbg = FALSE) {
    if (is.null(dtaFrm))  stop("The data frame to be written needs to be given as parameter (dtaFrm = ...).")
    if (!nzchar(fleOut)) stop("Output file name needs to be given as parameter (fleOut = ...).")

    # check that the file name isn't empty, that the destination directory exists and that it ends in .omv
    fleOut <- nrmFle(fleOut)
    chkDir(fleOut)
    chkExt(fleOut, "omv")

    # check whether dtaFrm is a data frame
    chkDtF(dtaFrm)

    # handle the attributes "variable.labels" and "value.labels" in the format provided by the R-package "foreign"
    # the attribute "variable.labels" (attached to the data frame) is converted them to the format used by jamovi ("jmv-desc" attached to the data column)
    if (chkAtt(dtaFrm, "variable.labels")) dtaFrm <- fgnLbl(dtaFrm)
    if (chkAtt(dtaFrm, "label.table")) stop("R-foreign-style value labels need to be implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no")

    # initialize metadata.json
    mtaDta <- mtaGlb
    # use the attributes stored in the data frame (for the whole data set) to
    # update the metadata
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
        mtaDta$fields[[i]] <- setAtt(names(mtaDta$fields[[i]]), dtaFrm[[i]], mtaDta$fields[[i]])

        # name
        mtaDta$fields[[i]][["name"]] <- names(dtaFrm[i])

        # variable label: if available, choose "jmv-desc", "label", ...
        # the attributes are concatenated if available (otherwise they will be NULL and dropped), if several are available,
        # the first ("jmv-desc") takes precedence, if all are NULL, the content of mtaDta$fields serves as fallback-option
        mtaDta$fields[[i]][["description"]] <- c(attr(dtaFrm[[i]], "jmv-desc"), mtaDta$fields[[i]][["description"]], attr(dtaFrm[[i]], "label"))[1]

        # assign column from the original data frame to crrCol (so that modifications don't affect the original)
        crrCol <- dtaFrm[[i]]

        # ID variables represent a special case and are therefore treated first
        # if the jmv-id marker is set or if the measureType is set to "ID" in the original data or if it is the first column with the values
        # being unique and being either a factor or an integer (rounded equals the original value; integers may be stored as doubles)
        if (chkAtt(dtaFrm[[i]], "jmv-id", TRUE) || chkAtt(dtaFrm[[i]], "measureType", "ID") ||
            (i == 1 && length(unique(crrCol)) == length(crrCol)) && (is.factor(crrCol) || (is.double(crrCol) && all(crrCol %% 1 == 0)))) {
            if (!is.character(crrCol)) crrCol <- as.character(crrCol)
            mtaDta$fields[[i]][["dataType"]]    <- "Text"
            mtaDta$fields[[i]][["type"]]        <- "string"
            mtaDta$fields[[i]][["measureType"]] <- "ID"
        # afterwards, the different variable types for each column of the original data frame are tested
        # [a] logical
        } else if (is.logical(crrCol)) {
            crrCol <- as.integer(crrCol)
            mtaDta$fields[[i]][["dataType"]]    <- "Integer"
            mtaDta$fields[[i]][["type"]]        <- "integer"
            # measureType not set as the correct type ("Nominal") is already the default
            xtdDta[[names(dtaFrm[i])]] <- list(labels = lapply(0:1, function(i) list(i, as.character(i), as.character(i), FALSE)))
            # NB: If jamovi imports RData / RDS-files, logical variables, labels are assigned as (0, "FALSE", "0", FALSE) and (1, "TRUE", "1", false)
            #     however, using "0" and "1" instead of "FALSE" and "TRUE" seems to make more sense
        # [b] factors or characters / strings
        } else if (is.factor(crrCol) || is.character(crrCol)) {
            if (is.character(crrCol)) {
                crrCol <- factor(trimws(crrCol), eval(parse(text = ifelse(!any(is.na(suppressWarnings(as.numeric(crrCol)))),
                                                                          "as.character(sort(as.numeric(unique(trimws(crrCol)))))",
                                                                          "sort(unique(trimws(crrCol)))"))))
            }
            # NB: If jamovi imports RData / RDS-files, character variables are given "ID" (measureType) / "Text" (dataType)
            #     however, converting them to factors and exporting those seems to make more sense
            facLvl <- attr(crrCol, "levels")
            facOrd <- is.ordered(crrCol)
            if (!is.null(attr(crrCol, "values")) && !identical(attr(crrCol, "values")[as.integer(crrCol)], as.integer(crrCol))) {
                stop(sprintf("\"values\"-attribute found for column \"%s\". Please send the file to sebastian.jentschke@uib.no for debugging.", names(dtaFrm[i])))
#               crrCol <- facVal[as.integer(crrCol)]
#               mtaDta$fields[[i]][["dataType"]] <- "Integer"
            }
            crrCol <- as.integer(crrCol) - 1
            # if "dataType" is already stored in the data frame, keep it, otherwise determine whether the factor levels are more likely to be "Integer" or "Text"
            mtaDta$fields[[i]][["dataType"]] <- ifelse(chkAtt(dtaFrm[[i]], "dataType"), attr(dtaFrm[[i]], "dataType"),
                ifelse(all(!is.na(suppressWarnings(as.integer(facLvl)))) && all(as.character(as.integer(facLvl)) == facLvl), "Integer", "Text"))
            mtaDta$fields[[i]][["type"]]     <- "integer"
            # if "measureType" is already stored in the data frame, keep it, otherwise set it to "Ordinal" if the properties indicate it to be likely ("Nominal" is already the default)
            if (facOrd) mtaDta$fields[[i]][["measureType"]] <- ifelse(chkAtt(dtaFrm[[i]], "measureType"), attr(dtaFrm[[i]], "measureType"), "Ordinal")
            # the code below permitted to "guess" whether a factor likely was ordered, but this lead to some problems when storing reshaped data
#           if (facOrd || (chkFld(mtaDta$fields[[i]], "dataType", "Integer") && length(facLvl) > 5 && !any(is.na(c(as.integer(facLvl), crrCol))) &&
#                          stats::sd(diff(as.integer(facLvl))) < diff(range(crrCol)) / 10)) {
#               mtaDta$fields[[i]][["measureType"]] <- ifelse(chkAtt(dtaFrm[[i]], "measureType"), attr(dtaFrm[[i]], "measureType"), "Ordinal")
#           }
            if (length(facLvl) > 0) {
                xtdDta[[names(dtaFrm[i])]] <- list(labels = lapply(seq_along(facLvl), function(i) list(i - 1, facLvl[[i]], facLvl[[i]], FALSE)))
            }
            rm(facLvl, facOrd)
        # [c] numerical (integer / decimals)
        } else if (is.numeric(crrCol)) {
            if (! all(is.na(crrCol)) && max(abs(crrCol), na.rm = TRUE) <= .Machine$integer.max && all(abs(crrCol - round(crrCol)) < sqrt(.Machine$double.eps), na.rm = TRUE)) {
                crrCol <- as.integer(crrCol)
                mtaDta$fields[[i]][["type"]]     <- "integer"
                mtaDta$fields[[i]][["dataType"]] <- "Integer"
                # if "measureType" is already stored in the data frame, keep it, otherwise assign "Continuous" if there are enough different values and a high value range and variability (sd)
                if (length(unique(crrCol)) > length(crrCol) / 5 && length(unique(crrCol)) > diff(range(crrCol, na.rm = TRUE)) / 5 &&
                    stats::sd(crrCol, na.rm = TRUE) > diff(range(crrCol, na.rm = TRUE)) / 10) {
                    mtaDta$fields[[i]][["measureType"]] <- ifelse(chkAtt(dtaFrm[[i]], "measureType"), attr(dtaFrm[[i]], "measureType"), "Continuous")
                }
            } else {
                mtaDta$fields[[i]][["type"]]     <- "number"
                mtaDta$fields[[i]][["dataType"]] <- "Decimal"
                # if "measureType" is already stored in the data frame, keep it, otherwise assign "Continuous"
                mtaDta$fields[[i]][["measureType"]] <- ifelse(chkAtt(dtaFrm[[i]], "measureType"), attr(dtaFrm[[i]], "measureType"), "Continuous")
            }
        # [d] dates / times - jamovi actually doesn't support it but i perhaps makes most sense to implement it as numeric
        # can be transformed back in R using - as.Date(..., origin = "1970-01-01") and hms::as_hms(...)
        } else if (inherits(crrCol, c("Date", "POSIXt"))) {
            crrCol <- as.numeric(crrCol)
            mtaDta$fields[[i]][["type"]]        <- "number"
            mtaDta$fields[[i]][["dataType"]]    <- "Decimal"
            mtaDta$fields[[i]][["measureType"]] <- "Continuous"
            mtaDta$fields[[i]][["description"]] <- paste(c(ifelse(nzchar(mtaDta$fields[[i]][["description"]]), mtaDta$fields[[i]][["description"]], names(dtaFrm[i])),
                                                         "(date converted to numeric; days since 1970-01-01)"), collapse = " ")
        } else if (inherits(crrCol, c("difftime"))) {
            crrCol <- as.numeric(crrCol)
            mtaDta$fields[[i]][["type"]]        <- "number"
            mtaDta$fields[[i]][["dataType"]]    <- "Decimal"
            mtaDta$fields[[i]][["measureType"]] <- "Continuous"
            mtaDta$fields[[i]][["description"]] <- paste(c(ifelse(nzchar(mtaDta$fields[[i]][["description"]]), mtaDta$fields[[i]][["description"]], names(dtaFrm[i])),
                                                         "(time converted to numeric; sec since 00:00)"), collapse = " ")
        } else {
            stop(sprintf("Variable type %s not implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no", class(crrCol)))
        }

        # remove atrributes that are only used with specific columnTypes
        if (all(class(dtaFrm[[i]]) != c("logical", "factor"))) {
            mtaDta$fields[[i]][["trimLevels"]]               <- NULL
        }
        if (! chkFld(mtaDta$fields[[i]], "columnType", "Filter")) {
            mtaDta$fields[[i]][["filterNo"]]                 <- NULL
            mtaDta$fields[[i]][["active"]]                   <- NULL
        } else {
            # if the variable is a filter, trimLevels is removed even in cases
            # where the original variable was a factor / logical
            mtaDta$fields[[i]][["trimLevels"]]               <- NULL
        }
        if (! chkFld(mtaDta$fields[[i]], "columnType", "Output")) {
            mtaDta$fields[[i]][["outputAnalysisId"]]         <- NULL
            mtaDta$fields[[i]][["outputOptionName"]]         <- NULL
            mtaDta$fields[[i]][["outputName"]]               <- NULL
            mtaDta$fields[[i]][["outputDesiredColumnName"]]  <- NULL
            mtaDta$fields[[i]][["outputAssignedColumnName"]] <- NULL
        }

        # check that dataType, and measureType are set accordingly to type (attribute and column in the data frame)
        # dataType: Text, Integer, Decimal
        #
        # cat(sprintf("%02d: %s - %s - %s\n", i, mtaDta$fields[[i]][["type"]], mtaDta$fields[[i]][["dataType"]], mtaDta$fields[[i]][["measureType"]]))

        # write to data.bin according to type
        if        (chkFld(mtaDta$fields[[i]], "type", "integer")) {
            wrtCol <- as.integer(crrCol)
        } else if (chkFld(mtaDta$fields[[i]], "type", "number"))  {
            wrtCol <- as.double(crrCol)
        } else if (chkFld(mtaDta$fields[[i]], "type", "string"))  {
            crrCol[is.na(crrCol)] <- ""
            wrtCol <- rep(NA, length(crrCol))
            for (j in seq_along(crrCol)) {
                if (crrCol[j] == "") next
                writeBin(crrCol[j], strHdl)
                wrtCol[j] <- strPos
                strPos <- strPos + length(charToRaw(crrCol[j])) + 1
            }
            wrtCol <- as.integer(wrtCol)
        } else {
            stop(sprintf("Variable type %s not implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no", mtaDta$fields[[i]][["type"]]))
        }
        writeBin(wrtCol, binHdl, endian = "little")

        # remove temporary variables for modifying and storing the current column from the data set
        rm(crrCol, wrtCol)
    }

    # double check whether ID is unique
    id_Lst <- unlist(lapply(seq_along(mtaDta$fields), function(i) mtaDta$fields[[i]][["id"]]))
    if (any(is.na(id_Lst)) || any(duplicated(id_Lst))) {
        for (i in seq_along(mtaDta$fields)) {
            mtaDta$fields[[i]][["id"]] <- i
        }
    }

    # compress data.bin and discard the temporary file
    add2ZIP(fleOut, binHdl, newFle = TRUE)
    rm(binHdl)

    # compress strings.bin (only if it contains data) and discard the temporary file
    add2ZIP(fleOut, strHdl, blnZIP = (strPos > 0))
    rm(strHdl, strPos)

    # create meta, write it and add it to ZIP file
    mnfHdl <- file(file.path(tempdir(), "meta"),         open = "wb")
    add2ZIP(fleOut, mnfHdl, txtOut = mnfTxt())
    rm(mnfHdl)

    # write metadata.json
    mtaHdl <- file(file.path(tempdir(), "metadata.json"), open = "w")
    add2ZIP(fleOut, mtaHdl, txtOut = fmtJSON(list(dataSet = mtaDta)))
    rm(mtaHdl)

    # write xdata.json
    xtdHdl <- file(file.path(tempdir(), "xdata.json"),    open = "w")
    add2ZIP(fleOut, xtdHdl, txtOut = fmtJSON(xtdDta))
    rm(xtdHdl)

    # write index.html and add it to ZIP file
    # currently, the HTML that is stored in the HTML attribute can't be saved because it is only a "front"
    # that doesn't work without the analyses and images contained in it being stored too
    htmHdl <- file(file.path(tempdir(), "index.html"),    open = "w")
#   if (!is.null(attr(dtaFrm, "HTML"))) {
#      add2ZIP(fleOut, htmHdl, txtOut = attr(dtaFrm, "HTML"))
#   } else {
#      add2ZIP(fleOut, htmHdl, txtOut = htmTxt())
#   }
    add2ZIP(fleOut, htmHdl, txtOut = htmTxt())
    rm(htmHdl)

    if (retDbg) {
        list(mtaDta = mtaDta, xtdDta = xtdDta, dtaFrm = dtaFrm)
    }
}

fmtJSON <- function(txtJSON = "") {
    gsub("00: 00", "00:00", gsub("  ", " ", gsub(":", ": ", gsub(",", ", ", jsonlite::toJSON(txtJSON, auto_unbox = TRUE)))))
}

htmTxt <- function() {
    c("<!DOCTYPE html>",
      "<html>",
      "    <head>",
      "        <meta charset=\"utf-8\" />",
      "        <title>Results</title>",
      "        <style>\n",
      "            body {",
      "                font-family: \"Segoe UI\",Roboto,Helvetica,Arial,sans-serif,\"Segoe UI Emoji\",\"Segoe UI Symbol\" ;",
      "                font-size: 12px ;",
      "                color: #333333 ;",
      "                margin: 24px;",
      "                cursor: default ;",
      "            }\n",
      "            h1 {",
      "                font-size: 160% ;",
      "                color: #3E6DA9 ;",
      "                margin-bottom: 12px ;",
      "                white-space: nowrap ;",
      "            }\n",
      "            h2 {",
      "                font-size: 130% ;",
      "                color: #3E6DA9 ;",
      "                margin-bottom: 12px ;",
      "            }\n",
      "            h3, h4, h5 {",
      "                font-size: 110% ;",
      "                margin-bottom: 12px ;",
      "            }\n",
      "            table {",
      "                page-break-inside: avoid;",
      "                border-spacing: 0 ;",
      "            }\n",
      "            table tr td, table tr th {",
      "                font-size: 12px ;",
      "                page-break-inside: avoid;",
      "            }\n",
      "        </style>",
      "</head>",
      "<body>\n",
      "</body>",
      "</html>")
}

mnfTxt <- function() {
    crrTxt <- paste0(sapply(lstMnf, "[[", 1), ":")
    for (i in seq_along(lstMnf)) {
        if (crrTxt[i] == "Created-By:") {
            crrTxt[i] <- paste(crrTxt[i], "jmvReadWrite", utils::packageVersion("jmvReadWrite"))
        } else {
            crrTxt[i] <- paste(crrTxt[i], paste(lstMnf[[i]][2:length(lstMnf[[i]])], collapse = "."))
        }
    }
    enc2utf8(crrTxt)
}

add2ZIP <- function(fleZIP = "", crrHdl = NULL, newFle = FALSE, blnZIP = TRUE, txtOut = "") {

    if (! all(class(crrHdl) == c("file", "connection"))) {
        cat(utils::str(crrHdl))
        stop("Parameter isn\'t a file handle pointing to a file to be zipped.")
    }

    if (all(nchar(txtOut) > 0)) {
        # writing the manifest requires a bit of special handling because of \r\n on Windows vs. \n on Mac / Linux
        # thanks to MAgojam for figuring out a solution
        # for the other files (metadata.json, xdata.json, index.html), it doesn't matter, therefore sep = "\n" is added for all
        writeLines(txtOut, crrHdl, sep = "\n")
    }
    crrFle <- summary(crrHdl)$description
    close(crrHdl)

    if (blnZIP) {
        if (newFle) {
            zip::zip(fleZIP,        basename(crrFle), root = dirname(crrFle))
        } else {
            zip::zip_append(fleZIP, basename(crrFle), root = dirname(crrFle))
        }
    }
    unlink(crrFle)
    rm(crrFle)
}
