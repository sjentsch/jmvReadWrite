#' Write files to be used with the statistical spreadsheet 'jamovi'
#' (www.jamovi.org)
#'
#' @param dtaFrm Data frame to be exported (default: NULL)
#' @param fleNme Name / position of the output file to be generated ("FILENAME.omv"; default: "")
#' @return a list containing the meta data (mtaDta, written to metadata.json in the OMV-file), the extended data (xtdDta, written to xdata.json in the OMV-file) and the original data frame (dtaFrm)
#'
#' @details
#' jamovi has a specific measurement level / type "ID" (in addition to the "standard" ones "Nominal", "Ordinal", and "Continuous"). "ID" is used for columns that contain some form of ID (e.g., a
#' participant code). In order to set a variable of your data frame to "ID", you have to manually set an attribute \code{jmv-id} (e.g., \code{attr(dtaFrm$column, "jmv-id") = TRUE}).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#'
#' # use the data set "ToothGrowth" and, if it exists, write it as jamovi-file using write_omv()
#' data("ToothGrowth");
#' wrtDta = write_omv(ToothGrowth, "Trial.omv");
#' print(names(wrtDta));
#' # the print-function is only used to force devtools::run_examples() to show output
#' # → "mtaDta" "xtdDta" "dtaFrm"
#' # returns a list with the metadata (mtaDta, e.g., column and data type),
#' # the extended data (xtdDta, e.g., variable lables), and the data frame (dtaFrm)
#' # the purpose of these variables is merely for checking (understanding the file format)
#' # and debugging
#'
#' # check whether the file was written to the disk, get the file information (size, etc.)
#' # and delete the file afterwards
#' print(list.files(".", "Trial.omv"));
#' # → "Trial.omv"
#' print(file.info("Trial.omv")$size);
#' # → 2111 (size may differ on different OSes)
#' unlink("Trial.omv");
#' }
#'
#' @export write_omv

write_omv <- function(dtaFrm = NULL, fleNme = "") {

    # check whether dtaFrm is a data frame
    if (is.null(dtaFrm) || ! is.data.frame(dtaFrm) || any(dim(dtaFrm) < 1)) {
        stop("Input data frame is either not a data frame or has not the correct dimensions (at least one dimension has a size of < 1).");
    }
    # check that the file name isn't empty, that it ends in .omv, and that the destination directory exists
    if (length(fleNme) <= 0 || ! grepl(".omv", fleNme) || ! dir.exists(dirname(fleNme))) {
        stop(sprintf("Output file name (%s) doesn't have the correct format (e.g., wrong extension) or destination directory doesn't exist.", fleNme));
    }

    colNum <- dim(dtaFrm)[2]

    # initialize metadata.json
    mtaDta <- list(dataSet = list(rowCount = dim(dtaFrm)[1], columnCount = colNum, removedRows = list(), addedRows = list(), fields = list(), transforms = list()));
    mtaDta$dataSet$fields <- rep(list(list(name = "", id = NA, columnType = "Data", dataType = "Integer", measureType = "Nominal", formula = "", formulaMessage = "",
                                           parentId = 0, width = 100, type = "number", outputAnalysisId = NA, outputOptionName = "", outputName = "",
                                           outputDesiredColumnName = "", outputAssignedColumnName = "", importName = "", description = "", transform = 0,
                                           edits = list(), missingValues = list(), trimLevels = TRUE, filterNo = NA, active = FALSE)), colNum);

    # initialize xdata.json
    xtdDta <- list();

    # create data.bin
    binHdl <- file(description = "data.bin", open = "wb");

    # create strings.bin
    strHdl <- file(description = "strings.bin", open = "wb");
    strPos <- 0

    for (i in seq_len(colNum)) {
        # assign fields stored in data frame (if the columns contain attributes);
        for (attNme in names(mtaDta$dataSet$fields[[i]])) {
            if (! is.null(attr(dtaFrm[[i]], attNme))) {
                mtaDta$dataSet$fields[[i]][[attNme]] <- attr(dtaFrm[[i]], attNme);
            }
        }
        rm("attNme");

        # name
        mtaDta$dataSet$fields[[i]][["name"]] <- names(dtaFrm[i]);

        # variable label: if column contains a "jmv-desc" use this, otherwise try whether the data frame contains "variable.labels" and use that
        if        (! is.null(attr(dtaFrm[[i]], "jmv-desc"))) {
            mtaDta$dataSet$fields[[i]][["description"]] <- attr(dtaFrm[[i]], "jmv-desc");
        } else if (! is.null(attr(dtaFrm, "variable.labels"))) {
            mtaDta$dataSet$fields[[i]][["description"]] <- attr(dtaFrm, "variable.labels")[[names(dtaFrm[i])]];
        }

        # value labels - R-foreign-style
        if (! is.null(attr(dtaFrm, "label.table")) && ! is.null(attr(dtaFrm, "label.table")[[names(dtaFrm[i])]])) {
            stop("R-foreign-style value labels need to be implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no");
        }

        # assign column from the original data frame to colCrr (so that modifications don't affect the original)
        colCrr <- dtaFrm[[i]]

        # ID variables represent a special case and are therefore treated first
        # only if the jmv-id marker is set or if the measureType is set to "ID" in the original data
        if ((! is.null(attr(dtaFrm[[i]],      "jmv-id")) && attr(dtaFrm[[i]],      "jmv-id") == TRUE) ||
            (! is.null(attr(dtaFrm[[i]], "measureType")) && attr(dtaFrm[[i]], "measureType") == "ID")) {
            if (is.character(colCrr)) {
                mtaDta$dataSet$fields[[i]][["dataType"]] <- "Text";
                mtaDta$dataSet$fields[[i]][["type"]]     <- "string";
            } else if (is.integer(colCrr)) {
                mtaDta$dataSet$fields[[i]][["dataType"]] <- "Integer";
                mtaDta$dataSet$fields[[i]][["type"]]     <- "integer";
            }
            mtaDta$dataSet$fields[[i]][["measureType"]]  <- "ID";
        # afterwards, the different variable types for each column of the original data frame are tested
        # [a] logical
        } else if (is.logical(colCrr)) {
            colCrr <- as.integer(colCrr);
            mtaDta$dataSet$fields[[i]][["dataType"]]    <- "Integer";
            mtaDta$dataSet$fields[[i]][["type"]]        <- "integer";
            # measureType not set as the correct type ("Nominal") is already the default
            xtdDta[[names(dtaFrm[i])]] <- list(labels = lapply(0:1, function(i) list(i, as.character(i), as.character(i), FALSE)));
        # [b] factors
        } else if (is.factor(colCrr)) {
            facLvl <- attr(colCrr, "levels");
            facVal <- attr(colCrr, "values");
            if (is.null(facVal)) {
                colCrr <- as.integer(colCrr);
                facVal <- unique(sort(colCrr));
                mtaDta$dataSet$fields[[i]][["dataType"]] <- ifelse(all(!is.na(suppressWarnings(as.integer(facLvl)))) && all(as.character(as.integer(facLvl)) == facLvl), "Integer", "Text")
            } else {
                colCrr <- facVal[as.integer(colCrr)]
                mtaDta$dataSet$fields[[i]][["dataType"]] <- "Integer";
            }
            mtaDta$dataSet$fields[[i]][["type"]]         <- "integer";
            # if the attribute is already stored in the data frame, it is overwritten further below (l. 238)
            # it is only set to "Ordinal" if the properties indicate it to be likely ("Nominal" doesn't need change as it is the default)
            if (mtaDta$dataSet$fields[[i]][["dataType"]] == "Integer" && length(facVal) > 5 && !any(is.na(c(facVal, colCrr))) && stats::sd(diff(facVal)) < diff(range(colCrr)) / 10) {
               mtaDta$dataSet$fields[[i]][["measureType"]] <- "Ordinal";
            }
            if (length(facVal) > 0) {
                xtdDta[[names(dtaFrm[i])]] <- list(labels = lapply(seq_along(facVal), function(i) list(facVal[[i]], facLvl[[i]], facLvl[[i]], FALSE)));
            }
            rm("facLvl", "facVal");
        # [c] characters / strings
        } else if (is.character(colCrr)) {
            facLvl <- unique(colCrr);
            facVal <- seq(1, length(facVal));
            colCrr <- as.integer(as.factor(colCrr));
            mtaDta$dataSet$fields[[i]][["type"]]        <- "integer";
            mtaDta$dataSet$fields[[i]][["dataType"]]    <- "Text";
            # measureType not set as the correct type ("Nominal") is already the default
            if (length(facVal) > 0) {
                xtdDta[[names(dtaFrm[i])]] <- list(labels = lapply(seq_along(facVal), function(i) list(facVal[[i]], facLvl[[i]], facLvl[[i]], FALSE)));
            }
        # [d] numerical (integer / decimals)
        } else if (is.numeric(colCrr)) {
            if (all(abs(colCrr - round(colCrr)) < sqrt(.Machine$double.eps), na.rm = TRUE)) {
                colCrr <- as.integer(colCrr);
                mtaDta$dataSet$fields[[i]][["type"]]     <- "integer";
                mtaDta$dataSet$fields[[i]][["dataType"]] <- "Integer";
                # if the attribute is already stored in the data frame, it is overwritten further below (l. 238)
                # integers are considered "Continuous" if they contain a high enough value range and variability (sd)
                if (length(unique(colCrr)) > diff(range(colCrr, na.rm = TRUE)) / 5 && stats::sd(colCrr, na.rm = TRUE) > diff(range(colCrr, na.rm = TRUE)) / 10) {
                    mtaDta$dataSet$fields[[i]][["measureType"]] <- "Continuous";
                }
            } else {
                mtaDta$dataSet$fields[[i]][["type"]]     <- "number";
                mtaDta$dataSet$fields[[i]][["dataType"]] <- "Decimal";
                # if the attribute is already stored in the data frame, it is overwritten further below (l. 238)
                mtaDta$dataSet$fields[[i]][["measureType"]] <- "Continuous";
            }
        # [e] dates / times - not implemented yet
        } else if (all(sapply(colCrr, inherits, c("Date", "POSIXt")))) {
            stop("Needs to be implemented: Date / Time. Please send the data file that caused this problem to sebastian.jentschke@uib.no");
        } else {
            stop(sprintf("Variable type %s not implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no", class(colCrr)));
        }

        # remove atrributes that are only used with specific columnTypes
        if (all(class(dtaFrm[[i]]) != c("logical", "factor"))) {
            mtaDta$dataSet$fields[[i]][["trimLevels"]]               <- NULL;
        }
        if (mtaDta$dataSet$fields[[i]][["columnType"]] != "Filter") {
            mtaDta$dataSet$fields[[i]][["filterNo"]]                 <- NULL;
            mtaDta$dataSet$fields[[i]][["active"]]                   <- NULL;
        } else {
            # if the variable is a filter, trimLevels is removed even in cases
            # where the original variable was a factor / logical
            mtaDta$dataSet$fields[[i]][["trimLevels"]]               <- NULL;
        }
        if (mtaDta$dataSet$fields[[i]][["columnType"]] != "Output") {
            mtaDta$dataSet$fields[[i]][["outputAnalysisId"]]         <- NULL;
            mtaDta$dataSet$fields[[i]][["outputOptionName"]]         <- NULL;
            mtaDta$dataSet$fields[[i]][["outputName"]]               <- NULL;
            mtaDta$dataSet$fields[[i]][["outputDesiredColumnName"]]  <- NULL;
            mtaDta$dataSet$fields[[i]][["outputAssignedColumnName"]] <- NULL;
        }

        # fix problem with transforms


        # check that dataType, and measureType are set accordingly to type (attribute and column in the data frame)
        # dataType: Text, Integer, Decimal
        #
        # print(sprintf("%02d: %s - %s - %s\n", i, mtaDta$dataSet$fields[[i]][["type"]], mtaDta$dataSet$fields[[i]][["dataType"]], mtaDta$dataSet$fields[[i]][["measureType"]]));

        # write to data.bin according to type
        if      (mtaDta$dataSet$fields[[i]][["type"]] == "integer") {
            colWrt <- as.integer(colCrr);
        } else if (mtaDta$dataSet$fields[[i]][["type"]] == "number") {
            colWrt <- as.double(colCrr);
        } else if (mtaDta$dataSet$fields[[i]][["type"]] == "string") {
            colWrt <- rep(0, length(colCrr));
            for (j in seq_along(colCrr)) {
                writeBin(colCrr[j], strHdl);
                colWrt[j] <- strPos;
                strPos <- strPos + nchar(colCrr[j]) + 1;
            }
            colWrt <- as.integer(colWrt);
        } else {
            stop(sprintf("Variable type %s not implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no", mtaDta$dataSet$fields[[i]][["type"]]));
        }
        writeBin(colWrt, binHdl, endian = "little");

        # remove temporary variables for modifying and storing the current column from the data set
        rm("colCrr", "colWrt");
    }

    # attach the attributes stored in the data frame
    for (attNme in names(mtaDta$dataSet)) {
        if (! is.null(attr(dtaFrm, attNme))) {
            mtaDta$dataSet[[attNme]] <- attr(dtaFrm, attNme);
        }
    }

    # double check whether ID is unique
    id_Lst <- unlist(lapply(seq_along(mtaDta$dataSet$fields), function(i) mtaDta$dataSet$fields[[i]][["id"]]));
    if (any(is.na(id_Lst)) || any(duplicated(id_Lst))) {
        for (i in seq_along(mtaDta$dataSet$fields)) {
            mtaDta$dataSet$fields[[i]][["id"]] <- i;
        }
    }

    # compress data.bin and discard the temporary file
    close(binHdl); rm("binHdl");
    add2ZIP(fleNme, "data.bin", newFle = TRUE);

    # compress strings.bin (only if it contains data) and discard the temporary file
    close(strHdl); rm("strHdl");
    add2ZIP(fleNme, "strings.bin", blnZIP = strPos > 0);

    # create meta, write it and add it to ZIP file
    # this needs a bit of special handling because of \r\n on Windows vs. \n on Mac / Linux
    # thanks to MAgojam for figuring out a solution
    mnfTxt <- enc2utf8(c("Manifest-Version: 1.0",
                         "Data-Archive-Version: 1.0.2",
                         "jamovi-Archive-Version: 8.0",
                         paste("Created-By: jmvReadWrite", utils::packageVersion("jmvReadWrite"))));

    writeLines(mnfTxt, mnfHdl <- file("meta", open = "wb"), sep = "\n"); close(mnfHdl); rm("mnfTxt", "mnfHdl");
    add2ZIP(fleNme, "meta");

    # write metadata.json
    writeLines(fmtJSON(mtaDta), "metadata.json");
    add2ZIP(fleNme, "metadata.json");

    # write xdata.json
    writeLines(fmtJSON(xtdDta), "xdata.json");
    add2ZIP(fleNme, "xdata.json");

    # write index.html and add it to ZIP file
    # currently, the HTML that is stored in the HTML attribute can't be saved because it is only a "front"
    # that doesn't work without the analyses and images contained in it being stored too
#   if (!is.null(attr(dtaFrm, "HTML"))) {
#      writeLines(attr(dtaFrm, "HTML"), "index.html");
#   } else {
#      writeLines(empHTM(),             "index.html");
#   }
    writeLines(empHTM(), "index.html");
    add2ZIP(fleNme, "index.html");

    list(mtaDta = mtaDta, xtdDta = xtdDta, dtaFrm = dtaFrm);
}

add2ZIP <- function(fleZIP = "", crrFle = "", newFle = FALSE, blnZIP = TRUE) {
    if (blnZIP) {
        if (newFle) {
            zip::zip(fleZIP, crrFle);
        } else {
            zip::zip_append(fleZIP, crrFle);
        }
    }
    unlink(crrFle);
}

fmtJSON <- function(txtJSON = "") {
    gsub(":", ": ", gsub(",", ", ", rjson::toJSON(txtJSON)))
}

empHTM <- function() {
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
