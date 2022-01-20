#' Write files to be used with the statistical spreadsheet 'jamovi'
#' (www.jamovi.org)
#'
#' @param dtaFrm Data frame to be exported (default: NULL)
#' @param fleNme Name / position of the output file to be generated ("FILENAME.omv"; default: "")
#' @param retDbg Whether to return a list with debugging information (see Value; default: FALSE)
#' @return a list (if retDbg == TRUE), containing the meta data (mtaDta, metadata.json in the OMV-file), the extended data (xtdDta, xdata.json in the OMV-file) and the original data frame (dtaFrm)
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
#' wrtDta = write_omv(ToothGrowth, "Trial.omv", retDbg = TRUE);
#' print(names(wrtDta));
#' # the print-function is only used to force devtools::run_examples() to show output
#' # -> "mtaDta" "xtdDta" "dtaFrm"
#' # returns a list with the metadata (mtaDta, e.g., column and data type),
#' # the extended data (xtdDta, e.g., variable lables), and the data frame (dtaFrm)
#' # the purpose of these variables is merely for checking (understanding the file format)
#' # and debugging
#'
#' # check whether the file was written to the disk, get the file information (size, etc.)
#' # and delete the file afterwards
#' print(list.files(".", "Trial.omv"));
#' # -> "Trial.omv"
#' print(file.info("Trial.omv")$size);
#' # -> 2111 (size may differ on different OSes)
#' unlink("Trial.omv");
#' }
#'
#' @export write_omv

write_omv <- function(dtaFrm = NULL, fleNme = "", retDbg = FALSE) {

    # check whether dtaFrm is a data frame
    if (is.null(dtaFrm) || ! is.data.frame(dtaFrm) || any(dim(dtaFrm) < 1)) {
        stop("Input data frame is either not a data frame or has not the correct dimensions (at least one dimension has a size of < 1).");
    }
    # check that the file name isn't empty, that it ends in .omv, and that the destination directory exists
    fleNme <- file.path(normalizePath(dirname(fleNme)), basename(fleNme));
    if (! grepl(".omv$", fleNme) || ! dir.exists(dirname(fleNme))) {
        stop(sprintf("Output file name (%s) doesn't have the correct format (e.g., wrong extension) or destination directory doesn't exist.", basename(fleNme)));
    }

    colNum <- dim(dtaFrm)[2];

    # initialize metadata.json
    mtaDta <- mtaGlb;
    # use the attributes stored in the data frame (for the whole data set) to
    # update the metadata
    mtaDta <- setAtt(names(mtaDta), dtaFrm, mtaDta);
    # the number of rows and columns has to be adjusted to the current data set
    mtaDta$rowCount    <- dim(dtaFrm)[1];
    mtaDta$columnCount <- colNum;
    # create the entries for storing the column specific information
    mtaDta$fields      <- rep(list(mtaFld), colNum);

    # initialize xdata.json
    xtdDta <- list();

    # create data.bin
    binHdl <- file(description = file.path(tempdir(), "data.bin"),    open = "wb");

    # create strings.bin
    strHdl <- file(description = file.path(tempdir(), "strings.bin"), open = "wb");
    strPos <- 0

    for (i in seq_len(colNum)) {
        # assign attributes that are stored in data frame for this data column
        # (if available)
        mtaDta$fields[[i]] <- setAtt(names(mtaDta$fields[[i]]), dtaFrm[[i]], mtaDta$fields[[i]]);

        # name
        mtaDta$fields[[i]][["name"]] <- names(dtaFrm[i]);

        # variable label: if column contains a "jmv-desc" use this, otherwise try whether the data frame contains "variable.labels" and use that
        if        (chkAtt(dtaFrm[[i]], "jmv-desc")) {
            mtaDta$fields[[i]][["description"]] <- attr(dtaFrm[[i]], "jmv-desc");
        } else if (chkAtt(dtaFrm, "variable.labels")) {
            mtaDta$fields[[i]][["description"]] <- attr(dtaFrm, "variable.labels")[[names(dtaFrm[i])]];
        }

        # value labels - R-foreign-style
        if (chkAtt(dtaFrm, "label.table")) {
            stop("R-foreign-style value labels need to be implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no");
        }

        # assign column from the original data frame to colCrr (so that modifications don't affect the original)
        colCrr <- dtaFrm[[i]]

        # ID variables represent a special case and are therefore treated first
        # only if the jmv-id marker is set or if the measureType is set to "ID" in the original data
        if (chkAtt(dtaFrm[[i]], "jmv-id", TRUE) || chkAtt(dtaFrm[[i]], "measureType", "ID")) {
            if (is.character(colCrr)) {
                mtaDta$fields[[i]][["dataType"]] <- "Text";
                mtaDta$fields[[i]][["type"]]     <- "string";
            } else if (is.integer(colCrr)) {
                mtaDta$fields[[i]][["dataType"]] <- "Integer";
                mtaDta$fields[[i]][["type"]]     <- "integer";
            }
            mtaDta$fields[[i]][["measureType"]]  <- "ID";
        # afterwards, the different variable types for each column of the original data frame are tested
        # [a] logical
        } else if (is.logical(colCrr)) {
            colCrr <- as.integer(colCrr);
            mtaDta$fields[[i]][["dataType"]]    <- "Integer";
            mtaDta$fields[[i]][["type"]]        <- "integer";
            # measureType not set as the correct type ("Nominal") is already the default
            xtdDta[[names(dtaFrm[i])]] <- list(labels = lapply(0:1, function(i) list(i, as.character(i), as.character(i), FALSE)));
        # [b] factors
        } else if (is.factor(colCrr)) {
            facLvl <- attr(colCrr, "levels");
            facVal <- attr(colCrr, "values");
            if (is.null(facVal)) {
                colCrr <- as.integer(colCrr);
                facVal <- unique(sort(colCrr));
                mtaDta$fields[[i]][["dataType"]] <- ifelse(all(!is.na(suppressWarnings(as.integer(facLvl)))) && all(as.character(as.integer(facLvl)) == facLvl), "Integer", "Text")
            } else {
                colCrr <- facVal[as.integer(colCrr)]
                mtaDta$fields[[i]][["dataType"]] <- "Integer";
            }
            mtaDta$fields[[i]][["type"]]         <- "integer";
            # if "measureType" is already stored in the data frame, keep it, otherwise set it to "Ordinal" if the properties indicate it to be likely ("Nominal" is already the default)
            if (chkFld(mtaDta$fields[[i]], "dataType", "Integer") && length(facVal) > 5 && !any(is.na(c(facVal, colCrr))) && stats::sd(diff(facVal)) < diff(range(colCrr)) / 10) {
                mtaDta$fields[[i]][["measureType"]] <- ifelse(chkAtt(dtaFrm[[i]], "measureType"), attr(dtaFrm[[i]], "measureType"), "Ordinal");
            }
            if (length(facVal) > 0) {
                xtdDta[[names(dtaFrm[i])]] <- list(labels = lapply(seq_along(facVal), function(i) list(facVal[[i]], facLvl[[i]], facLvl[[i]], FALSE)));
            }
            rm(facLvl, facVal);
        # [c] characters / strings
        } else if (is.character(colCrr)) {
            facLvl <- unique(colCrr);
            facVal <- seq(1, length(facVal));
            colCrr <- as.integer(as.factor(colCrr));
            mtaDta$fields[[i]][["type"]]        <- "integer";
            mtaDta$fields[[i]][["dataType"]]    <- "Text";
            # measureType not set as the correct type ("Nominal") is already the default
            if (length(facVal) > 0) {
                xtdDta[[names(dtaFrm[i])]] <- list(labels = lapply(seq_along(facVal), function(i) list(facVal[[i]], facLvl[[i]], facLvl[[i]], FALSE)));
            }
        # [d] numerical (integer / decimals)facLvl <- unique(colCrr)
        } else if (is.numeric(colCrr)) {
            if (all(abs(colCrr - round(colCrr)) < sqrt(.Machine$double.eps), na.rm = TRUE)) {
                colCrr <- as.integer(colCrr);
                mtaDta$fields[[i]][["type"]]     <- "integer";
                mtaDta$fields[[i]][["dataType"]] <- "Integer";
                # if "measureType" is already stored in the data frame, keep it, otherwise assign "Continuous" if there is a high enough value range and variability (sd)
                if (length(unique(colCrr)) > diff(range(colCrr, na.rm = TRUE)) / 5 && stats::sd(colCrr, na.rm = TRUE) > diff(range(colCrr, na.rm = TRUE)) / 10) {
                    mtaDta$fields[[i]][["measureType"]] <- ifelse(chkAtt(dtaFrm[[i]], "measureType"), attr(dtaFrm[[i]], "measureType"), "Continuous");
                }
            } else {
                mtaDta$fields[[i]][["type"]]     <- "number";
                mtaDta$fields[[i]][["dataType"]] <- "Decimal";
                # if "measureType" is already stored in the data frame, keep it, otherwise assign "Continuous"
                mtaDta$fields[[i]][["measureType"]] <- ifelse(chkAtt(dtaFrm[[i]], "measureType"), attr(dtaFrm[[i]], "measureType"), "Continuous");
            }
        # [e] dates / times - not implemented yet
        } else if (all(sapply(colCrr, inherits, c("Date", "POSIXt")))) {
            stop("Needs to be implemented: Date / Time. Please send the data file that caused this problem to sebastian.jentschke@uib.no");
        } else {
            stop(sprintf("Variable type %s not implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no", class(colCrr)));
        }

        # remove atrributes that are only used with specific columnTypes
        if (all(class(dtaFrm[[i]]) != c("logical", "factor"))) {
            mtaDta$fields[[i]][["trimLevels"]]               <- NULL;
        }
        if (! chkFld(mtaDta$fields[[i]], "columnType", "Filter")) {
            mtaDta$fields[[i]][["filterNo"]]                 <- NULL;
            mtaDta$fields[[i]][["active"]]                   <- NULL;
        } else {
            # if the variable is a filter, trimLevels is removed even in cases
            # where the original variable was a factor / logical
            mtaDta$fields[[i]][["trimLevels"]]               <- NULL;
        }
        if (! chkFld(mtaDta$fields[[i]], "columnType", "Output")) {
            mtaDta$fields[[i]][["outputAnalysisId"]]         <- NULL;
            mtaDta$fields[[i]][["outputOptionName"]]         <- NULL;
            mtaDta$fields[[i]][["outputName"]]               <- NULL;
            mtaDta$fields[[i]][["outputDesiredColumnName"]]  <- NULL;
            mtaDta$fields[[i]][["outputAssignedColumnName"]] <- NULL;
        }

        # fix problem with transforms


        # check that dataType, and measureType are set accordingly to type (attribute and column in the data frame)
        # dataType: Text, Integer, Decimal
        #
        # cat(sprintf("%02d: %s - %s - %s\n", i, mtaDta$fields[[i]][["type"]], mtaDta$fields[[i]][["dataType"]], mtaDta$fields[[i]][["measureType"]]));

        # write to data.bin according to type
        if        (chkFld(mtaDta$fields[[i]], "type", "integer")) {
            colWrt <- as.integer(colCrr);
        } else if (chkFld(mtaDta$fields[[i]], "type", "number"))  {
            colWrt <- as.double(colCrr);
        } else if (chkFld(mtaDta$fields[[i]], "type", "string"))  {
            colWrt <- rep(0, length(colCrr));
            for (j in seq_along(colCrr)) {
                writeBin(colCrr[j], strHdl);
                colWrt[j] <- strPos;
                strPos <- strPos + nchar(colCrr[j]) + 1;
            }
            colWrt <- as.integer(colWrt);
        } else {
            stop(sprintf("Variable type %s not implemented. Please send the data file that caused this problem to sebastian.jentschke@uib.no", mtaDta$fields[[i]][["type"]]));
        }
        writeBin(colWrt, binHdl, endian = "little");

        # remove temporary variables for modifying and storing the current column from the data set
        rm(colCrr, colWrt);
    }

    # double check whether ID is unique
    id_Lst <- unlist(lapply(seq_along(mtaDta$fields), function(i) mtaDta$fields[[i]][["id"]]));
    if (any(is.na(id_Lst)) || any(duplicated(id_Lst))) {
        for (i in seq_along(mtaDta$fields)) {
            mtaDta$fields[[i]][["id"]] <- i;
        }
    }

    # compress data.bin and discard the temporary file
    add2ZIP(fleNme, binHdl, newFle = TRUE);
    rm(binHdl);

    # compress strings.bin (only if it contains data) and discard the temporary file
    add2ZIP(fleNme, strHdl, blnZIP = strPos > 0);
    rm(strHdl);

    # create meta, write it and add it to ZIP file
    mnfHdl <- file(file.path(tempdir(), "meta"),         open = "wb");
    add2ZIP(fleNme, mnfHdl, txtOut = mnfTxt());
    rm(mnfHdl);

    # write metadata.json
    mtaHdl <- file(file.path(tempdir(), "metadata.json"), open = "w");
    add2ZIP(fleNme, mtaHdl, txtOut = fmtJSON(list(dataSet = mtaDta)));
    rm(mtaHdl);

    # write xdata.json
    xtdHdl <- file(file.path(tempdir(), "xdata.json"),    open = "w");
    add2ZIP(fleNme, xtdHdl, txtOut = fmtJSON(xtdDta));
    rm(xtdHdl);

    # write index.html and add it to ZIP file
    # currently, the HTML that is stored in the HTML attribute can't be saved because it is only a "front"
    # that doesn't work without the analyses and images contained in it being stored too
    htmHdl <- file(file.path(tempdir(), "index.html"),    open = "w");
#   if (!is.null(attr(dtaFrm, "HTML"))) {
#      add2ZIP(fleNme, htmHdl, txtOut = attr(dtaFrm, "HTML"));
#   } else {
#      add2ZIP(fleNme, htmHdl, txtOut = htmTxt());
#   }
    add2ZIP(fleNme, htmHdl, txtOut = htmTxt());
    rm(htmHdl);

    if (retDbg) {
        list(mtaDta = mtaDta, xtdDta = xtdDta, dtaFrm = dtaFrm)
    }
}

fmtJSON <- function(txtJSON = "") {
    gsub("  ", " ", gsub(":", ": ", gsub(",", ", ", rjson::toJSON(txtJSON))))
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
    crrTxt <- paste0(sapply(lstMnf, "[[", 1), ":");
    for (i in seq_along(lstMnf)) {
        if (crrTxt[i] == "Created-By:") {
            crrTxt[i] <- paste(crrTxt[i], "jmvReadWrite", utils::packageVersion("jmvReadWrite"));
        } else {
            crrTxt[i] <- paste(crrTxt[i], paste(lstMnf[[i]][2:length(lstMnf[[i]])], collapse = "."));
        }
    }
    enc2utf8(crrTxt)
}

add2ZIP <- function(fleZIP = "", crrHdl = NULL, newFle = FALSE, blnZIP = TRUE, txtOut = "") {

    if (! all(class(crrHdl) == c("file", "connection"))) {
        cat(utils::str(crrHdl));
        stop("Parameter isn't the file handle pointing to a file to be zipped.")
    }

    if (all(nchar(txtOut) > 0)) {
        # writing the manifest requires a bit of special handling because of \r\n on Windows vs. \n on Mac / Linux
        # thanks to MAgojam for figuring out a solution
        # for the other files (metadata.json, xdata.json, index.html), it doesn't matter, therefore sep = "\n" is added for all
        writeLines(txtOut, crrHdl, sep = "\n");
    }
    crrFle <- summary(crrHdl)$description;
    close(crrHdl);

    if (blnZIP) {
        if (newFle) {
            zip::zip(fleZIP,        basename(crrFle), root = dirname(crrFle));
        } else {
            zip::zip_append(fleZIP, basename(crrFle), root = dirname(crrFle));
        }
    }
    unlink(crrFle); rm(crrFle);
}
