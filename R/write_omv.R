#' Write files to be used with the statistical spreadsheet 'jamovi'
#' (www.jamovi.org)
#'
#' @param dtaFrm Data frame to be exported (default = NULL)
#' @param fleNme Name / position of the output file to be generated ("FILENAME.omv"; default = "")
#' @return a list containing the meta data (mtaDta, written to metadata.json in the OMV-file), the extended data (xtdDta, written to xdata.json in the OMV-file) and the orginal data frame (dtaFrm)
#'
#' @export write_omv

write_omv <- function(dtaFrm = NULL, fleNme = "") {

    # check whether dtaFrm is a data frame
    if (is.null(dtaFrm) || ! is.data.frame(dtaFrm) || any(dim(dtaFrm) < 1))              { stop("Input data frame is either not a data frame or has not the correct dimensions (at least one dimension has a size of < 1)."); }
    # check that the file name isn't empty, that it ends in .omv, and that the destination directory exists
    if (length(fleNme) <= 0 || ! grepl(".omv", fleNme) || ! dir.exists(dirname(fleNme))) { stop(sprintf("Output file name (%s) doesn't have the correct format (e.g., wrong extension) or destination directory doesn't exist.", fleNme)); }

    colNum <- dim(dtaFrm)[2]

    # initialize metadata.json
    mtaDta <- list(dataSet = list(rowCount = dim(dtaFrm)[1], columnCount = colNum, removedRows = list(), addedRows = list(), fields = list(), transforms = list()));
    mtaDta$dataSet$fields <- rep(list(list(name = "", id = NA, columnType = "Data", dataType = "Integer", measureType = "Nominal", formula = "", formulaMessage = "", parentId = 0,
                                           width = 100, type = "number", importName = "", description = "", transform = 0, edits = list(), missingValues = list(), trimLevels = TRUE,
                                           filterNo = NA, active = FALSE)), colNum);

    # initialize xdata.json
    xtdDta <- list();

    # create data.bin 
    binHdl <- file(description = "data.bin", open = "wb");

    # create strings.bin
    strHdl <- file(description = "strings.bin", open = "wb");
    strPos <- 0

    for (i in 1:colNum) {
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

        # assess measurementType (if not stored as attribute)
        if (is.null(attr(dtaFrm[[i]], "measureType"))) {
            # if the jmv-id marker is set in the original data
            if (! is.null(attr(dtaFrm[[i]], "jmv-id")) && attr(dtaFrm[[i]], "jmv-id") == TRUE) {
                mtaDta$dataSet$fields[[i]][["measureType"]] <- "ID";
            # if original data are text
            } else if (is.character(dtaFrm[[i]])) {
                mtaDta$dataSet$fields[[i]][["measureType"]] <- "Nominal";
            # if original data are a factor
            } else if (is.factor(dtaFrm[[i]])) {
                numLvl <- suppressWarnings(as.integer(dtaFrm[[i]]));
                if (any(is.na(numLvl)) || (stats::sd(diff(numLvl)) > diff(range(numLvl)) / 10)) {
                    mtaDta$dataSet$fields[[i]][["measureType"]] <- "Nominal";
                } else {
                    mtaDta$dataSet$fields[[i]][["measureType"]] <- "Ordinal";
                }
                rm("numLvl");
            # if original data are numeric
            } else if (is.numeric(dtaFrm[[i]])) {
                if (all(is.na(dtaFrm[[i]]))) {
                    if        (is.integer(dtaFrm[[i]])) {
                        mtaDta$dataSet$fields[[i]][["measureType"]] <- "Nominal";
                    } else if (is.double(dtaFrm[[i]]))  {
                        mtaDta$dataSet$fields[[i]][["measureType"]] <- "Continuous";
                    } else {
                        stop("Needs to be implemented: All values NA and column type neither integer nor number / double");
                    }
                } else {
                    if ((length(unique(dtaFrm[[i]])) < diff(range(dtaFrm[[i]])) / 5) || (stats::sd(dtaFrm[[i]]) < diff(range(dtaFrm[[i]])) / 10)) {
                        mtaDta$dataSet$fields[[i]][["measureType"]] <- "Nominal";
                    } else {
                        mtaDta$dataSet$fields[[i]][["measureType"]] <- "Continuous";
                    }
                }
            }
        }

        # value labels - R-foreign-style
        if (! is.null(attr(dtaFrm, "label.table")) && ! is.null(attr(dtaFrm, "label.table")[[names(dtaFrm[i])]])) {
            stop("R-foreign-style value labels need to be implemented.");
        }

        colCrr <- dtaFrm[[i]]
        if (is.null(attr(dtaFrm[[i]], "dataType"))) {
            if (mtaDta$dataSet$fields[[i]][["measureType"]] != "ID") {
                if (is.character(colCrr)) {
                    colCrr <- factor(colCrr);
                    mtaDta$dataSet$fields[[i]][["dataType"]] <- "Text";
                }

                if (is.factor(colCrr)) {
                    facLvl <- attr(colCrr, "levels");
                    facVal <- attr(colCrr, "values");
                    if (sum(is.na(suppressWarnings(as.numeric(facLvl)))) > 1) { 
                        mtaDta$dataSet$fields[[i]][["dataType"]] <- "Text";
                    }
                    if (is.null(facVal)) {
                        colCrr <- as.integer(colCrr);
                        facVal <- unique(sort(colCrr));
                    } else {
                        colCrr <-  facVal[as.integer(colCrr)]
                    }
                    if (length(facVal) > 0) {
                        xtdDta[[names(dtaFrm[i])]] <- list(labels = lapply(1:length(facVal), function(i) list(facVal[[i]], facLvl[[i]], facLvl[[i]])));
                    }
                    rm("facLvl", "facVal");
                } else {
                    mtaDta$dataSet$fields[[i]][["trimLevels"]] <- NULL
                }

                if (mtaDta$dataSet$fields[[i]][["type"]] == "number") {
                    if (mtaDta$dataSet$fields[[i]][["dataType"]] != "Decimal") {
                        if (all(abs(colCrr - round(colCrr)) < sqrt(.Machine$double.eps), na.rm = TRUE)) {
                            mtaDta$dataSet$fields[[i]][["type"]]     <- "integer";
                            mtaDta$dataSet$fields[[i]][["dataType"]] <- "Integer";
                        } else {
                            mtaDta$dataSet$fields[[i]][["type"]]     <- "number";
                            mtaDta$dataSet$fields[[i]][["dataType"]] <- "Decimal";
                        }
                    }
                }
            } else {
                if (is.character(colCrr)) {
                    mtaDta$dataSet$fields[[i]][["type"]]     <- "string";
                    mtaDta$dataSet$fields[[i]][["dataType"]] <- "Text";
                } else if (is.integer(colCrr)) {
                    mtaDta$dataSet$fields[[i]][["type"]]     <- "integer";
                    mtaDta$dataSet$fields[[i]][["dataType"]] <- "Integer";
                }
            }
        }

        # columnType-specific arguments
        if (mtaDta$dataSet$fields[[i]][["columnType"]] != "Filter") {
            mtaDta$dataSet$fields[[i]][["filterNo"]]   <- NULL;
            mtaDta$dataSet$fields[[i]][["active"]]     <- NULL;
        }
        if (is.null(attr(dtaFrm[[i]], "trimLevels")) &&
            mtaDta$dataSet$fields[[i]][["dataType"]]   != "Text") {
            mtaDta$dataSet$fields[[i]][["trimLevels"]] <- NULL;
        }

        # fix problem with transforms


        # check that dataType, and measureType are set accordingly to type (attribute and column in the data frame) 
        # dataType: Text, Integer, Decimal
        # 
        # print(paste0(format(i, width = 2), ": ", mtaDta$dataSet$fields[[i]][["type"]], " - ", mtaDta$dataSet$fields[[i]][["dataType"]], " - ", mtaDta$dataSet$fields[[i]][["measureType"]]));

        # write to data.bin according to type
        if      (mtaDta$dataSet$fields[[i]][["type"]] == "integer") { 
            colWrt <- as.integer(colCrr);
        } else if (mtaDta$dataSet$fields[[i]][["type"]] == "number") { 
            colWrt <- as.double(colCrr);
        } else if (mtaDta$dataSet$fields[[i]][["type"]] == "string") {
            colWrt <- rep(0, length(colCrr));
            for (j in 1:length(colCrr)) {
                writeBin(colCrr[j], strHdl);
                colWrt[j] <- strPos;
                strPos <- strPos + nchar(colCrr[j]) + 1;
            }
            colWrt <- as.integer(colWrt);
        } else {
            stop(sprintf("Variable type %s not implemented.", mtaDta$dataSet$fields[[i]][["type"]]));
        }
        writeBin(colWrt, binHdl, endian = "little");

        # check that dataType, and measureType are set accordingly
        rm("colCrr", "colWrt");
    }

    # attach the attributes stored in the data frame
    for (attNme in names(mtaDta$dataSet)) {
        if (! is.null(attr(dtaFrm, attNme))) {
            mtaDta$dataSet[[attNme]] <- attr(dtaFrm, attNme);
        }
    }

    # double check whether ID is unique
    id_Lst <- unlist(lapply(1:length(mtaDta$dataSet$fields), function(i) mtaDta$dataSet$fields[[i]][["id"]]));
    if (any(is.na(id_Lst)) || any(duplicated(id_Lst))) {
        for (i in 1:length(mtaDta$dataSet$fields)) {
            mtaDta$dataSet$fields[[i]][["id"]] <- i;
        }
    }

    # compress data.bin and discard the temporary file
    close(binHdl);
    utils::zip(fleNme, "data.bin", flags = "-r9Xq");
    unlink("data.bin");

    # check whether data were written to strings.bin
    close(strHdl);
    if (strPos > 0) { utils::zip(fleNme, "strings.bin", flags = "-r9Xq"); }
    unlink("strings.bin");

    # create meta, write it and add it to ZIP file
    # this needs a bit of special handling because of \r\n on Windows vs. \n on Mac / Linux
    # thanks to MAgojam for figuring out a solution
    mnfTxt <- enc2utf8(c("Manifest-Version: 1.0", 
                         "Data-Archive-Version: 1.0.2",
                         "jamovi-Archive-Version: 8.0", 
                         paste("Created-By: jmvReadWrite", utils::packageVersion("jmvReadWrite"))));
    
    writeLines(mnfTxt, mnfHdl <- file("meta", open="wb"), sep="\n"); close(mnfHdl);
    utils::zip(fleNme, "meta", flags = "-r9Xq");
    unlink("meta");
    rm("mnfTxt", "mnfHdl");

    # write metadata.json
    writeLines(gsub(":", ": ", gsub(",", ", ", rjson::toJSON(mtaDta))), "metadata.json");
    utils::zip(fleNme, "metadata.json", flags = "-r9Xq");
    unlink("metadata.json");

    # write xdata.json
    writeLines(gsub(":", ": ", gsub(",", ", ", rjson::toJSON(xtdDta))), "xdata.json");
    utils::zip(fleNme, "xdata.json", flags = "-r9Xq");
    unlink("xdata.json");

    # export empty HTML results output - index.html
    # TO-DO: implement properly writing HTML if this is implemented in read_omv
    resHTM <- c("<!DOCTYPE html>",
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
                "</html>");
    # write HTML and add it to ZIP file
    writeLines(resHTM, con = "index.html");
    utils::zip(fleNme, "index.html", flags = "-r9Xq");
    unlink("index.html");

    list(mtaDta = mtaDta, xtdDta = xtdDta, dtaFrm = dtaFrm);
}
