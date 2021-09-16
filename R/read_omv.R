#' Read files created of the statistical spreadsheet 'jamovi' (www.jamovi.org)
#'
#' @param fleNme name (including the path, if required) of the 'jamovi'-file to be read ("FILENAME.omv"; default: "")
#' @param useFlt apply filters (remove the lines where the filter is set to 0; default: FALSE)
#' @param rmMsVl remove values defined as missing values (replace them with NA; default - FALSE)
#' @param sveAtt store attributes that are not required in the data set (if you want to write the same data set using write_omv; default – FALSE)
#' @param getSyn extract syntax from the analyses in the 'jamovi'-file and store it in the attribute "syntax" (default – FALSE)
#' @param getHTM store index.html in the attribute "HTML" (default – FALSE)
#' @return data frame (can be directly used with functions included in the R-package 'jmv' and syntax from 'jamovi'; also compatible with the format of the R-package "foreign")
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' fleOMV <- system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite");
#' data <- read_omv(fleNme = fleOMV, getSyn = TRUE);
#' # if the syntax couldn't be extracted, an empty list - length = 0 - is returned,
#' # otherwise, the commands are shown and the first analysis is run, with the output
#' # from the second analysis being assigned to the variable result
#' if (length(attr(data, 'syntax')) >= 1) {
#'     print(attr(data, "syntax"));
#'     # the print-function is only used to force devtools::run_examples() to show output
#'     eval(parse(text=paste0('result = ', attr(data, 'syntax')[[1]])));
#'     # without assigning the output to a variable, the command would be:
#'     # eval(parse(text=attr(data, 'syntax')[[1]]))
#'     print(names(result));
#'     print(result$main);
#'     # → "main"      "assump"    "contrasts" "postHoc"   "emm"       "residsOV"
#'     # (the names of the six output tables)
#' }
#' }
#'
#' @export read_omv
#'
read_omv <- function(fleNme = "", useFlt = FALSE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE) {

    # check whether the file / archive exists, get list of files contained in the archive and check whether it has the correct format
    if (! file.exists(fleNme))                       stop(sprintf("File \"%s\" not found.", fleNme));
    hdrStr <- readBin(tmpHdl <- file(fleNme, "rb"), "character"); close(tmpHdl); rm("tmpHdl");
    if (! hdrStr == "PK\003\004\024")                stop(sprintf("File \"%s\" has not the correct file format (is not a ZIP archive).", fleNme));
    fleLst <- utils::unzip(fleNme, list = TRUE)$Name;

    if (! any(grepl("^meta$|MANIFEST.MF$", fleLst))) stop(sprintf("File \"%s\" has not the correct file format (is missing the jamovi-file-manifest).", fleNme));

    # get list of files contained in the archive
    fleLst <- utils::unzip(fleNme, list = TRUE)$Name;

    # read and decode files: Manifest, metadata (metadata.json), metadata about value labels (xdata.json), binary numeric data (data.bin)
    # and binary string data (strings.bin; if present: it only exists if there are columns that contain text variables)
    strBin <- any(grepl("strings.bin", fleLst));

    mnfTxt <- getTxt(fleNme, fleLst[grepl("^meta$|MANIFEST.MF$", fleLst)][[1]]);
    mtaDta <- getTxt(fleNme, "metadata.json");
    xtdDta <- getTxt(fleNme, "xdata.json");
                binHdl <- file(binFle <- utils::unzip(fleNme, "data.bin",      junkpaths = TRUE), "rb");
    if (strBin) strHdl <- file(strFle <- utils::unzip(fleNme, "strings.bin",   junkpaths = TRUE), "rb");

    # decode the manifest file and throw an error if an file version occurs that was written using a jamovi-version
    # have a look at https://github.com/jamovi/jamovi/blob/current-dev/server/jamovi/server/formatio/omv.py (jav) for
    # how to handle the different jamovi-archive-versions
    mnfVer <- unlist(strsplit(gsub("Manifest-Version: ",       "", mnfTxt[grepl("Manifest-Version:",       mnfTxt)]), "\\."));
    datVer <- unlist(strsplit(gsub("Data-Archive-Version: ",   "", mnfTxt[grepl("Data-Archive-Version:",   mnfTxt)]), "\\."));
    jmvVer <- unlist(strsplit(gsub("jamovi-Archive-Version: ", "", mnfTxt[grepl("jamovi-Archive-Version:", mnfTxt)]), "\\."));
#   crtStr <-                 gsub("Created-By: ",             "", mnfTxt[grepl("Created-By:",             mnfTxt)]);
    if (any(mnfVer != c("1", "0")) || any(datVer != c("1", "0", "2")) || as.integer(jmvVer[1]) > 11) {
        stop(sprintf("The file \"%s\" was written with a version of jamovi that currently is not implemented and therefore can\'t be read. Please send the file to sebastian.jentschke@uib.no!", fleNme));
    }

    # process meta-data
    if (any(names(mtaDta) != "dataSet")) stop("Unimplemeted field in the meta data");

    # rowCount, columnCount
    rowNum <- mtaDta$dataSet$rowCount
    colNum <- mtaDta$dataSet$columnCount
    if (length(mtaDta$dataSet$fields) != colNum) stop("Number of fields in the metadata is not matching up the number of columns.");

    # iterate through fields
    lblLst <- c()
    fltLst <- c()
    for (i in seq_len(colNum)) {
        # type: determines the format in the binary file
        if      (mtaDta$dataSet$fields[[i]]$type == "integer") {

            colRaw <- as.data.frame(readBin(binHdl,   integer(), n = rowNum))
        } else if (mtaDta$dataSet$fields[[i]]$type == "number") {

            colRaw <- as.data.frame(readBin(binHdl,    double(), n = rowNum))
        } else if (mtaDta$dataSet$fields[[i]]$type == "string") {
            colRaw <- as.data.frame(readBin(strHdl, character(), n = rowNum))
                                    readBin(binHdl,   integer(), n = rowNum)
        } else {
            stop(sprintf("Variable type \"%s\" not implemented.", mtaDta$dataSet$fields[[i]]$type));
        }

        # name, description
        nmeCrr <- mtaDta$dataSet$fields[[i]]$name
        lblCrr <- mtaDta$dataSet$fields[[i]]$description

        lblLst <- c(lblLst, lblCrr)

        # value labels
        if (any(nmeCrr == names(xtdDta))) {
            if    (any(mtaDta$dataSet$fields[[i]]$columnType == c("Data", "Recoded"))) {
                colRaw[[1]] <- factor(colRaw[[1]], levels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[1])), labels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[2])))
                if (mtaDta$dataSet$fields[[i]]$dataType == "Integer") {

                    attr(colRaw[[1]], "values") <- unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) as.integer(m[1])))
                }
            } else if (mtaDta$dataSet$fields[[i]]$columnType == "Filter") {
                colRaw[[1]] <- as.logical(colRaw[[1]])
                fltLst <- c(fltLst, i)
            } else {
                stop(sprintf("Error when reading value label - likely the column type is not implemented (yet): %s - %s - %s",
                             nmeCrr, mtaDta$dataSet$fields[[i]]$dataType, mtaDta$dataSet$fields[[i]]$columnType));
            }
        }

        if (i == 1) {

            names(colRaw) <- nmeCrr;
            dtaFrm <- colRaw;
        } else {

            dtaFrm[nmeCrr] <- colRaw;
        }

        if (lblCrr != "")
            attr(dtaFrm[[nmeCrr]], "jmv-desc") <- lblCrr;

        if (mtaDta$dataSet$fields[[i]]$measureType == "ID") {
            attr(dtaFrm[[nmeCrr]], "jmv-id") <- T;
        }

        if (length(mtaDta$dataSet$fields[[i]]$missingValues) > 0) {
            attr(dtaFrm[[nmeCrr]], "missingValues") <- mtaDta$dataSet$fields[[i]]$missingValues;
        }

        if (sveAtt) {
            for (attNme in c("id", "columnType", "dataType", "measureType", "formula", "formulaMessage", "parentId", "width",
                             "type", "importName", "transform", "edits", "trimLevels", "filterNo", "active")) {
                if (! is.null(mtaDta$dataSet$fields[[i]][attNme])) {
                    attr(dtaFrm[[nmeCrr]], attNme) <- mtaDta$dataSet$fields[[i]][[attNme]];
                }
            }
        }

        if (rmMsVl) {
            mssLst <- attr(dtaFrm[[nmeCrr]], "missingValues");
            if (length(mssLst) > 0) {
               attCrr <- attributes(dtaFrm[[nmeCrr]]);
               rmvLvl <- rep(FALSE, length(levels(dtaFrm[[nmeCrr]])));
               for (j in seq_along(mssLst)) {
                   dtaFrm[[nmeCrr]][eval(parse(text = paste0("dtaFrm[[\"", nmeCrr, "\"]]", mssLst[[j]])))] <- NA;
                   rmvLvl <- rmvLvl | eval(parse(text = paste0("levels(dtaFrm[[\"", nmeCrr, "\"]]) ", mssLst[j])));
               }
               dtaFrm[[nmeCrr]] <- dtaFrm[[nmeCrr]][, drop = TRUE];
               attCrr$missingValues <- list();
               attCrr$values <- attCrr$values[!rmvLvl];
               for (attNme in setdiff(names(attCrr), names(attributes(dtaFrm[[nmeCrr]])))) {
                   attr(dtaFrm[[nmeCrr]], attNme) <- attCrr[[attNme]];
               }
            }
        }

        rm("colRaw");
    }

    # close and remove the binary file(s)
    close(binHdl); unlink(binFle); rm("binHdl", "binFle");
    if (strBin) {
        close(strHdl); unlink(strFle); rm("strHdl", "strFle");
    }

    # handle filters
    if (useFlt) {
        fltInc <- rep(TRUE, dim(dtaFrm)[1]);
        for (i in fltLst) fltInc <- fltInc & dtaFrm[[i]];
        dtaFrm <- dtaFrm[fltInc, ];
        dtaFrm[fltLst] <- NULL;
    } else if (length(fltLst) > 0) {
        attr(dtaFrm, "fltLst") <- names(dtaFrm)[fltLst];
    }

    # handle variable labels: R-foreign-style
    if (! all(lblLst == "")) {
        names(lblLst) <- names(dtaFrm);
        attr(dtaFrm, "variable.labels") <- lblLst;
        rm("lblLst");
    }

    # removedRows, addedRows, transforms
    if (sveAtt) {
        for (attNme in c("removedRows", "addedRows", "transforms")) {
            attr(dtaFrm, attNme) <- mtaDta$dataSet[[attNme]];
        }
    }

    # import and extract syntax from the analyses
    if (getSyn) {
        anlLst <- fleLst[grepl("[0-9][0-9].*/analysis", fleLst)];
        savSyn <- list();
        savPBf <- list();
        if (length(anlLst) > 0) {
            flePtB <- system.file("jamovi.proto", package = "jmvcore");
            # check whether all required packages and files are present
            if (length(setdiff(c("RProtoBuf", "jmvcore", "rlang"), utils::installed.packages())) == 0 && file.exists(flePtB)) {
                # try reading the protobuffer-file (if it can be read / parsed, tryCatch returns TRUE and the syntax can be extracted)
                blnPtb <- tryCatch(expr  = {
                                             RProtoBuf::readProtoFiles(flePtB);
                                             TRUE
                                           },
                                   error = function(e) {
                                                         message("Error when loading protocol definition, syntax can\'t be extracted:\n", e);
                                                         FALSE
                                                       }
                                 );
                if (blnPtb) {
                    for (anlNme in anlLst) {
                        anlPBf <- RProtoBuf::read(jamovi.coms.AnalysisResponse, anlHdl <- file(anlFle <- utils::unzip(fleNme, anlNme, junkpaths = TRUE), "rb"));
                        close(anlHdl); unlink(anlFle); rm("anlHdl", "anlFle");
                        # for (anlFld in names(anlPBf)) { print(paste(anlFld, anlPBf[[anlFld]])) }                 # helper function to show all fields
                        # for (anlFld in names(anlPBf$options)) { print(paste(anlFld, anlPBf$options[[anlFld]])) } # helper function to show all fields in options
                        # for (anlFld in names(anlPBf$results)) { print(paste(anlFld, anlPBf$results[[anlFld]])) } # helper function to show all fields in results
                        # ..$bytesize() - size of the protocol buffer (or any field contained in it)
                        savSyn <- c(savSyn, gsub("\\( ", "\\(", gsub("\\n\\s+", " ", fndSyn(anlPBf$results))));
                        anlPBf$results <- NULL;
                        savPBf <- c(savPBf, anlPBf);
                    }
                }
            }
        }
        attr(dtaFrm, "syntax")   <- savSyn;
        attr(dtaFrm, "protobuf") <- savPBf;
    }

    # import the HTML output
    if (getHTM) {
        attr(dtaFrm, "HTML") <- readLines(htmHdl <- file(htmFle <- utils::unzip(fleNme, "index.html", junkpaths = TRUE), "r"), warn = FALSE); close(htmHdl); unlink(htmFle); rm("htmHdl", "htmFle");
    }

    # return the resulting data frame
    dtaFrm
}

fndSyn <- function(resElm = NULL) {
    if (utils::hasName(resElm, "name") && utils::hasName(resElm, "preformatted") && resElm[["name"]] == "syntax" && resElm[["preformatted"]] != "") {
        resElm[["preformatted"]];
    } else if (utils::hasName(resElm, "group") && length(resElm[["group"]]) > 0) {
        for (obj in resElm[["group"]][["elements"]]) {
            ret <- Recall(obj);
            if (!is.null(ret)) return(ret);
        }
    }
}

getTxt <- function(fleOMV = "", crrNme = "") {
    crrTxt <- readLines(crrHdl <- file(crrFle <- utils::unzip(fleOMV, crrNme, junkpaths = TRUE), "r"), warn = FALSE);
    close(crrHdl); unlink(crrFle); rm("crrHdl", "crrFle");

    # depending on whether the original was a JSON file or not, return the appropriate result
    if (grepl("\\.json$", crrNme, ignore.case = TRUE)) {
        rjson::fromJSON(crrTxt, simplify = FALSE)
    } else {
        crrTxt

    }
}
