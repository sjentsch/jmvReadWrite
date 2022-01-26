#' Read files created of the statistical spreadsheet 'jamovi' (www.jamovi.org)
#'
#' @param fleInp name (including the path, if required) of the 'jamovi'-file to be read ("FILENAME.omv"; default: "")
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
#' data <- read_omv(fleInp = fleOMV, getSyn = TRUE);
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
#'     # -> "main"      "assump"    "contrasts" "postHoc"   "emm"       "residsOV"
#'     # (the names of the six output tables)
#' }
#' }
#'
#' @export read_omv
#'
read_omv <- function(fleInp = "", useFlt = FALSE, rmMsVl = FALSE, sveAtt = TRUE, getSyn = FALSE, getHTM = FALSE) {
    if (nchar(fleInp) == 0) stop("File name to the input data file needs to be given as parameter (fleInp = ...).");

    # check whether the .omv-file exists and whether it has the correct format (must be a ZIP), then get the list of files contained in the .omv.-file
    fleInp <- file.path(normalizePath(dirname(fleInp)), basename(fleInp));
    fleLst <- zip::zip_list(fleInp)$filename;
    # check whether the file list contains either the file "meta" (newer jamovi file format) or MANIFEST.MF (older format)
    chkFle(fleInp, isZIP = TRUE)
    chkMnf(fleInp, fleLst[grepl("^meta$|MANIFEST.MF$", fleLst)])

    # https://github.com/jamovi/jamovi/blob/current-dev/server/jamovi/server/formatio/omv.py describes
    # how to handle the different jamovi-archive-versions
    # read and decode files: Manifest, metadata (metadata.json), metadata about value labels (xdata.json), binary numeric data (data.bin)
    # and binary string data (strings.bin; if present: it only exists if there are columns that contain text variables)
    strBin <- any(grepl("strings.bin", fleLst));

    # load the meta-data (global and data column attributes of the data set) and the extended
    # data (value labels)
    mtaDta <- getTxt(fleInp, "metadata.json")$dataSet;
    xtdDta <- getTxt(fleInp, "xdata.json");
                binHdl <- getHdl(fleInp, "data.bin",    "rb");
    if (strBin) strHdl <- getHdl(fleInp, "strings.bin", "rb");

    # process meta-data
    if (! all(grepl(grpMta, names(mtaDta)))) stop("Unimplemeted field in the meta data");

    # rowCount, columnCount
    rowNum <- mtaDta$rowCount
    colNum <- mtaDta$columnCount
    if (length(mtaDta$fields) != colNum) stop("Number of fields in the metadata is not matching up the number of columns.");

    # iterate through fields
    lblLst <- c()
    fltLst <- c()
    for (i in seq_len(colNum)) {
        # type: determines the format in the binary file
        if        (chkFld(mtaDta$fields[[i]], "type", "integer")) {
            colRaw <- as.data.frame(readBin(binHdl,   integer(), n = rowNum));
        } else if (chkFld(mtaDta$fields[[i]], "type", "number"))  {
            colRaw <- as.data.frame(readBin(binHdl,    double(), n = rowNum));
        } else if (chkFld(mtaDta$fields[[i]], "type", "string"))  {
            colRaw <- as.data.frame(readBin(strHdl, character(), n = rowNum));
                                    readBin(binHdl,   integer(), n = rowNum);
        } else {
            stop(sprintf("Variable type \"%s\" not implemented.", mtaDta$fields[[i]]$type));
        }

        # name, description
        nmeCrr <- mtaDta$fields[[i]]$name;
        lblCrr <- mtaDta$fields[[i]]$description;

        lblLst <- c(lblLst, lblCrr)

        # value labels
        if (any(nmeCrr == names(xtdDta))) {
            if        (chkFld(mtaDta$fields[[i]], "columnType", "Filter") || chkFld(mtaDta$fields[[i]], "name", "^Filter [0-9]+$")) {
                colRaw[[1]] <- as.logical(colRaw[[1]]);
                fltLst <- c(fltLst, i);
            } else if (chkFld(mtaDta$fields[[i]], "columnType", "Data|Recoded")) {
                colRaw[[1]] <- factor(colRaw[[1]], levels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[1])), labels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[2])));
                if    (chkFld(mtaDta$fields[[i]], "dataType",   "Integer")) {
                    attr(colRaw[[1]], "values") <- unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) as.integer(m[1])));
                }
            } else {
                stop(sprintf("Error when reading value label - likely the column type is not implemented (yet): %s - %s - %s",
                             nmeCrr, mtaDta$fields[[i]]$dataType, mtaDta$fields[[i]]$columnType));
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

        if (chkFld(mtaDta$fields[[i]], "measureType", "ID")) {
            attr(dtaFrm[[nmeCrr]], "jmv-id") <- TRUE;
        }

        dtaFrm[[nmeCrr]] <- setAtt("missingValues",   mtaDta$fields[[i]], dtaFrm[[nmeCrr]])

        if (sveAtt) {
            dtaFrm[[nmeCrr]] <- setAtt(names(mtaFld), mtaDta$fields[[i]], dtaFrm[[nmeCrr]])
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
               dtaFrm[[nmeCrr]] <- setAtt(setdiff(names(attCrr), names(attributes(dtaFrm[[nmeCrr]]))), attCrr, dtaFrm[[nmeCrr]]);
               rm(attCrr, rmvLvl);
            }
        }
        rm(colRaw);
    }

    # close and remove the binary file(s)
    clsHdl(binHdl); rm(binHdl);
    if (strBin) {
        clsHdl(strHdl); rm(strHdl);
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
        rm(lblLst);
    }

    # removedRows, addedRows, transforms
    if (sveAtt) {
        dtaFrm <- setAtt(c("removedRows", "addedRows", "transforms"), mtaDta, dtaFrm);
    }

    # import and extract syntax from the analyses
    if (getSyn) {
        anlLst <- fleLst[grepl("[0-9][0-9].*/analysis", fleLst)];
        savSyn <- list();
        savPBf <- list();
        if (length(anlLst) > 0) {
            flePtB <- system.file("jamovi.proto", package = "jmvcore");
            # check whether all required packages and files are present
            if (hasPkg(c("RProtoBuf", "jmvcore", "rlang")) && file.exists(flePtB)) {
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
                        anlPBf <- RProtoBuf::read(jamovi.coms.AnalysisResponse, anlHdl <- getHdl(fleInp, anlNme, "rb"));
                        clsHdl(anlHdl); rm(anlHdl);
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
        attr(dtaFrm, "HTML") <- getTxt(fleInp, "index.html");
    }

    # return the resulting data frame
    dtaFrm
}

fndSyn <- function(resElm = NULL) {
    if (chkSyn(resElm)) {
        resElm[["preformatted"]]
    } else if (utils::hasName(resElm, "group") && length(resElm[["group"]]) > 0) {
        for (obj in resElm[["group"]][["elements"]]) {
            ret <- Recall(obj);
            if (!is.null(ret)) return(ret);
        }
    }
}

chkSyn <- function(resElm = NULL) {
   # checks whether the results element is a syntax entry
   # it has to have the name syntax and the preformatted attribute must not be empty
   utils::hasName(resElm, "name")         && resElm[["name"]]         == "syntax" &&
   utils::hasName(resElm, "preformatted") && resElm[["preformatted"]] != ""
}

getTxt <- function(fleOMV = "", crrFle = "") {
    crrTxt <- readLines(crrHdl <- getHdl(fleOMV, crrFle), warn = FALSE);
    clsHdl(crrHdl); rm(crrHdl);

    # depending on whether the original was a JSON file or not, return the appropriate result
    if (hasExt(crrFle, "json")) {
        crrTxt <- rjson::fromJSON(crrTxt, simplify = FALSE);
    }
    crrTxt
}

getHdl <- function(fleOMV = "", crrFle = "", crrMde = "r") {
    zip::unzip(fleOMV, crrFle, exdir = tempdir(), junkpaths = TRUE);
    crrFle <- file.path(tempdir(), list.files(path = tempdir(), pattern = basename(crrFle)));
    if (length(crrFle) == 0) {
        stop(sprintf("The file \"%s\" could not be extracted from \"%s\". Please register an issue.", crrFle, fleOMV));
    }
    file(crrFle, crrMde)
}

clsHdl <- function(crrHdl = NULL) {
    crrFle <- summary(crrHdl)$description;
    close(crrHdl);
    unlink(crrFle);
    rm(crrFle, crrHdl);
}

chkMnf <- function(fleOMV = "", fleMnf = c("")) {
    if (length(fleMnf) < 1) {
        stop(sprintf("File \"%s\" has not the correct file format (is missing the jamovi-file-manifest).", basename(fleOMV)));
    }

    # check the version information in the manifest and whether they are currently supported
    # [[1]] points to the first manifest file, in case both (MANIFEST.MF and meta) exist
    crrTxt <- getTxt(fleOMV, fleMnf[[1]]);
    if (length(crrTxt) != length(lstMnf) || !all(grepl(paste(sapply(lstMnf, "[[", 1), collapse = "|"), crrTxt))) {
        stop(sprintf(paste("The file you are trying to read (%s) has an improper manifest file (meta) and is likely corrupted.",
                           "If the error persists, send the file to sebastian.jentschke@uib.no!"), basename(fleOMV)));
    }
    for (i in seq_along(lstMnf)) {
        # if no version number is given, skip this entry
        if (length(lstMnf[[i]]) == 1) break
        # compare versions, and if the current version is higher then the version defined in lstMnf, issue a warning
        crrVer <- trimws(strsplit(crrTxt[grepl(paste0(lstMnf[[i]][1], ":"), crrTxt)], ":")[[1]])[-1];
        if (utils::compareVersion(crrVer, lstMnf[[i]][-1]) > 0) {
             warning(sprintf(paste("The file that you are trying to read (%s) was written with a version of jamovi that currently is not implemented",
                                   "(%s: %s vs. %s) and therefore may be read incorrectly. Please send the file to sebastian.jentschke@uib.no!"),
                                   basename(fleOMV), lstMnf[[i]][1], crrVer, lstMnf[[i]][-1]));
        }
    }
}
