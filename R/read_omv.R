#' Read files created of the statistical spreadsheet 'jamovi' (www.jamovi.org)
#'
#' @param fleInp Name (including the path, if required) of the 'jamovi'-file to be read ("FILENAME.omv"; default: "")
#' @param useFlt Apply filters (remove the lines where the filter is set to 0; default: FALSE)?
#' @param rmMsVl Remove values defined as missing values (replace them with NA; default: FALSE)?
#' @param sveAtt Store attributes that are not required in the data set (if you want to write the same data set using write_omv; default: FALSE)?
#' @param getSyn Extract syntax from the analyses in the 'jamovi'-file and store it in the attribute "syntax" (default: FALSE)?
#' @param getHTM Store index.html in the attribute "HTML" (default: FALSE)?
#'
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
#' if (length(attr(data, "syntax")) >= 1) {
#'     print(attr(data, "syntax"));
#'     # the print-function is only used to force devtools::run_examples() to show output
#'     eval(parse(text=paste0("result = ", attr(data, "syntax")[[1]])));
#'     # without assigning the output to a variable, the command would be:
#'     # eval(parse(text=attr(data, "syntax")[[1]]))
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

    # check and format input file names
    fleInp <- fmtFlI(fleInp, maxLng = 1);
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
                colRaw[[1]] <- factor(colRaw[[1]], levels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[1])),
                                                   labels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[2])),
                                                   ordered = chkFld(mtaDta$fields[[i]], "measureType", "Ordinal"));
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
            synPkg <- c("RProtoBuf", "jmvcore", "rlang")
            flePtB <- system.file("jamovi.proto", package = "jmvcore");
            # check whether all required packages and files are present
            if (hasPkg(synPkg) && file.exists(flePtB)) {
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
            } else {
                cat(paste0("WARNING: For extracting syntax, the package(s) \"", paste0(synPkg[!sapply(synPkg, function(X) nzchar(system.file(package = X)))],
                                                                                       collapse = "\", \""), "\" need(s) to be installed.\n\n"))
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


# =================================================================================================
# read_all: for reading data files from various formats (incl. functions that are called)

read_all <- function(fleInp = "", usePkg = c("foreign", "haven"), selSet = "", ...) {
    # check whether the file exists
    chkFle(fleInp)
    varArg <- list(...);
    usePkg <- match.arg(usePkg);
    dtaFrm <- NULL;

    # OMV
    if        (hasExt(fleInp, c("omv"))) {
        dtaFrm <- tryCatch(do.call(read_omv, adjArg("read_omv", list(fleInp = fleInp), varArg, "fleInp")),
                           error = function(errMsg) tryErr(fleInp, errMsg), warning = function(wrnMsg) tryWrn(fleInp, wrnMsg));
    # CSV
    } else if (hasExt(fleInp, c("csv"))) {
        dtaFrm <- tryCatch(rmvQtn(do.call(utils::read.table, adjArg("read.table", list(file = fleInp, sep = ",",  header = TRUE, fill = TRUE), varArg, "file"))),
                           error = function(errMsg) tryErr(fleInp, errMsg), warning = function(wrnMsg) tryWrn(fleInp, wrnMsg));
    # TSV
    } else if (hasExt(fleInp, c("tsv"))) {
        dtaFrm <- tryCatch(rmvQtn(do.call(utils::read.table, adjArg("read.table", list(file = fleInp, sep = "\t", header = TRUE, fill = TRUE), varArg, "file"))),
                           error = function(errMsg) tryErr(fleInp, errMsg), warning = function(wrnMsg) tryWrn(fleInp, wrnMsg));
    # Rdata
    } else if (hasExt(fleInp, c("rdata", "rda"))) {
        dtaFrm <- tryCatch({
                             load(fleInp, rdaTmp <- new.env());
                             if (length(rdaTmp) != 1 && selSet == "") stop("The Rdata-file must include only one object.");
                             rdaTmp[[ifelse(selSet == "", names(rdaTmp)[1], selSet)]]
                           },
                           error = function(errMsg) tryErr(fleInp, errMsg), warning = function(wrnMsg) tryWrn(fleInp, wrnMsg));
    # RDS
    } else if (hasExt(fleInp, c("rds"))) {
        dtaFrm <- tryCatch(do.call(readRDS, adjArg("readRDS", list(file = fleInp), varArg, "file")),
                           error = function(errMsg) tryErr(fleInp, errMsg), warning = function(wrnMsg) tryWrn(fleInp, wrnMsg));
    # SPSS (haven / foreign)
    } else if (hasExt(fleInp, c("sav", "zsav"))) {
        dtaFrm <- tryCatch({
                             if        (usePkg == "haven"   && hasPkg("haven"))   {
                                 hvnTmp <- haven::as_factor(do.call(haven::read_sav, adjArg("haven::read_sav", list(file = fleInp), varArg, "file")), only_labelled = TRUE);
                                 hvnDrp(as.data.frame(hvnTmp@.Data, col.names = names(hvnTmp)), c("format.spss", "display_width"))
                             } else if (usePkg == "foreign" && hasPkg("foreign")) {
                                 fgnLbl(do.call(foreign::read.spss, adjArg("foreign::read.spss", list(file = fleInp, to.data.frame = TRUE), varArg, c("file", "to.data.frame"))))
                             } else {
                                 stop(sprintf("In order to read the SPSS-file \"%s\" either of the R-packages \"haven\" or \"foreign\" needs to be installed.", basename(fleInp)));
                             }
                           },
                           error = function(errMsg) tryErr(fleInp, errMsg), warning = function(wrnMsg) tryWrn(fleInp, wrnMsg));
    # Stata (haven / foreign)
    } else if (hasExt(fleInp, c("dta"))) {
        dtaFrm <- tryCatch({
                             if        (usePkg == "haven"   && hasPkg("haven"))   {
                                 hvnTmp <- haven::as_factor(do.call(haven::read_dta, adjArg("haven::read_dta", list(file = fleInp), varArg, "file")), only_labelled = TRUE);
                                 hvnDrp(as.data.frame(hvnTmp@.Data, col.names = names(hvnTmp)), c("format.stata", "display_width"))
                             } else if (usePkg == "foreign" && hasPkg("foreign")) {
                                 fgnLbl(do.call(foreign::read.dta, adjArg("foreign::read.dta", list(file = fleInp), varArg, c("file"))))
                             } else {
                                 stop(sprintf("In order to read the Stata-file \"%s\" either of the R-packages \"haven\" or \"foreign\" needs to be installed.", basename(fleInp)));
                             }
                           },
                           error = function(errMsg) tryErr(fleInp, errMsg), warning = function(wrnMsg) tryWrn(fleInp, wrnMsg));
    # SAS data (haven)
    } else if (hasExt(fleInp, c("sas7bdat", "sd2", "sd7"))) {
        dtaFrm <- tryCatch({
                             if        (usePkg == "haven"   && hasPkg("haven"))   {
                                 hvnTmp <- haven::as_factor(do.call(haven::read_sas, adjArg("haven::read_sas", list(data_file = fleInp), varArg, "data_file")), only_labelled = TRUE);
                                 hvnDrp(as.data.frame(hvnTmp@.Data, col.names = names(hvnTmp)), c("format.sas", "display_width"))
                             } else {
                                 stop(sprintf("In order to read the SAS-file \"%s\" the R-packages \"haven\" needs to be installed.", basename(fleInp)));
                             }
                           },
                           error = function(errMsg) tryErr(fleInp, errMsg), warning = function(wrnMsg) tryWrn(fleInp, wrnMsg));
    } else if (hasExt(fleInp, c("xpt", "stx", "stc"))) {
        dtaFrm <- tryCatch({
                             if        (usePkg == "haven"   && hasPkg("haven"))   {
                                 hvnTmp <- haven::as_factor(do.call(haven::read_xpt, adjArg("haven::read_xpt", list(file = fleInp), varArg, "file")), only_labelled = TRUE);
                                 hvnDrp(as.data.frame(hvnTmp@.Data, col.names = names(hvnTmp)), c("format.stata", "display_width"))
                             } else if (usePkg == "foreign" && hasPkg("foreign")) {
                                 fgnLbl(do.call(foreign::read.xport, adjArg("foreign::read.xport", list(file = fleInp), varArg, c("file"))))
                             } else {
                                 stop(sprintf("In order to read the SAS-transport-file \"%s\" either of the R-packages \"haven\" or \"foreign\" needs to be installed.", basename(fleInp)));
                             }
                           },
                           error = function(errMsg) tryErr(fleInp, errMsg), warning = function(wrnMsg) tryWrn(fleInp, wrnMsg));
    }

    # check whether the input data are a data frame with the correct dimensions
    chkDtF(dtaFrm);

    # check whether all attributes conform with unicode and do some cleaning if required
    dtaFrm <- rplAtt(dtaFrm);

    dtaFrm
}

tryErr <- function(fleInp = "", errMsg = NULL) {
    message(sprintf("File \"%s\" couldn\'t be read.\nThe error message was: %s\n", basename(fleInp), conditionMessage(errMsg)));
    return(NULL)
}

tryWrn <- function(fleInp = "", wrnMsg = NULL) {
    message(sprintf("Warnings were issued when reading the file \"%s\".\nThe warning was: %s\n", basename(fleInp), conditionMessage(wrnMsg)));
    return(NULL)
}

fgnLbl <- function(dtaFrm = NULL) {
    if (! is.null(attr(dtaFrm, "variable.labels"))) {
        varLbl <- trimws(attr(dtaFrm, "variable.labels"));
        for (crrCol in names(dtaFrm)) {
            if (varLbl[[crrCol]] != "") {
                attr(dtaFrm[[crrCol]], "label") <- varLbl[[crrCol]];
            }
        }
        attr(dtaFrm, "variable.labels") <- NULL;
    }
    dtaFrm
}

hvnDrp <- function(dtaFrm = NULL, rmvAtt = c()) {
   for (crrCol in names(dtaFrm)) {
       for (crrAtt in rmvAtt) {
             if (! is.null(attr(dtaFrm[[crrCol]], crrAtt))) {
                 attr(dtaFrm[[crrCol]], crrAtt) <- NULL
             }
        }
    }
    dtaFrm
}

rmvQtn <- function(dtaFrm = NULL) {
    for (crrCol in names(which(sapply(dtaFrm, is.character)))) {
        dtaFrm[[crrCol]] <- trimws(gsub("\"", "", dtaFrm[[crrCol]]))
    }
    dtaFrm
}

rplAtt <- function(dtaFrm = NULL) {
    # extract the attributes from the dataset and its columns and determine which attributes are
    # character
    setAtt <- setdiff(names(attributes(dtaFrm))[sapply(attributes(dtaFrm), is.character)], c("names", "row.names", "class"));
    colAtt <- setdiff(unique(unlist(sapply(sapply(dtaFrm, attributes), names), use.names = FALSE)), c("class"));

    # go through the data set attributes (except the attributes from R) and check their validity
    for (crrAtt in setAtt) {
        attr(dtaFrm, crrAtt) <- rplStr(attr(dtaFrm, crrAtt), crrAtt);
    }

    # go through the column attributes (except the attributes from R) and the detect columns
    # where those attributes are not validly encoded
    for (crrAtt in colAtt) {
        lstAtt <- sapply(dtaFrm[names(dtaFrm)], attr, crrAtt);
        lstAtt <- lstAtt[!sapply(lstAtt, is.null)];
        if (!any(sapply(lstAtt, is.character))) break
        if (!all(sapply(lstAtt, is.character))) stop(sprintf("Some attribute values of \"%s\" are not of the type character.", crrAtt));
        for (crrCol in names(lstAtt)[sapply(lstAtt, function(x) !all(validEnc(x)))]) {
            attr(dtaFrm[[crrCol]], crrAtt) <- rplStr(attr(dtaFrm[[crrCol]], crrAtt), paste0(c(crrCol, crrAtt), collapse = " - "));
        }
    }

    dtaFrm
}

rplStr <- function(strMod = "", crrAtt = "") {
    # lstRpl is defined in globals.R
    for (i in seq_len(dim(lstRpl)[2])) {
        strMod <- gsub(lstRpl[1, i], lstRpl[2, i], strMod);
    }
    if (! all(validEnc(strMod))) {
        stop(sprintf("The current data set still contains an invalid character in attribute: \"%s\".", crrAtt));
    }

    strMod
}
