#' Read files created of the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param fleInp Name (including the path, if required) of the 'jamovi'-file to be read ("FILENAME.omv"; default: "")
#' @param useFlt Apply filters (remove the lines where the filter is set to 0; default: FALSE)?
#' @param rmMsVl Remove values defined as missing values (replace them with NA; default: FALSE)?
#' @param sveAtt Store attributes that are not required in the data set (if you want to write the same data set using write_omv; default: FALSE)?
#' @param getSyn Extract syntax from the analyses in the 'jamovi'-file and store it in the attribute "syntax" (default: FALSE)?
#' @param getHTM Store index.html in the attribute "HTML" (default: FALSE)?
#'
#' @return data frame (can be directly used with functions included in the R-package `jmv` and syntax from 'jamovi'; also compatible with the format of the
#'         R-package `foreign`)
#'
#' @examples
#' \dontrun{
#' nmeInp <- system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite")
#' data <- jmvReadWrite::read_omv(fleInp = nmeInp, getSyn = TRUE)
#' # if the syntax couldn't be extracted, an empty list - length = 0 - is returned,
#' # otherwise, the commands are shown and the first analysis is run, with the output
#' # from the second analysis being assigned to the variable result
#' if (length(attr(data, "syntax")) >= 1) {
#'     print(attr(data, "syntax"))
#'     if (nzchar(system.file(package = "jmv"))) {
#'         # the print-function is only used to force devtools::run_examples() to show output
#'         eval(parse(text = paste0("result = ", attr(data, "syntax")[1])))
#'         # without assigning the output to a variable, the command would be:
#'         # eval(parse(text = attr(data, "syntax")[1]))
#'         print(names(result))
#'         print(result$main)
#'         # -> "main"      "assump"    "contrasts" "postHoc"   "emm"       "residsOV"
#'         # (the names of the six output tables)
#'     }
#' }
#' }
#'
#' @export read_omv
#'
read_omv <- function(fleInp = "", useFlt = FALSE, rmMsVl = FALSE, sveAtt = TRUE, getSyn = FALSE, getHTM = FALSE) {
    if (nchar(fleInp) == 0) stop("File name to the input data file needs to be given as parameter (fleInp = ...).")

    # check and format input file names - do not use chkExt here
    if (!hasExt(fleInp, c("omv", "omt"))) {
        stop("read_omv only reads jamovi files (.omv / .omt), use convert_to_omv first, if you want to read other files types.")
    }
    fleInp <- fmtFlI(fleInp, maxLng = 1)
    fleLst <- zip::zip_list(fleInp)$filename
    # check whether the file list contains either the file "meta" (newer jamovi file format) or MANIFEST.MF (older format)
    chkFle(fleInp, isZIP = TRUE)
    chkMnf(fleInp, fleLst[grepl("^meta$|MANIFEST.MF$", fleLst)])

    # https://github.com/jamovi/jamovi/blob/current-dev/server/jamovi/server/formatio/omv.py describes
    # how to handle the different jamovi-archive-versions
    # read and decode files: Manifest, metadata (metadata.json), metadata about value labels (xdata.json), binary numeric data (data.bin)
    # and binary string data (strings.bin; if present: it only exists if there are columns that contain text variables)
    strBin <- any(grepl("strings.bin", fleLst))

    # load the meta-data (global and data column attributes of the data set) and the extended
    # data (value labels)
    mtaDta <- getTxt(fleInp, "metadata.json")[["dataSet"]]
    xtdDta <- getTxt(fleInp, "xdata.json")
    binHdl <- getHdl(fleInp, "data.bin", "rb")
    if (strBin) {
        strHdl <- getHdl(fleInp, "strings.bin", "rb")
        strPos <- 0
    }

    # check meta-data, change weights if NULL
    if (!all(grepl(grpMta, names(mtaDta)))) stop("Unimplemeted field in the meta data (data frame).")

    # determine rows and columns and create data frame
    rowNum <- mtaDta$rowCount
    colNum <- mtaDta$columnCount
    if (length(mtaDta$fields) != colNum) stop("Number of fields in the metadata is not matching up the number of columns.")
    dtaFrm <- stats::setNames(data.frame(matrix(NA, nrow = rowNum, ncol = colNum)), vapply(mtaDta$fields, "[[", character(1), "name"))
    fltLst <- c()

    # iterate through fields
    for (i in seq_len(colNum)) {
        # assign metadate for the current column, and check whether there are any unimplemented entries
        mtaCol <- mtaDta$fields[[i]]
        if (!all(grepl(grpMta, names(mtaCol)))) stop("Unimplemeted field in the meta data (column).")

        # type: determines the format in the binary file
        if        (chkFld(mtaCol, "type", "integer")) {
            crrCol <- readBin(binHdl, integer(), n = rowNum)
        } else if (chkFld(mtaCol, "type", "number"))  {
            crrCol <- readBin(binHdl,  double(), n = rowNum)
        } else if (chkFld(mtaCol, "type", "string"))  {
            crrCol <- vector("character", length = rowNum)
            crrIdx <- readBin(binHdl, integer(), n = rowNum)
            if (rowNum > 0) {
                for (j in seq(rowNum)) {
                    if (is.na(crrIdx[j])) next
                    if (crrIdx[j] == strPos) {
                        crrCol[j] <- readBin(strHdl, "character", n = 1)
                        strPos <- strPos + length(charToRaw(crrCol[j])) + 1
                    } else {
                        stop("Mismatch between the assumed and the actual position in the file when reading string variables.")
                    }
                }
            }
            rm(crrIdx)
        } else {
            stop(sprintf("Variable type \"%s\" not implemented.", mtaCol$type))
        }

        # assign name of the current column, add value labels, assign crrCol to dtaFrm, and add attributes (if present)
        crrNme <- mtaCol$name
        dtaFrm[[crrNme]] <- valLbl(crrCol, mtaCol, xtdDta)
        if (chkFld(mtaCol, "measureType", "ID")) attr(dtaFrm[[crrNme]], "jmv-id")   <- TRUE
        if (chkFld(mtaCol, "description", ".+")) attr(dtaFrm[[crrNme]], "jmv-desc") <- mtaCol[["description"]]
        if (chkFld(mtaCol, "columnType", "Filter") || chkFld(mtaCol, "name", "^Filter [0-9]+$")) fltLst <- c(fltLst, i)

        if (sveAtt) {
            dtaFrm[crrNme] <- setAtt(names(mtaFld),   mtaCol, dtaFrm[crrNme])
        } else {
            dtaFrm[crrNme] <- setAtt("missingValues", mtaCol, dtaFrm[crrNme])
        }

        if (rmMsVl) {
            mssLst <- attr(dtaFrm[[crrNme]], "missingValues")
            if (length(mssLst) > 0) {
               crrAtt <- attributes(dtaFrm[crrNme])
               rmvLvl <- rep(FALSE, length(levels(dtaFrm[[crrNme]])))
               for (j in seq_along(mssLst)) {
                   dtaFrm[[crrNme]][eval(parse(text = paste0("dtaFrm[[\"", crrNme, "\"]]", mssLst[[j]])))] <- NA
                   rmvLvl <- rmvLvl | eval(parse(text = paste0("levels(dtaFrm[[\"", crrNme, "\"]]) ", mssLst[j])))
               }
               dtaFrm[[crrNme]] <- dtaFrm[[crrNme]][, drop = TRUE]
               crrAtt$missingValues <- list()
               crrAtt$values <- crrAtt$values[!rmvLvl]
               dtaFrm[crrNme] <- setAtt(setdiff(names(crrAtt), names(attributes(dtaFrm[crrNme]))), crrAtt, dtaFrm[crrNme])
               rm(crrAtt, rmvLvl)
            }
        }
        rm(crrCol)
    }

    # close and remove the binary file(s)
    clsHdl(binHdl)
    rm(binHdl)
    if (strBin) {
        clsHdl(strHdl)
        rm(strHdl, strPos)
    }

    # handle filters
    if (useFlt) {
        fltInc <- rep(TRUE, dim(dtaFrm)[1])
        for (i in fltLst) fltInc <- fltInc & dtaFrm[[i]]
        dtaFrm <- dtaFrm[fltInc, ]
        dtaFrm[fltLst] <- NULL
    } else if (length(fltLst) > 0) {
        attr(dtaFrm, "fltLst") <- names(dtaFrm)[fltLst]
    }

    # store data frame attributes except rowCount, columnCount and fields (the first two available
    # as data frame dimensions, the latter stored in the variables / columns)
    if (sveAtt) {
        dtaFrm <- setAtt(setdiff(names(mtaGlb), c("rowCount", "columnCount", "fields")), mtaDta, dtaFrm)
    }

    # reset some attributes ("addedRows" [for the data.frame] and "edits" for each column) for jamovi templates (.omt)
    if (hasExt(fleInp, "omt")) {
        dtaFrm <- rstAtt(dtaFrm, "addedRows")
        for (crrNme in names(dtaFrm)) dtaFrm[[crrNme]] <- rstAtt(dtaFrm[[crrNme]], "edits")
    }

    # handle weights
    if (is.character(mtaDta$weights) && nzchar(mtaDta$weights)) {
        # TO-DO: check whether the protobuffers (.. weights) contain
        # anything useful beyond that information
        attr(dtaFrm, "jmv-weights-name") <- mtaDta$weights
        attr(dtaFrm, "jmv-weights")      <- as.vector(dtaFrm[, mtaDta$weights])
        # it would be possible to keep both jmv-weights-name and weights, but
        # this might be rather confusing, so the version jamovi uses internally
        # (jmv-weights-name) is chosen and the other (weights) disregarded
        attr(dtaFrm, "weights")          <- NULL
    }

    # import and extract syntax from the analyses
    if (getSyn) {
        anlLst <- fleLst[grepl("[0-9]+\\s\\w+/analysis", fleLst)]
        savSyn <- c()
        savPtB <- list()
        if (length(anlLst) > 0 && jmvPtB()) {
            for (anlNme in anlLst) {
                anlPtB <- RProtoBuf::read(jamovi.coms.AnalysisResponse, anlHdl <- getHdl(fleInp, anlNme, "rb"))
                clsHdl(anlHdl)
                rm(anlHdl)
                if (!grepl("empty$", dirname(anlNme))) {
                    savSyn <- c(savSyn, gsub("\\( ", "\\(", gsub("\\n\\s+", " ", fndSyn(anlPtB$results))))
                }
                anlPtB$results <- NULL
                savPtB[[anlNme]] <- anlPtB
            }
        }
        attr(dtaFrm, "syntax")   <- savSyn
        attr(dtaFrm, "protobuf") <- savPtB
    }

    # import the HTML output
    if (getHTM) {
        attr(dtaFrm, "HTML") <- getTxt(fleInp, "index.html")
    }

    # return the resulting data frame
    dtaFrm
}

valLbl <- function(crrCol = NULL, mtaCol = NULL, xtdDta = NULL) {
    crrNme <- mtaCol$name
    if (any(crrNme == names(xtdDta))) {
        if        (chkFld(mtaCol, "columnType", "Filter") || chkFld(mtaCol, "name", "^Filter [0-9]+$")) {
            crrCol <- as.logical(crrCol)
        } else if (chkFld(mtaCol, "columnType", "Data|Recoded")) {
            crrCol <- factor(crrCol, levels = unlist(lapply(xtdDta[[crrNme]]$labels, function(m) m[1])),
                                     labels = unlist(lapply(xtdDta[[crrNme]]$labels, function(m) m[2])),
                                     ordered = chkFld(mtaCol, "measureType", "Ordinal"))
            if (identical(sort(levels(crrCol)), c("0", "1")))        crrCol <- as.logical(gsub("^1$", "TRUE", gsub("^0$", "FALSE", crrCol)))
            if (identical(sort(levels(crrCol)), c("FALSE", "TRUE"))) crrCol <- as.logical(crrCol)
            if (!is.logical(crrCol) && chkFld(mtaCol, "dataType", "Integer") &&
              all(vapply(xtdDta[[crrNme]]$labels, function(m) grepl("^\\d+$", m[2]) && m[1] == as.integer(m[2]), logical(1)))) {
                attr(crrCol, "values") <- unlist(lapply(xtdDta[[crrNme]]$labels, function(m) m[1]))
            }
        } else {
            stop(sprintf("Error when reading value label - likely the column type is not implemented (yet): %s - %s - %s",
                         crrNme, mtaCol$dataType, mtaCol$columnType))
        }
    }

    crrCol
}

# =================================================================================================
# read_all: for reading data files from various formats (incl. functions that are called)
read_all <- function(fleInp = "", usePkg = c("foreign", "haven"), selSet = "", ...) {
    if (nchar(fleInp) == 0) stop("File name to the input data file needs to be given as parameter (fleInp = ...).")

    # check whether the file exists
    fleInp <- fmtFlI(fleInp, maxLng = 1)
    varArg <- list(...)
    usePkg <- match.arg(usePkg)
    dtaFrm <- NULL

    # OMV / OMT
    if        (hasExt(fleInp, c("omv", "omt"))) {
        dtaFrm <- tryCatch(do.call(read_omv, adjArg("read_omv", list(fleInp = fleInp), varArg, "fleInp")),
                           error   = function(errMsg) tryErr(fleInp, errMsg),
                           warning = function(wrnMsg) tryWrn(fleInp, wrnMsg))
    # CSV
    } else if (hasExt(fleInp, c("csv"))) {
        dtaFrm <- tryCatch(rmvQtn(do.call(utils::read.table, adjArg("read.table", list(file = fleInp, sep = ",",  quote = "\"", header = TRUE, fill = TRUE), varArg, "file"))),
                           error   = function(errMsg) tryErr(fleInp, errMsg),
                           warning = function(wrnMsg) tryWrn(fleInp, wrnMsg))
    # TSV
    } else if (hasExt(fleInp, c("tsv"))) {
        dtaFrm <- tryCatch(rmvQtn(do.call(utils::read.table, adjArg("read.table", list(file = fleInp, sep = "\t", quote = "\"", header = TRUE, fill = TRUE), varArg, "file"))),
                           error   = function(errMsg) tryErr(fleInp, errMsg),
                           warning = function(wrnMsg) tryWrn(fleInp, wrnMsg))
    # Rdata
    } else if (hasExt(fleInp, c("rdata", "rda"))) {
        dtaFrm <- tryCatch({
                             load(fleInp, rdaTmp <- new.env())
                             if (length(rdaTmp) != 1 && selSet == "") stop("The Rdata-file must include only one object.")
                             rdaTmp[[ifelse(selSet == "", names(rdaTmp)[1], selSet)]]
                           },
                           error   = function(errMsg) tryErr(fleInp, errMsg),
                           warning = function(wrnMsg) tryWrn(fleInp, wrnMsg))
        if (methods::is(dtaFrm, "tbl_df") || any(vapply(dtaFrm, function(C) methods::is(C, "haven_labelled"), logical(1))))
            dtaFrm <- clnTbb(dtaFrm, c("format.sas", "format.spss", "format.stata", "display_width"), jmvLbl = TRUE)
    # RDS
    } else if (hasExt(fleInp, c("rds"))) {
        dtaFrm <- tryCatch(do.call(readRDS, adjArg("readRDS", list(file = fleInp), varArg, "file")),
                           error   = function(errMsg) tryErr(fleInp, errMsg),
                           warning = function(wrnMsg) tryWrn(fleInp, wrnMsg))
        if (methods::is(dtaFrm, "tbl_df") || any(vapply(dtaFrm, function(C) methods::is(C, "haven_labelled"), logical(1))))
            dtaFrm <- clnTbb(dtaFrm, c("format.sas", "format.spss", "format.stata", "display_width"), jmvLbl = TRUE)
    # SPSS (haven / foreign)
    } else if (hasExt(fleInp, c("sav", "zsav"))) {
        dtaFrm <-  getSPSS(fleInp, usePkg, varArg)
    # Stata (haven / foreign)
    } else if (hasExt(fleInp, c("dta"))) {
        dtaFrm <- getStata(fleInp, usePkg, varArg)
    # SAS (haven / foreign [not all formats])
    } else if (hasExt(fleInp, c("sas7bdat", "sd2", "sd7", "xpt", "stx", "stc"))) {
        dtaFrm <-   getSAS(fleInp, usePkg, varArg)
    }

    # check whether the input data are a data frame with the correct dimensions
    if (!is.null(dtaFrm)) chkDtF(dtaFrm)

    dtaFrm
}


# =================================================================================================
# helper functions for read_omv and read_all (in alphabetical order)

chkMnf <- function(fleOMV = "", fleMnf = c("")) {
    if (length(fleMnf) < 1) {
        stop(sprintf("File \"%s\" has not the correct file format (is missing the jamovi-file-manifest).", basename(fleOMV)))
    }

    # check the version information in the manifest and whether they are currently supported
    # [[1]] points to the first manifest file, in case both (MANIFEST.MF and meta) exist
    crrTxt <- getTxt(fleOMV, fleMnf[[1]])
    if (length(crrTxt) != length(lstMnf) || !all(grepl(paste(vapply(lstMnf, "[[", character(1), 1), collapse = "|"), crrTxt))) {
        stop(sprintf(paste("The file you are trying to read (%s) has an improper manifest file (meta) and is likely corrupted.",
                           "If the error persists, send the file to sebastian.jentschke@uib.no!"), basename(fleOMV)))
    }
    for (i in seq_along(lstMnf)) {
        # if no version number is given, skip this entry
        if (length(lstMnf[[i]]) == 1) break
        # compare versions, and if the current version is higher then the version defined in lstMnf, issue a warning
        crrVer <- trimws(strsplit(crrTxt[grepl(paste0(lstMnf[[i]][1], ":"), crrTxt)], ":")[[1]])[-1]
        if (utils::compareVersion(crrVer, lstMnf[[i]][-1]) > 0) {
             warning(sprintf(paste("The file that you are trying to read (%s) was written with a version of jamovi that currently is not implemented",
                                   "(%s: %s vs. %s) and therefore may be read incorrectly. Please send the file to sebastian.jentschke@uib.no!"),
                                   basename(fleOMV), lstMnf[[i]][1], crrVer, lstMnf[[i]][-1]))
        }
    }

    TRUE
}

chkSyn <- function(resElm = NULL) {
   # checks whether the results element is a syntax entry
   # it has to have the name syntax and the preformatted attribute must not be empty
   utils::hasName(resElm, "name")         && resElm[["name"]]         == "syntax" &&
   utils::hasName(resElm, "preformatted") && resElm[["preformatted"]] != ""
}

clsHdl <- function(crrHdl = NULL) {
    crrFle <- summary(crrHdl)$description
    close(crrHdl)
    unlink(crrFle)
    rm(crrFle, crrHdl)
}

clnFgn <- function(dtaFrm = NULL) {
    if (is.null(dtaFrm)) return(dtaFrm)
    chkDtF(dtaFrm)

    if (chkAtt(dtaFrm, "variable.labels")) {
        varLbl <- attr(dtaFrm, "variable.labels")
        Encoding(varLbl) <- "latin1"
        for (crrNme in names(dtaFrm)) {
            if (crrNme %in% names(varLbl) && varLbl[[crrNme]] != "") {
                attr(dtaFrm[[crrNme]], "jmv-desc") <- cnvUTF(varLbl[[crrNme]])
            }
        }
        attr(dtaFrm, "variable.labels") <- NULL
    }

    dtaFrm
}

clnTbb <- function(dtaFrm = NULL, rmvAtt = c(), jmvLbl = FALSE) {
    if (is.null(dtaFrm)) return(dtaFrm)

    # convert tibble to data.frame (remove tbl_df, tbl from class)
    class(dtaFrm) <- setdiff(class(dtaFrm), c("tbl_df", "tbl", rmvAtt))
    chkDtF(dtaFrm)

    for (crrNme in names(dtaFrm)) {
        # convert haven_labelled to factors
        if (methods::is(dtaFrm[[crrNme]], "haven_labelled")) {
            tmpLbl <- gsub("value label", "", names(attr(dtaFrm[[crrNme]], "labels")))
            if (all(nzchar(tmpLbl))) {
                dtaFrm[[crrNme]] <- cnvCol(dtaFrm[[crrNme]], "factor")
            } else if (!any(nzchar(tmpLbl))) {
                class(dtaFrm[[crrNme]]) <- setdiff(class(dtaFrm[[crrNme]]), c("haven_labelled", "vctrs_vctr"))
                attr(dtaFrm[[crrNme]], "labels") <- NULL
                if (is.numeric(dtaFrm[[crrNme]]) && all(dtaFrm[[crrNme]] %% 1 == 0, na.rm = TRUE))
                    class(dtaFrm[[crrNme]]) <- gsub("numeric", "integer", class(dtaFrm[[crrNme]]))
            } else {
                tmpLbl[!nzchar(tmpLbl)] <- sprintf("%d", which(!nzchar(tmpLbl)))
                names(attr(dtaFrm[[crrNme]], "labels")) <- tmpLbl
                dtaFrm[[crrNme]] <- cnvCol(dtaFrm[[crrNme]], "factor")
            }
        }
        # remove attributes given in rmvAtt
        for (crrAtt in rmvAtt) {
            if (! is.null(attr(dtaFrm[[crrNme]], crrAtt))) {
                attr(dtaFrm[[crrNme]], crrAtt) <- NULL
            }
        }
        # convert label to jamovi label
        if (jmvLbl && !is.null(attr(dtaFrm[[crrNme]], "label"))) {
            if (nzchar(gsub("variable label", "", attr(dtaFrm[[crrNme]], "label"))))
                attr(dtaFrm[[crrNme]], "jmv-desc") <- attr(dtaFrm[[crrNme]], "label")
            attr(dtaFrm[[crrNme]], "label") <- NULL
        }
    }

    dtaFrm
}

fndSyn <- function(resElm = NULL) {
    if (chkSyn(resElm)) {
        resElm[["preformatted"]]
    } else if (utils::hasName(resElm, "group") && length(resElm[["group"]]) > 0) {
        for (obj in resElm[["group"]][["elements"]]) {
            ret <- Recall(obj)
            if (!is.null(ret)) return(ret)
        }
    }
}

getHdl <- function(fleOMV = "", crrFle = "", crrMde = "r") {
    tryCatch({
                 zip::unzip(fleOMV, crrFle, exdir = tempdir(), junkpaths = TRUE)
                 file(file.path(tempdir(), list.files(path = tempdir(), pattern = basename(crrFle))), crrMde)
             },
             error = function(errMsg) {
                 message(sprintf("The file \"%s\" could not be extracted from \"%s\".\nPlease send the file to sebastian.jentschke@uib.no!\nError message: %s\n",
                           crrFle, fleOMV, errMsg))
                 return(invisible(NULL))
             }
        )
}

getSAS   <- function(fleInp = "", usePkg = "", varArg = list()) {
    # SAS data (haven)
    if (hasExt(fleInp, c("sas7bdat", "sd2", "sd7"))) {
        if        (usePkg == "haven"   && hasPkg("haven"))   {
            hvnTmp <- tryCatch(do.call(haven::read_sas, adjArg("haven::read_sas", list(data_file = fleInp), varArg, "data_file")),
                               error   = function(errMsg) tryErr(fleInp, errMsg),
                               warning = function(wrnMsg) tryErr(fleInp, wrnMsg))
            if (is.null(hvnTmp)) return(hvnTmp)
            clnTbb(hvnTmp, c("format.sas", "display_width"), jmvLbl = TRUE)
        } else {
            stop(sprintf("In order to read the SAS-file \"%s\" the R-packages \"haven\" needs to be installed.", basename(fleInp)))
        }
    # SAS-transport-files (haven / foreign)
    } else if (hasExt(fleInp, c("xpt", "stx", "stc"))) {
        if        (usePkg == "haven"   && hasPkg("haven"))   {
            hvnTmp <- tryCatch(do.call(haven::read_xpt, adjArg("haven::read_xpt", list(file = fleInp), varArg, "file")),
                               error   = function(errMsg) tryErr(fleInp, errMsg),
                               warning = function(wrnMsg) tryErr(fleInp, wrnMsg))
            clnTbb(hvnTmp, c("format.sas", "display_width"), jmvLbl = TRUE)
        } else if (usePkg == "foreign" && hasPkg("foreign")) {
            fgnTmp <- tryCatch(do.call(foreign::read.xport, adjArg("foreign::read.xport", list(file = fleInp), varArg, c("file"))),
                               error   = function(errMsg) tryErr(fleInp, errMsg))
            clnFgn(fgnTmp)
        } else {
            stop(sprintf("In order to read the SAS-transport-file \"%s\" either of the R-packages \"haven\" or \"foreign\" needs to be installed.", basename(fleInp)))
        }
    }
}

getSPSS  <- function(fleInp = "", usePkg = "", varArg = list()) {
    if        (usePkg == "haven"   && hasPkg("haven"))   {
        hvnTmp <- tryCatch(do.call(haven::read_sav, adjArg("haven::read_sav", list(file = fleInp), varArg, "file")),
                           error   = function(errMsg) tryErr(fleInp, errMsg),
                           warning = function(wrnMsg) tryErr(fleInp, wrnMsg))
        clnTbb(hvnTmp, c("format.spss", "display_width"), jmvLbl = TRUE)
    } else if (usePkg == "foreign" && hasPkg("foreign")) {
        fgnTmp <- tryCatch(suppressWarnings(do.call(foreign::read.spss,
                             adjArg("foreign::read.spss", list(file = fleInp, to.data.frame = TRUE, trim_values = TRUE, trim.factor.names = TRUE),
                               varArg, c("file", "to.data.frame", "trim_values", "trim.factor.names")))),
                           error   = function(errMsg) tryErr(fleInp, errMsg))
        clnFgn(fgnTmp)
    } else {
        stop(sprintf("In order to read the SPSS-file \"%s\" either of the R-packages \"haven\" or \"foreign\" needs to be installed.", basename(fleInp)))
    }
}

getStata  <- function(fleInp = "", usePkg = "", varArg = list()) {
    # NB: more recent versions of the Stata-format require "haven" and can't be read with foreign
    usePkg <- ifelse(grepl("^<stata_dta><header>", readBin(fleInp, character(), n = 1)), "haven", usePkg)
    if        (usePkg == "haven"   && hasPkg("haven"))   {
        hvnTmp <- tryCatch(do.call(haven::read_dta, adjArg("haven::read_dta", list(file = fleInp), varArg, "file")),
                           error   = function(errMsg) tryErr(fleInp, errMsg),
                           warning = function(wrnMsg) tryErr(fleInp, wrnMsg))
        clnTbb(hvnTmp, c("format.stata", "display_width"), jmvLbl = TRUE)
    } else if (usePkg == "foreign" && hasPkg("foreign")) {
        tmpFgn <- tryCatch(do.call(foreign::read.dta, adjArg("foreign::read.dta", list(file = fleInp), varArg, c("file"))),
                           error = function(errMsg) tryErr(fleInp, errMsg))
        clnFgn(tmpFgn)
    } else {
        stop(sprintf("In order to read the Stata-file \"%s\" either of the R-packages \"haven\" or \"foreign\" needs to be installed.", basename(fleInp)))
    }
}

getTxt <- function(fleOMV = "", crrFle = "") {
    crrTxt <- readLines(crrHdl <- getHdl(fleOMV, crrFle), warn = FALSE)
    clsHdl(crrHdl)
    rm(crrHdl)

    # depending on whether the original was a JSON file or not, return the appropriate result
    if (hasExt(crrFle, "json")) {
        crrTxt <- jsonlite::fromJSON(crrTxt, simplifyVector = FALSE)
    }

    crrTxt
}

rmvQtn <- function(dtaFrm = NULL) {
    for (crrNme in names(which(vapply(dtaFrm, is.character, logical(1))))) {
        dtaFrm[[crrNme]] <- trimws(gsub("\"", "", dtaFrm[[crrNme]]))
    }

    dtaFrm
}

tryErr <- function(fleInp = "", errMsg = NULL) {
    message(sprintf("File \"%s\" couldn\'t be read.\nThe error message was: %s\n", basename(fleInp), conditionMessage(errMsg)))
    return(invisible(NULL))
}

tryWrn <- function(fleInp = "", wrnMsg = NULL) {
    message(sprintf("Warnings were issued when reading the file \"%s\".\nThe warning was: %s\n", basename(fleInp), conditionMessage(wrnMsg)))
    return(invisible(NULL))
}
