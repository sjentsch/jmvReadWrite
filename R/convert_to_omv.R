#' Convert data files (CSV, R, other statistics packages) into .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org)
#'
#' @param fleInp name (including the path, if required) of the data file to be read ("FILENAME.ext"; default: ""); supports CSV and R-files natively, other file types if "haven" or "foreign" are installed
#' @param fleOut name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, the extension of fleInp is replaced with ".omv'
#' @param usePkg name of the package ("haven" or "foreign") that shall be used to read SPSS, Stata and SAS files; "haven" is the default (it is more comprehensive), but with problems you may try "foreign"
#' @param selSet name of the data set that is to be selected from the workspace (only applies when reading .Rdata-files)
#' @param ...
#'
#' @details
#' The ellipsis-parameter can be used to submit arguments / parameters to the functions that are actually used for reading the data. These are: read_omv (for jamovi-files), read.table (for CSV / TSV
#' files), readRDS (for rds-files), read_spss (needs R-package "haven") or read.spss (needs R-package "foreign") for SPSS-files,  read_dta ("haven") / read.dta ("foreign") for Stata-files, read_sas
#' ("haven") for SAS-data-files, and read_xpt ("haven") / read.xport ("foreign") for SAS-transport-files. For reading CSV / TSV files, "convert_to_omv" uses similar defaults as "read.csv" (CSV) and
#' "read.delim" (TSV) which both are based upon "read.table" but with setting reasonable defaults for the respective file types.
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' }
#'
#' @export convert_to_omv
#'
convert_to_omv <- function(fleInp = "", fleOut = "", usePkg = c("haven", "foreign"), selSet = "", ...) {
    # check whether the file exists
    chkFle(fleInp)
    if (! hasExt(fleInp, c("csv", "tsv", "rdata", "rda", "rds", "sav", "zsav", "dta", "sas7bdat", "sd2", "sd7", "xpt", "stx", "stc"))) {
        warning(sprintf("The input file \"%s\" is not of a type that is suitable for conversion (CSV, TSV, Rdata + RDS, SPSS, Stata and SAS).", fleInp));
        return()
    }
    varArg <- list(...);
    usePkg <- match.arg(usePkg);
    # if fleOut is empty, fleInp is used with its file extension replaced with ".omv"
    fleOut <- ifelse(fleOut == "", sub(tools::file_ext(fleInp), "omv", fleInp), fleOut);

    # read file
    dtaFrm <- read_all(fleInp, usePkg, selSet, varArg);
    if (is.null(dtaFrm)) {
        stop(sprintf("Error when reading the data file \"%s\" - data set can\'t be saved to \"%s\".", basename(fleInp), basename(fleOut)));
    }

    # adjust settings / attributes

    # write file
    write_omv(dtaFrm, fleOut)

}

# =================================================================================================
# functions for reading data files from various formats

read_all <- function(fleInp = "", usePkg = c("haven", "foreign"), selSet = "", ...) {
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
        dtaFrm <- tryCatch(rmvQtn(do.call(read.table, adjArg("read.table", list(file = fleInp, sep = ",",  header = TRUE, fill = TRUE), varArg, "file"))),
                           error = function(errMsg) tryErr(fleInp, errMsg), warning = function(wrnMsg) tryWrn(fleInp, wrnMsg));
    # TSV
    } else if (hasExt(fleInp, c("tsv"))) {
        dtaFrm <- tryCatch(rmvQtn(do.call(read.table, adjArg("read.table", list(file = fleInp, sep = "\t", header = TRUE, fill = TRUE), varArg, "file"))),
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
                                 hvnTmp <- haven::as_factor(do.call(haven::read_spss, adjArg("haven::read_spss", list(file = fleInp), varArg, "file")), only_labelled = TRUE);
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
    
    # check whether all attributes conform with unicode and do some cleaning if required
    dtaFrm <- rplAtt(dtaFrm);
    
    dtaFrm
}

adjArg <- function(fcnNme = "", dflArg = list(), varArg = list(), fxdArg = c()) {
    chgArg <- setdiff(intersect(names(fcnArg(fcnNme)), names(varArg)), fxdArg);
    c(dflArg[setdiff(names(dflArg), chgArg)], varArg[chgArg])
}

tryErr <- function(fleInp = "", errMsg = NULL) {
    message(sprintf("File \"%s\" couldn't be read.\nThe error message was: %s\n", basename(fleInp), conditionMessage(errMsg)));
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
        lstAtt = sapply(dtaFrm[names(dtaFrm)], attr, crrAtt);
        lstAtt = lstAtt[!sapply(lstAtt, is.null)];
        if (!any(sapply(lstAtt, is.character))) break
        if (!all(sapply(lstAtt, is.character))) stop(sprintf("Some attribute values of \"%s\" are not of the type character.", crrAtt));
        for (crrCol in names(lstAtt)[sapply(lstAtt, function(x) !all(validEnc(x)))]) {
            attr(dtaFrm[[crrCol]], crrAtt) <- rplStr(attr(dtaFrm[[crrCol]], crrAtt), paste0(c(crrCol, crrAtt), collapse = " – "));
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
