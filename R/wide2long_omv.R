#' Converts .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org) from wide to long format
#'
#' @param fleInp name (including the path, if required) of the data file to be read ("FILENAME.omv"; default: "")
#' @param fleOut name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, FILENAME from fleInp is extended with "_long"
#' @param varLst list / set of variables that are to be transformed into single (‘time-varying’) variables in long format (default: c())
#' @param varSep character that separates the variables in varLst into a time-varying part and a part that forms the variable name in long format ("." in "VAR.1", "VAR.2", default: ".")
#' @param varID  name(s) of one or more variables that (is created to) identify the same group / individual (if empty, "id" is added with row numbers identifying cases; default: "")
#' @param varTme name of the variable that (is created to) differentiate multiple records from the same group / individual (if empty, "time" is added with a marker for each time-varying part; default: "")
#' @param usePkg name of the package ("haven" or "foreign") that shall be used to read SPSS, Stata and SAS files; "haven" is the default (it is more comprehensive), but with problems you may try "foreign"
#' @param selSet name of the data set that is to be selected from the workspace (only applies when reading .Rdata-files)
#' @param ...
#'
#' @details
#' If varLst is empty, it is tried to generate it using all variables in the data frame except those defined by varID. For further arguments, see the help for reshape (where varLst ~ varying, varSep ~ sep,
#' varID ~ idvar, varTme ~ timevar).
#' The ellipsis-parameter can be used to submit arguments / parameters to the functions that are used for transforming or reading the data. The transformation uses "reshape". When reading the data, the
#' functions are: "read_omv" (for jamovi-files), "read.table" (for CSV / TSV files; using similar defaults as "read.csv" for CSV and "read.delim" for TSV which both are based upon "read.table" but with
#' adjusted defaults for the respective file types), "readRDS" (for rds-files), "read_sav" (needs R-package "haven") or "read.spss" (needs R-package "foreign") for SPSS-files, read_dta ("haven") /
#' read.dta ("foreign") for Stata-files, read_sas ("haven") for SAS-data-files, and read_xpt ("haven") / read.xport ("foreign") for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' }
#'
#' @export long2wide_omv
#'
wide2long_omv <- function(fleInp = "", fleOut = "", varLst = c(), varSep = ".", varID = "", varTme = "", usePkg = c("haven", "foreign"), selSet = "", ...) {

    # normalize the path of the input file and then check whether the file exists and whether it is of a supported file type
    # if fleOut is empty, fleInp is used with the file name extended by "_wide" and the extension set to ".omv"
    fleInp <- nrmFle(fleInp);
    chkFle(fleInp);
    chkExt(fleInp, vldExt);
    fleOut <- ifelse(nzchar(fleOut), nrmFle(fleOut), sub(paste0(".", tools::file_ext(fleInp)), "_long.omv", fleInp));

    # handle / check further input arguments
    usePkg <- match.arg(usePkg);
    varArg <- list(...);

    # read file
    dtaFrm <- read_all(fleInp, usePkg, selSet, varArg);

    # transform data set
    # [1] check whether varLst is empty; if so, all variables except the one given as varID are used as varLst
    if (is.null(varLst)) {
        if (nzchar(varID)) {
            warning(sprintf("Variable list (varLst) was generated using all variables in the data frame except those in varID (%s).", paste(varID, collapse = ",")));
            varLst <- setdiff(names(dtaFrm), varID);
        } else {
            stop("At the very minimum either a variable list to be transformed (varLst) or an ID variable (varID; all variables except varID are used as varLst) has to be given as input arguments.");
        }
    }
    # [2] check whether the variables in varLst are contained in the data frame and whether they all contain the separator
    chkVar(dtaFrm, varLst);
    if (!nzchar(varSep) || !all(grepl(varSep, varLst, fixed = TRUE))) {
        stop(sprintf("The variable separator (varSep, \"%s\") must not be empty and has to be contained in all variables in the variable list (varLst).\nDeviating variables: %s\n",
                     varSep, paste(varLst[!grepl(varSep, varLst, fixed = TRUE)], collapse = ", ")));
    }
    # [3] carry out the transformation (limiting the variable arguments - varArg - to those permitted by "reshape")
    crrArg <- list(data = dtaFrm, direction = "long", varying = varLst, sep = varSep);
    if (nzchar(varID))  crrArg <- c(crrArg, list(idvar   = varID));
    if (nzchar(varTme)) crrArg <- c(crrArg, list(timevar = varTme));
    dtaFrm <- do.call(reshape, adjArg("reshape", crrArg, varArg, c("data", "direction", "varying", "sep")));

    # [4] correct labels (if available)
    dtaFrm <- rplLbl(dtaFrm);

    # write file
    write_omv(dtaFrm, fleOut)
}

rplLbl <- function(dtaFrm = NULL) {
    varTme <- attr(attr(dtaFrm, "reshapeLong")$varying, "times");
    # return if the time-vector only contains consecutive numbers
    if (all(varTme == seq_along(varTme))) return(dtaFrm);
    varTme <- as.character(varTme[1]);
    for (crrAtt in c("jmv-desc", "label")) {
        lstAtt <- sapply(dtaFrm, attr, crrAtt);
        lstAtt <- lstAtt[!sapply(lstAtt, is.null)];
        for (crrNme in names(lstAtt)) {
            # removes the content of the first occurence of the time variable and any non alphanumeric characters at the end
            attr(dtaFrm[[crrNme]], crrAtt) <- trimws(gsub("[[:punct:]]", "", trimws(gsub(varTme, "", attr(dtaFrm[[crrNme]], crrAtt)))));
        }
    }
    dtaFrm
}
