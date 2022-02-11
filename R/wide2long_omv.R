#' Converts .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org) from wide to long format
#'
#' @param fleInp Name (including the path, if required) of the data file to be read ('FILENAME.omv'; default: '')
#' @param fleOut Name (including the path, if required) of the data file to be written ('FILENAME.omv'; default: ''); if empty, FILENAME from fleInp is extended with '_long'
#' @param varLst List / set of variables that are to be transformed into single (time-varying) variables in long format (default: c())
#' @param varSep Character that separates the variables in varLst into a time-varying part and a part that forms the variable name in long format ('.' in 'VAR.1', 'VAR.2', default: '.')
#' @param varID  Name(s) of one or more variables that (is created to) identify the same group / individual (if empty, 'id' is added with row numbers identifying cases; default: '')
#' @param varTme Name of the variable that (is created to) differentiate multiple records from the same group / individual (if empty, 'time' is added with a marker for each time-varying part; default: '')
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the order returned from reshape is kept; default: c())
#' @param usePkg Name of the package: 'haven' or 'foreign' that shall be used to read SPSS, Stata and SAS files; 'haven' is the default (it is more comprehensive), but with problems you may try 'foreign'
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @details
#' If varLst is empty, it is tried to generate it using all variables in the data frame except those defined by varID. For further arguments, see the help for reshape (where varLst ~ varying, varSep ~ sep,
#' varID ~ idvar, varTme ~ timevar).
#' varSrt is a character vector containing column names that are used to sort the data frame before it is written.
#' The ellipsis-parameter (...) can be used to submit arguments / parameters to the functions that are used for transforming or reading the data. The transformation uses 'reshape'. When reading the
#' data, the functions are: 'read_omv' (for jamovi-files), 'read.table' (for CSV / TSV files; using similar defaults as 'read.csv' for CSV and 'read.delim' for TSV which both are based upon
#' 'read.table' but with adjusted defaults for the respective file types), 'readRDS' (for rds-files), 'read_sav' (needs R-package 'haven') or 'read.spss' (needs R-package 'foreign') for SPSS-files,
#' 'read_dta' ('haven') / 'read.dta' ('foreign') for Stata-files, 'read_sas' ('haven') for SAS-data-files, and 'read_xpt' ('haven') / 'read.xport' ('foreign') for SAS-transport-files.
#' Please note that the R-packages 'haven' and 'foreign' are not marked as 'Imports' (i.e., they are not installed by default). If you wish to convert files from SPSS, SAS or Stata and haven't installed
#' them yet, please install them manually (e.g., `install.packages('haven', dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' }
#'
#' @export long2wide_omv
#'
wide2long_omv <- function(fleInp = "", fleOut = "", varLst = c(), varSep = ".", varID = "", varTme = "", varSrt = c(), usePkg = c("haven", "foreign"), selSet = "", ...) {

    # check and format input and output files, handle / check further input arguments
    fleInp <- fmtFlI(fleInp, maxLng = 1);
    fleOut <- fmtFlO(fleOut, fleInp, rplExt = "_long.omv");
    usePkg <- match.arg(usePkg);
    varArg <- list(...);

    # read file
    dtaFrm <- read_all(fleInp, usePkg, selSet, varArg);

    # check whether varLst is empty; if so, all variables except the one given as varID are used as varLst
    if (is.null(varLst)) {
        if (nzchar(varID)) {
            warning(sprintf("Variable list (varLst) was generated using all variables in the data frame except those in varID (%s).", paste(varID, collapse = ",")));
            varLst <- setdiff(names(dtaFrm), varID);
        } else {
            stop("At the very minimum either a variable list to be transformed (varLst) or an ID variable (varID; all variables except varID are used as varLst) has to be given as input arguments.");
        }
    }
    # check whether the variables in varLst are contained in the data frame and whether they all contain the separator
    chkVar(dtaFrm, varLst);
    if (!nzchar(varSep) || !all(grepl(varSep, varLst, fixed = TRUE))) {
        stop(sprintf("The variable separator (varSep, \"%s\") must not be empty and has to be contained in all variables in the variable list (varLst).\nDeviating variables: %s\n",
                     varSep, paste(varLst[!grepl(varSep, varLst, fixed = TRUE)], collapse = ", ")));
    }
    # carry out the transformation (limiting the variable arguments - varArg - to those permitted by "reshape")
    crrArg <- list(data = dtaFrm, direction = "long", varying = varLst, sep = varSep);
    if (nzchar(varID))  crrArg <- c(crrArg, list(idvar   = varID));
    if (nzchar(varTme)) crrArg <- c(crrArg, list(timevar = varTme));
    dtaFrm <- do.call(stats::reshape, adjArg("stats::reshape", crrArg, varArg, c("data", "direction", "varying", "sep")));

    # correct labels (if available)
    dtaFrm <- rplLbl(dtaFrm);

    # sort data set (if varSrt is not empty)
    dtaFrm <- srtFrm(dtaFrm, varSrt);

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
