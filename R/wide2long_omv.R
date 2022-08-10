#' Converts .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>) from wide to long format
#'
#' @param fleInp Name (including the path, if required) of the data file to be read ("FILENAME.omv"; default: ""); can be any supported file type, see Details below
#' @param fleOut Name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, FILENAME from fleInp is extended with "_long(file extension -> .omv)"
#' @param varLst List / set of variables that are to be transformed into single (time-varying) variables in long format (default: c())
#' @param varExc List / set of variables to be excluded from the variable list (default: c())
#' @param varID  Name(s) of one or more variables that (is created to) identify the same group / individual (if empty, "ID" is added with row numbers identifying cases; default: "ID")
#' @param varTme Name of the variable that (is created to) differentiate multiple records from the same group / individual (default: "cond"; a counter is added for each time-varying part)
#' @param varSep Character that separates the variables in varLst into a time-varying part and a part that forms the variable name in long format ("_" in "VAR_1", "VAR_2", default: "_")
#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the order returned from reshape is kept; default: c())
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ... Additional arguments passed on to methods; see Details below
#'
#' @details
#' If varLst is empty, it is tried to generate it using all variables in the data frame except those defined by varExc and varID. The variable(s) in varID have to be unique identifiers (in the original
#' dataset), those in varExc don't have this requirement. It is generally recommended that the variable names in varExc and varID should not include the variable separator (defined in varSep; default: "_")
#' For further arguments, see the help for reshape (where varLst ~ varying, varSep ~ sep, varID ~ idvar, varTme ~ timevar).
#' varSrt is a character vector containing column names that are used to sort the data frame before it is written.
#' The ellipsis-parameter (...) can be used to submit arguments / parameters to the functions that are used for transforming or reading the data. The transformation uses `reshape`. When reading the
#' data, the functions are: `read_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV which both are based upon
#' `read.table` but with adjusted defaults for the respective file types), `readRDS` (for rds-files), `read_sav` (needs R-package "haven") or `read.spss` (needs R-package "foreign") for SPSS-files,
#' `read_dta` ("haven") / `read.dta` ("foreign") for Stata-files, `read_sas` ("haven") for SAS-data-files, and `read_xpt` ("haven") / `read.xport` ("foreign") for SAS-transport-files. If you would like
#' to use "haven", it may be needed to install it manually (i.e., `install.packages("haven", dep = TRUE)`).
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite)
#' # generate a test dataframe with 100 (imaginary) participants / units of
#' # observation (ID), and 8 repeated measurements of variable (X_1, X_2, ...)
#' dtaInp <- cbind(data.frame(ID = as.character(seq(1:100))),
#'                 stats::setNames(
#'                     as.data.frame(matrix(runif(800, -10, 10), nrow = 100)),
#'                     paste0("X_", 1:8)))
#' cat(str(dtaInp))
#' # 'data.frame':	100 obs. of  9 variables:
#' #  $ ID : chr  "1" "2" "3" "4" ...
#' #  $ X_1: num  ...
#' #  $ X_2: num  ...
#' #  $ X_3: num  ...
#' #  $ X_4: num  ...
#' #  $ X_5: num  ...
#' #  $ X_6: num  ...
#' #  $ X_7: num  ...
#' #  $ X_8: num  ...
#' # this data set is stored as (temporary) RDS-file and later processed by wide2long
#' nmeInp <- paste0(tempfile(), ".rds")
#' nmeOut <- paste0(tempfile(), ".omv")
#' saveRDS(dtaInp, nmeInp)
#' wide2long_omv(fleInp = nmeInp, fleOut = nmeOut, varID = "ID", varTme = "measure",
#'     varLst = setdiff(names(dtaInp), "ID"), varSrt = c("ID", "measure"))
#' # it is required to give at least the arguments fleInp and varID
#' # "reshape" then assigns all variables expect the variable defined by varID to
#' # varLst (but throws a warning)
#' # varSrt enforces sorting the data set after the transformation (sorted, the
#' # measurements within one person come after another; unsorted all measurements
#' # for one repetition would come after another)
#'
#' # check whether the file was created and its size
#' cat(list.files(dirname(nmeOut), basename(nmeOut)))
#' # -> "file[...].omv" ([...] contains a random combination of numbers / characters
#' cat(file.info(nmeOut)$size)
#' # -> 6939 (approximate size; size may differ in every run [in dependence of how
#' #          well the generated random data can be compressed])
#' cat(str(read_omv(nmeOut, sveAtt = FALSE)))
#' # the data set is now transformed into long (and each the measurements is now
#' # indicated by the "measure")
#' # 'data.frame':	800 obs. of  3 variables:
#' #  $ ID     : Factor w/ 100 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 2 2 ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ measure: Factor w/ 8 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 1 2 ...
#' #   ..- attr(*, "missingValues")= list()
#' #  $ X      : num  ...
#' #   ..- attr(*, "missingValues")= list()
#'
#' unlink(nmeInp)
#' unlink(nmeOut)
#' }
#'
#' @export wide2long_omv
#'
wide2long_omv <- function(fleInp = "", fleOut = "", varLst = c(), varExc = c(), varID = "ID", varTme = "cond", varSep = "_", varSrt = c(), usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and format input and output files, handle / check further input arguments
    fleInp <- fmtFlI(fleInp, maxLng = 1)
    fleOut <- fmtFlO(fleOut, fleInp, rplExt = "_long.omv")
    usePkg <- match.arg(usePkg)
    varArg <- list(...)

    # read file
    dtaFrm <- read_all(fleInp, usePkg, selSet, varArg)
    dtaNmV <- names(dtaFrm)
    hasID  <- all(varID %in% dtaNmV)

    # check whether varLst is empty; if so, all variables except the one given as varID are used as varLst
    # otherwise, check whether the variables in varLst are contained in the data frame
    if (is.null(varLst)) {
        cat(paste0("Variable list (varLst) was generated using all variables in the data frame", ifelse(hasID || (!is.null(varExc) && all(nzchar(varExc))),
                  sprintf(" except those defined in varExc or varID (%s).", paste(c(varExc, rep(varID, hasID)), collapse = ", ")), "."), "\n"))
        varLst <- setdiff(dtaNmV, c(varExc, varID))
    } else {
        chkVar(dtaFrm, varLst)
    }
    # check whether they all contain the separator
    if (nzchar(varSep) && !all(grepl(varSep, varLst, fixed = TRUE))) {
        stop(sprintf("\n\nThe variable separator (varSep, \"%s\") must be contained in all variables in the variable list (varLst).\nDeviating variables: %s\n",
                     varSep, paste(varLst[!grepl(varSep, varLst, fixed = TRUE)], collapse = ", ")))
    }
    # use varSep to split the variable names in varLst
    if (nzchar(varSep)) {
        varSpl <- strsplit(varLst, varSep)
        lngSpl <- unique(unlist(lapply(varSpl, length)))
        if (length(lngSpl) != 1) {
            cat(paste0(paste0(varLst, collapse = ", "), "\n\n"))
            stop("The variable names in varLst need to have the same structure, i.e., the same number of separators within all variable names.")
        }
    } else {
        varSpl <- as.list(varLst)
        lngSpl <- 1
    }
    unqSpl <- c()
    for (i in seq_len(lngSpl)) unqSpl <- c(unqSpl, rep(i, length(unique(sapply(varSpl, "[[", i))) == 1))
    cntTme <- (lngSpl - length(unqSpl))

    # carry out the transformation (limiting the variable arguments - varArg - to those permitted by "reshape")
    # the transformation also corrects labels (if available)
    for (i in setdiff(seq(lngSpl, 1), unqSpl)) {
        crrTms <- unique(sapply(varSpl, "[[", i))
        crrArg <- list(data    = dtaFrm, direction = "long", idvar = varID, sep = varSep,
                       varying = unique(sapply(varSpl, function(x) paste(x[unique(sort(c(seq(1, i), unqSpl)))], collapse = varSep))),
                       timevar = paste0(varTme, rep(cntTme, (lngSpl - length(unqSpl)) > 1)), times = crrTms)
        if (all(crrArg$varying %in% crrTms)) {
            crrArg <- c(crrArg, list(v.names = "measure"))
        } else {
            crrArg <- c(crrArg, list(v.names = unique(sub(paste0(c(paste0("_", crrTms), paste0(crrTms, "_")), collapse = "|"), "", crrArg$varying))))
        }
        dtaFrm <- rplLbl(do.call(stats::reshape, adjArg("stats::reshape", crrArg, varArg,
                                                        c("data", "direction", "idvar", "sep", "varying", "times", "timevar", "v.names"))))
        dtaFrm[[crrArg$timevar]] <- as.factor(dtaFrm[[crrArg$timevar]])
        varID  <- c(varID, crrArg$timevar)
        cntTme <- cntTme - 1
    }
    # remove attributes from reshape attached to the data set
    attr(dtaFrm, "reshapeLong") <- NULL

    # sort data set (if varSrt is not empty, otherwise sort after the first variable in varID (defining the participant)
    if (length(varSrt) > 0) {
        dtaFrm <- srtFrm(dtaFrm, varSrt)
    } else {
        dtaFrm <- srtFrm(dtaFrm, varID[1])
    }
    # correct the column order (ID should come first, otherwise the order is kept with the transformed variables inserted into
    # were the respective original (i.e., before the transformation) variables were
    dtaFrm <- dtaFrm[, ordCol(names(dtaFrm), dtaNmV, varID, varLst)]

    # remove the jmv-id, and change the measurement type to "Nominal" (if it was ID; -> the ID variable may be used
    # in analyses, e.g. as random-effects-variable, and this wouldn't be possible if it were still marked as "ID")
    dtaFrm <- rmvID(dtaFrm, varID, hasID)

    # write file
    write_omv(dtaFrm, fleOut)
}

ordCol <- function(varNme = c(), dtaNmV = c(), varID = c(), varLst = c()) {
    splNmV <- gsub("\\|$|^\\|", "", strsplit(paste0(setdiff(dtaNmV, varID), collapse = "|"), paste0(varLst, collapse = "\\|"))[[1]])
    varOrd <- c(varID[1], rep(strsplit(splNmV[1], "\\|")[[1]], length(splNmV) > 0), sort(varID[-1]),
                setdiff(varNme, c(varID, setdiff(dtaNmV, varLst))), rep(strsplit(splNmV[2], "\\|")[[1]], length(splNmV) > 1))
    if (length(c(setdiff(varNme, varOrd), setdiff(varOrd, varNme))) != 0) {
        stop(paste0("Mismatch between old and new variable order - old: ", paste0(varNme, collapse = ", "), "; new: ", paste0(varOrd, collapse = ", "), "."))
    }

    varOrd
}

rmvID <- function(dtaFrm = NULL, varID = c(), hasID = TRUE) {
    if ("jmv-id" %in% names(attributes(dtaFrm[, varID[1]]))) attr(dtaFrm[, varID[1]], "jmv-id") <- NULL
    if (!hasID || ("measureType" %in% names(attributes(dtaFrm[, varID[1]])) && attr(dtaFrm[, varID[1]], "measureType") == "ID")) {
        dtaFrm[, varID[1]] <- as.factor(dtaFrm[, varID[1]])
        attr(dtaFrm[, varID[1]], "measureType") <- "Nominal"
        attr(dtaFrm[, varID[1]], "dataType")    <- "Text"
    }

    dtaFrm
}

rplLbl <- function(dtaFrm = NULL) {
    varTme <- attr(attr(dtaFrm, "reshapeLong")$varying, "times")
    # return if the time-vector only contains consecutive numbers
    if (all(varTme == seq_along(varTme))) return(dtaFrm)
    varTme <- as.character(varTme[1])
    lstAtt <- sapply(dtaFrm, attr, "jmv-desc")
    lstAtt <- lstAtt[!sapply(lstAtt, is.null)]
    for (crrNme in names(lstAtt)) {
        # removes the content of the first occurence of the time variable and any non alphanumeric characters at the end
        attr(dtaFrm[[crrNme]], "jmv-desc") <- trimws(gsub(paste0("[[:punct:]]", varTme, "[[:punct:]]|", varTme), "", attr(dtaFrm[[crrNme]], "jmv-desc")))
    }

    dtaFrm
}
