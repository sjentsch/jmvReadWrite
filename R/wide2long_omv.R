#' Converts .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>) from wide to long format
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of
#'               any supported file type, see Details below
#' @param fleOut Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
#'               returned instead
#' @param varLst List / set of variables that are to be transformed into single (time-varying) variables in long format (default: c())
#' @param varExc Name of the variable(s) should be excluded from the transformation, typically this will be between-subject-variable(s) (default: c())
#' @param varID  Name(s) of one or more variables that (is created to) identify the same group / individual (if empty, "ID" is added with row numbers
#'               identifying cases; default: NULL)
#' @param varTme Name of the variable that (is created to) differentiate multiple records from the same group / individual (default: "cond"; a counter is added
#'               for each time-varying part)
#' @param varSep Character that separates the variables in varLst into a time-varying part and a part that forms the variable name in long format ("_" in
#'               "VAR_1", "VAR_2", default: "_")

#' @param varOrd Whether to arrange the variables before the transformation, so that they are in accordance with the different split levels (default: TRUE)

#' @param varSrt Variable(s) that are used to sort the data frame (see Details; if empty, the order returned from reshape is kept; default: c())
#' @param excLvl Integer (or vector of integers) determining which parts of the variable names in varLst shall not be transformed (default: NULL), see Details
#'               below
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
#'               base R), but "haven" is newer and more comprehensive
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below
#'
#' @return a data frame (only returned if `fleOut` is empty) where the input data set is converted from wide to long format
#'
#' @details
#' * If `varLst` is empty, it is tried to generate it using all variables in the data frame except those defined by `varExc` and `varID`. The variable(s) in
#'   `varID` need to be unique identifiers (in the original dataset), those in `varExc` don't have this requirement. It is recommended that the variable names
#'   in `varExc` and `varID` should not contain the variable separator (defined in `varSep`; default: "_").
#' * `varOrd` determines whether the variables are rearranged to match the order of split levels. Consider the `varLst` X_1, Y_1, X_2, Y_2. If `varOrd` were
#'   set to FALSE, the original order would be preserved and the second part of the variable name (1, 2, ...) would become condition 1, and the first part
#'   condition 2. In most cases, leaving `varOrd` set to TRUE is recommended.
#' * `varSrt` can be either a character or a character vector (with one or more variables respectively). The sorting order for a particular variable can be
#'   inverted with preceding the variable name with "-". Please note that this doesn't make sense and hence throws a warning for certain variable types (e.g.,
#'   factors).
#' * `exclLvl` points to a part of the variable names in `varLst` to be excluded. For example, if the variable name is `PART1_PART2_PART3` (split at _), then
#'   `excLvl` = 1 would exclude PART1 from the transformation. Quite often, one has more that one variable of a particular type (e.g., responses, reaction
#'   times, etc.). Those would typically be the first part of each variable name in `varLst` (the conditions then being PART2, PART3, and so on). `excLvl` = 1
#'   would exclude those variable types / categories from being transformed into long (i.e., they would be kept as separate columns).
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions that are used for transforming or reading the data. By
#'   clicking on the respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take.
#' * The transformation from long to wide uses `reshape`: `varID` matches (~) `idvar` in `reshape`, `varTme` ~ `timevar`, `varLst` ~ `varying`, and `varSep` ~
#'   `sep`. The help for `reshape` is very explanatory, click on the link under “See also” to access it, particularly what is explained under “Details”.
#' * The functions for reading and writing the data are: `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files; using similar
#'   defaults as `read.csv` for CSV and `read.delim` for TSV which both are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files),
#'   `read_sav` (needs R-package `haven`) or `read.spss` (needs R-package `foreign`) for SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for
#'   Stata-files, `read_sas` (`haven`) for SAS-data-files, and `read_xpt` (`haven`) / `read.xport` (`foreign`) for SAS-transport-files. If you would like to
#'   use `haven`, you may need to install it using `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `long2wide_omv` internally uses the following functions: The transformation from long to wide uses [stats::reshape()]. For reading and writing data
#'   files in different formats: [jmvReadWrite::read_omv()] and [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files,
#'   [load()] for reading .RData-files, [readRDS()] for .rds-files, [haven::read_sav()] or [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or
#'   [foreign::read.dta()] for Stata-files, [haven::read_sas()] for SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
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
#' nmeInp <- tempfile(fileext = ".rds")
#' nmeOut <- tempfile(fileext = ".omv")
#' saveRDS(dtaInp, nmeInp)
#' jmvReadWrite::wide2long_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "ID",
#'     varTme = "measure", varLst = setdiff(names(dtaInp), "ID"),
#'     varSrt = c("ID", "measure"))
#' # it is required to give at least the arguments dtaInp (if dtaInp is a data frame,
#' # fleOut needs to be provided too) and varID
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
#' cat(str(jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)))
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
wide2long_omv <- function(dtaInp = NULL, fleOut = "", varLst = c(), varExc = c(), varID = NULL, varTme = "cond", varSep = "_", varOrd = TRUE, varSrt = c(),
                          excLvl = NULL, usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    if (is.null(varID)) varID <- "ID"
    dtaFrm <- inp2DF(dtaInp, usePkg = usePkg, selSet = selSet, ...)
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
    # re-arrange the variable in the variable list so that they are in accordance with the order
    # of the splits in the variable names
    if (varOrd) varLst <- sort(varLst)
    # use varSep to split the variable names in varLst
    if (nzchar(varSep)) {
        varSpl <- strsplit(varLst, varSep)
        lngSpl <- unique(unlist(lapply(varSpl, length)))
        if (length(lngSpl) != 1) {
            stop(sprintf("The variable names in varLst need to have the same structure, i.e., the same number of separators within all variable names:\n%s\n\n",
              paste0(varLst, collapse = ", ")))
        }
    } else {
        varSpl <- as.list(varLst)
        lngSpl <- 1
    }

    # check excLvl: must be numeric and not contain any values that are outside lngSpl
    if (!is.null(excLvl) && length(excLvl) > 0 && (!is.numeric(excLvl) || excLvl < 1 || excLvl > lngSpl)) {
        stop(sprintf("excLvl must be numeric and must not be less (excLvl < 1) or more then the number of available levels (excLvl > %d).", lngSpl))
    }
    # dffSpl determines for each of the splits how soon the first element is differing; it is
    # assumed that the variables in varLst are hierarchically organized, i.e., one condition varies
    # first (here the difference should be smallest), for the next factor in the hierarchy, the
    # difference should be larger and so on; if factors are unique (i.e., if there is no difference
    # for this part of the split), an NA is returned; however: further below (in the while-loop),
    # NAs are used to indicate which positions have already been processed, setting NAs and the
    # levels to be excluded (excLvl) to Inf ensures that they would never be found as crrPos
    # (Inf is larger than any finite number and the while loop stops if there aren't any finite
    # numbers, i.e., values other than Inf or NA) left
    dffSpl <- rep(NA, lngSpl)
    for (i in seq(lngSpl)) dffSpl[i] <- which(sapply(varSpl, "[[", i)[1] != sapply(varSpl, "[[", i)[-1])[1]
    dffSpl[is.na(dffSpl)] <- Inf
    dffSpl[excLvl] <- Inf
    nmbTme <- sum(is.finite(dffSpl)) > 1

    # carry out the transformation
    crrNmV <- varLst
    while (any(is.finite(dffSpl))) {
        # crrPos: the smallest level that is valid (i.e., that is not unique or excluded because of excLvl)
        crrPos <- which(dffSpl == min(dffSpl, na.rm = TRUE))
        # crrTms is used as parameter in crrArg and the if-condition below
        crrTms <- unique(sapply(varSpl, "[[", crrPos))
        # assemble the list for varying, if crrTms are the only elements of left in varLst, an output
        # variable “measure” is used as target, otherwise crrVry is assembled as named list with the
        # target as name and all former variables for that step of the hierarchy as entries
        if (all(crrNmV %in% crrTms)) {
            crrVry <- list(measure = crrNmV)
        } else {
            vldPos <- which(!is.na(dffSpl))
            crrVry <- list()
            for (i in seq_along(varSpl)) {
                tmpTgt <- paste0(varSpl[[i]][setdiff(vldPos, crrPos)], collapse = varSep)
                crrVry[[tmpTgt]] <- unique(c(crrVry[[tmpTgt]], paste0(varSpl[[i]][vldPos], collapse = varSep)))
            }
        }
        # assemble the arguments to call reshape (limiting the variable arguments - ... - to those permitted)
        # rplAtt also corrects labels (if available) and variable names
        crrArg <- list(data = dtaFrm, direction = "long", idvar = varID, sep = varSep, varying = crrVry, v.names = names(crrVry),
                       timevar = paste0(varTme, rep(sum(is.finite(dffSpl)), nmbTme)), times = crrTms)
        dtaFrm <- rplAtt(do.call(stats::reshape, adjArg("stats::reshape", crrArg, ...,
                                                        c("data", "direction", "idvar", "sep", "varying", "times", "timevar", "v.names"))), crrTms)
        dtaFrm[[crrArg$timevar]] <- as.factor(dtaFrm[[crrArg$timevar]])
        varID  <- c(varID, crrArg$timevar)
        crrNmV <- names(crrVry)
        dffSpl[crrPos] <- NA
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

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, dtaTtl = jmvTtl("_long"), ...)
}

ordCol <- function(varNme = c(), dtaNmV = c(), varID = c(), varLst = c()) {
    posVrL <- range(which(dtaNmV %in% varLst))
    varOrd <- c(varID[1], setdiff(dtaNmV[rep(seq(1, posVrL[1] - 1), posVrL[1] > 1)], varID), sort(varID[-1]),
                setdiff(varNme, c(varID, setdiff(dtaNmV, varLst))),
                setdiff(dtaNmV[rep(seq(posVrL[2] + 1, length(dtaNmV)), posVrL[2] < length(dtaNmV))], varID))
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

rplAtt <- function(dtaFrm = NULL, crrTms = c()) {
    varNme <-      attr(dtaFrm, "reshapeLong")$v.names
    for (crrNme in varNme) {
        attr(dtaFrm[[crrNme]], "name") <- crrNme
        crrDsc <- c(attr(dtaFrm[, crrNme], "jmv-desc"), attr(dtaFrm[, crrNme], "description"))[1]
        if (is.null(crrDsc) || !nzchar(crrDsc)) next
        splDsc <- trimws(strsplit(crrDsc, "\\(|\\)")[[1]])
        if (length(splDsc) == 2) {
            rplDsc <- paste0(trimws(strsplit(splDsc[2], ",")[[1]])[!grepl(paste(paste0("\\w+: ", crrTms, "$"), collapse = "|"), trimws(strsplit(splDsc[2], ",")[[1]]))], collapse = ", ")
            attr(dtaFrm[[crrNme]], "jmv-desc")    <- ifelse(nzchar(rplDsc), paste0(splDsc[1], " (", rplDsc, ")"), splDsc[1])
            attr(dtaFrm[[crrNme]], "description") <- ifelse(nzchar(rplDsc), paste0(splDsc[1], " (", rplDsc, ")"), splDsc[1])
        }
    }

    dtaFrm
}
