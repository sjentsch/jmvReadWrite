# binds the protcol buffer variables jamovi.coms.AnalysisResponse, jamovi.coms.AnalysisOptions,
# jamovi.coms.AnalysisOption, jamovi.coms.ResultsElement, and jamovi.coms.ResultsGroup locally
# to the function, otherwise devtools::check() - required before submitting to CRAN - throws an
# error
if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("jamovi.coms.AnalysisResponse", "jamovi.coms.AnalysisOptions",
                             "jamovi.coms.AnalysisOption", "jamovi.coms.ResultsElement",
                             "jamovi.coms.ResultsGroup"))
}

# =================================================================================================
# define characters from latin1 (after enc2utf8) and their respective replacements
lstRpl <- rbind(c("<84>",   "<93>",   "<c4>",   "<d6>",   "<dc>",   "<df>",   "<e4>",   "<f6>",   "<fc>"),
                c("\u0084", "\u0093", "\u00c4", "\u00d6", "\u00dc", "\u00df", "\u00e4", "\u00f6", "\u00fc"))

# =================================================================================================
# the next lines store the currently supported versions (stored in meta / MANIFEST.MF)
# and the string that precedes the version number
lstMnf <- list(mnfVer = c("Manifest-Version",        "1.0"),
               datVer = c("Data-Archive-Version",    "1.0.2"),
               jmvVer = c("jamovi-Archive-Version",  "11.0"),
               crtStr = c("Created-By"))

# the next lines are dealing with storing the global and the data column attributes (that go into
# metadata.json inside the .omv-file; the currently defined defaults are in accordance with
# jamovi-Archive-Version: 11.0 (from jamovi 1.8)
mtaGlb <- list(rowCount = NA, columnCount = NA, removedRows = list(), addedRows = list(), fields = list(), transforms = list(), weights = NULL)
mtaFld <- list(name = "", id = NA, columnType = "Data", dataType = "Integer", measureType = "Nominal", formula = "", formulaMessage = "",
               parentId = 0, width = 100, type = "number", outputAnalysisId = NA, outputOptionName = "", outputName = "",
               outputDesiredColumnName = "", outputAssignedColumnName = "", importName = "", description = "", transform = 0,
               edits = list(), missingValues = list(), trimLevels = TRUE, filterNo = NA, active = FALSE)
grpMta <- paste0("^", paste(c(names(mtaGlb), names(mtaFld)), collapse = "$|^"), "$")

# =================================================================================================
# functions for checking parameters (file and directory existence, correct file extension, correct
# dimensions and existence of data frames) and normalizing the file name

#            jamovi  CSV   TSV     Rdata           RDS   SPSS           Stata  SAS
vldExt <- c("omv",  "csv", "tsv", "rdata", "rda", "rds", "sav", "zsav", "dta", "sas7bdat", "sd2", "sd7", "xpt", "stx", "stc")

# REMEMBER: requires the full file name, NOT the directory
chkDir <- function(fleNme = "", wrtPrm = TRUE) {
    if (! utils::file_test("-d", dirname(fleNme))) {
        stop(sprintf("Directory (%s) doesn\'t exist.", dirname(fleNme)))
    }
    if (file.access(dirname(fleNme), mode = 2) != 0) {
        stop(sprintf("The directory (%s) exists, but you don\'t have writing permissions in that directory.", dirname(fleNme)))
    }
    TRUE
}

chkDtF <- function(dtaFrm = NULL, minSze = c(1, 1)) {
    if (length(minSze) != 2) minSze <- rep(minSze[1], 2)
    if (is.null(dtaFrm) || !is.data.frame(dtaFrm) || length(dim(dtaFrm)) != 2) {
        stop("Input data are either not a data frame or have incorrect (only one or more than two) dimensions.")
    } else if (any(dim(dtaFrm) < minSze)) {
        stop(sprintf("The %s dimension of the input data frame has not the required size (%d < %d).",
                     ifelse(which(dim(dtaFrm) < minSze)[1] == 1, "first", "second"), dim(dtaFrm)[dim(dtaFrm) < minSze][1], minSze[dim(dtaFrm) < minSze][1]))
    }
    TRUE
}

chkExt <- function(fleNme = "", extNme = c("")) {
    if (! hasExt(fleNme, extNme)) {
        stop(sprintf("File name (%s) contains an unsupported file extension (%s).", basename(fleNme), paste(paste0(".", extNme[tools::file_ext(fleNme) != extNme]), collapse = ", ")))
    }
    TRUE
}

chkFle <- function(fleNme = "", isZIP = FALSE, fleCnt = "") {
    if (!is.character(fleNme) || !is.logical(isZIP) || !is.character(fleCnt)) {
        stop("chkFle: Unsupported input parameter type.")
    }
    if (!utils::file_test("-f", fleNme)) {
        stop(sprintf("File \"%s\" not found.", fleNme))
    }
    if (isZIP) {
        hdrStr <- readBin(tmpHdl <- file(fleNme, "rb"), "character")
        close(tmpHdl)
        # only "PK\003\004" is considered, not "PK\005\006" (empty ZIP) or "PK\007\008" (spanned [over several files])
        if (! hdrStr == "PK\003\004\024") {
            stop(sprintf("chkFle: File \"%s\" has not the correct file format (is not a ZIP archive).", basename(fleNme)))
        }
    }
    if (nchar(fleCnt) > 0) {
        if (!any(grepl(fleCnt, zip::zip_list(fleNme)$filename))) {
            stop(sprintf("chkFle: File \"%s\" doesn\'t contain the file \"%s\".", basename(fleNme), fleCnt))
        }
    }
    TRUE
}

chkVar <- function(dtaFrm = NULL, varNme = c()) {
    if (is.null(varNme) || length(varNme) == 0 || !all(nzchar(varNme))) return(FALSE)
    if (!all(varNme %in% names(dtaFrm))) {
        stop(sprintf("The variable(s) %s are not contained in the current data set.", paste(varNme[! (varNme %in% names(dtaFrm))], collapse = ", ")))
    }
    TRUE
}

hasExt <- function(fleNme = "", extNme = c("")) {
    any(tolower(tools::file_ext(fleNme)) == tolower(extNme))
}

hasPkg <- function(usePkg = c()) {
    all(sapply(usePkg, function(X) nzchar(system.file(package = X))))
}

nrmFle <- function(fleNme = "") {
    file.path(normalizePath(dirname(fleNme)), basename(fleNme))
}

fmtFlI <- function(fleInp = c(), minLng = 1, maxLng = Inf, excExt = "") {
    # normalize the path of the input file and then check whether the file exists and whether it is of a supported file type
    if (length(fleInp) < minLng || length(fleInp) > maxLng) {
        clsRmv()
        stop(sprintf("The fleInp-argument is supposed to be a character vector with a minimal length of %.0f and a maximal length of %.0f (current length is %.0f).%s",
                     minLng, maxLng, length(fleInp), ifelse(length(fleInp) > maxLng, "\n  If you would like to process several files, call the function individually for each.", "")))
    }
    fleInp <- unname(sapply(fleInp, nrmFle))
    all(sapply(fleInp, chkFle))
    all(sapply(fleInp, chkExt, setdiff(vldExt, excExt)))
    fleInp
}

fmtFlO <- function(fleOut = "") {
    if (!nzchar(fleOut) || (nzchar(fleOut) && !hasExt(fleOut, "omv"))) {
        clsRmv()
        stop("fleOut needs to be a valid non-empty file name (character), and the file extension for output file needs to be .omv.")
    }
    nrmFle(fleOut)
}

# close and remove files, and remove the file handles
clsRmv <- function() {
    for (i in getAllConnections()) {
        if (i < 3) next # on all OSes: stdin [0], stdout [1], stderr [2]
        fleHdl <- getConnection(i)
        crrFle <- summary(fleHdl)[["description"]]
        close(fleHdl)
        unlink(crrFle)
        rm(crrFle)
    }

    return(TRUE)
}

# =================================================================================================
# initializing and handling ProtoBuffers

jmvPtB <- function() {
    # exit with TRUE if the ProtoBuffers are already initialized
    if (exists("jamovi.coms.Status")) return(TRUE)
    # check whether all required packages are present
    synPkg <- c("RProtoBuf", "jmvcore")
    if (!hasPkg(synPkg)) {
        warning(sprintf("For using protocol buffers, the package(s) \"%s\" need(s) to be installed.\n\n",
          paste0(synPkg[!sapply(synPkg, hasPkg)], collapse = "\", \"")))
        return(FALSE)
    }
    # check the two possible places for the jamovi.proto file
    flePtB <- system.file("jamovi.proto", package = "jmvcore")
    if (!nzchar(flePtB)) flePtB <- system.file("inst", "jamovi.proto", package = "jmvcore")
    if (!nzchar(flePtB)) {
        warning("For using protocol buffers, the protocol file \"jamovi.proto\" (from the jmvcore-package) is required.\n\n")
        return(FALSE)
    }
    # read protocol file and initialize the protobuffers with it
    if (requireNamespace("RProtoBuf", quietly = TRUE)) {
        # try reading the protobuffer-file (if it can be read / parsed, tryCatch returns TRUE and the syntax can be extracted)
        # the is.null() is a way to enforce one-liners: either command readProtoFiles and message returns NULL and hence, either
        # TRUE (first line - is.null = TRUE) or FALSE (second line - !is.null = FALSE) are returned
        tryCatch(expr  =             return(is.null(RProtoBuf::readProtoFiles(flePtB))),
                 error = function(e) return(!is.null(message("Error when loading protocol definition, syntax can\'t be extracted:\n", e))))
    }
}

var2PB <- function(inpVar = NULL) {
    # ensure that the jamovi protocol buffers are initiailized
    jmvPtB()
    # the protocol buffers in jamovi actually only support lists as data
    # structures, hence the as.list() conversions for converting vectors

    # NULL (o) ================================================================
    if        (is.null(inpVar)) {
        tmpPB   <- RProtoBuf::new(jamovi.coms.AnalysisOption)
        tmpPB$o <- 2
        return(tmpPB)
    # BOOLEAN (o) =============================================================
    } else if (is.logical(inpVar)) {
        if (length(inpVar) == 1) {
            tmpPB   <- RProtoBuf::new(jamovi.coms.AnalysisOption)
            tmpPB$o <- as.integer(inpVar)
            return(tmpPB)
        } else {
            var2PB(as.list(inpVar))
        }
    # INTEGER (i) =============================================================
    } else if (is.numeric(inpVar) &&  all(inpVar - floor(inpVar) == 0)) {
        if (length(inpVar) == 1) {
            return(RProtoBuf::new(jamovi.coms.AnalysisOption, i = inpVar))
        } else {
            var2PB(as.list(inpVar))
        }
    # DECIMAL (d) =============================================================
    } else if (is.numeric(inpVar)) {
        if (length(inpVar) == 1) {
            tmpPB   <- RProtoBuf::new(jamovi.coms.AnalysisOption)
            tmpPB$d <- inpVar
            return(tmpPB)
        } else {
            var2PB(as.list(inpVar))
        }
    # STRING (s) ==============================================================
    } else if (is.character(inpVar)) {
        if (length(inpVar) == 1) {
            return(RProtoBuf::new(jamovi.coms.AnalysisOption, s = inpVar))
        } else {
            var2PB(as.list(inpVar))
        }
    # CONTAINER (c) ===========================================================
    } else if (is.list(inpVar)) {
        resLst <- list()
        for (i in seq_along(inpVar)) {
            resLst[[i]] <- var2PB(inpVar[[i]])
        }
        tmpPB <- RProtoBuf::new(jamovi.coms.AnalysisOptions, options = resLst)
        if (!is.null(names(inpVar))) {
            tmpPB$hasNames <- TRUE
            tmpPB$names    <- names(inpVar)
        }
        return(RProtoBuf::new(jamovi.coms.AnalysisOption, c = tmpPB))
    # otherwise, throw error ==================================================
    } else {
        clsRmv()
        stop("Element not implemented for conversion to protocol buffer.")
    }
}


# =================================================================================================
# get function arguments and adjust them / select those valid for the current function call

adjArg <- function(fcnNme = c(), dflArg = list(), varArg = list(), fxdArg = c()) {
    chgArg <- setdiff(intersect(fcnArg(fcnNme), names(varArg)), fxdArg)
    c(dflArg[setdiff(names(dflArg), chgArg)], varArg[chgArg])
}

fcnArg <- function(fcnNme = c()) {
    if        (is.character(fcnNme) && length(fcnNme) == 1) {
        eval(parse(text = paste0("formalArgs(", fcnNme, ")")))
    } else if (is.character(fcnNme) && length(fcnNme) == 2) {
        eval(parse(text = paste0("formalArgs(getS3method(\"", fcnNme[1], "\", \"", fcnNme[2], "\"))")))
    } else {
        clsRmv()
        stop("The argument to fcnArg must be a character (vector) with 1 or 2 elements.")
    }
}


# =================================================================================================
# functions for handling setting and storing metadata-information

setAtt <- function(attLst = c(), inpObj = NULL, outObj = NULL) {
    if (!is.character(attLst)) stop("setAtt: The parameter attLst is supposed to be a character vector.")
    if (!is.list(inpObj))      stop("setAtt: The parameter inpObj is supposed to be either a list or a data frame.")
    if (!is.list(outObj))      stop("setAtt: The parameter outObj is supposed to be either a list or a data frame.")

    for (attNme in attLst) {
        # ensure that we have one data frame and one list; the problem is that data frames
        # frames are both lists and data frames, and therefore an error is thrown if BOTH
        # input and output objects are lists but not data frames
        if (identical(sort(c(class(inpObj), class(outObj))), c("data.frame", "list"))) {
            # if the output object is the mtaDta-variable, the input object must be the data frame
            # which contains the attribute in attNme (chkAtt), that are then stored in the mtaDta-
            # variable; the attribute might be empty (chkAtt == FALSE), and then the default is kept
            if        (is.data.frame(inpObj)) {
                if        (dim(inpObj)[2] >  1 &&  chkAtt(inpObj,      attNme)) {
                    outObj[[attNme]] <- attr(inpObj,      attNme)
                } else if (dim(inpObj)[2] == 1 &&  chkAtt(inpObj[[1]], attNme)) {
                    outObj[[attNme]] <- attr(inpObj[[1]], attNme)
                }
                eval(parse(text = paste0("")))
            # if the input object is the mtaDta-variable (which is a list), then the attribute is set
            # in the output object unless the attribute already exists in the ouput object (!chkAtt -
            # it shouldn't be overwritten)
            } else if (is.data.frame(outObj)) {
                if        (dim(outObj)[2] >  1 && !chkAtt(outObj,      attNme)) {
                    attr(outObj,      attNme) <- inpObj[[attNme]]
                } else if (dim(outObj)[2] == 1 && !chkAtt(outObj[[1]], attNme)) {
                    attr(outObj[[1]], attNme) <- inpObj[[attNme]]
                }
#               eval(parse(text = paste0("attr(outObj", ifelse(dim(outObj)[2] == 1, "[[1]]", ""), ", attNme) <- inpObj[[attNme]]")))
            }
        # the case which is critical is if both input and output objects are lists (then the first
        # part of the if-conditions above - is.list - wouldn't work)
        } else {
            errDsc <- paste0("\nOne input object (inpObj or outObj) must be a list, the other must be a data frame.\n\n",
                             "attNme: ", attNme, "\n",
                             "attLst: ", paste0(attLst, collapse = ", "), "\n\n",
                             "inpObj:\n", utils::capture.output(utils::str(inpObj)), "\n\n",
                             "outObj:\n", utils::capture.output(utils::str(outObj)), "\n\n")
            stop(sprintf("Error when storing or accessing meta-data information. Please send the file causing the error to sebastian.jentschke@uib.no\n%s", errDsc))
        }
    }

    outObj
}

rmvAtt <- function(attObj = NULL) {
    for (crrAtt in setdiff(names(attributes(attObj)), c("class", "comment", "dim", "jmv-id", "jmv-desc", "levels", "names", "row.names", "values"))) {
        attr(attObj, crrAtt) <- NULL
    }

    attObj
}

chkAtt <- function(attObj = NULL, attNme = "", attVal = NULL) {
   ((attNme %in% names(attributes(attObj))) && length(attr(attObj, attNme)) > 0 && ifelse(!is.null(attVal), grepl(attVal, attr(attObj, attNme)), TRUE))
}

chkFld <- function(fldObj = NULL, fldNme = "", fldVal = NULL) {
   ((fldNme %in% names(fldObj))    && length(fldObj[[fldNme]])     > 0 && ifelse(!is.null(fldVal), grepl(fldVal, fldObj[[fldNme]]),     TRUE))
}

# =================================================================================================
# function handling to have either a data frame or a character (pointing to a file) as input
inp2DF <- function(dtaInp = NULL, minDF = 1, maxDF = 1, usePkg = c("foreign", "haven"), selSet = "", ...) {
    usePkg <- match.arg(usePkg)
    # check and format input and output files, handle / check further input arguments:
    # if the input is a data frame, it is “embedded” in a list (in order to permit to read
    # and to concatenate this data frame with further data frames given as fleInp-attribute
    # and read via the lapply function)
    if (is.data.frame(dtaInp) && chkDtF(dtaInp)) {
        lstDF <- list(dtaInp)
        if (!is.null(attr(dtaInp, "fleInp"))) {
            lstDF <- c(lstDF, lapply(fmtFlI(attr(dtaInp, "fleInp"), minLng = minDF - 1, maxLng = maxDF - 1), function(x) read_all(fleInp = x, usePkg = usePkg, selSet = selSet, ...)))
        }
    # if the input is a character vector (with file names), all file names are read into
    # data frames (using the lapply function)
    } else if (is.character(dtaInp)) {
        lstDF <-              lapply(fmtFlI(dtaInp,                 minLng = minDF - 0, maxLng = maxDF - 0), function(x) read_all(fleInp = x, usePkg = usePkg, selSet = selSet, ...))
    } else {
        clsRmv()
        stop("dtaInp must either be a data frame or a character (pointing to a location where the input file can be found).")
    }
    # most functions expect only one data frame to be returned, thus, the list
    # used for reading processing those data frames is unpacked if there is
    # only one data frame to return
    if (maxDF == 1) lstDF[[1]] else lstDF
}

# =================================================================================================
# Unified function to handle data frames at the end of the helper functions
# * if the output file name is not empty, the data frame is written to the output file
# * if no output file name was given:
#   - open the data frame in a new session (only in jamovi, and if fleOut is an empty character vector)
#   - return the data frame (in R in any case, or in jamovi if fleOut is NULL)
#   NB: this makes opening the data frame in a new session the default, if in jamovi
rtnDta <- function(dtaFrm = NULL, fleOut = "", dtaTtl = "", wrtPtB = FALSE, psvAnl = FALSE, dtaInp = NULL, ...) {
    if (!is.null(fleOut) && nzchar(fleOut[1])) {
        fleOut <- fmtFlO(fleOut[1])
        write_omv(dtaFrm = dtaFrm, fleOut = fleOut, wrtPtB = wrtPtB, ...)
        # transfer analyses from input to output file
        if (psvAnl) {
            if (is.character(dtaInp)) {
                xfrAnl(dtaInp[1], fleOut)
            } else {
                warning("psvAnl is only possible if dtaInp is a file name (analyses are not stored in data frames, only in the jamovi files).")
            }
        }
        return()
    } else if (isJmv() && is.character(fleOut)) {
        if (psvAnl) warning("psvAnl is only possible if fleOut is a file name (analyses are not stored in data frames, only in the jamovi files).")
        jmvOpn(dtaFrm, dtaTtl = dtaTtl)
        return()
    } else {
        if (psvAnl) warning("psvAnl is only possible if fleOut is a file name (analyses are not stored in data frames, only in the jamovi files).")
        return(dtaFrm)
    }
}

# =================================================================================================
# function for copying analyses from one data file to another

xfrAnl <- function(fleOrg = "", fleTgt = "") {
    # check whether input and output files are valid and format input and output file names
    chkExt(fleOrg, "omv") && chkFle(fleOrg, isZIP = TRUE) && chkFle(fleOrg, fleCnt = "meta|MANIFEST.MF")
    chkExt(fleTgt, "omv") && chkFle(fleTgt, isZIP = TRUE) && chkFle(fleTgt, fleCnt = "meta|MANIFEST.MF")
    fleOrg <- fmtFlI(fleOrg, maxLng = 1)
    fleTgt <- fmtFlI(fleTgt, maxLng = 1)

    # extract the list of files contained in the input file, assign tempdir()
    lstOrg <- zip::zip_list(fleOrg)$filename
    lst2Cp <- lstOrg[grepl("index.html|[0-9].*\\s[a-z].*?/", lstOrg)]
    lstCmb <- union(zip::zip_list(fleTgt)$filename, lst2Cp)
    xfrDir <- file.path(tempdir(), "xfrAnl")

    # create a list of files to be copied, extract them from the input file and
    # append them to the output file
    zip::unzip(fleTgt,                 exdir = xfrDir)
    zip::unzip(fleOrg, files = lst2Cp, exdir = xfrDir, overwrite = TRUE)
    zip::zip(fleTgt,   files = lstCmb, root  = xfrDir)

    # remove the files and directories from the list of files to be copied
    unlink(xfrDir, recursive = TRUE)

    TRUE
}

# =================================================================================================
# function for checking which OS is running, whether we are running inside jamovi, for adding
# attributes used by jamovi to data frames (e.g., those opened in Rj or via jTransform), and for
# opening a data set in jamovi (if fleOut is left blank, typically a data frame is returned; if we
# are running inside jamovi, a new data set is opened)

getOS <- function() {
    sysInf <- Sys.info()
    if (!is.null(sysInf)) {
        return(tolower(gsub("Darwin", "macos", sysInf[["sysname"]])))
    } else {
        return(ifelse(grepl("^darwin",   R.version$os), "macos",
               ifelse(grepl("linux-gnu", R.version$os), "linux",
               tolower(.Platform$OS.type))))
    }
}

isJmv <- function() {
    nzchar(Sys.getenv("JAMOVI_R_VERSION"))
}

jmvAtt <- function(dtaFrm = NULL) {
    chkDtF(dtaFrm)

    for (crrNme in names(dtaFrm)) {
         # if the attributes already exist, go to the next column
         if (chkAtt(dtaFrm[[crrNme]], "measureType") && chkAtt(dtaFrm[[crrNme]], "measureType")) next
         # jmv-id
         if (!is.null(attr(dtaFrm[[crrNme]], "jmv-id")) && attr(dtaFrm[[crrNme]], "jmv-id")) {
             attr(dtaFrm[[crrNme]], "measureType")  <- "ID"
             attr(dtaFrm[[crrNme]], "dataType")     <- ifelse(is.integer(dtaFrm[[crrNme]]), "Integer", "Text")
         } else if (is.integer(dtaFrm[[crrNme]])) {
             attr(dtaFrm[[crrNme]], "measureType")  <- "Continuous"
             attr(dtaFrm[[crrNme]], "dataType")     <- "Integer"
         } else if (is.numeric(dtaFrm[[crrNme]])) {
             attr(dtaFrm[[crrNme]], "measureType")  <- "Continuous"
             attr(dtaFrm[[crrNme]], "dataType")     <- "Decimal"
         } else if (is.ordered(dtaFrm[[crrNme]]) &&  is.null(attr(dtaFrm[[crrNme]], "values"))) {
             attr(dtaFrm[[crrNme]], "measureType")  <- "Ordinal"
             attr(dtaFrm[[crrNme]], "dataType")     <- "Text"
         } else if (is.ordered(dtaFrm[[crrNme]]) && !is.null(attr(dtaFrm[[crrNme]], "values"))) {
             attr(dtaFrm[[crrNme]], "measureType")  <- "Ordinal"
             attr(dtaFrm[[crrNme]], "dataType")     <- "Integer"
         } else if (is.factor(dtaFrm[[crrNme]]) &&  is.null(attr(dtaFrm[[crrNme]], "values"))) {
             attr(dtaFrm[[crrNme]], "measureType")  <- "Nominal"
             attr(dtaFrm[[crrNme]], "dataType")     <- "Text"
         } else if (is.factor(dtaFrm[[crrNme]]) && !is.null(attr(dtaFrm[[crrNme]], "values"))) {
             attr(dtaFrm[[crrNme]], "measureType")  <- "Nominal"
             attr(dtaFrm[[crrNme]], "dataType")     <- "Integer"
         } else if (is.character(dtaFrm[[crrNme]])) {
             crrAtt <- attributes(dtaFrm[[crrNme]])
             dtaFrm[[crrNme]] <- as.factor(dtaFrm[[crrNme]])
             dffAtt <- setdiff(names(crrAtt), c("levels", "class"))
             if (length(dffAtt) > 0) dtaFrm[crrNme] <- setAtt(attLst = dffAtt, inpObj = crrAtt, outObj = dtaFrm[crrNme])
             attr(dtaFrm[[crrNme]], "measureType")  <- "Nominal"
             attr(dtaFrm[[crrNme]], "dataType")     <- "Text"
         } else {
             stop(sprintf("\n\n%s: Variable type %s not implemented:\n%s\n\n", crrNme, class(dtaFrm[[crrNme]]), trimws(utils::capture.output(utils::str(dtaFrm[[crrNme]])))))
         }
    }

    dtaFrm
}

jmvOpn <- function(dtaFrm = NULL, dtaTtl = "") {
    # on both Windows and Linux, jamovi is in the path, and, hence,
    # Sys.which should give the full location
    jmvEXE <- Sys.which("jamovi")
    # if not, we have to determine the position of jamovi under the
    # current OS
    if (!nzchar(jmvEXE)) {
        crrOS <- getOS()
        if        (crrOS == "windows") {
            jmvHme <- jmvPth(R.home(), "Frameworks", TRUE)
            if (!is.null(jmvHme)) jmvEXE <- normalizePath(file.path(jmvHme, "bin", "jamovi.exe"))
        } else if (crrOS == "macos")   {
            jmvHme <- jmvPth(R.home(), "Contents", FALSE)
            if (!is.null(jmvHme)) jmvEXE <- file.path(jmvHme,  "MacOS", "jamovi")
        } else if (crrOS == "linux")   {
            jmvHme <- jmvPth(R.home(), "lib", TRUE)
            if (!is.null(jmvHme)) jmvEXE <- file.path(jmvHme, "bin", "jamovi")
        } else {
            stop(sprintf("Your OS (%s) is currently not implemented. Please report more details to sebastian.jentschke@uib.no to fix that.", crrOS))
        }
    }
    if (nzchar(jmvEXE) && file.exists(jmvEXE)) {
        tmpOut <- tempfile(fileext = ".omv")
        jmvReadWrite::write_omv(dtaFrm, fleOut = tmpOut)
        system2(jmvEXE, args = paste0(" --temp --title=\"", dtaTtl, "\" ", tmpOut), stderr = TRUE, stdout = TRUE)
    } else {
        stop(sprintf("The position of the jamovi executable could not be determined or it was not found at the determined position. Determined position: %s", jmvEXE))
    }
}

jmvPth <- function(inpPth = "", strTgt = "", bfrTgt = TRUE) {
    mtcTgt <- gregexpr(strTgt, inpPth)[[1]][1]
    if (mtcTgt > 0) {
        return(substr(inpPth, 1, mtcTgt + ifelse(bfrTgt, -2, nchar(strTgt) - 1)))
    } else {
        return()
    }
}

jmvTtl <- function(sfxTtl = "") {
    # return empty string when not inside jamove (then the title is irrelevant)
    if (!isJmv()) return("")
# TO-DO: replace Dataset with the name of the current data set (once this is implemented)
    return(paste0("Dataset", sfxTtl))
}
