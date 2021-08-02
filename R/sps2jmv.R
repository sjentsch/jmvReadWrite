#' Converts SPPS-syntax (either from a SPSS-syntax file .sps or read from a SPSS-output-file .spv [using spv2sps]) into jamovi / jmv-syntax
#'
#' The function expects that either the parameter lstSPS [with the output from spv2sps; otherwise the list has to have identical format – see below] is used or that both fleSPS and fleSAV are given.
#'
#' @param list with SPSS-commands, the format is supposed to be the same as what is output from spv2sps (a list containing one command per entry, the command being a character vector, and the attribute 'datafile' with a character vector containing the position of the SPSS-data-file [incl. path]; if not found at the path, the file is searched in the current directory)
#' @param position of a SPSS-syntax-file [.sps; incl. path]
#' @param position of a SPSS-data-file [.sav; incl. path]
#' 
#' @return list with jamovi / jmv-analysis-function-calls and the data set stored in the attribute 'dataset'
#'
#' @export sps2jmv
#'
sps2jmv <- function(lstSPS = list(), fleSPS = "", fleSAV = "") {

    # check whether lstSPS or fleSPS / fleSAV was given, do some plausibility checks
    if (length(lstSPS) > 0) {
        if (! is.null(attr(lstSPS, 'datafile'))) { fleSAV = attr(lstSPS, 'datafile'); }
    } else if (fleSPS > "") {
        if (! file.exists(fleSPS) && ! file.exists(file.path(getwd(), basename(fleSPS)))) { stop(paste0("'", fleSPS, "' does not exist.")); }
        lstSPS = readLines(hdlSPS <- file(fleSPS, 'r'), warn = FALSE); close(hdlSPS); rm('hdlSPS');
        # go through the lines and join the lines with the next lines if those don't end with a '.'
        lneCtd = which(! (grepl('\\.$', lstSPS) | grepl('^$', lstSPS)));
        if (max(lneCtd) >= length(lstSPS)) { stop('The last entry / command in the syntax file isn\'t terminated properly with a full stop (.)') }
        for (Z in lneCtd) { lstSPS[Z + 1] = gsub("\\s+", " ", paste(lstSPS[Z], lstSPS[Z + 1])); lstSPS[Z] = ""; }
        lstSPS = as.list(lstSPS[lstSPS != ""]);
    }
    if (fleSAV > "") {
        if (! file.exists(fleSAV) && file.exists(file.path(getwd(), basename(fleSAV)))) { fleSAV = file.path(getwd(), basename(fleSAV)); }
        data = suppressWarnings(foreign::read.spss(file = fleSAV, to.data.frame=TRUE));
    } else {
        stop('sps2jmv requires an SPSS-data-file. This can be either provided by lstSPS containing an attribute \"datafile\" or via the parameter fleSAV. This should be a character vector containing the position of the SPSS-data-file (.sav; incl. path).');
    }
    if (length(lstSPS) == 0) {
        stop('sps2jmv requires either a list [lstSPS] containing one SPSS-command (each as character vector) per list entry, or a parameter [fleSPS] pointing to a SPSS-syntax-file (.sps; incl. path).') 
    }

    lstJMV = list();
    for (crrSPS in lstSPS) {
       
        # General: Split, handle generic attributes (e.g., VAR1 TO VARx) ======
        crrSPS = unlist(strsplit(gsub('\\.$', '', crrSPS), ' /'));

        # Modifications to the data set =======================================

        # ADD VALUE LABELS ----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-add-value-labels
        # (adds new value labels but keeps existing ones, see also VALUE LABELS)
        if      (grepl('^ADD VALUE LABELS', crrSPS[1])) {
            warning('ADD VALUE LABELS – not implemented yet');
        }
        
        # ALTER TYPE ----------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-alter-type
        else if (grepl('^ALTER TYPE', crrSPS[1])) {
            warning('ALTER TYPE – not implemented yet');
        }
        
        # COMPUTE -------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-compute
        else if (grepl('^COMPUTE', crrSPS[1])) {
            warning('COMPUTE – not implemented yet');
        }

        # FILTER --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-filter
        else if (grepl('^FILTER', crrSPS[1])) {
            warning('FILTER – not implemented yet');
        }

        # NUMERIC -------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-numeric
        else if (grepl('^NUMERIC', crrSPS[1])) {
            warning('NUMERIC – not implemented yet');
        }

        # RANK ----------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-rank
        else if (grepl('^RANK', crrSPS[1])) {
            warning('RANK – not implemented yet');
        }

        # RECODE --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-recode
        else if (grepl('^RECODE', crrSPS[1])) {
            warning('RECODE – not implemented yet');
            # COMPUTED → RANK(VARNAME)
        }

        # RENAME VARIABLES ----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-rename-variables
        else if (grepl('^RENAME VARIABLES', crrSPS[1])) {
            warning('RENAME VARIABLES – not implemented yet');
        }

        # SAMPLE --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sample
        else if (grepl('^SAMPLE', crrSPS[1])) {
            warning('SAMPLE – not implemented yet');
            # COMPUTED → SAMPLE(1, N FROM M / DECIMAL VALUE, 0) + FILTER
            # double-check whether the sample changes, otherwise use a fixed assignment based on R-sample()
        }

        # SORT CASES ----------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sort-cases
        else if (grepl('^SORT CASES', crrSPS[1])) {
            warning('SORT CASES – not implemented yet');
        }

        # SORT VARIABLES ------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sort-variables
        else if (grepl('^SORT VARIABLES', crrSPS[1])) {
            warning('SORT VARIABLES – not implemented yet');
        }

        # STRING --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-string
        else if (grepl('^STRING', crrSPS[1])) {
            warning('STRING – not implemented yet');
        }

        # VALUE LABELS --------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-value-labels
        else if (grepl('^VALUE LABELS', crrSPS[1])) {
            warning('VALUE LABELS – not implemented yet');
        }
   
        # VARIABLE LABELS -----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-variable-labels
        else if (grepl('^VARIABLE LABELS', crrSPS[1])) {
            warning('VARIABLE LABELS – not implemented yet');
        }

        # VARIABLE LEVEL ------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-variable-level
        else if (grepl('^VARIABLE LEVEL', crrSPS[1])) {
            warning('VARIABLE LEVEL – not implemented yet');
        }


        # Comments ============================================================

        # COMMENT -------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-comment
        else if (grepl('^COMMENT', crrSPS[1])) {
            warning('COMMENT – not implemented yet');
        }


        # Exploration – Descriptives ==========================================
        # https://www.jamovi.org/jmv/descriptives.html

        # DESCRIPTIVES --------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-descriptives
        else if (grepl('^DESCRIPTIVES', crrSPS[1])) {
            print('DESCRIPTIVES');
            crrSPS = rmvCmd(crrSPS, 'DESCRIPTIVES');
            crrFnc = 'jmv::descriptives';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            # extract variables and assign them as argument "formula"
            varLst = substr(crrSPS[1], (gregexpr(pattern = 'VARIABLES\\s?=', crrSPS[1])[[1]] + 10), nchar(crrSPS[1]));
            chkVar(varLst, data);
            crrArg = asgArg(crrArg, pairlist(formula = as.symbol(gsub(' ', ' + ', varLst))));
            # STATISTICS subcommand
            crrArg = asgArg(crrArg, pairlist(mean     = any(grepl('^STATISTICS', crrSPS) & (grepl('MEAN',     crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             sd       = any(grepl('^STATISTICS', crrSPS) & (grepl('STDDEV',   crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             min      = any(grepl('^STATISTICS', crrSPS) & (grepl('MIN',      crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             max      = any(grepl('^STATISTICS', crrSPS) & (grepl('MAX',      crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             se       = any(grepl('^STATISTICS', crrSPS) & (grepl('SEMEAN',   crrSPS)                            | grepl('ALL', crrSPS))),
                                             variance = any(grepl('^STATISTICS', crrSPS) & (grepl('VARIANCE', crrSPS)                            | grepl('ALL', crrSPS))),
                                             skew     = any(grepl('^STATISTICS', crrSPS) & (grepl('SKEWNESS', crrSPS)                            | grepl('ALL', crrSPS))),
                                             kurt     = any(grepl('^STATISTICS', crrSPS) & (grepl('KURTOSIS', crrSPS)                            | grepl('ALL', crrSPS))),
                                             sum      = any(grepl('^STATISTICS', crrSPS) & (grepl('SUM',      crrSPS)                            | grepl('ALL', crrSPS))),
                                             range    = any(grepl('^STATISTICS', crrSPS) & (grepl('RANGE',    crrSPS)                            | grepl('ALL', crrSPS))),
                                             median = FALSE, mode = FALSE, sum = FALSE, ci = FALSE, iqr = FALSE, sw = FALSE, pc = FALSE));           
            # not implemented in jmv: MISSING, SAVE, SORT
            # SAVE could be implemented manually later - generate computed variable Z()
        }

        # EXAMINE -------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-examine
        else if (grepl('^EXAMINE', crrSPS[1])) {
            print('EXAMINE');
            crrSPS = rmvCmd(crrSPS, 'EXAMINE');
            crrFnc = 'jmv::descriptives';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            # extract variables and assign them as argument "formula"
            varLst = substr(crrSPS[1], (gregexpr(pattern = 'VARIABLES\\s?=', crrSPS[1])[[1]] + 10), nchar(crrSPS[1]));
            chkVar(gsub(' BY ', ' ', varLst), data);
            crrArg = asgArg(crrArg, pairlist(formula = as.symbol(gsub(' \\+ BY \\+ ', ' ~ ', gsub(' ', ' + ', varLst)))));
            # PLOT subcommand: STEMLEAF (not implemented), SPREADLEVEL(not implemented)
            crrArg = asgArg(crrArg, pairlist(box  = any(grepl('^PLOT', crrSPS) & (grepl('BOXPLOT',   crrSPS) | grepl('ALL', crrSPS))),
                                             hist = any(grepl('^PLOT', crrSPS) & (grepl('HISTOGRAM', crrSPS) | grepl('ALL', crrSPS))),
                                             qq   = any(grepl('^PLOT', crrSPS) & (grepl('NPPLOT',    crrSPS) | grepl('ALL', crrSPS))),
                                             sw   = any(grepl('^PLOT', crrSPS) & (grepl('NPPLOT',    crrSPS) | grepl('ALL', crrSPS))),
                                             dens = FALSE, bar = FALSE, violin = FALSE, dot = FALSE, boxMean = FALSE));
            # STATISTICS subcommand: EXTREMES (not implemented)
            if (any(grepl('^STATISTICS[=,\\s]DESCRIPTIVES', crrSPS, perl = TRUE) | grepl('^STATISTICS[=,\\s]ALL', crrSPS, perl = TRUE))) {
                crrArg = asgArg(crrArg, pairlist(mean = TRUE, median = TRUE, se = TRUE, variance = TRUE, sd = TRUE, min = TRUE, max = TRUE, range = TRUE, iqr = TRUE, skew = TRUE, kurt = TRUE, 
                                                 mode = FALSE, sum = FALSE)); }
            # CINTERVAL subcommand
            if (any(grepl('^CINTERVAL', crrSPS))) {
                crrArg = asgArg(crrArg, pairlist(ci = TRUE, ciWidth = as.double(trimws(sub("CINTERVAL", "", crrSPS[grepl('^CINTERVAL', crrSPS)]))))); }
            # PERCENTILES subcommand
            if (any(grepl('^PERCENTILES\\(', crrSPS))) {
                crrArg = asgArg(crrArg, pairlist(pc = TRUE, pcValues = sub("\\).*", "", sub(".*\\(", "", crrSPS[grepl('^PERCENTILES\\(', crrSPS)])))); }
            # not implemented in jmv: COMPARE (not necessary), TOTAL, ID, MESTIMATOR, MISSING
        }

        # FREQUENCIES ---------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-frequencies
        else if (grepl('^FREQUENCIES', crrSPS[1])) {
            print('FREQUENCIES');
            crrSPS = rmvCmd(crrSPS, 'FREQUENCIES');
            crrFnc = 'jmv::descriptives';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            # extract variables and assign them as argument "formula"
            varLst = substr(crrSPS[1], (gregexpr(pattern = 'VARIABLES\\s?=', crrSPS[1])[[1]] + 10), nchar(crrSPS[1]));
            chkVar(varLst, data);
            crrArg = asgArg(crrArg, pairlist(formula = as.symbol(gsub(' ', ' + ', varLst))));
            # BARCHART subcommand: MINIMUM, MAXIMUM, FREQ, PERCENT not implemented
            if (any(grepl('^BARCHART', crrSPS))) { crrArg = asgArg(crrArg, pairlist(bar = TRUE, barCounts = TRUE)); }
            # HISTOGRAM subcommand: MINIMUM, MAXIMUM, FREQ, NORMAL (replaced by density)
            if (any(grepl('HISTOGRAM', crrSPS))) { crrArg = asgArg(crrArg, pairlist(hist = TRUE, dens = TRUE)); }
            # NTILES subcommand
            if (any(grepl('^NTILES', crrSPS))) {
                crrArg = asgArg(crrArg, pairlist(pc = TRUE, pcNEqGr  = sub("NTILES\\s?=", "",      crrSPS[grepl('^NTILES',      crrSPS)]))); }
            # PERCENTILES subcommand
            if (any(grepl('^PERCENTILES', crrSPS))) {
                crrArg = asgArg(crrArg, pairlist(pc = TRUE, pcValues = sub("PERCENTILES\\s?=", "", crrSPS[grepl('^PERCENTILES', crrSPS)]))); }
            # STATISTICS subcommand
            crrArg = asgArg(crrArg, pairlist(mean     = any(grepl('^STATISTICS', crrSPS) & (grepl('MEAN',     crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             sd       = any(grepl('^STATISTICS', crrSPS) & (grepl('STDDEV',   crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             min      = any(grepl('^STATISTICS', crrSPS) & (grepl('MIN',      crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             max      = any(grepl('^STATISTICS', crrSPS) & (grepl('MAX',      crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             median   = any(grepl('^STATISTICS', crrSPS) & (grepl('MEDIAN',   crrSPS)                            | grepl('ALL', crrSPS))),
                                             se       = any(grepl('^STATISTICS', crrSPS) & (grepl('SEMEAN',   crrSPS)                            | grepl('ALL', crrSPS))),
                                             variance = any(grepl('^STATISTICS', crrSPS) & (grepl('VARIANCE', crrSPS)                            | grepl('ALL', crrSPS))),
                                             skew     = any(grepl('^STATISTICS', crrSPS) & (grepl('SKEWNESS', crrSPS)                            | grepl('ALL', crrSPS))),
                                             kurt     = any(grepl('^STATISTICS', crrSPS) & (grepl('KURTOSIS', crrSPS)                            | grepl('ALL', crrSPS))),
                                             sum      = any(grepl('^STATISTICS', crrSPS) & (grepl('SUM',      crrSPS)                            | grepl('ALL', crrSPS))),
                                             range    = any(grepl('^STATISTICS', crrSPS) & (grepl('RANGE',    crrSPS)                            | grepl('ALL', crrSPS))),
                                             mode     = any(grepl('^STATISTICS', crrSPS) & (grepl('MODE',     crrSPS)                            | grepl('ALL', crrSPS))),
                                             sum      = any(grepl('^STATISTICS', crrSPS) & (grepl('SUM',      crrSPS)                            | grepl('ALL', crrSPS))),
                                             ci = FALSE, iqr = FALSE, sw = FALSE));
            # not implemented in jmv: FORMAT, MISSING, PIECHART, GROUPED, ORDER
        }

        # MEANS ---------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-means
        else if (grepl('^MEANS', crrSPS[1])) {
            print('MEANS');
            crrSPS = rmvCmd(crrSPS, 'MEANS');
            crrFnc = 'jmv::descriptives';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            # extract variables and assign them as argument "formula", NB: Only the first variable list is considered
            varLst = gsub('MEANS ', '', gsub('TABLES=', '', crrSPS[1]));
            if (trimws(varLst) == 'ALL') { varLst = names(data); } else { chkVar(gsub(' BY ', ' ', varLst), data); }
            crrArg = asgArg(crrArg, pairlist(formula = as.symbol(gsub(' \\+ BY \\+ ', ' ~ ', gsub(' ', ' + ', varLst)))));
            # CELLS subcommand - not implemented: GMEDIAN, SEKURT, SESKEW, FIRST, LAST, NPCT, SPCT, HARMONIC, GEOMETRIC
            crrArg = asgArg(crrArg, pairlist(mean     = any(grepl('^CELLS', crrSPS) & (grepl('MEAN',     crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             sd       = any(grepl('^CELLS', crrSPS) & (grepl('STDDEV',   crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             median   = any(grepl('^CELLS', crrSPS) & (grepl('MEDIAN',   crrSPS)                            | grepl('ALL', crrSPS))),
                                             se       = any(grepl('^CELLS', crrSPS) & (grepl('SEMEAN',   crrSPS)                            | grepl('ALL', crrSPS))),
                                             sum      = any(grepl('^CELLS', crrSPS) & (grepl('SUM',      crrSPS)                            | grepl('ALL', crrSPS))),
                                             min      = any(grepl('^CELLS', crrSPS) & (grepl('MIN',      crrSPS)                            | grepl('ALL', crrSPS))),
                                             max      = any(grepl('^CELLS', crrSPS) & (grepl('MAX',      crrSPS)                            | grepl('ALL', crrSPS))),
                                             range    = any(grepl('^CELLS', crrSPS) & (grepl('RANGE',    crrSPS)                            | grepl('ALL', crrSPS))),
                                             variance = any(grepl('^CELLS', crrSPS) & (grepl('VARIANCE', crrSPS)                            | grepl('ALL', crrSPS))),
                                             kurt     = any(grepl('^CELLS', crrSPS) & (grepl('KURT',     crrSPS)                            | grepl('ALL', crrSPS))),
                                             skew     = any(grepl('^CELLS', crrSPS) & (grepl('SKEW',     crrSPS)                            | grepl('ALL', crrSPS))),
                                             mode = FALSE, missing = FALSE, ci = FALSE, iqr = FALSE, sw = FALSE));
            # not implemented in jmv: MISSING, STATISTICS
        }

        # SUMMARIZE -----------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-summarize
        else if (grepl('^SUMMARIZE', crrSPS[1])) {
            print('SUMMARIZE');
            crrSPS = rmvCmd(crrSPS, 'SUMMARIZE');
            crrFnc = 'jmv::descriptives';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            # extract variables and assign them as argument "formula", NB: Only the first variable list is considered
            varLst = gsub('SUMMARIZE ', '', gsub('TABLES=', '', crrSPS[1]));
            if (trimws(varLst) == 'ALL') { varLst = names(data); } else { chkVar(gsub(' BY ', ' ', varLst), data); }
            crrArg = asgArg(crrArg, pairlist(formula = as.symbol(gsub(' \\+ BY \\+ ', ' ~ ', gsub(' ', ' + ', varLst)))));
            # CELLS subcommand - not implemented: GMEDIAN, SEKURT, SESKEW, FIRST, LAST, NPCT, SPCT, HARMONIC, GEOMETRIC
            crrArg = asgArg(crrArg, pairlist(mean     = any(grepl('^CELLS', crrSPS) & (grepl('MEAN',     crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             sd       = any(grepl('^CELLS', crrSPS) & (grepl('STDDEV',   crrSPS) | grepl('DEFAULT', crrSPS) | grepl('ALL', crrSPS))),
                                             median   = any(grepl('^CELLS', crrSPS) & (grepl('MEDIAN',   crrSPS)                            | grepl('ALL', crrSPS))),
                                             se       = any(grepl('^CELLS', crrSPS) & (grepl('SEMEAN',   crrSPS)                            | grepl('ALL', crrSPS))),
                                             sum      = any(grepl('^CELLS', crrSPS) & (grepl('SUM',      crrSPS)                            | grepl('ALL', crrSPS))),
                                             min      = any(grepl('^CELLS', crrSPS) & (grepl('MIN',      crrSPS)                            | grepl('ALL', crrSPS))),
                                             max      = any(grepl('^CELLS', crrSPS) & (grepl('MAX',      crrSPS)                            | grepl('ALL', crrSPS))),
                                             range    = any(grepl('^CELLS', crrSPS) & (grepl('RANGE',    crrSPS)                            | grepl('ALL', crrSPS))),
                                             variance = any(grepl('^CELLS', crrSPS) & (grepl('VARIANCE', crrSPS)                            | grepl('ALL', crrSPS))),
                                             kurt     = any(grepl('^CELLS', crrSPS) & (grepl('KURT',     crrSPS)                            | grepl('ALL', crrSPS))),
                                             skew     = any(grepl('^CELLS', crrSPS) & (grepl('SKEW',     crrSPS)                            | grepl('ALL', crrSPS))),
                                             mode = FALSE, missing = FALSE, ci = FALSE, iqr = FALSE, sw = FALSE));
            # not implemented in jmv: TITLE, FOOTNOTE, MISSING, FORMAT, STATISTICS
        }


        # T-Tests – Independent Samples T-Test ================================
        # https://www.jamovi.org/jmv/ttestis.html

        # NPAR TESTS - M-W ----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', crrSPS[1]) & any(grepl('M-W', crrSPS))) {
            warning('NPAR TESTS - M-W – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPAR TESTS');
            crrFnc = 'jmv::ttestIS';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            crrArg$students = FALSE; crrArg$welchs = FALSE; crrArg$mann = TRUE;
            
        }

        # NPTESTS - INDEPENDENT - MANN_WHITNEY --------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', crrSPS[1]) & any(grepl('INDEPENDENT', crrSPS)) & any(grepl('MANN_WHITNEY', crrSPS))) {
            warning('NPTESTS - INDEPENDENT - MANN_WHITNEY – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPTESTS');            
            crrFnc = 'jmv::ttestIS';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            crrArg$students = FALSE; crrArg$welchs = FALSE; crrArg$mann = TRUE;
            
        }

        # T-TEST (with GROUPS) ------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl('^T-TEST', crrSPS[1]) & ! any(grepl('PAIRS', crrSPS)) & ! any(grepl('TESTVAL', crrSPS))) {
            warning('T-TEST - INDEPENDENT – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'T-TEST');
            crrFnc = 'jmv::ttestIS';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            crrArg$students = TRUE; crrArg$welchs = TRUE; crrArg$mann = FALSE;
            
        }


        # T-Tests – Paired Samples T-Test =====================================
        # https://www.jamovi.org/jmv/ttestps.html

        # NPAR TESTS - WILCOXON - PAIRED --------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', crrSPS[1]) & any(grepl('WILCOXON', crrSPS)) & any(grepl('PAIRED', crrSPS))) {
            warning('NPAR TESTS - WILCOXON - PAIRED – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPAR TESTS');
            crrFnc = 'jmv::ttestPS';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            crrArg$students = FALSE; crrArg$wilcoxon = TRUE;
            
        }

        # NPTESTS - RELATED - WILCOXON -----------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', crrSPS[1]) & any(grepl('RELATED', crrSPS)) & any(grepl('WILCOXON', crrSPS))) {
            warning('NPTESTS - RELATED - WILCOXON – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPTESTS');            
            crrFnc = 'jmv::ttestPS';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            crrArg$students = FALSE; crrArg$wilcoxon = TRUE;
            
        }

        # T-TEST (with PAIRS) -------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl('^T-TEST', crrSPS[1]) & any(grepl('PAIRS', crrSPS))) {
            warning('T-TEST - Paired – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'T-TEST');
            crrFnc = 'jmv::ttestPS';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            crrArg$students = TRUE; crrArg$wilcoxon = FALSE;
            
        }


        # T-Tests – One Sample T-Test =========================================
        # https://www.jamovi.org/jmv/ttestones.html

        # NPAR TESTS - WILCOXON - not PAIRED ----------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', crrSPS[1]) & any(grepl('WILCOXON', crrSPS)) & ! any(grepl('PAIRED', crrSPS))) {
            warning('NPAR TESTS WILCOXON - not PAIRED (one sample) – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPAR TESTS');
            crrFnc = 'jmv::ttestOneS';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            crrArg$students = FALSE; crrArg$wilcoxon = TRUE;
            
        }

        # NPTESTS - ONESAMPLE - WILCOXON ---------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', crrSPS[1]) & any(grepl('ONESAMPLE', crrSPS)) & any(grepl('WILCOXON', crrSPS))) {
            warning('NPTESTS - ONESAMPLE - WILCOXON – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPTESTS');            
            crrFnc = 'jmv::ttestOneS';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            crrArg$students = FALSE; crrArg$wilcoxon = TRUE;
            
        }

        # T-TEST (with TESTVAL) -----------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl('^T-TEST', crrSPS[1]) & any(grepl('TESTVAL', crrSPS))) {
            warning('T-TEST - One sample – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'T-TEST');
            crrFnc = 'jmv::ttestOneS';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            crrArg$students = TRUE; crrArg$wilcoxon = FALSE;
            
        }


        # ANOVAs – One-Way ANOVA ==============================================
        # https://www.jamovi.org/jmv/anovaonew.html
        
        # ONEWAY --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-oneway
        else if (grepl('^ONEWAY', crrSPS[1])) {
            warning('ONEWAY – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'ONEWAY');
            crrFnc = 'jmv::anovaOneW';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # UNIANOVA ------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-unianova
        else if (grepl('^UNIANOVA', crrSPS[1])) {
            warning('UNIANOVA – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'UNIANOVA');
            crrFnc = 'jmv::anovaOneW';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # ANOVAs – ANOVA ======================================================
        # https://www.jamovi.org/jmv/anova.html

        # ANOVA ---------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-anova
        else if (grepl('^ANOVA', crrSPS[1])) {
            warning('ANOVA – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'ANOVA');
            crrFnc = 'jmv::ANOVA';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # GLM: Univariate -----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-univariate
        else if (grepl('^GLM', crrSPS[1])) {
            warning('GLM – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'GLM');            
            crrFnc = 'jmv::ANOVA';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # MANOVA: Univariate --------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-univariate
        else if (grepl('^MANOVA', crrSPS[1])) {
            warning('MANOVA – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'MANOVA');
            crrFnc = 'jmv::ANOVA';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # ANOVAs – Repeated Measures ANOVA ====================================
        # https://www.jamovi.org/jmv/anovarm.html

        # GLM: Repeated Measures ----------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-repeated-measures
        else if (grepl('^GLM', crrSPS[1])) {
            warning('GLM – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'GLM');            
            crrFnc = 'jmv::anovaRM';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # MANOVA: Repeated Measures -------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-repeated-measures
        else if (grepl('^MANOVA', crrSPS[1])) {
            warning('MANOVA – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'MANOVA');
            crrFnc = 'jmv::anovaRM';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # ANOVAs – ANCOVA =====================================================
        # https://www.jamovi.org/jmv/ancova.html

        # GLM: Univariate -----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-univariate
        else if (grepl('^GLM', crrSPS[1])) {
            warning('GLM – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'GLM');            
            crrFnc = 'jmv::ancova';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # MANOVA: Univariate --------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-univariate
        else if (grepl('^MANOVA', crrSPS[1])) {
            warning('MANOVA – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'MANOVA');
            crrFnc = 'jmv::ancova';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # ANOVAs – MANCOVA ====================================================
        # https://www.jamovi.org/jmv/mancova.html

        # GLM: Multivariate ---------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-multivariate
        else if (grepl('^GLM', crrSPS[1])) {
            warning('GLM – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'GLM');            
            crrFnc = 'jmv::mancova';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # MANOVA: Multivariate ------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-multivariate
        else if (grepl('^MANOVA', crrSPS[1])) {
            warning('MANOVA – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'MANOVA');
            crrFnc = 'jmv::mancova';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # ANOVAs – One-Way ANOVA (Non-parametric) =============================
        # https://www.jamovi.org/jmv/anovanp.html

        # NPAR TESTS K-W ------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', crrSPS[1]) & any(grepl('K-W', crrSPS))) {
            warning('NPAR TESTS - Kruskal-Wallis – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPAR TESTS');
            crrFnc = 'jmv::anovaNP';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # NPTESTS /INDEPENDENT - KRUSKAL_WALLIS -------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', crrSPS[1]) & any(grepl('INDEPENDENT', crrSPS)) & any(grepl('KRUSKAL_WALLIS', crrSPS))) {
            warning('NPTESTS - Kruskal-Wallis – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPTESTS');            
            crrFnc = 'jmv::anovaNP';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # ANOVAs – Repeated Measures ANOVA (Non-parametric) ===================
        # https://www.jamovi.org/jmv/anovarmnp.html

        # NPAR TESTS FRIEDMAN -------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', crrSPS[1]) & any(grepl('FRIEDMAN', crrSPS))) {
            warning('NPAR TESTS - FRIEDMAN – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPAR TESTS');
            crrFnc = 'jmv::anovaRMNP';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # NPTESTS /RELATED - FRIEDMAN -----------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', crrSPS[1]) & any(grepl('RELATED', crrSPS)) & any(grepl('FRIEDMAN', crrSPS))) {
            warning('NPTESTS - Friedman – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPTESTS');            
            crrFnc = 'jmv::anovaRMNP';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Correlation and regression – Correlation Matrix =====================
        # https://www.jamovi.org/jmv/corrmatrix.html

        # CORRELATIONS --------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-correlations
        else if (grepl('^CORRELATIONS', crrSPS[1])) {
            warning('CORRELATIONS – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'CORRELATIONS');
            crrFnc = 'jmv::corrMatrix';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            crrArg$pearson = TRUE; crrArg$spearman = FALSE; crrArg$kendall = FALSE;
            
        }

        # NONPAR CORR ---------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nonpar-corr
        else if (grepl('^NONPAR CORR', crrSPS[1])) {
            warning('NONPAR CORR – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NONPAR CORR');
            crrFnc = 'jmv::corrMatrix';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            crrArg$pearson = FALSE; crrArg$spearman = TRUE; crrArg$kendall = TRUE;
            
        }


        # Correlation and regression – Partial Correlation ====================
        # https://www.jamovi.org/jmv/corrpart.html

        # PARTIAL CORR --------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-partial-corr
        else if (grepl('^PARTIAL CORR', crrSPS[1])) {
            warning('PARTIAL CORR – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'PARTIAL CORR');
            crrFnc = 'jmv::corrPart';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Correlation and regression – Linear Regression ======================
        # https://www.jamovi.org/jmv/linreg.html

        # LINEAR --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-linear
        else if (grepl('^LINEAR', crrSPS[1])) {
            warning('LINEAR – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'LINEAR');
            crrFnc = 'jmv::linReg';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # REGRESSION ----------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-regression
        else if (grepl('^REGRESSION', crrSPS[1])) {
            warning('REGRESSION – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'REGRESSION');
            crrFnc = 'jmv::linReg';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # Correlation and regression – Binomial Logistic Regression ===========
        # https://www.jamovi.org/jmv/logregbin.html

        # LOGISTIC REGRESSION (outcome variable has two steps) ----------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-logistic-regression
        else if (grepl('^LOGISTIC REGRESSION', crrSPS[1])) {
            warning('LOGISTIC REGRESSION - Binomial – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'LOGISTIC REGRESSION');
            crrFnc = 'jmv::logRegBin';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Correlation and regression – Multinomial Logistic Regression ========
        # https://www.jamovi.org/jmv/logregmulti.html

        # LOGISTIC REGRESSION (outcome variable has more than two steps) ------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-logistic-regression
        else if (grepl('^LOGISTIC REGRESSION', crrSPS[1])) {
            warning('LOGISTIC REGRESSION - Multinomial – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'LOGISTIC REGRESSION');
            crrFnc = 'jmv::logRegMulti';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Correlation and regression – Ordinal Logistic Regression ============
        # https://www.jamovi.org/jmv/logregord.html

        # LOGISTIC REGRESSION (outcome variable has more than two steps and is ordered)
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-logistic-regression
        else if (grepl('^LOGISTIC REGRESSION', crrSPS[1])) {
            warning('LOGISTIC REGRESSION - Ordinal – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'LOGISTIC REGRESSION');
            crrFnc = 'jmv::logRegOrd';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Frequency analyses – Proportion Test (2 Outcomes) ===================
        # https://www.jamovi.org/jmv/proptest2.html

        # NPAR TESTS BINOMIAL -------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', crrSPS[1]) & any(grepl('BINOMIAL', crrSPS))) {
            warning('NPAR TESTS - Binomial – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPAR TESTS');
            crrFnc = 'jmv::propTest2';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # NPTESTS /ONESAMPLE BINOMIAL -------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', crrSPS[1]) & any(grepl('ONESAMPLE', crrSPS)) & any(grepl('BINOMIAL', crrSPS))) {
            warning('NPTESTS - Binomial – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPTESTS');            
            crrFnc = 'jmv::propTest2';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Frequency analyses – Proportion Test (N Outcomes) ===================
        # https://www.jamovi.org/jmv/proptestn.html

        # NPAR TESTS CHISQUARE ------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', crrSPS[1]) & any(grepl('CHISQUARE', crrSPS))) {
            warning('NPAR TESTS - Chi-squared – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPAR TESTS');
            crrFnc = 'jmv::propTestN';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # NPTESTS /ONESAMPLE CHISQUARE ----------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', crrSPS[1]) & any(grepl('ONESAMPLE', crrSPS)) & any(grepl('CHISQUARE', crrSPS))) {
            warning('NPTESTS - Binomial – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPTESTS');            
            crrFnc = 'jmv::propTest2';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Frequency analyses – Contingency Tables =============================
        # https://www.jamovi.org/jmv/conttables.html

        # CROSSTABS -----------------------------------------------------------
        else if (grepl('^CROSSTABS', crrSPS[1])) {
            warning('CROSSTABS – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'CROSSTABS');
            crrFnc = 'jmv::contTables';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Frequency analyses – Paired Samples Contingency Tables ==============
        # https://www.jamovi.org/jmv/conttablespaired.html

        # NPAR TESTS MCNEMAR --------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', crrSPS[1]) & any(grepl('MCNEMAR', crrSPS))) {
            warning('NPAR TESTS MCNEMAR – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPAR TESTS');
            crrFnc = 'jmv::contTablesPaired';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # NPTESTS /RELATED MCNEMAR ----------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', crrSPS[1]) & any(grepl('RELATED', crrSPS)) & any(grepl('MCNEMAR', crrSPS))) {
            warning('NPTESTS - Binomial – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'NPTESTS');            
            crrFnc = 'jmv::contTablesPaired';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Frequency analyses – Log-Linear Regression ==========================
        # https://www.jamovi.org/jmv/loglinear.html

        # LOGLINEAR -----------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-loglinear
        else if (grepl('^LOGLINEAR', crrSPS[1])) {
            warning('LOGLINEAR – not implemented yet');
            crrFnc = 'jmv::logLinear';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Factor analysis – Reliability Analysis ==============================
        # https://www.jamovi.org/jmv/reliability.html

        # RELIABILITY ---------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-reliability
        else if (grepl('^RELIABILITY', crrSPS[1])) {
            warning('RELIABILITY – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'RELIABILITY');            
            crrFnc = 'jmv::reliability';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Factor analysis – Principal Component Analysis ======================
        # https://www.jamovi.org/jmv/pca.html

        # FACTOR - /EXTRACTION = PC, PA1, DEFAULT -----------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-factor
        else if (grepl('^', crrSPS[1])) {
            warning('FACTOR - PCA – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'FACTOR');            
            crrFnc = 'jmv::pca';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }


        # Factor analysis – Exploratory Factor Analysis =======================
        # https://www.jamovi.org/jmv/efa.html

        # FACTOR - /EXTRACTION = PAF, PA2, ML, GLS / ULS ----------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-factor
        # PAF, PA2 → Principal axis 
        # ML → maximum likelihood 
        # CHECK: unclear whether "minimum residuals" = GLS / ULS
        else if (grepl('^', crrSPS[1])) {
            warning('FACTOR - EFA – not implemented yet');
            crrSPS = rmvCmd(crrSPS, 'FACTOR');            
            crrFnc = 'jmv::efa';
            crrArg = eval(parse(text = paste0('formals(', crrFnc, ')')));
            
        }

        # Factor analysis – Confirmatory Factor Analysis ======================
        # https://www.jamovi.org/jmv/cfa.html
        
        # not contained in SPSS -----------------------------------------------

        # =====================================================================
        
        # other not (yet) implemented SPSS-commands ---------------------------
        else {
            print(paste('SPSS-command', crrSPS[1], 'not (yet) implemented.'));
            crrFnc = '';
            crrArg = pairlist();
        }

        # add converted command / function the the syntax list ================ 
        if (crrFnc > '') {
            lstJMV = c(lstJMV, gsub('list\\( ', paste0(crrFnc, '('), gsub('data=,', 'data=data,', gsub('\\n\\s+', ' ', jmvcore::sourcify(crrArg)))));
        }
    }

    attr(data, 'syntax') = lstJMV;
    data
}

chkVar <- function(lstVar = list(), data = data.frame()) {
    if (is.character(lstVar)) { lstVar = strsplit(lstVar, ' ')[[1]]; }
    allVar = names(data);
    for (crrVar in lstVar) {
        if (! crrVar %in% allVar) { stop(paste('Variable', crrVar, 'is contained in the SPSS-syntax but not in the current data set.')); }  
    }
}

asgArg <- function(lstArg = list(), addArg = list()) {
    nmeArg = names(lstArg)
    for (i in 1:length(addArg)) {
        # check whether the argument exists in lstArg
        if (! names(addArg[i]) %in% nmeArg) { stop(paste('Argument', names(addArg[i]), 'is not a valid argumenent / parameter name.')); }
        # check whether the variable type of the argument to add and the argument in the parameter list is the same
        if (typeof(addArg[[i]]) != typeof(lstArg[[names(addArg[i])]])) { warning(paste('Variable type is not the same for the argument', names(addArg[[i]]), '–', typeof(addArg[[i]]), 'vs.',  typeof(lstArg[[names(addArg[i])]]))); };
        lstArg[[names(addArg[i])]] <- addArg[[i]];
    }
    lstArg
}

rmvCmd <- function(crrSPS = list(), crrCmd = "") {
    crrSPS[1] = trimws(gsub(crrCmd, '', crrSPS[1]));
    crrSPS[crrSPS != ""]
}
