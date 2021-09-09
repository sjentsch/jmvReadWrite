#' Converts SPPS-syntax (either from a SPSS-syntax file .sps or read from a SPSS-output-file .spv [using spv2sps]) into jamovi / jmv-syntax
#'
#' The function expects that either the parameter vecSPS is used [with the output from spv2sps] or that both fleSPS and fleSAV are given.
#'
#' @param character vector with SPSS-commands, the format is supposed to be the same as what is output from spv2sps: a character vector containing one command per element, and the attribute "datafile" that contains a string pointing to the location of the SPSS-data-file [incl. path] (if not found at the path or if no path is given, the file is searched in the current directory)
#' @param string containing the location of a SPSS-syntax-file [.sps; incl. path]
#' @param string containing the location of a SPSS-data-file [.sav; incl. path]
#' @param string a filter condition to be applied to the dataset (default = "" → no filter; e.g., "VARNAME == 1")
#' 
#' @return list with jamovi / jmv-analysis-function-calls and the data set stored in the attribute "dataset"
#'
#' @export sps2jmv
#'
sps2jmv <- function(vecSPS = c(), fleSPS = "", fleSAV = "", fltCnd = "") {

    # check the input parameter, at least either vecSPS has to be given (and to contain the attribute "datafile" with the location of an SPSS-SAV-file)
    # or a combination of fleSPS and fleSAV has to be given (and both files have to exist)
    # if fleSAV is given together with vecSPS than, fleSAV overrides the file given in the attribute "datafile"
    if (length(vecSPS) > 0) {
        # if fleSAV was given as parameter 
        if (fleSAV == "" && ! is.null(attr(vecSPS, "datafile"))) { fleSAV = attr(vecSPS, "datafile"); }
    } else if (fleSPS > "") {
        # check whether fleSPS exists, load it and do some plausibility checks
        vecSPS = getSPS(fleSPS)
    }
    if (fleSAV > "") {
        if (! file.exists(fleSAV) && file.exists(file.path(getwd(), basename(fleSAV)))) { fleSAV = file.path(getwd(), basename(fleSAV)); }
        data = suppressMessages(suppressWarnings(foreign::read.spss(file = fleSAV, to.data.frame=TRUE)));
    } else {
        stop(paste("sps2jmv requires an SPSS-data-file. This can be either provided by vecSPS containing an attribute \"datafile\" or via the parameter fleSAV.",
                   "This should be a character vector containing the position of the SPSS-data-file (.sav; incl. path)."));
    }
    if (length(vecSPS) == 0) {
        stop(paste("sps2jmv requires either a list [vecSPS] containing one SPSS-command (each as character vector) per list entry,",
                   "or a parameter [fleSPS] pointing to a SPSS-syntax-file (.sps; incl. path)."));
    }

    vecJMV = c();

    for (i in 1:length(vecSPS)) {
       
        # General: Split, handle generic attributes (e.g., VAR1 TO VARx) ==============================================================================================================================
        if (grepl(grpCnv, vecSPS[i])) {
            crrSPS = strSpl(gsub("\\.$", "", vecSPS[i]), " /");
            crrFnc = "";
            crrVar = getVar(crrSPS);
            # some commands don't require a variable list (they are parsed using other approaches)
            if (length(crrVar) == 0 && ! grepl("^COMPUTE", crrSPS[1])) { stop(sprintf("Variable list empty. - Current command: %s\n\n", crrSPS[1])); }
        } else if (grepl("^DATASET\\s+|SAVE\\s+OUTFILE\\s*=|GET\\s+FILE\\s*=", vecSPS[i])) {
            vecJMV = c(vecJMV, sprintf("# %s – This SPSS-command is used to handle datasets and data files in SPSS and therefore not implemented.", vecSPS[i]));
            next
        } else if (grepl("^EXECUTE\\.$", vecSPS[i])) {
            vecJMV = c(vecJMV, sprintf("# %s – This SPSS-command is used to execute previous COMPUTE, FILTER, etc.-commands and therefore not implemented.", vecSPS[i]));
            next
        } else {
            warning(sprintf("SPSS-command in l. %d - \"%s\" not (yet) implemented.", i, vecSPS[i]));
            next
        }

        # Modifications to the data set ===============================================================================================================================================================

        # ADD VALUE LABELS ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-add-value-labels
        # (adds new value labels but keeps existing ones, see also VALUE LABELS)
        if      (grepl("^ADD VALUE LABELS", crrSPS[1])) {
            stop(sprintf("ADD VALUE LABELS – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }
        
        # ALTER TYPE ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-alter-type
        else if (grepl("^ALTER TYPE", crrSPS[1])) {
            stop(sprintf("ALTER TYPE – not implemented yet: l. %d - %s", i, vecSPS[i]));
            
        }
        
        # COMPUTE -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-compute
        # https://github.com/jamovi/jamovi/blob/current-dev/client/main/vareditor/formulatoolbar.js#L48 (which functions are implemented in jamovi)
        else if (grepl("^COMPUTE", crrSPS[1])) {
            if (grepl('filter_\\$', crrSPS[1])) {
                fltCnd = gsub("^\\(|\\)$", "", strSpl(crrSPS[1], "filter_\\$\\s*=")[2]);
            } else {
                data = cmpFrm(crrSPS, data);
            }
        }

        # DELETE VARIABLES ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-compute
        else if (grepl("^DELETE VARIABLES", crrSPS[1])) {
            if (grepl('filter_\\$', crrSPS[1])) {
                fltCnd = "";
            } else {
                stop(sprintf("DELETE VARS – not implemented yet: l. %d - %s", i, vecSPS[i]));
                data = subset(data, select = -get(trimws(gsub("DELETE VARIABLES", "", crrSPS[1]))));
            }
        }

        # NUMERIC -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-numeric
        else if (grepl("^NUMERIC", crrSPS[1])) {
            stop(sprintf("NUMERIC – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # RANK ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-rank
        else if (grepl("^RANK", crrSPS[1])) {
            stop(sprintf("RANK – not implemented yet: l. %d - %s", i, vecSPS[i]));
            # COMPUTED → RANK(VARNAME)            
        }

        # RECODE --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-recode
        else if (grepl("^RECODE", crrSPS[1])) {
            stop(sprintf("RECODE – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # RENAME VARIABLES ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-rename-variables
        else if (grepl("^RENAME VARIABLES", crrSPS[1])) {
            stop(sprintf("RENAME VARIABLES – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # SAMPLE --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sample
        else if (grepl("^SAMPLE", crrSPS[1])) {
            stop(sprintf("SAMPLE – not implemented yet: l. %d - %s", i, vecSPS[i]));
            # COMPUTED → SAMPLE(1, N FROM M / DECIMAL VALUE, 0) + FILTER
            # double-check whether the sample changes, otherwise use a fixed assignment based on R-sample()
        }

        # SORT CASES ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sort-cases
        else if (grepl("^SORT CASES", crrSPS[1])) {
            stop(sprintf("SORT CASES – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # SORT VARIABLES ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sort-variables
        else if (grepl("^SORT VARIABLES", crrSPS[1])) {
            stop(sprintf("SORT VARIABLES – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # STRING --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-string
        else if (grepl("^STRING", crrSPS[1])) {
            stop(sprintf("STRING – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # VALUE LABELS --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-value-labels
        else if (grepl("^VALUE LABELS", crrSPS[1])) {
            stop(sprintf("VALUE LABELS – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }
   
        # VARIABLE LABELS -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-variable-labels
        else if (grepl("^VARIABLE LABELS", crrSPS[1])) {
            stop(sprintf("VARIABLE LABELS – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # VARIABLE LEVEL ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-variable-level
        else if (grepl("^VARIABLE LEVEL", crrSPS[1])) {
            stop(sprintf("VARIABLE LEVEL – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }


        # Filtering (and removing the filter) =========================================================================================================================================================
        # see also COMPUTE and DELETE VARIABLES in combination with the SPSS-typical filter variable "filter_$"

        # FILTER --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-filter
        else if (grepl("^FILTER", crrSPS[1])) {
            if (!grepl("filter_\\S", crrSPS[1])) { fltCnd = gsub("FILTER\\s+BY\\s+", "", crrSPS[1]); }
            fltCnd = chkFlt(fltCnd, data);
        }

        # USE -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-use
        else if (grepl("^USE", crrSPS[1])) {
            if (grepl("^USE ALL$", crrSPS[1])) {
                fltCnd = "";
            } else {
                vecJMV = c(vecJMV, sprintf("# %s – This SPSS-command is used to designate a time range of observations (to be used with time series procedures) and therefore not implemented.", crrSPS[1]));
            }           
        }


        # Comments ====================================================================================================================================================================================

        # COMMENT -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-comment
        else if (grepl("^COMMENT", crrSPS[1])) {
            stop(sprintf("COMMENT – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }


        # Exploration – Descriptives ==================================================================================================================================================================
        # https://www.jamovi.org/jmv/descriptives.html

        # DESCRIPTIVES --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-descriptives
        else if (grepl("^DESCRIPTIVES", crrSPS[1])) {
            print("DESCRIPTIVES");
            crrFnc = "jmv::descriptives";
            # "formula" and STATISTICS
            crrArg = updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar),
                                                     freq         = FALSE,
                                                     desc         = "columns",
                                                     mean         = any(grepl("^STATISTICS", crrSPS) & (grepl("MEAN",     crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     sd           = any(grepl("^STATISTICS", crrSPS) & (grepl("STDDEV",   crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     min          = any(grepl("^STATISTICS", crrSPS) & (grepl("MIN",      crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     max          = any(grepl("^STATISTICS", crrSPS) & (grepl("MAX",      crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     se           = any(grepl("^STATISTICS", crrSPS) & (grepl("SEMEAN",   crrSPS)                            | grepl("ALL", crrSPS))),
                                                     variance     = any(grepl("^STATISTICS", crrSPS) & (grepl("VARIANCE", crrSPS)                            | grepl("ALL", crrSPS))),
                                                     skew         = any(grepl("^STATISTICS", crrSPS) & (grepl("SKEWNESS", crrSPS)                            | grepl("ALL", crrSPS))),
                                                     kurt         = any(grepl("^STATISTICS", crrSPS) & (grepl("KURTOSIS", crrSPS)                            | grepl("ALL", crrSPS))),
                                                     sum          = any(grepl("^STATISTICS", crrSPS) & (grepl("SUM",      crrSPS)                            | grepl("ALL", crrSPS))),
                                                     range        = any(grepl("^STATISTICS", crrSPS) & (grepl("RANGE",    crrSPS)                            | grepl("ALL", crrSPS))),
                                                     median       = FALSE,
                                                     mode         = FALSE,
                                                     sum          = FALSE,
                                                     ci           = FALSE,
                                                     iqr          = FALSE,
                                                     sw           = FALSE,
                                                     pc           = FALSE));
            # not implemented in jmv: MISSING, SAVE, SORT
            #                         SAVE could be implemented manually later - generate computed variable Z()
        }

        # EXAMINE -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-examine
        else if (grepl("^EXAMINE", crrSPS[1])) {
            print("EXAMINE");
            crrFnc = "jmv::descriptives";
            # "formula" and STATISTICS and PLOT subbcommands
            crrArg = updArg(getArg(crrFnc), pairlist(formula      = fmtFrm(crrVar, "formula"),
                                                     freq         = FALSE,
                                                     desc         = "columns",
                                                     mean         = any(grepl("^STATISTICS[=,\\s]DESCRIPTIVES", crrSPS, perl = TRUE) | grepl("^STATISTICS[=,\\s]ALL", crrSPS, perl = TRUE)),
                                                     median       = any(grepl("^STATISTICS[=,\\s]DESCRIPTIVES", crrSPS, perl = TRUE) | grepl("^STATISTICS[=,\\s]ALL", crrSPS, perl = TRUE)),
                                                     se           = any(grepl("^STATISTICS[=,\\s]DESCRIPTIVES", crrSPS, perl = TRUE) | grepl("^STATISTICS[=,\\s]ALL", crrSPS, perl = TRUE)),
                                                     variance     = any(grepl("^STATISTICS[=,\\s]DESCRIPTIVES", crrSPS, perl = TRUE) | grepl("^STATISTICS[=,\\s]ALL", crrSPS, perl = TRUE)),
                                                     sd           = any(grepl("^STATISTICS[=,\\s]DESCRIPTIVES", crrSPS, perl = TRUE) | grepl("^STATISTICS[=,\\s]ALL", crrSPS, perl = TRUE)),
                                                     min          = any(grepl("^STATISTICS[=,\\s]DESCRIPTIVES", crrSPS, perl = TRUE) | grepl("^STATISTICS[=,\\s]ALL", crrSPS, perl = TRUE)),
                                                     max          = any(grepl("^STATISTICS[=,\\s]DESCRIPTIVES", crrSPS, perl = TRUE) | grepl("^STATISTICS[=,\\s]ALL", crrSPS, perl = TRUE)),
                                                     range        = any(grepl("^STATISTICS[=,\\s]DESCRIPTIVES", crrSPS, perl = TRUE) | grepl("^STATISTICS[=,\\s]ALL", crrSPS, perl = TRUE)),
                                                     iqr          = any(grepl("^STATISTICS[=,\\s]DESCRIPTIVES", crrSPS, perl = TRUE) | grepl("^STATISTICS[=,\\s]ALL", crrSPS, perl = TRUE)),
                                                     skew         = any(grepl("^STATISTICS[=,\\s]DESCRIPTIVES", crrSPS, perl = TRUE) | grepl("^STATISTICS[=,\\s]ALL", crrSPS, perl = TRUE)),
                                                     kurt         = any(grepl("^STATISTICS[=,\\s]DESCRIPTIVES", crrSPS, perl = TRUE) | grepl("^STATISTICS[=,\\s]ALL", crrSPS, perl = TRUE)),
                                                     mode         = FALSE,
                                                     sum          = FALSE,            
                                                     box          = any(grepl("^PLOT", crrSPS) & (grepl("BOXPLOT",   crrSPS) | grepl("ALL", crrSPS))),
                                                     hist         = any(grepl("^PLOT", crrSPS) & (grepl("HISTOGRAM", crrSPS) | grepl("ALL", crrSPS))),
                                                     qq           = any(grepl("^PLOT", crrSPS) & (grepl("NPPLOT",    crrSPS) | grepl("ALL", crrSPS))),
                                                     sw           = any(grepl("^PLOT", crrSPS) & (grepl("NPPLOT",    crrSPS) | grepl("ALL", crrSPS))),
                                                     dens         = FALSE,
                                                     bar          = FALSE,
                                                     violin       = FALSE,
                                                     dot          = FALSE,
                                                     boxMean      = FALSE));
            # CINTERVAL and PERCENTILES subcommand
            if (any(grepl("^CINTERVAL", crrSPS)))      { crrArg = updArg(crrArg, argCI(crrSPS)); }
            if (any(grepl("^PERCENTILES\\(", crrSPS))) { crrArg = updArg(crrArg, argTls(crrSPS)); }
            # not implemented in jmv: COMPARE (not necessary), TOTAL, ID, MESTIMATOR, MISSING, STATISTICS subcommand: EXTREMES
        }

        # FREQUENCIES ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-frequencies
        else if (grepl("^FREQUENCIES", crrSPS[1])) {
            print("FREQUENCIES");
            crrFnc = "jmv::descriptives";
            # "formula" and STATISTICS, BARCHART and HISTOGRAM subcommand
            crrArg = updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar),
                                                     freq         = TRUE,
                                                     desc         = "columns",
                                                     mean         = any(grepl("^STATISTICS", crrSPS) & (grepl("MEAN",     crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     sd           = any(grepl("^STATISTICS", crrSPS) & (grepl("STDDEV",   crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     min          = any(grepl("^STATISTICS", crrSPS) & (grepl("MIN",      crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     max          = any(grepl("^STATISTICS", crrSPS) & (grepl("MAX",      crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     median       = any(grepl("^STATISTICS", crrSPS) & (grepl("MEDIAN",   crrSPS)                            | grepl("ALL", crrSPS))),
                                                     se           = any(grepl("^STATISTICS", crrSPS) & (grepl("SEMEAN",   crrSPS)                            | grepl("ALL", crrSPS))),
                                                     variance     = any(grepl("^STATISTICS", crrSPS) & (grepl("VARIANCE", crrSPS)                            | grepl("ALL", crrSPS))),
                                                     skew         = any(grepl("^STATISTICS", crrSPS) & (grepl("SKEWNESS", crrSPS)                            | grepl("ALL", crrSPS))),
                                                     kurt         = any(grepl("^STATISTICS", crrSPS) & (grepl("KURTOSIS", crrSPS)                            | grepl("ALL", crrSPS))),
                                                     sum          = any(grepl("^STATISTICS", crrSPS) & (grepl("SUM",      crrSPS)                            | grepl("ALL", crrSPS))),
                                                     range        = any(grepl("^STATISTICS", crrSPS) & (grepl("RANGE",    crrSPS)                            | grepl("ALL", crrSPS))),
                                                     mode         = any(grepl("^STATISTICS", crrSPS) & (grepl("MODE",     crrSPS)                            | grepl("ALL", crrSPS))),
                                                     sum          = any(grepl("^STATISTICS", crrSPS) & (grepl("SUM",      crrSPS)                            | grepl("ALL", crrSPS))),
                                                     ci           = FALSE,
                                                     iqr          = FALSE,
                                                     sw           = FALSE,
                                                     bar          = any(grepl("^BARCHART",  crrSPS)),
                                                     barCounts    = any(grepl("^BARCHART",  crrSPS)),
                                                     hist         = any(grepl("^HISTOGRAM", crrSPS)),
                                                     dens         = any(grepl("^HISTOGRAM", crrSPS))));
            # NTILES and PERCENTILES subcommand
            if (any(grepl("^NTILES|^PERCENTILES", crrSPS))) { crrArg = updArg(crrArg, argTls); }
            # not implemented in jmv: FORMAT, MISSING, PIECHART, GROUPED, ORDER;
            #                         BARCHART and HISTOGRAM subcommand: MINIMUM, MAXIMUM, FREQ (define the minima and maxima of the values [x-axis] and maximum for frequencies [Y-axis]),
            #                                                            PERCENT (BARCHART: maximum percentage [y-axis], NORMAL (HISTOGRAM: replaced by dens)
        }

        # MEANS ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-means
        else if (grepl("^MEANS", crrSPS[1])) {
            stop("MEANS");
            crrFnc = "jmv::descriptives";
            # formula (NB: Only the first variable list is considered) and CELLS subcommand
            crrArg = updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar),
                                                     freq         = FALSE,
                                                     desc         = "columns",
                                                     mean         = any(grepl("^CELLS", crrSPS) & (grepl("MEAN",     crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     sd           = any(grepl("^CELLS", crrSPS) & (grepl("STDDEV",   crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     median       = any(grepl("^CELLS", crrSPS) & (grepl("MEDIAN",   crrSPS)                            | grepl("ALL", crrSPS))),
                                                     se           = any(grepl("^CELLS", crrSPS) & (grepl("SEMEAN",   crrSPS)                            | grepl("ALL", crrSPS))),
                                                     sum          = any(grepl("^CELLS", crrSPS) & (grepl("SUM",      crrSPS)                            | grepl("ALL", crrSPS))),
                                                     min          = any(grepl("^CELLS", crrSPS) & (grepl("MIN",      crrSPS)                            | grepl("ALL", crrSPS))),
                                                     max          = any(grepl("^CELLS", crrSPS) & (grepl("MAX",      crrSPS)                            | grepl("ALL", crrSPS))),
                                                     range        = any(grepl("^CELLS", crrSPS) & (grepl("RANGE",    crrSPS)                            | grepl("ALL", crrSPS))),
                                                     variance     = any(grepl("^CELLS", crrSPS) & (grepl("VARIANCE", crrSPS)                            | grepl("ALL", crrSPS))),
                                                     kurt         = any(grepl("^CELLS", crrSPS) & (grepl("KURT",     crrSPS)                            | grepl("ALL", crrSPS))),
                                                     skew         = any(grepl("^CELLS", crrSPS) & (grepl("SKEW",     crrSPS)                            | grepl("ALL", crrSPS))),
                                                     mode         = FALSE,
                                                     missing      = FALSE,
                                                     ci           = FALSE,
                                                     iqr          = FALSE,
                                                     sw           = FALSE));
            # not implemented in jmv: MISSING (jmv's missing shows the number of missing cases, SPSS's excludes cases), STATISTICS;
            #                         CELLS subcommand: GMEDIAN, SEKURT, SESKEW, FIRST, LAST, NPCT, SPCT, HARMONIC, GEOMETRIC
        }
        
        # NPTEST /ONESAMPLE TEST ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("ONESAMPLE TEST \\(.*\\)$", crrSPS))) {
            print("NPTESTS ONESAMPLE");
            crrFnc = "jmv::descriptives";
            crrArg = updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar),
                                                     freq         = FALSE,
                                                     desc         = "rows",
                                                     mean         = FALSE,
                                                     sd           = FALSE,
                                                     median       = FALSE,
                                                     se           = FALSE,
                                                     sum          = FALSE,
                                                     min          = FALSE,
                                                     max          = FALSE,
                                                     range        = FALSE,
                                                     variance     = FALSE,
                                                     kurt         = FALSE,
                                                     skew         = FALSE,
                                                     mode         = FALSE,
                                                     missing      = FALSE,
                                                     ci           = FALSE,
                                                     iqr          = FALSE,
                                                     sw           = TRUE));
            # not implemented in jmv: MISSING (jmv's missing shows the number of missing cases, SPSS's excludes cases), CRITERIA
        }        

        # SUMMARIZE -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-summarize
        else if (grepl("^SUMMARIZE", crrSPS[1])) {
            stop("SUMMARIZE");
            crrFnc = "jmv::descriptives";
            # "vars" (NB: Only the first variable list is considered) and CELLS subcommand
            crrArg = updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar),
                                                     freq         = FALSE,
                                                     mean         = any(grepl("^CELLS", crrSPS) & (grepl("MEAN",     crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     sd           = any(grepl("^CELLS", crrSPS) & (grepl("STDDEV",   crrSPS) | grepl("DEFAULT", crrSPS) | grepl("ALL", crrSPS))),
                                                     median       = any(grepl("^CELLS", crrSPS) & (grepl("MEDIAN",   crrSPS)                            | grepl("ALL", crrSPS))),
                                                     se           = any(grepl("^CELLS", crrSPS) & (grepl("SEMEAN",   crrSPS)                            | grepl("ALL", crrSPS))),
                                                     sum          = any(grepl("^CELLS", crrSPS) & (grepl("SUM",      crrSPS)                            | grepl("ALL", crrSPS))),
                                                     min          = any(grepl("^CELLS", crrSPS) & (grepl("MIN",      crrSPS)                            | grepl("ALL", crrSPS))),
                                                     max          = any(grepl("^CELLS", crrSPS) & (grepl("MAX",      crrSPS)                            | grepl("ALL", crrSPS))),
                                                     range        = any(grepl("^CELLS", crrSPS) & (grepl("RANGE",    crrSPS)                            | grepl("ALL", crrSPS))),
                                                     variance     = any(grepl("^CELLS", crrSPS) & (grepl("VARIANCE", crrSPS)                            | grepl("ALL", crrSPS))),
                                                     kurt         = any(grepl("^CELLS", crrSPS) & (grepl("KURT",     crrSPS)                            | grepl("ALL", crrSPS))),
                                                     skew         = any(grepl("^CELLS", crrSPS) & (grepl("SKEW",     crrSPS)                            | grepl("ALL", crrSPS))),
                                                     mode         = FALSE,
                                                     missing      = FALSE,
                                                     ci           = FALSE,
                                                     iqr          = FALSE,
                                                     sw           = FALSE));
            # not implemented in jmv: TITLE, FOOTNOTE, MISSING (jmv's missing shows the number of missing cases, SPSS's excludes cases), FORMAT, STATISTICS;
            #                         CELLS subcommand: GMEDIAN, SEKURT, SESKEW, FIRST, LAST, NPCT, SPCT, HARMONIC, GEOMETRIC
        }


        # T-Tests – Independent Samples T-Test ========================================================================================================================================================
        # https://www.jamovi.org/jmv/ttestis.html

        # NPAR TESTS - M-W ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("M-W", crrSPS))) {
            stop(sprintf("NPAR TESTS - M-W – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ttestIS";
            crrArg = updArg(getArg(crrFnc), pairlist(students     = FALSE,
                                                     welchs       = FALSE,
                                                     mann         = TRUE));
            # CINTERVAL and PERCENTILES subcommand
            
            # not implemented in jmv:            
        }

        # NPTESTS - INDEPENDENT - MANN_WHITNEY --------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("INDEPENDENT", crrSPS)) & any(grepl("MANN_WHITNEY", crrSPS))) {
            stop(sprintf("NPTESTS - INDEPENDENT - MANN_WHITNEY – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ttestIS";
            crrArg = updArg(getArg(crrFnc), pairlist(students = FALSE, welchs = FALSE, mann = TRUE));
            
            # not implemented in jmv:            
        }

        # T-TEST (with GROUPS) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl("^T-TEST GROUPS", crrSPS[1])) {
            print("T-TEST GROUPS");
            crrFnc = "jmv::ttestIS";
            crrArg = updArg(getArg(crrFnc), pairlist(formula      = fmtFrm(crrVar, "formula"),
                                                     students     = TRUE,
                                                     welchs       = TRUE,
                                                     mann         = FALSE,
                                                     eqv          = TRUE,
                                                     meanDiff     = TRUE,
                                                     effectSize   = any(grepl("^ES ", crrSPS))));
            # CRITERIA=CI and MISSING subcommand
            if (any(grepl("^CRITERIA=CI", crrSPS))) { crrArg = updArg(crrArg,  argCI(crrSPS)); }
            if (any(grepl("^MISSING",     crrSPS))) { crrArg = updArg(crrArg, argMsV(crrSPS)); }
            # not implemented in jmv: subcommand ES: STANDARDIZER (defaults to SD, i.e., Cohen's d - which is what jmv gives)
        }


        # T-Tests – Paired Samples T-Test =============================================================================================================================================================
        # https://www.jamovi.org/jmv/ttestps.html

        # NPAR TESTS - WILCOXON - PAIRED --------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("WILCOXON", crrSPS)) & any(grepl("PAIRED", crrSPS))) {
            stop(sprintf("NPAR TESTS - WILCOXON - PAIRED – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ttestPS";
            crrArg = updArg(getArg(crrFnc), pairlist(students = FALSE, wilcoxon = TRUE));
            
            # not implemented in jmv:            
        }

        # NPTESTS - RELATED - WILCOXON -----------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("RELATED", crrSPS)) & any(grepl("WILCOXON", crrSPS))) {
            stop(sprintf("NPTESTS - RELATED - WILCOXON – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ttestPS";
            crrArg = updArg(getArg(crrFnc), pairlist(students = FALSE, wilcoxon = TRUE));
            
            # not implemented in jmv:            
        }

        # T-TEST PAIRS --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl("^T-TEST PAIRS", crrSPS[1])) {
            print("T-TEST PAIRS");
            crrFnc = "jmv::ttestPS";
            crrArg = updArg(getArg(crrFnc), pairlist(pairs        = fmtVar(crrVar, asPrs = TRUE),
                                                     students     = TRUE,
                                                     wilcoxon     = FALSE,
                                                     meanDiff     = TRUE,
                                                     effectSize   = any(grepl("^ES ", crrSPS))));
            # CRITERIA=CI and MISSING subcommand
            if (any(grepl("^CRITERIA=CI", crrSPS))) { crrArg = updArg(crrArg,  argCI(crrSPS)); }
            if (any(grepl("^MISSING",     crrSPS))) { crrArg = updArg(crrArg, argMsV(crrSPS)); }
            # not implemented in jmv: subcommand ES: STANDARDIZER (defaults to SD, i.e., Cohen's d - which is what jmv gives)
        }


        # T-Tests – One Sample T-Test =================================================================================================================================================================
        # https://www.jamovi.org/jmv/ttestones.html

        # NPAR TESTS - WILCOXON - not PAIRED ----------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("WILCOXON", crrSPS)) & ! any(grepl("PAIRED", crrSPS))) {
            stop(sprintf("NPAR TESTS WILCOXON - not PAIRED (one sample) – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ttestOneS";
            crrArg = updArg(getArg(crrFnc), pairlist(students = FALSE, wilcoxon = TRUE));

            # not implemented in jmv:
        }

        # NPTESTS - ONESAMPLE - WILCOXON ---------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("ONESAMPLE", crrSPS)) & any(grepl("WILCOXON", crrSPS))) {
            stop(sprintf("NPTESTS - ONESAMPLE - WILCOXON – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ttestOneS";
            crrArg = updArg(getArg(crrFnc), pairlist(students = FALSE, wilcoxon = TRUE));

            # not implemented in jmv:
        }

        # T-TEST (with TESTVAL) -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl("^T-TEST", crrSPS[1]) & any(grepl("TESTVAL", crrSPS))) {
            stop(sprintf("T-TEST - One sample – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ttestOneS";
            crrArg = updArg(getArg(crrFnc), pairlist(students = TRUE, wilcoxon = FALSE));

            # not implemented in jmv:
        }


        # ANOVAs – One-Way ANOVA ======================================================================================================================================================================
        # https://www.jamovi.org/jmv/anovaonew.html
        
        # ONEWAY --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-oneway
        else if (grepl("^ONEWAY", crrSPS[1]) && all(cntVar(crrVar) == c(1, 1, 0))) {
            stop(sprintf("ONEWAY – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::anovaOneW";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # UNIANOVA: one dependent variable, one factor, no covariate ----------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-unianova
        else if (grepl("^UNIANOVA", crrSPS[1]) && all(cntVar(crrVar) == c(1, 1, 0))) {
            print("UNIANOVA");
            crrFnc = "jmv::anovaOneW";
            crrArg = updArg(getArg(crrFnc), pairlist(formula      = fmtFrm(crrVar, "formula"),
                                                     welchs       = any(grepl('^PRINT|^STATISTICS',    crrSPS) & grepl('WELCH', crrSPS)),
                                                     fishers      = TRUE,
                                                     desc         = any(grepl('^PRINT|^STATISTICS',    crrSPS) & grepl('DESCRIPTIVES', crrSPS)),
                                                     descPlot     = any(grepl('^PLOT$',                crrSPS)),
                                                     norm         = any(grepl('^SAVE\\s*=\\s*ZRESID$', crrSPS)),
                                                     qq           = any(grepl('^SAVE\\s*=\\s*ZRESID$', crrSPS)),
                                                     eqv          = any(grepl('^PRINT|^STATISTICS',    crrSPS) & grepl('HOMOGENEITY', crrSPS))));
            # POSTHOC and MISSING subcommands
            if (any(grepl("^POSTHOC", crrSPS))) { crrArg = updArg(crrArg, argPHT(crrSPS, crrFnc)); } 
            if (any(grepl("^MISSING", crrSPS))) { crrArg = updArg(crrArg, argMsV(crrSPS)); }

            # not implemented in jmv: RANDOM, REGWGT, METHOD (SS type), INTERCEPT, CRITERIA, TEST, LMATRIX, KMATRIX, CONTRAST, EMMEANS, ROBUST, SAVE, OUTFILE, MBPDESIGN, BPDESIGN, FDESIGN,
            #                         DESIGN (typically not required, as there can only be main effects), subcommand PRINT / STATISTICS (only DESCRIPTIVE, HOMOGENEITY, and WELCH),
            #                         subcommand PLOT (no further plot customization)
            #                         SAVE could be implemented manually later (at least for some outputs, e.g., PRED, RESID, ZRESID, possibly COOK)
        }


        # ANOVAs – ANOVA ==============================================================================================================================================================================
        # https://www.jamovi.org/jmv/anova.html

        # ANOVA ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-anova
        # TO ADD: 1 dependent variable, independent variables as factors (no covariates)
        else if (grepl("^ANOVA", crrSPS[1])) {
            stop(sprintf("ANOVA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ANOVA";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # GLM: Univariate -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-univariate
        # TO ADD: 1 dependent variable, independent variables as factors (no covariates)
        else if (grepl("^GLM", crrSPS[1]) && ! any(grepl("^WSFACTOR", crrSPS)) && cntVar(crrVar)[1] == 1 && cntVar(crrVar)[2] > 1 && cntVar(crrVar)[3] == 0) {
            stop(sprintf("GLM – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ANOVA";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # MANOVA: Univariate --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-univariate
        # TO ADD: 1 dependent variable, independent variables as factors (no covariates), check for repeated measurements version
        else if (grepl("^MANOVA", crrSPS[1]) && ! any(grepl("^WSFACTOR", crrSPS)) && cntVar(crrVar)[1] == 1 && cntVar(crrVar)[2] > 1 && cntVar(crrVar)[3] == 0) {
            stop(sprintf("MANOVA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ANOVA";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # UNIANOVA: one dependent variable, more than one factor, no covariate ------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-unianova
        else if (grepl("^UNIANOVA", crrSPS[1]) && cntVar(crrVar)[1] == 1 && cntVar(crrVar)[2] > 1 && cntVar(crrVar)[3] == 0) {
            print("UNIANOVA → ANOVA");
            crrFnc = "jmv::ANOVA";
            crrArg = updArg(getArg(crrFnc), pairlist(formula      = fmtFrm(crrVar, "formula"),
                                                     modelTest    = TRUE,
                                                     modelTerms   = fmtFrm(getLst(crrSPS, "^DESIGN\\s*="), "terms"),
                                                     ss           = argSS(crrSPS, crrFnc),
                                                     homo         = any(grepl('^PRINT|^STATISTICS',    crrSPS) & grepl('HOMOGENEITY', crrSPS)),
                                                     norm         = any(grepl('^SAVE\\s*=\\s*ZRESID$', crrSPS)),
                                                     qq           = any(grepl('^SAVE\\s*=\\s*ZRESID$', crrSPS))));
            # effect size, CONTRAST, POSTHOC, PLOT-PROFILE and MISSING subcommands
            if (any(grepl('^PRINT|^STATISTICS', crrSPS) & grepl('ETASQ', crrSPS))) { crrArg = updArg(pairlist(effectSize = 'partEta')); }
            if (any(grepl("^CONTRAST",          crrSPS)))                          { crrArg = updArg(crrArg, argCon(crrSPS)); }
            if (any(grepl("^POSTHOC",           crrSPS)))                          { crrArg = updArg(crrArg, argPHT(crrSPS, crrFnc)); }
            if (any(grepl("^PLOT.*?PROFILE",    crrSPS)))                          { crrArg = updArg(crrArg, argEmm(crrSPS)); }
            if (any(grepl("^MISSING",           crrSPS)))                          { crrArg = updArg(crrArg, argMsV(crrSPS)); }

            # not implemented in jmv: RANDOM, REGWGT, INTERCEPT, CRITERIA, TEST, LMATRIX, KMATRIX, CONTRAST, EMMEANS, ROBUST, SAVE, OUTFILE, MBPDESIGN, BPDESIGN, FDESIGN,
            #                         subcommand PRINT / STATISTICS (only HOMOGENEITY)
            #                         SAVE could be implemented manually later (at least for some outputs, e.g., PRED, RESID, ZRESID, possibly COOK)
        }

        # ANOVAs – Repeated Measures ANOVA ============================================================================================================================================================
        # https://www.jamovi.org/jmv/anovarm.html

        # GLM: Repeated Measures ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-repeated-measures
        # TO ADD: if MEASURE contains more than one variable - split models
        else if (grepl("^GLM", crrSPS[1]) && any(grepl("^WSFACTOR", crrSPS))) {
            print("GLM – Repeated Measures");
            crrFnc = "jmv::anovaRM";
            crrVar = splVar(crrVar);
            crrRpM = getRpM(crrSPS, crrVar[[1]]);
            # return number of levels
            # TO-DO: check regularly on "contrasts" (announced as "under development")
            crrArg = updArg(getArg(crrFnc), pairlist(rm           = str2lang(jmvcore::sourcify(crrRpM[[1]])),
                                                     rmCells      = str2lang(jmvcore::sourcify(crrRpM[[2]])),
                                                     ss           = argSS(crrSPS, crrFnc),
                                                     contrasts    = list(NULL), # currently stored in crrRpM[[3]]
                                                     groupSumm    = TRUE));
            # within and between subjects design (WSDESIGN, DESIGN), type of square sum (METHOD), effect size (PRINT), and estimated marginal means (PLOT - PROFILE)
            if (any(grepl("^WSDESIGN\\s*=",     crrSPS))) { crrArg = updArg(crrArg, pairlist(rmTerms      = fmtFrm(getLst(crrSPS, "^WSDESIGN\\s*="), "terms"))); }
            if (any(grepl("^DESIGN\\s*=",       crrSPS))) { crrArg = updArg(crrArg, pairlist(bsTerms      = fmtFrm(getLst(crrSPS,   "^DESIGN\\s*="), "terms"))); }
            if (length(crrVar[[2]]) > 0)                  { crrArg = updArg(crrArg, pairlist(bs           = fmtVar(crrVar[[2]], asLng = TRUE))); }
            if (length(crrVar[[3]]) > 0)                  { crrArg = updArg(crrArg, pairlist(cov          = fmtVar(crrVar[[3]], asLng = TRUE))); }
            if (any(crrRpM[[4]] > 2))                     { crrArg = updArg(crrArg, pairlist(spherTests   = TRUE,
                                                                                             spherCorr    = str2lang("list(\"none\", \"GG\", \"HF\")"))); }
            if (any(grepl("PRINT\\s*=.*?ETASQ", crrSPS))) { crrArg = updArg(crrArg, pairlist(effectSize   = "partEta")); }
            if (any(grepl("^PLOT.*?PROFILE",    crrSPS))) { crrArg = updArg(crrArg, argEmm(crrSPS)); }
            if (any(grepl("^POSTHOC\\s*=",      crrSPS))) { crrArg = updArg(crrArg, argPHT(crrSPS, crrFnc)); }
            # TO ADD: if MEASURE contains more than one variable - split models
            if (any(grepl("^MEASURE\\s*=",      crrSPS))) { crrArg = updArg(crrArg, pairlist(depLabel     = getLst(crrSPS, "^MEASURE\\s*=", FALSE))); }
            # not implemented in jmv: contrasts (POLYNOMIAL, etc.), subcommand REGWGT, subcommand METHOD (only type 2 and 3), subcommand INTERCEPT, subcommand MISSING,
            #                         subcommand PLOT (some parameters), subcommand PRINT (all parameters execpt ETASQ), subcommand SAVE, subcommand CRITERIA, subcommand EMMEANS (more complex customisation)
        }

        # MANOVA: Repeated Measures -------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-repeated-measures
        # TO ADD: check for WSFACTOR
        else if (grepl("^MANOVA", crrSPS[1]) && any(grepl("^WSFACTOR", crrSPS))) {
            stop(sprintf("MANOVA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::anovaRM";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # ANOVAs – ANCOVA =============================================================================================================================================================================
        # https://www.jamovi.org/jmv/ancova.html

        # GLM: Univariate -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-univariate
        # TO ADD: 1 dependent variable, independent variables can contain covariates
        else if (grepl("^GLM", crrSPS[1])    && ! any(grepl("^WSFACTOR", crrSPS)) && cntVar(crrVar)[1] == 1 && all(cntVar(crrVar)[2:3] >= 1)) {
            stop(sprintf("GLM – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ancova";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # MANOVA: Univariate --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-univariate
        # TO ADD: 1 dependent variable, independent variables can contain covariates
        else if (grepl("^MANOVA", crrSPS[1]) && ! any(grepl("^WSFACTOR", crrSPS)) && cntVar(crrVar)[1] == 1 && all(cntVar(crrVar)[2:3] >= 1)) {
            stop(sprintf("MANOVA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ancova";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }
        
        # UNIANOVA: one dependent variable, one or more factors, one or more covariates ---------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-unianova
        # TO ADD: 1 dependent and 1 independent variable (factor, no covariate)
        else if (grepl("^UNIANOVA", crrSPS[1]) && cntVar(crrVar)[1] == 1 && all(cntVar(crrVar)[2:3] >= 1)) {
            stop(sprintf("UNIANOVA → ancova not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::ancova";
            crrArg = updArg(getArg(crrFnc), pairlist(formula      = fmtFrm(crrVar, "formula"),
                                                     modelTest    = TRUE,
                                                     ss           = argSS(crrSPS, crrFnc),
                                                     homo         = any(grepl('^PRINT|^STATISTICS',    crrSPS) & grepl('HOMOGENEITY', crrSPS)),
                                                     norm         = any(grepl('^SAVE\\s*=\\s*ZRESID$', crrSPS)),
                                                     qq           = any(grepl('^SAVE\\s*=\\s*ZRESID$', crrSPS))));
            # effect size, POSTHOC and MISSING subcommands
            if (any(grepl('^PRINT|^STATISTICS', crrSPS) & grepl('ETASQ', crrSPS))) { crrArg = updArg(pairlist(effectSize = 'partEta')); }
            if (any(grepl("^POSTHOC", crrSPS)))                                    { crrArg = updArg(crrArg, argPHT(crrSPS, crrFnc)); }
            if (any(grepl("^PLOT.*?PROFILE", crrSPS)))                             { crrArg = updArg(crrArg, argEmm(crrSPS)); }            

            # not implemented in jmv: RANDOM, REGWGT, INTERCEPT, CRITERIA, TEST, LMATRIX, KMATRIX, CONTRAST, EMMEANS, ROBUST, SAVE, OUTFILE, MBPDESIGN, BPDESIGN, FDESIGN,
            #                         DESIGN, subcommand PRINT / STATISTICS (only HOMOGENEITY)
            #                         SAVE could be implemented manually later (at least for some outputs, e.g., PRED, RESID, ZRESID, possibly COOK)
        }


        # ANOVAs – MANCOVA ============================================================================================================================================================================
        # https://www.jamovi.org/jmv/mancova.html

        # GLM: Multivariate ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-multivariate
        # TO ADD: multiple dependent variables
        else if (grepl("^GLM", crrSPS[1])    && ! any(grepl("^WSFACTOR", crrSPS)) && cntVar(crrVar)[1] > 1 && any(cntVar(crrVar)[2:3] >= 1)) {
            stop(sprintf("GLM – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::mancova";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # MANOVA: Multivariate ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-multivariate
        # TO ADD: multiple dependent variables
        else if (grepl("^MANOVA", crrSPS[1]) && ! any(grepl("^WSFACTOR", crrSPS)) && cntVar(crrVar)[1] > 1 && any(cntVar(crrVar)[2:3] >= 1)) {
            stop(sprintf("MANOVA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::mancova";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # ANOVAs – One-Way ANOVA (Non-parametric) =====================================================================================================================================================
        # https://www.jamovi.org/jmv/anovanp.html

        # NPAR TESTS K-W ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        # TO ADD?: 1 dependent variables, 1 independent variable (factor, not covariate)
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("K-W", crrSPS))) {
            stop(sprintf("NPAR TESTS - Kruskal-Wallis – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::anovaNP";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # NPTESTS /INDEPENDENT - KRUSKAL_WALLIS -------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        # TO ADD?: 1 dependent variables, 1 independent variable (factor, not covariate)
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("INDEPENDENT", crrSPS)) & any(grepl("KRUSKAL_WALLIS", crrSPS))) {
            stop(sprintf("NPTESTS - Kruskal-Wallis – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::anovaNP";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # ANOVAs – Repeated Measures ANOVA (Non-parametric) ===========================================================================================================================================
        # https://www.jamovi.org/jmv/anovarmnp.html

        # NPAR TESTS FRIEDMAN -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("FRIEDMAN", crrSPS))) {
            stop(sprintf("NPAR TESTS - FRIEDMAN – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::anovaRMNP";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # NPTESTS /RELATED - FRIEDMAN -----------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("RELATED", crrSPS)) & any(grepl("FRIEDMAN", crrSPS))) {
            stop(sprintf("NPTESTS - Friedman – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::anovaRMNP";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Correlation and regression – Correlation Matrix =============================================================================================================================================
        # https://www.jamovi.org/jmv/corrmatrix.html

        # CORRELATIONS --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-correlations
        else if (grepl("^CORRELATIONS", crrSPS[1])) {
            print("CORRELATIONS");
            crrFnc = "jmv::corrMatrix";
            crrArg = updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar),
                                                     pearson      = TRUE,
                                                     spearman     = FALSE,
                                                     kendall      = FALSE,
                                                     sig          = TRUE,
                                                     flag         = any(grepl("^PRINT.*?NOSIG", crrSPS)),
                                                     n            = TRUE,
                                                     hypothesis   = ifelse(any(grepl("^PRINT.*?ONETAIL", crrSPS)), "pos", "corr")));
            if (any(grepl("^CI ", crrSPS))) { crrArg = updArg(crrArg, argCI(crrSPS)); }
            # not implemented in jmv: MISSING, PRINT [LOWER, LNODIAG, NOMATRIX], MATRIX, STATISTICS, calculation method and bias from CI
            # NOTE: SPSS only implements ONETAIL without sepcifying the direction (pos. / neg. correlations); it was assumed that "pos" would be more likely
        }

        # NONPAR CORR ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nonpar-corr
        else if (grepl("^NONPAR CORR", crrSPS[1])) {
            stop(sprintf("NONPAR CORR – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::corrMatrix";
            crrArg = updArg(getArg(crrFnc), pairlist(pearson      = FALSE,
                                                     spearman     = TRUE,
                                                     kendall      = TRUE));

            # not implemented in jmv:
        }


        # Correlation and regression – Partial Correlation ============================================================================================================================================
        # https://www.jamovi.org/jmv/corrpart.html

        # PARTIAL CORR --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-partial-corr
        else if (grepl("^PARTIAL CORR", crrSPS[1])) {
            stop(sprintf("PARTIAL CORR – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::corrPart";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Correlation and regression – Linear Regression ==============================================================================================================================================
        # https://www.jamovi.org/jmv/linreg.html

        # LINEAR --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-linear
        else if (grepl("^LINEAR", crrSPS[1])) {
            stop(sprintf("LINEAR – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::linReg";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # REGRESSION ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-regression
        else if (grepl("^REGRESSION", crrSPS[1])) {
            stop(sprintf("REGRESSION – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::linReg";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # Correlation and regression – Binomial Logistic Regression ===================================================================================================================================
        # https://www.jamovi.org/jmv/logregbin.html

        # LOGISTIC REGRESSION (outcome variable has two steps) ----------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-logistic-regression
        else if (grepl("^LOGISTIC REGRESSION", crrSPS[1])) {
            stop(sprintf("LOGISTIC REGRESSION - Binomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::logRegBin";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Correlation and regression – Multinomial Logistic Regression ================================================================================================================================
        # https://www.jamovi.org/jmv/logregmulti.html

        # LOGISTIC REGRESSION (outcome variable has more than two steps) ------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-logistic-regression
        else if (grepl("^LOGISTIC REGRESSION", crrSPS[1])) {
            stop(sprintf("LOGISTIC REGRESSION - Multinomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::logRegMulti";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Correlation and regression – Ordinal Logistic Regression ====================================================================================================================================
        # https://www.jamovi.org/jmv/logregord.html

        # LOGISTIC REGRESSION (outcome variable has more than two steps and is ordered)
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-logistic-regression
        else if (grepl("^LOGISTIC REGRESSION", crrSPS[1])) {
            stop(sprintf("LOGISTIC REGRESSION - Ordinal – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::logRegOrd";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Frequency analyses – Proportion Test (2 Outcomes) ===========================================================================================================================================
        # https://www.jamovi.org/jmv/proptest2.html

        # NPAR TESTS BINOMIAL -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("BINOMIAL", crrSPS))) {
            stop(sprintf("NPAR TESTS - Binomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::propTest2";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # NPTESTS /ONESAMPLE BINOMIAL -----------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("ONESAMPLE", crrSPS)) & any(grepl("BINOMIAL", crrSPS))) {
            stop(sprintf("NPTESTS - Binomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::propTest2";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Frequency analyses – Proportion Test (N Outcomes) ===========================================================================================================================================
        # https://www.jamovi.org/jmv/proptestn.html

        # NPAR TESTS CHISQUARE ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("CHISQUARE", crrSPS))) {
            stop(sprintf("NPAR TESTS - Chi-squared – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::propTestN";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # NPTESTS /ONESAMPLE CHISQUARE ----------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("ONESAMPLE", crrSPS)) & any(grepl("CHISQUARE", crrSPS))) {
            stop(sprintf("NPTESTS - Binomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::propTest2";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Frequency analyses – Contingency Tables =====================================================================================================================================================
        # https://www.jamovi.org/jmv/conttables.html

        # CROSSTABS -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        else if (grepl("^CROSSTABS", crrSPS[1])) {
            stop(sprintf("CROSSTABS – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::contTables";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Frequency analyses – Paired Samples Contingency Tables ======================================================================================================================================
        # https://www.jamovi.org/jmv/conttablespaired.html

        # NPAR TESTS MCNEMAR --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("MCNEMAR", crrSPS))) {
            stop(sprintf("NPAR TESTS MCNEMAR – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::contTablesPaired";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # NPTESTS /RELATED MCNEMAR ----------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("RELATED", crrSPS)) & any(grepl("MCNEMAR", crrSPS))) {
            stop(sprintf("NPTESTS - Binomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::contTablesPaired";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Frequency analyses – Log-Linear Regression ==================================================================================================================================================
        # https://www.jamovi.org/jmv/loglinear.html

        # LOGLINEAR -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-loglinear
        else if (grepl("^LOGLINEAR", crrSPS[1])) {
            stop(sprintf("LOGLINEAR – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::logLinear";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Factor analysis – Reliability Analysis ======================================================================================================================================================
        # https://www.jamovi.org/jmv/reliability.html

        # RELIABILITY ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-reliability
        else if (grepl("^RELIABILITY", crrSPS[1])) {
            stop(sprintf("RELIABILITY – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::reliability";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Factor analysis – Principal Component Analysis ==============================================================================================================================================
        # https://www.jamovi.org/jmv/pca.html

        # FACTOR - /EXTRACTION = PC, PA1, DEFAULT -----------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-factor
        else if (grepl("^FACTOR", crrSPS[1])) {
            stop(sprintf("FACTOR - PCA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::pca";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Factor analysis – Exploratory Factor Analysis ===============================================================================================================================================
        # https://www.jamovi.org/jmv/efa.html

        # FACTOR - /EXTRACTION = PAF, PA2, ML, GLS / ULS ----------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-factor
        # PAF, PA2 → Principal axis 
        # ML → maximum likelihood 
        # CHECK: unclear whether "minimum residuals" = GLS / ULS
        else if (grepl("^FACTOR", crrSPS[1])) {
            stop(sprintf("FACTOR - EFA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc = "jmv::efa";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # Factor analysis – Confirmatory Factor Analysis ==============================================================================================================================================
        # https://www.jamovi.org/jmv/cfa.html
        
        # not contained in SPSS -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

        # =============================================================================================================================================================================================
        
        # other not (yet) implemented SPSS-commands - remember to include them in cmdCnv in globals.R -------------------------------------------------------------------------------------------------
        # SPSS COMMAND --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-...
        else if (grepl("^[COMMAND NAME]", crrSPS[1])) {
            stop(sprintf("[COMMAND NAME] - not implemented yet: l. %d - %s", i, vecSPS[i]));
#           crrFnc = "";
#           crrArg = updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:      
        }

        # add converted command / function the the syntax list ======================================================================================================================================== 
        if (crrFnc > "") {
#           vecJMV = c(vecJMV, gsub("^list\\(", paste0(crrFnc, "("), gsub("\\( ", "\\(", gsub("\\n\\s+", " ", jmvcore::sourcify(clnArg(crrArg, crrFnc))))));
            vecJMV = c(vecJMV, gsub("^list\\(", paste0(crrFnc, "("), gsub("\\( ", "\\(", gsub("\\n\\s+", " ",          sourcify(clnArg(crrArg, crrFnc))))));
#           for testing commands: eval(parse(text = gsub("^list\\(", paste0(crrFnc, "("), gsub("\\( ", "\\(", gsub("\\n\\s+", " ", sourcify(clnArg(crrArg, crrFnc)))))))
        }
        rm(list = ls(pattern="crr*"));
    }

    attr(data, "syntax") = vecJMV;
    data
}


# =====================================================================================================================================================================================================
# only temporary: jmvcore::sourcify() currently can't handle formulas
# =====================================================================================================================================================================================================

sourcify <- function(object, indent="") {

    if (is.null(object)) {
        return("NULL")
    }
    if (is.logical(object)) {
        if (length(object) == 0)
            return("logical()")
        if (length(object) == 1)
            return(paste0(object))

        source <- "c("
        sep <- ""

        for (item in object) {
            source <- paste0(source, sep, item)
            sep=", "
        }
        source <- paste0(source, ")")

        return(source)
    }
    if (is.numeric(object)) {
        if (length(object) == 0)
            return("numeric()")
        if (length(object) == 1)
            return(paste(object))

        source <- "c("
        sep <- ""

        for (item in object) {
            source <- paste0(source, sep, item)
            sep=", "
        }
        source <- paste0(source, ")")

        if (nchar(source) > 40)
            source <- gsub(", ", paste0(",\n    ", indent), source, fixed=TRUE)

        return(source)

    } else if (is.character(object)) {
        if (length(object) == 0)
            return("character()")
        if (length(object) == 1)
            return(paste0("\"", object, "\""))

        source <- "c("
        sep <- ""

        for (item in object) {
            source <- paste0(source, sep, "\"", item, "\"")
            sep=", "
        }
        source <- paste0(source, ")")

        if (nchar(source) > 40) {
            source <- gsub("c(\"", paste0("c(\n    ", indent, "\""), source, fixed=TRUE)
            source <- gsub("\", ", paste0("\",\n    ", indent), source, fixed=TRUE)
        }

        return(source)

    } else if (is.symbol(object) || is.language(object)) {
        if (any(nchar(format(object)) > 0)) {
            return(paste(trimws(format(object)), collapse=" "))
        }

    } else if (is.list(object) || is.environment(object)) {
        if (length(object) == 0)
            return("list()")

        indent <- paste0(indent, "    ")
        source <- paste0("list(\n", indent)
        sep <- ""

        nams <- names(object)
        if (is.null(nams)) {

            for (item in object) {
                source <- paste0(source, sep, sourcify(item, indent))
                sep <- paste0(",\n", indent)
            }

        } else {

            for (name in nams) {
                source <- paste0(source, sep, name, "=", sourcify(object[[name]], indent))
                sep <- paste0(",\n", indent)
            }
        }

        source <- paste0(source, ")")
        return(source)
    }

    ""
}

# =====================================================================================================================================================================================================
# end - sourcify
# =====================================================================================================================================================================================================

#======================================================================================================================================================================================================
# SUB-FUNCTIONS
#======================================================================================================================================================================================================

getSPS <- function(fleSPS = '') {
    if (! file.exists(fleSPS) && ! file.exists(file.path(getwd(), basename(fleSPS)))) { stop(sprintf("\"%s\" does not exist.", fleSPS)); }
    vecSPS = readLines(hdlSPS <- file(fleSPS, "r"), warn = FALSE); close(hdlSPS); rm("hdlSPS");
    vecSPS = clnSPS(vecSPS);
    vecSPS = vecSPS[! grepl("^$|^.$", vecSPS)];
    # check that all lines end with a '.' - possibly check for the command being in capitals too
    if (! all(grepl(grcSPS, vecSPS) & grepl('\\.$', vecSPS))) { stop(sprintf('\n\nThe syntax contains commands that could not be parsed or that don\'t end with a \".\":\n\n%s\n\n',
                                                                paste0(vecSPS[! (grepl(grcSPS, vecSPS) & grepl('\\.$', vecSPS))], collapse='\n'))); }
    vecSPS
}

getVar <- function(crrSPS = c()) {
    if (grepl(paste0("^ADD FILES|^BEGIN GPL|^CACHE|^CASESTOVARS|^COMPUTE|^CTABLES|^DATASET|^DELETE VARIABLES|^DO IF|^ELSE|^END GPL|^END IF|^EXECUTE$|^FILTER|^FORMATS|^GET\\s+|^GRAPH|^GGRAPH|^IF\\s+|",
                     "^LEAVE|^MATCH FILES|^NEW FILE|^PRESERVE|^RENAME VARIABLES|^RESTORE|^SAVE\\s+|^SET\\s+|^SORT CASES|^SPLIT FILE|^STAR JOIN|^TITLE|^USE ALL$|^VALUE LABELS|^VARIABLE LABELS|",
                     "^VARIABLE LEVEL"), crrSPS[1])) { return(""); }

    vecVar = "";
    # handle the most common cases (the keywords VARIABLES and TABLES) first
    if (any(grepl("VARIABLES", crrSPS))) {
        # TO DO: possibly remove parentheses around the variable list
        vecVar = strSpl(gsub("\\(.*?\\)", "", gsub("\\s*=\\s*", "", gsub("MISSING\\s*=\\s*[A-z]+\\s*", "", gsub("REPORTMISSING\\s*=\\s*NO\\s*", "", strSpl(crrSPS[grepl("VARIABLES", crrSPS)], "VARIABLES\\s?")[2])))), "\\s+");
        if (grepl("^T-TEST\\s+GROUPS=", crrSPS[1])) { vecVar = c(vecVar, "BY", getLst(crrSPS[1], c("^T-TEST\\s+GROUPS=", "\\(.*?\\)"))); }
    } else if (any(grepl("TABLES\\s*=", crrSPS))) {
        vecVar = strSpl(gsub("=", "", strSpl(crrSPS[grepl("TABLES", crrSPS)], "TABLES\\s?")[2]), "\\s+");
    # afterwards, go through a list with keywords to match and remove
    } else if (grepl("^REGRESSION", crrSPS[1])) {
        vecVar = c(getLst(crrSPS, "^DEPENDENT"), "BY", getLst(crrSPS, c("METHOD=", "STEPWISE", "FORWARD", "BACKWARD", "ENTER", "REMOVE", "TEST")));
    } else {
        for (begKey in list(c("COUNT", "^.*?=\\s*", "\\(.*?\\)$"), c("^GLM\\s+"), c("K-S\\(NORMAL\\)="), c("^MEANS", "TABLES="), c("M-W=", "\\(.*?\\)"),
                            c("^ONESAMPLE", "TEST\\s+\\(", "\\)"), c("ONEWAY"), c("RECODE", "\\(.*?\\)", "INTO.*?$"), c("^SUMMARIZE", "TABLES="),
                            c("^T-TEST\\s+PAIRS=", "\\(PAIRED\\)"), c("^UNIANOVA\\s+"), c("WILCOXON=", "\\(PAIRED\\)"))) {
            if (any(grepl(begKey[1], crrSPS))) { vecVar = getLst(crrSPS, begKey); break }
        }
    }
    if (any(grepl("^ALL$|^TO$", vecVar))) { stop("Encountered TO - not implemented yet."); vecVar = ifelse(any(grepl("^ALL$", vecVar)), allVar, ""); }
    vecVar = gsub("^WITH$", "WITH", gsub("^BY$", "BY", vecVar, ignore.case = TRUE), ignore.case = TRUE);

    # check and return the checked variables
    chkVar(vecVar, crrSPS, names(get("data", envir=parent.frame())))
}

cntVar <- function(crrVar = c()) {
    if (! any(grepl("^BY$|^WITH$", crrVar))) {
        c(length(crrVar), 0, 0)
    } else {
        numVar = diff(c(0, grep("^BY$|^WITH$", crrVar), length(crrVar) + 1)) - 1;
        if (any(grepl("^BY$", crrVar)) && ! any(grepl("^WITH$", crrVar))) {
            c(numVar[1], numVar[2], 0)
        } else if (any(grepl("^WITH$", crrVar)) && ! any(grepl("^BY$", crrVar))) {
            c(numVar[1], 0, numVar[2])
        } else if (grep("^BY$", crrVar) < grep("^WITH$", crrVar)) {
            c(numVar[1], numVar[2], numVar[3])
        } else if (grep("^BY$", crrVar) > grep("^WITH$", crrVar)) {
           c(numVar[1], numVar[3], numVar[2])
        }
    }
}

getLst <- function(crrSPS = c(), begKey = c(), splLst = TRUE) {
    vecLst = crrSPS[grepl(begKey[1], crrSPS)];
    for (j in 1:length(begKey)) { vecLst = trimws(gsub(begKey[j], "", vecLst)); }
    if (splLst) { strSpl(vecLst, "\\s+") } else { vecLst }
}

getArg <- function(crrFnc = "", addDta = TRUE) {
    if (addDta) {
        fltCnd = get("fltCnd", envir=parent.frame());
        updArg(eval(parse(text = paste0("formals(", crrFnc, ")"))), pairlist(data = as.symbol(paste0("data", ifelse(fltCnd != "", paste0("[" , fltCnd, ", ]"), "")))))
    } else {
        eval(parse(text = paste0("formals(", crrFnc, ")")))
    }
}

getRpM <- function(crrSPS = c(), inpVar = c()) {
    crrWSF = getLst(crrSPS, "WSFACTOR\\s*=");
    # we know that each WSF has a name followed by the levels (which has to be integer) and the contrast type (may be left out) and a possible fourth parameter (the reference category)
    posLvl = grep("\\d+", crrWSF);
    numLvl = as.integer(crrWSF[posLvl]);    
    # TO-DO: extract contrasts
    outRpM = list(vector(mode = "list", length = length(posLvl)), vector(mode = "list", length = prod(numLvl)), vector(mode = "list", length = length(posLvl)), numLvl);
    for (j in 1:length(posLvl)) { 
        outRpM[[1]][[j]] = list(label = crrWSF[posLvl[j] - 1], levels = sprintf(paste0(crrWSF[posLvl[j] - 1], "%d"), 1:numLvl[j]));
        if (j == 1) { cmbLvl = outRpM[[1]][[j]]$levels; } else { cmbLvl = sprintf("%s, %s", rep(cmbLvl, each=numLvl[j]), rep(outRpM[[1]][[j]]$levels, length(cmbLvl))); }
        if (grepl("^DEVIATION|^SIMPLE|^DIFFERENCE|^HELMERT|^REPEATED|^POLYNOMIAL", toupper(crrWSF[posLvl[j] + 1]))) { outRpM[[3]][[j]] = toupper(crrWSF[posLvl[j] + 1]); }
    }
    if (! all(duplicated(c(length(inpVar), length(cmbLvl), prod(numLvl)))[-1])) {
        stop("Mismatch between the number of possible combinations of factor levels and the number of within-subject variables.");
    }
    for (j in 1:prod(numLvl)) {
        outRpM[[2]][[j]] = list(measure = inpVar[j], cell = strSpl(cmbLvl[j], ","))
    }
    
    outRpM
}

splVar <- function(inpVar = c()) {
    grpKey = c("^BY$", "^WITH$");
    outVar = vector(mode = "list", length = length(grpKey) + 1);
    posBnW = grep(paste(grpKey, collapse="|"), inpVar);
    if (length(posBnW) > 0) {
        for (j in posBnW) {
            for (k in 1:length(grpKey)) {
                if (grepl(grpKey[k], inpVar[j])) { outVar[[k + 1]] = inpVar[(j + 1):min(c(posBnW[posBnW > j] - 1, length(inpVar)))]; }
            }
        }
        outVar[[1]] = inpVar[1:(min(posBnW) - 1)];
    } else {
        outVar[[1]] = inpVar;
    }
    outVar
}

clnArg <- function(crrArg = list(), crrFnc = "") {
   defArg = getArg(crrFnc, FALSE);
   for (argNme in names(defArg)) {
       if (! hasName(crrArg, argNme)) { stop(sprintf("The argument list doesn\'t contain an entry for \"%s\".", argNme)); }
       if (identical(crrArg[[argNme]], defArg[[argNme]])) {crrArg[[argNme]] <- NULL}
   }
   crrArg
}

updArg <- function(crrArg = list(), addArg = list()) {
    nmeArg = names(crrArg);
    for (addNme in names(addArg)) {
        # check whether the argument exists in crrArg
        if (! any(grepl(addNme, nmeArg))) { stop(sprintf("Argument \"%s\" is not a valid argumenent / parameter name.", addNme)); }
        # check whether the variable type of the argument to add and the argument in the parameter list is the same
        if (is.null(addArg[[addNme]]))    { stop(sprintf("The argument value for \"%s\" (to be added / updated) is currently set to NULL (which would cause that argument to be removed).", addNme)); }
        # issue a warning if the original argument and the argument to be updated are of different type, unless the original argument is NULL
        if (typeof(crrArg[[addNme]]) != typeof(addArg[[addNme]]) && ! is.null(crrArg[[addNme]])) {
            warning(sprintf("Variable type is not the same for the argument \"%s\" – %s [default] vs. %s [to be added].", addNme, typeof(crrArg[[addNme]]), typeof(addArg[[addNme]])));
        };
        crrArg[[addNme]] <- addArg[[addNme]];
    }
    crrArg
}

fmtFrm <- function(inpVar = c(), outFmt = "") {
    if        (grepl("^formula$", outFmt)) {
        # covers both BY (factors) and WITH (covariates)
        # the occurrence that comes first is replaced with the ~ symbol (marking the transition from dependent [before] to independent variables [after])
        # afterwards, all further occurences are replaced with ' + ', indicating a list containing either factors or covariates
        as.symbol(gsub(' \\+ BY \\+ | \\+ WITH \\+ ' , ' + ', sub(' \\+ BY \\+ | \\+ WITH \\+ ', ' ~ ', paste(inpVar, collapse=' + '))))
    } else if (grepl("^terms$",   outFmt)) {
        str2lang(paste0(" ~ ", paste(gsub("\\*", ":", inpVar), collapse = " + ")))
    } else {
        stop(sprintf("fmtFrm: Not implemented for \"%s\" (or parameter outFmt not given).", outFmt));
    }
}

fmtVar <- function(inpVar = c(), asLng = FALSE, asPrs = FALSE) {
    if        (asPrs) {
        posWth = grep("^WITH$", inpVar) - 1;
        if (length(posWth) != 1 || (length(inpVar) != posWth * 2 + 1)) { stop("Variables do not match up as pairs."); }
        outVar = vector(mode = "list", length = posWth); 
        for (j in 1:posWth) { outVar[[j]] = list(i1 = inpVar[j], i2 = inpVar[j +  posWth + 1]); }
        as.symbol(jmvcore::sourcify(outVar))
    } else if (asLng) {
        str2lang( paste0(ifelse(length(inpVar) > 1, "vars(", ""), paste(inpVar, collapse=", "), ifelse(length(inpVar) > 1, ")", "")))   
    } else {
        as.symbol(paste0(ifelse(length(inpVar) > 1, "vars(", ""), paste(inpVar, collapse=", "), ifelse(length(inpVar) > 1, ")", "")))
    }
}

chkVar <- function(vecVar = c(), crrSPS = c(), allVar = "") {
# the function checks, and - if necessary - automatically fixes (concatenate variables that were
# separated by a line feed) a vector of variables

    selVar = which(! grepl("^BY$|^WITH$", vecVar));
    for (j in 1:length(selVar)) {
        if (! any(grepl(paste0("^", vecVar[selVar[j]], "$"), allVar, ignore.case = TRUE))) {
            if (j < length(selVar) && any(grepl(paste0("^", vecVar[selVar[j + 0]], vecVar[selVar[j + 1]], "$"), allVar, ignore.case = TRUE))) {
                vecVar[selVar[j + 1]] = paste0(vecVar[selVar[j + 0]], vecVar[selVar[j + 1]]); vecVar[selVar[j + 0]] = "";
                next
            }
            if (j > 1              && any(grepl(paste0("^", vecVar[selVar[j - 1]], vecVar[selVar[j + 0]], "$"), allVar, ignore.case = TRUE))) {
                vecVar[selVar[j + 0]] = paste0(vecVar[selVar[j - 1]], vecVar[selVar[j + 0]]); vecVar[selVar[j - 1]] = "";
                next
            }
            if (grepl("^ZRE_\\d+$", vecVar[selVar[j]])) {
                warning(sprintf("Variable \"%s\" was generated as an output of an earlier analysis and not contained in the data.", vecVar[selVar[j]]));
                vecVar[selVar[j]] = "";
                next
            }
            stop(sprintf("Variable \"%s\" not contained in the dataset.\nSPSS-command: %s\n\n", vecVar[selVar[j]], paste(crrSPS, collapse = " /")));
        }
        # replace the variable name with the variable name from the column header if it is not a perfect match (i.e., if lower- / uppercase don't match up)
        if (! any(grepl(paste0("^", vecVar[selVar[j]], "$"), allVar))) {
            vecVar[selVar[j]] = allVar[grepl(paste0("^", vecVar[selVar[j]], "$"), allVar, ignore.case = TRUE)];
        }
    }

    vecVar[vecVar != ""]
}

chkFlt <- function(fltCnd = "", data = data.frame()) {
    # the comparison operator in SPSS ios "=" vs. "==" in R
    fltCnd = gsub("\\s*=\\s*", " == ", fltCnd);
    fltVar = trimws(gsub("^[[|]]]$", "", gsub("\\$", "", gsub("data", "", unlist(lapply(strsplit(strSpl(fltCnd, '&|\\|'), "=|>|<"), function(x) x[1]))))));
    for (j in 1:length(fltVar)) {
        if (any(grepl(fltVar[j], names(data), ignore.case = TRUE))) {
            # SPSS is agnostic to variable names and accepts even those misspelled (i.e., different small / capital letters)
            rplVar = names(data)[grepl(fltVar[j], names(data), ignore.case = TRUE)];
            fltCnd = gsub(fltVar[j], rplVar, fltCnd);
            fltVar[j] = rplVar;
            rm('rplVar');
            # check for whether variable is followed by a "="
            if (!grepl(paste0(fltVar[j], "\\s*", "=|<|>"), fltCnd)) {
                if (is.factor(data[[fltVar[j]]])) {
                    # if the variable is a factor, SPSS permits 0 as the smallest value whereas R starts counting with 0
                    # therefore the value of the lowest level is chosen (which equates to the level which is "0" in SPSS)
                    fltCnd = gsub(fltVar[j], paste0(fltVar[j], " != '", levels(data[[fltVar[j]]])[1], "'"), fltCnd);
                } else {
#                   implement: "data[selPrt > 0, ]"
                    stop(sprintf("FILTER – not implemented yet for non-factor-variables - %s", fltVar[j]));
                }
            }
        } else {
            stop(sprintf("FILTER – variable with the name \"%s\" not found in the data set.", fltVar[j]));
        }
    }

            
    # return the filter condition with all fltVar preceeded by data$
    pcdDta(fltCnd, fltVar)
}

argCI  <- function(crrSPS = c()) {
# SPSS likes inconsistent: with EXAMINE it is CINTERVAL and a percent value (multiply by 1), with T-TEST it is CRITERIA=CI and a decimal value (0 - 1; multiply by 100),
#                          with NPTEST it is CRITERIA ... CILEVEL
# the c()-vector is a safety measure: if no value for the CI is given (empty string), as.double results in NA, which is the omitted (moving the 95 which is the second element in the vector
#                                     to the first place, extracted with [1]

    pairlist(ci = TRUE, ciWidth = na.omit(c(as.single(strSpl(gsub("^CINTERVAL|^CRITERIA=CI\\(|\\)|^CRITERIA.*?CILEVEL\\s*=|CI.*?CILEVEL\\s*=", "", 
                                                             crrSPS[grepl("^CINTERVAL|^CRITERIA|^CI\\s+", crrSPS)]), "\\s+")[1])
                                            * ifelse(any(grepl("^CRITERIA=CI", crrSPS)), 100, 1), 95))[1])
}

argCon <- function(crrSPS = c()) {
    lneCon = grep("^CONTRAST", crrSPS);
    lstCon = vector(mode = "list", length = length(lneCon));
    for (j in 1:length(lneCon)) {
        splCon = strSpl(gsub("^CONTRAST\\(", "", crrSPS[[lneCon[j]]]), "\\)\\s*=");
        lstCon[[j]] = pairlist(var = splCon[1], type = gsub("special", "deviation", tolower(strSpl(splCon[2], "\\(")[1])));
    }
    
    pairlist(contrasts = lstCon)
}

argEmm <- function(crrSPS = c()) {
    lstEmm = pairlist(emMeans      = fmtFrm(strSpl(strSpl(crrSPS[grepl("^PLOT\\s*=", crrSPS)], "\\(|\\)")[2], "\\s+"), "terms"),
                      emmPlots     = TRUE,
                      emmPlotError = tolower(gsub("NO", "NONE", strSpl(strSpl(crrSPS[grepl("^PLOT\\s*=", crrSPS)], "ERRORBAR=")[2], "\\(|\\s+")[1])),
                      emmTables    = any(grepl("^EMMEANS\\s*=\\s*TABLES", crrSPS)));
    # two further arguments that are not implemented: emmWeights and ciWidthEmm
    
    lstEmm
}

argMsV <- function(crrSPS = c()) {
    # in some cases (e.g., FREQUENCIES) there is INCLUDE (which enforces SPSS to include the invalid cases in the calculation of the valid and cumulative percentages [also for charts and histograms])
    pairlist(miss = ifelse(grepl("=LISTWISE", crrSPS[grepl("^MISSING", crrSPS)]), "listwise", "perAnalysis"))
}

argPHT <- function(crrSPS = c(), crrFnc = "") {
    # those post-hoc tests are not implemented in jamovi anyway
    excPHT = c("snk", "btukey", "duncan", "dunnett", "dunnettl", "dunnettr", "lsd", "sidak", "gt2", "gabriel", "fregw", "qregw", "t2", "t3", "c", "waller");

    if (grepl('^jmv::anovaRM$|jmv::ANOVA', crrFnc)) {
        excPHT = c(excPHT, "gamesHowell");             defPHT = 'tukey';
    } else if (grepl('^jmv::anovaOneW$',          crrFnc)) {
        excPHT = c(excPHT, "scheffe", "bonf", "holm"); defPHT = 'none';
    } else { 
        stop(sprintf("argPHT: Not implemented for \"%s\" (or parameter crrFnc not given).", crrFnc));
    }

    lnePHT = getLst(crrSPS, "^POSTHOC\\s*=", FALSE);
    effPHT = strSpl(lnePHT, "\\(")[1];
    tstPHT = gsub(paste0("^", paste(excPHT, collapse = "$|^"), "$"), "",
             strSpl(gsub("gh", "gamesHowell", gsub("bonferroni", "bonf holm", tolower(gsub("\\s*\\(|\\)$|\\(\\S+\\)", "", gsub(effPHT, "", lnePHT))))), "\\s+"))

    lstPHT = pairlist(postHoc      = as.list(strSpl(effPHT, "\\s+")),
                      postHocCorr  = as.list(unique(c(defPHT, tstPHT[tstPHT != ""]))));

    lstPHT
}

argSS  <- function(crrSPS = c(), crrFnc = "") {
    strSS = getLst(crrSPS, c("^METHOD\\s*=", "SSTYPE\\(", "\\)"))

    # SS type 4 is currently not implemented in any of the jmv-ANOVAs, anovaRM doesn't support "1"
    ifelse(is.null(strSS) || strSS == "4" || (strSS == "1" && grepl("^jmv::anovaRM$", crrFnc)), "3", strSS)
}

argTls <- function(crrSPS = c()) {
    # if PRECENTILES or NTILES are requested, they to be switched on (set to TRUE): pc vs pcEqGr, the second argument is then either pcValues (for percentiles) or pcNEqGr (for ntiles)
    # SPSS likes inconsistent: the format of PERCENTILES in EXAMINE and FREQUENCIES is different: /PERCENTILES(10,50,90)=EMPIRICAL vs /PERCENTILES=value thus the many sub()-functions
    c(ifelse(grepl("^PERCENTILES", crrSPS), pairlist(pc     = TRUE, pcValues = sub(" ", ",", sub("PERCENTILES\\s?=", "", sub("\\).*", "", sub(".*\\(", "", crrSPS[grepl("^PERCENTILES", crrSPS)]))))),
                                            pairlist(pcEqGr = TRUE, pcNEqGr  = as.integer(sub("NTILES\\s?=", "",                                           crrSPS[grepl("^NTILES",      crrSPS)])))))
}

cmpFrm <- function(crrSPS = c(), data = data.frame()) {
    # initialize calculation commands
    clcVld = FALSE;
    clcJMV = strSpl(gsub("^COMPUTE", "", crrSPS), "=")[2];

    # extract and check variables
    tgtVar = strSpl(gsub("^COMPUTE", "", crrSPS), "=")[1];
    clcVar = strSpl(gsub("\\)$", "", gsub("^[A-z]+\\(", "", strSpl(clcJMV, "\\+|-|\\*|/|&|\\||!"))), "\\s*,\\s*");
    clcVar = chkVar(clcVar, crrSPS, names(data));

    # not any function used, just arithmetic, relational or boolean operators
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-arithmetic-operations
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-relational-operators
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-logical-operators
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-not-logical-operator
    if (!grepl("\\(|\\)", clcJMV)) {
        # add handling for possible transformations - EQ, etc. possibly AND, OR, NOT
        # clcJMV = gsub("", "", clcJMV);
        # as long as no functions are used, clcR is just clcJMV with the variables preceded by "data$"
        clcR   = pcdDta(clcJMV, clcVar);
        clcVld = TRUE;
    }
    # ABS(number): Returns the absolute value of a number.
    # BETA(alpha, beta): Draws samples from a Beta distribution.
    # BOXCOX(variable, lambda): Returns a Box Cox transformation of the variable.
    # CONTAINS(item1, item2, item3, …, in1, in2, in3, …): Determines if any of the items appear in in1, in2, in3, ....
    #                                                     Note that most of these arguments are optional -- it is possible to simply use CONTAINS(needle, haystack).
    # EXP(number): Returns the exponent for basis \u212F of a number.
    # FILTER(variable, filter expression): Filters a variable using the filter expression.
    # GAMMA(shape, scale): Draws samples from a Gamma distribution.
    # HLOOKUP(index, value 1, value 2, …): The value in the provided values at index.
    # IF(expression, value, else): If the expression resolves true, use the value, otherwise the else.
    # IFMISS(variable, value, else): When the variable contains a missing value, use the value, otherwise the else.
    # INT(number): Converts a number to an integer.
    # IQR(variable): Returns a whether the variable is an outlier according to the IQR: If the value is within the box of a Boxplot 0 is returned, absolute values larger than 1.5 are outside the whiskers.
    # LN(number): Returns the natural logarithm of a number.
    # LOG10(number): Returns the base-10 logarithm of a number.
    # MATCH(value, value 1, value 2, …): The index of value in the provided values.
    # MAX(variable): Returns the largest value of a set of numbers.
    # MAXABSIQR( variable 1, variable 2, … ): Max. absolute IQR-value (i.e., how far outside the box an individual datapoint is in terms of IQR - Q1/Q3-distance)
    # MAXABSZ(variable 1, variable 2, …, group_by=0): Max. absolute z-value / normalized value.
    # MEAN(number 1, number 2, …, ignore_missing=0, min_valid=0): Returns the mean of a set of numbers.
    if (grepl("^MEAN\\(", clcJMV)) {
        clcR = encQtn(gsub("\\)$", ")])", gsub("^MEAN\\(", "rowMeans(data[, c(", clcJMV)), clcVar);
        clcVld = TRUE;
    }    
    # MIN(variable): Returns the smallest value of a set of numbers.
    # NORM(mean, sd): Draws samples from a normal (Gaussian) distribution.
    # NOT(value): Inverts the value.
    # OFFSET(variable, integer): Offsets the values up or down.
    # RANK(variable): Ranks each value
    # ROUND(variable, digits=0): Rounds each value
    # ROW(NO ARGUMENTS): Returns the row numbers.
    # SAMPLE(variable, n, otherwise=NA): Draws a sample of n from the variable. i.e. SAMPLE(var, 20), i.e. SAMPLE(1, 20), i.e. SAMPLE(\'training\', 20, \'test\')
    # SCALE(variable, group_by=0): Returns the normalized values of a set of numbers.
    # SPLIT(variable, sep=',', piece): Splits text into pieces based on a separator. piece specifies the desired piece by index.
    # SQRT(number): Returns the square root of a number.
    # STDEV(number 1, number 2, …, ignore_missing=0): Returns the standard deviation of a set of numbers.
    # SUM(number 1, number 2, …, ignore_missing=0, min_valid=0): Returns the sum of a set of numbers.
    # TEXT(number): Converts the value to text.
    # UNIF(low, high): Draws samples from a uniform distribution.
    # VALUE(text): Converts text to a number (if possible).
    # VAR(number 1, number 2, …, ignore_missing=0): Returns the variance of a set of numbers.
    # VMAX(variable, group_by=0): Returns the largest value of a variable.
    # VMEAN(variable, group_by=0): Returns the overall mean of a variable.
    # VMED(variable, group_by=0): Returns the median of a variable.
    # VMIN(variable, group_by=0): Returns the smallest value of a variable.
    # VMODE(variable, group_by=0): Returns the most common value in a variable.
    # VN(variable, group_by=0): Returns the number of cases in a variable.
    # VROWS(variable, group_by=0): Returns the number of rows of a variable.
    # VSE(variable, group_by=0): Returns the standard error of the mean of a variable.
    # VSTDEV(variable, group_by=0): Returns the standard deviation of a variable.
    # VSUM(variable, group_by=0): Returns the overall sum of a variable.
    # VVAR(variable, group_by=0): Returns the variance of a variable.
    # Z(variable, group_by=0): Returns the normalized values of a set of numbers.

    if (clcVld) {
        # add a data column (with the name of tgtVar)
        data[[tgtVar]] = eval(parse(text = clcR));
        # assign attributes to mark it as "Computed variable" in jamovi
        attr(data[[tgtVar]], "columnType")     = "Computed";
        attr(data[[tgtVar]], "formula")        = clcJMV;
        attr(data[[tgtVar]], "formulaMessage") = "";
    }
    rm("clcVld", "clcJMV", "clcR", "clcVar", "tgtVar");
    
    data
}

strSpl <- function(inpStr = "", splDlm = "") {
    trimws(unlist(strsplit(trimws(inpStr), splDlm)))
}

pcdDta <- function(strCmd, vecVar) {
    for (j in 1:length(vecVar)) {
        if (!grepl(paste0("data$", vecVar[j]), strCmd)) {
            strCmd = gsub(vecVar[j], paste0("data$", vecVar[j]), strCmd);
        }
    }
    
    strCmd
}

encQtn <- function(strCmd, vecVar) {
    for (j in 1:length(vecVar)) {
        strCmd = gsub(vecVar[j], paste0("\"", vecVar[j], "\""), strCmd)
    }

    strCmd
}

# =====================================================================================================================================================================================================
