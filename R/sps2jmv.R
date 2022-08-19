#' Converts SPPS-syntax either from a SPSS-syntax file .sps or read from a SPSS-output-file .spv (using spv2sps) into jamovi / jmv-syntax
#'
#' The function expects that either the parameter vecSPS is used (with the output from spv2sps) or that both fleSPS and fleSAV are given.
#'
#' @param vecSPS  vector with SPSS-commands, the format is supposed to be the same as what is output from spv2sps: a character vector containing one command per element,
#'                and the attribute "datafile" that contains a string pointing to the location of the SPSS-data-file (incl. path; if not found at the path or if no path
#'                is given, the file is searched in the current directory)
#' @param fleSPS  containing the location of a SPSS-syntax-file (.sps; incl. path)
#' @param fleSAV  containing the location of a SPSS-data-file (.sav; incl. path)
#' @param runMsV  run analyses even though there are variables missing; if the value is set to FALSE (default), the whole analysis is preceded by "#" which would cause it not
#'                to be evaluated with "eval(parse(text = ...))"; if the value is set to TRUE, only the variable that is not found in the data set is excluded and the analyses
#'                are conducted with the remaining variables (which will lead to results that are different from those originally conducted in SPSS)
#'
#' @return list with jamovi / jmv-analysis-function-calls and the data set stored in the attribute "dataset"
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' }
#'
#' @export sps2jmv
#'
sps2jmv <- function(vecSPS = c(), fleSPS = "", fleSAV = "", runMsV = FALSE) {
    # reset filter conditions
    fltAnl <- fltMod <- "";

    # check the input parameter, at least either vecSPS has to be given (and to contain the attribute "datafile" with the location of an SPSS-SAV-file)
    # or a combination of fleSPS and fleSAV has to be given (and both files have to exist)
    # if fleSAV is given together with vecSPS than, fleSAV overrides the file given in the attribute "datafile"
    if (length(vecSPS) > 0) {
        # if fleSAV was given as parameter
        if (fleSAV == "" && ! is.null(attr(vecSPS, "datafile"))) {
            fleSAV <- attr(vecSPS, "datafile");
        }
    } else if (fleSPS > "") {
        # check whether fleSPS exists, load it and do some plausibility checks
        vecSPS <- getSPS(fleSPS)
    }
    if (fleSAV > "") {
        data <- read_all(fleSAV)
        # store the original data type, later used for being compared with
        clsVar <- unlist(lapply(names(data), function(x) class(data[[x]])));
        names(clsVar) <- names(data);
    } else {
        stop(paste("sps2jmv requires an SPSS-data-file. This can be either provided by vecSPS containing an attribute \"datafile\" or via the parameter fleSAV.",
                   "This should be a character vector containing the position of the SPSS-data-file (.sav; incl. path)."));
    }
    if (length(vecSPS) == 0) {
        stop(paste("sps2jmv requires either a list (vecSPS) containing one SPSS-command (each as character vector) per list entry,",
                   "or a parameter (fleSPS) pointing to a SPSS-syntax-file (.sps; incl. path)."));
    }

    vecJMV <- c();

    for (i in seq_along(vecSPS)) {

        # General: Split, handle generic attributes (e.g., VAR1 TO VARx) ==============================================================================================================================
        if (grepl(grpAnl, vecSPS[i])) {
            crrSPS <- strSpl(gsub("\\.$", "", vecSPS[i]), " /");
            crrVar <- getVar(crrSPS, data);
            # assess whether there are missing variables and handle them accordingly
            # 0 - no missing variables; 1 - missing variables, but run analyses with those variables excluded; 2 - missing variables, do not run analyses including such variables
            crrMsV <- chkMsV(crrVar, runMsV);
            # running analysis with missing variables excluded requires removing those variables, otherwise (2) the whole analysis is commented out
            if (crrMsV == 1) crrVar <- lapply(crrVar, function(x) x[!grepl("^#", x)]);
            if (crrMsV == 2) crrVar <- lapply(crrVar, function(x) if (!is.null(x)) gsub("^#", "", x));
            # number of variables in each category (dep., ind. - categ., indep. - cont.)
            crrNmV <- unlist(lapply(crrVar, length));
            # check whether at least one of the variable categories is available; otherwise throw an error
            if (all(crrNmV == 0)) stop("Handling missing variables has changed - crrNmV == 0 needs implementation.")
        } else if (grepl(grpMod, vecSPS[i])) {
            # for those commands, crrSPS, crrVar, etc. do not need to be extracted; those commands are one-liners where vecSPS[i] can directly be used
            # however, crrMsV is used by getCmt when assembling commands at the end and needs to be set; NB: if variable can't be found for COMPUTE or RECODE, an error is thrown
            crrMsV <- 0;
        } else if (grepl(paste0(grpDta, "|^DATASET\\s+|^PRESERVE\\.$|^RESTORE.$|^CACHE\\.$|^SET\\s+DECIMAL"), vecSPS[i])) {
            vecJMV <- c(vecJMV, "", sprintf("# SPSS: %s", vecSPS[i]),
                               "# This SPSS-command is used to handle datasets and data files in SPSS and therefore not implemented.", "");
            next
        } else if (grepl("^EXECUTE\\.$", vecSPS[i])) {
            vecJMV <- c(vecJMV, "", sprintf("# SPSS: %s", vecSPS[i]),
                               "# This SPSS-command is used to execute previous COMPUTE, FILTER, etc.-commands and therefore not implemented.", "");
            next
        } else if (grepl("^TITLE\\s+", vecSPS[i])) {
            vecJMV <- c(vecJMV, "", sprintf("# SPSS: %s", vecSPS[i]), "");
            next
        } else if (grepl("^SPLIT\\s+FILE\\s+.*?BY", vecSPS[i])) {
            vecJMV <- c(vecJMV, "", paste("#", strrep("=", 100)), sprintf("# SPSS: %s", vecSPS[i]),
                               "# The SPLIT-command produces two (or more) separate analyses for each step of this variable, the following analyses DON\'T honour that split.",
                               "# Results will be different from those in SPSS.", paste("#", strrep("=", 100)), "");
            next
        } else if (grepl("^SPLIT\\s+FILE\\s+OFF.", vecSPS[i])) {
            vecJMV <- c(vecJMV, "", paste("#", strrep("=", 100)), sprintf("# SPSS: %s", vecSPS[i]),
                               "# The SPLIT-command produces two (or more) separate analyses for each step of this variable, the analyses above (up to the other SPLIT-comment) DON\'T honour that split.",
                               paste("#", strrep("=", 100)), "");
            next
        } else if (grepl("^FORMATS", vecSPS[i])) {
            vecJMV <- c(vecJMV, "", sprintf("# %s", vecSPS[i]),
                               "# This SPSS-command is used to define the format of variable columns in SPSS and is therefore not implemented.", "");
            next
        } else if (grepl("^COMPUTE", vecSPS[i])) {
            # proceed further down
        } else {
            warning(sprintf("SPSS-command in l. %d - \"%s\" not (yet) implemented.", i, vecSPS[i]));
            next
        }
        # special case: UNIANOVA with the subcommand RANDOM (applying a repeated-measures-type analysis to a long dataset) has no equivalent in jmv
        if (grepl("^UNIANOVA.*?RANDOM\\s*=", vecSPS[i])) {
            stop("UNIANOVA with RANDOM: ", vecSPS[i]);
            vecJMV <- c(vecJMV, "", sprintf("# SPSS: %s", vecSPS[i]),
                               "# The RANDOM-subcommand of the UNIANOVA permits to conduct an analysis that is better set up using a \"wide\" data set and an ANOVA for repeated measurements.", "");
            next
        }

        # Modifications to the dataset / the variables (incl. filtering and removing the filter) ======================================================================================================

        # ADD VALUE LABELS ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-add-value-labels
        # (adds new value labels but keeps existing ones, see also VALUE LABELS)
        if      (grepl("^ADD VALUE LABELS", vecSPS[i])) {
            stop(sprintf("ADD VALUE LABELS – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # ALTER TYPE ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-alter-type
        else if (grepl("^ALTER TYPE", vecSPS[i])) {
            stop(sprintf("ALTER TYPE – not implemented yet: l. %d - %s", i, vecSPS[i]));

        }

        # CASETOVARS ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-castovars
        else if (grepl("^CASETOVARS", vecSPS[i])) {
            stop(sprintf("CASETOVARS – not implemented yet: l. %d - %s", i, vecSPS[i]));
            # CASESTOVARS /ID=SubjCode /INDEX=blkNmb emoCls sngNmb /GROUPBY=VARIABLE.
        }

        # COMPUTE -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-compute
        # https://github.com/jamovi/jamovi/blob/current-dev/client/main/vareditor/formulatoolbar.js#L48 (which functions are implemented in jamovi)
        else if (grepl("^COMPUTE", vecSPS[i])) {
            if (grepl("filter_\\$", vecSPS[i])) {
                fltAnl <- gsub("^\\(|\\)$", "", strSpl(vecSPS[i], "filter_\\$\\s*=")[2]);
            } else {
                data <- clcCmp(vecSPS[i], data, fltMod);
            }
        }

        # DELETE VARIABLES ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-compute
        else if (grepl("^DELETE VARIABLES", vecSPS[i])) {
            if (grepl("filter_\\$", vecSPS[i])) {
                fltAnl <- "";
            } else {
                stop(sprintf("DELETE VARS – not implemented yet: l. %d - %s", i, vecSPS[i]));
                data <- subset(data, select = -get(trimws(gsub("DELETE VARIABLES", "", vecSPS[i]))));
            }
        }

        # FILTER --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-filter
        else if (grepl("^FILTER", vecSPS[i])) {
            if (grepl("^FILTER\\s+OFF", vecSPS[i])) {
                fltAnl <- "";
            } else if (!grepl("filter_\\S", vecSPS[i])) {
                fltAnl <- fixVar(gsub("\\.$", "", gsub("FILTER\\s+BY\\s+", "", vecSPS[i])), vecSPS[i], names(data));
            } else if (fltAnl == "" && hasName(data, "filter_.")) {
                fltAnl <- "filter_.";
            } else if (fltAnl != "") {
                # use the temporary filter condition - typically defined by a COMPUTE-command
            } else {
                stop(sprintf("Filter condition not (yet) implemented: %s", vecSPS[i]));
            }
            fltAnl <- chkFlt(fltAnl, data);
        }

        # IF (DO IF, ELSE IF, END IF; IF) -------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-do-if
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=di-else-if-command-do-if-command
        else if (grepl("^DO\\s+IF\\s+|^DO\\s+IF\\s+", vecSPS[[i]])) {
            fltMod <- gsub("^DO\\s+IF\\s+|^ELSE\\s+IF\\s+", "", gsub("\\.$", "", vecSPS[i]));
        }
        else if (grepl("^END IF.$", vecSPS[i])) {
            fltMod <- "";
        }
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=if-else-command-do-command
        else if (grepl("^ELSE$", vecSPS[i])) {
            stop(sprintf("Can't handle ELSE-conditions (since that would require to store the conditions of all previous IF-clauses) - : l. %d - %s", i, vecSPS[i]));
        }
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-if
        else if (grepl("IF\\s+\\(.*?\\.$", vecSPS[i])) {
            # do calculation after IF, using the condition in IF as a filter
            data <- clcCmp(gsub("\\)\\s+,", "),", gsub("\\)\\s+\\.", ").", paste0(strSpl(vecSPS[i], "\\)")[-1], collapse = ") "))), data, strSpl(vecSPS[i], "\\(|\\)")[2]);
        }

        # NUMERIC -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-numeric
        else if (grepl("^NUMERIC", vecSPS[i])) {
            stop(sprintf("NUMERIC – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # RANK ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-rank
        else if (grepl("^RANK", vecSPS[i])) {
            stop(sprintf("RANK – not implemented yet: l. %d - %s", i, vecSPS[i]));
            # COMPUTED → RANK(VARNAME)
            # doesn't work yet for NTILES-subcommand
        }

        # RECODE --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-recode
        else if (grepl("^RECODE", vecSPS[i])) {
            data <- clcRcd(gsub("\\.$", "", vecSPS[i]), data, fltMod);
        }

        # RENAME VARIABLES ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-rename-variables
        else if (grepl("^RENAME VARIABLES", vecSPS[i])) {
            stop(sprintf("RENAME VARIABLES – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # SAMPLE --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sample
        else if (grepl("^SAMPLE", vecSPS[i])) {
            stop(sprintf("SAMPLE – not implemented yet: l. %d - %s", i, vecSPS[i]));
            # COMPUTED → SAMPLE(1, N FROM M / DECIMAL VALUE, 0) + FILTER
            # double-check whether the sample changes, otherwise use a fixed assignment based on R-sample()
        }

        # SORT CASES ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sort-cases
        else if (grepl("^SORT CASES", vecSPS[i])) {
            crrVar <- fixVar(trimws(gsub("\\(A\\)|\\(D\\)", "", gsub("SORT\\s+CASES\\s+BY", "", gsub("\\.$", "", vecSPS[i])))), vecSPS[i], names(data));
            if (chkMsV(crrVar, runMsV) < 2)
#              crrVar =
               data <- data[order(data[[crrVar]], decreasing = grepl("\\(D\\)", vecSPS[i])), ];
        }

        # SORT VARIABLES ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sort-variables
        else if (grepl("^SORT VARIABLES", vecSPS[i])) {
            stop(sprintf("SORT VARIABLES – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # STRING --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-string
        else if (grepl("^STRING", vecSPS[i])) {
            stop(sprintf("STRING – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # USE -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-use
        else if (grepl("^USE", vecSPS[i])) {
            if (grepl("^USE ALL$", vecSPS[i])) {
                fltAnl <- "";
            } else {
                vecJMV <- c(vecJMV, sprintf("# %s – This SPSS-command is used to designate a time range of observations (to be used with time series procedures) and therefore not implemented.", vecSPS[i]));
            }
        }

        # VALUE LABELS --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-value-labels
        else if (grepl("^VALUE LABELS", vecSPS[i])) {
            if (!grepl("filter_\\$", vecSPS[i])) {
                stop(sprintf("VALUE LABELS – not implemented yet: l. %d - %s", i, vecSPS[i]));
            }
        }

        # VARIABLE LABELS -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-variable-labels
        else if (grepl("^VARIABLE LABELS", vecSPS[i])) {
            if (!grepl("filter_\\$", vecSPS[i])) {
                stop(sprintf("VARIABLE LABELS – not implemented yet: l. %d - %s", i, vecSPS[i]));
                # 90 -  7: "VARIABLE LEVEL neovertr_mean_qrt (ORDINAL)."
                # 90 - 20: "VARIABLE LEVEL Eg_SJ_qrt_MS (ORDINAL)."
            }
        }

        # VARIABLE LEVEL ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-variable-level
        else if (grepl("^VARIABLE LEVEL", vecSPS[i])) {
            stop(sprintf("VARIABLE LEVEL – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }

        # VARSTOCASES ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-varstocases
        else if (grepl("^VARSTOCASES", vecSPS[i])) {
            stop(sprintf("VARSTOCASES – not implemented yet: l. %d - %s", i, vecSPS[i]));
            # CASESTOVARS /ID=SubjCode /INDEX=blkNmb emoCls sngNmb /GROUPBY=VARIABLE.
        }


        # Comments ====================================================================================================================================================================================

        # COMMENT -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-comment
        else if (grepl("^COMMENT", vecSPS[i])) {
            stop(sprintf("COMMENT – not implemented yet: l. %d - %s", i, vecSPS[i]));
        }


        # Exploration – Descriptives ==================================================================================================================================================================
        # https://www.jamovi.org/jmv/descriptives.html

        # DESCRIPTIVES --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-descriptives
        else if (grepl("^DESCRIPTIVES", crrSPS[1])) {
            print("DESCRIPTIVES");
            crrFnc <- "jmv::descriptives";
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      splitBy      = fmtVar(crrVar[[2]]),
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
            crrFnc <- "jmv::descriptives";
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      splitBy      = fmtVar(crrVar[[2]]),
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
#           if (any(grepl("^CINTERVAL", crrSPS)))      crrArg <- updArg(crrArg, argCI(crrSPS));
            if (any(grepl("^PERCENTILES\\(", crrSPS))) crrArg <- updArg(crrArg, argTls(crrSPS));
            # not implemented in jmv: COMPARE (not necessary), TOTAL, ID, MESTIMATOR, MISSING, STATISTICS subcommand: EXTREMES
        }

        # FREQUENCIES ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-frequencies
        else if (grepl("^FREQUENCIES", crrSPS[1])) {
            print("FREQUENCIES");
            crrFnc <- "jmv::descriptives";
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      splitBy      = fmtVar(crrVar[[2]]),
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
            if (any(grepl("^NTILES|^PERCENTILES", crrSPS))) crrArg <- updArg(crrArg, argTls);
            # not implemented in jmv: FORMAT, MISSING, PIECHART, GROUPED, ORDER;
            #                         BARCHART and HISTOGRAM subcommand: MINIMUM, MAXIMUM, FREQ (define the minima and maxima of the values [x-axis] and maximum for frequencies [Y-axis]),
            #                                                            PERCENT (BARCHART: maximum percentage [y-axis], NORMAL (HISTOGRAM: replaced by dens)
        }

        # GRAPH - BAR and HISTOGRAM -------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-graph
        else if (grepl("^GRAPH", crrSPS[1]) && any(grepl("^BAR|^HISTOGRAM", crrSPS))) {
            print("GRAPH - BAR or HISTOGRAM");
            crrFnc <- "jmv::descriptives";
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      splitBy      = fmtVar(crrVar[[2]]),
                                                      freq         = FALSE,
                                                      desc         = "columns",
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
                                                      sw           = FALSE,
                                                      bar          = any(grepl("^BAR",       crrSPS)),
                                                      barCounts    = any(grepl("^BAR",       crrSPS)),
                                                      hist         = any(grepl("^HISTOGRAM", crrSPS)),
                                                      dens         = any(grepl("^HISTOGRAM", crrSPS))));
            # not implemented in jmv: TITLE, SUBTITLE, FOOTNOTE, LINE, PIE, PARETO, HILO, ERRORBAR, PANEL, INTERVAL (CI), TEMPLATE, MISSING
        }

        # MEANS ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-means
        else if (grepl("^MEANS", crrSPS[1])) {
            print("MEANS");
            crrFnc <- "jmv::descriptives";
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      splitBy      = fmtVar(crrVar[[2]]),
                                                      freq         = FALSE,
                                                      desc         = "columns",
                                                      n            = !any(grepl("^CELLS", crrSPS)) || any(grepl("^CELLS", crrSPS) & (grepl("COUNT|DEFAULT|ALL",  crrSPS))),
                                                      missing      = !any(grepl("^CELLS", crrSPS)) || any(grepl("^CELLS", crrSPS) & (grepl("COUNT|DEFAULT|ALL",  crrSPS))),
                                                      mean         = !any(grepl("^CELLS", crrSPS)) || any(grepl("^CELLS", crrSPS) & (grepl("MEAN|DEFAULT|ALL",   crrSPS))),
                                                      sd           = !any(grepl("^CELLS", crrSPS)) || any(grepl("^CELLS", crrSPS) & (grepl("STDDEV|DEFAULT|ALL", crrSPS))),
                                                      median       =                                  any(grepl("^CELLS", crrSPS) & (grepl("MEDIAN|ALL",         crrSPS))),
                                                      se           =                                  any(grepl("^CELLS", crrSPS) & (grepl("SEMEAN|ALL",         crrSPS))),
                                                      sum          =                                  any(grepl("^CELLS", crrSPS) & (grepl("SUM|ALL",            crrSPS))),
                                                      min          =                                  any(grepl("^CELLS", crrSPS) & (grepl("MIN|ALL",            crrSPS))),
                                                      max          =                                  any(grepl("^CELLS", crrSPS) & (grepl("MAX|ALL",            crrSPS))),
                                                      range        =                                  any(grepl("^CELLS", crrSPS) & (grepl("RANGE|ALL",          crrSPS))),
                                                      variance     =                                  any(grepl("^CELLS", crrSPS) & (grepl("VARIANCE|ALL",       crrSPS))),
                                                      kurt         =                                  any(grepl("^CELLS", crrSPS) & (grepl("KURT|ALL",           crrSPS))),
                                                      skew         =                                  any(grepl("^CELLS", crrSPS) & (grepl("SKEW|ALL",           crrSPS))),
                                                      ci           =                                  any(grepl("^CELLS", crrSPS) & (grepl("SEKURT|SESKEW|ALL",  crrSPS))),
                                                      mode         = FALSE,
                                                      iqr          = FALSE,
                                                      sw           = FALSE));
            # not implemented in jmv: MISSING (jmv's missing shows the number of missing cases, SPSS's excludes cases), STATISTICS;
            #                         CELLS subcommand: GMEDIAN, FIRST, LAST, NPCT, SPCT, HARMONIC, GEOMETRIC
        }

        # NPAR TESTS - K-S or NPTEST /ONESAMPLE TEST --------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if ((grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("K-S\\s*\\(NORMAL\\)",      crrSPS))) ||
                 (grepl("^NPTESTS",    crrSPS[1]) & any(grepl("ONESAMPLE TEST \\(.*\\)$", crrSPS)))) {
            print("NPAR TESTS - K-S - NORMAL or NPTESTS ONESAMPLE");
            crrFnc <- "jmv::descriptives";
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      splitBy      = fmtVar(crrVar[[2]]),
                                                      desc         = "rows",
                                                      missing      = FALSE,
                                                      mean         = FALSE,
                                                      median       = FALSE,
                                                      sd           = FALSE,
                                                      min          = FALSE,
                                                      max          = FALSE,
                                                      range        = FALSE,
                                                      sw           = TRUE));
            # not implemented in jmv: MISSING (jmv's missing shows the number of missing cases, SPSS's excludes cases), CRITERIA
        }

        # SUMMARIZE -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-summarize
        else if (grepl("^SUMMARIZE", crrSPS[1])) {
            print("SUMMARIZE");
            crrFnc <- "jmv::descriptives";
            # "vars" (NB: Only the first variable list is considered) and CELLS subcommand
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      splitBy      = fmtVar(crrVar[[2]]),
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
            print("NPAR TESTS - M-W");
            crrFnc <- "jmv::ttestIS";
            crrFlt <- paste0("(", chkFlt(paste0(paste0(crrVar[[2]][1], " == ", getLst(crrSPS, c(paste0("BY\\s+", crrVar[[2]][1]), paste0(crrVar[[1]], collapse = "\\s+"), "M-W", "=", "\\(|\\)"))),
                                                                               collapse = " | "), data), ")");
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      group        = fmtVar(crrVar[[2]][1]),
                                                      students     = FALSE,
                                                      welchs       = FALSE,
                                                      mann         = TRUE,
                                                      eqv          = TRUE,
                                                      meanDiff     = TRUE,
                                                      desc         = any(grepl("^STATISTICS",   crrSPS) & grepl("DESCRIPTIVES|ALL|^STATISTICS$", crrSPS)),
                                                      effectSize   = FALSE));
            # CRITERIA=CI and MISSING subcommand
            if (any(grepl("^METHOS.*?CIN", crrSPS))) crrArg <- updArg(crrArg,  argCI(crrSPS, ci4ES = crrArg$effectSize));
            # not implemented in jmv: STATISTICS (METHODS = MC, SAMPLES, EXACT)
        }

        # NPTESTS - INDEPENDENT - MANN_WHITNEY --------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("INDEPENDENT", crrSPS)) & any(grepl("MANN_WHITNEY", crrSPS))) {
            stop(sprintf("NPTESTS - INDEPENDENT - MANN_WHITNEY – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::ttestIS";
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      group        = fmtVar(crrVar[[2]][1]),
                                                      students     = FALSE,
                                                      welchs       = FALSE,
                                                      mann         = TRUE));

            # not implemented in jmv:
        }

        # T-TEST (with GROUPS) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl("^T-TEST GROUPS", crrSPS[1])) {
            print("T-TEST GROUPS");
            crrFnc <- "jmv::ttestIS";
            crrFlt <- paste0("(", chkFlt(paste0(paste0(crrVar[[2]][1], " == ", gsub("\\(|\\)", "", getLst(crrSPS, c("T-TEST\\s+GROUPS\\s*=", crrVar[[2]][1])))), collapse = " | "), data), ")");
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      group        = fmtVar(crrVar[[2]][1]),
                                                      students     = TRUE,
                                                      welchs       = TRUE,
                                                      mann         = FALSE,
                                                      eqv          = TRUE,
                                                      meanDiff     = TRUE,
                                                      desc         = TRUE,
                                                      effectSize   = any(grepl("^ES ", crrSPS))));
            # CRITERIA=CI and MISSING subcommand
            if (any(grepl("^CRITERIA=CI", crrSPS))) crrArg <- updArg(crrArg,  argCI(crrSPS, ci4ES = crrArg$effectSize));
            if (any(grepl("^MISSING",     crrSPS))) crrArg <- updArg(crrArg, argMsV(crrSPS));
            # not implemented in jmv: subcommand ES: STANDARDIZER (defaults to SD, i.e., Cohen's d - which is what jmv gives)
        }


        # T-Tests – Paired Samples T-Test =============================================================================================================================================================
        # https://www.jamovi.org/jmv/ttestps.html

        # NPAR TESTS - WILCOXON - PAIRED --------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("WILCOXON", crrSPS)) & any(grepl("PAIRED", crrSPS))) {
            print("NPAR TESTS - WILCOXON - PAIRED");
            crrFnc <- "jmv::ttestPS";
            crrArg <- updArg(getArg(crrFnc), pairlist(pairs        = fmtVar(crrVar, asPrs = TRUE),
                                                      students     = FALSE,
                                                      wilcoxon     = TRUE,
                                                      meanDiff     = TRUE,
                                                      desc         = any(grepl("^STATISTICS",   crrSPS) & grepl("DESCRIPTIVES|ALL|^STATISTICS$", crrSPS)),
                                                      effectSize   = FALSE));
            # CRITERIA=CI and MISSING subcommand
            if (any(grepl("^METHOS.*?CIN", crrSPS))) crrArg <- updArg(crrArg,  argCI(crrSPS, ci4ES = crrArg$effectSize));
            # not implemented in jmv: STATISTICS (METHODS = MC, SAMPLES, EXACT)
        }

        # NPTESTS - RELATED - WILCOXON -----------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("RELATED", crrSPS)) & any(grepl("WILCOXON", crrSPS))) {
            stop(sprintf("NPTESTS - RELATED - WILCOXON – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::ttestPS";
            crrArg <- updArg(getArg(crrFnc), pairlist(students = FALSE, wilcoxon = TRUE));

            # not implemented in jmv:
        }

        # T-TEST PAIRS --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl("^T-TEST PAIRS", crrSPS[1])) {
            print("T-TEST PAIRS");
            crrFnc <- "jmv::ttestPS";
            crrArg <- updArg(getArg(crrFnc), pairlist(pairs        = fmtVar(crrVar, asPrs = TRUE),
                                                      students     = TRUE,
                                                      wilcoxon     = FALSE,
                                                      meanDiff     = TRUE,
                                                      desc         = TRUE,
                                                      effectSize   = any(grepl("^ES ", crrSPS))));
            # CRITERIA=CI and MISSING subcommand
            if (any(grepl("^CRITERIA=CI", crrSPS))) crrArg <- updArg(crrArg,  argCI(crrSPS, ci4ES = crrArg$effectSize));
            if (any(grepl("^MISSING",     crrSPS))) crrArg <- updArg(crrArg, argMsV(crrSPS));
            # not implemented in jmv: subcommand ES: STANDARDIZER (defaults to SD, i.e., Cohen's d - which is what jmv gives)
        }


        # T-Tests – One Sample T-Test =================================================================================================================================================================
        # https://www.jamovi.org/jmv/ttestones.html

        # NPAR TESTS - WILCOXON - not PAIRED ----------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("WILCOXON", crrSPS)) & ! any(grepl("PAIRED", crrSPS))) {
            stop(sprintf("NPAR TESTS WILCOXON - not PAIRED (one sample) – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::ttestOneS";
            crrArg <- updArg(getArg(crrFnc), pairlist(students = FALSE, wilcoxon = TRUE));

            # not implemented in jmv:
        }

        # NPTESTS - ONESAMPLE - WILCOXON ---------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("ONESAMPLE", crrSPS)) & any(grepl("WILCOXON", crrSPS))) {
            stop(sprintf("NPTESTS - ONESAMPLE - WILCOXON – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::ttestOneS";
            crrArg <- updArg(getArg(crrFnc), pairlist(students = FALSE, wilcoxon = TRUE));

            # not implemented in jmv:
        }

        # T-TEST (with TESTVAL) -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl("^T-TEST", crrSPS[1]) & any(grepl("TESTVAL", crrSPS))) {
            print("T-TEST - One sample");
            crrFnc <- "jmv::ttestOneS";
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      students     = TRUE,
                                                      wilcoxon     = FALSE,
                                                      testValue    = as.numeric(getLst(crrSPS, "^TESTVAL\\s*=")),
                                                      hypothesis   = "dt",
                                                      meanDiff     = TRUE,
                                                      desc         = TRUE,
                                                      effectSize   = any(grepl("^ES ", crrSPS))));
            # CRITERIA=CI and MISSING subcommand
            if (any(grepl("^CRITERIA=CI", crrSPS))) crrArg <- updArg(crrArg,  argCI(crrSPS, ci4ES = FALSE));
            if (any(grepl("^MISSING",     crrSPS))) crrArg <- updArg(crrArg, argMsV(crrSPS));

            # not implemented in jmv: -
        }


        # ANOVAs – One-Way ANOVA ======================================================================================================================================================================
        # https://www.jamovi.org/jmv/anovaonew.html

        # ONEWAY --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-oneway
        else if (grepl("^ONEWAY", crrSPS[1]) && crrNmV[1] >= 1 && crrNmV[2] == 1 && crrNmV[3] == 0) {
            print("ONEWAY");
            crrFnc <- "jmv::anovaOneW";
            crrArg <- updArg(getArg(crrFnc), pairlist(deps         = fmtVar(crrVar[[1]][1]),
                                                      group        = fmtVar(crrVar[[2]][1]),
                                                      welchs       = any(grepl("^STATISTICS",    crrSPS) & grepl("WELCH|ALL",        crrSPS)),
                                                      fishers      = TRUE,
                                                      desc         = any(grepl("^STATISTICS",    crrSPS) & grepl("DESCRIPTIVES|ALL", crrSPS)),
                                                      descPlot     = any(grepl("^PLOT$",         crrSPS)),
                                                      eqv          = any(grepl("^STATISTICS",    crrSPS) & grepl("HOMOGENEITY|ALL",  crrSPS))));
            # POSTHOC and MISSING subcommands
            if (any(grepl("^POSTHOC", crrSPS))) crrArg <- updArg(crrArg, argPHT(crrSPS, crrFnc));
            if (any(grepl("^MISSING", crrSPS))) crrArg <- updArg(crrArg, argMsV(crrSPS));

            # not implemented in jmv: POLYNOMIAL / CONTRAST, STATISTICS (EFFECTS, BROWNFORSYTHE), MATRIX, TEMPLATE, CRITERIA, ES
        }

        # UNIANOVA: one dependent variable, one factor, no covariate ----------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-unianova
        else if (grepl("^UNIANOVA", crrSPS[1]) && crrNmV[1] >= 1 && crrNmV[2] == 1 && crrNmV[3] == 0 && !any(grepl("^RANDOM\\s*=", crrSPS))) {
            print("UNIANOVA");
            crrFnc <- "jmv::anovaOneW";
            crrArg <- updArg(getArg(crrFnc), pairlist(deps         = fmtVar(crrVar[[1]][1]),
                                                      group        = fmtVar(crrVar[[2]][1]),
                                                      welchs       = any(grepl("^PRINT|^STATISTICS",    crrSPS) & grepl("WELCH", crrSPS)),
                                                      fishers      = TRUE,
                                                      desc         = any(grepl("^PRINT|^STATISTICS",    crrSPS) & grepl("DESCRIPTIVES", crrSPS)),
                                                      descPlot     = any(grepl("^PLOT$",                crrSPS)),
                                                      norm         = any(grepl("^SAVE\\s*=\\s*ZRESID$", crrSPS)),
                                                      qq           = any(grepl("^SAVE\\s*=\\s*ZRESID$", crrSPS)),
                                                      eqv          = any(grepl("^PRINT|^STATISTICS",    crrSPS) & grepl("HOMOGENEITY", crrSPS))));
            # POSTHOC and MISSING subcommands
            if (any(grepl("^POSTHOC", crrSPS))) crrArg <- updArg(crrArg, argPHT(crrSPS, crrFnc));
            if (any(grepl("^MISSING", crrSPS))) crrArg <- updArg(crrArg, argMsV(crrSPS));

            # not implemented in jmv: REGWGT, METHOD (SS type), INTERCEPT, CRITERIA, TEST, LMATRIX, KMATRIX, CONTRAST, EMMEANS, ROBUST, SAVE, OUTFILE, MBPDESIGN, BPDESIGN, FDESIGN,
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
            crrFnc <- "jmv::ANOVA";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # GLM: Univariate -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-univariate
        # TO ADD: 1 dependent variable, independent variables as factors (no covariates)
        else if (grepl("^GLM", crrSPS[1]) && ! any(grepl("^WSFACTOR", crrSPS)) && crrNmV[1] == 1 && crrNmV[2] > 1 && crrNmV[3] == 0) {
            stop(sprintf("GLM – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::ANOVA";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # MANOVA: Univariate --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-univariate
        # TO ADD: 1 dependent variable, independent variables as factors (no covariates), check for repeated measurements version
        else if (grepl("^MANOVA", crrSPS[1]) && ! any(grepl("^WSFACTOR", crrSPS)) && crrNmV[1] == 1 && crrNmV[2] > 1 && crrNmV[3] == 0) {
            stop(sprintf("MANOVA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::ANOVA";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # UNIANOVA: one dependent variable, more than one factor, no covariate ------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-unianova
        else if (grepl("^UNIANOVA", crrSPS[1]) && crrNmV[1] == 1 && crrNmV[2] > 1 && crrNmV[3] == 0 && !any(grepl("^RANDOM\\s*=", crrSPS))) {
            print("UNIANOVA → ANOVA");
            crrFnc <- "jmv::ANOVA";
            crrArg <- updArg(getArg(crrFnc), pairlist(dep          = fmtVar(crrVar[[1]][1]),
                                                      factors      = fmtVar(crrVar[[2]]),
                                                      modelTest    = TRUE,
                                                      modelTerms   = fmtTrm(getLst(crrSPS, "^DESIGN\\s*=")),
                                                      ss           = argSS(crrSPS, crrFnc),
                                                      homo         = any(grepl("^PRINT|^STATISTICS",    crrSPS) & grepl("HOMOGENEITY", crrSPS)),
                                                      norm         = any(grepl("^SAVE\\s*=\\s*ZRESID$", crrSPS)),
                                                      qq           = any(grepl("^SAVE\\s*=\\s*ZRESID$", crrSPS))));
            # effect size, CONTRAST, POSTHOC, PLOT-PROFILE and MISSING subcommands
            if (any(grepl("^PRINT|^STATISTICS", crrSPS) & grepl("ETASQ", crrSPS))) crrArg <- updArg(crrArg, pairlist(effectSize = "partEta"));
            if (any(grepl("^CONTRAST",          crrSPS)))                          crrArg <- updArg(crrArg, argCon(crrSPS));
            if (any(grepl("^POSTHOC",           crrSPS)))                          crrArg <- updArg(crrArg, argPHT(crrSPS, crrFnc));
            if (any(grepl("^PLOT.*?PROFILE",    crrSPS)))                          crrArg <- updArg(crrArg, argEmm(crrSPS));
            if (any(grepl("^MISSING",           crrSPS)))                          crrArg <- updArg(crrArg, argMsV(crrSPS));

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
            crrFnc <- "jmv::anovaRM";
            crrRpM <- getRpM(crrSPS, crrVar[[1]]);
            # return number of levels
            # TO-DO: check regularly on "contrasts" (announced as "under development")
            crrArg <- updArg(getArg(crrFnc), pairlist(rm           = str2lang(jmvcore::sourcify(crrRpM[[1]])),
                                                      rmCells      = str2lang(jmvcore::sourcify(crrRpM[[2]])),
                                                      bs           = fmtVar(crrVar[[2]], asLng = TRUE),
                                                      cov          = fmtVar(crrVar[[3]], asLng = TRUE),
                                                      ss           = argSS(crrSPS, crrFnc),
                                                      contrasts    = list(NULL), # currently stored in crrRpM[[3]]
                                                      groupSumm    = TRUE));
            # within and between subjects design (WSDESIGN, DESIGN), type of square sum (METHOD), effect size (PRINT), and estimated marginal means (PLOT - PROFILE)
            if (any(grepl("^WSDESIGN\\s*=",     crrSPS))) crrArg <- updArg(crrArg, pairlist(rmTerms = fmtTrm(getLst(crrSPS, "^WSDESIGN\\s*="))));
            if (any(grepl("^DESIGN\\s*=",       crrSPS))) {
                crrTrm <- fixVar(getLst(crrSPS, "^DESIGN\\s*="), crrSPS, names(data));
                crrMsV <- max(crrMsV, chkMsV(crrTrm, runMsV));
                if (crrMsV == 1) stop("");
                crrArg <- updArg(crrArg, pairlist(bsTerms = fmtTrm(crrTrm)));
            }
            if (any(crrRpM[[4]] > 2))                     crrArg <- updArg(crrArg, pairlist(spherTests = TRUE, spherCorr = str2lang("list(\"none\", \"GG\", \"HF\")")));
            if (any(grepl("PRINT\\s*=.*?ETASQ", crrSPS))) crrArg <- updArg(crrArg, pairlist(effectSize = "partEta"));
            if (any(grepl("^PLOT.*?PROFILE",    crrSPS))) crrArg <- updArg(crrArg, argEmm(crrSPS));
            if (any(grepl("^POSTHOC\\s*=",      crrSPS))) crrArg <- updArg(crrArg, argPHT(crrSPS, crrFnc));
            # TO ADD: if MEASURE contains more than one variable - split models
            if (any(grepl("^MEASURE\\s*=",      crrSPS))) crrArg <- updArg(crrArg, pairlist(depLabel = getLst(crrSPS, "^MEASURE\\s*=", FALSE)));
            # not implemented in jmv: contrasts (POLYNOMIAL, etc.), subcommand REGWGT, subcommand METHOD (only type 2 and 3), subcommand INTERCEPT, subcommand MISSING,
            #                         subcommand PLOT (some parameters), subcommand PRINT (all parameters execpt ETASQ), subcommand SAVE, subcommand CRITERIA, subcommand EMMEANS (more complex customisation)
        }

        # MANOVA: Repeated Measures -------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-repeated-measures
        # TO ADD: check for WSFACTOR
        else if (grepl("^MANOVA", crrSPS[1]) && any(grepl("^WSFACTOR", crrSPS))) {
            stop(sprintf("MANOVA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::anovaRM";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # ANOVAs – ANCOVA =============================================================================================================================================================================
        # https://www.jamovi.org/jmv/ancova.html

        # GLM: Univariate -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-univariate
        # TO ADD: 1 dependent variable, independent variables can contain covariates
        else if (grepl("^GLM", crrSPS[1])    && ! any(grepl("^WSFACTOR", crrSPS)) && crrNmV[1] == 1 && all(crrNmV[2:3] >= 1)) {
            stop(sprintf("GLM – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::ancova";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # MANOVA: Univariate --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-univariate
        # TO ADD: 1 dependent variable, independent variables can contain covariates
        else if (grepl("^MANOVA", crrSPS[1]) && ! any(grepl("^WSFACTOR", crrSPS)) && crrNmV[1] == 1 && all(crrNmV[2:3] >= 1)) {
            stop(sprintf("MANOVA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::ancova";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # UNIANOVA: one dependent variable, one or more factors, one or more covariates ---------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-unianova
        # TO ADD: 1 dependent and 1 independent variable (factor, no covariate)
        else if (grepl("^UNIANOVA", crrSPS[1]) && crrNmV[1] == 1 && all(crrNmV[2:3] >= 1) && !any(grepl("^RANDOM\\s*=", crrSPS))) {
            print("UNIANOVA → ancova");
            crrFnc <- "jmv::ancova";
            crrArg <- updArg(getArg(crrFnc), pairlist(dep          = fmtVar(crrVar[[1]][1]),
                                                      factors      = fmtVar(crrVar[[2]]),
                                                      covs         = fmtVar(crrVar[[3]]),
                                                      modelTest    = TRUE,
                                                      modelTerms   = fmtTrm(getLst(crrSPS, "^DESIGN\\s*=")),
                                                      ss           = argSS(crrSPS, crrFnc),
                                                      homo         = any(grepl("^PRINT|^STATISTICS",    crrSPS) & grepl("HOMOGENEITY", crrSPS)),
                                                      norm         = any(grepl("^SAVE\\s*=\\s*ZRESID$", crrSPS)),
                                                      qq           = any(grepl("^SAVE\\s*=\\s*ZRESID$", crrSPS))));
            # effect size, CONTRAST, POSTHOC, PLOT-PROFILE and MISSING subcommands
            if (any(grepl("^PRINT|^STATISTICS", crrSPS) & grepl("ETASQ", crrSPS))) crrArg <- updArg(crrArg, pairlist(effectSize = "partEta"));
            if (any(grepl("^CONTRAST",          crrSPS)))                          crrArg <- updArg(crrArg, argCon(crrSPS));
            if (any(grepl("^POSTHOC",           crrSPS)))                          crrArg <- updArg(crrArg, argPHT(crrSPS, crrFnc));
            if (any(grepl("^PLOT.*?PROFILE",    crrSPS)))                          crrArg <- updArg(crrArg, argEmm(crrSPS));
            if (any(grepl("^MISSING",           crrSPS)))                          crrArg <- updArg(crrArg, argMsV(crrSPS));

            # not implemented in jmv: RANDOM, REGWGT, INTERCEPT, CRITERIA, TEST, LMATRIX, KMATRIX, CONTRAST, EMMEANS, ROBUST, SAVE, OUTFILE, MBPDESIGN, BPDESIGN, FDESIGN,
            #                         subcommand PRINT / STATISTICS (only HOMOGENEITY)
            #                         SAVE could be implemented manually later (at least for some outputs, e.g., PRED, RESID, ZRESID, possibly COOK)
        }


        # ANOVAs – MANCOVA ============================================================================================================================================================================
        # https://www.jamovi.org/jmv/mancova.html

        # GLM: Multivariate ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-multivariate
        # TO ADD: multiple dependent variables
        else if (grepl("^GLM", crrSPS[1])    && ! any(grepl("^WSFACTOR", crrSPS)) && crrNmV[1] > 1 && any(crrNmV[2:3] >= 1)) {
            stop(sprintf("GLM – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::mancova";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # MANOVA: Multivariate ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-multivariate
        # TO ADD: multiple dependent variables
        else if (grepl("^MANOVA", crrSPS[1]) && ! any(grepl("^WSFACTOR", crrSPS)) && crrNmV[1] > 1 && any(crrNmV[2:3] >= 1)) {
            stop(sprintf("MANOVA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::mancova";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # ANOVAs – One-Way ANOVA (Non-parametric) =====================================================================================================================================================
        # https://www.jamovi.org/jmv/anovanp.html

        # NPAR TESTS K-W ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        # TO ADD?: 1 dependent variables, 1 independent variable (factor, not covariate)
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("K-W", crrSPS))) {
            stop(sprintf("NPAR TESTS - Kruskal-Wallis – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::anovaNP";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # NPTESTS /INDEPENDENT - KRUSKAL_WALLIS -------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        # TO ADD?: 1 dependent variables, 1 independent variable (factor, not covariate)
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("INDEPENDENT", crrSPS)) & any(grepl("KRUSKAL_WALLIS", crrSPS))) {
            stop(sprintf("NPTESTS - Kruskal-Wallis – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::anovaNP";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # ANOVAs – Repeated Measures ANOVA (Non-parametric) ===========================================================================================================================================
        # https://www.jamovi.org/jmv/anovarmnp.html

        # NPAR TESTS FRIEDMAN -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("FRIEDMAN", crrSPS))) {
            stop(sprintf("NPAR TESTS - FRIEDMAN – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::anovaRMNP";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # NPTESTS /RELATED - FRIEDMAN -----------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("RELATED", crrSPS)) & any(grepl("FRIEDMAN", crrSPS))) {
            stop(sprintf("NPTESTS - Friedman – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::anovaRMNP";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Correlation and regression – Correlation Matrix =============================================================================================================================================
        # https://www.jamovi.org/jmv/corrmatrix.html

        # CORRELATIONS --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-correlations
        else if (grepl("^CORRELATIONS", crrSPS[1])) {
            print("CORRELATIONS");
            crrFnc <- "jmv::corrMatrix";
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      pearson      = TRUE,
                                                      spearman     = FALSE,
                                                      kendall      = FALSE,
                                                      sig          = TRUE,
                                                      flag         = any(grepl("^PRINT.*?NOSIG", crrSPS)),
                                                      n            = TRUE,
                                                      hypothesis   = ifelse(any(grepl("^PRINT.*?ONETAIL", crrSPS)), "pos", "corr")));
            if (any(grepl("^CI ", crrSPS))) crrArg <- updArg(crrArg, argCI(crrSPS));
            # not implemented in jmv: MISSING, PRINT [LOWER, LNODIAG, NOMATRIX], MATRIX, STATISTICS, calculation method and bias from CI
            # NOTE: SPSS only implements ONETAIL without sepcifying the direction (pos. / neg. correlations); it was assumed that "pos" would be more likely
        }

        # NONPAR CORR ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nonpar-corr
        else if (grepl("^NONPAR CORR", crrSPS[1])) {
            print("NONPAR CORR");
            crrFnc <- "jmv::corrMatrix";
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(crrVar[[1]]),
                                                      pearson      = FALSE,
                                                      spearman     = (!any(grepl("^PRINT", crrSPS)) || any(grepl("^PRINT", crrSPS) & grepl("SPEARMAN|BOTH", crrSPS))),
                                                      kendall      =                                   any(grepl("^PRINT", crrSPS) & grepl("KENDALL|BOTH",  crrSPS)),
                                                      sig          = TRUE,
                                                      flag         = any(grepl("^PRINT.*?NOSIG", crrSPS)),
                                                      n            = TRUE,
                                                      hypothesis   = ifelse(any(grepl("^PRINT.*?ONETAIL", crrSPS)), "pos", "corr")));
            if (any(grepl("^CI ", crrSPS))) crrArg <- updArg(crrArg, argCI(crrSPS));
            # not implemented in jmv: MISSING, PRINT [LOWER, LNODIAG, NOMATRIX], MATRIX, STATISTICS, calculation method and bias from CI
            # NOTE: SPSS only implements ONETAIL without sepcifying the direction (pos. / neg. correlations); it was assumed that "pos" would be more likely
        }

        # GRAPH - SCATTERPLOT -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-graph
        else if (grepl("^GRAPH", crrSPS[1]) && any(grepl("^SCATTERPLOT", crrSPS))) {
            print("GRAPH - SCATTERPLOT");
            crrFnc <- "jmv::corrMatrix";
            if (!is.null(crrVar[[2]])) crrCmt <- "The original SPSS-command contained as group-factor (\"%\") as well. This factor could not be used with the jmv-command."
            crrArg <- updArg(getArg(crrFnc), pairlist(vars         = fmtVar(c(crrVar[[1]], crrVar[[3]])),
                                                      pearson      = TRUE,
                                                      spearman     = FALSE,
                                                      kendall      = FALSE,
                                                      sig          = FALSE,
                                                      plots        = TRUE,
                                                      plotStats    = TRUE));
            # not implemented in jmv: TITLE, SUBTITLE, FOOTNOTE, LINE, PIE, PARETO, HILO, ERRORBAR, PANEL, INTERVAL (CI), TEMPLATE, MISSING
        }


        # Correlation and regression – Partial Correlation ============================================================================================================================================
        # https://www.jamovi.org/jmv/corrpart.html

        # PARTIAL CORR --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-partial-corr
        else if (grepl("^PARTIAL CORR", crrSPS[1])) {
            stop(sprintf("PARTIAL CORR – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::corrPart";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Correlation and regression – Linear Regression ==============================================================================================================================================
        # https://www.jamovi.org/jmv/linreg.html

        # LINEAR --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-linear
        else if (grepl("^LINEAR", crrSPS[1])) {
            stop(sprintf("LINEAR – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::linReg";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # REGRESSION ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-regression
        else if (grepl("^REGRESSION", crrSPS[1])) {
            print("REGRESSION");
            crrFnc <- "jmv::linReg";
            crrArg <- updArg(getArg(crrFnc), pairlist(dep          = fmtVar(crrVar[[1]][1]),
                                                      factors      = fmtVar(crrVar[[2]], asLng = TRUE),
                                                      covs         = fmtVar(crrVar[[3]], asLng = TRUE),
                                                      blocks       = getBlk(crrSPS),
                                                      refLevels    = getRfL(crrVar[[2]], data),
                                                      intercept    = "grandMean",
                                                      r            = any(grepl("^STATISTICS",   crrSPS) & grepl("R\\s+|DEFAULTS|ALL|^STATISTICS$", crrSPS)),
                                                      r2           = any(grepl("^STATISTICS",   crrSPS) & grepl("R\\s+|DEFAULTS|ALL|^STATISTICS$", crrSPS)),
                                                      r2Adj        = any(grepl("^STATISTICS",   crrSPS) & grepl("R\\s+|DEFAULTS|ALL|^STATISTICS$", crrSPS)),
                                                      modelTest    = any(grepl("^STATISTICS",   crrSPS) & grepl("ANOVA|DEFAULTS|ALL|^STATISTICS$", crrSPS)),
                                                      stdEst       = any(grepl("^STATISTICS",   crrSPS) & grepl("COEFF|DEFAULTS|ALL|^STATISTICS$", crrSPS)),
                                                      anova        = any(grepl("^STATISTICS",   crrSPS) & grepl("F\\s+|ALL",                       crrSPS)),
                                                      collin       = any(grepl("^STATISTICS",   crrSPS) & grepl("COLLIN|ALL",                      crrSPS)),
                                                      norm         = any((grepl("^RESIDUALS",   crrSPS) & grepl("NORMPROB|DEFAULTS",               crrSPS)) | grepl("^SAVE", crrSPS)),
                                                      qqPlot       = any((grepl("^RESIDUALS",   crrSPS) & grepl("NORMPROB|DEFAULTS",               crrSPS)) | grepl("^SAVE", crrSPS)),
                                                      durbin       = any((grepl("^RESIDUALS",   crrSPS) & grepl("DURBIN|DEFAULTS",                 crrSPS)) | grepl("^SAVE", crrSPS)),
                                                      resPlots     = any(grepl("^SCATTERPLOT",  crrSPS) | grepl("^PARTIALPLOT",                    crrSPS)  | grepl("^SAVE", crrSPS)),
                                                      cooks        = any(grepl("^SAVE.*?COOKS", crrSPS))));
            if (any(grepl("^STATISTICS.*?CI", crrSPS))) crrArg <- updArg(crrArg, argCI(crrSPS, ci4Std = crrArg$stdEst, ci4EMM = FALSE));

            # not implemented in jmv: CASEWISE, CRITERIA (jamovi doesn't support step-wise), DESCRIPTIVES (some measures), MATRIX, METHOD, MISSING,
            #                         ORIGIN, OUTFILE, REGWGT, RESIDUALS (some measures), SELECT, STATISTICS (some measures), SAVE, TEMPLATE
        }

        # Correlation and regression – Binomial / Multinomial / Ordinal Logistic Regression ===========================================================================================================
        # https://www.jamovi.org/jmv/logregbin.html
        # https://www.jamovi.org/jmv/logregmulti.html
        # https://www.jamovi.org/jmv/logregord.html

#data    the data as a data frame
#dep    a string naming the dependent variable from data, variable must be a factor
#covs    a vector of strings naming the covariates from data
#factors    a vector of strings naming the fixed factors from data
#blocks    a list containing vectors of strings that name the predictors that are added to the model. The elements are added to the model according to their order in the list
#refLevels    a list of lists specifying reference levels of the dependent variable and all the factors
#modelTest    TRUE or FALSE (default), provide the model comparison between the models and the NULL model
#dev    TRUE (default) or FALSE, provide the deviance (or -2LogLikelihood) for the models
#aic    TRUE (default) or FALSE, provide Aikaike's Information Criterion (AIC) for the models
#bic    TRUE or FALSE (default), provide Bayesian Information Criterion (BIC) for the models
#pseudoR2    one or more of 'r2mf', 'r2cs', or 'r2n'; use McFadden's, Cox & Snell, and Nagelkerke pseudo-R², respectively
#omni    TRUE or FALSE (default), provide the omnibus likelihood ratio tests for the predictors
#thres    TRUE or FALSE (default), provide the thresholds that are used as cut-off scores for the levels of the dependent variable (ONLY ORDINAL!!!)
#ci    TRUE or FALSE (default), provide a confidence interval for the model coefficient estimates
#ciWidth    a number between 50 and 99.9 (default: 95) specifying the confidence interval width
#OR    TRUE or FALSE (default), provide the exponential of the log-odds ratio estimate, or the odds ratio estimate
#ciOR    TRUE or FALSE (default), provide a confidence interval for the model coefficient odds ratio estimates
#ciWidthOR    a number between 50 and 99.9 (default: 95) specifying the confidence interval width
#emMeans    a list of lists specifying the variables for which the estimated marginal means need to be calculate. Supports up to three variables per term. (ONLY: Binary / Multinomial)
#ciEmm    TRUE (default) or FALSE, provide a confidence interval for the estimated marginal means  (ONLY: Binary / Multinomial)
#ciWidthEmm    a number between 50 and 99.9 (default: 95) specifying the confidence interval width for the estimated marginal means  (ONLY: Binary / Multinomial)
#emmPlots    TRUE (default) or FALSE, provide estimated marginal means plots  (ONLY: Binary / Multinomial)
#emmTables    TRUE or FALSE (default), provide estimated marginal means tables  (ONLY: Binary / Multinomial)
#emmWeights    TRUE (default) or FALSE, weigh each cell equally or weigh them according to the cell frequency  (ONLY: Binary / Multinomial)
#class    TRUE or FALSE (default), provide a predicted classification table (or confusion matrix) (ONLY: Binary)
#acc    TRUE or FALSE (default), provide the predicted accuracy of outcomes grouped by the cut-off value (ONLY: Binary)
#spec    TRUE or FALSE (default), provide the predicted specificity of outcomes grouped by the cut-off value (ONLY: Binary)
#sens    TRUE or FALSE (default), provide the predicted sensitivity of outcomes grouped by the cut-off value (ONLY: Binary)
#auc    TRUE or FALSE (default), provide the rea under the ROC curve (AUC) (ONLY: Binary)
#rocPlot    TRUE or FALSE (default), provide a ROC curve plot (ONLY: Binary)
#cutOff    TRUE or FALSE (default), set a cut-off used for the predictions (ONLY: Binary)
#cutOffPlot    TRUE or FALSE (default), provide a cut-off plot (ONLY: Binary)
#collin    TRUE or FALSE (default), provide VIF and tolerence collinearity statistics (ONLY: Binary)
#boxTidwell    TRUE or FALSE (default), provide Box-Tidwell test for linearity of the logit (ONLY: Binary)
#cooks    TRUE or FALSE (default), provide summary statistics for the Cook's distance (ONLY: Binary)

        # LOGISTIC REGRESSION -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-logistic-regression
        else if (grepl("^LOGISTIC REGRESSION", crrSPS[1])) {
            stop(sprintf("LOGISTIC REGRESSION - Binomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::logRegBin";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        else if (grepl("^DISCRIMINANT", crrSPS[1])) {
            stop(sprintf("DISCRIMINANT - Binomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::logRegBin";
            crrCmt <- c("# Linear discriminant analysis (LDA) and logistic regression (LR) typically reveal similar results if the underlying assumptions (e.g., normality) are not badly violated.",
                       "# Logistic regression (which is available in jmv) was chosen as replacement for LDA. In cases where a LDA is required, this has to be implemented in R-code (e.g. with MASS).")
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Frequency analyses – Proportion Test (2 Outcomes) ===========================================================================================================================================
        # https://www.jamovi.org/jmv/proptest2.html

        # NPAR TESTS BINOMIAL -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("BINOMIAL", crrSPS))) {
            stop(sprintf("NPAR TESTS - Binomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::propTest2";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # NPTESTS /ONESAMPLE BINOMIAL -----------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("ONESAMPLE", crrSPS)) & any(grepl("BINOMIAL", crrSPS))) {
            stop(sprintf("NPTESTS - Binomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::propTest2";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Frequency analyses – Proportion Test (N Outcomes) ===========================================================================================================================================
        # https://www.jamovi.org/jmv/proptestn.html

        # NPAR TESTS CHISQUARE ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("CHISQUARE", crrSPS))) {
            stop(sprintf("NPAR TESTS - Chi-squared – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::propTestN";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # NPTESTS /ONESAMPLE CHISQUARE ----------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("ONESAMPLE", crrSPS)) & any(grepl("CHISQUARE", crrSPS))) {
            stop(sprintf("NPTESTS - Binomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::propTest2";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Frequency analyses – Contingency Tables =====================================================================================================================================================
        # https://www.jamovi.org/jmv/conttables.html

        # CROSSTABS -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-crosstabs
        else if (grepl("^CROSSTABS", crrSPS[1])) {
            print("CROSSTABS");
            if (grepl("VARIABLES\\s*=", crrSPS[1])) stop(sprintf("CROSSTABS – VARIABLES not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::contTables";
            chiSqr <- any(grepl("^STATISTIC\\s*=", crrSPS) & grepl("CHISQ|ALL", crrSPS)) | all(!grepl("^STATISTIC\\s*=", crrSPS));
            crrArg <- updArg(getArg(crrFnc), pairlist(rows         = fmtVar(crrVar[[1]][1]),
                                                      cols         = fmtVar(crrVar[[2]][1]),
                                                      layers       = fmtVar(crrVar[[2]][2]),
                                                      chiSq        = chiSqr,
                                                      likeRat      = chiSqr,
                                                      mh           = chiSqr,
                                                      fisher       = chiSqr,
                                                      chiSqCorr    = chiSqr,
                                                      phiCra       = any(grepl("^STATISTIC\\s*=", crrSPS) & grepl("ALL|PHI",   crrSPS)),
                                                      contCoef     = any(grepl("^STATISTIC\\s*=", crrSPS) & grepl("ALL|CC",    crrSPS)),
                                                      taub         = any(grepl("^STATISTIC\\s*=", crrSPS) & grepl("ALL|BTAU",  crrSPS)),
                                                      gamma        = any(grepl("^STATISTIC\\s*=", crrSPS) & grepl("ALL|GAMMA", crrSPS)),
                                                      relRisk      = any(grepl("^STATISTIC\\s*=", crrSPS) & grepl("ALL|RISK",  crrSPS)),
                                                      obs          = any(grepl("^CELLS\\s*=.*?COUNT",    crrSPS)),
                                                      exp          = any(grepl("^CELLS\\s*=.*?EXPECTED", crrSPS)),
                                                      pcRow        = any(grepl("^CELLS\\s*=.*?ROW",      crrSPS)),
                                                      pcCol        = any(grepl("^CELLS\\s*=.*?COLUMN",   crrSPS)),
                                                      pcTot        = any(grepl("^CELLS\\s*=.*?TOTAL",    crrSPS)),
                                                      diffProp     = any(grepl("^CELLS\\s*=.*?PROP",     crrSPS)),
                                                      barplot      = any(grepl("^BARCHART",              crrSPS))));
            if (any(grepl("^METHOD.*?CIN", crrSPS))) crrArg <- updArg(crrArg, argCI(crrSPS));

            # not implemented in jmv: VARIABLES, CELLS (RESID, SRESID, ASRESID, BPROP, ALL), STATISTIC (LAMBDA, UC, CTAU, D, ETA, CORR, KAPPA, MCNEMAR, CMH),
            #                         METHOD (all except CIN), MISSING, FORMAT, COUNT, WRITE, HIDESMALLCOUNTS, SHOWDIM
        }

        # CTABLES -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-ctables
        else if (grepl("^CTABLES", crrSPS[1])) {
            print("CTABLES - only simple counts, no advanced statistics, are converted");
            crrFnc <- "jmv::contTables";
            crrArg <- updArg(getArg(crrFnc), pairlist(rows         = fmtVar(crrVar[[1]][1]),
                                                      cols         = fmtVar(crrVar[[1]][2]),
                                                      layers       = fmtVar(crrVar[[1]][3]),
                                                      chiSq        = any(grepl("^SIGTEST\\s*=.*?CHISQUARE",  crrSPS)),
                                                      obs          = any(grepl("^TABLE.*?COUNT",             crrSPS)),
                                                      pcRow        = any(grepl("^TABLE.*?ROWPCT.COUNT",      crrSPS)),
                                                      pcCol        = any(grepl("^TABLE.*?COLPCT.COUNT",      crrSPS)),
                                                      pcTot        = any(grepl("^TABLE.*?TABLEPCT.COUNT",    crrSPS))));
            if (any(grepl("^CRITERIA.*?CILEVEL", crrSPS))) crrArg <- updArg(crrArg, argCI(crrSPS));

            # not implemented in jmv: FORMAT, MRSETS, SMISSING, PCOMPUTE, PPROPERTIES, WEIGHT, HIDESMALLCOUNTS, SLABELS, CLABELS, CATEGORIES, TITLES, TABLES (most statistics), COMPARETEST
        }


        # Frequency analyses – Paired Samples Contingency Tables ======================================================================================================================================
        # https://www.jamovi.org/jmv/conttablespaired.html

        # NPAR TESTS MCNEMAR --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl("^NPAR TESTS", crrSPS[1]) & any(grepl("MCNEMAR", crrSPS))) {
            stop(sprintf("NPAR TESTS MCNEMAR – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::contTablesPaired";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # NPTESTS /RELATED MCNEMAR ----------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl("^NPTESTS", crrSPS[1]) & any(grepl("RELATED", crrSPS)) & any(grepl("MCNEMAR", crrSPS))) {
            stop(sprintf("NPTESTS - Binomial – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::contTablesPaired";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Frequency analyses – Log-Linear Regression ==================================================================================================================================================
        # https://www.jamovi.org/jmv/loglinear.html

        # LOGLINEAR -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-loglinear
        else if (grepl("^LOGLINEAR", crrSPS[1])) {
            stop(sprintf("LOGLINEAR – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::logLinear";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Factor analysis – Reliability Analysis ======================================================================================================================================================
        # https://www.jamovi.org/jmv/reliability.html

        # RELIABILITY ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-reliability
        else if (grepl("^RELIABILITY", crrSPS[1])) {
            stop(sprintf("RELIABILITY – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::reliability";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }


        # Factor analysis – Principal Component Analysis ==============================================================================================================================================
        # https://www.jamovi.org/jmv/pca.html

        # FACTOR - /EXTRACTION = PC, PA1, DEFAULT -----------------------------------------------------------------------------------------------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-factor
        else if (grepl("^FACTOR", crrSPS[1])) {
            stop(sprintf("FACTOR - PCA – not implemented yet: l. %d - %s", i, vecSPS[i]));
            crrFnc <- "jmv::pca";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

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
            crrFnc <- "jmv::efa";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

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
#           crrFnc <- "";
#           crrArg <- updArg(getArg(crrFnc), pairlist());

            # not implemented in jmv:
        }

        # check that the variables are of the correct type and add converted command / function the the syntax list ===================================================================================
        if (exists("crrFnc") && grepl("^jmv::", crrFnc)) {
            # ensure that the variable class matches what is required of the analyses
#           data <- xfmVar(data, crrFnc, crrVar, clsVar, i, vecSPS[i]);
            if (!exists("crrCmt")) crrCmt <- c();
            vecJMV <- c(vecJMV, "", paste0("# SPSS: ", vecSPS[i]), getCmt(crrMsV, crrSPS, crrCmt),
                                    paste0(rep("# ", crrMsV > 1), gsub("^list\\(", paste0(crrFnc, "("), gsub("\\( ", "\\(", gsub("\\n\\s+", " ", sourcify(clnArg(crrArg, crrFnc)))))), "");
#           for testing commands: if(!crrMsV) eval(parse(text = gsub("^list\\(", paste0(crrFnc, "("), gsub("\\( ", "\\(", gsub("\\n\\s+", " ", sourcify(clnArg(crrArg, crrFnc)))))))
        }
        rm(list = ls(pattern = "crr*"));
    }

    # clean up the output vector: remove first and last line if those are empty, and adjacent duplicate lines
    if (vecJMV[1] == "") vecJMV <- vecJMV[-1];
    if (vecJMV[length(vecJMV)] == "") vecJMV <- vecJMV[-length(vecJMV)];
    vecJMV <- vecJMV[c(TRUE, vecJMV[-length(vecJMV)] != vecJMV[-1])];
    attr(data, "syntax") <- vecJMV;
    data
}


# =====================================================================================================================================================================================================
# only temporary: jmvcore::sourcify() currently can't handle formulas
# =====================================================================================================================================================================================================

sourcify <- function(object, indent = "") {

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
            sep <- ", "
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
            sep <- ", "
        }
        source <- paste0(source, ")")

        if (nchar(source) > 40)
            source <- gsub(", ", paste0(",\n    ", indent), source, fixed = TRUE)

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
            sep <- ", "
        }
        source <- paste0(source, ")")

        if (nchar(source) > 40) {
            source <- gsub("c(\"", paste0("c(\n    ",  indent, "\""), source, fixed = TRUE)
            source <- gsub("\", ", paste0("\",\n    ", indent),       source, fixed = TRUE)
        }

        return(source)

    } else if (is.symbol(object) || is.language(object)) {
        if (any(nchar(format(object)) > 0)) {
            return(paste(trimws(format(object)), collapse = " "))
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

getSPS <- function(fleSPS = "") {
    if (!file.exists(fleSPS) && !file.exists(file.path(getwd(), basename(fleSPS)))) {
        stop(sprintf("\"%s\" does not exist.", fleSPS));
    }
    vecSPS <- readLines(hdlSPS <- file(fleSPS, "r"), warn = FALSE); close(hdlSPS); rm("hdlSPS");
    vecSPS <- clnSPS(vecSPS);
    vecSPS <- vecSPS[! grepl("^$|^.$", vecSPS)];
    # check that all lines end with a "." - possibly check for the command being in capitals too
    if (! all(grepl(grcSPS, vecSPS) & grepl("\\.$", vecSPS))) {
        stop(sprintf("\n\nThe syntax contains commands that could not be parsed or that don\'t end with a \".\":\n\n%s\n\n",
             paste0(vecSPS[!(grepl(grcSPS, vecSPS) & grepl("\\.$", vecSPS))], collapse = "\n")));
    }
    vecSPS
}

getVar <- function(crrSPS = c(), data = data.frame()) {
    vecVar <- "";
    # handle REGRESSION first (may contain VARIABLES), and afterwards the most common cases (the keywords VARIABLES and TABLES)
    if (grepl("^REGRESSION", crrSPS[1])) {
        vecVar <- unique(getLst(crrSPS, c("METHOD=", "STEPWISE", "FORWARD", "BACKWARD", "ENTER", "REMOVE", "TEST")));
        clsVar <- unlist(lapply(data[vecVar], class));
        if (any(!grepl("character|factor|numeric|integer", clsVar))) stop(sprintf("Data.frame contains an illegal column type: \"%s\"", clsVar[!grepl("character|factor|numeric|integer", clsVar)]));
        vecVar <- c(getLst(crrSPS, c("^DEPENDENT", "=")), rep("BY",   length(grep("character|factor", clsVar))), vecVar[grepl("character|factor", clsVar)],
                                                          rep("WITH", length(grep("numeric|integer",  clsVar))), vecVar[grepl("numeric|integer",  clsVar)]);
    } else if (any(grepl("VARIABLES", crrSPS))) {
        # TO DO: possibly remove parentheses around the variable list
        vecVar <- strSpl(gsub("\\(.*?\\)", "", gsub("\\s*=\\s*", "", gsub("DISPLAY\\s*=\\s*[A-z]+\\s*", "", gsub("MISSING\\s*=\\s*[A-z]+\\s*", "", gsub("REPORTMISSING\\s*=\\s*NO\\s*", "",
                         strSpl(crrSPS[grepl("VARIABLES", crrSPS)], "VARIABLES\\s?")[2]))))), "\\s+");
        if (grepl("^T-TEST\\s+GROUPS=", crrSPS[1])) vecVar <- c(vecVar, "BY", getLst(crrSPS[1], c("^T-TEST\\s+GROUPS=", "\\(.*?\\)")));
    } else if (any(grepl("TABLES\\s*=", crrSPS))) {
        vecVar <- strSpl(gsub("=", "", strSpl(crrSPS[grepl("TABLES", crrSPS)], "TABLES\\s?")[2]), "\\s+");
    # afterwards, go through a list with less common keywords to match and remove
    } else {
        for (begKey in list(c("COUNT", "^.*?=\\s*", "\\(.*?\\)$"), c("^GLM\\s+"), c("^K-S\\(NORMAL\\)="), c("^MEANS", "TABLES="), c("^M-W=", "\\(.*?\\)"),
                            c("^ONESAMPLE", "TEST\\s+\\(", "\\)"), c("^ONEWAY"), c("^SUMMARIZE", "TABLES="), c("^T-TEST\\s+PAIRS=", "\\(PAIRED\\)"), c("^UNIANOVA\\s+"),
                            c("^WILCOXON=", "\\(PAIRED\\)"), c("^SCATTERPLOT", "\\(BIV.*?\\)", "=", "VALUE\\(", "\\)"),
                            c("^BAR", "\\(SIMPLE\\)|\\(GROUPED\\)|\\(STACKED\\)|\\(RANGE\\)", "=", "VALUE\\(", "\\)"),
                            c("^HISTOGRAM", "\\(NORMAL\\)", "=", "VALUE\\(", "\\)"))) {
            if (any(grepl(begKey[1], crrSPS))) {
                vecVar <- getLst(crrSPS, begKey);
                break
            }
        }
    }
    vecVar <- unique(gsub("^WITH$", "WITH", gsub("^BY$", "BY", vecVar, ignore.case = TRUE), ignore.case = TRUE));

    # check whether all variables are contained in the data file, also takes care of replacing ALL and TO
    vecVar <- fixVar(vecVar, crrSPS, names(data));

    # split the variables into a list with 3 elements: dependent, indep.: factor / categorical, indep.: numeric / continuous
    grpKey <- c("^BY$", "^WITH$");
    outVar <- vector(mode = "list", length = length(grpKey) + 1);
    posBnW <- grep(paste(grpKey, collapse = "|"), vecVar);
    if (length(posBnW) > 0) {
        for (j in posBnW) {
            for (k in seq_along(grpKey)) {
                if (grepl(grpKey[k], vecVar[j])) {
                    outVar[[k + 1]] <- vecVar[(j + 1):min(c(posBnW[posBnW > j] - 1, length(vecVar)))];
                }
            }
        }
        outVar[[1]] <- vecVar[seq_len(min(posBnW) - 1)];
    } else {
        outVar[[1]] <- vecVar;
    }

    outVar
}

getLst <- function(crrSPS = c(), begKey = c(), splLst = TRUE) {
    vecLst <- crrSPS[grepl(begKey[1], crrSPS)];
    for (j in seq_along(begKey)) vecLst <- trimws(gsub(begKey[j], "", vecLst));
    if (splLst) {
        strSpl(vecLst, "\\s+|,")
    } else {
        vecLst
    }
}

getArg <- function(crrFnc = "", addDta = TRUE) {
    if (addDta) {
        fltAnl <- tryCatch(get("fltAnl", envir = parent.frame()), error = function(e) return(""));
        crrFlt <- tryCatch(get("crrFlt", envir = parent.frame()), error = function(e) return(""));
        argFlt <- paste0(c(fltAnl, crrFlt), collapse = ifelse(all(c(nchar(fltAnl), nchar(crrFlt)) > 0), " & ", ""));
        crrRnd <- tryCatch(get("crrRnd", envir = parent.frame()), error = function(e) return(NULL));
        # if there is a variable that represents a random factor in SPSS, jmv wouldn't accept the variable if all occurrences
        # of a particular level of that variable (i.e., in most cases a level is a participant) contain NAs
        if (!is.null(crrRnd) && !identical(crrRnd, character(0))) {
            crrVar <- tryCatch(get("crrVar", envir = parent.frame()), error = function(e) return(NULL));
            data   <- tryCatch(get("data",   envir = parent.frame()), error = function(e) return(NULL));
            if (argFlt == "") {
                crrFlt <- rep(TRUE, dim(data)[1]);
            } else {
                crrFlt <- eval(parse(text = argFlt));
            }
            crrDta <- data[crrFlt, unlist(crrVar)];
            if (any(is.na(crrDta))) {
                # a little complicated: (1) use table to count how often per step of crrRnd NA occurs in any other variable,
                # (2) if "FALSE" (no occur. of NA) is 0 (i.e., all cases where all occur. are NA) and (3) extracts the names
                # (i.e., the steps of the variable in crrRnd containing only NAs) from those
                rndExc <- attr(which(table(crrDta[[crrRnd]], apply(crrDta, MARGIN = 1, function(x) any(is.na(x))))[, "FALSE"] == 0), "names");
                for (j in seq_along(rndExc)) argFlt <- paste0(argFlt, ifelse(argFlt == "", "", " & "), "data$", crrRnd, " != ",
                                                                      ifelse(is.character(crrDta[[crrRnd]]), paste0("\"", rndExc[[j]], "\""), rndExc[j]));
                print(argFlt);
                print(paste(unlist(crrVar), collapse = ", "));
            }
        }
        updArg(getArg(crrFnc, FALSE), pairlist(data = as.symbol(paste0("data", ifelse(argFlt != "", paste0("[", argFlt, ", ]"), "")))))
    } else {
#       eval(parse(text = paste0("formals(", crrFnc, ")")))
        crrArg <- as.list(eval(parse(text = paste0(gsub("ANOVA", "anova", gsub("::", ":::", crrFnc)), "Options$new()$values()"))))
        crrArg[setdiff(names(crrArg), "levels")]
    }
}

getRpM <- function(crrSPS = c(), inpVar = c()) {
    crrWSF <- getLst(crrSPS, "WSFACTOR\\s*=");
    # we know that each WSF has a name followed by the levels (which has to be integer) and the contrast type (may be left out) and a possible fourth parameter (the reference category)
    posLvl <- grep("\\d+", crrWSF);
    numLvl <- as.integer(crrWSF[posLvl]);
    # TO-DO: extract contrasts
    outRpM <- list(vector(mode = "list", length = length(posLvl)), vector(mode = "list", length = prod(numLvl)), vector(mode = "list", length = length(posLvl)), numLvl);
    for (j in seq_along(posLvl)) {
        outRpM[[1]][[j]] <- list(label = crrWSF[posLvl[j] - 1], levels = sprintf(paste0(crrWSF[posLvl[j] - 1], "%d"), seq_len(numLvl[j])));
        if (j == 1) {
                cmbLvl <- outRpM[[1]][[j]]$levels;
            } else {
                cmbLvl <- sprintf("%s, %s", rep(cmbLvl, each = numLvl[j]), rep(outRpM[[1]][[j]]$levels, length(cmbLvl)));
            }
        if (grepl("^DEVIATION|^SIMPLE|^DIFFERENCE|^HELMERT|^REPEATED|^POLYNOMIAL", toupper(crrWSF[posLvl[j] + 1]))) outRpM[[3]][[j]] <- toupper(crrWSF[posLvl[j] + 1]);
    }
    if (! all(duplicated(c(length(inpVar), length(cmbLvl), prod(numLvl)))[-1])) {
        stop("Mismatch between the number of possible combinations of factor levels and the number of within-subject variables.");
    }
    for (j in seq_len(prod(numLvl))) {
        outRpM[[2]][[j]] <- list(measure = inpVar[j], cell = strSpl(cmbLvl[j], ","))
    }

    outRpM
}

getBlk <- function(crrSPS = c()) {
   crrBlk <- grep("^METHOD\\s*=", crrSPS);
   outBlk <- vector(mode = "list", length = length(crrBlk))
   for (j in seq_along(outBlk)) {
       if (grepl("BACKWARD|REMOVE|TEST", crrSPS[crrBlk[j]])) {
           warning("REGRESSION: The METHODs BACKWARD, REMOVE and TEST are currently not implemented.");
           next
       }
       outBlk[[j]] <- as.list(setdiff(getLst(crrSPS[crrBlk[j]], c("^METHOD", "=", "ENTER|STEPWISE|FORWARD")), unlist(outBlk)));
   }

   str2lang(jmvcore::sourcify(outBlk))
}

getRfL <- function(facVar = c(), data = data.frame()) {
    if (is.null(facVar)) return(NULL)

    outRfL <- vector(mode = "list", length = length(facVar))
    for (j in seq_along(outRfL)) {
        outRfL[[j]] <- pairlist(var = facVar[j], ref = names(which.max(table(data[[facVar[j]]]))));
    }

    str2lang(jmvcore::sourcify(outRfL))
}

getCmt <- function(inpMsV = 0, inpSPS = c(), inpCmt = c()) {
    outCmt <- character(0)

    if        (inpMsV == 1) {
        outCmt <- c(outCmt, "# The analysis below (jmv::...) can be run but (at least) one variable is missing. The results are therefore going to be different from the results obtained in SPSS.");
    } else if (inpMsV == 2) {
        outCmt <- c(outCmt, "# The analysis below (jmv::...) involves variables that are not contained in the dataset; it is therefore commented out (begins with \"#\") so that it is not run.",
                            paste("# If the command is commented out although you tried to enforce runnning analyses containing missing variables (runMsV = TRUE),",
                                  "all variables within one category (e.g., dependent) were missing."));
    }
    if (any(grepl("ZRE_\\d+", inpSPS))) {
        outCmt <- c(outCmt, "# Saving residuals or std. residuals is often done to evaluate whether they are normally distributed, etc. jamovi often offers this within analyses.");
    } else if (any(grepl("MAH_\\d+", inpSPS))) {
        outCmt <- c(outCmt, "# Saving the Mahalanobis distance is done to evaluate whether the data contain multivariate outliers; jamovi needs to do this with syntax.");
    }

    if (length(inpCmt) > 0 && !all(grepl("^$", inpCmt))) {
        outCmt <- c(outCmt, inpCmt[inpCmt != ""]);
    }

    outCmt
}

clnArg <- function(crrArg = list(), crrFnc = "") {
   defArg <- getArg(crrFnc, FALSE);
   for (argNme in names(defArg)) {
       if (! hasName(crrArg, argNme)) stop(sprintf("The argument list doesn\'t contain an entry for \"%s\".", argNme));
       if (identical(crrArg[[argNme]], defArg[[argNme]])) crrArg[[argNme]] <- NULL
   }
   crrArg
}

updArg <- function(crrArg = list(), addArg = list()) {
    nmeArg <- names(crrArg);
    for (addNme in names(addArg)) {
        # check whether the argument exists in crrArg
        if (! any(grepl(addNme, nmeArg))) {
            stop(sprintf("Argument \"%s\" is not a valid argumenent / parameter name.", addNme));
        }
        # check whether the argument to add is NULL, if the original argument is also NULL don't do anything, otherwise throw an error
        if (is.null(addArg[[addNme]])) {
            if (is.null(crrArg[[addNme]])) next
            stop(sprintf("The argument value for \"%s\" (to be added / updated) is currently set to NULL (which would cause that argument to be removed).", addNme));
        }
        # check whether the variable type of the argument to add and the argument in the parameter list is the same
        # issue a warning if the original argument and the argument to be updated are of different type, unless the original argument is NULL
        if (typeof(crrArg[[addNme]]) != typeof(addArg[[addNme]]) && ! is.null(crrArg[[addNme]])) {
            warning(sprintf("Variable type is not the same for the argument \"%s\" – %s [default] vs. %s [to be added].", addNme, typeof(crrArg[[addNme]]), typeof(addArg[[addNme]])));
        };
        crrArg[[addNme]] <- addArg[[addNme]];
    }
    crrArg
}

fmtClc <- function(strCmd = "", rplCmd = c(), vecVar = c()) {
    if (length(rplCmd) %% 2 != 0) stop(sprintf("Vector with replacement strings must be multiples of 2 (original, replacement): \"%s\"\n\n", paste0(rplCmd, collapse = ", ")));
    for (j in seq_along(rplCmd)) {
        if (j %% 2 == 1) {
            strCmd <- gsub(paste0("^", rplCmd[j + 0]), rplCmd[j + 1], strCmd);
        }
    }

    # enclose all variable names with "data[, c(\"" and "\")]" in the first place
    for (j in seq_along(vecVar)) {
        strCmd <- gsub(vecVar[j], paste0("data[, c(\"", vecVar[j], "\")]"), strCmd, ignore.case = TRUE);
    }

    # if those variables are contained inside a function call, they would be separated only by commata, in such case, simplify the formula such that all variable names
    # are contained in one c()-vector inside data[, ...]
    strCmd <- gsub("\\)\\], data\\[, c\\(", ", ", strCmd)

    strCmd
}

fmtTrm <- function(inpVar = c()) {
    str2lang(paste0(" ~ ", paste(gsub("\\*", ":", inpVar), collapse = " + ")))
}

fmtVar <- function(inpVar = c(), asLng = FALSE, asPrs = FALSE) {
    if (length(inpVar) == 0 || any(is.na(inpVar))) {
        NULL
    } else if (asPrs) {
        if (length(inpVar[[1]]) != length(inpVar[[3]])) {
            stop("Variables do not match up as pairs.");
        }
        outVar <- vector(mode = "list", length = length(inpVar[[1]]));
        for (j in seq_len(length(inpVar[[1]]))) {
            outVar[[j]] <- list(i1 = inpVar[[1]][j], i2 = inpVar[[3]][j]);
        }
        as.symbol(gsub("\\(\\s*", "(", gsub("\\n\\s*", " ", jmvcore::sourcify(outVar))))
    } else if (asLng) {
        str2lang(paste0(ifelse(length(inpVar)  > 1, "vars(", ""), paste(inpVar, collapse = ", "), ifelse(length(inpVar) > 1, ")", "")))
    } else {
        as.symbol(paste0(ifelse(length(inpVar) > 1, "vars(", ""), paste(inpVar, collapse = ", "), ifelse(length(inpVar) > 1, ")", "")))
    }
}

fixVar <- function(vecVar = c(), crrSPS = c(), allVar = "") {
# the function checks, and - if necessary - automatically fixes a vector of variables

    selVar <- which(! grepl("^BY$|^WITH$|^TO$|^ALL$", vecVar));
    for (j in seq_along(selVar)) {
        # (1) concatenate variables that were separated by a line feed
        if (!any(grepl(paste0("^", vecVar[selVar[j]], "$"), allVar, ignore.case = TRUE))) {
            if (j < length(selVar) && any(grepl(paste0("^", vecVar[selVar[j + 0]], vecVar[selVar[j + 1]], "$"), allVar, ignore.case = TRUE))) {
                vecVar[selVar[j + 1]] <- paste0(vecVar[selVar[j + 0]], vecVar[selVar[j + 1]]);
                vecVar[selVar[j + 0]] <- "";
                # skip to the next variable
                next
            }
            if (j > 1              && any(grepl(paste0("^", vecVar[selVar[j - 1]], vecVar[selVar[j + 0]], "$"), allVar, ignore.case = TRUE))) {
                vecVar[selVar[j + 0]] <- paste0(vecVar[selVar[j - 1]], vecVar[selVar[j + 0]]);
                vecVar[selVar[j - 1]] <- "";
            }
        }
        # (2) replace the variable name with the variable name from the column header if it is not a perfect match (i.e., if lower- / uppercase don't match up)
        if  (any(grepl(paste0("^", vecVar[selVar[j]], "$"), allVar, ignore.case = TRUE)) &&
            !any(grepl(paste0("^", vecVar[selVar[j]], "$"), allVar, ignore.case = FALSE))) {
            vecVar[selVar[j]] <- allVar[grepl(paste0("^", vecVar[selVar[j]], "$"), allVar, ignore.case = TRUE)];
        }
        # (3) if the variable is still not found, precede it with a "#" so that it can be excluded from the analysis
        if (!any(grepl(paste0("^", vecVar[selVar[j]], "$"), allVar))) {
            if (grepl("^ZRE_\\d+$|^MAH_\\d+$", vecVar[selVar[j]])) {
                warning(sprintf("Variable \"%s\" was generated as an output of an earlier analysis and is not contained in the data.\nSPSS-command: %s\n\n",
                                vecVar[selVar[j]], paste(crrSPS, collapse = " /")));
            } else {
                warning(sprintf("Variable \"%s\" not contained in the dataset.\nSPSS-command: %s\n\n",
                                vecVar[selVar[j]], paste(crrSPS, collapse = " /")));
            }
            vecVar[selVar[j]] <- paste0("#", vecVar[selVar[j]]);
        }
    }
    # remove empty entries that may have occured as consequence of (1)
    vecVar <- vecVar[vecVar != ""]

    # handle the keywords ALL and TO, and return vecVar
    if (any(grepl("^ALL$", vecVar))) {
        allVar
    } else if (any(grepl("^TO$", vecVar))) {
        for (j in grep("^TO$", vecVar)) {
            if (j < 2 || j > length(vecVar) - 1) stop(sprintf("The keyword \"TO\" is supposed to appear between variable names and not at the begin or the and of a variable list:\n%s\n\n", crrSPS));
            vecVar <- c(vecVar[1:(j - 1)], allVar[seq(grep(vecVar[j - 1], allVar) + 1, grep(vecVar[j + 1], allVar) - 1, 1)], vecVar[(j + 1):length(vecVar)]);
        }
        vecVar
    } else {
        vecVar
    }
}

chkMsV <- function(inpVar = list(), inpMsV = FALSE) {
    # the first line checks if there are any variables that were not found (preceded by "#") and returns 0 if there weren't any
    # the second line checks for whether analyses should be run even though there are missing variables (inpMsV)
    # if this flag is not set, 2 is returned; 2 is also returned if the flag is set but if there is not at least one variable per
    # category (dep. / indep.-categ. / indep. - continuos) left (if there wasn't any variable in the category, the list entry is
    # NULL and grepl("^#"... also returns FALSE; 1 is therefore only returned if the inpMsV is TRUE AND there are variables left
    # to be included in an analyses
    ifelse(!any(unlist(lapply(inpVar, function(x) any(grepl("^#", x))))), 0,
        ifelse(inpMsV && all(unlist(lapply(inpVar, function(x) is.null(x) || !all(grepl("^#", x))))), 1, 2));
}

chkFlt <- function(crrFlt = "", data = data.frame()) {
    # the comparison operator in SPSS is "=" vs. "==" in R
    crrFlt <- gsub("\\s*OR\\s*", " | ", gsub("\\s*AND\\s*", " & ", gsub("> ==", ">=", gsub("< ==", "<=", gsub(" ==  == ", " == ", gsub("\\s*=\\s*", " == ", crrFlt))))));
    fltSpl <- strSpl(crrFlt, "&|\\|");
    for (j in seq_along(fltSpl)) {
        crrSpl <- unlist(lapply(lapply(strsplit(fltSpl[[j]], "=|>|<"), trimws), function(x) x[x != ""]));
        crrVar <- fixVar(trimws(gsub("^[[|]]]$", "", gsub("data\\$", "^data", gsub("", "", crrSpl[1])))), sprintf("Filtering with the condition: %s", crrFlt), names(data));
# TO-DO: handle variable not in the data - return crrFlt = ""?
        # prevents that the same variable is processed twice
        if (j > 1 && grepl(crrVar, fltSpl[seq_len(j - 1)])) next
        if (is.logical(data[[crrVar]])) {
            # if the variable is logical, it can be used as it is (typically, this only occurs for variables
            # generated within sps2jmv.R) → do nothing in such case
        } else if (is.factor(data[[crrVar]])) {
            if (length(crrSpl) == 1) {
                # if the variable is a factor, SPSS permits 0 as the smallest value whereas R starts counting with 0
                # therefore the value of the lowest level is chosen (which equates to the level which is "0" in SPSS)
                crrFlt <- gsub(crrVar, paste0(crrVar, " != '", levels(data[[crrVar]])[1], "'"), crrFlt, ignore.case = TRUE);
            } else if (!grepl("'|\"", crrSpl[2])) {
                # if a value label is assigned (making the variable a factor in R), it is assumed that the same logic
                # as described above applies (i.e., 0 becoming 1)
                crrFlt <- gsub(fltSpl[[j]], gsub(crrSpl[2], paste0("\"", levels(data[[crrVar]])[as.integer(crrSpl[2]) + 1], "\""), fltSpl[[j]]), crrFlt);
            }
        } else if (is.numeric(data[[crrVar]])) {
            if (length(crrSpl) == 1 && any(data[[crrVar]] == 0)) {
                # if the variable is numeric and if there exists 0 then all larger values can be selected: [FILTER] > 0
                crrFlt <- gsub(crrVar, paste0(crrVar, " > 0"), crrFlt, ignore.case = TRUE);
            }
        } else if (is.character(data[[crrVar]]) && length(crrSpl) == 2 && any(data[[crrVar]] == gsub("\"|'", "", crrSpl[2]))) {
            # the condition above demonstrates that a search term is present (length...) and that cases can be selected using this term (any...)
        } else {
            stop(sprintf("FILTER – not implemented yet for this class of variable - %s", crrVar));
        }
        crrFlt <- fmtClc(crrFlt, c(), crrVar);
    }

    crrFlt
}

argCI  <- function(crrSPS = c(), ci4ES = FALSE, ci4Std = FALSE, ci4EMM = FALSE) {
# SPSS likes inconsistent: with EXAMINE it is CINTERVAL and a percent value (multiply by 1), with T-TEST it is CRITERIA=CI and a decimal value (0 - 1; multiply by 100),
#                          with NPTEST it is CRITERIA ... CILEVEL
# the c()-vector is a safety measure: if no value for the CI is given (empty string), as.double results in NA, which is the omitted (moving the 95 which is the second element in the vector
#                                     to the first place, extracted with [1]
    crrCIW <- na.omit(c(as.single(strSpl(gsub("^.*?CI\\(|^.*?CILEVEL\\s*=|^.*?CIN\\(|^CINTERVAL|\\).*$", "",
                                        crrSPS[grepl("^CINTERVAL|^CRITERIA|^CI\\s+|^METHOD.*?CI|^STATISTICS", crrSPS)]), "\\s+")[1])
                       * ifelse(any(grepl("^CRITERIA=CI", crrSPS)), 100, 1), 95))[1];
    outCI <- pairlist(ci = TRUE, ciWidth = crrCIW)
    if (ci4ES)  outCI <- c(outCI, pairlist(ciES     = TRUE, ciWidthES     = crrCIW))
    if (ci4Std) outCI <- c(outCI, pairlist(ciStdEst = TRUE, ciWidthStdEst = crrCIW))
    if (ci4EMM) outCI <- c(outCI, pairlist(ciEmm    = TRUE, ciWidthEmm    = crrCIW))

    outCI
}

argCon <- function(crrSPS = c()) {
    lneCon <- grep("^CONTRAST", crrSPS);
    lstCon <- vector(mode = "list", length = length(lneCon));
    for (j in seq_along(lneCon)) {
        splCon <- strSpl(gsub("^CONTRAST\\(", "", crrSPS[[lneCon[j]]]), "\\)\\s*=");
        lstCon[[j]] <- pairlist(var = splCon[1], type = gsub("special", "deviation", tolower(strSpl(splCon[2], "\\(")[1])));
    }

    pairlist(contrasts = lstCon)
}

argEmm <- function(crrSPS = c()) {
    lstEmm <- pairlist(emMeans      = fmtTrm(strSpl(strSpl(crrSPS[grepl("^PLOT\\s*=", crrSPS)], "\\(|\\)")[2], "\\s+")),
                       emmPlots     = TRUE,
                       emmPlotError = tolower(na.omit(c(gsub("NO", "NONE", strSpl(strSpl(crrSPS[grepl("^PLOT\\s*=", crrSPS)], "ERRORBAR=")[2], "\\(|\\s+")[1]), "CI"))[[1]]),
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
    excPHT <- c("snk", "btukey", "duncan", "dunnett", "dunnettl", "dunnettr", "lsd", "sidak", "gt2", "gabriel", "fregw", "qregw", "t2", "t3", "c", "waller");
    defPHT <- "tukey";

    if (grepl("^jmv::anovaRM$|jmv::ANOVA", crrFnc)) {
        excPHT <- c(excPHT, "gamesHowell");
    } else if (grepl("^jmv::anovaOneW$",   crrFnc)) {
        excPHT <- c(excPHT, "scheffe", "bonf", "holm");
    } else {
        stop(sprintf("argPHT: Not implemented for \"%s\" (or parameter crrFnc not given).", crrFnc));
    }

    lnePHT <- getLst(crrSPS, c("^POSTHOC\\s*=", "ALPHA\\(.*?\\)"), FALSE);
    effPHT <- ifelse(grepl("^jmv::anovaRM$|jmv::ANOVA", crrFnc), strSpl(lnePHT, "\\(")[1], "");
    tstPHT <- gsub(paste0("^", paste(excPHT, collapse = "$|^"), "$"), "",
             strSpl(gsub("gh", "gamesHowell", gsub("bonferroni", "bonf holm", tolower(gsub("\\s*\\(|\\)$|\\(\\S+\\)", "", gsub(effPHT, "", lnePHT))))), "\\s+"))

    if (grepl("^jmv::anovaRM$|jmv::ANOVA", crrFnc)) {
        pairlist(postHoc     = as.list(strSpl(effPHT, "\\s+")),
                 postHocCorr = as.list(unique(c(tstPHT[tstPHT != ""], defPHT))))
    } else if (grepl("^jmv::anovaOneW$",   crrFnc)) {
        pairlist(phMethod    =         unique(c(tstPHT[tstPHT != ""], defPHT))[1])
    }
}

argSS  <- function(crrSPS = c(), crrFnc = "") {
    strSS <- getLst(crrSPS, c("^METHOD\\s*=", "SSTYPE\\(", "\\)"))

    # SS type 4 is currently not implemented in any of the jmv-ANOVAs, anovaRM doesn't support "1"
    ifelse(is.null(strSS) || strSS == "4" || (strSS == "1" && grepl("^jmv::anovaRM$", crrFnc)), "3", strSS)
}

argTls <- function(crrSPS = c()) {
    # if PRECENTILES or NTILES are requested, they to be switched on (set to TRUE): pc vs pcEqGr, the second argument is then either pcValues (for percentiles) or pcNEqGr (for ntiles)
    # SPSS likes inconsistent: the format of PERCENTILES in EXAMINE and FREQUENCIES is different: /PERCENTILES(10,50,90)=EMPIRICAL vs /PERCENTILES=value thus the many sub()-functions
    c(ifelse(grepl("^PERCENTILES", crrSPS), pairlist(pc     = TRUE, pcValues = sub(" ", ",", sub("PERCENTILES\\s?=", "", sub("\\).*", "", sub(".*\\(", "", crrSPS[grepl("^PERCENTILES", crrSPS)]))))),
                                            pairlist(pcEqGr = TRUE, pcNEqGr  = as.integer(sub("NTILES\\s?=", "",                                           crrSPS[grepl("^NTILES",      crrSPS)])))))
}


strSpl <- function(inpStr = "", dlmSpl = "") {
    trimws(unlist(strsplit(trimws(inpStr), dlmSpl)))
}

xfmVar <- function(data = data.frame(), inpFnc = "", inpVar = list(), inpCls = c(), crrLne = NA, crrCmd = "") {
    # most analyses require numeric / continuous dependent variables; some (e.g., "descriptives") take either (-> "") and some (e.g., all "logReg...") a factor / categorical dependent var.
    tgtCls <- c(ifelse(grepl("logReg", inpFnc), "factor", ifelse(grepl("descriptives|contTables", inpFnc), "", "numeric")), "factor", "numeric");
    # inpVar: [1] dependent - depends on the analysis; [2] independent, categorical / factorial; [3] independent, numeric / continuous
    # inpCls: variable classes at load time
    for (j in seq_along(inpVar)) {
        if (tgtCls[j] == "" || length(inpVar[[j]]) == 0) next
        for (k in seq_along(inpVar[[j]])) {
            if (tgtCls[j] != class(data[[inpVar[[j]][k]]])) {
                if (class(data[[inpVar[[j]][k]]]) != inpCls[[inpVar[[j]][k]]]) {
                    stop(sprintf("Class of the variable \"%s\" (currently \"%s\") was already changed once (from \"%s\"), whereas now a change to \"%s\" is requested.",
                                 inpVar[[j]][k], class(data[[inpVar[[j]][k]]]), inpCls[[inpVar[[j]][k]]], tgtCls[j]));
                }
                if        (tgtCls[j] == "numeric") {
                    if        (class(data[[inpVar[[j]][k]]]) == "character") {
                        stop(sprintf("Variable \"%s\" is about to be transformed from character to numeric, double-check whether this is intended (l. %d):\n%s\n\n",
                                     inpVar[[j]][k], crrLne, crrCmd));
                    } else if (class(data[[inpVar[[j]][k]]]) == "factor") {
                        stop(sprintf("Variable \"%s\" is about to be transformed from factor to numeric, double-check whether this is intended (l. %d):\n%s\n\n",
                                     inpVar[[j]][k], crrLne, crrCmd));
                    } else {
                        stop(sprintf("Variable \"%s\" - unexpected class \"%s\", double-check whether this is intended (l. %d):\n%s\n\n",
                                     inpVar[[j]][k], class(inpVar[[j]][k]), crrLne, crrCmd));
                    }
                    data[[inpVar[[j]][k]]] <- numeric(data[[inpVar[[j]][k]]]);
                } else if (tgtCls[j] == "factor") {
                    if        (class(data[[inpVar[[j]][k]]]) == "character") {
                        data[[inpVar[[j]][k]]] <- factor(trimws(data[[inpVar[[j]][k]]]), exclude = "");
                    } else if (class(data[[inpVar[[j]][k]]]) == "numeric") {
                        if ((length(unique(data[[inpVar[[j]][k]]])) / length(data[[inpVar[[j]][k]]])) > 0.1) {
                            stop(sprintf("Variable \"%s\" is about to be transformed from numeric to factor, double-check whether this is intended (l. %d):\n%s\n\n",
                                         inpVar[[j]][k], crrLne, crrCmd));
                        }
                        data[[inpVar[[j]][k]]] <- factor(trimws(data[[inpVar[[j]][k]]]), exclude = NA);
                    } else {
                        stop(sprintf("Variable \"%s\" - unexpected class \"%s\", double-check whether this is intended (l. %d):\n%s\n\n",
                                     inpVar[[j]][k], class(inpVar[[j]][k]), crrLne, crrCmd));
                    }
                }
            }
        }
    }

    data
}

clcRcd <- function(crrSPS = c(), data = data.frame(), crrFlt = "") {
    rcdSpl <- c(regexpr("\\(", crrSPS)[[1]], max(gregexpr("\\)", crrSPS)[[1]]));
    rcdLst <- strsplit(strSpl(substr(crrSPS, rcdSpl[1] + 1, rcdSpl[2] - 1), "\\)\\s*\\("), "=");
    rcdVrO <- fixVar(strSpl(trimws(gsub("^RECODE", "",  substr(crrSPS, 1, rcdSpl[1] - 1))), "\\s+"), crrSPS, names(data));
    rcdVrT <-        strSpl(trimws(gsub("^\\s*INTO", "", substr(crrSPS, rcdSpl[2] + 1, nchar(crrSPS)))), "\\s+");
# TO-DO: handle variable not in the data - issue warning and remove the variable from rcdVrO

    if (identical(rcdVrT, character(0))) rcdVrT <- rcdVrO; # if the target variable is empty, recode into the original variable
    if (length(rcdVrO) != length(rcdVrT)) stop(sprintf("The number of original and target variables have to macht up:\n%s\n\n", crrSPS));

    rcdIsC <- function(x) ifelse(grepl("ELSE|SYSMIS", x), NA, grepl("'|\"", x));
    rcdTmp <- rep(list(rep(ifelse(rcdIsC(rcdLst[[1]][2]), ifelse(any(grepl("ELSE", rcdLst)),            gsub("'|\"", "", rcdLst[[grep("ELSE", rcdLst)]][[2]]), " "),
                                                          ifelse(any(grepl("ELSE", rcdLst)), suppressWarnings(as.numeric(rcdLst[[grep("ELSE", rcdLst)]][[2]])), 0)), dim(data)[1])), length(rcdVrO));
    for (j in seq_along(rcdLst)) {
        if (grepl("ELSE", rcdLst[[j]][1])) next
        rcdRpl <- ifelse(rcdIsC(rcdLst[[j]][2]), gsub("'|\"", "", rcdLst[[j]][2]), as.numeric(rcdLst[[j]][2]));
        if (grepl("character", class(data[[rcdVrO[1]]]))) {
            rcdGrp <- gsub("^\"|\"$", "", gsub("\"\\s*,\\s*\"", "|", gsub("'", "\"", rcdLst[[j]][1])));
            for (k in seq_along(rcdVrO)) {
                chkRcd(class(data[[rcdVrO[k]]]), class(rcdTmp[[k]]), rcdIsC(rcdLst[[j]][1]), rcdIsC(rcdLst[[j]][2]), rcdVrO[k], rcdVrT[k], crrSPS);
                rcdTmp[[k]] <- do_Rcd(rcdTmp[[k]], grepl(rcdGrp, data[[rcdVrO[k]]]), rcdRpl);

            }
        } else if (grepl("numeric|integer", class(data[[rcdVrO[1]]]))) {
            rcdSeq <- strSpl(rcdLst[[j]][1], ",|\\s+");
            if (any(grepl("THRU", rcdSeq))) {
                rcdSeq <- c(paste0(" >= ", rcdSeq[grep("THRU", rcdSeq) - 1]), paste0(" <= ", rcdSeq[grep("THRU", rcdSeq) + 1]));
                rcdClp <- " & ";
            } else {
                rcdSeq <- paste0(" == ", rcdSeq);
                rcdClp <- " | ";
            }
            for (k in seq_along(rcdVrO)) {
                chkRcd(class(data[[rcdVrO[k]]]), class(rcdTmp[[k]]), rcdIsC(rcdLst[[j]][1]), rcdIsC(rcdLst[[j]][2]), rcdVrO[k], rcdVrT[k], crrSPS);
                rcdTmp[[k]] <- do_Rcd(rcdTmp[[k]], eval(parse(text = paste(paste0("data[[\"", rcdVrO[k], "\"]]", rcdSeq), collapse = rcdClp))), rcdRpl);
            }
        } else {
           stop(sprintf("RECODE: Unsupported type of original column \"%s\" - \"%s\".", rcdVrO[k], class(data[[rcdVrO[k]]])));
        }
    }

    # filter / select cases and transfer values from the temporary variable (rcdTmp)
    rcdFlt <- eval(parse(text = chkFlt(crrFlt, data)));
    for (j in seq_along(rcdVrT)) {
        if (!any(grepl(rcdVrT[j], names(data)))) data[[rcdVrT[j]]] <- NA;
        data[[rcdVrT[j]]][rcdFlt] <- rcdTmp[[j]][rcdFlt];
    }

    data
}

chkRcd <- function(clsVrO = "", clsVrT = "", isCrpO = FALSE, isCrpT = FALSE, nmeClO = "", nmeClT = "", crrSPS = "") {
    if        (grepl("character",       clsVrO) && !is.na(isCrpO) && !isCrpO) {
        stop(sprintf("If the original data column (\"%\") is defined as string / character, the recode original terms also have to be characters:\n%s\n\n", nmeClO, crrSPS));
    } else if (grepl("numeric|integer", clsVrO) && !is.na(isCrpO) &&  isCrpO) {
        stop(sprintf("If the original data column (\"%\") is defined as numeric, the recode original terms also have to be numeric:\n%s\n\n",               nmeClO, crrSPS));
    } else if (grepl("character",       clsVrT) && !is.na(isCrpT) && !isCrpT) {
        stop(sprintf("If the target data column (\"%\") is defined as string / character, the recode target terms also have to be characters:\n%s\n\n",     nmeClT, crrSPS));
    } else if (grepl("numeric|integer", clsVrT) && !is.na(isCrpT) &&  isCrpT) {
        stop(sprintf("If the target data column (\"%\") is defined as numeric, the recode target terms also have to be numeric:\n%s\n\n",                   nmeClT, crrSPS));
    }
}

do_Rcd <- function(rcdCrC = c(), rcdSel = c(), rcdRpl = NULL) {
    if (is.character(rcdRpl)) {
        rcdCrC[rcdSel] <- rcdRpl;
    } else {
        rcdNA <- is.na(rcdCrC);
        # if cell contains NA, assign a value, otherwise add to the existing value
        rcdCrC[rcdSel &  rcdNA] <- rcdRpl;
        rcdCrC[rcdSel & !rcdNA] <- rcdCrC[rcdSel & !rcdNA] + rcdRpl;
    }
    rcdCrC
}

clcCmp <- function(crrSPS = c(), data = data.frame(), crrFlt = "") {
    # initialize calculation commands
    cmpJMV <- gsub(",\\s+", ", ", gsub(",", ", ", gsub("\\.$", "", paste0(strSpl(crrSPS, "=")[-1], collapse = "= "))));
    cmpRpR <- c();
    cmpVld <- TRUE;

    # extract target variable (cmprT) and variables in the formula (cmprT; incl. removing further arguments, e.g., numbers)
    cmpVrT <- strSpl(gsub("^COMPUTE", "", crrSPS), "=")[1];
    cmpVrF <- strSpl(gsub("\\).*$", "", gsub("^\\(", "", gsub("^[A-z\\.]+\\(", "", gsub("[A-z]+\\d+\\(", "", strSpl(cmpJMV, "\\+|-\\s+|\\*|/|&|\\||!\\s|<=|>=|!="))))), "\\s*,\\s*");
    cmpVrF <- cmpVrF[!grepl("^\\d+$|^-\\d+$|^\\d+\\.\\d+$|^-\\d+\\.\\d+$", cmpVrF)];
    # check whether all variables are contained in the data
    cmpVrF <- fixVar(cmpVrF, crrSPS, names(data));
    if (any(any(grepl("^#", cmpVrF)))) stop(sprintf("Variable \"%s\" is not contained in the data, formula can't be calculated:\n \"%s\"\n\n", cmpVrF[grepl("^#", cmpVrF)], crrSPS));

    # ABS(number): Returns the absolute value of a number.
    if (grepl("ABS\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("ABS\\(", "ABS(", cmpJMV, ignore.case = TRUE);
        cmpRpR <- c(cmpRpR, "ABS\\(", "abs(");
    }
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
    if (grepl("LG10\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("LG10\\(", "LOG10(", cmpJMV, ignore.case = TRUE);
        cmpRpR <- c(cmpRpR, "LOG10\\(", "log10(");
    }
    # MATCH(value, value 1, value 2, …): The index of value in the provided values.
    # MAX(variable): Returns the largest value of a set of numbers.
    # MAXABSIQR( variable 1, variable 2, … ): Max. absolute IQR-value (i.e., how far outside the box an individual datapoint is in terms of IQR - Q1/Q3-distance)
    # MAXABSZ(variable 1, variable 2, …, group_by=0): Max. absolute z-value / normalized value.
    # MEAN(number 1, number 2, …, ignore_missing=0, min_valid=0): Returns the mean of a set of numbers.
    if (grepl("MEAN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("MEAN\\(", "MEAN(", cmpJMV, ignore.case = TRUE);
        cmpRpR <- c(cmpRpR, "MEAN\\(", "rowMeans(");
    }
    # MIN(variable): Returns the smallest value of a set of numbers.
    # NORM(mean, sd): Draws samples from a normal (Gaussian) distribution.
    # NOT(value): Inverts the value.
    # OFFSET(variable, integer): Offsets the values up or down.
    # RANK(variable): Ranks each value
    # ROUND(variable, digits=0): Rounds each value
    if (grepl("RND\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("RND\\(", "ROUND(", cmpJMV, ignore.case = TRUE);
        cmpRpR <- c(cmpRpR, "ROUND\\(", "round(");
    }
    # ROW(NO ARGUMENTS): Returns the row numbers.
    # SAMPLE(variable, n, otherwise=NA): Draws a sample of n from the variable. i.e. SAMPLE(var, 20), i.e. SAMPLE(1, 20), i.e. SAMPLE(\"training\", 20, \"test\")
    # SCALE(variable, group_by=0): Returns the normalized values of a set of numbers.
    # SPLIT(variable, sep=",", piece): Splits text into pieces based on a separator. piece specifies the desired piece by index.
    # SQRT(number): Returns the square root of a number.
    # STDEV(number 1, number 2, …, ignore_missing=0): Returns the standard deviation of a set of numbers.
    # SUM(number 1, number 2, …, ignore_missing=0, min_valid=0): Returns the sum of a set of numbers.
    if (grepl("SUM\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("SUM\\(", "SUM(", cmpJMV, ignore.case = TRUE);
        cmpRpR <- c(cmpRpR, "SUM\\(", "rowSums(");
    }
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

    # there is a couple of SPSS-functions / -operations that won't be implemented
    if (grepl("IDF.NORMAL\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c("IDF.NORMAL\\(", "qnorm(");
        cmpVld <- FALSE;
    }

    # not any function used, just arithmetic, relational or boolean operators
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-arithmetic-operations
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-relational-operators
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-logical-operators
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-not-logical-operator
    # add handling for possible transformations - EQ, etc. possibly AND, OR, NOT
    # cmpJMV <- gsub("", "", cmpJMV);
    # cmpRpR <- c(cmpRpR, "", "");

    if (length(cmpRpR) ==  0) {
        stop();
    }

    # add a data column (with the name of cmpVrT) and assign it to a temporary variable (which has the correct number of lines so that the filter is working)
    if (!any(grepl(cmpVrT, names(data)))) data[[cmpVrT]] <- NA;
    cmpTmp <- data[[cmpVrT]];
    # as long as no functions are used, the R-function is just cmpJMV with the variables preceded by "data$"; otherwise, function-keywords have to be replaced
    cmpTmp <- eval(parse(text = fmtClc(cmpJMV, cmpRpR, cmpVrF)));
    # calculate the filter and apply it to the data
    cmpFlt <- eval(parse(text = chkFlt(crrFlt, data)));
    data[[cmpVrT]][cmpFlt] <- cmpTmp[cmpFlt];

    # if the command doesn't contain any commands, not available in jamovi, assign the required attributes to mark the variable as "Computed variable" in jamovi
    if (cmpVld) {
        attr(data[[cmpVrT]], "columnType")     <- "Computed";
        attr(data[[cmpVrT]], "formula")        <- cmpJMV;
        attr(data[[cmpVrT]], "formulaMessage") <- "";
    }

    data
}

# =====================================================================================================================================================================================================
