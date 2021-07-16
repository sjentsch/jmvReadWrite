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
        dtaFrm = suppressWarnings(foreign::read.spss(file = fleSAV, as.data.frame=TRUE));
    } else {
        stop('sps2jmv requires an SPSS-data-file. This can be either provided by lstSPS containing an attribute \"datafile\" or via the parameter fleSAV. This should be a character vector containing the position of the SPSS-data-file (.sav; incl. path).');
    }
    if (length(lstSPS) == 0) {
        stop('sps2jmv requires either a list [lstSPS] containing one SPSS-command (each as character vector) per list entry, or a parameter [fleSPS] pointing to a SPSS-syntax-file (.sps; incl. path).') 
    }

    lstJMV = list();
    for (inpCmd in lstSPS) {
       
        # General: Split, handle generic attributes (e.g., VAR1 TO VARx) ======
        inpCmd = unist(strsplit(inpCmd, ' /'));

        # Modifications to the data set =======================================

        # ADD VALUE LABELS ----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-add-value-labels
        # (adds new value labels but keeps existing ones, see also VALUE LABELS)
        if      (grepl('^ADD VALUE LABELS', inpCmd[1])) {
            print('ADD VALUE LABELS');
        }
        
        # ALTER TYPE ----------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-alter-type
        else if (grepl('^ALTER TYPE', inpCmd[1])) {
            print('ALTER TYPE');
        }
        
        # COMPUTE -------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-compute
        else if (grepl('^COMPUTE', inpCmd[1])) {
            print('COMPUTE');
        }

        # FILTER --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-filter
        else if (grepl('^FILTER', inpCmd[1])) {
            print('FILTER');
        }

        # FORMATS? ------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-formats
        else if (grepl('^FORMATS', inpCmd[1])) {
            print('FORMATS');
        }

        # NUMERIC -------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-numeric
        else if (grepl('^NUMERIC', inpCmd[1])) {
            print('NUMERIC');
        }

        # RANK ----------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-rank
        else if (grepl('^RANK', inpCmd[1])) {
            print('RANK');
        }

        # RECODE --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-recode
        else if (grepl('^RECODE', inpCmd[1])) {
            print('RECODE');
        }

        # RENAME VARIABLES ----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-rename-variables
        else if (grepl('^RENAME VARIABLES', inpCmd[1])) {
            print('RENAME VARIABLES');
        }

        # SAMPLE --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sample
        else if (grepl('^SAMPLE', inpCmd[1])) {
            print('SAMPLE');
        }

        # SORT CASES ----------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sort-cases
        else if (grepl('^SORT CASES', inpCmd[1])) {
            print('SORT CASES');
        }

        # SORT VARIABLES ------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-sort-variables
        else if (grepl('^SORT VARIABLES', inpCmd[1])) {
            print('SORT VARIABLES');
        }

        # STRING --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-string
        else if (grepl('^STRING', inpCmd[1])) {
            print('STRING');
        }

        # VALUE LABELS --------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-value-labels
        else if (grepl('^VALUE LABELS', inpCmd[1])) {
            print('VALUE LABELS');
        }
   
        # VARIABLE ATTRIBUTE --------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-variable-attribute
        else if (grepl('^VARIABLE ATTRIBUTE', inpCmd[1])) {
            print('VARIABLE ATTRIBUTE');
        }

        # VARIABLE LABELS -----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-variable-labels
        else if (grepl('^VARIABLE LABELS', inpCmd[1])) {
            print('VARIABLE LABELS');
        }

        # VARIABLE LEVEL ------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-variable-level
        else if (grepl('^VARIABLE LEVEL', inpCmd[1])) {
            print('VARIABLE LEVEL');
        }


        # Comments ============================================================

        # COMMENT -------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-comment
        else if (grepl('^COMMENT', inpCmd[1])) {
            print('COMMENT');
        }


        # Exploration – Descriptives ==========================================
        # https://www.jamovi.org/jmv/descriptives.html

        # CTABLES -------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-ctables
        else if (grepl('^CTABLES', inpCmd[1])) {
            print('CTABLES');
            outCmd = 'jmv::descriptives(data=data, '
        }

        # DESCRIPTIVES --------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-descriptives
        else if (grepl('^DESCRIPTIVES', inpCmd[1])) {
            print('DESCRIPTIVES');
            outCmd = 'jmv::descriptives(data=data, '
        }


        # EXAMINE -------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-examine
        else if (grepl('^EXAMINE', inpCmd[1])) {
            print('EXAMINE');
            outCmd = 'jmv::descriptives(data=data, '

#EXAMINE VARIABLES=attdrug phyheal menheal psydrug BY emplmnt religion /PLOT BOXPLOT NPPLOT /COMPARE GROUPS /STATISTICS DESCRIPTIVES /CINTERVAL 95 /MISSING LISTWISE /NOTOTAL.
#EXAMINE VARIABLES=varlist [[BY varlist] [varname BY varname]]
# 
# [/COMPARE={GROUPS** }] 
#           {VARIABLES}
# 
# [/{TOTAL**}]
#   {NOTOTAL}

# [/ID={case number**}] 
#      {varname      }
# 
# [/PERCENTILES [{NONE                   }]]
#                {({5,10,25,50,75,90,95})}=[{HAVERAGE  }]  
#                  {value list         }    {WAVERAGE  }
#                                           {ROUND     }
#                                           {AEMPIRICAL}
#                                           {EMPIRICAL }
# 
# [/PLOT=[STEMLEAF**] [BOXPLOT**] [NPPLOT] [SPREADLEVEL(n)] [HISTOGRAM]]
# 
#        [{ALL   }]
#         {NONE}
# 
# [/STATISTICS=[DESCRIPTIVES**] [EXTREME({5})]] 
#                                        {n}
#              [{ALL }]
#               {NONE}
# 
# [/CINTERVAL {95**}]
#             {n   }
# 
# [/MESTIMATOR=[{NONE**}]] 
#               {ALL   } 
# 
#              [HUBER({1.339})] [ANDREW({1.34}] 
#                     {c    }           {c    }
# 
#              [HAMPEL({1.7,3.4,8.5})] 
#                      {a  ,b  ,c  }
# 
#              [TUKEY({4.685})] 
#                     {c    }
#  
#[/MISSING=[{LISTWISE**}] [{EXCLUDE**}] [{NOREPORT**}]] 
#           {PAIRWISE  }   {INCLUDE  }   {REPORT    }

#data	the data as a data frame
#vars	a vector of strings naming the variables of interest in data
#splitBy	a vector of strings naming the variables used to split vars
#freq	TRUE or FALSE (default), provide frequency tables (nominal, ordinal variables only)
#hist	TRUE or FALSE (default), provide histograms (continuous variables only)
#dens	TRUE or FALSE (default), provide density plots (continuous variables only)
#bar	TRUE or FALSE (default), provide bar plots (nominal, ordinal variables only)
#barCounts	TRUE or FALSE (default), add counts to the bar plots
#box	TRUE or FALSE (default), provide box plots (continuous variables only)
#violin	TRUE or FALSE (default), provide violin plots (continuous variables only)
#dot	TRUE or FALSE (default), provide dot plots (continuous variables only)
#dotType	
#boxMean	TRUE or FALSE (default), add mean to box plot
#qq	TRUE or FALSE (default), provide Q-Q plots (continuous variables only)
#n	TRUE (default) or FALSE, provide the sample size
#missing	TRUE (default) or FALSE, provide the number of missing values
#mean	TRUE (default) or FALSE, provide the mean
#median	TRUE (default) or FALSE, provide the median
#mode	TRUE or FALSE (default), provide the mode
#sum	TRUE or FALSE (default), provide the sum
#sd	TRUE (default) or FALSE, provide the standard deviation
#variance	TRUE or FALSE (default), provide the variance
#range	TRUE or FALSE (default), provide the range
#min	TRUE or FALSE (default), provide the minimum
#max	TRUE or FALSE (default), provide the maximum
#se	TRUE or FALSE (default), provide the standard error
#iqr	TRUE or FALSE (default), provide the interquartile range
#skew	TRUE or FALSE (default), provide the skewness
#kurt	TRUE or FALSE (default), provide the kurtosis
#sw	TRUE or FALSE (default), provide Shapiro-Wilk p-value
#pcEqGr	TRUE or FALSE (default), provide quantiles
#pcNEqGr	an integer (default: 4) specifying the number of equal groups
#pc	TRUE or FALSE (default), provide percentiles
#pcValues	a comma-sepated list (default: 25,50,75) specifying the percentiles 
            
        }

        # FREQUENCIES ---------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-frequencies
        else if (grepl('^FREQUENCIES', inpCmd[1])) {
            print('FREQUENCIES');
            outCmd = 'jmv::descriptives(data=data, '
        }

        # MEANS ---------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-means
        else if (grepl('^MEANS', inpCmd[1])) {
            print('MEANS');
            outCmd = 'jmv::descriptives(data=data, '
        }

        # NPAR TESTS - K-S ----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS K-S', inpCmd[1]) & any(grepl('K-S', inpCmd))) {
            print('NPAR TESTS - K-S');
            outCmd = 'jmv::descriptives(data=data, '
        }

        # NPTESTS - ONESAMPLE - KOLMOGOROV_SMIRNOV ----------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', inpCmd[1]) & any(grepl('ONESAMPLE', inpCmd)) & any(grepl('KOLMOGOROV_SMIRNOV', inpCmd))) {
            print('NPTESTS - ONESAMPLE - KOLMOGOROV_SMIRNOV');
            outCmd = 'jmv::descriptives(data=data, '
        }

        # SUMMARIZE -----------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-summarize
        else if (grepl('^SUMMARIZE', inpCmd[1])) {
            print('SUMMARIZE');
            outCmd = 'jmv::descriptives(data=data, '
        }


        # T-Tests – Independent Samples T-Test ================================
        # https://www.jamovi.org/jmv/ttestis.html

        # NPAR TESTS - M-W ----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', inpCmd[1]) & any(grepl('M-W', inpCmd))) {
            print('NPAR TESTS - M-W');
            outCmd = 'jmv::ttestIS(data=data, students=FALSE, welchs=FALSE, mann=TRUE,  '
        }

        # NPTESTS - INDEPENDENT - MANN_WHITNEY --------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', inpCmd[1]) & any(grepl('INDEPENDENT', inpCmd)) & any(grepl('MANN_WHITNEY', inpCmd))) {
            print('NPTESTS - INDEPENDENT - MANN_WHITNEY');
            outCmd = 'jmv::ttestIS(data=data, students=FALSE, welchs=FALSE, mann=TRUE,  '
        }

        # T-TEST (without PAIRS and TESTVAL) ----------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl('^T-TEST', inpCmd[1]) & ! any(grepl('PAIRS', inpCmd)) & ! any(grepl('TESTVAL', inpCmd))) {
            print('T-TEST - INDEPENDENT');
            outCmd = 'jmv::ttestIS(data=data, students=TRUE,  welchs=TRUE,  mann=FALSE, '
        }


        # T-Tests – Paired Samples T-Test =====================================
        # https://www.jamovi.org/jmv/ttestps.html

        # NPAR TESTS - WILCOXON - PAIRED --------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', inpCmd[1]) & any(grepl('WILCOXON', inpCmd)) & any(grepl('PAIRED', inpCmd))) {
            print('NPAR TESTS - WILCOXON - PAIRED');
            outCmd = 'jmv::ttestIS(data=data, students=FALSE, wilcoxon=TRUE,  ' 
        }

        # NPTESTS - RELATED - WILCOXON -----------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', inpCmd[1]) & any(grepl('RELATED', inpCmd)) & any(grepl('WILCOXON', inpCmd))) {
            print('NPTESTS - RELATED - WILCOXON');
            outCmd = 'jmv::ttestIS(data=data, students=FALSE, wilcoxon=TRUE,  ' 
        }

        # T-TEST (with PAIRS) -------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl('^T-TEST', inpCmd[1]) & any(grepl('PAIRS', inpCmd))) {
            print('T-TEST - Paired');
            outCmd = 'jmv::ttestIS(data=data, students=TRUE,  wilcoxon=FALSE, ' 
        }


        # T-Tests – One Sample T-Test =========================================
        # https://www.jamovi.org/jmv/ttestones.html

        # NPAR TESTS - WILCOXON - not PAIRED ----------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', inpCmd[1]) & any(grepl('WILCOXON', inpCmd)) & ! any(grepl('PAIRED', inpCmd))) {
            print('NPAR TESTS WILCOXON - not PAIRED (one sample)');
            outCmd = 'jmv::ttestOneS(data=data, students=FALSE, wilcoxon=TRUE,  '
        }

        # NPTESTS - ONESAMPLE - WILCOXON ---------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', inpCmd[1]) & any(grepl('ONESAMPLE', inpCmd)) & any(grepl('WILCOXON', inpCmd))) {
            print('NPTESTS - ONESAMPLE - WILCOXON');
            outCmd = 'jmv::ttestOneS(data=data, students=FALSE, wilcoxon=TRUE,  '
        }

        # T-TEST (with TESTVAL) -----------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-t-test
        else if (grepl('^T-TEST', inpCmd[1]) & any(grepl('TESTVAL', inpCmd))) {
            print('T-TEST - One sample');
            outCmd = 'jmv::ttestOneS(data=data, students=TRUE,  wilcoxon=FALSE, '
        }


        # ANOVAs – One-Way ANOVA ==============================================
        # https://www.jamovi.org/jmv/anovaonew.html
        
        # ONEWAY --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-oneway
        else if (grepl('^ONEWAY', inpCmd[1])) {
            print('ONEWAY');
            outCmd = 'jmv::anovaOneW(data=data, '
        }

        # UNIANOVA ------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-unianova
        else if (grepl('^UNIANOVA', inpCmd[1])) {
            print('UNIANOVA');
            outCmd = 'jmv::anovaOneW(data=data, '
        }


        # ANOVAs – ANOVA ======================================================
        # https://www.jamovi.org/jmv/anova.html

        # ANOVA ---------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-anova
        else if (grepl('^ANOVA', inpCmd[1])) {
            print('ANOVA');
            outCmd = 'jmv::ANOVA(data=data, '
        }

        # GLM: Univariate -----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-univariate
        else if (grepl('^GLM', inpCmd[1])) {
            print('GLM');
            outCmd = 'jmv::ANOVA(data=data, '
        }

        # MANOVA: Univariate --------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-univariate
        else if (grepl('^MANOVA', inpCmd[1])) {
            print('MANOVA');
            outCmd = 'jmv::ANOVA(data=data, '
        }


        # ANOVAs – Repeated Measures ANOVA ====================================
        # https://www.jamovi.org/jmv/anovarm.html

        # GLM: Repeated Measures ----------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-repeated-measures
        else if (grepl('^GLM', inpCmd[1])) {
            print('GLM');
            outCmd = 'jmv::anovaRM(data=data, '
        }

        # MANOVA: Repeated Measures -------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-repeated-measures
        else if (grepl('^MANOVA', inpCmd[1])) {
            print('MANOVA');
            outCmd = 'jmv::anovaRM(data=data, '
        }


        # ANOVAs – ANCOVA =====================================================
        # https://www.jamovi.org/jmv/ancova.html

        # GLM: Univariate -----------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-univariate
        else if (grepl('^GLM', inpCmd[1])) {
            print('GLM');
            outCmd = 'jmv::ancova(data=data, '
        }

        # MANOVA: Univariate --------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-univariate
        else if (grepl('^MANOVA', inpCmd[1])) {
            print('MANOVA');
            outCmd = 'jmv::ancova(data=data, '
        }


        # ANOVAs – MANCOVA ====================================================
        # https://www.jamovi.org/jmv/mancova.html

        # GLM: Multivariate ---------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-glm-multivariate
        else if (grepl('^GLM', inpCmd[1])) {
            print('GLM');
            outCmd = 'jmv::mancova(data=data, '
        }

        # MANOVA: Multivariate ------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-manova-multivariate
        else if (grepl('^MANOVA', inpCmd[1])) {
            print('MANOVA');
            outCmd = 'jmv::mancova(data=data, '
        }


        # ANOVAs – One-Way ANOVA (Non-parametric) =============================
        # https://www.jamovi.org/jmv/anovanp.html

        # NPAR TESTS K-W ------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', inpCmd[1]) & any(grepl('K-W', inpCmd))) {
            print('NPAR TESTS - Kruskal-Wallis');
            outCmd = 'jmv::anovaNP(data=data, '
        }

        # NPTESTS /INDEPENDENT - KRUSKAL_WALLIS -------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', inpCmd[1]) & any(grepl('INDEPENDENT', inpCmd)) & any(grepl('KRUSKAL_WALLIS', inpCmd))) {
            print('NPTESTS - Kruskal-Wallis');
            outCmd = 'jmv::anovaNP(data=data, '
        }


        # ANOVAs – Repeated Measures ANOVA (Non-parametric) ===================
        # https://www.jamovi.org/jmv/anovarmnp.html

        # NPAR TESTS FRIEDMAN -------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', inpCmd[1]) & any(grepl('FRIEDMAN', inpCmd))) {
            print('NPAR TESTS - Kruskal-Wallis');
            outCmd = 'jmv::anovaRMNP(data=data, '
        }

        # NPTESTS /RELATED - FRIEDMAN -----------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', inpCmd[1]) & any(grepl('RELATED', inpCmd)) & any(grepl('FRIEDMAN', inpCmd))) {
            print('NPTESTS - Friedman');
            outCmd = 'jmv::anovaRMNP(data=data, '
        }


        # Correlation and regression – Correlation Matrix =====================
        # https://www.jamovi.org/jmv/corrmatrix.html

        # CORRELATIONS --------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-correlations
        else if (grepl('^CORRELATIONS', inpCmd[1])) {
            print('CORRELATIONS');
            outCmd = 'jmv::corrMatrix(data=data, pearson = TRUE,  spearman = FALSE, kendall = FALSE, '
        }

        # NONPAR CORR ---------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nonpar-corr
        else if (grepl('^NONPAR CORR', inpCmd[1])) {
            print('NONPAR CORR');
            outCmd = 'jmv::corrMatrix(data=data, pearson = FALSE, spearman = TRUE,  kendall = TRUE,  '
        }


        # Correlation and regression – Partial Correlation ====================
        # https://www.jamovi.org/jmv/corrpart.html

        # PARTIAL CORR --------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-partial-corr
        else if (grepl('^PARTIAL CORR', inpCmd[1])) {
            print('PARTIAL CORR');
            outCmd = 'jmv::corrPart(data=data, '
        }


        # Correlation and regression – Linear Regression ======================
        # https://www.jamovi.org/jmv/linreg.html

        # LINEAR --------------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-linear
        else if (grepl('^LINEAR', inpCmd[1])) {
            print('LINEAR');
            outCmd = 'jmv::linReg(data=data, '
        }

        # REGRESSION ----------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-regression
        else if (grepl('^REGRESSION', inpCmd[1])) {
            print('REGRESSION');
            outCmd = 'jmv::linReg(data=data, '
        }

        # Correlation and regression – Binomial Logistic Regression ===========
        # https://www.jamovi.org/jmv/logregbin.html

        # LOGISTIC REGRESSION (outcome variable has two steps) ----------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-logistic-regression
        else if (grepl('^LOGISTIC REGRESSION', inpCmd[1])) {
            print('LOGISTIC REGRESSION - Binomial');
            outCmd = 'jmv::logRegBin(data=data, '
        }


        # Correlation and regression – Multinomial Logistic Regression ========
        # https://www.jamovi.org/jmv/logregmulti.html

        # LOGISTIC REGRESSION (outcome variable has more than two steps) ------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-logistic-regression
        else if (grepl('^LOGISTIC REGRESSION', inpCmd[1])) {
            print('LOGISTIC REGRESSION - Multinomial');
            outCmd = 'jmv::logRegMulti(data=data, '
        }


        # Correlation and regression – Ordinal Logistic Regression ============
        # https://www.jamovi.org/jmv/logregord.html

        # LOGISTIC REGRESSION (outcome variable has more than two steps and is ordered)
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-logistic-regression
        else if (grepl('^LOGISTIC REGRESSION', inpCmd[1])) {
            print('LOGISTIC REGRESSION - Ordinal');
            outCmd = 'jmv::logRegOrd(data=data, '
        }


        # Frequency analyses – Proportion Test (2 Outcomes) ===================
        # https://www.jamovi.org/jmv/proptest2.html

        # NPAR TESTS BINOMIAL -------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', inpCmd[1]) & any(grepl('BINOMIAL', inpCmd))) {
            print('NPAR TESTS - Binomial');
            outCmd = 'jmv::propTest2(data=data, '
        }

        # NPTESTS /ONESAMPLE BINOMIAL -------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', inpCmd[1]) & any(grepl('ONESAMPLE', inpCmd)) & any(grepl('BINOMIAL', inpCmd))) {
            print('NPTESTS - Binomial');
            outCmd = 'jmv::propTest2(data=data, '
        }


        # Frequency analyses – Proportion Test (N Outcomes) ===================
        # https://www.jamovi.org/jmv/proptestn.html

        # NPAR TESTS CHISQUARE ------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', inpCmd[1]) & any(grepl('CHISQUARE', inpCmd))) {
            print('NPAR TESTS - Chi-squared');
            outCmd = 'jmv::propTestN(data=data, '
        }

        # NPTESTS /ONESAMPLE CHISQUARE ----------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', inpCmd[1]) & any(grepl('ONESAMPLE', inpCmd)) & any(grepl('CHISQUARE', inpCmd))) {
            print('NPTESTS - Binomial');
            outCmd = 'jmv::propTestN(data=data, '
        }


        # Frequency analyses – Contingency Tables =============================
        # https://www.jamovi.org/jmv/conttables.html

        # CROSSTABS -----------------------------------------------------------
        else if (grepl('^CROSSTABS', inpCmd[1])) {
            print('CROSSTABS');
            outCmd = 'jmv::contTables(data=data, '
        }


        # Frequency analyses – Paired Samples Contingency Tables ==============
        # https://www.jamovi.org/jmv/conttablespaired.html

        # NPAR TESTS MCNEMAR --------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-npar-tests
        else if (grepl('^NPAR TESTS', inpCmd[1]) & any(grepl('MCNEMAR', inpCmd))) {
            print('NPAR TESTS MCNEMAR');
            outCmd = 'jmv::contTablesPaired(data=data, '
        }

        # NPTESTS /RELATED MCNEMAR ----------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-nptests
        else if (grepl('^NPTESTS', inpCmd[1]) & any(grepl('RELATED', inpCmd)) & any(grepl('MCNEMAR', inpCmd))) {
            print('NPTESTS - Binomial');
            outCmd = 'jmv::contTablesPaired(data=data, '
        }


        # Frequency analyses – Log-Linear Regression ==========================
        # https://www.jamovi.org/jmv/loglinear.html

        # LOGLINEAR -----------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-loglinear
        else if (grepl('^LOGLINEAR', inpCmd[1])) {
            print('LOGLINEAR');
            outCmd = 'jmv::logLinear(data=data, '
        }


        # Factor analysis – Reliability Analysis ==============================
        # https://www.jamovi.org/jmv/reliability.html

        # RELIABILITY ---------------------------------------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-reliability
        else if (grepl('^RELIABILITY', inpCmd[1])) {
            print('RELIABILITY');
            outCmd = 'jmv::reliability(data=data, '
        }


        # Factor analysis – Principal Component Analysis ======================
        # https://www.jamovi.org/jmv/pca.html

        # FACTOR - /EXTRACTION = PC, PA1, DEFAULT -----------------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-factor
        else if (grepl('^', inpCmd[1])) {
            print('');
            outCmd = 'jmv::pca(data=data, '
        }


        # Factor analysis – Exploratory Factor Analysis =======================
        # https://www.jamovi.org/jmv/efa.html

        # FACTOR - /EXTRACTION = PAF, PA2, ML, GLS / ULS ----------------------
        # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=reference-factor
        # PAF, PA2 → Principal axis 
        # ML → maximum likelihood 
        # CHECK: unclear whether "minimum residuals" = GLS / ULS
        else if (grepl('^', inpCmd[1])) {
            print('');
            outCmd = 'jmv::efa(data=data, '
        }

        # Factor analysis – Confirmatory Factor Analysis ======================
        # https://www.jamovi.org/jmv/cfa.html
        
        # not contained in SPSS -----------------------------------------------

        # =====================================================================
        
        # other not (yet) implemented SPSS-commands ---------------------------
        else {
            print(paste('SPSS-command', inpCmd[1], 'not (yet) implemented.'));
            outCmd = "";
        }

        # add converted command / function the the syntax list ================ 
        if (inpCmd > "") { lstJMV = c(lstJMV, outCmd); }
    }

    attr(dtaFrm, 'syntax') = lstJMV;
}

chkVar <- function(varLst = list(), dtaFrm = data.frame()) {

}
