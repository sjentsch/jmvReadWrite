compute <- function(crrCmd = c(), dtaFrm = NULL) {
    # jamovi Computed variables
    # https://raw.githubusercontent.com/jamovi/jamovi/current-dev/client/main/vareditor/formulatoolbar.js
    # on Linux: curl -s https://raw.githubusercontent.com/jamovi/jamovi/current-dev/client/main/vareditor/formulatoolbar.js | grep "^\s*descriptions" | cut -d\' -f2 | sed 's/ <i>//' | sed 's/<\/i> //' | sed 's/<\/i>,/,/' | sort
    # SPSS COMPUTE command
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-arithmetic-operations
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-relational-operators
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-logical-operators
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-not-logical-operator

    # determine the number of rows in the data frame
    nRow <- dim(dtaFrm)[1]
    # get rid of the COMPUTE-command in case SPSS-syntax is submitted
    isSPSS <- grepl("^COMPUTE", crrCmd)
    crrCmd <- trimws(gsub("\\.$", "", gsub("^COMPUTE", "", crrCmd)))
    # initialize calculation commands
    cmpJMV <- gsub(",\\s+", ", ", gsub(",", ", ", paste0(trimws(strSpl(crrCmd, "=")[-1]), collapse = " = ")))
    cmpRpR <- c()
# figure out what I intended with this variable
#   cmpClR <- TRUE
    cmpDim <- 1
    cmpVld <- TRUE

    # extract target variable (cmprT) and variables in the formula (cmprT; incl. removing further arguments, e.g., numbers)
    cmpVrT <- strSpl(crrCmd, "=")[1]
    cmpVrF <- gsub("\\).*$", "", gsub("^[A-z\\.]+\\(", "", gsub("[A-z\\.]+\\d+\\(", "", gsub("^\\(+", "", strSpl(cmpJMV, "\\+|-\\s+|\\*|/|&|\\||!\\s|<=|>=|!=")))))
    cmpVrF <- gsub(",\\s*group_by\\s*=\\s*\\w*", "", cmpVrF[is.na(suppressWarnings(as.numeric(cmpVrF)))])
    cmpJMV <- gsub(paste0("\\(", cmpVrF), "([VARNAMES]", cmpJMV)
    # check whether all variables are contained in the data
    cmpVrF <- fixVar(strSpl(cmpVrF, "\\s*,\\s*|\\s* \\s*"), crrCmd, names(dtaFrm))
    if (any(any(grepl("^#", cmpVrF)))) stop(sprintf("Variable \"%s\" is not contained in the data, formula can't be calculated:\n \"%s\"\n\n", cmpVrF[grepl("^#", cmpVrF)], crrCmd))

    # jamovi-functions ========================================================
    # ABS(number): Returns the absolute value of a number
    if (grepl("ABS\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("ABS\\(", "ABS(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "ABS\\(", "abs(")
    }
    # ABSIQR(variable): Convenience short-hand for ABS(IQR(variable))
    if (grepl("ABSIQR\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("ABSIQR\\(", "ABSIQR(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "ABSIQR\\(", "abs(iqr(", "[VARNAMES]", "[VARNAMES])")
    }
    # ABSZ(variable): Convenience short-hand for ABS(Z(variable))
    if (grepl("ABSZ\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("ABSZ\\(", "ABSZ(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "ABSZ\\(", "abs(scale(", "[VARNAMES]", "[VARNAMES])")
    }
    # BETA(alpha, beta): Draws samples from a Beta distribution.
    if (grepl("BETA\\(|RV\\.BETA\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("BETA\\(|RV\\.BETA\\(", "BETA(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "BETA\\(", paste0("rbeta(", as.character(nRow), ", "))
    }
    # BOXCOX(variable, lambda): Returns a Box Cox transformation of the variable.
    if (grepl("BOXCOX\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("BOXCOX\\(", "BOXCOX(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "BOXCOX\\(", "boxcox(")
    }
    # CEILING(variable): Rounds each value to the integer above.
    if (grepl("CEILING\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("CEILING\\(", "CEILING(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "CEILING\\(", "ceiling(")
    }
    # CONTAINS(item1, item2, item3, …, in1, in2, in3, …): Determines if any of the items appear in in1, in2, in3, ....
    #                                                     Note that most of these arguments are optional -- it is possible
    #                                                     to simply use CONTAINS(needle, haystack).
    # -> SPSS: CHAR.INDEX(haystack, needle[, divisor]) / INDEX - find needle in haystack
    # NB: currently only works for two input parameters
    if (grepl("CONTAINS\\(|CHAR\\.INDEX\\(|INDEX\\(", cmpJMV, ignore.case = TRUE)) {
        # if SPSS: haystack and needle are switched
        if (isSPSS) {
            cmpJMV <- gsub("\\(\\s*", "(", gsub("CONTAINS\\((.*)?, (.*)?\\)", "CONTAINS\\(\\2, \\1)", cmpJMV, ignore.case = TRUE))
        }
        cmpJMV <- gsub("CONTAINS\\(|CHAR\\.INDEX\\(|INDEX\\(", "CONTAINS(", cmpJMV, ignore.case = TRUE)
# to check
        cmpRpR <- c(cmpRpR, "CONTAINS\\(", "contains(")
    }
    # EXP(number): Returns the exponent for basis \u212F of a number.
    if (grepl("EXP\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("EXP\\(", "EXP(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "EXP\\(", "exp(")
    }
    # FILTER(variable, filter_expression): Filters a variable using the filter expression.
    if (grepl("FILTER\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("FILTER\\(", "FILTER(", cmpJMV, ignore.case = TRUE)
# to check
        cmpRpR <- c(cmpRpR, "FILTER\\(", "filter(")
    }
    # FLOOR(variable): Rounds each value to the integer below
    if (grepl("FLOOR\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("FLOOR\\(", "FLOOR(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "FLOOR\\(", "floor(")
    }
    # GAMMA(shape, scale): Draws samples from a Gamma distribution.
    if (grepl("GAMMA\\(|RV\\.GAMMA\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("GAMMA\\(|RV\\.GAMMA\\(", "GAMMA(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "GAMMA\\(", paste0("rgamma(", as.character(nRow), ", "))
    }
    # HLOOKUP(index, value 1, value 2, …): The value in the provided values at index.
    if (grepl("HLOOKUP\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("HLOOKUP\\(", "HLOOKUP(", cmpJMV, ignore.case = TRUE)
# to check
        cmpRpR <- c(cmpRpR, "HLOOKUP\\(", "hlookup(")
    }
    # IF(expression, value, else): If the expression resolves true, use the value, otherwise the else.
    if (grepl("IF\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("IF\\(", "IF(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "IF\\(", "ifelse(")
    }
    # IFMISS(variable, value, else): When the variable contains a missing value, use the value, otherwise the else.
    if (grepl("IFMISS\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("IFMISS\\(", "IFMISS(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "IFMISS\\(", "ifelse(is.na(", "[VARNAMES]", "[VARNAMES])")
    }
    # INT(number): Converts a number to an integer.
    if (grepl("INT\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("INT\\(", "INT(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "INT\\(", "as.integer(")
    }
    # IQR(variable): Returns a whether the variable is an outlier according to the IQR: If the value is
    # within the box of a Boxplot 0 is returned, absolute values larger than 1.5 are outside the whiskers.
    if (grepl("IQR\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("IQR\\(", "IQR(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "IQR\\(", "iqr(")
    }
    # LN(number): Returns the natural logarithm of a number.
    if (grepl("LN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("LN\\(", "LN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "LN\\(", "ln(")
    }
    # LOG10(number): Returns the base-10 logarithm of a number. [SPSS: LG10]
    if (grepl("LG10\\(|LOG10\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("LG10\\(|LOG10\\(", "LOG10(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "LOG10\\(", "log10(")
    }
    # MATCH(value, value 1, value 2, …): The index of value in the provided values.
    if (grepl("MATCH\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("MATCH\\(", "MATCH(", cmpJMV, ignore.case = TRUE)
# to check
        cmpRpR <- c(cmpRpR, "MATCH\\(", "match(")
    }
    # MAX(number 1, number 2, …): Returns the largest value of a set of numbers.
    if (grepl("MAX\\(|MAX\\.\\d+\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("MAX\\(|MAX\\.\\d+\\(", "MAX(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "MAX\\(", "max(", "[VARNAMES]", "[VARNAMES], na.rm = TRUE")
    }
    # MAXABSIQR(variable 1, variable 2, …): Max. absolute IQR-value (i.e., how far outside the box an individual datapoint is in terms of IQR - Q1/Q3-distance)
    if (grepl("MAXABSIQR\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("MAXABSIQR\\(", "MAXABSIQR(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "MAXABSIQR\\(", "abs(iqr(", "[VARNAMES]", "[VARNAMES])")
    }
    # MAXABSZ(variable 1, variable 2, …, group_by=0): Max. absolute z-value / normalized value.
    if (grepl("MAXABSZ\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("MAXABSZ\\(", "MAXABSZ(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "MAXABSZ\\(", "abs(scale(", "[VARNAMES]", "[VARNAMES])")
    }
    # MEAN(number 1, number 2, …, ignore_missing=0, min_valid=0): Returns the mean of a set of numbers.
    if (grepl("MEAN\\(|MEAN\\.\\d+\\(", cmpJMV, ignore.case = TRUE)) {
        if (isSPSS && grepl("MEAN\\.\\d+\\(", cmpJMV, ignore.case = TRUE)) {
            cmpJMV <- gsub("[VARNAMES]", paste0("[VARNAMES], min_valid = ", gsub("MEAN\\.(\\d+)\\(.*?\\)", "\\1", cmpJMV, ignore.case = TRUE)), cmpJMV)
        }
        cmpJMV <- gsub("MEAN\\(|MEAN\\.\\d+\\(", "MEAN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "MEAN\\(", "mean(")
    }
# to implement
    # MEDIAN(variable 1, variable 2, …)
    if (grepl("MEDIAN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("MEDIAN\\(", "MEDIAN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "MEDIAN\\(", "median(")
    }
    # MIN(number 1, number 2, …): Returns the smallest value of a set of numbers.
    if (grepl("MIN\\(|MIN\\.\\d+\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("MIN\\(|MIN\\.\\d+\\(", "MIN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "MIN\\(", "min(", "[VARNAMES]", "[VARNAMES], na.rm = TRUE")
    }
    # MOD()
    if (grepl("MOD\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("MOD\\((\\w*),\\s*(\\w*)\\)", "\\1 % \\2", cmpJMV)
        cmpRpR <- c(cmpRpR, "%", "%%")
    }
    # NORM(mean, sd): Draws samples from a normal (Gaussian) distribution. [SPSS: RV.NORMAL, NORMAL]
    if (grepl("NORM\\(|RV.NORMAL\\(|NORMAL\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("NORMAL\\(", "NORM(0, ", gsub("NORM\\(|RV.NORMAL\\(", "NORM(", cmpJMV, ignore.case = TRUE), ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "NORM\\(", paste0("rnorm(", as.character(nRow), ", "))
    }
    # NOT(value): Inverts the value.
    if (grepl("NOT\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("NOT\\(", "NOT(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "NOT\\(", "!", "[VARNAMES])", "[VARNAMES]")
    }
# implement negative offset in jamovi
    # OFFSET(variable, integer): Offsets the values up or down [SPSS: LAG(variable[, n])]
    if (grepl("OFFSET\\(|LAG\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("OFFSET\\(|LAG\\(", "OFFSET(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "OFFSET\\(", "offset(")
    }
    # RANK(variable): Ranks each value
    if (grepl("RANK\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("RANK\\(", "RANK(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "RANK\\(", "rank(")
    }
    # ROUND(variable, digits=0): Rounds each value [SPSS: RND(numexpr[,mult,fuzzbits])]
    if (grepl("RND\\(|ROUND\\(", cmpJMV, ignore.case = TRUE)) {
# to-do: SPSS fuzzbits to digits
        cmpJMV <- gsub("RND\\(|ROUND\\(", "ROUND(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "ROUND\\(", "round(")
    }
    # ROW(NO ARGUMENTS): Returns the row numbers.
    if (grepl("ROW\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("ROW\\(", "ROW(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "ROW\\(\\)", paste0("seq(", as.character(nRow), ")"))
    }
    # SAMPLE(variable, n, otherwise=NA): Draws a sample of n from the variable. i.e. SAMPLE(var, 20), i.e. SAMPLE(1, 20), i.e. SAMPLE(\"training\", 20, \"test\")
    if (grepl("SAMPLE\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("SAMPLE\\(", "SAMPLE(", cmpJMV, ignore.case = TRUE)
# to check
        cmpRpR <- c(cmpRpR, "SAMPLE\\(", paste0("jmvSmp(", dim(data)[1], ", "))
    }
    # SCALE -> Z
    # SPLIT(variable, sep=",", piece): Splits text into pieces based on a separator. piece specifies the desired piece by index.
    if (grepl("SPLIT\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("SPLIT\\(", "SPLIT(", cmpJMV, ignore.case = TRUE)
# to check
        cmpRpR <- c(cmpRpR, "SPLIT\\(", "split(")
    }
    # SQRT(number): Returns the square root of a number.
    if (grepl("SQRT\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("SQRT\\(", "SQRT(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "SQRT\\(", "sqrt(")
    }
    # STDEV(number 1, number 2, …, ignore_missing=0): Returns the standard deviation of a set of numbers. [SPSS: SD(numexpr,numexpr[,..])]
    if (grepl("STDEV\\(|SD\\(|SD\\.\\d+\\(", cmpJMV, ignore.case = TRUE)) {
        if (isSPSS && grepl("SD\\.\\d+\\(", cmpJMV, ignore.case = TRUE)) {
            cmpJMV <- gsub("[VARNAMES]", paste0("[VARNAMES], min_valid = ", gsub("SD\\.(\\d+)\\(.*?\\)", "\\1", cmpJMV, ignore.case = TRUE)), cmpJMV)
        }
        cmpJMV <- gsub("STDEV\\(|SD\\(|SD\\.\\d+\\(", "STDEV(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "STDEV\\(", "sd(")
    }
    # SUM(number 1, number 2, …, ignore_missing=0, min_valid=0): Returns the sum of a set of numbers.
    if (grepl("SUM\\(|SUM\\.\\d+\\(", cmpJMV, ignore.case = TRUE)) {
        if (isSPSS && grepl("SUM\\.\\d+\\(", cmpJMV, ignore.case = TRUE)) {
            cmpJMV <- gsub("[VARNAMES]", paste0("[VARNAMES], min_valid = ", gsub("SUM\\.(\\d+)\\(.*?\\)", "\\1", cmpJMV, ignore.case = TRUE)), cmpJMV)
        }
        cmpJMV <- gsub("SUM\\(|SUM\\.\\d+\\(", "SUM(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "SUM\\(", "sum(")
    }
    # TEXT(number): Converts the value to text [SPSS: STRING(numexpr,format)]
    if (grepl("TEXT\\(|STRING\\(", cmpJMV, ignore.case = TRUE)) {
# SPSS: remove second argument
        cmpJMV <- gsub("TEXT\\(|STRING\\(", "TEXT(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "TEXT\\(", "as.character(")
    }
    # UNIF(low, high): Draws samples from a uniform distribution. [SPSS: RV.UNIFORM, UNIFORM]
    if (grepl("UNIF\\(|RV\\.UNIFORM\\(|UNIFORM\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("|UNIFORM\\(", "UNIF(0, ", gsub("UNIF\\(|RV\\.UNIFORM\\(", "UNIF(", cmpJMV, ignore.case = TRUE), ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "UNIF\\(", paste0("runif(", as.character(nRow), ", "))
    }
    # VALUE(text): Converts text to a number (if possible) [SPSS: NUMBER(strexpr,format)]
    if (grepl("VALUE\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VALUE\\(", "VALUE(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VALUE\\(", "as.numeric(")
    }
    # VAR(number 1, number 2, …, ignore_missing=0): Returns the variance of a set of numbers. [SPSS: VARIANCE(numexpr,numexpr[,..])]
    if (grepl("VAR\\(|VARIANCE\\(", cmpJMV, ignore.case = TRUE)) {
        if (isSPSS && grepl("VARIANCE\\.\\d+\\(", cmpJMV, ignore.case = TRUE)) {
            cmpJMV <- gsub("[VARNAMES]", paste0("[VARNAMES], min_valid = ", gsub("VARIANCE\\.(\\d+)\\(.*?\\)", "\\1", cmpJMV, ignore.case = TRUE)), cmpJMV)
        }
        cmpJMV <- gsub("VAR\\(|VARIANCE\\(", "VAR(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VAR\\(", "var(")
    }
    # VMAX(variable, group_by=0): Returns the largest value of a variable.
    if (grepl("VMAX\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VMAX\\(", "VMAX(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VMAX\\(", "max(")
        cmpDim <- 2
    }
    # VMEAN(variable, group_by=0): Returns the overall mean of a variable.
    if (grepl("VMEAN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VMEAN\\(", "VMEAN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VMEAN\\(", "mean(")
        cmpDim <- 2
    }
    # VMED(variable, group_by=0): Returns the median of a variable.
    if (grepl("VMED\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VMED\\(", "VMED(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VMED\\(", "median(")
        cmpDim <- 2
    }
    # VMIN(variable, group_by=0): Returns the smallest value of a variable.
    if (grepl("VMIN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VMIN\\(", "VMIN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VMIN\\(", "min(")
        cmpDim <- 2
    }
    # VMODE(variable, group_by=0): Returns the most common value in a variable.
    if (grepl("VMODE\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VMODE\\(", "VMODE(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VMODE\\(", "mode(")
        cmpDim <- 2
    }
    # VN(variable, group_by=0): Returns the number of cases in a variable.
    if (grepl("VN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VN\\(", "VN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VN\\(", "sum(!is.na(", "[VARNAMES]", "[VARNAMES])")
        cmpDim <- 2
    }
    # VROWS(variable, group_by=0): Returns the number of rows of a variable.
    if (grepl("VROWS\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VROWS\\(", "VROWS(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VROWS\\(", "length(")
        cmpDim <- 2
    }
    # VSE(variable, group_by=0): Returns the standard error of the mean of a variable.
    if (grepl("VSE\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VSE\\(", "VSE(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VSE\\(", "se(")
        cmpDim <- 2
    }
    # VSTDEV(variable, group_by=0): Returns the standard deviation of a variable.
    if (grepl("VSTDEV\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VSTDEV\\(", "VSTDEV(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VSTDEV\\(", "sd(")
        cmpDim <- 2
    }
    # VSUM(variable, group_by=0): Returns the overall sum of a variable.
    if (grepl("VSUM\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VSUM\\(", "VSUM(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VSUM\\(", "sum(")
        cmpDim <- 2
    }
    # VVAR(variable, group_by=0): Returns the variance of a variable.
    if (grepl("VVAR\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VVAR\\(", "VVAR(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VVAR\\(", "var(")
        cmpDim <- 2
    }
    # SCALE/Z(variable, group_by=0): Returns the normalized values of a set of numbers.
    if (grepl("SCALE\\(|Z\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("SCALE\\(", "SCALE(", gsub("Z\\(", "Z(", cmpJMV, ignore.case = TRUE), ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "SCALE\\(|Z\\(", "scale(")
        cmpDim <- 2
    }
    # SPSS-functions / -operations ============================================
    # ANY(test,value[,value,...])
    if (grepl("ANY\\(", cmpJMV)) {
        stop("ANY is not implemented.")
    }
    # ARSIN(numexpr)
    if (grepl("ARSIN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c(cmpRpR, "ARSIN\\(", "asin(")
        cmpVld <- FALSE
    }
    # ARTAN(numexpr)
    if (grepl("ARTAN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c(cmpRpR, "ARTAN\\(", "atan(")
        cmpVld <- FALSE
    }
    # CFVAR(numexpr,numexpr[,...])
    if (grepl("CFVAR\\(", cmpJMV)) {
        stop("CFVAR is not implemented.")
    }
    # CHAR.LENGTH(strexpr) / LENGTH(strexpr)
    if (grepl("CHAR\\.LENGTH\\(", cmpJMV)) {
        stop("CHAR.LENGTH, and LENGTH are not implemented.")
    }
    # CHAR.LPAD(strexpr1,length[,strexpr2])
    if (grepl("CHAR\\.LPAD(", cmpJMV)) {
        stop("CHAR.LPAD is not implemented.")
    }
    # CHAR.MBLEN(strexpr,pos)
    if (grepl("CHAR\\.MBLEN\\(", cmpJMV)) {
        stop("CHAR.MBLEN is not implemented.")
    }
    # CHAR.RINDEX(haystack,needle[,divisor])
    if (grepl("CHAR\\.RINDEX\\(", cmpJMV)) {
        stop("CHAR.RINDEX is not implemented.")
    }
    # CHAR.RPAD(strexpr1,length[,strexpr2])
    if (grepl("CHAR\\.RPAD\\(", cmpJMV)) {
        stop("CHAR.RPAD is not implemented.")
    }
# implement SUBSTR
    # CHAR.SUBSTR(strexpr,pos[,length])
    if (grepl("CHAR\\.SUBSTR\\(", cmpJMV)) {
        stop("CHAR.SUBSTR is not implemented.")
    }
# implement CONCAT
    # CONCAT(strexpr,strexpr[,..])
    if (grepl("CONCAT\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c(cmpRpR, "CONCAT\\(", "paste0(")
        cmpVld <- FALSE
    }
    # COS(radians)
    if (grepl("COS\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c(cmpRpR, "COS\\(", "cos(")
        cmpVld <- FALSE
    }
    # LNGAMMA(numexpr)
    if (grepl("LNGAMMA\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c(cmpRpR, "LNGAMMA\\(", "lgamma(")
        cmpVld <- FALSE
    }
    # LOWER(strexpr)
    if (grepl("LOWER\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c(cmpRpR, "LOWER\\(", "tolower(")
        cmpVld <- FALSE
    }
    # LTRIM(strexpr[,char])
    if (grepl("LTRIM\\(", cmpJMV)) {
        cmpRpR <- c(cmpRpR, "LTRIM\\(", "trimws(", "[VARNAMES]", "[VARNAMES], which = \"left\"")
        cmpVld <- FALSE
    }
    # MBLEN.BYTE(strexpr,pos)
    if (grepl("MBLEN\\.BYTE\\(", cmpJMV)) {
        stop("MBLEN.BYTE is not implemented.")
    }
    # NORMALIZE(strexp)
    if (grepl("NORMALIZE\\(", cmpJMV)) {
        stop("NORMALIZE is not implemented.")
    }
    # NTRIM(varname)
    if (grepl("NTRIM\\(", cmpJMV)) {
        stop("NTRIM is not implemented.")
    }
    # RANGE(test,lo,hi[,lo,hi,..])
    if (grepl("RANGE\\(", cmpJMV)) {
        stop("RANGE is not implemented.")
    }
    # REPLACE(a1, a2, a3[, a4])
    if (grepl("REPLACE\\(", cmpJMV)) {
        stop("REPLACE is not implemented.")
    }
    # RTRIM(strexpr[,char])
    if (grepl("RTRIM\\(", cmpJMV)) {
        cmpRpR <- c(cmpRpR, "RTRIM\\(", "trimws(", "[VARNAMES]", "[VARNAMES], which = \"right\"")
        cmpVld <- FALSE
    }
    # SIN(radians)
    if (grepl("SIN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c(cmpRpR, "SIN\\(", "sin(")
        cmpVld <- FALSE
    }
    # STRUNC(strexp, length)
    if (grepl("STRUNC\\(", cmpJMV)) {
        stop("STRUNC is not implemented.")
    }
    # TRUNC(numexpr[,mult,fuzzbits]) - truncate towards 0
    if (grepl("TRUNC\\(", cmpJMV)) {
        stop("TRUNC is not implemented.")
    }
    # UPCASE(strexpr) -> UPPER
    if (grepl("UPCASE\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c(cmpRpR, "UPCASE\\(", "toupper(")
        cmpVld <- FALSE
    }
    # VALUELABEL(varname)
    if (grepl("VALUELABEL\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c(cmpRpR, "VALUELABEL\\(", "attr(dtaFrm[[", "\\)$", "]], 'jmv-desc')")
        cmpVld <- FALSE
    }
    # Distribution functions --------------------------------------------------
    # CDF... - cumulative distribution functions
    if (grepl("CDF\\.[BERNOULLI|BETA|BINOM|CAUCHY|CHISQ|EXP|F|GAMMA|GEOM|HYPER|LNORMAL|LOGISTIC|NORMAL|POISSON|SRANGE|T|UNIFORM|WEIBULL]", cmpJMV, ignore.case = TRUE)) cmpVld <- FALSE
    if (grepl("CDF\\.BERNOULLI\\(", cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.BERNOULLI\\(", "pbinom(", "[VARNAMES]", "[VARNAMES], 1")
    if (grepl("CDF\\.BETA\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.BETA\\(",      "pbeta(")
    if (grepl("CDF\\.BINOM\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.BINOM\\(",     "pbinom(")
    if (grepl("CDF\\.CAUCHY\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.CAUCHY\\(",    "pcauchy(")
    if (grepl("CDF\\.CHISQ\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.CHISQ\\(",     "pchisq(")
    if (grepl("CDF\\.EXP\\(",       cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.EXP\\(",       "pexp(")
    if (grepl("CDF\\.F\\(",         cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.F\\(",         "pf(")
    if (grepl("CDF\\.GAMMA\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.GAMMA\\(",     "pgamma(")
    if (grepl("CDF\\.GEOM\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.GEOM\\(",      "pgeom(")
    if (grepl("CDF\\.HYPER\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.HYPER\\(",     "phyper(")
    if (grepl("CDF\\.LNORMAL\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.LNORMAL\\(",   "plnorm(")
    if (grepl("CDF\\.LOGISTIC\\(",  cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.LOGISTIC\\(",  "plogis(")
    if (grepl("CDF\\.NORMAL\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.NORMAL\\(",    "pnorm(")
    if (grepl("CDFNORM\\(",         cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDFNORM\\(",         "pnorm(", "[VARNAMES]", "[VARNAMES], 0, 1")
    if (grepl("CDF\\.POISSON\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.POISSON\\(",   "ppois(")
    if (grepl("CDF\\.SRANGE\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.SRANGE\\(",    "psignrank(")
    if (grepl("CDF\\.T\\(",         cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.T\\(",         "pt(")
    if (grepl("CDF\\.UNIFORM\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.UNIFORM\\(",   "punif(")
    if (grepl("CDF\\.WEIBULL\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.WEIBULL\\(",   "pweibull(")
    if (grepl("CDF\\.[BVNOR|HALFNRM|IGAUSS|LAPLACE|NEGBIN|PARETO|SMOD]", cmpJMV, ignore.case = TRUE) ||
        grepl("NCDF\\.[BETA|CHISQ|F|T]",                                 cmpJMV, ignore.case = TRUE)) {
        stop("CDF.BVNOR, CDF.HALFNRM, CDF.IGAUSS, CDF.LAPLACE, CDF.NEGBIN, CDF.PARETO, CDF.SMOD, NCDF.BETA, NCDF.CHISQ, NCDF.F, and NCDF.T are not implemented.")
    }
    # IDF... - inverse distribution functions
    if (grepl("IDF\\.[BETA|CAUCHY|CHISQ|EXP|F|GAMMA|LNORMAL|LOGISTIC|NORMAL|SRANGE|T|UNIFORM|WEIBULL]", cmpJMV, ignore.case = TRUE)) cmpVld <- FALSE
    if (grepl("IDF\\.BETA\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.BETA\\(",      "qbeta(")
    if (grepl("IDF\\.CAUCHY\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.CAUCHY\\(",    "qcauchy(")
    if (grepl("IDF\\.CHISQ\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.CHISQ\\(",     "qchisq(")
    if (grepl("IDF\\.EXP\\(",       cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.EXP\\(",       "qexp(")
    if (grepl("IDF\\.F\\(",         cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.F\\(",         "qf(")
    if (grepl("IDF\\.GAMMA\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.GAMMA\\(",     "qgamma(")
    if (grepl("IDF\\.LNORMAL\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.LNORMAL\\(",   "qlnorm(")
    if (grepl("IDF\\.LOGISTIC\\(",  cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.LOGISTIC\\(",  "qlogis(")
    if (grepl("IDF\\.NORMAL\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.NORMAL\\(",    "qnorm(")
    if (grepl("PROBIT\\(",          cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PROBIT\\(",          "qnorm(", "[VARNAMES]", "[VARNAMES], 0, 1")
    if (grepl("IDF\\.SRANGE\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.SRANGE\\(",    "qsignrank(")
    if (grepl("IDF\\.T\\(",         cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.T\\(",         "qt(")
    if (grepl("IDF\\.UNIFORM\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.UNIFORM\\(",   "qunif(")
    if (grepl("IDF\\.WEIBULL\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.WEIBULL\\(",   "qweibull(")
    if (grepl("IDF\\.[HALFNRM|IGAUSS|LAPLACE|PARETO|SMOD]", cmpJMV, ignore.case = TRUE)) {
        stop("IDF.HALFNRM, IDF.IGAUSS, IDF.LAPLACE, IDF.PARETO, and IDF.SMOD are not implemented.")
    }
    # PDF... - probability density functions
    if (grepl("PDF\\.[BERNOULLI|BETA|BINOM|CAUCHY|CHISQ|EXP|F|GAMMA|GEOM|HYPER|LNORMAL|LOGISTIC|NORMAL|POISSON|T|UNIFORM|WEIBULL]", cmpJMV, ignore.case = TRUE)) cmpVld <- FALSE
    if (grepl("PDF\\.BERNOULLI\\(", cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.BERNOULLI\\(", "dbinom(", "[VARNAMES]", "[VARNAMES], 1")
    if (grepl("PDF\\.BETA\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.BETA\\(",      "dbeta(")
    if (grepl("PDF\\.BINOM\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.BINOM\\(",     "dbinom(")
    if (grepl("PDF\\.CAUCHY\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.CAUCHY\\(",    "dcauchy(")
    if (grepl("PDF\\.CHISQ\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.CHISQ\\(",     "dchisq(")
    if (grepl("PDF\\.EXP\\(",       cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.EXP\\(",       "dexp(")
    if (grepl("PDF\\.F\\(",         cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.F\\(",         "df(")
    if (grepl("PDF\\.GAMMA\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.GAMMA\\(",     "dgamma(")
    if (grepl("PDF\\.GEOM\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.GEOM\\(",      "dgeom(")
    if (grepl("PDF\\.HYPER\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.HYPER\\(",     "dhyper(")
    if (grepl("PDF\\.LNORMAL\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.LNORMAL\\(",   "dlnorm(")
    if (grepl("PDF\\.LOGISTIC\\(",  cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.LOGISTIC\\(",  "dlogis(")
    if (grepl("PDF\\.NORMAL\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.NORMAL\\(",    "dnorm(")
    if (grepl("PDF\\.POISSON\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.POISSON\\(",   "dpois(")
    if (grepl("PDF\\.T\\(",         cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.T\\(",         "dt(")
    if (grepl("PDF\\.UNIFORM\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.UNIFORM\\(",   "dunif(")
    if (grepl("PDF\\.WEIBULL\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.WEIBULL\\(",   "dweibull(")
    if (grepl("PDF\\.[BVNOR|HALFNRM|IGAUSS|LAPLACE|NEGBIN|PARETO]", cmpJMV, ignore.case = TRUE) ||
        grepl("NPDF\\.[BETA|CHISQ|F|T]",                            cmpJMV, ignore.case = TRUE)) {
        stop("PDF.BVNOR, PDF.HALFNRM, PDF.IGAUSS, PDF.LAPLACE, PDF.NEGBIN, PDF.PARETO, NPDF.BETA, NPDF.CHISQ, NPDF.F, and NPDF.T are not implemented.")
    }
    # RV... - random number generation functions
    # RV.BETA, RV.GAMMA, RV.NORMAL/NORMAL, RV.UNIFORM/UNIFORM -> see jamovi-functions above
    if (grepl("RV\\.[BERNOULLI|BINOM|CAUCHY|CHISQ|EXP|F|GEOM|HYPER|LNORMAL|LOGISTIC|POISSON|T|WEIBULL]", cmpJMV, ignore.case = TRUE)) cmpVld <- FALSE
    if (grepl("RV\\.BERNOULLI\\(",  cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.BERNOULLI\\(",  paste0("rbinom(",   as.character(nRow), ", 1, "))
    if (grepl("RV\\.BINOM\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.BINOM\\(",      paste0("rbinom(",   as.character(nRow), ", "))
    if (grepl("RV\\.CAUCHY\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.CAUCHY\\(",     paste0("rcauchy(",  as.character(nRow), ", "))
    if (grepl("RV\\.CHISQ\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.CHISQ\\(",      paste0("rchisq(",   as.character(nRow), ", "))
    if (grepl("RV\\.EXP\\(",        cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.EXP\\(",        paste0("rexp(",     as.character(nRow), ", "))
    if (grepl("RV\\.F\\(",          cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.F\\(",          paste0("rf(",       as.character(nRow), ", "))
    if (grepl("RV\\.GEOM\\(",       cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.GEOM\\(",       paste0("rgeom(",    as.character(nRow), ", "))
    if (grepl("RV\\.HYPER\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.HYPER\\(",      paste0("rhyper(",   as.character(nRow), ", "))
    if (grepl("RV\\.LNORMAL\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.LNORMAL\\(",    paste0("rlnorm(",   as.character(nRow), ", "))
    if (grepl("RV\\.LOGISTIC\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.LOGISTIC\\(",   paste0("rlogis(",   as.character(nRow), ", "))
    if (grepl("RV\\.POISSON\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.POISSON\\(",    paste0("rpois(",    as.character(nRow), ", "))
    if (grepl("RV\\.T\\(",          cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.T\\(",          paste0("rt(",       as.character(nRow), ", "))
    if (grepl("RV\\.WEIBULL\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.WEIBULL\\(",    paste0("rweibull(", as.character(nRow), ", "))
    if (grepl("RV\\.[HALFNRM|IGAUSS|LAPLACE|NEGBIN|PARETO]", cmpJMV, ignore.case = TRUE)) {
        stop("RV.HALFNRM, RV.IGAUSS, RV.LAPLACE, RV.NEGBIN, and RV.PARETO are not implemented.")
    }
    # SIG... - tail probability functions
    if (grepl("SIG\\.[CHISQ|F]", cmpJMV)) {
        stop("SIG.CHISQ, and SIG.F are not implemented.")
    }
    # Date / time functions ---------------------------------------------------
    if (grepl("DATE\\.|TIME\\.]", cmpJMV)) {
        stop("Date and time functions are not implemented.")
    }

    # not any function used, just arithmetic, relational or boolean operators
    # add handling for possible transformations - EQ, etc. possibly AND, OR, NOT
    # cmpJMV <- gsub("", "", cmpJMV)
    # cmpRpR <- c(cmpRpR, "", "")

    # calculate values in R ===================================================
    # add a data column (with the name of cmpVrT) and assign NA to it
    if (!any(grepl(cmpVrT, names(dtaFrm)))) dtaFrm[[cmpVrT]] <- NA
    # deal with min_valid: either select all row numbers (if cmpMnV is NA; default) or select row numbers
    # where the count of non-NA values is equal or exceeds the threshold set by cmpMnV
    if (grepl("min_valid\\s*=\\s*", cmpJMV)) {
        selRow <- which(rowSums(!is.na(dtaFrm[, cmpVrF])) >= as.integer(sub(".*?min_valid\\s=\\s(\\d+).*?[,|)].*", "\\1", cmpJMV, perl = TRUE)))
    } else {
        selRow <- seq(nRow)
    }
    # if a ignore_missing-flag exists and if it is not (i.e., larger than) then
    # add na.rm to the R-commands used to calculate the variable(s)
    if (grepl("ignore_missing\\s*=\\s*", cmpJMV) && !grepl("ignore_missing\\s*=\\s*0", cmpJMV)) {
        cmpRpR <- c("[VARNAMES]", "[VARNAMES], na.rm = TRUE", cmpRpR)
    }

    # deal with group_by: if no variable is set as grouping variable, all placeholder ([ALL]) is
    # assigned to unqGrp, and in the loop below, all row numbers (selRow) are selected; if a group
    # variable is set, the unique levels / values of the variable are determined and assigned to
    # unqGrp and afterwards, the for-loop underneath sequentially selects those unique groups
    if (grepl("group_by\\s*=\\s*", cmpJMV)) {
        cmpGrp <- sub(".*?group_by\\s=\\s(\\w+).*?[,|)].*", "\\1", cmpJMV, perl = TRUE)
        unqGrp <- unique(dtaFrm[selRow, cmpGrp])
    } else {
        cmpGrp <- c()
        unqGrp <- ""
    }
    for (crrGrp in unqGrp) {
        selGrp <- ifelse(is.null(cmpGrp), rep(TRUE, length(selRow)), dtaFrm[selRow, cmpGrp] == crrGrp)
        if (grepl("MAXABSIQR\\(|MAXABSZ\\(", cmpJMV)) {
            dtaFrm[selRow[selGrp], cmpVrT] <- max(unname(apply(dtaFrm[selRow[selGrp], cmpVrF], cmpDim, eval(parse(text = cmdClR(cmpJMV, cmpRpR))))))
        } else {
          # unname(apply(dtaFrm[, cmpVrF], 1, eval(parse(text = "function(X) abs(-mean(X))"))))
            dtaFrm[selRow[selGrp], cmpVrT] <-     unname(apply(dtaFrm[selRow[selGrp], cmpVrF], cmpDim, eval(parse(text = cmdClR(cmpJMV, cmpRpR)))))
        }
    }
    # if the command contains only commands that are available in jamovi, assign the required attributes to mark the variable as "Computed variable"
    if (cmpVld) {
        attr(dtaFrm[[cmpVrT]], "columnType")     <- "Computed"
        attr(dtaFrm[[cmpVrT]], "formula")        <- cmpJMV
        attr(dtaFrm[[cmpVrT]], "formulaMessage") <- ""
    }

    dtaFrm
}

fixVar <- function(vecVar = c(), crrSPS = c(), allVar = "") {
# the function checks, and - if necessary - automatically fixes a vector of variables

    selVar <- which(!grepl("^BY$|^WITH$|^TO$|^ALL$", vecVar))
    for (j in seq_along(selVar)) {
        # (1) concatenate variables that were separated by a line feed
        if (!any(grepl(paste0("^", vecVar[selVar[j]], "$"), allVar, ignore.case = TRUE))) {
            if (j < length(selVar) && any(grepl(paste0("^", vecVar[selVar[j + 0]], vecVar[selVar[j + 1]], "$"), allVar, ignore.case = TRUE))) {
                vecVar[selVar[j + 1]] <- paste0(vecVar[selVar[j + 0]], vecVar[selVar[j + 1]])
                vecVar[selVar[j + 0]] <- ""
                # skip to the next variable
                next
            }
            if (j > 1              && any(grepl(paste0("^", vecVar[selVar[j - 1]], vecVar[selVar[j + 0]], "$"), allVar, ignore.case = TRUE))) {
                vecVar[selVar[j + 0]] <- paste0(vecVar[selVar[j - 1]], vecVar[selVar[j + 0]])
                vecVar[selVar[j - 1]] <- ""
            }
        }
        # (2) replace the variable name with the variable name from the column header if it is not a perfect match (i.e., if lower- / uppercase don't match up)
        if  (any(grepl(paste0("^", vecVar[selVar[j]], "$"), allVar, ignore.case = TRUE)) &&
            !any(grepl(paste0("^", vecVar[selVar[j]], "$"), allVar, ignore.case = FALSE))) {
            vecVar[selVar[j]] <- allVar[grepl(paste0("^", vecVar[selVar[j]], "$"), allVar, ignore.case = TRUE)]
        }
        # (3) if the variable is still not found, precede it with a "#" so that it can be excluded from the analysis
        if (!any(grepl(paste0("^", vecVar[selVar[j]], "$"), allVar))) {
            if (grepl("^ZRE_\\d+$|^MAH_\\d+$", vecVar[selVar[j]])) {
                warning(sprintf("Variable \"%s\" was generated as an output of an earlier analysis and is not contained in the data.\nSPSS-command: %s\n\n",
                                vecVar[selVar[j]], paste(crrSPS, collapse = " /")))
            } else {
                warning(sprintf("Variable \"%s\" not contained in the dataset.\nSPSS-command: %s\n\n",
                                vecVar[selVar[j]], paste(crrSPS, collapse = " /")))
            }
            vecVar[selVar[j]] <- paste0("#", vecVar[selVar[j]])
        }
    }
    # remove empty entries that may have occured as consequence of (1)
    vecVar <- vecVar[vecVar != ""]

    # handle the keywords ALL and TO, and return vecVar
    if (any(grepl("^ALL$", vecVar))) {
        allVar
    } else if (any(grepl("^TO$", vecVar))) {
        for (j in grep("^TO$", vecVar)) {
            if (j < 2 || j > length(vecVar) - 1) stop(sprintf("The keyword \"TO\" is supposed to appear between variable names and not at the begin or the and of a variable list:\n%s\n\n", crrSPS))
            vecVar <- c(vecVar[1:(j - 1)], allVar[seq(grep(vecVar[j - 1], allVar) + 1, grep(vecVar[j + 1], allVar) - 1, 1)], vecVar[(j + 1):length(vecVar)])
        }
        vecVar
    } else {
        vecVar
    }
}

strSpl <- function(inpStr = "", dlmSpl = "") trimws(unlist(strsplit(trimws(inpStr), dlmSpl)))

cmdClR <- function(strCmd = "", rplCmd = c()) {
    if (length(rplCmd) %% 2 != 0) stop(sprintf("Vector with replacement strings must be multiples of 2 (original, replacement): \"%s\"\n\n", paste0(rplCmd, collapse = ", ")))
    for (j in seq_along(rplCmd)) {
        if (j %% 2 == 0) next
        strCmd <- gsub(paste0("^", rplCmd[j + 0]), rplCmd[j + 1], strCmd, ignore.case = TRUE)
    }
    # add self-created functions ----------------------------------------------
    if (grepl("boxcox\\(", strCmd)) {
        strCmd <- paste0("boxcox <- function(X, lambda = 0) ifelse(lambda == 0, log(X), (X ** lambda - 1) / lambda); ", strCmd)
    }
    # NB: contains currently only works for two input parameters
    if (grepl("contains\\(", strCmd)) {
        strCmd <- paste0("contains <- function(...) { inpLst <- list(...); if (length(inpLst) == 2) as.integer(apply(as.data.frame(inpLst), ",
                                                     "1, function(x) any(duplicated(x)))) else NA }; ", strCmd)
    }
    if (grepl("filter\\(", strCmd)) {
        strCmd <- paste0("filter <- function(X, fltExp = \"\") ifelse(eval(parse(text = fltExp)), X, ifelse(is.numeric(NA), \"\")); ", strCmd)
    }
    if (grepl("hlookup\\(", strCmd)) {
        strCmd <- paste0("hlookup <- function(...) { inpLst <- list(...); as.factor(diag(as.matrix(as.data.frame(inpLst[-1])[, as.integer(inpLst[[1]])]))) }; ", strCmd)
    }
    if (grepl("iqr\\(", strCmd)) {
        strCmd <- paste0("iqr <- function(X) { Q <- quantile(X, c(0.25, 0.75)); ",
                                              "Y <- rep(0, length(X)); ",
                                              "Y[X <= Q[1]] <- (X[X <= Q[1]] - Q[1]) / diff(Q); ",
                                              "Y[X >= Q[2]] <- (X[X >= Q[2]] - Q[2]) / diff(Q); ",
                                              "Y }; ", strCmd)
    }
    if (grepl("jmvSmp\\(", strCmd)) {
        strCmd <- paste0("jmvSmp <- function(m, x, n, y) { isNum <- all(is.numeric(c(x, y))); s <- factor(sample(c(rep(x, n), rep(y, m - n))), ordered = isNum); ",
                                                          "if (isNum) attr(s, \"values\") <- sort(c(x, y)); s }; ", strCmd)
    }
    if (grepl("ln\\(", strCmd)) {
        strCmd <- paste0("ln <- function(x) { x[x == 0] <- NaN; log(x) }; ", strCmd)
    }
    if (grepl("match\\(", strCmd)) {
        strCmd <- paste0("match <- function(...) { inpLst <- list(...); as.ordered(apply(matrix(unlist(inpLst[-1]), ncol = length(inpLst) - 1), 1, ",
                                                                          "function(x) grep(inpLst[[1]], x)[1])) }; ", strCmd)
    }
    if (grepl("offset\\(", strCmd)) {
        strCmd <- paste0("offset <- function(x, n) c(rep(NaN, ifelse(n > 0, n, 0)), x, rep(NA, ifelse(n < 0, -n, 0)))[seq_along(x) + ifelse(n > 0, 0, -n)]; ", strCmd)
    }
    if (grepl("se\\(", strCmd)) {
        strCmd <- paste0("se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))); ", strCmd)
    }
    if (grepl("split\\(", strCmd)) {
        strCmd <- paste0("split <- function(x, sep = ",", piece = NA) { x <- as.character(x); ",
                                                                       "if (is.na(piece)) {",
                                                                       " as.factor(unname(vapply(x, function(c) paste(strsplit(c, sep)[[1]], collapse = " "), character(1)))) ", 
                                                                       "} else if (piece > 0) {",
                                                                       " as.factor(unname(vapply(x, function(c) strsplit(c, sep)[[1]][piece], character(1)))) ",
                                                                       "} else {",
                                                                       "  as.factor(rep(NA, length(x))) ",
                                                                       "} }; ", strCmd)
    # replace [VARNAMES] with X (used for apply-function), put function(X) in front of the command
    paste0("function(X) ", gsub("\\[VARNAMES\\]", "X", strCmd))
}
