compute <- function(crrCmd = c(), data = data.frame()) {
    # jamovi Computed variables
    # https://raw.githubusercontent.com/jamovi/jamovi/current-dev/client/main/vareditor/formulatoolbar.js
    # on Linux: curl -s https://raw.githubusercontent.com/jamovi/jamovi/current-dev/client/main/vareditor/formulatoolbar.js | grep "^\s*descriptions" | cut -d\' -f2 | sed 's/ <i>//' | sed 's/<\/i> //' | sed 's/<\/i>,/,/' | sort
    # SPSS COMPUTE command
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-arithmetic-operations
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-relational-operators
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-logical-operators
    # https://www.ibm.com/docs/en/spss-statistics/SaaS?topic=expressions-not-logical-operator

    # get rid of the COMPUTE-command in case SPSS-syntax is submitted
    isSPSS <- grepl("^COMPUTE", crrCmd)
    crrCmd <- trimws(gsub(".$", "", gsub("^COMPUTE", "", crrCmd)))
    # initialize calculation commands
    cmpJMV <- gsub(",\\s+", ", ", gsub(",", ", ", gsub("\\.$", "", paste0(strSpl(crrCmd, "=")[-1], collapse = "= "))))
    cmpRpR <- c()
    cmpClR <- TRUE
    cmpDim <- 1
    cmpMnV <- NA
    cmpGrp <- c()
    cmpVld <- TRUE

    # extract target variable (cmprT) and variables in the formula (cmprT; incl. removing further arguments, e.g., numbers)
    cmpVrT <- strSpl(crrCmd, "=")[1]
    cmpVrF <- gsub("\\).*$", "", gsub("^[A-z\\.]+\\(", "", gsub("[A-z\\.]+\\d+\\(", "", gsub("^\\(+", "", strSpl(cmpJMV, "\\+|-\\s+|\\*|/|&|\\||!\\s|<=|>=|!=")))))
    cmpVrF <- cmpVrF[is.na(suppressWarnings(as.numeric(cmpVrF)))]
    cmpJMV <- gsub(cmpVrF, "[VARNAMES]", cmpJMV)
    # check whether all variables are contained in the data
    cmpVrF <- fixVar(strSpl(cmpVrF, "\\s*,\\s*|\\s* \\s*"), crrCmd, names(data))
    if (any(any(grepl("^#", cmpVrF)))) stop(sprintf("Variable \"%s\" is not contained in the data, formula can't be calculated:\n \"%s\"\n\n", cmpVrF[grepl("^#", cmpVrF)], crrCmd))

    # ABS(number): Returns the absolute value of a number
    if (grepl("ABS\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("ABS\\(", "ABS(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "ABS\\(", "abs(")
    }
    # ABSIQR(variable): Convenience short-hand for ABS(IQR(variable))
    # ABSZ(variable): Convenience short-hand for ABS(Z(variable))
    if (grepl("ABSZ\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("ABSZ\\(", "ABSZ(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "ABSZ\\(", "abs(scale(")
    }
    # BETA(alpha, beta): Draws samples from a Beta distribution.
#   if (grepl("RV\\.BETA\\(",       cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.BETA\\(",       "rbeta(")
    # BOXCOX(variable, lambda): Returns a Box Cox transformation of the variable.
    # CEILING(variable): Rounds each value to the integer above
    if (grepl("CEILING\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("CEILING\\(", "CEILING(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "CEILING\\(", "ceiling(")
    }    
    # CONTAINS(item1, item2, item3, …, in1, in2, in3, …): Determines if any of the items appear in in1, in2, in3, ....
    #                                                     Note that most of these arguments are optional -- it is possible to simply use CONTAINS(needle, haystack).
    # -> SPSS: CHAR.INDEX(haystack, needle[, divisor]) - find needle in haystack
    # EXP(number): Returns the exponent for basis \u212F of a number.
    if (grepl("EXP\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("EXP\\(", "EXP(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "EXP\\(", "exp(")
    }    
    # FILTER(variable, filter_expression): Filters a variable using the filter expression.
    # FLOOR(variable): Rounds each value to the integer below
    if (grepl("FLOOR\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("FLOOR\\(", "FLOOR(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "FLOOR\\(", "floor(")
    }    
    # GAMMA(shape, scale): Draws samples from a Gamma distribution.
#   if (grepl("RV\\.GAMMA\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.GAMMA\\(",      "rgamma(")
    # HLOOKUP(index, value 1, value 2, …): The value in the provided values at index.
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
    # IQR(variable): Returns a whether the variable is an outlier according to the IQR: If the value is within the box of a Boxplot 0 is returned, absolute values larger than 1.5 are outside the whiskers.
# to-do: handle NAs
    # LN(number): Returns the natural logarithm of a number.
    if (grepl("LN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("LN\\(", "LN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "LN\\(", "log(")
    }
    # LOG10(number): Returns the base-10 logarithm of a number. [SPSS: LG10]
    if (grepl("LG10\\(|LOG10\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("LG10\\(|LOG10\\(", "LOG10(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "LOG10\\(", "log10(")
    }
    # MATCH(value, value 1, value 2, …): The index of value in the provided values.
    # MAX(number 1, number 2, …): Returns the largest value of a set of numbers.
    if (grepl("MAX\\(|MAX\\.\d+\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("MAX\\(", "MAX(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "MAX\\(", "max(", "[VARNAMES]", "[VARNAMES], na.rm = TRUE")
    }
    # MAXABSIQR(variable 1, variable 2, …): Max. absolute IQR-value (i.e., how far outside the box an individual datapoint is in terms of IQR - Q1/Q3-distance)
    # MAXABSZ(variable 1, variable 2, …, group_by=0): Max. absolute z-value / normalized value.
    # MEAN(number 1, number 2, …, ignore_missing=0, min_valid=0): Returns the mean of a set of numbers.
    if (grepl("MEAN\\(|MEAN\\.\d+\\(", cmpJMV, ignore.case = TRUE)) {
# to-do: SPSS to min_valid
# , "[VARNAMES]", "[VARNAMES], na.rm = TRUE"
# cmpMnV <- NA
        cmpJMV <- gsub("MEAN\\(|MEAN\\.\d+\\(", "MEAN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "MEAN\\(", "mean(")
    }
# to implement
    # MEDIAN(variable 1, variable 2, …)
    if (grepl("MEDIAN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("MEDIAN\\(", "MEDIAN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "MEDIAN\\(", "median(")
    }
    # MIN(number 1, number 2, …): Returns the smallest value of a set of numbers.
    if (grepl("MIN\\(|MIN\\.\d+\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("MIN\\(", "MIN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "MIN\\(", "min(", "[VARNAMES]", "[VARNAMES], na.rm = TRUE")
    }
# to implement: MOD(numexpr, modulus) - modulus
    # NORM(mean, sd): Draws samples from a normal (Gaussian) distribution. [SPSS: RV.NORMAL, NORMAL]
    if (grepl("NORM\\(|RV.NORMAL\\(", cmpJMV, ignore.case = TRUE)) {
#   if (grepl("RV\\.NORMAL\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.NORMAL\\(",     "rnorm(")
#   if (grepl(" NORMAL\\(",         cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, " NORMAL\\(",         " rnorm(0, ")
       cmpJMV <- gsub("MIN\\(", "MIN(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "MIN\\(", "min(")
    }

    # NOT(value): Inverts the value.
    if (grepl("NOT\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("NOT\\(", "NOT(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "NOT\\(", "!")
    }    
    # OFFSET(variable, integer): Offsets the values up or down.

    # RANK(variable): Ranks each value

    # ROUND(variable, digits=0): Rounds each value [SPSS: RND(numexpr[,mult,fuzzbits])]
    if (grepl("RND\\(|ROUND\\(", cmpJMV, ignore.case = TRUE)) {
# to-do: SPSS fuzzbits to digits
        cmpJMV <- gsub("RND\\(|ROUND\\(", "ROUND(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "ROUND\\(", "round(")
    }
    # ROW(NO ARGUMENTS): Returns the row numbers.
    if (grepl("ROW\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("ROW\\(", "ROW(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "ROW\\(\\)", "")
    }
    # SAMPLE(variable, n, otherwise=NA): Draws a sample of n from the variable. i.e. SAMPLE(var, 20), i.e. SAMPLE(1, 20), i.e. SAMPLE(\"training\", 20, \"test\")
    # SCALE -> Z
    # SPLIT(variable, sep=",", piece): Splits text into pieces based on a separator. piece specifies the desired piece by index.
    # SQRT(number): Returns the square root of a number.
    if (grepl("SQRT\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("SQRT\\(", "SQRT(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "SQRT\\(", "sqrt(")
    }
    # STDEV(number 1, number 2, …, ignore_missing=0): Returns the standard deviation of a set of numbers. [SPSS: SD(numexpr,numexpr[,..])]
    if (grepl("STDEV\\(|SD\\(|SD\\.\d+\\(", cmpJMV, ignore.case = TRUE)) {
# to-do: SPSS to ignore_missing
# , "[VARNAMES]", "[VARNAMES], na.rm = TRUE"
        cmpJMV <- gsub("SD\\(|STDEV\\(", "STDEV(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "STDEV\\(", "sd(")
    }
    # SUM(number 1, number 2, …, ignore_missing=0, min_valid=0): Returns the sum of a set of numbers.
    if (grepl("SUM\\(|SUM\\.\d+\\(", cmpJMV, ignore.case = TRUE)) {
# to-do: SPSS to min_valid
# , "[VARNAMES]", "[VARNAMES], na.rm = TRUE"
# cmpMnV <- NA
        cmpJMV <- gsub("SUM\\(|SUM\\.\d+\\(", "SUM(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "SUM\\(", "sum(")
    }
    # TEXT(number): Converts the value to text.
    if (grepl("TEXT\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("TEXT\\(", "TEXT(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "TEXT\\(", "as.character(")
    }
    # UNIF(low, high): Draws samples from a uniform distribution. [SPSS: RV.UNIFORM, UNIFORM]
#   if (grepl("RV\\.UNIFORM\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.UNIFORM\\(",    "runif(")
#   if (grepl(" UNIFORM\\(",        cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, " UNIFORM\\(",        " runif(0, ")

    # VALUE(text): Converts text to a number (if possible) [SPSS: NUMBER(strexpr,format)]
    if (VALUE("TEXT\\(", cmpJMV, ignore.case = TRUE)) {
        cmpJMV <- gsub("VALUE\\(", "VALUE(", cmpJMV, ignore.case = TRUE)
        cmpRpR <- c(cmpRpR, "VALUE\\(", "as.numeric(")
    }
    # VAR(number 1, number 2, …, ignore_missing=0): Returns the variance of a set of numbers. [SPSS: VARIANCE(numexpr,numexpr[,..])]
    if (grepl("VAR\\(|VARIANCE\\(", cmpJMV, ignore.case = TRUE)) {
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
        cmpRpR <- c(cmpRpR, "VN\\(", "sum(!is.na(")
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
#       cmpRpR <- c(cmpRpR, "VMAX\\(", "max(")
# currently no idea how to implement this
        cmpRpR <- c()
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

    # there is a couple of SPSS-functions / -operations that won't be implemented
    # ANY(test,value[,value,...])
    # ARSIN(numexpr)
    if (grepl("ARSIN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c("ARSIN\\(", "asin(")
        cmpVld <- FALSE
    }
    # ARTAN(numexpr)
    if (grepl("ARTAN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c("ARTAN\\(", "atan(")
        cmpVld <- FALSE
    }
    # CFVAR(numexpr,numexpr[,...])
    # CHAR.LENGTH(strexpr) / LENGTH(strexpr)
    # CHAR.LPAD(strexpr1,length[,strexpr2])
    # CHAR.MBLEN(strexpr,pos)
    # CHAR.RINDEX(haystack,needle[,divisor])
    # CHAR.RPAD(strexpr1,length[,strexpr2])
    # CHAR.SUBSTR(strexpr,pos[,length])
    # CONCAT(strexpr,strexpr[,..])
    # COS(radians)
    if (grepl("COS\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c("COS\\(", "cos(")
        cmpVld <- FALSE
    }
    # LAG(variable[, n])
    # LNGAMMA(numexpr)
    # LOWER(strexpr)
    # LTRIM(strexpr[,char])
    # MBLEN.BYTE(strexpr,pos)
    # NORMALIZE(strexp)
    # NTRIM(varname)
    # RANGE(test,lo,hi[,lo,hi,..])
    # REPLACE(a1, a2, a3[, a4])
    # RTRIM(strexpr[,char])
    # SIN(radians)
    if (grepl("SIN\\(", cmpJMV, ignore.case = TRUE)) {
        cmpRpR <- c("SIN\\(", "sin(")
        cmpVld <- FALSE
    }
    # STRING(numexpr,format)
    # STRUNC(strexp, length)
    # TRUNC(numexpr[,mult,fuzzbits]) - truncate towards 0
    # UPCASE(strexpr)
    # VALUELABEL(varname)
    # Distribution functions ==================================================
    # CDF... - cumulative distribution functions
    if (grepl("CDF\\.[BERNOULLI|BETA|BINOM|CAUCHY|CHISQ|EXP|F|GAMMA|GEOM|HYPER|LNORMAL|LOGISTIC|NORMAL|POISSON|SRANGE|T|UNIFORM|WEIBULL]", cmpJMV, ignore.case = TRUE)) cmpVld <- FALSE
    if (grepl("CDF\\.BERNOULLI\\(", cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.BERNOULLI\\(", "pbinom(", "[VARNAMES]", "[VARNAMES], 1")
    if (grepl("CDF\\.BETA\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.BETA\\(",      "pbeta(")
    if (grepl("CDF\\.BINOM\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "CDF\\.BETA\\(",      "pbinom(")
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
    if (grepl("CDF\\.[BVNOR|HALFNRM|IGAUSS|LAPLACE|NEGBIN|PARETO|SMOD]", cmpJMV, ignore.case = TRUE) | 
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
    if (grepl(" PROBIT\\(",         cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, " PROBIT\\(",         " qnorm(", "[VARNAMES]", "[VARNAMES], 0, 1")
    if (grepl("IDF\\.SRANGE\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.SRANGE\\(",    "qsignrank(")
    if (grepl("IDF\\.T\\(",         cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.T\\(",         "qt(")
    if (grepl("IDF\\.UNIFORM\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.UNIFORM\\(",   "qunif(")
    if (grepl("IDF\\.WEIBULL\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "IDF\\.WEIBULL\\(",   "qweibull(")
    if (grepl("IDF\\.[HALFNRM|IGAUSS|LAPLACE|PARETO|SMOD]", cmpJMV, ignore.case = TRUE)
        stop("IDF.HALFNRM, IDF.IGAUSS, IDF.LAPLACE, IDF.PARETO, and IDF.SMOD are not implemented.")
    }
    # PDF... - probability density functions
    if (grepl("PDF\\.[BERNOULLI|BETA|BINOM|CAUCHY|CHISQ|EXP|F|GAMMA|GEOM|HYPER|LNORMAL|LOGISTIC|NORMAL|POISSON|T|UNIFORM|WEIBULL]", cmpJMV, ignore.case = TRUE)) cmpVld <- FALSE
    if (grepl("PDF\\.BERNOULLI\\(", cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.BERNOULLI\\(", "dbinom(", "[VARNAMES]", "[VARNAMES], 1")
    if (grepl("PDF\\.BETA\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.BETA\\(",      "dbeta(")
    if (grepl("PDF\\.BINOM\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "PDF\\.BETA\\(",      "dbinom(")
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
    if (grepl("PDF\\.[BVNOR|HALFNRM|IGAUSS|LAPLACE|NEGBIN|PARETO]", cmpJMV, ignore.case = TRUE) |
        grepl("NPDF\\.[BETA|CHISQ|F|T]",                                 cmpJMV, ignore.case = TRUE)) {
        stop("PDF.BVNOR, PDF.HALFNRM, PDF.IGAUSS, PDF.LAPLACE, PDF.NEGBIN, PDF.PARETO, NPDF.BETA, NPDF.CHISQ, NPDF.F, and NPDF.T are not implemented.")
    }
    # RV... - random number generation functions
    # RV.BETA, RV.GAMMA, RV.NORMAL/NORMAL, RV.UNIFORM/UNIFORM -> see jamovi-functions above
    if (grepl("RV\\.[BERNOULLI|BINOM|CAUCHY|CHISQ|EXP|F|GEOM|HYPER|LNORMAL|LOGISTIC|POISSON|T|WEIBULL]", cmpJMV, ignore.case = TRUE)) cmpVld <- FALSE
    if (grepl("RV\\.BERNOULLI\\(",  cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.BERNOULLI\\(",  "rbinom(", "[VARNAMES]", "[VARNAMES], 1")
    if (grepl("RV\\.BINOM\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.BETA\\(",       "rbinom(")
    if (grepl("RV\\.CAUCHY\\(",     cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.CAUCHY\\(",     "rcauchy(")
    if (grepl("RV\\.CHISQ\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.CHISQ\\(",      "rchisq(")
    if (grepl("RV\\.EXP\\(",        cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.EXP\\(",        "rexp(")
    if (grepl("RV\\.F\\(",          cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.F\\(",          "rf(")
    if (grepl("RV\\.GEOM\\(",       cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.GEOM\\(",       "rgeom(")
    if (grepl("RV\\.HYPER\\(",      cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.HYPER\\(",      "rhyper(")
    if (grepl("RV\\.LNORMAL\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.LNORMAL\\(",    "rlnorm(")
    if (grepl("RV\\.LOGISTIC\\(",   cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.LOGISTIC\\(",   "rlogis(")
    if (grepl("RV\\.POISSON\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.POISSON\\(",    "rpois(")
    if (grepl("RV\\.T\\(",          cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.T\\(",          "rt(")
    if (grepl("RV\\.WEIBULL\\(",    cmpJMV, ignore.case = TRUE)) cmpRpR <- c(cmpRpR, "RV\\.WEIBULL\\(",    "rweibull(")
    if (grepl("RV\\.[HALFNRM|IGAUSS|LAPLACE|NEGBIN|PARETO]", cmpJMV, ignore.case = TRUE)) {
        stop("RV.HALFNRM, RV.IGAUSS, RV.LAPLACE, RV.NEGBIN, and RV.PARETO are not implemented.")
    }
    # SIG... - tail probability functions
    if (grepl("SIG\\.[CHISQ|F]", cmpJMV)) {
        stop("SIG.CHISQ, and SIG.F are not implemented.")
    }
    # Date / time functions ===================================================
    if (grepl("DATE\\.|TIME\\.]", cmpJMV)) {
        stop("Date and time functions are not implemented.")
    }

    # not any function used, just arithmetic, relational or boolean operators
    # add handling for possible transformations - EQ, etc. possibly AND, OR, NOT
    # cmpJMV <- gsub("", "", cmpJMV)
    # cmpRpR <- c(cmpRpR, "", "")

    if (length(cmpRpR) ==  0) {
        stop()
    }

    # add a data column (with the name of cmpVrT) and assign it to a temporary variable (which has the correct number of lines so that the filter is working)
    if (!any(grepl(cmpVrT, names(data)))) data[[cmpVrT]] <- NA
    # unname(apply(data[, cmpVrF], 1, eval(parse(text = "function(X) abs(-mean(X))"))))
# to implement
    # cmpMnV <- NA
    # cmpGrp <- c()
    # , ignore_missing=0 -> na.rm = TRUE if not 0
    # fmtClc(cmpJMV, cmpRpR, cmpVrF)
    data[[cmpVrT]] <- unname(apply(data[, cmpVrF], cmpDim, eval(parse(text = ""))))

    # if the command doesn't contain any commands, not available in jamovi, assign the required attributes to mark the variable as "Computed variable" in jamovi
    if (cmpVld) {
        attr(data[[cmpVrT]], "columnType")     <- "Computed"
        attr(data[[cmpVrT]], "formula")        <- cmpJMV
        attr(data[[cmpVrT]], "formulaMessage") <- ""
    }

    data
}

strSpl <- function(inpStr = "", dlmSpl = "") {
    trimws(unlist(strsplit(trimws(inpStr), dlmSpl)))
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
