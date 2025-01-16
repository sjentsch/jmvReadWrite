#' Calculates distances (returning a symmetric matrix) from a raw data matrix in .omv-files for the
#' statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)
#'
#' @param dtaInp Either a data frame or the name of a data file to be read (including the path,
#'               if required; "FILENAME.ext"; default: NULL); files can be of any supported file
#'               type, see Details below.
#' @param fleOut Name of the data file to be written (including the path, if required;
#'               "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is returned
#'               instead.
#' @param varDst Variable (default: c()) containing a character vector with the names of the
#'               variables for which distances are to be calculated. See Details for more
#'               information.
#' @param clmDst Whether the distances shall be calculated between columns (TRUE) or rows
#'               (FALSE; default: TRUE). See Details for more information.
#' @param stdDst Character string indicating whether the variables in varDst are to be standardized
#'               and how (default: "none"). See Details for more information.
#' @param nmeDst Character string indicating which distance measure is to be calculated calculated
#'               (default: "euclidean"). See Details for more information.
#' @param mtxSps Whether the symmetric matrix to be returned should be sparse (default: FALSE)
#' @param mtxTrL Whether the symmetric matrix to be returned should only contain the lower
#'               triangular (default: FALSE)
#' @param mtxDgn Whether the symmetric matrix to be returned should retain the values in the main
#'               diagonal (default: TRUE)
#' @param usePkg Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata,
#'               and SAS files; "foreign" is the default (it comes with base R), but "haven" is
#'               newer and more comprehensive.
#' @param selSet Name of the data set that is to be selected from the workspace (only applies when
#'               reading .RData-files)
#' @param ...    Additional arguments passed on to methods; see Details below.
#'
#' @return a data frame containing a symmetric matrix (only returned if `fleOut` is empty)
#'         containing the distances between the variables / columns (clmDst == TRUE) or rows
#'         (clmDst == FALSE)
#'
#' @details
#' * `varDst` must a character vector containing the variables to calculated distances over. If
#'   `clmDst` is set to TRUE, distances are calculated between all possible variable pairs and over
#'   subjects / rows in the original data frame. If `clmDst` is set to FALSE, distances are
#'   calculated between participants and over all variables given in `varDst`. If `clmDst` is set
#'   to `TRUE`, the symmetric matrix that is returned has the size V x V (V being the number of
#'   variables in varDst; if `mtxSps` is set to `TRUE`, the size is V - 1 x V - 1, see below); if
#'   `clmDst` is set to `FALSE`, the symmetric matrix that is returned has the size R x R (R being
#'   the number of rows in the original dataset; it is if `mtxSps` is set to `TRUE`, the size is
#'   R - 1 x R - 1, see below).
#' * `stdDst` can be one of the following calculations to standardize the selected variables before
#'   calculating the distances: `none` (do not standardize; default), `z` (z scores), `sd` (divide
#'   by the std. dev.), `range` (divide by the range), `max` (divide by the absolute maximum),
#'   `mean` (divide by the mean), `rescale` (subtract the mean and divide by the range).
#' * `nmeDst` can be one of the following distance measures.
#'   (1) For interval data: `euclid` (Euclidean), `seuclid` (squared Euclidean), `block` (city
#'   block / Manhattan), `canberra` (Canberra). `chebychev` (maximum distance / supremum norm /
#'   Chebychev), `minkowski_p` (Minkowski with power p; NB: needs p), `power_p_r` (Minkowski with
#'   power p, and the r-th root; NB: needs p and r), `cosine` (cosine between the two vectors),
#'   `correlation` (correlation between the two vectors).
#'   (2) For frequency count data: `chisq` (chi-square dissimilarity between two sets of
#'   frequencies), `ph2` (chi-square dissimilarity normalized by the square root of the number
#'   of values used in the calculation).
#'   (3) For binary data, all measure have to optional parts `p` and `np` which indicate presence
#'   (`p`; defaults to 1 if not given) or absence (`np`; defaults to zero if not given).
#'   (a) matching coefficients: `rr_p_np` (Russell and Rao), `sm_p_np` (simple matching),
#'   `jaccard_p_np` / `jaccards_p_np` (Jaccard similarity; as in SPSS), `jaccardd_p_np` (Jaccard
#'   dissimiliarity; as in `dist(..., "binary")` in R), `dice_p_np` (Dice or Czekanowski or
#'   Sorenson similarity), `ss1_p_np` (Sokal and Sneath measure 1), `rt_p_np` (Rogers and
#'   Tanimoto), `ss2_p_np` (Sokal and Sneath measure 2), `k1_p_np` (Kulczynski measure 1),
#'   `ss3_p_np` (Sokal and Sneath measure 3).
#'   (b) conditional probabilities: `k2_p_np` (Kulczynski measure 2), `ss4_p_np` (Sokal and Sneath
#'   measure 4), `hamann_p_np` (Hamann).
#'   (c) predictability measures: `lambda_p_np` (Goodman and Kruskal Lambda), `d_p_np` (Anderberg’s
#'   D), `y_p_np` (Yule’s Y coefficient of colligation), `q_p_np` (Yule’s Q).
#'   (d) other measures: `ochiai_p_np` (Ochiai), `ss5_p_np` (Sokal and Sneath measure 5),
#'   `phi_p_np` (fourfold point correlation), `beuclid_p_np` (binary Euclidean distance),
#'   `bseuclid_p_np` (binary squared Euclidean distance), `size_p_np` (size difference),
#'   `pattern_p_np` (pattern difference), `bshape_p_np` (binary Shape difference), `disper_p_np`
#'   (dispersion similarity), `variance_p_np` (variance dissimilarity), `blwmn_p_np` (binary Lance
#'   and Williams non-metric dissimilarity).
#'   (4) `none` (only carry out standardization, if stdDst is different from `none`).
#' * If `mtxSps` is set, a sparse matrix is returned. Those matrices are similar to the format one
#'   often finds for correlation matrices. The values are only retained in the lower triangular,
#'   the columns range from the first to the variable that is second to the last in `varDst` (or
#'   respectively, the columns contain the first to the second to the last row of the original
#'   dataset when `clmDst` is set to `FALSE`), and the rows contain the second to the last variable
#'   in `varDst` (or respectively, the rows contain the second to the last row of the original
#'   dataset when `clmDst` is set to `FALSE`).
#' * By default, a full symmetric matrix is returned (i.e., a matrix that has no NAs in any cell).
#'   This behaviour can be changed with setting `mtxTrL` and `mtxDgn`: If `mtxTrL` is set to
#'   `TRUE`, the values from the upper triangular matrix are removed / replaced with NAs; if
#'   `mtxDgn` is set to `FALSE`, the values from the main diagonal are removed / replaced with NAs.
#' * The ellipsis-parameter (`...`) can be used to submit arguments / parameters to the functions
#'   that are used for reading and writing the data. By clicking on the respective function under
#'   “See also”, you can get a more detailed overview over which parameters each of those functions
#'   take. The functions are: `read_omv` and `write_omv` (for jamovi-files), `read.table` (for CSV
#'   / TSV files; using similar defaults as `read.csv` for CSV and `read.delim` for TSV which both
#'   are based upon `read.table`), `load` (for .RData-files), `readRDS` (for .rds-files),
#'   `read_sav` (needs the R-package `haven`) or `read.spss` (needs the R-package `foreign`) for
#'   SPSS-files, `read_dta` (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas`
#'   (`haven`) for SAS-data-files, and `read_xpt` (`haven`) / `read.xport` (`foreign`) for
#'   SAS-transport-files. If you would like to use `haven`, you may need to install it using
#'   `install.packages("haven", dep = TRUE)`.
#'
#' @seealso `distances_omv` internally uses the following function for calculating the distances
#'   for interval data [stats::dist()]. It furthermore uses the following functions for reading
#'   and writing data files in different formats: [jmvReadWrite::read_omv()] and
#'   [jmvReadWrite::write_omv()] for jamovi-files, [utils::read.table()] for CSV / TSV files,
#'   [load()] for reading .RData-files, [readRDS()] for .rds-files, [haven::read_sav()] or
#'   [foreign::read.spss()] for SPSS-files, [haven::read_dta()] or [foreign::read.dta()] for
#'   Stata-files, [haven::read_sas()] for SAS-data-files, and [haven::read_xpt()] or
#'   [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' # create matrices for the different types of distance measures: continuous
#' # (cntFrm), frequency counts (frqFrm) or binary (binFrm); all 20 R x 5 C
#' set.seed(1)
#' cntFrm <- stats::setNames(as.data.frame(matrix(rnorm(100, sd = 10),
#'             ncol = 5)), sprintf("C_%02d", seq(5)))
#' frqFrm <- stats::setNames(as.data.frame(matrix(sample(seq(10), 100,
#'             replace = TRUE), ncol = 5)), sprintf("F_%02d", seq(5)))
#' binFrm <- stats::setNames(as.data.frame(matrix(sample(c(TRUE, FALSE), 100,
#'             replace = TRUE), ncol = 5)), sprintf("B_%02d", seq(5)))
#' nmeOut <- tempfile(fileext = ".omv")
#'
#' # calculates the distances between columns, nmeDst is not required: "euclid"
#' # is the default
#' jmvReadWrite::distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst =
#'   names(cntFrm), nmeDst = "euclid")
#' dtaFrm <- jmvReadWrite::read_omv(nmeOut)
#' unlink(nmeOut)
#' # the resulting matrix (10 x 10) with the Euclidian distances
#' print(dtaFrm)
#'
#' # calculates the (Euclidean) distances between rows (clmDst = FALSE)
#' jmvReadWrite::distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst =
#'   names(cntFrm), clmDst = FALSE, nmeDst = "euclid")
#' dtaFrm <- jmvReadWrite::read_omv(nmeOut)
#' unlink(nmeOut)
#' # the resulting matrix (20 x 20) with the Euclidian distances
#' print(dtaFrm)
#'
#' # calculates the (Euclidean) distances between columns; the original data
#' # are z-standardized before calculating the distances (stdDst = "z")
#' jmvReadWrite::distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst =
#'   names(cntFrm), stdDst = "z", nmeDst = "euclid")
#' dtaFrm <- jmvReadWrite::read_omv(nmeOut)
#' unlink(nmeOut)
#' # the resulting matrix (10 x 10) with the Euclidian distances using the
#' # z-standardized data
#' print(dtaFrm)
#'
#' # calculates the correlations between columns
#' jmvReadWrite::distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst =
#'   names(cntFrm), nmeDst = "correlation")
#' dtaFrm <- jmvReadWrite::read_omv(nmeOut)
#' unlink(nmeOut)
#' # the resulting matrix (10 x 10) with the correlations
#' print(dtaFrm)
#'
#' # calculates the chi-square dissimilarity (nmeDst = "chisq") between columns
#' jmvReadWrite::distances_omv(dtaInp = frqFrm, fleOut = nmeOut, varDst =
#'   names(frqFrm), nmeDst = "chisq")
#' dtaFrm <- jmvReadWrite::read_omv(nmeOut)
#' unlink(nmeOut)
#' # the resulting matrix (10 x 10) with the chi-square dissimilarities
#' print(dtaFrm)
#'
#' # calculates the Jaccard similarity (nmeDst = "jaccard") between columns
#' jmvReadWrite::distances_omv(dtaInp = binFrm, fleOut = nmeOut, varDst =
#'   names(binFrm), nmeDst = "jaccard")
#' dtaFrm <- jmvReadWrite::read_omv(nmeOut)
#' unlink(nmeOut)
#' # the resulting matrix (10 x 10) with the Jaccard similarities
#' print(dtaFrm)
#'
#' }
#'
#' @export distances_omv
#'
distances_omv <- function(dtaInp = NULL, fleOut = "", varDst = c(), clmDst = TRUE, stdDst = "none",
                          nmeDst = "euclid", mtxSps = FALSE, mtxTrL = FALSE, mtxDgn = TRUE,
                          usePkg = c("foreign", "haven"), selSet = "", ...) {

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, usePkg = usePkg, selSet = selSet, ...)

    if (!all(nzchar(varDst)) || length(intersect(varDst, names(dtaFrm))) < 2) {
        stop("Calling distances_omv requires giving at least two (valid) variables to calculate distances between.")
    }

    # convert to matrix, and transpose if necessary ===============================================
    if (clmDst) dtaMtx <- as.matrix(dtaFrm[, varDst]) else dtaMtx <- t(as.matrix(dtaFrm[, varDst]))

    # standardize the data ========================================================================
    if        (grepl("^none$",             stdDst)) {
    } else if (grepl("^z$",                stdDst)) {
        dtaMtx <- scale(dtaMtx)
    } else if (grepl("^sd$",               stdDst)) {
        dtaMtx <- scale(dtaMtx, center = FALSE)
    } else if (grepl("^range$",            stdDst)) {
        dtaMtx <- scale(dtaMtx, center = FALSE, scale = (apply(dtaMtx, 2, max) - apply(dtaMtx, 2, min)))
    } else if (grepl("^max$",              stdDst)) {
        dtaMtx <- scale(dtaMtx, center = FALSE, scale =  apply(abs(dtaMtx), 2, max))
    } else if (grepl("^mean$",             stdDst)) {
        dtaMtx <- scale(dtaMtx, center = FALSE, scale =  apply(dtaMtx, 2, mean))
    } else if (grepl("^rescale$",          stdDst)) {
        dtaMtx <- scale(dtaMtx, center = apply(dtaMtx, 2, min), scale = (apply(dtaMtx, 2, max) - apply(dtaMtx, 2, min)))
    } else {
        stop(sprintf("Invalid standardization: %s. See Details in the help for further information.", stdDst))
    }

    # calculate distances (or proximities) ========================================================
    # (1) interval data ---------------------------------------------------------------------------
    if        (grepl("^euclid",             nmeDst)) {
        dstMtx <- as.matrix(stats::dist(t(dtaMtx), "euclidean", TRUE, TRUE))
    } else if (grepl("^seuclid",            nmeDst)) {
        dstMtx <- as.matrix(stats::dist(t(dtaMtx), "euclidean", TRUE, TRUE) ^ 2)
    } else if (grepl("^block$|^manhattan$", nmeDst)) {
        dstMtx <- as.matrix(stats::dist(t(dtaMtx), "manhattan", TRUE, TRUE))
    } else if (grepl("^canberra$",          nmeDst)) {
        dstMtx <- as.matrix(stats::dist(t(dtaMtx), "canberra",  TRUE, TRUE))
    } else if (grepl("^chebychev$",         nmeDst)) {
        dstMtx <- as.matrix(stats::dist(t(dtaMtx), "maximum",   TRUE, TRUE))
    } else if (grepl("^minkowski_\\d+$",    nmeDst)) {
        dstMtx <- as.matrix(stats::dist(t(dtaMtx), "minkowski", TRUE, TRUE, p = getPw(nmeDst)))
    } else if (grepl("^power_\\d+_\\d+$",   nmeDst)) {
        dstMtx <- as.matrix(stats::dist(t(dtaMtx), "minkowski", TRUE, TRUE, p = getPw(nmeDst)) ^ (getPw(nmeDst) / getRt(nmeDst)))
    } else if (grepl("^cosine$",            nmeDst)) {
        dstMtx <- clcCos(dtaMtx)
    } else if (grepl("^correlation$",       nmeDst)) {
        dstMtx <- stats::cor(dtaMtx, use = "pairwise")
    # (2) frequency count data --------------------------------------------------------------------
    } else if (grepl("^chisq$|^ph2$",       nmeDst)) {
        dstMtx <- clcFrq(dtaMtx, nmeDst)
    # (3) binary data -----------------------------------------------------------------------------
    } else if (binDst(nmeDst)) {
        dstMtx <- clcBin(dtaMtx, nmeDst)
    # (4) none ------------------------------------------------------------------------------------
    } else if (grepl("^none$",              nmeDst)) {
        dstMtx <- dtaMtx
    # ---------------------------------------------------------------------------------------------
    } else {
        stop(sprintf("Invalid distance measure: %s. See Details in the help for further information.",     nmeDst))
    }

    # converting distance matrix back to data frame, and apply mtxF2S
    dstFrm <- as.data.frame(dstMtx)
    if (mtxSps || mtxTrL) dstFrm <- mtxF2S(dstFrm, rmvTrU = mtxTrL, rmvDgn = !mtxDgn, mtxSps = mtxSps)

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dstFrm, fleOut = fleOut, dtaTtl = jmvTtl("_dist"), ...)
}

# helper and calculation functions ================================================================
# binary measures: check whether name is valid
binDst <- function(nmeDst = "") {
    grepl(paste0("^beuclid$|^blwmn$|^bseuclid$|^bshape$|^d$|^dice$|^disper$|^hamann$|^jaccard$|^jaccard[s,d]$|^k[1-2]$|^lambda$|",
                 "^ochiai$|^pattern$|^phi$|^q$|^rr$|^rt$|^size$|^sm$|^ss[1-5]$|^variance$|^y$"), gsub("_\\w+_\\w+$", "", nmeDst))
}

# binary measures: calculation, calls mtcBin for each cell (variable pair comparison / matches)
clcBin <- function(m = NULL, t = "jaccard") {
    # transform data matrix into a logical matrix
    m <- mkeBin(m, getP(t), getNP(t))
    # extract transformation name
    t <- strsplit(t, "_")[[1]][1]
    n <- ncol(m)
    # create a result matrix
    r <- matrix(NA, n, n, dimnames = rep(dimnames(m)[2], 2))

    for (i in seq(1, n)) {
        for (j in seq(1, i)) {
            # skip calculation of distance in the main diagonal for k1 and ss3
            # (keep NA; if calculated, the limitation to 9999.999 [l. 366 / 392] would hit)
            if (i == j && grepl("^k1$|^ss3$", t)) next
            r[i, j] <- r[j, i] <- mtcBin(m[, i], m[, j], t)
        }
    }

    r
}

# measure for continuos variables: cosine (between variable pairs)
clcCos <- function(m = NULL) {
    n <- ncol(m)
    # create a result matrix
    r <- matrix(1, n, n, dimnames = rep(dimnames(m)[2], 2))

    for (i in seq(2, n)) {
        for (j in seq(1, i - 1)) {
            r[i, j] <- r[j, i] <- crossprod(m[, i], m[, j]) / sqrt(crossprod(m[, i]) * crossprod(m[, j]))
        }
    }

    r
}

clcFrq <- function(m = NULL, t = "chisq") {
    n <- ncol(m)
    # create a result matrix
    r <- matrix(0, n, n, dimnames = rep(dimnames(m)[2], 2))

    for (i in seq(2, n)) {
        for (j in seq(1, i - 1)) {
            if        (t == "chisq") {
                r[i, j] <- r[j, i] <- suppressWarnings(stats::chisq.test(rbind(table(m[, i]), table(m[, j])))[["statistic"]])
            } else if (t == "ph2") {
                r[i, j] <- r[j, i] <- suppressWarnings(stats::chisq.test(rbind(table(m[, i]), table(m[, j])))[["statistic"]]) / sqrt(nrow(m) * 2)
            } else {
                stop(sprintf("clcFrq: Method %s is not implemented.", t))
            }
        }
    }

    r
}

# helper functions: get P (power / present), NP (not present), and R (root)
getP  <- function(s) stats::na.omit(c(strsplit(s, "_")[[1]][2], "1"))[1]
getNP <- function(s) stats::na.omit(c(strsplit(s, "_")[[1]][3], "0"))[1]
getPw <- function(s) as.numeric(strsplit(s, "_")[[1]][2])
getRt <- function(s) as.numeric(strsplit(s, "_")[[1]][3])

# binary measures: transform the data matrix from numeric into logical
mkeBin <- function(m = NULL, p = 1, np = 0) {
    if (all(apply(m, 2, is.logical))) return(m)

    if        (all(apply(m, 2, function(c) is.numeric(c)   &&   all(as.numeric(c(p, np)) %in% unique(c))))) {
        r <- matrix(as.logical(NA), nrow = nrow(m), ncol = ncol(m), dimnames = dimnames(m))
        r[m == as.numeric(p)]  <- TRUE
        r[m == as.numeric(np)] <- FALSE
    } else if (all(apply(m, 2, function(c) is.character(c) && all(as.character(c(p, np)) %in% unique(c))))) {
        r <- matrix(as.logical(NA), nrow = nrow(m), ncol = ncol(m), dimnames = dimnames(m))
        r[m == as.character(p)]  <- TRUE
        r[m == as.character(np)] <- FALSE
    } else {
        stop(paste("The input matrix for binary data either needs to be logical (then it will be kept as it is),",
                   "numeric or character (for the latter two, p and np are used to derive TRUE and FALSE)."))
    }

    r
}

# binary measures: calculation of variable pair matches
# implemented based upon: www.sussex.ac.uk/its/pdfs/SPSS_Statistics_Algorithms_22.pdf
# see www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/binmatch.htm for further possible measures
mtcBin <- function(x, y, t = "") {
    if (!all(is.logical(c(x, y)))) stop("mtcBin: Input columns to the calculation of binary measures must be logical.")
    o <- c(sum(x & y, na.rm = TRUE), sum(x & !y, na.rm = TRUE), sum(!x & y, na.rm = TRUE), sum(!x & !y, na.rm = TRUE))

    # binary - dissimilarity - Euclidian distance: binEuc - BEUCLID
    if      (t == "beuclid")  return(sqrt(sum(o[2:3])))
    # binary - dissimilarity - Lance and Williams: BLWMN
    else if (t == "blwmn")    return(sum(o[2], o[3]) / sum(o[-4], o[1]))
    # binary - dissimilarity - Squared Euclidian distance: BSEUCLID
    else if (t == "bseuclid") return(sum(o[2], o[3]))
    # binary - dissimilarity - Shape: BSHAPE
    else if (t == "bshape")   return((sum(o) * sum(o[2], o[3]) - sum(o[2], -o[3]) ^ 2) / sum(o) ^ 2)
    # binary - similarity - Anderberg's D: D
    else if (t == "d")        return((t1_Bin(o) - t2_Bin(o)) / (2 * sum(o)))
    # binary - similarity - Dice: DICE
    else if (t == "dice")     return((o[1] * 2) / sum(o[1], o[-4]))
    # binary - similarity - Dispersion: DISPER
    else if (t == "disper")   return((o[1] * o[4] - o[2] * o[3]) / (sum(o) ^ 2))
    # binary - similarity - Hamann: HAMANN
    else if (t == "hamann")   return(sum(o[1], -o[2], -o[3], o[4]) / sum(o))
    # binary - similarity - Jaccard: JACCARD
    else if (t == "jaccard")  return(o[1] / sum(o[-4]))
    # binary - similarity - Jaccard
    else if (t == "jaccards") return(o[1] / sum(o[-4]))
    # binary - distance - Jaccard (as in R - stats::dist - "binary")
    else if (t == "jaccardd") return(sum(o[2], o[3]) / sum(o[-4]))
    # binary - similarity - Kulczynski 1: K1
    else if (t == "k1")       return(min(o[1] / sum(o[2], o[3]), 9999.999))
    # binary - similarity - Kulczynski 2: K2
    else if (t == "k2")       return(sum(o[1] / sum(o[1], o[2]), o[1] / sum(o[1], o[3])) / 2)
    # binary - similarity - Lambda: LAMBDA
    else if (t == "lambda")   return((t1_Bin(o) - t2_Bin(o)) / (2 * sum(o) - t2_Bin(o)))
    # binary - similarity - Ochiai: OCHIAI
    else if (t == "ochiai")   return(sqrt((o[1] / sum(o[1], o[2])) * (o[1] / sum(o[1], o[3]))))
    # binary - similarity - Phi 4-point correlation: PHI
    else if (t == "phi")      return(((o[1] * o[4]) - (o[2] * o[3])) / sqrt(sum(o[1], o[2]) * sum(o[1], o[3]) * sum(o[2], o[4]) * sum(o[3], o[4])))
    # binary - dissimilarity - Pattern difference: PATTERN
    else if (t == "pattern")  return((o[2] * o[3]) / (sum(o) ^ 2))
    # binary - similarity - Yule's Q: Q
    else if (t == "q")        return((o[1] * o[4] - o[2] * o[3]) / sum(o[1] * o[4], o[2] * o[3]))
    # binary - similarity - Russel and Rao: RR
    else if (t == "rr")       return(o[1] / sum(o))
    # binary - similarity - Rogers and Tanimoto: RT
    else if (t == "rt")       return(sum(o[1], o[4]) / sum(o, o[2], o[3]))
    # binary - dissimilarity - Size difference: SIZE
    else if (t == "size")     return(((o[2] - o[3]) ^ 2) / (sum(o) ^ 2))
    # binary - similarity - Simple matching: SM
    else if (t == "sm")       return(sum(o[1], o[4]) / sum(o))
    # binary - similarity - Sokal and Sneath 1: SS1
    else if (t == "ss1")      return(2 * sum(o[1], o[4]) / sum(o, o[1], o[4]))
    # binary - similarity - Sokal and Sneath 2: SS2
    else if (t == "ss2")      return(o[1] / sum(o[-4], o[2], o[3]))
    # binary - similarity - Sokal and Sneath 3: SS3
    else if (t == "ss3")      return(min(sum(o[1], o[4]) / sum(o[2], o[3]), 9999.999))
    # binary - similarity - Sokal and Sneath 4: SS4
    else if (t == "ss4")      return(sum(o[1] / sum(o[1], o[2]), o[1] / sum(o[1], o[3]), o[4] / sum(o[2], o[4]), o[4] / sum(o[3], o[4])) / 4)
    # binary - similarity - Sokal and Sneath 5: SS5
    else if (t == "ss5")      return((o[1] * o[4]) / sqrt(sum(o[1], o[2]) * sum(o[1], o[3]) * sum(o[2], o[4]) * sum(o[3], o[4])))
    # binary - dissimilarity - Variance: VARIANCE
    else if (t == "variance") return(sum(o[2], o[3]) / (4 * sum(o)))
    # binary - similarity - Yule's Y: binYlY - Y
    else if (t == "y")        return((sqrt(o[1] * o[4]) - sqrt(o[2] * o[3])) / sum(sqrt(o[1] * o[4]), sqrt(o[2] * o[3])))
#   PLACEHOLDER FOR FUTURE IMPLEMENTATIONS
#   else if (t == "")         return()
    stop(sprintf("mtcBin: Method %s is not implemented.", t))
}

t1_Bin <- function(o = c()) {
    sum(max(o[1], o[2], na.rm = TRUE), max(o[3], o[4], na.rm = TRUE), max(o[1], o[3], na.rm = TRUE), max(o[2], o[4], na.rm = TRUE))
}

t2_Bin <- function(o = c()) {
    sum(max(sum(o[1], o[3], na.rm = TRUE), sum(o[2], o[4], na.rm = TRUE), na.rm = TRUE),
        max(sum(o[1], o[2], na.rm = TRUE), sum(o[3], o[4], na.rm = TRUE), na.rm = TRUE))
}
