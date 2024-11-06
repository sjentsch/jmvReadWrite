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
#' * `stdDst` can be one of the following calculations to standardize the selected variabĺes before
#'   calculating the distances: `none` (do not standardize; default), `z` (z scores), `sd` (divide
#'   by the std. dev.), `range` (divide by the range), `max` (divide by the absolute maximum),
#'   `mean` (divide by the mean), `rescale` (subtract the mean and divide by the range).
#' * `nmeDst` can be one of the following distance measures.
#'   (1) For interval data: `euclid` (), `seuclid` (), 
#'   `block` (), `chebychev` (), `minkowski_p` (NB: needs p), `power_p_r` (NB: needs p and r), `cosine` (), `correlation` ().
#'   (2) For frequency count data: `chisq` (), `ph2` ().
#'   (3) For binary data, all measure have to optional parts `p` and `np` which indicate presence
#'   (`p`; defaults to 1 if not given) or absence (`np`; defaults to zero if not given).
#'   (a) matching coefficients: `rr_p_np` (Russell and Rao), `sm_p_np` (), `jaccard_p_np` (),
#'   `dice_p_np` (), `ss1_p_np` (), `rt_p_np` (), `ss2_p_np` (), `k1_p_np` (), `ss3_p_np` ().
#'   (b) conditional probabilities: `k2_p_np` (), `ss4_p_np` (), `hamann_p_np` ().
#'   (c) predictability measures: `lambda_p_np` (), `d_p_np` (), `y_p_np` (), `q_p_np` ().
#'   (d) other measures: `ochiai_p_np` (), `ss5_p_np` (), `phi_p_np` (), `beuclid_p_np` (),
#'   `bseuclid_p_np` (), `size_p_np` (), `pattern_p_np` (), `bshape_p_np` (),
#'   `disper_p_np` (), `variance_p_np` (), `blwmn_p_np` ().
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
#' @seealso `distances_omv` internally uses the following functions for reading and writing data
#'   files in different formats: [jmvReadWrite::read_omv()] and [jmvReadWrite::write_omv()] for
#'   jamovi-files, [utils::read.table()] for CSV / TSV files, [load()] for reading .RData-files,
#'   [readRDS()] for .rds-files, [haven::read_sav()] or [foreign::read.spss()] for SPSS-files,
#'   [haven::read_dta()] or [foreign::read.dta()] for Stata-files, [haven::read_sas()] for
#'   SAS-data-files, and [haven::read_xpt()] or [foreign::read.xport()] for SAS-transport-files.
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export distances_omv
#'
distances_omv <- function(dtaInp = NULL, fleOut = "", varDst = c(), clmDst = TRUE, stdDst = "none",
                          nmeDst = "euclidean", mtxSps = FALSE, mtxTrL = FALSE, mtxDgn = TRUE,
                          usePkg = c("foreign", "haven"), selSet = "", ...) {

    if (length(varDst) < 2 || !all(nzchar(varDst))) {
        stop("Calling distances_omv requires giving at least two variables to calculate distances between.")
    }

    # check and import input data set (either as data frame or from a file)
    if (!is.null(list(...)[["fleInp"]])) stop("Please use the argument dtaInp instead of fleInp.")
    dtaFrm <- inp2DF(dtaInp = dtaInp, usePkg = usePkg, selSet = selSet, ...)

    # convert to matrix, and transpose if necessary ===============================================
    if (clmDst) dtaMtx <- as.matrix(dtaFrm) else dtaMtx <- t(as.matrix(dtaFrm))

    # standardize the data ========================================================================
    if        (grepl("^none$",        stdDst)) {
    } else if (grepl("^z$",           stdDst)) {
        dtaMtx <- scale(dtaMtx)
    } else if (grepl("^sd$",          stdDst)) {
        dtaMtx <- scale(dtaMtx, center = FALSE)
    } else if (grepl("^range$",       stdDst)) {
        dtaMtx <- scale(dtaMtx, center = FALSE, scale = (apply(dtaMtx, 2, max) - apply(dtaMtx, 2, min)))
    } else if (grepl("^max$",         stdDst)) {
        dtaMtx <- scale(dtaMtx, center = FALSE, scale = apply(abs(dtaMtx), 2, max))
    } else if (grepl("^mean$",        stdDst)) {
        dtaMtx <- scale(dtaMtx, center = FALSE, scale = colMeans(dtaMtx))
    } else if (grepl("^rescale$",     stdDst)) {
        dtaMtx <- scale(dtaMtx, center = apply(dtaMtx, 2, min), scale = (apply(dtaMtx, 2, max) - apply(dtaMtx, 2, min)))
    } else {
        stop(sprintf("Invalid standardization: %s. See Details in the help for further information.", stdDst))
    }
    
    # calculate distances (or similarities) =======================================================
    # (1) interval data ---------------------------------------------------------------------------
    if        (grepl("^euclid$",          nmeDst)) {
        dstRes <- as.matrix(stats::dist(t(dtaMtx), "euclidean", TRUE, TRUE))
    } else if (grepl("^seuclid$",         nmeDst)) {
        dstRes <- as.matrix(stats::dist(t(dtaMtx), "euclidean", TRUE, TRUE) ^ 2)
    } else if (grepl("^block$",           nmeDst)) {
        dstRes <- as.matrix(stats::dist(t(dtaMtx), "manhattan", TRUE, TRUE))
    } else if (grepl("^chebychev$",       nmeDst)) {
        dstRes <- as.matrix(stats::dist(t(dtaMtx), "maximum",   TRUE, TRUE))
    } else if (grepl("^minkowski_\\d+$",  nmeDst)) {
        dstRes <- as.matrix(stats::dist(t(dtaMtx), "minkowski", TRUE, TRUE, p = getP(nmeDst)))
    } else if (grepl("^power_\\d+_\\d+$", nmeDst)) {
        dstRes <- as.matrix(stats::dist(t(dtaMtx), "minkowski", TRUE, TRUE, p = getP(nmeDst)) ^ (getP(nmeDst) / getR(nmeDst)))
    } else if (grepl("^cosine$",          nmeDst)) {
        dstRes <- clcCos(dtaMtx)
    } else if (grepl("^correlation$",     nmeDst)) {
        dstRes <- cor(dtaMtx, use = "pairwise")
#"canberra",
#"binary" = "jaccard"
    # (2) frequency count data --------------------------------------------------------------------
    } else if (grepl("^chisq$|^ph2$",     nmeDst)) {
        dstRes <- clcFrq(dtaMtx, nmeDst)
    # (3) binary data -----------------------------------------------------------------------------
    } else if (grepl(paste0("^beuclid|^blwmn|^bseuclid|^bshape|^d$|^d_|^dice|^disper|^hamann|^jaccard|^k[1-2]$|^k[1-2]_|",
                            "^lambda|^ochiai|^pattern|^phi|^q$|^q_|^rr|^rt|^size|^ss[1-5]|^sm|^y$|^y_|^variance", nmeDst))) {
        dstRes <- clcBin(dtaMtx, nmeDst)
    # (4) none ------------------------------------------------------------------------------------
    } else if (grepl("^none$",            nmeDst)) {
        dstRes <- dtaMtx
    # ---------------------------------------------------------------------------------------------
    } else {
        stop(sprintf("Invalid distance measure: %s. See Details in the help for further information.",     nmeDst))
    }

    # converting distance matrix back to data frame, and apply mtxF2S
    dtaFrm <- 
    if (mtxSps || mtxTrL) dtaFrm <- mtxF2S(dtaFrm, rmvTrU = mtxTrL, rmvDgn = !mtxDgn, mtxSps = mtxSps)

    # rtnDta in globals.R (unified function to either write the data frame, open it in a new jamovi session or return it)
    rtnDta(dtaFrm = dtaFrm, fleOut = fleOut, dtaTtl = jmvTtl("_dist"), psvAnl = psvAnl, dtaInp = dtaInp, ...)
}

# helper and calculation functions ================================================================
# binary measures: calculation, calls mtcBin for each cell (variable pair comparison / matches)
clcBin <- function(m = NULL, t = "") {
    # transform data matrix into a logical matrix
    m <- mkeBin(m, getP(t), getNP(t))
    # extract transformation name
    t <- strsplit(t, "_")[[1]][1]
    n <- ncol(m)
    # create a result matrix
## TO-DO: change 0 / 1 as default
    r <- matrix(1, n, n)
   
    for (i in seq(2, n)) {
        for (j in seq(1, i - 1)) {
            r[i, j] <- mtcBin(m[, i], m[, j], t)
        }
    }
    r[upper.tri(r)] <- t(r)[upper.tri(r)]
    
    r
}

clcChi <- function(x = c()) { t = table(x); sum((t - mean(t)) ^ 2 / mean(t)) }

# measure for continuos variables: cosine (between variable pairs)
clcCos <- function(m = NULL) {
    n <- ncol(m)
    # create a result matrix
    r <- matrix(1, n, n)
   
    for (i in seq(2, n)) {
        for (j in seq(1, i - 1)) {
            r[i, j] <- crossprod(m[, i], m[, j]) / sqrt(crossprod(m[, i]) * crossprod(m[, j]))
        }
    }
    r[upper.tri(r)] <- t(r)[upper.tri(r)]
    
    r
}

clcFrq <- function(m = NULL, t = "") {
    n <- ncol(m)
    # create a result matrix
## TO-DO: change 0 / 1 as default
    r <- matrix(1, n, n)
   
    for (i in seq(2, n)) {
        for (j in seq(1, i - 1)) {
            if        (t == "chisq") {
                r[i, j] <- sqrt(clcChi(m[, i]) + clcChi(m[, j]))
            } else if (t == "ph2") {
                r[i, j] <- sqrt(clcChi(m[, i]) + clcChi(m[, j])) / sqrt(n)
            } else {
                stop()
        }
    }
    r[upper.tri(r)] <- t(r)[upper.tri(r)]
    
    r
}

# helper functions: get P (power / present), NP (not present), and R (root)
getP <-          function(s) c(as.numeric(na.omit(strsplit(s, "_")[[1]][2])), 1)[1]
getR <- getNP <- function(s) c(as.numeric(na.omit(strsplit(s, "_")[[1]][3])), 0)[1]

# binary measures: transform the data matrix from numeric into logical
mkeBin <- function(m = NULL, p = 1, np = 0) {
    if (all(apply(m, 2, is.logical))) return(m)

    if (all(apply(m, 2, is.numeric))) {
        r <- matrix(as.logical(m * NA), nrow = nrow(m))
        r[m ==  p] <- TRUE
        r[m == np] <- FALSE
    } else {
        stop(paste("The input matrix for binary data either needs to be logical (then it will be kept as it is),",
                   "or numeric (where p and np are used to derive TRUE and FALSE)."))
    }

    r
}

# binary measures: calculation of variable pair matches
# implemented based upon: www.sussex.ac.uk/its/pdfs/SPSS_Statistics_Algorithms_22.pdf
# see www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/binmatch.htm for further possible measures
mtcBin <- function(x, y, t = "") {
    o <- c(sum(x & y, na.rm = TRUE), sum(x & !y, na.rm = TRUE), sum(!x & y, na.rm = TRUE), sum(!x & !y, na.rm = TRUE))

    # binary - dissimilarity - Euclidian distance: binEuc - BEUCLID
    if      (t == "beuclid")  return(sqrt(sum(o[2:3])))
    # binary - dissimilarity - Lance and Williams: BLWMN
    else if (t == "blwmn")    return(sum(o[2], o[3]) / sum(o[-4], o[1]))
    # binary - dissimilarity - Squared Euclidian distance: BSEUCLID
    else if (t == "bseuclid") return(sum(o[2], o[3]))
    # binary - dissimilarity - Shape: BSHAPE
    else if (t == "bshape")   return((sum(o) * sum(o[2], o[3]) - sum(o[2], o[3]) ^ 2) / sum(o) ^ 2)
    # binary - similarity - Anderberg's D: D
    else if (t == "d")        return((t1_Bin(o) - t2_Bin(o)) / (2 * sum(o)))
    # binary - similarity - Dice: DICE
    else if (t == "dice")     return((o[1] * 2) / sum(o[1], o[-4]))
    # binary - similarity - Dispersion: DISPER
    else if (t == "disper")   return((o[1] * o[4] - o[2] * o[3]) / (sum(o) ^ 2))
    # binary - similarity - Jaccard: JACCARD
    else if (t == "jaccard")  return(o[1] / sum(o[-4]))
    # binary - similarity - Hamann: HAMANN
    else if (t == "hamann")   return(sum(o[1], -o[2], -o[3], o[4]) / sum(o))
    # binary - similarity - Kulczynski 1: K1
    else if (t == "k1")       return(min(o[1] / sum(o[2], o[3]), 9999.999))
    # binary - similarity - Kulczynski 2: K2
    else if (t == "k2")       return((o[1] / sum(2 * o[1], o[2]) / sum(o[1], o[3])) / 2)
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
    stop(sprint("mtcBin: Method %s is not implemented", t))
}

t1_Bin <- function(o = c()) sum(max(o[1], o[2], na.rm = T), max(o[3], o[4], na.rm=T), max(o[1], o[3], na.rm=T), max(o[2], o[4], na.rm=T))
t2_Bin <- function(o = c()) sum(max(sum(o[1], o[3], na.rm = TRUE), sum(o[2], o[4], na.rm = TRUE), na.rm = TRUE),
                                max(sum(o[1], o[2], na.rm = TRUE), sum(o[3], o[4], na.rm = TRUE), na.rm = TRUE))
