test_that("distances_omv works", {
    set.seed(1)
    cntFrm <- stats::setNames(as.data.frame(matrix(rnorm(1000, sd = 10),                         nrow = 50)), sprintf("C_%02d", seq(20)))
    frqFrm <- stats::setNames(as.data.frame(matrix(sample(seq(10),        1000, replace = TRUE), nrow = 50)), sprintf("C_%02d", seq(20)))
    binFrm <- stats::setNames(as.data.frame(matrix(sample(c(TRUE, FALSE), 1000, replace = TRUE), nrow = 50)), sprintf("C_%02d", seq(20)))
    binFm2 <- as.data.frame(lapply(binFrm, function(c) as.integer(c)  + 0))
    binFm3 <- as.data.frame(lapply(binFrm, function(c) as.integer(c)  + 1))
    binFm4 <- as.data.frame(lapply(binFrm, function(c) as.integer(!c) + 1))
    binFm5 <- stats::setNames(as.data.frame(matrix(sample(c(rep(1, 5), rep(2, 5), 4), 1000, replace = TRUE), nrow = 50)), sprintf("C_%02d", seq(20)))
    nmeInp <- tempfile(fileext = ".rds")
    nmeOut <- tempfile(fileext = "_D.omv")
    saveRDS(cntFrm, nmeInp)

    # Euclidean distances (column-wise), no standardization
    expect_null(distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst = names(cntFrm)))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(20, 20))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), rep("double", 20))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)), c(90.18679, 93.75375, 93.49854,  95.84402, 100.25257, 96.79214,  95.25020, 96.61567, 97.88351, 108.10851,
                                                   91.54438, 96.83193, 99.66558, 104.26397, 102.46154, 99.06569, 104.29886, 89.90709, 97.13592, 101.83002),
                                                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd)),   c(23.05755, 23.22398, 23.30027,  23.63582,  24.78627, 24.50848,  24.47311, 24.23558, 24.02617,  27.30439,
                                                   23.24474, 24.65757, 25.18948,  25.89397,  25.80464, 24.56810,  27.01149, 22.32947, 24.55965,  25.65252),
                                                 tolerance = 1e-6)
    unlink(nmeOut)

    # Euclidean distances (row-wise), no standardization
    expect_null(distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst = names(cntFrm), clmDst = FALSE))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(50, 50))
    expect_equal(names(df4Chk), as.character(seq(50)))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), rep("double", 50))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 50))
    expect_equal(unname(apply(df4Chk, 2, mean)), c(59.23618, 64.02938, 62.95174, 60.92444, 66.50634, 76.44711, 68.87178, 57.46921, 64.34043, 64.93839,
                                                   68.70524, 58.17400, 63.75770, 67.27705, 64.39134, 67.93244, 56.30517, 68.48069, 56.41789, 67.58459,
                                                   63.14606, 65.99261, 57.17930, 69.23091, 62.35964, 62.27133, 61.43129, 65.34220, 61.63603, 56.70968,
                                                   61.44346, 66.03810, 57.51696, 64.25885, 62.64196, 64.52206, 61.71255, 57.74651, 63.29933, 54.38568,
                                                   65.92930, 66.02469, 67.45406, 54.62625, 76.43019, 66.94794, 67.35090, 60.45972, 70.91475, 63.27496),
                                                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd)),   c(11.95075, 13.17128, 11.88307, 13.26369, 13.00918, 13.04877, 13.84253, 12.70425, 13.44566, 12.97648,
                                                   13.15931, 11.53839, 12.25830, 13.28330, 12.82124, 13.21762, 12.26558, 12.71885, 12.02038, 13.12634,
                                                   12.40275, 14.07321, 12.40036, 13.39745, 11.58568, 11.92935, 12.37711, 13.38654, 13.28438, 11.73747,
                                                   13.02186, 14.00567, 11.24524, 13.15485, 12.62797, 12.11199, 12.48867, 11.61827, 12.75744, 11.22085,
                                                   13.40663, 13.04554, 13.89652, 11.80896, 14.62947, 13.57283, 13.25454, 12.48747, 13.82016, 12.33878),
                                                 tolerance = 1e-6)
    unlink(nmeOut)

    # Euclidean distances (column-wise), no standardization, sparse matrix
    expect_null(distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst = names(cntFrm), mtxSps = TRUE))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(19, 20))
    expect_equal(names(df4Chk), c("Variable", names(cntFrm)[-ncol(cntFrm)]))
    expect_equal(as.vector(df4Chk[, "Variable"]), names(cntFrm)[-1])
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("integer", rep("double", 19)))
    expect_true(all(is.na(df4Chk[, -1][upper.tri(matrix(NA, nrow = 19, ncol = 19))])))
    expect_equal(unname(apply(df4Chk[, -1], 2, mean, na.rm = TRUE)),
      c(94.93346,  99.11064,  99.36124, 101.83715, 107.08739, 103.51847, 103.06181, 102.75189, 105.06215, 116.83354,
        95.91321, 103.83493, 107.79650, 111.57576, 112.16093, 104.30065, 104.62081,  89.44986, 104.53152),
      tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk[, -1], 2, sd,   na.rm = TRUE)),
       c(9.24849,   7.41334,   7.66104,   6.74420,   7.47457,   9.51210,  10.06599,   9.95656,   7.32652,
        10.28519,   9.26913,   9.05088,  12.73900,   8.10547,   7.71139,   3.56249,  13.96156,   1.45965,  NA),
      tolerance = 1e-6)
    unlink(nmeOut)

    # Euclidean distances (column-wise), no standardization, lower triangular matrix
    expect_null(distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst = names(cntFrm), mtxTrL = TRUE))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(20, 20))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c(rep("double", 19), "integer"))
    expect_true(all(is.na(df4Chk[upper.tri(matrix(NA, nrow = 20, ncol = 20))])))
    expect_equal(unname(apply(df4Chk, 2, mean, na.rm = TRUE)),
      c(90.18679, 93.89429, 93.84117, 95.84673, 100.39442, 96.61724, 95.70025, 94.84790, 96.30697, 106.21231,
        86.32188, 92.29771, 94.32194, 95.63636,  93.46744, 83.44052, 78.46561, 59.63324, 52.26576,   0.00000),
      tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd,   na.rm = TRUE)),
      c(23.05755, 23.85163, 24.57072, 25.54777,  27.72862, 28.25636, 29.19290, 30.05032, 31.12292,  36.55301,
        31.56429, 35.63207, 39.89499, 42.81587,  46.30606, 46.74659, 53.53810, 51.65422, 73.91495,  NA),
    tolerance = 1e-6)
    unlink(nmeOut)

    # Euclidean distances (column-wise), no standardization, lower triangular matrix, lower triangular matrix without elements in the main diagonal
    expect_null(distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst = names(cntFrm), mtxTrL = TRUE, mtxDgn = FALSE))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(20, 20))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), rep("double", 20))
    expect_true(all(is.na(df4Chk[upper.tri(matrix(NA, nrow = 20, ncol = 20), TRUE)])))
    expect_equal(unname(apply(df4Chk[, -ncol(cntFrm)], 2, mean, na.rm = TRUE)),
      c(94.93346,  99.11064,  99.36124, 101.83715, 107.08739, 103.51847, 103.06181, 102.75189, 105.06215, 116.83354,
        95.91321, 103.83493, 107.79650, 111.57576, 112.16093, 104.30065, 104.62081,  89.44986, 104.53152),
      tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk[, -ncol(cntFrm)], 2, sd,   na.rm = TRUE)),
       c(9.24849,   7.41334,   7.66104,   6.74420,   7.47457,   9.51210,  10.06599,   9.95656,   7.32652,  10.28519,
         9.26913,   9.05088,  12.73900,   8.10547,   7.71139,   3.56249,  13.96156,   1.45965,  NA),
      tolerance = 1e-6)
    unlink(nmeOut)

    # =================================================================================================================
    # standardization methods: `none`, `z`, `sd`, `range`, `max`, `mean`, `rescale`; no calculation of distances
    # standardization: none, distance measure: none
    # with no standardization and no distance calculation, the output data set should be equal to the input data set
    expect_equal(cntFrm, rmvMsV(distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "none")))

    # standardization: z, distance measure: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "z", nmeDst = "none")
    expect_equal(dim(df4Chk), dim(cntFrm))
    expect_equal(unname(apply(df4Chk, 2, mean)), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, sd)),   rep(1, 20))
    expect_equal(as.vector(as.matrix(rmvMsV(df4Chk) / apply(cntFrm, 2, scale))), rep(1, 1000))
    unlink(nmeOut)

    # standardization: sd, distance measure: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "sd", nmeDst = "none")
    expect_equal(dim(df4Chk), dim(cntFrm))
    expect_equal(unname((apply(cntFrm, 2, mean) / apply(df4Chk, 2, mean)) / apply(cntFrm, 2, sd)), rep(1, 20), tolerance = 1e-2)
    expect_equal(unname(apply(df4Chk, 2, sd)),   rep(1, 20), tolerance = 1e-2)
    expect_equal(as.vector(as.matrix(rmvMsV(df4Chk) / apply(cntFrm, 2, scale, center = FALSE))), rep(1, 1000))
    unlink(nmeOut)

    # standardization: range, distance measure: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "range", nmeDst = "none")
    expect_equal(dim(df4Chk), dim(cntFrm))
    expect_equal(unname(apply(df4Chk, 2, mean)), as.vector(apply(cntFrm, 2, mean) / diff(apply(cntFrm, 2, range))))
    expect_equal(unname(apply(df4Chk, 2, sd)),   as.vector(apply(cntFrm, 2, sd)   / diff(apply(cntFrm, 2, range))))
    expect_equal(unname(apply(df4Chk, 2, max) - apply(df4Chk, 2, min)), rep(1, 20))
    unlink(nmeOut)

    # standardization: max, distance measure: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "max", nmeDst = "none")
    expect_equal(dim(df4Chk), dim(cntFrm))
    expect_equal(unname(apply(df4Chk, 2, mean)), unname(apply(cntFrm, 2, mean) / apply(abs(cntFrm), 2, max)))
    expect_equal(unname(apply(df4Chk, 2, sd)),   unname(apply(cntFrm, 2, sd)   / apply(abs(cntFrm), 2, max)))
    expect_equal(colSums(abs(rbind(unname(apply(df4Chk, 2, min)), unname(apply(df4Chk, 2, max)))) == 1), rep(1, 20))
    unlink(nmeOut)

    # standardization: mean, distance measure: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "mean", nmeDst = "none")
    expect_equal(dim(df4Chk), dim(cntFrm))
    expect_equal(unname(apply(df4Chk, 2, mean)), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, sd)),   unname(apply(cntFrm, 2, sd) / abs(apply(cntFrm, 2, mean))))
    unlink(nmeOut)

    # standardization: rescale, distance measure: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "rescale", nmeDst = "none")
    expect_equal(dim(df4Chk), dim(cntFrm))
    expect_equal(unname(apply(df4Chk, 2, min)),  rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, max)),  rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, sd)), as.vector(apply(cntFrm, 2, sd) / diff(apply(cntFrm, 2, range))))
    unlink(nmeOut)

    # =================================================================================================================
    # distance meausres for continuos data: euclid, seuclid, block, canberra, chebychev, minkowski_p, power_p_r, cosine,
    # correlation; no standardization
    # distance measure: euclid, standardization: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "euclid")
    expect_equal(dim(df4Chk), rep(dim(cntFrm)[2], 2))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)), c(90.18679, 93.75375, 93.49854,  95.84402, 100.25257, 96.79214,  95.25020, 96.61567, 97.88351, 108.10851,
                                                   91.54438, 96.83193, 99.66558, 104.26397, 102.46154, 99.06569, 104.29886, 89.90709, 97.13592, 101.83002),
                                                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd)),   c(23.05755, 23.22398, 23.30027,  23.63582,  24.78627, 24.50848,  24.47311, 24.23558, 24.02617,  27.30439,
                                                   23.24474, 24.65757, 25.18948,  25.89397,  25.80464, 24.56810,  27.01149, 22.32947, 24.55965,  25.65252),
                                                 tolerance = 1e-6)
    # euclidean == euclid
    expect_equal(distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "euclidean"), df4Chk)

    # distance measure: seuclid, standardization: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "seuclid")
    expect_equal(dim(df4Chk), rep(dim(cntFrm)[2], 2))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)), c(8638.72502, 9302.15162,  9257.73442,  9716.79546, 10634.21924,  9939.35126,  9641.58669, 9892.58203, 10129.57618, 12395.70229,
                                                   8893.67530, 9954.01874, 10536.01146, 11507.94836, 11130.95307, 10387.42201, 11571.39124, 8556.95973, 10008.40523, 10994.50290),
                                                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd)),   c(2706.95087, 2612.18298,  2657.96515,  2682.29126,  2974.69185,  2976.31068,  3042.57714, 2883.08202,  2770.73040,  3710.39530,
                                                   2683.03967, 3049.76569,  3146.80743,  3249.34860,  3258.55781,  2914.40577,  3724.65631, 2435.83129,  2965.04294,  3220.06283),
                                                 tolerance = 1e-6)
    # seuclidean == seuclid
    expect_equal(distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "seuclidean"), df4Chk)

    # distance measure: block, standardization: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "block")
    expect_equal(dim(df4Chk), rep(dim(cntFrm)[2], 2))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)), c(510.11848, 531.72723, 527.03184, 541.47101, 563.65123, 552.49981, 539.29376, 547.19063, 560.14147, 619.83929,
                                                   514.77227, 548.20019, 566.08846, 594.66377, 580.03450, 560.21567, 599.48532, 514.68472, 552.15867, 578.28306),
                                                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd)),   c(130.57798, 134.10532, 128.83121, 135.26859, 141.28170, 145.57155, 137.79444, 136.52438, 138.00417, 156.99662,
                                                   133.49714, 142.72953, 144.74001, 151.31945, 149.44571, 141.14608, 154.58736, 128.54925, 140.88010, 144.38364),
                                                 tolerance = 1e-6)
    # manhattan == block
    expect_equal(distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "manhattan"), df4Chk)

    # distance measure: canberra, standardization: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "canberra")
    expect_equal(dim(df4Chk), rep(dim(cntFrm)[2], 2))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)), c(34.49320, 34.03633, 34.97755, 33.71225, 34.58030, 34.32198, 33.97928, 34.50087, 33.79525, 35.26895,
                                                   34.28703, 32.95946, 34.23159, 34.90192, 33.03279, 34.09206, 34.74617, 33.79520, 34.19561, 35.30816),
                                                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd)),    c(8.47184,  8.47098,  8.32530,  8.30726,  8.40608,  8.44222,  8.41203,  8.31785,  8.30473,  8.58373,
                                                    8.39323,  8.19873,  8.37566,  8.47766,  8.23869,  8.30194,  8.54616,  8.17902,  8.31659,  8.51508),
                                                 tolerance = 1e-6)

    # distance measure: chebychev, standardization: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "chebychev")
    expect_equal(dim(df4Chk), rep(dim(cntFrm)[2], 2))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)), c(33.35363, 34.63043, 33.21321, 33.21751, 36.14409, 34.50494, 35.56581, 34.76410, 33.96757, 39.74970,
                                                   33.26305, 34.51619, 34.50881, 38.05685, 35.67290, 35.33634, 36.56601, 30.28183, 33.55651, 35.04377),
                                                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd)),   c(10.22961, 10.39461, 10.23548,  9.94799, 10.46579,  9.75279, 10.94785,  9.79454,  8.88057, 12.89452,
                                                    9.69607, 10.54373,  9.16828, 11.26589, 10.33764,  9.49605, 11.07437,  7.94100,  9.46650,  9.83562),
                                                 tolerance = 1e-6)

    # distance measure: minkowski_p, standardization: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "minkowski_3")
    expect_equal(dim(df4Chk), rep(dim(cntFrm)[2], 2))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)), c(54.67712, 56.99251, 56.82751, 57.96354, 61.19111, 58.47326, 57.96689, 58.45795, 58.71571, 65.33263,
                                                   55.87355, 58.55490, 59.94234, 63.21778, 61.92123, 60.09938, 62.57737, 53.89934, 58.56803, 61.46659),
                                                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd)),   c(14.18010, 14.04591, 14.62228, 14.40142, 15.23499, 14.66176, 15.14758, 14.87106, 14.46439, 16.73602,
                                                   14.03609, 15.05682, 15.14051, 15.63788, 15.78960, 14.90891, 16.55806, 13.43597, 14.88961, 15.68011),
                                                 tolerance = 1e-6)
    # minkowski_2 == euclid
    expect_equal(distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "minkowski_2"),
                 distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "euclid"))
    # minkowski_1 == block
    expect_equal(distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "minkowski_1"),
                 distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "block"))

    # distance measure: power_p_r, standardization: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "power_3_2")
    expect_equal(dim(df4Chk), rep(dim(cntFrm)[2], 2))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)), c(416.45985, 442.22845, 441.11214, 453.74088, 492.19694, 459.91073, 454.75426, 460.00974, 462.43128, 543.66607,
                                                   429.61070, 461.35882, 477.49091, 516.71302, 501.55846, 479.01644, 510.38992, 406.90613, 461.30873, 496.06172),
                                                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd)),   c(119.68786, 114.87371, 124.92054, 119.39272, 130.33552, 123.43098, 132.83875, 127.15831, 119.87500, 152.16527,
                                                   115.95281, 130.67957, 130.34089, 135.02989, 138.66486, 125.73364, 151.64728, 108.35405, 126.96224, 136.96509),
                                                 tolerance = 1e-6)
    # power_2_2 == minkowski_2
    expect_equal(distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "power_2_2"),
                 distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "minkowski_2"))
    # power_4_4 == minkowski_4
    expect_equal(distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "power_4_4"),
                 distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "minkowski_4"))

    # distance measure: cosine, standardization: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "cosine")
    expect_equal(dim(df4Chk), rep(dim(cntFrm)[2], 2))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)), c(0.0261721, 0.0727720, 0.0255531,  0.0594165, 0.0500769, 0.0181540, 0.0526752, 0.0438387, 0.1013408, 0.0327727,
                                                   0.0789248, 0.0405412, 0.0554114, -0.0059643, 0.0553022, 0.0349486, 0.0960476, 0.1117722, 0.0674172, 0.0028155),
                                                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd)),   c(0.2857365, 0.2518821, 0.2552830,  0.2632538, 0.2516072, 0.2659546, 0.2764758, 0.2578368, 0.2352152, 0.2818756,
                                                   0.2667546, 0.2699523, 0.2699902,  0.2642751, 0.2621859, 0.2623423, 0.2600199, 0.2525837, 0.2512283, 0.2679373),
                                                 tolerance = 1e-6)

    # distance measure: correlation, standardization: none
    df4Chk <- distances_omv(dtaInp = cntFrm, varDst = names(cntFrm), stdDst = "none", nmeDst = "correlation")
    expect_equal(dim(df4Chk), rep(dim(cntFrm)[2], 2))
    expect_equal(names(df4Chk), names(cntFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)), c(0.0282471, 0.0750318, 0.0246367,  0.0603677, 0.0499838, 0.0191165, 0.0545289, 0.0434007, 0.1012484, 0.0330809,
                                                   0.0784566, 0.0402646, 0.0545752, -0.0073858, 0.0559124, 0.0343765, 0.0965748, 0.1115375, 0.0698606, 0.0015639),
                                                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd)),   c(0.2866331, 0.2517791, 0.2594964,  0.2642852, 0.2517053, 0.2675609, 0.2756468, 0.2583735, 0.2358153, 0.2829945,
                                                   0.2672026, 0.2703650, 0.2736224,  0.2718538, 0.2623670, 0.2627199, 0.2594415, 0.2535597, 0.2497000, 0.2686121),
                                                 tolerance = 1e-6)

    # =================================================================================================================
    # distance measures for frequency data: chisq, ph2; no standardization
    # distance measure: chisq, standardization: none
    df4Chk <- distances_omv(dtaInp = frqFrm, varDst = names(frqFrm), stdDst = "none", nmeDst = "chisq")
    expect_equal(dim(df4Chk), rep(dim(frqFrm)[2], 2))
    expect_equal(names(df4Chk), names(frqFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(7.747222, 10.663834, 10.396158, 7.620559,  7.808307, 6.532916, 10.848895, 6.095187, 7.102744,  8.519774,
                                                    9.919973,  8.877894,  9.220077, 6.708244, 10.539968, 9.265459,  7.193613, 6.704473, 7.738743, 11.206640),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(3.220719,  4.143478,  4.850509, 3.472757,  3.441681, 3.346350,  4.644328, 2.437293, 3.515119,  4.077974,
                                                    3.678504,  4.087233,  4.132481, 2.957086,  4.274442, 3.846881,  3.410927, 2.797160, 4.067294,  4.667570),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)

    # distance measure: ph2, standardization: none
    df4Chk <- distances_omv(dtaInp = frqFrm, varDst = names(frqFrm), stdDst = "none", nmeDst = "ph2")
    expect_equal(dim(df4Chk), rep(dim(frqFrm)[2], 2))
    expect_equal(names(df4Chk), names(frqFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.774722, 1.066383, 1.039616, 0.762056, 0.780831, 0.653292, 1.084890, 0.609519, 0.710274, 0.851977,
                                                    0.991997, 0.887789, 0.922008, 0.670824, 1.053997, 0.926546, 0.719361, 0.670447, 0.773874, 1.120664),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.322072, 0.414348, 0.485051, 0.347276, 0.344168, 0.334635, 0.464433, 0.243729, 0.351512, 0.407797,
                                                    0.367850, 0.408723, 0.413248, 0.295709, 0.427444, 0.384688, 0.341093, 0.279716, 0.406729, 0.466757),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)

    # =================================================================================================================
    # distance measures for binary data: beuclid, blwmn, bseuclid, bshape, d, dice, disper, hamann, jaccard, k1, k2,
    # lambda, ochiai, pattern, phi, q, rr, rt, size, sm, ss1, ss2, ss3, ss4, ss5, variance, y; no standardization
    # distance measure: beuclid, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "beuclid")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(4.689500, 4.858037, 4.623153, 4.728271, 4.765985, 4.797172, 4.704819, 4.646293, 4.836648, 4.722894,
                                                    4.716488, 4.704718, 4.728416, 4.614760, 4.743886, 4.658794, 4.741526, 4.654649, 4.583997, 4.792678),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(1.151013, 1.191848, 1.112825, 1.166847, 1.185611, 1.186388, 1.176382, 1.152555, 1.195094, 1.189547,
                                                    1.171927, 1.176806, 1.166225, 1.148909, 1.190112, 1.145075, 1.155278, 1.162683, 1.141086, 1.160872),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "beuclid"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "beuclid_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "beuclid_1_2"))

    # distance measure: blwmn, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "blwmn")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.471614, 0.462666, 0.424808, 0.454607, 0.497064, 0.523678, 0.484746, 0.522466, 0.520449, 0.480162,
                                                    0.443808, 0.503348, 0.528201, 0.457711, 0.450691, 0.493759, 0.531400, 0.440961, 0.468459, 0.500525),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.133840, 0.134304, 0.113729, 0.135335, 0.143792, 0.150475, 0.143404, 0.146677, 0.143397, 0.147464,
                                                    0.131636, 0.144830, 0.142485, 0.134467, 0.140197, 0.139029, 0.141272, 0.134183, 0.133866, 0.131386),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "blwmn"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "blwmn_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "blwmn_1_2"))

    # distance measure: bseuclid, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "bseuclid")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(23.250000, 24.950000, 22.550000, 23.650000, 24.050000, 24.350000, 23.450000, 22.850000, 24.750000, 23.650000,
                                                    23.550000, 23.450000, 23.650000, 22.550000, 23.850000, 22.950000, 23.750000, 22.950000, 22.250000, 24.250000),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   -  c(6.373258,  6.816968,  5.789964,  6.579674,  6.870800,  6.753362,  6.770485,  6.474850,  6.912041,  7.005825,
                                                     6.692297,  6.739319,  6.531503,  6.411133,  6.937882,  6.303508,  6.323515,  6.613185,  6.364995,  6.323515),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "bseuclid"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "bseuclid_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "bseuclid_1_2"))

    # distance measure: bshape, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "bshape")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.460700, 0.482900, 0.438860, 0.464020, 0.476660, 0.480180, 0.464660, 0.440460, 0.489820, 0.468700,
                                                    0.458860, 0.462180, 0.460500, 0.446700, 0.464860, 0.452180, 0.462500, 0.450020, 0.439820, 0.480660),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.126045, 0.131225, 0.113546, 0.128046, 0.137094, 0.133547, 0.134332, 0.125128, 0.135745, 0.138601,
                                                    0.131514, 0.132424, 0.126058, 0.127841, 0.134177, 0.124929, 0.122934, 0.130107, 0.126138, 0.124965),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "bshape"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "bshape_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "bshape_1_2"))

    # distance measure: d, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "d")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), c(0.500000, 0.400000, 0.420000, 0.440000, 0.480000, 0.440000, 0.480000, 0.380000, 0.460000, 0.500000,
                                                    0.420000, 0.440000, 0.400000, 0.500000, 0.420000, 0.440000, 0.400000, 0.440000, 0.460000, 0.480000))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.063000, 0.036000, 0.028000, 0.039000, 0.059500, 0.041000, 0.057500, 0.031500, 0.049500, 0.073000,
                                                    0.039000, 0.044000, 0.031000, 0.068500, 0.045500, 0.039500, 0.029500, 0.043500, 0.052000, 0.045500),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.107415, 0.090403, 0.092884, 0.098723, 0.106152, 0.096349, 0.106715, 0.082925, 0.102211, 0.109405,
                                                    0.094195, 0.099441, 0.088133, 0.107863, 0.097142, 0.096979, 0.089058, 0.098209, 0.101650, 0.105555),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "d"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "d_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "d_1_2"))

    # distance measure: dice, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "dice")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.528386, 0.537334, 0.575192, 0.545393, 0.502936, 0.476322, 0.515254, 0.477534, 0.479551, 0.519838,
                                                    0.556192, 0.496652, 0.471799, 0.542289, 0.549309, 0.506241, 0.468600, 0.559039, 0.531541, 0.499475),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.133840, 0.134304, 0.113729, 0.135335, 0.143792, 0.150475, 0.143404, 0.146677, 0.143397, 0.147464,
                                                    0.131636, 0.144830, 0.142485, 0.134467, 0.140197, 0.139029, 0.141272, 0.134183, 0.133866, 0.131386),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "dice"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "dice_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "dice_1_2"))

    # distance measure: disper, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "disper")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), c(0.250000, 0.240000, 0.243600, 0.246400, 0.249600, 0.246400, 0.249600, 0.235600, 0.248400, 0.250000,
                                                    0.243600, 0.246400, 0.240000, 0.250000, 0.243600, 0.246400, 0.240000, 0.246400, 0.248400, 0.249600))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.017500, 0.001400, 0.025220, 0.014040, 0.009320, 0.005960, 0.015320, 0.020420, 0.002140, 0.013500,
                                                    0.015220, 0.014960, 0.012600, 0.024500, 0.012220, 0.019960, 0.011600, 0.021040, 0.027140, 0.007320),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.063733, 0.064905, 0.056230, 0.064434, 0.068640, 0.066833, 0.067634, 0.061603, 0.068427, 0.070058,
                                                    0.065290, 0.066312, 0.062216, 0.064111, 0.066939, 0.062432, 0.060675, 0.064851, 0.063193, 0.063035),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "disper"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "disper_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "disper_1_2"))

    # distance measure: hamann, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "hamann")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.070000, 0.002000, 0.098000, 0.054000, 0.038000, 0.026000, 0.062000, 0.086000, 0.010000, 0.054000,
                                                    0.058000, 0.062000, 0.054000, 0.098000, 0.046000, 0.082000, 0.050000, 0.082000, 0.110000, 0.030000),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.254930, 0.272679, 0.231599, 0.263187, 0.274832, 0.270134, 0.270819, 0.258994, 0.276482, 0.280233,
                                                    0.267692, 0.269573, 0.261260, 0.256445, 0.277515, 0.252140, 0.252941, 0.264527, 0.254600, 0.252941),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "hamann"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "hamann_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "hamann_1_2"))

    # distance measure: jaccard, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "jaccard")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.372793, 0.381299, 0.414720, 0.389012, 0.351160, 0.328574, 0.362228, 0.329147, 0.330369, 0.367062,
                                                    0.398811, 0.345661, 0.323499, 0.386091, 0.393669, 0.353358, 0.320593, 0.402027, 0.375827, 0.346207),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.161525, 0.162120, 0.146869, 0.161925, 0.169331, 0.173258, 0.168425, 0.171797, 0.169527, 0.170670,
                                                    0.159361, 0.169905, 0.169132, 0.162884, 0.166213, 0.165965, 0.168725, 0.161480, 0.162228, 0.161989),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "jaccard"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "jaccard_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "jaccard_1_2"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "jaccards"))
    # distance measure: jaccards, standardization: none
    expect_equal(df4Chk, distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "jaccards"))
    # distance measure: jaccard, standardization: none
    expect_true(all(df4Chk + distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "jaccardd") == 1))
    expect_equal(as.matrix(distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "jaccardd")),
                 as.matrix(stats::dist(t(as.matrix(binFrm)), "binary", upper = TRUE, diag = TRUE)))

    # distance measure: k1, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "k1")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_true(all(is.na(diag(as.matrix(df4Chk)))))
    expect_equal(unname(apply(df4Chk, 2, mean, na.rm = TRUE)),
                 c(0.529244, 0.554404, 0.634104, 0.575568, 0.481972, 0.429257, 0.509615, 0.429813, 0.430074, 0.525043,
                   0.601247, 0.468599, 0.413306, 0.570882, 0.596938, 0.483544, 0.406245, 0.614369, 0.539410, 0.461229),
                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd,   na.rm = TRUE)),
                 c(0.151432, 0.178762, 0.136927, 0.185283, 0.172207, 0.147623, 0.182650, 0.147587, 0.132521, 0.199638,
                   0.187701, 0.167360, 0.116168, 0.210129, 0.235243, 0.153660, 0.109127, 0.210847, 0.171373, 0.114502),
                 tolerance = 1e-5)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "k1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "k1_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "k1_1_2"))

    # distance measure: k2, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "k2")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.530571, 0.544611, 0.581644, 0.549715, 0.505157, 0.479658, 0.517415, 0.486805, 0.481851, 0.521943,
                                                    0.562362, 0.500027, 0.478165, 0.544657, 0.555061, 0.509834, 0.475082, 0.563743, 0.534299, 0.501551),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-3)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.133118, 0.131063, 0.111346, 0.133039, 0.143951, 0.150667, 0.143232, 0.147246, 0.142928, 0.146791,
                                                    0.129895, 0.144653, 0.141873, 0.134176, 0.137326, 0.139532, 0.141270, 0.132616, 0.134015, 0.130903),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-4)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "k2"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "k2_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "k2_1_2"))

    # distance measure: lambda, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "lambda")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.129500, 0.086240, 0.065687, 0.087403, 0.125925, 0.092580, 0.122388, 0.079794, 0.108648, 0.151168,
                                                    0.090533, 0.098449, 0.075527, 0.141509, 0.104574, 0.088939, 0.071768, 0.097606, 0.113185, 0.095457),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.214238, 0.224516, 0.221229, 0.223766, 0.220777, 0.218954, 0.222921, 0.218527, 0.222756, 0.219136,
                                                    0.223520, 0.225204, 0.220354, 0.216099, 0.229415, 0.220264, 0.222340, 0.222695, 0.220523, 0.219662),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "lambda"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "lambda_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "lambda_1_2"))

    # distance measure: ochiai, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ochiai")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.529476, 0.540947, 0.578398, 0.547543, 0.504044, 0.477983, 0.516332, 0.482130, 0.480698, 0.520888,
                                                    0.559257, 0.498333, 0.474961, 0.543470, 0.552167, 0.508030, 0.471819, 0.561378, 0.532916, 0.500511),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.133476, 0.132665, 0.112496, 0.134183, 0.143866, 0.150557, 0.143314, 0.146892, 0.143157, 0.147125,
                                                    0.130728, 0.144729, 0.142143, 0.134318, 0.138748, 0.139263, 0.141228, 0.133375, 0.133931, 0.131141),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ochiai"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ochiai_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "ochiai_1_2"))

    # distance measure: pattern, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "pattern")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.056840, 0.062640, 0.051000, 0.057800, 0.061240, 0.061920, 0.058260, 0.052060, 0.064500, 0.059520,
                                                    0.056680, 0.057600, 0.056860, 0.053680, 0.058420, 0.054740, 0.057080, 0.054580, 0.052060, 0.061520),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.021072, 0.023033, 0.016784, 0.021758, 0.025124, 0.022724, 0.023894, 0.021234, 0.024409, 0.025806,
                                                    0.023233, 0.022714, 0.020641, 0.021306, 0.024060, 0.020606, 0.019603, 0.022429, 0.021230, 0.019601),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "pattern"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "pattern_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "pattern_1_2"))

    # distance measure: phi, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "phi")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.069881, 0.006466, 0.103550, 0.056606, 0.037509, 0.024129, 0.061340, 0.085827, 0.008034, 0.053614,
                                                    0.062668, 0.060670, 0.052676, 0.098636, 0.050075, 0.081125, 0.048516, 0.085693, 0.109710, 0.028984),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.255569, 0.269311, 0.230680, 0.261642, 0.275595, 0.271525, 0.271811, 0.259880, 0.276028, 0.281245,
                                                    0.267533, 0.269014, 0.258417, 0.256921, 0.273895, 0.253377, 0.252156, 0.263321, 0.254730, 0.252888),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "phi"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "phi_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "phi_1_2"))

    # distance measure: q, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "q")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.089590, -0.032455, 0.158563, 0.063247, 0.029430, -0.003108, 0.072154, 0.121641, -0.029004, 0.059150,
                                                    0.076419,  0.067936, 0.054189, 0.141785, 0.050632,  0.110989, 0.047092, 0.118760,  0.165151, 0.007468),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.335152,  0.355854, 0.271792, 0.347915, 0.377516,  0.368495, 0.370935, 0.356046,  0.370574, 0.392907,
                                                    0.364581,  0.366533, 0.341529, 0.341989, 0.370806,  0.331959, 0.320924, 0.360723,  0.342162, 0.316062),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "q"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "q_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "q_1_2"))

    # distance measure: rr, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "rr")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_equal(unname(diag(as.matrix(df4Chk))), c(0.500000, 0.600000, 0.580000, 0.560000, 0.480000, 0.440000, 0.480000, 0.380000, 0.460000, 0.500000,
                                                    0.580000, 0.440000, 0.400000, 0.500000, 0.580000, 0.440000, 0.400000, 0.560000, 0.460000, 0.480000))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.263000, 0.296000, 0.310000, 0.289000, 0.245000, 0.222000, 0.251000, 0.207000, 0.228000, 0.259000,
                                                    0.300000, 0.231000, 0.209000, 0.270000, 0.297000, 0.236000, 0.208000, 0.296000, 0.253000, 0.243000),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.072627, 0.090286, 0.076365, 0.083974, 0.074516, 0.068947, 0.073549, 0.058138, 0.067870, 0.079598,
                                                    0.084355, 0.065687, 0.056373, 0.074410, 0.090443, 0.065727, 0.057455, 0.083754, 0.065943, 0.066578),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "rr"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "rr_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "rr_1_2"))

    # distance measure: rt, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "rt")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.378029, 0.348230, 0.389600, 0.371211, 0.364678, 0.359001, 0.375532, 0.385861, 0.352103, 0.372481,
                                                    0.373400, 0.375518, 0.371140, 0.391471, 0.368649, 0.383495, 0.368652, 0.384443, 0.397012, 0.359507),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.157430, 0.164380, 0.149532, 0.160539, 0.164160, 0.164271, 0.163041, 0.158383, 0.165378, 0.165616,
                                                    0.161833, 0.163321, 0.160556, 0.158189, 0.165618, 0.156568, 0.157813, 0.160687, 0.156697, 0.158442),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "rt"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "rt_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "rt_1_2"))

    # distance measure: size, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "size")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.004300, 0.016100, 0.012140, 0.008980, 0.004340, 0.006820, 0.004340, 0.016540, 0.005180, 0.004300,
                                                    0.012140, 0.006820, 0.012500, 0.004300, 0.012140, 0.006820, 0.012500, 0.008980, 0.005180, 0.004340),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.004173, 0.014547, 0.012014, 0.009555, 0.004605, 0.008452, 0.004605, 0.015917, 0.006262, 0.004173,
                                                    0.012014, 0.008452, 0.013361, 0.004173, 0.012014, 0.008452, 0.013361, 0.009555, 0.006262, 0.004605),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "size"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "size_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "size_1_2"))

    # distance measure: sm, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "sm")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.535000, 0.501000, 0.549000, 0.527000, 0.519000, 0.513000, 0.531000, 0.543000, 0.505000, 0.527000,
                                                    0.529000, 0.531000, 0.527000, 0.549000, 0.523000, 0.541000, 0.525000, 0.541000, 0.555000, 0.515000),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.127465, 0.136339, 0.115799, 0.131593, 0.137416, 0.135067, 0.135410, 0.129497, 0.138241, 0.140116,
                                                    0.133846, 0.134786, 0.130630, 0.128223, 0.138758, 0.126070, 0.126470, 0.132264, 0.127300, 0.126470),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "sm"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "sm_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "sm_1_2"))

    # distance measure: ss1, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ss1")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.689748, 0.658743, 0.703130, 0.682303, 0.674427, 0.669715, 0.685174, 0.696240, 0.661988, 0.680932,
                                                    0.683675, 0.685360, 0.682522, 0.701551, 0.677754, 0.695067, 0.681365, 0.694201, 0.706625, 0.672704),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.093731, 0.103262, 0.081069, 0.097985, 0.104996, 0.099912, 0.102170, 0.096130, 0.105630, 0.108121,
                                                    0.100670, 0.100162, 0.095781, 0.093540, 0.105752, 0.091856, 0.091631, 0.098304, 0.093489, 0.090954),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ss1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ss1_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "ss1_1_2"))

    # distance measure: ss2, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ss2")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.246189, 0.252788, 0.276838, 0.258670, 0.231113, 0.215139, 0.239207, 0.215393, 0.215954, 0.243143,
                                                    0.265948, 0.227137, 0.210972, 0.256644, 0.262973, 0.232265, 0.208855, 0.268798, 0.248634, 0.226481),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.183543, 0.183579, 0.174552, 0.182865, 0.188470, 0.191038, 0.187394, 0.190644, 0.189540, 0.188066,
                                                    0.181188, 0.188987, 0.189734, 0.184140, 0.185180, 0.186800, 0.189778, 0.182197, 0.183985, 0.185591),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ss2"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ss2_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "ss2_1_2"))

    # distance measure: ss3, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ss3")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_true(all(is.na(diag(as.matrix(df4Chk)))))
    expect_equal(unname(apply(df4Chk, 2, mean, na.rm = TRUE)),
                 c(1.078226, 0.936743, 1.125170, 1.048680, 1.021168, 0.994574, 1.079788, 1.124509, 0.958833, 1.067129,
                   1.063652, 1.081734, 1.049790, 1.159014, 1.048984, 1.107437, 1.029689, 1.122979, 1.185479, 0.982876),
                 tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd,   na.rm = TRUE)),
                 c(0.272941, 0.258805, 0.199197, 0.296731, 0.311652, 0.326131, 0.361464, 0.317131, 0.289661, 0.359135,
                   0.321630, 0.375726, 0.312558, 0.363458, 0.375300, 0.291035, 0.262476, 0.349591, 0.340473, 0.233838),
                 tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ss3"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ss3_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "ss3_1_2"))

    # distance measure: ss4, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ss4")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.534940, 0.503229, 0.551776, 0.528301, 0.518756, 0.512065, 0.530669, 0.542918, 0.504015, 0.526804,
                                                    0.531335, 0.530336, 0.526337, 0.549320, 0.525038, 0.540563, 0.524257, 0.542848, 0.554857, 0.514490),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.127788, 0.134661, 0.115341, 0.130823, 0.137800, 0.135765, 0.135909, 0.129949, 0.138015, 0.140627,
                                                    0.133769, 0.134508, 0.129212, 0.128462, 0.136950, 0.126689, 0.126081, 0.131663, 0.127367, 0.126445),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ss4"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ss4_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "ss4_1_2"))

    # distance measure: ss5, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ss5")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.299469, 0.264204, 0.311828, 0.291720, 0.284751, 0.275816, 0.296759, 0.301993, 0.269360, 0.294081,
                                                    0.294157, 0.294795, 0.286582, 0.315214, 0.288819, 0.303570, 0.283345, 0.307405, 0.320439, 0.277582),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.177577, 0.183828, 0.168467, 0.180540, 0.184313, 0.185869, 0.183833, 0.180206, 0.185693, 0.186395,
                                                    0.182245, 0.184307, 0.180703, 0.178606, 0.185307, 0.177226, 0.178403, 0.180671, 0.176808, 0.178720),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ss5"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ss5_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "ss5_1_2"))

    # distance measure: variance, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "variance")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(0, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.116250, 0.124750, 0.112750, 0.118250, 0.120250, 0.121750, 0.117250, 0.114250, 0.123750, 0.118250,
                                                    0.117750, 0.117250, 0.118250, 0.112750, 0.119250, 0.114750, 0.118750, 0.114750, 0.111250, 0.121250),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.031866, 0.034085, 0.028950, 0.032898, 0.034354, 0.033767, 0.033852, 0.032374, 0.034560, 0.035029,
                                                    0.033461, 0.033697, 0.032658, 0.032056, 0.034689, 0.031518, 0.031618, 0.033066, 0.031825, 0.031618),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "variance"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "variance_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "variance_1_2"))

    # distance measure: y, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "y")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(diag(as.matrix(df4Chk))), rep(1, 20))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.069727, 0.005456, 0.105310, 0.056502, 0.037736, 0.023116, 0.061273, 0.087993, 0.007216, 0.053119,
                                                    0.063911, 0.060523, 0.052541, 0.099385, 0.050221, 0.081807, 0.048464, 0.086953, 0.110664, 0.028590),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.256289, 0.271226, 0.231255, 0.262755, 0.276392, 0.273275, 0.272829, 0.263103, 0.276997, 0.282468,
                                                    0.269575, 0.270260, 0.260290, 0.257506, 0.275180, 0.254593, 0.253612, 0.264899, 0.255524, 0.253307),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "y"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "y_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm4, varDst = names(binFm4), stdDst = "none", nmeDst = "y_1_2"))

    # check mkeBin
    mt4Chk <- mkeBin(as.matrix(binFm5), 1, 2)
    expect_equal(dim(mt4Chk), dim(frqFrm))
    expect_equal(unname(apply(mt4Chk, 2, class)), rep("logical", dim(binFm3)[2]))
    expect_equal(unname(apply(mt4Chk, 2, table)), unname(apply(binFm5, 2, table)[c(2, 1), ]))


    # test cases for error messages ===================================================================================
    expect_error(distances_omv(fleInp = nmeInp, fleOut = nmeOut, varDst = names(cntFrm)),
      regexp = "^Please use the argument dtaInp instead of fleInp\\.")
    expect_error(distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst = c("A1", "A2", "A3")),
      regexp = "^Calling distances_omv requires giving at least two \\(valid\\) variables to calculate distances between\\.")
    expect_error(distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst = names(cntFrm)[1]),
      regexp = "^Calling distances_omv requires giving at least two \\(valid\\) variables to calculate distances between\\.")
    expect_error(distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst = names(cntFrm), stdDst = "wrong"),
      regexp = "^Invalid standardization: \\w+\\. See Details in the help for further information\\.")
    expect_error(distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst = names(cntFrm), nmeDst = "wrong"),
      regexp = "^Invalid distance measure: \\w+\\. See Details in the help for further information\\.")
    expect_error(clcFrq(as.matrix(frqFrm), "wrong"), regexp = "clcFrq: Method wrong is not implemented\\.")
    expect_error(mkeBin(apply(as.matrix(binFrm), 2, as.character), "TRUE", "FALSE"),
      regexp = paste("The input matrix for binary data either needs to be logical \\(then it will be kept as it is\\),",
                     "or numeric \\(where p and np are used to derive TRUE and FALSE\\)."))
    expect_error(mtcBin(binFm3[, 1], binFm3[, 1], "beuclid"),
      regexp = "mtcBin: Input columns to the calculation of binary measures must be logical\\.")
    expect_error(mtcBin(binFrm[, 1], binFrm[, 2], "wrong"), regexp = "mtcBin: Method wrong is not implemented\\.")
})
