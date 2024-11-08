test_that("distances_omv works", {
    set.seed(1)
    cntFrm <- stats::setNames(as.data.frame(matrix(rnorm(                             1000, sd = 10),        nrow = 50)), sprintf("C_%02d", seq(20)))
    frqFrm <- stats::setNames(as.data.frame(matrix(sample(seq(10),                    1000, replace = TRUE), nrow = 50)), sprintf("C_%02d", seq(20)))
    binFrm <- stats::setNames(as.data.frame(matrix(sample(c(TRUE, FALSE),             1000, replace = TRUE), nrow = 50)), sprintf("C_%02d", seq(20)))
    binFm2 <- as.data.frame(lapply(binFrm, function(c) as.integer(c)  + 1))
    binFm3 <- as.data.frame(lapply(binFrm, function(c) as.integer(!c) + 1))
    binFm4 <- stats::setNames(as.data.frame(matrix(sample(c(rep(1, 5), rep(2, 5), 4), 1000, replace = TRUE), nrow = 50)), sprintf("C_%02d", seq(20)))
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
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(4.202606, 5.031263, 4.561546, 4.390465, 4.005588, 3.954730, 4.526082, 3.744019, 3.954730, 4.105330,
                                                    4.436145, 3.850896, 4.344288, 3.577324, 4.827346, 4.481346, 3.903176, 3.577324, 3.797855, 4.909950),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.428094, 0.358931, 0.395096, 0.410159, 0.448696, 0.454345, 0.398126, 0.479391, 0.454345, 0.438020,
                                                    0.406025, 0.466344, 0.414425, 0.501314, 0.373782, 0.402016, 0.460223, 0.501314, 0.472727, 0.367620),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)

    # distance measure: ph2, standardization: none
    df4Chk <- distances_omv(dtaInp = frqFrm, varDst = names(frqFrm), stdDst = "none", nmeDst = "ph2")
    expect_equal(dim(df4Chk), rep(dim(frqFrm)[2], 2))
    expect_equal(names(df4Chk), names(frqFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.939731, 1.125025, 1.019993, 0.981738, 0.895677, 0.884305, 1.012063, 0.837188, 0.884305, 0.917980,
                                                    0.991952, 0.861086, 0.971412, 0.799914, 1.079427, 1.002059, 0.872777, 0.799914, 0.849226, 1.097898),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.095725, 0.080260, 0.088346, 0.091714, 0.100331, 0.101595, 0.089024, 0.107195, 0.101595, 0.097944,
                                                    0.090790, 0.104278, 0.092668, 0.112097, 0.083580, 0.089894, 0.102909, 0.112097, 0.105705, 0.082202),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6) 

    # =================================================================================================================
    # distance measures for binary data: beuclid, blwmn, bseuclid, bshape, d, dice, disper, hamann, jaccard, k1, k2,
    # lambda, ochiai, pattern, phi, q, rr, rt, size, sm, ss1, ss2, ss3, ss4, ss5, variance, y; no standardization
    # distance measure: beuclid, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "beuclid")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(4.689500, 4.858037, 4.623153, 4.728271, 4.765985, 4.797172, 4.704819, 4.646293, 4.836648, 4.722894,
                                                    4.716488, 4.704718, 4.728416, 4.614760, 4.743886, 4.658794, 4.741526, 4.654649, 4.583997, 4.792678),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(1.151013, 1.191848, 1.112825, 1.166847, 1.185611, 1.186388, 1.176382, 1.152555, 1.195094, 1.189547,
                                                    1.171927, 1.176806, 1.166225, 1.148909, 1.190112, 1.145075, 1.155278, 1.162683, 1.141086, 1.160872),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "beuclid_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "beuclid_1_2"))

    # distance measure: blwmn, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "blwmn")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.471614, 0.462666, 0.424808, 0.454607, 0.497064, 0.523678, 0.484746, 0.522466, 0.520449, 0.480162,
                                                    0.443808, 0.503348, 0.528201, 0.457711, 0.450691, 0.493759, 0.531400, 0.440961, 0.468459, 0.500525),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.133840, 0.134304, 0.113729, 0.135335, 0.143792, 0.150475, 0.143404, 0.146677, 0.143397, 0.147464,
                                                    0.131636, 0.144830, 0.142485, 0.134467, 0.140197, 0.139029, 0.141272, 0.134183, 0.133866, 0.131386),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "blwmn_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "blwmn_1_2"))

    # distance measure: bseuclid, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "bseuclid")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(23.250000, 24.950000, 22.550000, 23.650000, 24.050000, 24.350000, 23.450000, 22.850000, 24.750000, 23.650000,
                                                    23.550000, 23.450000, 23.650000, 22.550000, 23.850000, 22.950000, 23.750000, 22.950000, 22.250000, 24.250000),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   -  c(6.373258,  6.816968,  5.789964,  6.579674,  6.870800,  6.753362,  6.770485,  6.474850,  6.912041,  7.005825,
                                                     6.692297,  6.739319,  6.531503,  6.411133,  6.937882,  6.303508,  6.323515,  6.613185,  6.364995,  6.323515),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "bseuclid_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "bseuclid_1_2"))

    # distance measure: bshape, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "bshape")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.233340, 0.232340, 0.234860, 0.232820, 0.231700, 0.232500, 0.231620, 0.232220, 0.231820, 0.230620,
                                                    0.232140, 0.231780, 0.233060, 0.231980, 0.231180, 0.233220, 0.234180, 0.231700, 0.231580, 0.234580),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.055064, 0.055103, 0.055351, 0.055079, 0.054977, 0.054959, 0.055167, 0.054928, 0.055146, 0.054924,
                                                    0.055046, 0.055044, 0.055069, 0.055103, 0.055292, 0.055093, 0.055347, 0.054815, 0.054823, 0.055295),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "bshape_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "bshape_1_2"))

    # distance measure: d, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "d")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.038000, 0.016000, 0.007000, 0.017000, 0.035500, 0.019000, 0.033500, 0.012500, 0.026500, 0.048000,
                                                    0.018000, 0.022000, 0.011000, 0.043500, 0.024500, 0.017500, 0.009500, 0.021500, 0.029000, 0.021500),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.032216, 0.029092, 0.010809, 0.029218, 0.039266, 0.021981, 0.039507, 0.012513, 0.033916, 0.044674,
                                                    0.029128, 0.035034, 0.015183, 0.037735, 0.041228, 0.023141, 0.018202, 0.030997, 0.034012, 0.026611),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "d_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "d_1_2"))

    # distance measure: dice, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "dice")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.478386, 0.487334, 0.525192, 0.495393, 0.452936, 0.426322, 0.465254, 0.427534, 0.429551, 0.469838,
                                                    0.506192, 0.446652, 0.421799, 0.492289, 0.499309, 0.456241, 0.418600, 0.509039, 0.481541, 0.449475),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.135165, 0.139053, 0.134973, 0.143046, 0.135476, 0.132358, 0.139781, 0.128521, 0.125612, 0.145611,
                                                    0.143567, 0.134133, 0.121251, 0.141072, 0.149044, 0.131736, 0.118410, 0.146929, 0.136413, 0.120729),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "dice_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "dice_1_2"))

    # distance measure: disper, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "disper")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.005000, -0.010600, 0.013040, 0.001720, -0.003160, -0.006360,  0.002840, 0.008640, -0.010280,  0.001000,
                                                    0.003040,  0.002640, 0.000600, 0.012000,  0.000040,  0.007640, -0.000400, 0.008720,  0.014720, -0.005160),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.032687,  0.032633, 0.023002, 0.034069,  0.038902,  0.035582,  0.039166, 0.035125,  0.036446,  0.042538,
                                                    0.037064,  0.037818, 0.031717, 0.036070,  0.038921,  0.032562,  0.028132, 0.037366,  0.035960,  0.026885),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "disper_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "disper_1_2"))

    # distance measure: hamann, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "hamann")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.020000, -0.048000, 0.048000, 0.004000, -0.012000, -0.024000, 0.012000, 0.036000, -0.040000,  0.004000,
                                                    0.008000,  0.012000, 0.004000, 0.048000, -0.004000,  0.032000, 0.000000, 0.032000,  0.060000, -0.020000),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.130747,  0.138928, 0.093223, 0.140315,  0.155787,  0.142991, 0.156864, 0.144455,  0.149103,  0.170152,
                                                    0.150004,  0.154702, 0.136667, 0.144280,  0.163076,  0.130166, 0.118233, 0.152785,  0.145385,  0.108966),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "hamann_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "hamann_1_2"))

    # distance measure: jaccard, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "jaccard")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.322793, 0.331299, 0.364720, 0.339012, 0.301160, 0.278574, 0.312228, 0.279147, 0.280369, 0.317062,
                                                    0.348811, 0.295661, 0.273499, 0.336091, 0.343669, 0.303358, 0.270593, 0.352027, 0.325827, 0.296207),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.100342, 0.105624, 0.099811, 0.109111, 0.101854, 0.096653, 0.105986, 0.094329, 0.090839, 0.111818,
                                                    0.110058, 0.099949, 0.085995, 0.109130, 0.117483, 0.097348, 0.083375, 0.114591, 0.103032, 0.086140),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "jaccard_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "jaccard_1_2"))

#"binary" = "jaccard" from stats::dist vs. jaccard here


    # distance measure: k1, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "k1")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.502782, 0.526684, 0.602399, 0.546789, 0.457873, 0.407795, 0.484134, 0.408323, 0.408570, 0.498791,
                                                    0.571185, 0.445169, 0.392640, 0.542338, 0.567091, 0.459367, 0.385933, 0.583651, 0.512439, 0.438167),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.189023, 0.213640, 0.194594, 0.221556, 0.199272, 0.172796, 0.211165, 0.172837, 0.160890, 0.227027,
                                                    0.226831, 0.193686, 0.146034, 0.241092, 0.265035, 0.184552, 0.139763, 0.246960, 0.205842, 0.151846),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "k1_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "k1_1_2"))

    # distance measure: k2, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "k2")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.006397, 0.005759, 0.006191, 0.006076, 0.006324, 0.006403, 0.006435, 0.007086, 0.006275, 0.006299,
                                                    0.006030, 0.006628, 0.006794, 0.006518, 0.005965, 0.006732, 0.006757, 0.006185, 0.006789, 0.006313),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-3)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.001623, 0.001434, 0.001497, 0.001531, 0.001667, 0.001749, 0.001699, 0.001904, 0.001662, 0.001678,
                                                    0.001527, 0.001787, 0.001821, 0.001656, 0.001516, 0.001742, 0.001762, 0.001566, 0.001749, 0.001574),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-4)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "k2_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "k2_1_2"))

    # distance measure: lambda, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "lambda")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.079500, 0.036240, 0.015687, 0.037403, 0.075925, 0.042580, 0.072388, 0.029794, 0.058648, 0.101168,
                                                    0.040533, 0.048449, 0.025527, 0.091509, 0.054574, 0.038939, 0.021768, 0.047606, 0.063185, 0.045457),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.065317, 0.064979, 0.024359, 0.063312, 0.082065, 0.049225, 0.085519, 0.029840, 0.076117, 0.093113,
                                                    0.065020, 0.076267, 0.035254, 0.079571, 0.091521, 0.051139, 0.041551, 0.067855, 0.072675, 0.055105),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "lambda_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "lambda_1_2"))

    # distance measure: ochiai, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ochiai")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.479476, 0.490947, 0.528398, 0.497543, 0.454044, 0.427983, 0.466332, 0.432130, 0.430698, 0.470888,
                                                    0.509257, 0.448333, 0.424961, 0.493470, 0.502167, 0.458030, 0.421819, 0.511378, 0.482916, 0.450511),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.135229, 0.138847, 0.135190, 0.142751, 0.135983, 0.133110, 0.140094, 0.130632, 0.125819, 0.145647,
                                                    0.143860, 0.134683, 0.122218, 0.141370, 0.148696, 0.132695, 0.119780, 0.147032, 0.137007, 0.120914),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ochiai_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ochiai_1_2"))

    # distance measure: pattern, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "pattern")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.056840, 0.062640, 0.051000, 0.057800, 0.061240, 0.061920, 0.058260, 0.052060, 0.064500, 0.059520,
                                                    0.056680, 0.057600, 0.056860, 0.053680, 0.058420, 0.054740, 0.057080, 0.054580, 0.052060, 0.061520),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.021072, 0.023033, 0.016784, 0.021758, 0.025124, 0.022724, 0.023894, 0.021234, 0.024409, 0.025806,
                                                    0.023233, 0.022714, 0.020641, 0.021306, 0.024060, 0.020606, 0.019603, 0.022429, 0.021230, 0.019601),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "pattern_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "pattern_1_2"))

    # distance measure: phi, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "phi")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.019881, -0.043534, 0.053550, 0.006606, -0.012491, -0.025871,  0.011340, 0.035827, -0.041966,  0.003614,
                                                    0.012668,  0.010670, 0.002676, 0.048636,  0.000075,  0.031125, -0.001484, 0.035693,  0.059710, -0.021016),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.131941,  0.133962, 0.094075, 0.138390,  0.156965,  0.144923,  0.158351, 0.145975,  0.147560,  0.171695,
                                                    0.151351,  0.153270, 0.130617, 0.145354,  0.158198,  0.132197,  0.115872, 0.151971,  0.145509,  0.108351),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "phi_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "phi_1_2"))

    # distance measure: q, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "q")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.039590, -0.082455, 0.108563, 0.013247, -0.020570, -0.053108,  0.022154, 0.071641, -0.079004,  0.009150,
                                                    0.026419,  0.017936, 0.004189, 0.091785,  0.000632,  0.060989, -0.002908, 0.068760,  0.115151, -0.042532),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.257865,  0.260677, 0.187879, 0.269145,  0.300588,  0.283193,  0.299875, 0.290361,  0.281085,  0.324559,
                                                    0.292747,  0.293657, 0.259003, 0.276800,  0.295912,  0.258102,  0.229534, 0.295565,  0.281418,  0.213115),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "q_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "q_1_2"))

    # distance measure: rr, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "rr")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.238000, 0.266000, 0.281000, 0.261000, 0.221000, 0.200000, 0.227000, 0.188000, 0.205000, 0.234000,
                                                    0.271000, 0.209000, 0.189000, 0.245000, 0.268000, 0.214000, 0.188000, 0.268000, 0.230000, 0.219000),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.072808, 0.083376, 0.078533, 0.082200, 0.072104, 0.065855, 0.073205, 0.060663, 0.062870, 0.078432,
                                                    0.082711, 0.065687, 0.055998, 0.077017, 0.087876, 0.067465, 0.056717, 0.084455, 0.070038, 0.063071),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "rr_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "rr_1_2"))

    # distance measure: rt, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "rt")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.328029, 0.298230, 0.339600, 0.321211, 0.314678, 0.309001, 0.325532, 0.335861, 0.302103, 0.322481,
                                                    0.323400, 0.325518, 0.321140, 0.341471, 0.318649, 0.333495, 0.318652, 0.334443, 0.347012, 0.309507),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.096506, 0.091725, 0.090040, 0.097901, 0.100362, 0.097526, 0.104157, 0.102172, 0.095657, 0.106650,
                                                    0.101153, 0.104589, 0.097890, 0.104728, 0.104745, 0.098073, 0.091909, 0.104998, 0.105268, 0.087656),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "rt_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "rt_1_2"))

    # distance measure: size, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "size")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.004300, 0.016100, 0.012140, 0.008980, 0.004340, 0.006820, 0.004340, 0.016540, 0.005180, 0.004300,
                                                    0.012140, 0.006820, 0.012500, 0.004300, 0.012140, 0.006820, 0.012500, 0.008980, 0.005180, 0.004340),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.004173, 0.014547, 0.012014, 0.009555, 0.004605, 0.008452, 0.004605, 0.015917, 0.006262, 0.004173,
                                                    0.012014, 0.008452, 0.013361, 0.004173, 0.012014, 0.008452, 0.013361, 0.009555, 0.006262, 0.004605),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "size_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "size_1_2"))

    # distance measure: sm, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "sm")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.485000, 0.451000, 0.499000, 0.477000, 0.469000, 0.463000, 0.481000, 0.493000, 0.455000, 0.477000,
                                                    0.479000, 0.481000, 0.477000, 0.499000, 0.473000, 0.491000, 0.475000, 0.491000, 0.505000, 0.465000),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.131529, 0.126736, 0.126237, 0.132391, 0.135098, 0.130307, 0.137722, 0.136617, 0.130404, 0.140866,
                                                    0.135410, 0.137109, 0.131433, 0.137722, 0.137997, 0.132582, 0.126470, 0.138484, 0.139152, 0.122238),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "sm_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "sm_1_2"))

    # distance measure: ss1, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ss1")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.639748, 0.608743, 0.653130, 0.632303, 0.624427, 0.619715, 0.635174, 0.646240, 0.611988, 0.630932,
                                                    0.633675, 0.635360, 0.632522, 0.651551, 0.627754, 0.645067, 0.631365, 0.644201, 0.656625, 0.622704),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.161640, 0.157294, 0.159131, 0.161738, 0.163565, 0.158794, 0.165224, 0.165125, 0.159929, 0.167642,
                                                    0.163821, 0.164050, 0.160485, 0.165330, 0.165114, 0.162294, 0.157657, 0.165754, 0.166909, 0.154338),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ss1_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ss1_1_2"))

    # distance measure: ss2, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ss2")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.196189, 0.202788, 0.226838, 0.208670, 0.181113, 0.165139, 0.189207, 0.165393, 0.165954, 0.193143,
                                                    0.215948, 0.177137, 0.160972, 0.206644, 0.212973, 0.182265, 0.158855, 0.218798, 0.198634, 0.176481),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.065874, 0.071044, 0.065924, 0.073518, 0.067715, 0.062278, 0.070943, 0.061278, 0.058259, 0.075496,
                                                    0.074568, 0.066046, 0.054252, 0.075230, 0.081913, 0.063874, 0.052319, 0.078911, 0.068989, 0.054965),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ss2_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ss2_1_2"))

    # distance measure: ss3, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ss3")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(1.024315, 0.889906, 1.068912, 0.996246, 0.970109, 0.944846, 1.025799, 1.068283, 0.910891, 1.013773,
                                                    1.010469, 1.027647, 0.997300, 1.101063, 0.996535, 1.052065, 0.978204, 1.066830, 1.126205, 0.933733),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.358754, 0.327611, 0.317634, 0.372023, 0.379677, 0.387585, 0.426705, 0.398126, 0.354197, 0.423235,
                                                    0.393153, 0.438460, 0.384257, 0.438537, 0.434114, 0.376250, 0.343920, 0.422890, 0.424369, 0.316393),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ss3_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ss3_1_2"))

    # distance measure: ss4, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ss4")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.484940, 0.453229, 0.501776, 0.478301, 0.468756, 0.462065, 0.480669, 0.492918, 0.454015, 0.476804,
                                                    0.481335, 0.480336, 0.476337, 0.499320, 0.475038, 0.490563, 0.474257, 0.492848, 0.504857, 0.464490),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.131818, 0.125865, 0.126973, 0.132145, 0.135394, 0.130654, 0.138087, 0.137014, 0.129766, 0.141301,
                                                    0.136238, 0.136580, 0.129755, 0.138067, 0.136965, 0.132997, 0.125770, 0.138614, 0.139159, 0.121992),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ss4_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ss4_1_2"))

    # distance measure: ss5, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ss5")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.249469, 0.214204, 0.261828, 0.241720, 0.234751, 0.225816, 0.246759, 0.251993, 0.219360, 0.244081,
                                                    0.244157, 0.244795, 0.236582, 0.265214, 0.238819, 0.253570, 0.233345, 0.257405, 0.270439, 0.227582),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.088280, 0.079629, 0.077084, 0.089659, 0.093176, 0.091201, 0.098838, 0.094867, 0.087021, 0.102156,
                                                    0.094415, 0.098676, 0.086931, 0.099083, 0.097351, 0.090004, 0.079939, 0.098677, 0.098632, 0.076790),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "ss5_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "ss5_1_2"))

    # distance measure: variance, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "variance")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.116250, 0.124750, 0.112750, 0.118250, 0.120250, 0.121750, 0.117250, 0.114250, 0.123750, 0.118250,
                                                    0.117750, 0.117250, 0.118250, 0.112750, 0.119250, 0.114750, 0.118750, 0.114750, 0.111250, 0.121250),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.031866, 0.034085, 0.028950, 0.032898, 0.034354, 0.033767, 0.033852, 0.032374, 0.034560, 0.035029,
                                                    0.033461, 0.033697, 0.032658, 0.032056, 0.034689, 0.031518, 0.031618, 0.033066, 0.031825, 0.031618),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "variance_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "variance_1_2"))

    # distance measure: y, standardization: none
    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "y")
    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
    expect_equal(names(df4Chk), names(binFrm))
    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
    expect_equal(unname(apply(df4Chk, 2, mean)) - c(0.019727, -0.044544, 0.055310, 0.006502, -0.012264, -0.026884,  0.011273, 0.037993, -0.042784,  0.003119,
                                                    0.013911,  0.010523, 0.002541, 0.049385,  0.000221,  0.031807, -0.001536, 0.036953,  0.060664, -0.021410),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(unname(apply(df4Chk, 2, sd))   - c(0.133269,  0.137386, 0.096441, 0.140445,  0.158435,  0.147815,  0.160070, 0.152389,  0.149077,  0.173541,
                                                    0.155355,  0.155396, 0.134232, 0.146655,  0.160460,  0.134780,  0.118986, 0.155116,  0.147235,  0.109137),
                                                  rep(0, dim(frqFrm)[2]), tolerance = 1e-6)
    expect_equal(df4Chk, distances_omv(dtaInp = binFm2, varDst = names(binFm2), stdDst = "none", nmeDst = "y_2_1"))
    expect_equal(df4Chk, distances_omv(dtaInp = binFm3, varDst = names(binFm3), stdDst = "none", nmeDst = "y_1_2"))

    # check mkeBin
    mt4Chk <- mkeBin(as.matrix(binFm4), 1, 2)
    expect_equal(dim(mt4Chk), dim(frqFrm))
    expect_equal(unname(apply(mt4Chk, 2, class)), rep("logical", dim(binFm2)[2]))
    expect_equal(unname(apply(mt4Chk, 2, table)), unname(apply(binFm4, 2, table)[c(2, 1), ]))


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
    expect_error(mtcBin(binFm2[, 1], binFm2[, 1], "beuclid"),
      regexp = "mtcBin: Input columns to the calculation of binary measures must be logical\\.")
    expect_error(mtcBin(binFrm[, 1], binFrm[, 2], "wrong"), regexp = "mtcBin: Method wrong is not implemented\\.")
})
