test_that("distances_omv works", {
    set.seed(1)
    cntFrm <- stats::setNames(as.data.frame(matrix(rnorm(                 1000, sd = 10),        nrow = 50)), sprintf("C_%02d", seq(20)))
    frqFrm <- stats::setNames(as.data.frame(matrix(sample(seq(10),        1000, replace = TRUE), nrow = 50)), sprintf("C_%02d", seq(20)))
    binFrm <- stats::setNames(as.data.frame(matrix(sample(c(TRUE, FALSE), 1000, replace = TRUE), nrow = 50)), sprintf("C_%02d", seq(20)))
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
#    df4Chk <- distances_omv(dtaInp = frqFrm, varDst = names(frqFrm), stdDst = "none", nmeDst = "chisq")
#    expect_equal(dim(df4Chk), rep(dim(frqFrm)[2], 2))
#    expect_equal(names(df4Chk), names(frqFrm))
#    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
#    expect_equal(unname(apply(df4Chk, 2, mean)), c(0.0282471, 0.0750318, 0.0246367,  0.0603677, 0.0499838, 0.0191165, 0.0545289, 0.0434007, 0.1012484, 0.0330809,
#                                                   0.0784566, 0.0402646, 0.0545752, -0.0073858, 0.0559124, 0.0343765, 0.0965748, 0.1115375, 0.0698606, 0.0015639),
#                                                 tolerance = 1e-6)
#    expect_equal(unname(apply(df4Chk, 2, sd)),   c(0.2866331, 0.2517791, 0.2594964,  0.2642852, 0.2517053, 0.2675609, 0.2756468, 0.2583735, 0.2358153, 0.2829945,
#                                                   0.2672026, 0.2703650, 0.2736224,  0.2718538, 0.2623670, 0.2627199, 0.2594415, 0.2535597, 0.2497000, 0.2686121),
#                                                 tolerance = 1e-6) 

#    # distance measure: ph2, standardization: none
#    df4Chk <- distances_omv(dtaInp = frqFrm, varDst = names(frqFrm), stdDst = "none", nmeDst = "ph2")
#    expect_equal(dim(df4Chk), rep(dim(frqFrm)[2], 2))
#    expect_equal(names(df4Chk), names(frqFrm))
#    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
#    expect_equal(unname(apply(df4Chk, 2, mean)), c(0.0282471, 0.0750318, 0.0246367,  0.0603677, 0.0499838, 0.0191165, 0.0545289, 0.0434007, 0.1012484, 0.0330809,
#                                                   0.0784566, 0.0402646, 0.0545752, -0.0073858, 0.0559124, 0.0343765, 0.0965748, 0.1115375, 0.0698606, 0.0015639),
#                                                 tolerance = 1e-6)
#    expect_equal(unname(apply(df4Chk, 2, sd)),   c(0.2866331, 0.2517791, 0.2594964,  0.2642852, 0.2517053, 0.2675609, 0.2756468, 0.2583735, 0.2358153, 0.2829945,
#                                                   0.2672026, 0.2703650, 0.2736224,  0.2718538, 0.2623670, 0.2627199, 0.2594415, 0.2535597, 0.2497000, 0.2686121),
#                                                 tolerance = 1e-6) 

    # =================================================================================================================
    # distance measures for binary data: beuclid, blwmn, bseuclid, bshape, d, dice, disper, hamann, jaccard, k1, k2,
    # lambda, ochiai, pattern, phi, q, rr, rt, size, sm, ss1, ss2, ss3, ss4, ss5, variance, y; no standardization
    # distance measure: beuclid, standardization: none
#    df4Chk <- distances_omv(dtaInp = binFrm, varDst = names(binFrm), stdDst = "none", nmeDst = "ph2")
#    expect_equal(dim(df4Chk), rep(dim(binFrm)[2], 2))
#    expect_equal(names(df4Chk), names(binFrm))
#    expect_true(isSymmetric(unname(as.matrix(df4Chk))))
#    expect_equal(unname(apply(df4Chk, 2, mean)), c(0.0282471, 0.0750318, 0.0246367,  0.0603677, 0.0499838, 0.0191165, 0.0545289, 0.0434007, 0.1012484, 0.0330809,
#                                                   0.0784566, 0.0402646, 0.0545752, -0.0073858, 0.0559124, 0.0343765, 0.0965748, 0.1115375, 0.0698606, 0.0015639),
#                                                 tolerance = 1e-6)
#    expect_equal(unname(apply(df4Chk, 2, sd)),   c(0.2866331, 0.2517791, 0.2594964,  0.2642852, 0.2517053, 0.2675609, 0.2756468, 0.2583735, 0.2358153, 0.2829945,
#                                                   0.2672026, 0.2703650, 0.2736224,  0.2718538, 0.2623670, 0.2627199, 0.2594415, 0.2535597, 0.2497000, 0.2686121),
#                                                 tolerance = 1e-6) 

    # distance measure: blwmn, standardization: none


    # distance measure: bseuclid, standardization: none


    # distance measure: bshape, standardization: none


    # distance measure: d, standardization: none


    # distance measure: dice, standardization: none


    # distance measure: disper, standardization: none


    # distance measure: hamann, standardization: none


    # distance measure: jaccard, standardization: none


#"binary" = "jaccard" from stats::dist vs. jaccard here


    # distance measure: k1, standardization: none


    # distance measure: k2, standardization: none


    # distance measure: lambda, standardization: none


    # distance measure: ochiai, standardization: none


    # distance measure: pattern, standardization: none


    # distance measure: phi, standardization: none


    # distance measure: q, standardization: none


    # distance measure: rr, standardization: none


    # distance measure: rt, standardization: none


    # distance measure: size, standardization: none


    # distance measure: sm, standardization: none


    # distance measure: ss1, standardization: none


    # distance measure: ss2, standardization: none


    # distance measure: ss3, standardization: none


    # distance measure: ss4, standardization: none


    # distance measure: ss5, standardization: none


    # distance measure: variance, standardization: none





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

# TO-DO: l. 233, 255, 324

})
