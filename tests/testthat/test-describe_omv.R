test_that("describe_omv works", {
    nmeOut <- tempfile(fileext = ".omv")
    lstDsc <- list(description = paste("The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. Each animal received one of three dose",
                                       "levels of vitamin C (0.5, 1, and 2 mg / day) by one of two delivery methods, orange juice or ascorbic acid (a form of vitamin C and coded as VC)."),
                   variables = list(len = "Tooth length",
                                    supp = "Supplement type (VC or OJ)",
                                    dose = "Dose (in milligrams / day"),
                   references = paste("Crampton, E. W. (1947). The growth of the odontoblast of the incisor teeth as a criterion of vitamin C intake of the guinea pig.",
                                      "<em>The Journal of Nutrition, 33</em>(5), 491-504. https://doi.org/10.1093/jn/33.5.491"),
                   license = "The dataset was originally a part of the R-package datasets, and should therefore be licensed under GPL-3.")
    chrDsc <- paste0("<p><strong>Trial – all formattings:</strong><br/><strong>bold</strong><br/><strong><em>bold, italics</em></strong><br/><em>italics</em><br/><u>underlined</u><br/>",
                     "link: <a href=\"https://jamovi.org﻿﻿﻿\" target=\"_blank\">https://jamovi.org﻿﻿﻿</a><br/><s>strikethrough</s><br/>H<sub>2</sub>CO<sub>3</sub><br/>R<sup>2</sup><br/>",
                     "<span style=\"background-color:#e60000\">background colour: red</span><br/><span style=\"color:#e60000\">foreground color: red</span></p><p class=\"ql-align-center\">",
                     "centered</p><p class=\"ql-align-right\">right</p><p class=\"ql-align-justify\">justify justify justify justify justify justify justify justify justify justify ",
                     "justify justify justify justify justify justify justify justify justify justify justify justify justify justify justify</p><p><br/></p><ol><li>numbered list</li>",
                     "<li>numbered list</li></ol><p><br/></p><ul><li>bullet point</li><li>bullet point</li></ul><p class=\"ql-indent-1\">indented once</p><p class=\"ql-indent-2\">indented ",
                     "twice</p><p class=\"ql-indent-1\">indented once</p><p>Formula: <span class=\"ql-formula\">e=mc^2</span></p><pre>Preformatted</pre><p>normal again</p><h2>Heading</h2>")

    if (packageVersion("jmvcore") >= "2.4.3") {
        describe_omv(dtaInp = jmvReadWrite::ToothGrowth[, c("len", "supp", "dose")], fleOut = nmeOut, dtaTtl = "ToothGrowth", dtaDsc = lstDsc)
        expect_true(file.exists(nmeOut))
        expect_gt(file.info(nmeOut)$size, 1)
        expect_true(chkFle(nmeOut, isZIP = TRUE))
        expect_true(chkFle(nmeOut, fleCnt = "01 empty/analysis"))
        df4Chk <- read_omv(fleInp = nmeOut, getSyn = TRUE, getHTM = TRUE)
        expect_s3_class(df4Chk, "data.frame")
        expect_equal(dim(df4Chk), c(60, 3))
        expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("double", "integer", "double"))
        expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "removedRows", "addedRows", "transforms", "protobuf", "HTML"))
        expect_type(attr(df4Chk, "protobuf"), "list")
        expect_length(attr(df4Chk, "protobuf"), 1)
        expect_s4_class(attr(df4Chk, "protobuf")[[1]], "Message")
        expect_length(attr(df4Chk, "protobuf")[[1]], 8)
        expect_s4_class(attr(df4Chk, "protobuf")[[1]]$options, "Message")
        expect_length(attr(df4Chk, "protobuf")[[1]]$options, 3)
        expect_equal(vapply(attr(df4Chk, "protobuf")[[1]]$options$options, class, character(1)), rep("Message", 2))
        expect_length(attr(df4Chk, "protobuf")[[1]]$options$options, 2)
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$names, c("results//topText", "results//heading"))
        expect_length(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options, 24)
        expect_equal(vapply(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options, class, character(1)), rep("Message", 24))
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options[[1]]$c$names, c("attributes", "insert"))
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options[[1]]$c$as.character(),
          paste0("options {\n  c {\n    options {\n      o: TRUE\n    }\n    hasNames: true\n    names: \"bold\"\n  }\n}\n",
                 "options {\n  s: \"Description:\"\n}\nhasNames: true\nnames: \"attributes\"\nnames: \"insert\"\n"))
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options[[2]]$c$as.character(),
          "options {\n  s: \"\\n\"\n}\nhasNames: true\nnames: \"insert\"\n")
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[2]]$s, "ToothGrowth")
        unlink(nmeOut)

        describe_omv(dtaInp = jmvReadWrite::ToothGrowth[, c("len", "supp", "dose")], fleOut = nmeOut, dtaTtl = "ToothGrowth", dtaDsc = chrDsc)
        expect_true(file.exists(nmeOut))
        expect_gt(file.info(nmeOut)$size, 1)
        expect_true(chkFle(nmeOut, isZIP = TRUE))
        expect_true(chkFle(nmeOut, fleCnt = "01 empty/analysis"))
        df4Chk <- read_omv(fleInp = nmeOut, getSyn = TRUE, getHTM = TRUE)
        expect_s3_class(df4Chk, "data.frame")
        expect_equal(dim(df4Chk), c(60, 3))
        expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("double", "integer", "double"))
        expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "removedRows", "addedRows", "transforms", "protobuf", "HTML"))
        expect_type(attr(df4Chk, "protobuf"), "list")
        expect_length(attr(df4Chk, "protobuf"), 1)
        expect_s4_class(attr(df4Chk, "protobuf")[[1]], "Message")
        expect_length(attr(df4Chk, "protobuf")[[1]], 8)
        expect_s4_class(attr(df4Chk, "protobuf")[[1]]$options, "Message")
        expect_length(attr(df4Chk, "protobuf")[[1]]$options, 3)
        expect_equal(vapply(attr(df4Chk, "protobuf")[[1]]$options$options, class, character(1)), rep("Message", 2))
        expect_length(attr(df4Chk, "protobuf")[[1]]$options$options, 2)
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$names, c("results//topText", "results//heading"))
        expect_length(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options, 46)
        expect_equal(vapply(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options, class, character(1)), rep("Message", 46))
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options[[1]]$c$names, c("attributes", "insert"))
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options[[1]]$c$as.character(),
          paste0("options {\n  c {\n    options {\n      o: TRUE\n    }\n    hasNames: true\n    names: \"bold\"\n  }\n}\n",
                 "options {\n  s: \"Trial \\342\\200\\223 all formattings:\"\n}\nhasNames: true\nnames: \"attributes\"\nnames: \"insert\"\n"))
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options[[2]]$c$as.character(),
          "options {\n  s: \"\\n\"\n}\nhasNames: true\nnames: \"insert\"\n")
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[2]]$s, "ToothGrowth")
        unlink(nmeOut)

        describe_omv(dtaInp = jmvReadWrite::ToothGrowth[, c("len", "supp", "dose")], fleOut = nmeOut, dtaTtl = "ToothGrowth")
        expect_true(file.exists(nmeOut))
        expect_gt(file.info(nmeOut)$size, 1)
        expect_true(chkFle(nmeOut, isZIP = TRUE))
        expect_true(chkFle(nmeOut, fleCnt = "01 empty/analysis"))
        df4Chk <- read_omv(fleInp = nmeOut, getSyn = TRUE, getHTM = TRUE)
        expect_s3_class(df4Chk, "data.frame")
        expect_equal(dim(df4Chk), c(60, 3))
        expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("double", "integer", "double"))
        expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "removedRows", "addedRows", "transforms", "protobuf", "HTML"))
        expect_type(attr(df4Chk, "protobuf"), "list")
        expect_length(attr(df4Chk, "protobuf"), 1)
        expect_s4_class(attr(df4Chk, "protobuf")[[1]], "Message")
        expect_length(attr(df4Chk, "protobuf")[[1]], 8)
        expect_s4_class(attr(df4Chk, "protobuf")[[1]]$options, "Message")
        expect_length(attr(df4Chk, "protobuf")[[1]]$options, 3)
        expect_equal(vapply(attr(df4Chk, "protobuf")[[1]]$options$options, class, character(1)), rep("Message", 1))
        expect_length(attr(df4Chk, "protobuf")[[1]]$options$options, 1)
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$names, c("results//heading"))
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$s, "ToothGrowth")
        unlink(nmeOut)

        describe_omv(dtaInp = jmvReadWrite::ToothGrowth[, c("len", "supp", "dose")], fleOut = nmeOut, dtaDsc = chrDsc)
        expect_true(file.exists(nmeOut))
        expect_gt(file.info(nmeOut)$size, 1)
        expect_true(chkFle(nmeOut, isZIP = TRUE))
        expect_true(chkFle(nmeOut, fleCnt = "01 empty/analysis"))
        df4Chk <- read_omv(fleInp = nmeOut, getSyn = TRUE, getHTM = TRUE)
        expect_s3_class(df4Chk, "data.frame")
        expect_equal(dim(df4Chk), c(60, 3))
        expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("double", "integer", "double"))
        expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "removedRows", "addedRows", "transforms", "protobuf", "HTML"))
        expect_type(attr(df4Chk, "protobuf"), "list")
        expect_length(attr(df4Chk, "protobuf"), 1)
        expect_s4_class(attr(df4Chk, "protobuf")[[1]], "Message")
        expect_length(attr(df4Chk, "protobuf")[[1]], 8)
        expect_s4_class(attr(df4Chk, "protobuf")[[1]]$options, "Message")
        expect_length(attr(df4Chk, "protobuf")[[1]]$options, 3)
        expect_equal(vapply(attr(df4Chk, "protobuf")[[1]]$options$options, class, character(1)), rep("Message", 1))
        expect_length(attr(df4Chk, "protobuf")[[1]]$options$options, 1)
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$names, c("results//topText"))
        expect_length(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options, 46)
        expect_equal(vapply(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options, class, character(1)), rep("Message", 46))
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options[[1]]$c$names, c("attributes", "insert"))
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options[[1]]$c$as.character(),
          paste0("options {\n  c {\n    options {\n      o: TRUE\n    }\n    hasNames: true\n    names: \"bold\"\n  }\n}\n",
                 "options {\n  s: \"Trial \\342\\200\\223 all formattings:\"\n}\nhasNames: true\nnames: \"attributes\"\nnames: \"insert\"\n"))
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$c$options[[1]]$c$options[[2]]$c$as.character(),
          "options {\n  s: \"\\n\"\n}\nhasNames: true\nnames: \"insert\"\n")
        unlink(nmeOut)
    } else {
        expect_warning(expect_null(describe_omv(dtaInp = jmvReadWrite::ToothGrowth[, c("len", "supp", "dose")], fleOut = nmeOut, dtaTtl = "ToothGrowth", dtaDsc = lstDsc)))
        expect_false(file.exists(nmeOut))
        expect_warning(expect_null(describe_omv(dtaInp = jmvReadWrite::ToothGrowth[, c("len", "supp", "dose")], fleOut = nmeOut, dtaTtl = "ToothGrowth", dtaDsc = chrDsc)))
        expect_false(file.exists(nmeOut))
    }

    # code coverage
    expect_error(describe_omv(dtaInp = jmvReadWrite::ToothGrowth[, c("len", "supp", "dose")], fleOut = nmeOut),
      regexp = "^Calling describe_omv requires either the parameter dtaTtl \\(character vector\\) or the parameter dtaDsc \\(character vector or named list\\)\\.")
    expect_error(describe_omv(fleInp = file.path("..", "ToothGrowth.omv"), fleOut = nmeOut, dtaTtl = "ToothGrowth"), regexp = "Please use the argument dtaInp instead of fleInp\\.")
    if (packageVersion("jmvcore") >= "2.4.3") {
        expect_warning(df4Chk <- describe_omv(dtaInp = file.path("..", "ToothGrowth.omv"), dtaTtl = "ToothGrowth"),
          regexp = "^The data set contains analyses\\. Those will be overwritten\\.")
        expect_s3_class(df4Chk, "data.frame")
        expect_equal(dim(df4Chk), c(60, 17))
        expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "fltLst", "removedRows", "addedRows", "transforms", "HTML", "protobuf"))
        expect_length(attr(df4Chk, "protobuf")[[1]]$options$options, 1)
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$names, c("results//heading"))
        expect_equal(attr(df4Chk, "protobuf")[[1]]$options$options[[1]]$s, "ToothGrowth")
        expect_warning(df4Chk <- describe_omv(dtaInp = file.path("..", "ToothGrowth.omv"), fleOut = file.path("..", "ToothGrowth.omv"), dtaTtl = "ToothGrowth"),
          regexp = "^The data set contains analyses\\. Those will be overwritten\\.")
        expect_null(df4Chk)
        expect_error(splHTM("<p Trial with incomplete HTML tags.</p>"), regexp = paste("^Error when decoding HTML\\. Please check whether the HTML-string the you used for the description",
          "is valid \\(i\\.e\\., no typos, etc\\.\\)\\. If it is valid, send it to sebastian\\.jentschke@uib\\.no"))
        expect_equal(splHTM("Trial without HTML tags."), "Trial without HTML tags.")
        expect_warning(getAtt(list("<p>", "Trial", "</trial>"), 3, list()), regexp = "^The HTML tag \".*?\" isn't implemented, and hence the jamovi formatting attributes remain unchanged.")
        expect_warning(addHTM(gsub("</p>", "</p mismatch>", htmTxt()), "Title", "Description"), regexp = "^Unexpected HTML to be replaced with description:")
        suppressWarnings(expect_equal(getAtt(list("<p>", "Trial", "</trial>"), 3, list(bold = TRUE)), list(bold = TRUE)))
        suppressWarnings(expect_equal(getAtt(list("<p>", "Trial", "</trial>"), 3, list()), list()))
        rm(df4Chk)
    }

    rm(nmeOut, lstDsc, chrDsc)
})
