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
    expect_error(describe_omv(dtaInp = jmvReadWrite::ToothGrowth[, c("len", "supp")], fleOut = nmeOut, dtaTtl = "ToothGrowth", dtaDsc = lstDsc),
      regexp = "^The variable description \\(in dtaDsc\\) should contain all variables in the data set and no variables not contained in it\\.")
    expect_error(describe_omv(dtaInp = jmvReadWrite::ToothGrowth[, c("len", "supp", "supp2", "dose")], fleOut = nmeOut, dtaTtl = "ToothGrowth", dtaDsc = lstDsc),
      regexp = "^The variable description \\(in dtaDsc\\) should contain all variables in the data set and no variables not contained in it\\.")
    expect_equal(defHdr("EN"), c(description = "Description", variables = "Variables", references = "References"))
    expect_equal(defHdr("DE"), c(description = "Beschreibung", variables = "Variablen", references = "Referenzen"))
    expect_equal(defHdr("JP"), c(description = "説明", variables = "変数", references = "引用文献"))
    expect_equal(defHdr("NB"), c(description = "Beskrivelse", variables = "Variabler", references = "Referanser"))
    expect_equal(defLic("CC0"),
                 paste("This data set is in the public domain, and the author has waived all of his or her rights to",
                       "the work. You can therefore copy, modify and distribute the data set, even for commercial",
                       "purposes, all without asking permission. If the data set is based upon empirical work, the",
                       "authors are given in the 'References'-section of the data set. In such case, please cite the",
                       "respective reference(s) when using the dataset."))
    expect_equal(defLic("CC0", crrLng = "DE"),
                 paste("Dieser Datensatz ist uneingeschränkt zugänglich, und der Autor hat auf alle seine Rechte an",
                       "dem Werk verzichtet. Sie können den Datensatz daher ohne vorherige Genehmigung kopieren,",
                       "verändern, und verbreiten, auch für kommerzielle Zwecke. Wenn der Datensatz auf empirischen",
                       "Daten basiert, sind die Autoren im Abschnitt 'Referenzen' des Datensatzes angegeben. Zitieren",
                       "Sie in diesem Fall bitte die entsprechenden Referenzen, wenn Sie den Datensatz verwenden."))
    expect_equal(defLic("CC0", crrLng = "EN"),
                 paste("This data set is in the public domain, and the author has waived all of his or her rights to",
                       "the work. You can therefore copy, modify and distribute the data set, even for commercial",
                       "purposes, all without asking permission. If the data set is based upon empirical work, the",
                       "authors are given in the 'References'-section of the data set. In such case, please cite the",
                       "respective reference(s) when using the dataset."))
    expect_error(defLic("CC0", crrLng = "JP"), "No translation available \\(yet\\) for JP\\.")
    expect_equal(defLic("CC0", crrLng = "NB"),
                 paste("Datasettet er offentlig tilgjengelig, og forfatteren har fraskrevet seg alle rettigheter til",
                       "datasettet. Du kan derfor kopiere, endre og distribuere datasettet, også til kommersielle",
                       "formål, uten å be om tillatelse. Hvis datasettet er empirisk, er forfatterne oppgitt under",
                       "'Referanser', vennligst siter de aktuelle referansene hvis det er tilfellet."))
    expect_error(defLic("CC0", crrLng = "XX"), "No translation available \\(yet\\) for XX\\.")
    expect_equal(defLic("DT_CC4-BY-NC-ND"),
                 paste("This data set contains data of a scientific study and the study authors therefore own the",
                       "copyright. Without the study authors` explicit consent, this data set may not be distributed",
                       "for commercial purposes, not be edited, and not be used without acknowledging its source",
                       "(i.e., the terms of a CC BY-NC-ND license)."))
    expect_equal(defLic("DT_CC4-BY-NC-ND", crrLng = "DE"),
                 paste("Dieser Datensatz enthält Daten einer wissenschaftlichen Studie, deren Urheberrecht bei den",
                       "Autoren der Studie liegt. Ohne die ausdrückliche Zustimmung der Autoren darf dieser Datensatz",
                       "nicht zu kommerziellen Zwecken verbreitet, bearbeitet oder verwendet werden ohne die Quelle",
                       "anzugeben (d. h. der Datensatz unterliegt den Bedingungen der CC BY-NC-ND-Lizenz)."))
    expect_equal(defLic("DT_CC4-BY-NC-ND", crrLng = "EN"),
                 paste("This data set contains data of a scientific study and the study authors therefore own the",
                       "copyright. Without the study authors` explicit consent, this data set may not be distributed",
                       "for commercial purposes, not be edited, and not be used without acknowledging its source",
                       "(i.e., the terms of a CC BY-NC-ND license)."))
    expect_error(defLic("DT_CC4-BY-NC-ND", crrLng = "JP"), "No translation available \\(yet\\) for JP\\.")
    expect_equal(defLic("DT_CC4-BY-NC-ND", crrLng = "NB"),
                 paste("Dette datasettet inneholder data fra en vitenskapelig studie, og studieforfatterne eier",
                       "derfor opphavsretten. Uten uttrykkelig samtykke fra studieforfatterne kan dette datasettet",
                       "ikke distribueres for kommersielle formål, ikke redigeres og ikke brukes uten å oppgi kilden",
                       "(dvs. vilkårene i en CC BY-NC-ND-lisens)."))
    expect_error(defLic("DT_CC4-BY-NC-ND", crrLng = "XX"), "No translation available \\(yet\\) for XX\\.")
    expect_equal(defLic("DT_CC4-BY-NC-ND", licHld = "John Doe"),
                 paste("This data set contains data of a scientific study and the study authors therefore own the",
                       "copyright. Without the study authors` and John Doe`s explicit consent, this data set may not",
                       "be distributed for commercial purposes, not be edited, and not be used without acknowledging",
                       "its source (i.e., the terms of a CC BY-NC-ND license)."))
    expect_equal(defLic("DT_CC4-BY-NC-ND", crrLng = "DE", licHld = "Max Mustermann"),
                 paste("Dieser Datensatz enthält Daten einer wissenschaftlichen Studie, deren Urheberrecht bei den",
                       "Autoren der Studie liegt. Ohne die ausdrückliche Zustimmung der Autoren und von Max",
                       "Mustermann darf dieser Datensatz nicht zu kommerziellen Zwecken verbreitet, bearbeitet oder",
                       "verwendet werden ohne die Quelle anzugeben (d. h. der Datensatz unterliegt den Bedingungen",
                       "der CC BY-NC-ND-Lizenz)."))
    expect_equal(defLic("DT_CC4-BY-NC-ND", crrLng = "EN", licHld = "John Doe"),
                 paste("This data set contains data of a scientific study and the study authors therefore own the",
                       "copyright. Without the study authors` and John Doe`s explicit consent, this data set may not",
                       "be distributed for commercial purposes, not be edited, and not be used without acknowledging",
                       "its source (i.e., the terms of a CC BY-NC-ND license)."))
    expect_error(defLic("DT_CC4-BY-NC-ND", crrLng = "JP", licHld = "\\u5c71\\u7530\\u592a\\u90ce"),
                 "No translation available \\(yet\\) for JP\\.")
    expect_equal(defLic("DT_CC4-BY-NC-ND", crrLng = "NB", licHld = "Kari Normann"),
                 paste("Dette datasettet inneholder data fra en vitenskapelig studie, og studieforfatterne eier",
                       "derfor opphavsretten. Uten uttrykkelig samtykke fra studieforfatterne og Kari Normann kan",
                       "dette datasettet ikke distribueres for kommersielle formål, ikke redigeres og ikke brukes",
                       "uten å oppgi kilden (dvs. vilkårene i en CC BY-NC-ND-lisens)."))
    expect_error(defLic("DT_CC4-BY-NC-ND", crrLng = "XX", licHld = "John Doe"),
                 "No translation available \\(yet\\) for XX\\.")
    expect_error(defLic("FC_CC4-BY-NC-ND"), "When using this license, a license holder needs to be defined\\.")
    expect_error(defLic("FC_CC4-BY-NC-ND", crrLng = "DE"),
                 "When using this license, a license holder needs to be defined\\.")
    expect_equal(defLic("FC_CC4-BY-NC-ND", licHld = "John Doe"),
                 paste("This data set was constructed by John Doe, who therefore owns the copyright. Without the",
                       "explicit consent of John Doe, this data set may not be distributed for commercial purposes,",
                       "not be edited, and not be used without acknowledging its source (i.e., the terms of the CC",
                       "BY-NC-ND license)."))
    expect_equal(defLic("FC_CC4-BY-NC-ND", crrLng = "DE", licHld = "Max Mustermann"),
                 paste("Dieser Datensatz wurde von Max Mustermann erstellt, der somit das Urheberrecht daran besitzt.",
                       "Ohne die ausdrückliche Zustimmung von Max Mustermann darf dieser Datensatz nicht für",
                       "kommerzielle Zwecke verbreitet, nicht bearbeitet und nicht ohne Angabe der Quelle verwendet",
                       "werden (d. h. der Datensatz unterliegt den Bedingungen der CC BY-NC-ND-Lizenz)."))
    expect_equal(defLic("FC_CC4-BY-NC-ND", crrLng = "EN", licHld = "John Doe"),
                 paste("This data set was constructed by John Doe, who therefore owns the copyright. Without the",
                       "explicit consent of John Doe, this data set may not be distributed for commercial purposes,",
                       "not be edited, and not be used without acknowledging its source (i.e., the terms of the CC",
                       "BY-NC-ND license)."))
    expect_error(defLic("FC_CC4-BY-NC-ND", crrLng = "JP", licHld = "\\u5c71\\u7530\\u592a\\u90ce"),
                 "No translation available \\(yet\\) for JP\\.")
    expect_equal(defLic("FC_CC4-BY-NC-ND", crrLng = "NB", licHld = "Kari Normann"),
                 paste("Datasettet er utarbeidet av Kari Normann, som derfor eier opphavsretten. Uten uttrykkelig",
                       "samtykke fra Kari Normann kan dette datasettet ikke distribueres for kommersielle formål,",
                       "redigeres eller brukes uten å oppgi kilden (dvs. vilkårene i CC BY-NC-ND-lisens)."))
    expect_error(defLic("FC_CC4-BY-NC-ND", crrLng = "XX", licHld = "John Doe"),
                 "No translation available \\(yet\\) for XX\\.")
    expect_error(defLic("RP_GPL2"),
                 "When using this license, the R-package where the data are originating from needs to be defined\\.")
    expect_error(defLic("RP_GPL2", crrLng = "DE"),
                 "When using this license, the R-package where the data are originating from needs to be defined\\.")
    expect_equal(defLic("RP_GPL2", licHld = "RPKG"),
                 paste("This data set was provided as part of the R-package 'RPKG', which is published under the",
                       "terms of the GNU General Public License 2.x. You may use the data both privately and",
                       "commercially, distribute or modify them. When using the data set, you need to disclose the",
                       "source, and publish any modifications you may make under the same license. If the data set",
                       "is based on empirical data, the authors are given in the 'References'-section. In such case,",
                       "please  cite them when using the dataset."))
    expect_equal(defLic("RP_GPL2", crrLng = "DE", licHld = "RPKG"),
                 paste("Dieser Datensatz ist Teil des R-Pakets 'RPKG, das unter den Bedingungen der GNU General",
                       "Public License 2.x veröffentlicht wurde. Sie können die Daten sowohl privat als auch",
                       "kommerziell nutzen, verbreiten oder verändern. Bei der Verwendung des Datensatzes müssen",
                       "Sie die  Quelle angeben und alle änderungen, die Sie vornehmen, unter derselben Lizenz",
                       "veröffentlichen. Wenn der Datensatz auf empirischer Forschung basiert, sind die Autoren",
                       "im Abschnitt 'Referenzen' angegeben. Zitieren Sie in diesem Fall die Studie, wenn Sie",
                       "den Datensatz verwenden."))
    expect_equal(defLic("RP_GPL2", crrLng = "EN", licHld = "RPKG"),
                 paste("This data set was provided as part of the R-package 'RPKG', which is published under the",
                       "terms of the GNU General Public License 2.x. You may use the data both privately and",
                       "commercially, distribute or modify them. When using the data set, you need to disclose the",
                       "source, and publish any modifications you may make under the same license. If the data set",
                       "is based on empirical data, the authors are given in the 'References'-section. In such case,",
                       "please  cite them when using the dataset."))
    expect_error(defLic("RP_GPL2", crrLng = "JP", licHld = "RPKG"),
                 "No translation available \\(yet\\) for JP\\.")
    expect_equal(defLic("RP_GPL2", crrLng = "NB", licHld = "RPKG"),
                 paste("Dette datasettet er en del av R-pakken 'RPKG', som er publisert under vilkårene i GNU General",
                       "Public License 2.x. Du kan bruke dataene både privat og kommersielt, distribuere eller endre",
                       "dem. Når du bruker datasettet, må du oppgi kilden og publisere eventuelle endringer du gjør",
                       "under samme lisens. Hvis datasettet er basert på empiriske data, er forfatterne oppgitt under",
                       "'Referanser'. Vennligst oppgi referansene i slike tilfeller når du bruker datasettet."))
    expect_error(defLic("RP_GPL2", crrLng = "XX", licHld = "RPKG"),
                 "No translation available \\(yet\\) for XX\\.")
    expect_error(defLic("RP_GPL3"),
                 "When using this license, the R-package where the data are originating from needs to be defined\\.")
    expect_equal(defLic("RP_GPL3", licHld = "RPKG"),
                 paste("This data set was provided as part of the R-package 'RPKG', which is published under the",
                       "terms of the GNU General Public License 3.0. You may use the data both privately and",
                       "commercially, distribute or modify them. When using the data set, you need to disclose the",
                       "source, and publish any modifications you may make under the same license. If the data set",
                       "is based on empirical data, the authors are given in the 'References'-section. In such case,",
                       "please  cite them when using the dataset."))
    expect_error(defLic("RP_GPL3", crrLng = "XX", licHld = "RPKG"),
                 "No translation available \\(yet\\) for XX\\.")
    expect_error(defLic("RP_AGPL3"),
                 "When using this license, the R-package where the data are originating from needs to be defined\\.")
    expect_equal(defLic("RP_AGPL3", licHld = "RPKG"),
                 paste("This data set was provided as part of the R-package 'RPKG', which is published under the",
                       "terms of the GNU Affero General Public License 3.0. You may use the data both privately and",
                       "commercially, distribute or modify them. When using the data set, you need to disclose the",
                       "source, and publish any modifications you may make under the same license. If the data set",
                       "is based on empirical data, the authors are given in the 'References'-section. In such case,",
                       "please  cite them when using the dataset."))
    expect_error(defLic("RP_AGPL3", crrLng = "XX", licHld = "RPKG"),
                 "No translation available \\(yet\\) for XX\\.")
    expect_error(defLic("RP_LGPL3"),
                 "When using this license, the R-package where the data are originating from needs to be defined\\.")
    expect_equal(defLic("RP_LGPL3", licHld = "RPKG"),
                 paste("This data set was provided as part of the R-package 'RPKG', which is published under the",
                       "terms of the GNU Lesser General Public License 3.0. You may use the data both privately and",
                       "commercially, distribute or modify them. When using the data set, you need to disclose the",
                       "source, and publish any modifications you may make under the same license. If the data set",
                       "is based on empirical data, the authors are given in the 'References'-section. In such case,",
                       "please  cite them when using the dataset."))
    expect_error(defLic("RP_LGPL3", crrLng = "XX", licHld = "RPKG"),
                 "No translation available \\(yet\\) for XX\\.")
    expect_null(defLic())
    expect_null(defLic(licNme = ""))
    expect_null(defLic(licNme = "", crrLng = "DE", licHld = "Max Mustermann"))
    expect_null(defLic(licNme = "", crrLng = "EN", licHld = "John Doe"))
    expect_null(defLic(crrLng = "DE"))
    expect_null(defLic(crrLng = "DE", licHld = "Max Mustermann"))
    expect_null(defLic(licHld = "John Doe"))
    if (packageVersion("jmvcore") >= "2.4.3") {
        expect_warning(df4Chk <- describe_omv(dtaInp = file.path("..", "ToothGrowth.omv"), dtaTtl = "ToothGrowth"),
          regexp = "^The data set contains analyses\\. Those will be overwritten\\.")
        expect_s3_class(df4Chk, "data.frame")
        expect_equal(dim(df4Chk), c(60, 16))
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
