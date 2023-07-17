recode <- function(crrCmd = c(), data = data.frame()) {
    rcdSpl <- c(regexpr("\\(", crrSPS)[[1]], max(gregexpr("\\)", crrSPS)[[1]]));
    rcdLst <- strsplit(strSpl(substr(crrSPS, rcdSpl[1] + 1, rcdSpl[2] - 1), "\\)\\s*\\("), "=");
    rcdVrO <- fixVar(strSpl(trimws(gsub("^RECODE", "",  substr(crrSPS, 1, rcdSpl[1] - 1))), "\\s+"), crrSPS, names(data));
    rcdVrT <-        strSpl(trimws(gsub("^\\s*INTO", "", substr(crrSPS, rcdSpl[2] + 1, nchar(crrSPS)))), "\\s+");
# TO-DO: handle variable not in the data - issue warning and remove the variable from rcdVrO

    if (identical(rcdVrT, character(0))) rcdVrT <- rcdVrO; # if the target variable is empty, recode into the original variable
    if (length(rcdVrO) != length(rcdVrT)) stop(sprintf("The number of original and target variables have to macht up:\n%s\n\n", crrSPS));

    rcdIsC <- function(x) ifelse(grepl("ELSE|SYSMIS", x), NA, grepl("'|\"", x));
    rcdTmp <- rep(list(rep(ifelse(rcdIsC(rcdLst[[1]][2]), ifelse(any(grepl("ELSE", rcdLst)),            gsub("'|\"", "", rcdLst[[grep("ELSE", rcdLst)]][[2]]), " "),
                                                          ifelse(any(grepl("ELSE", rcdLst)), suppressWarnings(as.numeric(rcdLst[[grep("ELSE", rcdLst)]][[2]])), 0)), dim(data)[1])), length(rcdVrO));
    for (j in seq_along(rcdLst)) {
        if (grepl("ELSE", rcdLst[[j]][1])) next
        rcdRpl <- ifelse(rcdIsC(rcdLst[[j]][2]), gsub("'|\"", "", rcdLst[[j]][2]), as.numeric(rcdLst[[j]][2]));
        if (grepl("character", class(data[[rcdVrO[1]]]))) {
            rcdGrp <- gsub("^\"|\"$", "", gsub("\"\\s*,\\s*\"", "|", gsub("'", "\"", rcdLst[[j]][1])));
            for (k in seq_along(rcdVrO)) {
                chkRcd(class(data[[rcdVrO[k]]]), class(rcdTmp[[k]]), rcdIsC(rcdLst[[j]][1]), rcdIsC(rcdLst[[j]][2]), rcdVrO[k], rcdVrT[k], crrSPS);
                rcdTmp[[k]] <- do_Rcd(rcdTmp[[k]], grepl(rcdGrp, data[[rcdVrO[k]]]), rcdRpl);

            }
        } else if (grepl("numeric|integer", class(data[[rcdVrO[1]]]))) {
            rcdSeq <- strSpl(rcdLst[[j]][1], ",|\\s+");
            if (any(grepl("THRU", rcdSeq))) {
                rcdSeq <- c(paste0(" >= ", rcdSeq[grep("THRU", rcdSeq) - 1]), paste0(" <= ", rcdSeq[grep("THRU", rcdSeq) + 1]));
                rcdClp <- " & ";
            } else {
                rcdSeq <- paste0(" == ", rcdSeq);
                rcdClp <- " | ";
            }
            for (k in seq_along(rcdVrO)) {
                chkRcd(class(data[[rcdVrO[k]]]), class(rcdTmp[[k]]), rcdIsC(rcdLst[[j]][1]), rcdIsC(rcdLst[[j]][2]), rcdVrO[k], rcdVrT[k], crrSPS);
                rcdTmp[[k]] <- do_Rcd(rcdTmp[[k]], eval(parse(text = paste(paste0("data[[\"", rcdVrO[k], "\"]]", rcdSeq), collapse = rcdClp))), rcdRpl);
            }
        } else {
           stop(sprintf("RECODE: Unsupported type of original column \"%s\" - \"%s\".", rcdVrO[k], class(data[[rcdVrO[k]]])));
        }
    }

    # transfer values from the temporary variable (rcdTmp)
    for (j in seq_along(rcdVrT)) {
        if (!any(grepl(rcdVrT[j], names(data)))) data[[rcdVrT[j]]] <- NA;
        data[[rcdVrT[j]]] <- rcdTmp[[j]];
    }

    data
}

chkRcd <- function(clsVrO = "", clsVrT = "", isCrpO = FALSE, isCrpT = FALSE, nmeClO = "", nmeClT = "", crrSPS = "") {
    if        (grepl("character",       clsVrO) && !is.na(isCrpO) && !isCrpO) {
        stop(sprintf("If the original data column (\"%\") is defined as string / character, the recode original terms also have to be characters:\n%s\n\n", nmeClO, crrSPS));
    } else if (grepl("numeric|integer", clsVrO) && !is.na(isCrpO) &&  isCrpO) {
        stop(sprintf("If the original data column (\"%\") is defined as numeric, the recode original terms also have to be numeric:\n%s\n\n",               nmeClO, crrSPS));
    } else if (grepl("character",       clsVrT) && !is.na(isCrpT) && !isCrpT) {
        stop(sprintf("If the target data column (\"%\") is defined as string / character, the recode target terms also have to be characters:\n%s\n\n",     nmeClT, crrSPS));
    } else if (grepl("numeric|integer", clsVrT) && !is.na(isCrpT) &&  isCrpT) {
        stop(sprintf("If the target data column (\"%\") is defined as numeric, the recode target terms also have to be numeric:\n%s\n\n",                   nmeClT, crrSPS));
    }
}

do_Rcd <- function(rcdCrC = c(), rcdSel = c(), rcdRpl = NULL) {
    if (is.character(rcdRpl)) {
        rcdCrC[rcdSel] <- rcdRpl;
    } else {
        rcdNA <- is.na(rcdCrC);
        # if cell contains NA, assign a value, otherwise add to the existing value
        rcdCrC[rcdSel &  rcdNA] <- rcdRpl;
        rcdCrC[rcdSel & !rcdNA] <- rcdCrC[rcdSel & !rcdNA] + rcdRpl;
    }
    rcdCrC
}
