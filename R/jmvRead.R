jmvRead <- function(fleNme = "", useFlt = FALSE, sveAtt = FALSE) {
# read files created of jamovi (statistical spreadsheet, www.jamovi.org)
# fleNme - name of the jamovi file (.omv; default = "")
# useFlt - apply filters (default: FALSE; i.e., remove the lines where the filter is set to 0)
# sveAtt - store attributes that are not required in the data set
# 
# (C) Sebastian Jentschke, 2020-
# License: GPL-3
  
    # check whether the file exists and whether it has the correct format
    if (! file.exists(fleNme))                                                    { stop(paste0('File "', fleNme, '" not found.')) }
    hdrStr = readBin(tmpHdl <- file(fleNme, 'rb'), 'character'); close(tmpHdl); rm('tmpHdl');
    if (! hdrStr == "PK\003\004\024")                                             { stop(paste0('File "', fleNme, '" has not the correct file format (is not a ZIP archive).')) }
    if (! grepl('META-INF/MANIFEST.MF', toString(unzip(fleNme, list=TRUE)$Name))) { stop(paste0('File "', fleNme, '" has not the correct file format (is not a jamovi-file).')) }
    
    library(rjson)

    mnfTxt <-          readLines(mnfHdl <- file(mnfFle <- unzip(fleNme, 'META-INF/MANIFEST.MF', junkpaths = T), 'r'), warn=F);  close(mnfHdl); unlink(mnfFle); rm('mnfHdl', 'mnfFle');
    mtaDta <- fromJSON(readLines(mtaHdl <- file(mtaFle <- unzip(fleNme, 'metadata.json',        junkpaths = T), 'r'), warn=F)); close(mtaHdl); unlink(mtaFle); rm('mtaHdl', 'mtaFle');
    xtdDta <- fromJSON(readLines(xtdHdl <- file(xtdFle <- unzip(fleNme, 'xdata.json',           junkpaths = T), 'r'), warn=F)); close(xtdHdl); unlink(xtdFle); rm('xtdHdl', 'xtdFle');
                                 binHdl <- file(binFle <- unzip(fleNme, 'data.bin',             junkpaths = T), 'rb');
    # TO-DO: import HTML output - index.html
   
    # process meta-data
    if (names(mtaDta) != "dataSet") { stop('Unimplemeted field in the meta data') }
    # rowCount, columnCount
    rowNum = mtaDta$dataSet$rowCount
    colNum = mtaDta$dataSet$columnCount
    # TO-DO: removedRows, addedRows 
    if (length(mtaDta$dataSet$removedRows) > 0 || length(mtaDta$dataSet$addedRows) > 0) { stop('addedRow or removedRows not empty: Needs to be implemented') }

    # fields
    nmeLst = c()
    lblLst = c()
    fltLst = c()
    for (i in 1:length(mtaDta$dataSet$fields)) {
        # type: determines the format in the binary file
        if      (mtaDta$dataSet$fields[[i]]$type == 'integer') { 
            colRaw = as.data.frame(readBin(binHdl, integer(), n = rowNum))
        } else if (mtaDta$dataSet$fields[[i]]$type == 'number') { 
            colRaw = as.data.frame(readBin(binHdl,  double(), n = rowNum))
        } else {
            stop(paste('Variable type', mtaDta$dataSet$fields[[i]]$type, 'not implemented.'))
        }
        
        # name, description
        nmeCrr = mtaDta$dataSet$fields[[i]]$name
        lblCrr = mtaDta$dataSet$fields[[i]]$description        
        nmeLst = c(nmeLst, nmeCrr)
        lblLst = c(lblLst, lblCrr)
        if (lblCrr != "")
            attr(colRaw[[1]], 'jmv-desc') = lblCrr
        # dataType - value labels
        if (any(grepl(nmeCrr, names(xtdDta)))) {
            if (mtaDta$dataSet$fields[[i]]$dataType == 'Text') {
                colRaw[[1]] = factor(colRaw[[1]], levels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[1])), labels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[2])))
            } else if (mtaDta$dataSet$fields[[i]]$columnType == 'Recoded') {
                colRaw[[1]] = factor(colRaw[[1]], levels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[1])), labels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[2])))
                attr(colRaw[[1]], 'values') = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) as.integer(m[1])))
            } else if (mtaDta$dataSet$fields[[i]]$columnType == 'Filter') {
                colRaw[[1]] = as.logical(colRaw[[1]])
                fltLst = c(fltLst, i)
            } else {
                print('HERE')
                print(paste(nmeCrr, ' - ', mtaDta$dataSet$fields[[i]]$dataType, ' - ', mtaDta$dataSet$fields[[i]]$columnType))
            }
        }
        
        # TO-DO: id, columnType, measureType, formula, formulaMessage, parentId, width, importName, transform, edits, missingValues, trimLevels
        str(colRaw)
        if (i == 1) { names(colRaw) = nmeCrr; dtaFrm = colRaw } else { dtaFrm[nmeCrr] = colRaw }
        print(i)
        str(dtaFrm)
        rm('colRaw')
    }
    
    # handle filters
    if (useFlt) {
        fltInc = rep(TRUE, dim(dtaFrm)[1])
        for (i in fltLst) fltInc = fltInc & dtaFrm[[i]]
        print(str(dtaFrm))
        dtaFrm = dtaFrm[fltInc, ]
        print(str(dtaFrm))
#       dtaFrm[fltLst] = NULL
        print(str(dtaFrm))
    } else {
        attr(dtaFrm, 'fltLst') = nmeLst[fltLst]
    }   
    
    print(lblLst);
    # handle variable labels
    names(lblLst) = nmeLst; attr(dtaFrm, 'variable.labels') = lblLst; rm('nmeLst', 'lblLst');   
            
    # TO-DO: convert transforms into attributes
    # 'name', 'id', 'suffix', 'formula', 'formulaMessage', 'measureType', 'description'
    
    # close and remove the binary file
    close(binHdl); unlink(binFle); rm('binHdl', 'binFle');
    
    # TO-DO: decode the manifest file
    # TO-DO: import HTML output - index.html
   
    # return the resulting data frame
    dtaFrm      
}
