jmvRead <- function(fleNme = "", useFlt = FALSE) {
# read files created of jamovi (statistical spreadsheet, www.jamovi.org)
# fleNme - name of the jamovi file (.omv; default = "")
# useFlt - apply filters (default: FALSE; i.e., remove the lines where the filter is set to 0)
# 
# (C) Sebastian Jentschke, 2020-
# License: GPL-3
  
    # check whether the file exists and whether it has the correct format
    if (! file.exists(fleNme))                                                    { stop(paste('File', fleNme, 'not found.')) }
    hdrStr = readBin(tmpHdl <- file(fleNme, 'rb'), 'character'); close(tmpHdl); rm('tmpHdl');
    if (! hdrStr == "PK\003\004\024")                                             { stop(paste('File', fleNme, 'has not the correct file format (is not a ZIP archive).')) }
    if (! grepl('META-INF/MANIFEST.MF', toString(unzip(fleNme, list=TRUE)$Name))) { stop(paste('File', fleNme, 'has not the correct file format (is not a jamovi-file).')) }
    
    library(rjson)
    

    mnfTxt <-          readLines(mnfHdl <- file(mnfFle <- unzip(fleNme, 'META-INF/MANIFEST.MF', junkpaths = T), 'r'), warn=F);  close(mnfHdl); unlink(mnfFle); rm('mnfHdl', 'mnfFle');
    mtaDta <- fromJSON(readLines(mtaHdl <- file(mtaFle <- unzip(fleNme, 'metadata.json',        junkpaths = T), 'r'), warn=F)); close(mtaHdl); unlink(mtaFle); rm('mtaHdl', 'mtaFle');
    xtdDta <- fromJSON(readLines(xtdHdl <- file(xtdFle <- unzip(fleNme, 'xdata.json',           junkpaths = T), 'r'), warn=F)); close(xtdHdl); unlink(xtdFle); rm('xtdHdl', 'xtdFle');
                                 binHdl <- file(binFle <- unzip(fleNme, 'data.bin',             junkpaths = T), 'rb');
    # TO-DO: import HTML output - index.html
   
    # process meta-data
    # rowCount, columnCount
    rowNum = mtaDta$dataSet$rowCount
    colNum = mtaDta$dataSet$columnCount
    # TO-DO: removedRows, addedRows 
    if (length(mtaDta$dataSet$removedRows) > 0 || length(mtaDta$dataSet$addedRows) > 0) { stop('addedRow or removedRows not empty: Needs to be implemented') }

    # fields
    nmeVec = c()
    lblVec = c()
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
        
        # name, description: same as foreign
        nmeVec = c(nmeVec, mtaDta$dataSet$fields[[i]]$name)
        lblVec = c(lblVec, mtaDta$dataSet$fields[[i]]$description)
        if (lblVec[[i]] != "")
            attr(colRaw[[1]], 'jmv-desc') = mtaDta$dataSet$fields[[i]]$description
        # dataType - value labels
        if (any(grepl(nmeVec[[i]], names(xtdDta)))) {
            if (mtaDta$dataSet$fields[[i]]$dataType == 'Text') {
                colRaw[[1]] = factor(colRaw[[1]], levels = unlist(sapply(xtdDta[[nmeVec[[i]]]]$labels, function(m) m[1])), labels = unlist(sapply(xtdDta[[nmeVec[[i]]]]$labels, function(m) m[2])))
            } else if (mtaDta$dataSet$fields[[i]]$columnType == 'Recoded') {
                colRaw[[1]] = factor(colRaw[[1]], levels = unlist(sapply(xtdDta[[nmeVec[[i]]]]$labels, function(m) m[1])), labels = unlist(sapply(xtdDta[[nmeVec[[i]]]]$labels, function(m) m[2])))
                attr(colRaw[[1]], 'values') = unlist(sapply(xtdDta[[nmeVec[[i]]]]$labels, function(m) as.integer(m[1])))
            } else if (mtaDta$dataSet$fields[[i]]$columnType == 'Filter') {
                colRaw[[1]] = factor(colRaw[[1]], levels = unlist(sapply(xtdDta[[nmeVec[[i]]]]$labels, function(m) m[1])), labels = unlist(sapply(xtdDta[[nmeVec[[i]]]]$labels, function(m) m[2])))
                attr(colRaw[[1]], 'values') = unlist(sapply(xtdDta[[nmeVec[[i]]]]$labels, function(m) as.integer(m[1])))
            } else {
# Use the factor() function for nominal data and the ordered() function for ordinal data. R      
#mydata$y <- ordered(mydata$y,
#levels = c(1,3, 5),
#labels = c("Low", "Medium", "High"))
                print(paste(nmeVec[[i]], ' - ', mtaDta$dataSet$fields[[i]]$dataType, ' - ', mtaDta$dataSet$fields[[i]]$columnType))
            }
        }

        
        # TO-DO: id, columnType, measureType, formula, formulaMessage, parentId, width, importName, transform, edits, missingValues, trimLevels
        if (i == 1) { names(colRaw) = nmeVec[[i]]; dtaFrm = colRaw } else { dtaFrm[nmeVec[[i]]] = colRaw }       
        rm('colRaw')
    }
    # handle variable labels
    names(lblVec) = nmeVec; attr(dtaFrm, 'variable.labels') = lblVec; rm('nmeVec', 'lblVec');
    
    # TO-DO: convert transforms into attributes
    # 'name', 'id', 'suffix', 'formula', 'formulaMessage', 'measureType', 'description'
    
    # close and remove the binary file
    close(binHdl); unlink(binFle); rm('binHdl', 'binFle');
    
    # TO-DO: decode the manifest file
    # TO-DO: import HTML output - index.html
   
    # return the resulting data frame
    dtaFrm      
}
