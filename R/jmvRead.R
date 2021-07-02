#' Read files created of the statistical spreadsheet 'jamovi' (www.jamovi.org)
#'
#' @param fleNme name of the 'jamovi' file to be read ("FILENAME.omv"; default = "")
#' @param useFlt apply filters (remove the lines where the filter is set to 0; default: FALSE)
#' @param rmMsVl remove values defined as missing values (replace them with NA; default - FALSE)
#' @param sveAtt store attributes that are not required in the data set (if you want to write the same data set using jmvWrite; default – FALSE)
#' @return data frame (can be directly used with functions included in the R-package 'jmv' and syntax from 'jamovi'; also compatible with the format of the  R-package "foreign")
#'
#' @export jmvRead

jmvRead <- function(fleNme = "", useFlt = FALSE, rmMsVl = FALSE, sveAtt = FALSE) {

    # check whether the file exists and whether it has the correct format
    if (! file.exists(fleNme))                                                    { stop(paste0('File "', fleNme, '" not found.')) }
    hdrStr <- readBin(tmpHdl <- file(fleNme, 'rb'), 'character'); close(tmpHdl); rm('tmpHdl');
    if (! hdrStr == "PK\003\004\024")                                             { stop(paste0('File "', fleNme, '" has not the correct file format (is not a ZIP archive).')) }
    if (! grepl('META-INF/MANIFEST.MF', toString(unzip(fleNme, list=TRUE)$Name))) { stop(paste0('File "', fleNme, '" has not the correct file format (is not a jamovi-file).')) }
    
    # check whether the archive contains a string.bin-file (it only exists if there are columns that contain text variables)
    strBin = grepl('strings.bin', toString(unzip(fleNme, list=TRUE)$Name))
    
    # read and decode
    mnfTxt <-                 readLines(mnfHdl <- file(mnfFle <- unzip(fleNme, 'META-INF/MANIFEST.MF', junkpaths = TRUE), 'r'), warn = FALSE);                    close(mnfHdl); unlink(mnfFle); rm('mnfHdl', 'mnfFle');
    mtaDta <- rjson::fromJSON(readLines(mtaHdl <- file(mtaFle <- unzip(fleNme, 'metadata.json',        junkpaths = TRUE), 'r'), warn = FALSE), simplify = FALSE); close(mtaHdl); unlink(mtaFle); rm('mtaHdl', 'mtaFle');
    xtdDta <- rjson::fromJSON(readLines(xtdHdl <- file(xtdFle <- unzip(fleNme, 'xdata.json',           junkpaths = TRUE), 'r'), warn = FALSE), simplify = FALSE); close(xtdHdl); unlink(xtdFle); rm('xtdHdl', 'xtdFle');
                                        binHdl <- file(binFle <- unzip(fleNme, 'data.bin',             junkpaths = TRUE), 'rb');
    if (strBin)                       { strHdl <- file(strFle <- unzip(fleNme, 'strings.bin',          junkpaths = TRUE), 'rb'); }

    # decode the manifest file and throw an error if an file version occurs that was written using a jamovi-version
    mnfVer = gsub('Manifest-Version: ',       '', mnfTxt[grepl('Manifest-Version:',       mnfTxt)])
    datVer = gsub('Data-Archive-Version: ',   '', mnfTxt[grepl('Data-Archive-Version:',   mnfTxt)])
    jmvVer = gsub('jamovi-Archive-Version: ', '', mnfTxt[grepl('jamovi-Archive-Version:', mnfTxt)])    
    crtStr = gsub('Created-By: ',             '', mnfTxt[grepl('Created-By:',             mnfTxt)])
    if (mnfVer != "1.0" || datVer != "1.0.2" || jmvVer != "8.0") { stop(paste0('File "', fleNme, '" was written with an older version of jamovi. It currently can''t be read. Please send the file to sebastian.jentschke@uib.no!')) }
   
    # process meta-data
    if (any(names(mtaDta) != "dataSet")) { stop('Unimplemeted field in the meta data') }

    # rowCount, columnCount
    rowNum = mtaDta$dataSet$rowCount
    colNum = mtaDta$dataSet$columnCount
    if (length(mtaDta$dataSet$fields) != colNum) { stop('Number of fields in the metadata is not matching up the number of columns.') }

    # iterate through fields
    lblLst = c()
    fltLst = c()
    for (i in 1:colNum) {
        # type: determines the format in the binary file
        if      (mtaDta$dataSet$fields[[i]]$type == 'integer') { 
            colRaw = as.data.frame(readBin(binHdl,   integer(), n = rowNum))
        } else if (mtaDta$dataSet$fields[[i]]$type == 'number') { 
            colRaw = as.data.frame(readBin(binHdl,    double(), n = rowNum))
        } else if (mtaDta$dataSet$fields[[i]]$type == 'string') {
            colRaw = as.data.frame(readBin(strHdl, character(), n = rowNum))
                                   readBin(binHdl,   integer(), n = rowNum)
        } else {
            stop(paste('Variable type', mtaDta$dataSet$fields[[i]]$type, 'not implemented.'))
        }

        # name, description
        nmeCrr = mtaDta$dataSet$fields[[i]]$name
        lblCrr = mtaDta$dataSet$fields[[i]]$description        
        lblLst = c(lblLst, lblCrr)
        
        # value labels
        if (any(nmeCrr == names(xtdDta))) {
            if    (any(mtaDta$dataSet$fields[[i]]$columnType == c('Data', 'Recoded'))) {
                colRaw[[1]] = factor(colRaw[[1]], levels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[1])), labels = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) m[2])))
                if (mtaDta$dataSet$fields[[i]]$dataType == 'Integer') { 
                    attr(colRaw[[1]], 'values') = unlist(sapply(xtdDta[[nmeCrr]]$labels, function(m) as.integer(m[1])))
                }
            } else if (mtaDta$dataSet$fields[[i]]$columnType ==   'Filter') {
                colRaw[[1]] = as.logical(colRaw[[1]])
                fltLst = c(fltLst, i)
#           } else {
#               print(paste(nmeCrr, ' - ', mtaDta$dataSet$fields[[i]]$dataType, ' - ', mtaDta$dataSet$fields[[i]]$columnType))
            }
        }
        
        if (i == 1) { 
            names(colRaw) = nmeCrr
            dtaFrm = colRaw
        } else { 
            dtaFrm[nmeCrr] = colRaw
        }

        if (lblCrr != "")
            attr(dtaFrm[[nmeCrr]], 'jmv-desc') = lblCrr
            
        if (mtaDta$dataSet$fields[[i]]$measureType == "ID") {
            attr(dtaFrm[[nmeCrr]], 'jmv-id') = T
        }
        
        if (length(mtaDta$dataSet$fields[[i]]$missingValues) > 0) {
            attr(dtaFrm[[nmeCrr]], 'missingValues') = mtaDta$dataSet$fields[[i]]$missingValues
        }
        
        if (sveAtt) {
            for (attNme in c('id', 'columnType', 'dataType', 'measureType', 'formula', 'formulaMessage', 'parentId', 'width', 'type', 'importName', 'transform', 'edits', 'trimLevels', 'filterNo', 'active')) {
                if (! is.null(mtaDta$dataSet$fields[[i]][attNme])) {
                    attr(dtaFrm[[nmeCrr]], attNme) = mtaDta$dataSet$fields[[i]][[attNme]]
                }
            }
        }

        if (rmMsVl) {
            mssLst = attr(dtaFrm[[nmeCrr]], 'missingValues')
            if (length(mssLst) > 0) {
               attCrr = attributes(dtaFrm[[nmeCrr]])
               rmvLvl = rep(FALSE, length(levels(dtaFrm[[nmeCrr]])))
               for (j in 1:length(mssLst)) {
                   dtaFrm[[nmeCrr]][eval(parse(text = paste0('dtaFrm[["', nmeCrr, '"]]', mssLst[[j]])))] <- NA
                   rmvLvl <- rmvLvl | eval(parse(text = paste0('levels(dtaFrm[["', nmeCrr, '"]]) ', mssLst[j])))
               }
               dtaFrm[[nmeCrr]] = dtaFrm[[nmeCrr]][, drop = TRUE]
               attCrr$missingValues = list()
               attCrr$values <- attCrr$values[!rmvLvl]
               for (attNme in setdiff(names(attCrr), names(attributes(dtaFrm[[nmeCrr]])))) {
                   attr(dtaFrm[[nmeCrr]], attNme) <- attCrr[[attNme]]
               }
            }
        }
        
        rm('colRaw')
    }
    
    # close and remove the binary file(s)
    close(binHdl); unlink(binFle); rm('binHdl', 'binFle');
    if (strBin) {
        close(strHdl); unlink(strFle); rm('strHdl', 'strFle');
    }    
    
    # handle filters
    if (useFlt) {
        fltInc = rep(TRUE, dim(dtaFrm)[1])
        for (i in fltLst) fltInc = fltInc & dtaFrm[[i]]
        str(dtaFrm)
        dtaFrm = dtaFrm[fltInc, ]
        str(dtaFrm)
        dtaFrm[fltLst] = NULL
        str(dtaFrm)
    } else if(length(fltLst) > 0) {
        attr(dtaFrm, 'fltLst') = names(dtaFrm)[fltLst]
    }
    
    # handle variable labels: R-foreign-style
    if (! all(lblLst == "")) {
        names(lblLst) <- names(dtaFrm)
        attr(dtaFrm, 'variable.labels') = lblLst
        rm('lblLst');
    }
    
    # removedRows, addedRows, transforms
    if (sveAtt) {
        for (attNme in c('removedRows', 'addedRows', 'transforms')) {
            attr(dtaFrm, attNme) = mtaDta$dataSet[[attNme]]
        }
    }

    # import and extract syntax from the analyses
    anlLst = unzip(fleNme, list = TRUE)$Name
    anlLst = anlLst[grepl('[0-9][0-9].*/analysis', anlLst)]
    if (length(anlLst) > 0) {
        RProtoBuf::readProtoFiles(system.file("jamovi.proto", package="jmvcore"))
        for (anlNme in anlLst) {
            anlPBf <- RProtoBuf::read(jamovi.coms.AnalysisResponse, anlHdl <- file(anlFle <- unzip(fleNme, anlNme, junkpaths = TRUE), 'rb'));  close(anlHdl); unlink(anlFle); rm('anlHdl', 'anlFle');
            # for (anlFld in names(anlPBf)) { print(paste(anlFld, anlPBf[[anlFld]])) } - helper function to show all fields
            # instanceId     - instance ID (e.g., d70a2417-4438-49f6-839e-7e54daa4458f)
            # analysisId     - continuous number (same as the folder where the analysis was stored)
            # name           - name of the analysis (e.g., descriptives, anovaRM)
            # ns             - name of the module / package
            # options        - list of [command] options with 3 fields  (can be inherited):
            #                  each entry in options matches one of the parameters of the respective function
            #                  in jmv - at the end there are three further display options (.ppi, theme, palette)
            #   hasNames     - whether the entries of the list have names
            #   names        - names of the options (e.g., data, effectsize, etc.)
            #   options      - list with sub-options
            #     i          - option as integer
            #     d          - option as decimal
            #     s          - option as string
            #     o          - option as boolean (TRUE, FALSE, NONE)
            #     c          - child (for nested options) - same list as above (hasNames, names, options)          
            # results        - list of [analysis] results with 15 fields (can be inherited):
            #   name         - name of the results output (e.g., )
            #   title        - 
            #   status       -
            #   error        -
            #   stale        -
            #   table        -
            #   image        -
            #   group        -
            #   array        -
            #   preformatted -
            #   syntax       -
            #   html         -
            #   state        -
            #   visible      -
            #   refs         - reference(s) for that particular output
            # status         - ? integer (3)
            # error          - error message (string)
            # incAsText      - ? boolean (TRUE)
            # revision       - ? integer (8, 17)
            # restartEngines - ? boolean (FALSE)
            # stacktrace     - ? likely in connection with an error
            # version        - ? integer (16777216)
            # index          - ? integer (0)
            # references     - list of references with 8 fields:
            #   name         - internal code like the BibTex-ref (e.g., of the R-package - afex, emmeans)
            #   type         - publication type (e.g., software, article, etc.)
            #   authors      - list - entry "complete" contains the whole list of authors
            #   year         - year (integer)
            #   title        - publication title
            #   publisher    - publisher (or CRAN for R-packages).
            #   url          - URL of the publication
            #   volume       - volume (for articles)
            #   issue        - issue (for articles)
            #   pages        - page numbers (for articles)
            #   year2        - year (as string)
    }
    
    # import the HTML output
    attr(dtaFrm, 'outputHTML') <- readLines(htmHdl <- file(htmFle <- unzip(fleNme, 'index.html', junkpaths = TRUE), 'r'), warn = FALSE); close(htmHdl); unlink(htmFle); rm('htmHdl', 'htmFle');
    
    # return the resulting data frame
    dtaFrm
}
