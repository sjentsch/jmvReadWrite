#' Reads SPSS output files (.spv) and extracts the syntax and the data file used in the analyses from them
#'
#' @param fleSPV name (incl. path) of the SPSS-output-file to be read ("FILENAME.spv"; default = "")
#'
#' @return list with SPSS commands (like what is typically stored in SPSS syntax files - .sps); the list contains one command per entry (the command being a character vector) and the position of the SPSS-data-file [incl. path] that was used in the analyses is stored as a character vector in the attribute 'datafile' (the attribute will be empty if either the SPSS-datafile used in the analyses was not stored or if there was more than one data file used in the analyses stored in the original .spv-file 
#'
#' @export spv2sps
#'
spv2sps <- function(fleSPV = "") {

    # check whether the file / archive exists, get list of files contained in the archive and check whether it has the correct format
    if (! file.exists(fleSPV))      { stop(paste0('File "', fleSPV, '" not found.')) }
    fleSPV <- normalizePath(fleSPV);
    binHdr <- readBin(hdlTmp <- file(fleSPV, 'rb'), 'character'); close(hdlTmp); rm('hdlTmp');
    if (binHdr != "PK\003\004\024") { stop(paste0('File "', fleSPV, '" has not the correct file format (is not a ZIP archive).')) }
    lstZIP = unzip(fleSPV, list=TRUE)$Name;

    # extract the syntax from the XML-files where the different elements of the
    # output are stored (those files start with 'outputViewer' followed by
    # numbers and the .xml-file-extension); the text with the syntax is just
    # HTML-code (the formatting nd the HTML-tags are removed)
    txtSPS = list();
    lstLog = lstZIP[grepl('outputViewer[[:digit:]]+\\.xml', lstZIP)];
    for (fleLog in lstLog) {
        # read from output / log file and extract container
        txtLog <- rawToChar(readBin(hdlTmp <- file(fleTmp <- unzip(fleSPV, fleLog, junkpaths = TRUE), 'rb'), 'raw', file.info(fleTmp)$size)); close(hdlTmp); unlink(fleTmp); rm('hdlTmp', 'fleTmp');
        txtLog <- strsplit(strsplit(txtLog, '<container.*?>')[[1]][2], '</container>')[[1]][1];
        # check the type of the container content: skip if page title
        if (grepl('<vtx:text.*type=[",\"]page-title[",\"]>', txtLog) | grepl('<vtx:text.*type=[",\"]title[",\"]>', txtLog)) { next }
        if (! grepl('^<label>Log</label>', txtLog)) { stop(paste0('Unrecognized content type of the input file.\n\nPlease register an issue at https://github.com/sjentsch/jmvReadWrite/issues', '\nRemember to include the SPV-file that caused the error (', fleSPV, ').')); }
        # extract the actual command(s) - '[CDATA[' to ']]' - and remove the header if it exists
        crrSPS <- gsub('<head.*</head>', '', strsplit(strsplit(txtLog, '<!\\[CDATA\\[')[[1]][2], '\\]\\]>')[[1]][1]);
        # do some cleaning (HTML tags, unprintable characters)
        crrSPS <- gsub('\\h+', ' ', gsub(' \\+\\n', '', gsub("\'\\+\\s+\'", "", gsub('\\r\\n', '\\\n', gsub('&amp;', '&', gsub('&gt;', '>', gsub('&nbsp;', ' ', gsub('\ua0', ' ', gsub('&#160;', ' ', 
                  trimws(gsub('<.*?>', ' ', gsub('<br>', '\\\n ', gsub('<BR>', '\\\n ', crrSPS)))))))))))), perl = TRUE);
        # remove comments about data written to an output file (SAVE TRANSLATE)
        if (grepl('Data written to', crrSPS)) { repeat {
            rmvPos = gregexpr("Data written to.*\nVariable:.*?\n\n", paste0(crrSPS, '\n\n'))[[1]];
            if (rmvPos[1] == -1) { rm('rmvPos'); break; }
            crrSPS = gsub('\\h+', ' ', paste0(substr(crrSPS, 1, rmvPos[1] - 1), trimws(substr(crrSPS, rmvPos[1] + attr(rmvPos, 'match.length'), nchar(crrSPS)))), perl = TRUE);
        } }
        # remove error, warning and specific symptom messages
        # [a] Errors where the command wasn't executed, incl. trying to remove the preceding command that caused the error
        if (grepl('>Error', crrSPS)) { repeat {
            rmvPos = gregexpr(">Error.*?Execution of this command stops.", crrSPS)[[1]];
            if (rmvPos[1] == -1) { rm('rmvPos'); break; }
            crrSPS = gsub('\\h+', ' ', paste0(substr(crrSPS, 1, max(gregexpr('\\.\n+', substr(crrSPS, 1, rmvPos[1]))[[1]])), '\n', trimws(substr(crrSPS, rmvPos[1] + attr(rmvPos, 'match.length'), nchar(crrSPS)))), perl = TRUE);
        } }
        # [a] Errors and warnings and specific symptom messages with a number
        if (grepl('>Error #', crrSPS)) { repeat {
            rmvPos = gregexpr("\\n+>Error #.*?\\.\\n", paste0('\n', crrSPS, '\n'))[[1]];
            if (rmvPos[1] == -1) { rm('rmvPos'); break; }
            crrSPS = gsub('\\h+', ' ', paste0(substr(crrSPS, 1, rmvPos - 1), '\n', substr(crrSPS, rmvPos + attr(rmvPos, 'match.length') - 2, nchar(crrSPS))), perl = TRUE);
        } }
        if (grepl('>Warning #', crrSPS)) { repeat {
            rmvPos = gregexpr("\\n>Warning #.*?\\.\\n", paste0('\n', crrSPS, '\n'))[[1]];
            if (rmvPos[1] == -1) { rm('rmvPos'); break; }
            crrSPS = gsub('\\h+', ' ', paste0(substr(crrSPS, 1, rmvPos - 1), '\n', substr(crrSPS, rmvPos + attr(rmvPos, 'match.length') - 2, nchar(crrSPS))), perl = TRUE);
        } }
        if (grepl('>Specific symptom number: ', crrSPS)) { repeat {
            rmvPos = gregexpr("\\.\n+>Specific symptom number: \\d+", paste0('\n', crrSPS))[[1]];
            if (rmvPos[1] == -1) { rm('rmvPos'); break; }
            crrSPS = gsub('\\h+', ' ', paste0(substr(crrSPS, 1, rmvPos - 1), '\n', substr(crrSPS, rmvPos + attr(rmvPos, 'match.length') - 2, nchar(crrSPS))), perl = TRUE);
        } }
        # [c] Other warnings
        if (grepl('>Warning. ', crrSPS)) { repeat {
            rmvPos = gregexpr("\\n>Warning. .*?\\n[A-Z]", paste0('\n', crrSPS, '\n'))[[1]];
            if (rmvPos[1] == -1) { rm('rmvPos'); break; }
            crrSPS = gsub('\\h+', ' ', paste0(substr(crrSPS, 1, rmvPos - 1), '\n', substr(crrSPS, rmvPos + attr(rmvPos, 'match.length') - 2, nchar(crrSPS))), perl = TRUE);
        } }
        if (grepl('\n\\*', crrSPS) | grepl('\n/\\*', crrSPS)) { repeat {
            rmvPos = min(c(gregexpr('\n\\*', crrSPS)[[1]], gregexpr('\n/\\*', crrSPS)[[1]]));
            if (rmvPos[1] == -1) { rm('rmvPos'); break; }
            crrSPS = gsub('\\h+', ' ', paste0(substr(crrSPS, 1, rmvPos[1]), substr(crrSPS, rmvPos[1] + min(gregexpr('\\n', substr(crrSPS, rmvPos[1] + 2, nchar(crrSPS)))[[1]]) + 2, nchar(crrSPS))), perl = TRUE);
        } }
        if (grepl('\\n>', crrSPS)) {
            stop(paste0('Warning / error: ', fleSPV, ' - ', fleLog, ' - ', crrSPS));
        }        
        txtSPS <- c(txtSPS, gsub('\\s+', ' ', unlist(strsplit(trimws(gsub('\\.\n', '\\.\\.\\\n', gsub('\\.\\n+', '.\\\n', crrSPS))), '\\.\n'))));
        rm('txtLog', 'crrSPS');
    }
    txtSPS[  grep('^[/\\*,\\*].*[\\*,\\*\\.,\\*/,\\*/\\.]$', txtSPS)   ] <- NULL;
    txtSPS[c(grep('TITLE ',        txtSPS), grep('TITLE ', txtSPS) + 1)] <- NULL;
    txtSPS[c(grep('CACHE.',        txtSPS), grep('CACHE.', txtSPS) + 1)] <- NULL;
    txtSPS[  grep('DATASET ',      txtSPS)                             ] <- NULL;
    txtSPS[  grep('NEW FILE.',     txtSPS)                             ] <- NULL;
    txtSPS[  grep('GET FILE=',     txtSPS)                             ] <- NULL;
    txtSPS[  grep('GET DATA ',     txtSPS)                             ] <- NULL;
    txtSPS[  grep('SAVE OUTFILE=', txtSPS)                             ] <- NULL;
    txtSPS[  grep('PRESERVE.',     txtSPS)                             ] <- NULL;
    txtSPS[  grep('SET DECIMAL ',  txtSPS)                             ] <- NULL;
    txtSPS[  grep('RESTORE.',      txtSPS)                             ] <- NULL;
    txtSPS[  grep('SET TLook=',    txtSPS)                             ] <- NULL;
    
    # the data file (.sav) that was used for a particular analysis is stored in
    # files whose file names begin with numbers and end with lightNotesData.xml
    # (those contain the element called Notes (usually hidden) at the begin of
    # each analysis; inside the files are binary data within which we look for
    # the string '.sav' followed by character(0) which is the end of the file
    # name; the begin is another character(0)
    fleSAV = c();
    lstNte = lstZIP[grepl('[[:digit:]]+_lightNotesData\\.bin', lstZIP) | grepl('[[:digit:]]+_notesData\\.bin', lstZIP)];
    for (fleNte in lstNte) {
        binNte <- readBin(hdlTmp <- file(fleTmp <- unzip(fleSPV, fleNte, junkpaths = TRUE), 'rb'), 'raw', file.info(fleTmp)$size); close(hdlTmp); unlink(fleTmp); rm('hdlTmp', 'fleTmp');
        savPos = binNte[seq(1, length(binNte) - 4)] == charToRaw('.') & binNte[seq(2, length(binNte) - 3)] == charToRaw('s') & binNte[seq(3, length(binNte) - 2)] == charToRaw('a') & binNte[seq(4, length(binNte) - 1)] == charToRaw('v') & (binNte[seq(5, length(binNte))] == '00' | binNte[seq(5, length(binNte))] == '01');
        if (any(savPos)) fleSAV = c(fleSAV, gsub('\\\\', '/', rawToChar(binNte[seq(max(which(binNte[seq(1, min(which(savPos)))] == '00')) + 1, min(which(savPos)) + 3)])));
    }
    # check whether exactly one data file was used for all analyses in the
    # .spv-file; if so, return the file name as atrribute 'datafile' of txtSPS;
    # if not throw a warning
    fleSAV = unique(fleSAV);
    if (length(fleSAV) == 1) {
        # check for existence of the SPSS-data-file
        if (file.exists(fleSAV)) { 
            attr(txtSPS, 'datafile') <- fleSAV;
        } else if (file.exists(file.path(getwd(), basename(fleSAV)))) {
            attr(txtSPS, 'datafile') <- file.path(getwd(), basename(fleSAV));
        } else if (file.exists(file.path(dirname(fleSPV), basename(fleSAV)))) {
            attr(txtSPS, 'datafile') <- file.path(dirname(fleSPV), basename(fleSAV));
        } else {
            stop(paste0('SPSS data file \"', basename(fleSAV), '\" not found: It is expected to be found either at the position that is stored in the .spv-file, the current working directory, or the directory where the .spv-file was located (', fleSPV, '). Please correct it (copy the data file to one of these places) and run the function again.'));
        }
    } else if (length(fleSAV) == 0) {
        warning('The name of the data file was unavailable / not stored (e.g., because you worked with a dataset with values that were typed in but not stored before conducting the analyses).');
    } else {
        warning(paste('There was more than one data file used in the analyses that are stored in the .spv-file:', paste(as.character(fleSAV), collapse=", ")));
    }

    # return the syntax
    txtSPS
}
