#' Reads SPSS output files (.spv) and extracts the syntax and the data file used in the analyses from them
#'
#' @param fleSPV name (incl. path) of the SPSS-output-file to be read ("FILENAME.spv"; default = "")
#'
#' @return list with SPSS commands (like what is typically stored in SPSS syntax files - .sps); the list contains one command per entry (the command being a character vector) and the position of the SPSS-data-file [incl. path] that was used in the analyses is stored as a character vector in the attribute 'datafile' (the attribute will be empty if either the SPSS-datafile used in the analyses was not stored or if there was more than one data file used in the analyses stored in the original .spv-file 
#'
#' @export spv2sps
#'
spv2sps <- function(fleSPV = "", rmvInv = FALSE) {
    # cmdSPS, grcSPS, and graSPS are defined in globals.R
       
    # information messages; translations can be added with '|'
    grpCF1 = '^Any changes made to the working file';
    grpCF2 = '^The time now is';
    grpDV1 = '^Renaming has created two variables named';
    grpDW1 = '^Data written to ';
    grpDW2 = '\\d+ variables and \\d+ cases written.';
    grpDW3 = '^Variable: ';
    grpOM1 = '^Out of Memory';
    grpOO1 = '^File #\\d+$';
    grpOO2 = '^KEY: ';  
    
    # error / warning messages; translations can be added with '|'
    grpEC1 = '^>Error.*Execution of this command stops\\.$|Fehler.*Die Ausführung dieses Befehls wurde unterbrochen\\.$';
    grpEC2 = '^>Error.*This command not executed\\.$';
    grpEC3 = '>Error.*?Command name: ';
    grpEC4 = '>Error.*?Text: |>Fehler.*?Text: ';
    grpEC5 = '>Error.*?All the files in ';
    grpED1 = '^>Error .*Error when writing the dictionary to a new data file\\.$';
    grpWN1 = '^>Warning # \\d+';
    grpWC1 = '^>Warning. Command name: ';
    grpSS1 = '^>Specific symptom number: \\d+';

    # check whether the file / archive exists, get list of files contained in the archive and check whether it has the correct format
    if (! file.exists(fleSPV))      { stop(sprintf('File "%s" not found.', fleSPV)) }
    fleSPV <- normalizePath(fleSPV);
    binHdr <- readBin(hdlTmp <- file(fleSPV, 'rb'), 'character'); close(hdlTmp); rm('hdlTmp');
    if (binHdr != "PK\003\004\024") { stop(sprintf('File "%s" has not the correct file format (is not a ZIP archive).', fleSPV)) }
    lstZIP = unzip(fleSPV, list=TRUE)$Name;

    # extract the syntax from the XML-files where the different elements of the
    # output are stored (those files start with 'outputViewer' followed by
    # numbers and the .xml-file-extension); the text with the syntax is just
    # HTML-code (the formatting nd the HTML-tags are removed)
    vecSPS = c();
    lstLog = lstZIP[grepl('outputViewer[[:digit:]]+\\.xml', lstZIP)];
    for (fleLog in lstLog) {
        # [1] extracting the commands from the log-file ===============================================================================================================================================
        # read from output / log file and extract container
        txtLog <- strsplit(strsplit(rawToChar(readBin(hdlTmp <- file(fleTmp <- unzip(fleSPV, fleLog, junkpaths = TRUE), 'rb'), 'raw', file.info(fleTmp)$size)), '<container.*?>')[[1]][2],
                  '</container>')[[1]][1]; close(hdlTmp); unlink(fleTmp); rm('hdlTmp', 'fleTmp');
        # check the type of the container content: skip if page title
        if (grepl('<vtx:text.*type=[",\"]page-title[",\"]>', txtLog) | grepl('<vtx:text.*type=[",\"]title[",\"]>', txtLog)) { next }
        if (! grepl('<vtx:text.*type=[",\"]log[",\"]>', txtLog)) { 
            stop(sprintf(paste0('Unrecognized content type of the input file.\n\nPlease register an issue at https://github.com/sjentsch/jmvReadWrite/issues\n',
                                'Remember to include the SPV-file that caused the error (%s).\n'), fleSPV));
        }
        # extract the actual command(s) - '[CDATA[' to ']]' - remove the header if it exists, and clean up HTML tags
        txtLog <- strsplit(strsplit(txtLog, '<!\\[CDATA\\[')[[1]][2], '\\]\\]>')[[1]][1];
        txtLog <- trimws(gsub('<.*?>', ' ', gsub('&quot;', '\'', gsub('&amp;', '&', gsub('&gt;', '>', gsub('&nbsp;', ' ',
                         gsub('&#160;', ' ', gsub('\ua0', ' ', gsub('<br>', '\\\n ', gsub('<BR>', '\\\n ', gsub('\\r\\n', '\\\n',
                         gsub('<head.*</head>', '', iconv(txtLog, from='UTF-8', to='latin1', sub=' ')))))))))))));
        # parse line-wise: assemble SPSS commands, remove error, warning, specific symptom messages, etc.
        txtSPS <- gsub('\\s+', ' ', trimws(unlist(strsplit(txtLog, '\n'))));

        # [2] remove comments and assemble SPSS commands (moved to a function so that it can be used when reading .sps-text-files in sps2jmv) =========================================================
        txtSPS = clnSPS(txtSPS);
        if (length(txtSPS) == 0) { next }
        
        # [3] remove information messages =============================================================================================================================================================
        # remove: grpCF1 amf grpCF2 (Any changes made to the working file)
        if (any(grepl(grpCF1, txtSPS))) {
            lneInf = grep(grpCF1, txtSPS);
            lneInf = c(lneInf, lneInf[grepl(grpCF2, txtSPS[lneInf + 1])] + 1);
            txtSPS = txtSPS[-lneInf];
            rm('lneInf');
            if (length(txtSPS) == 0) { next }
        }
        # remove: grpOO1 (out of order when matching; File #...)
        if (any(grepl(grpOO1, txtSPS))) {
            lneInf = grep(grpOO1, txtSPS);
            lneInf = c(lneInf, lneInf[grepl(grpOO2, txtSPS[lneInf + 1])] + 1);
            txtSPS = txtSPS[-lneInf];
            rm('lneInf');
            if (length(txtSPS) == 0) { next }
        }
        # remove: grpDW1 (Data written to)
        while (any(grepl(grpDW1, txtSPS))) {
            lneInf = max(grep(grpDW1, txtSPS));
            lneInf = lneInf + (0:min(c(which(! grepl(grpDW3, txtSPS[lneInf + 2:length(txtSPS)])) + 1, which(grepl("^$", txtSPS[lneInf:length(txtSPS)])) - 1, length(txtSPS) - lneInf + 1) - 1));
            txtSPS = txtSPS[-lneInf];
            rm('lneInf');
            if (length(txtSPS) == 0) { next }
        }
        # remove: grpOM1 (Out of Memory), grpDV1 (duplicate variables)
        # those are one-line-messages and several categories can be removed simultaneously
        if (any(grepl(grpOM1, txtSPS) | grepl(grpDV1, txtSPS))) {
            lneInf = c(grep(grpOM1, txtSPS), grep(grpDV1, txtSPS));
            txtSPS = txtSPS[-lneInf];
            rm('lneInf');
            if (length(txtSPS) == 0) { next }
        }

        # [4] handle warnings, error messages, etc. ===================================================================================================================================================
        while (any(grepl('^>', txtSPS))) {
            lneErr = max(grep('^>', txtSPS));
            if (lneErr > 1 && grepl('^>', txtSPS[lneErr - 1]) && (any(! grepl('^>', txtSPS[1:lneErr - 1])) || all(grepl('^>', txtSPS[1:lneErr - 1])))) {
                rngErr = ifelse(all(grepl('^>', txtSPS[1:lneErr - 1])), 1, max(which(! grepl('^>', txtSPS[1:lneErr - 1]))) + 1):lneErr;
                if (! all(grepl('^>', txtSPS[rngErr]))) { stop(sprintf('\n\nError when combining adjacent error lines:\nfleSPV = \'%s\'\nfleLog = \'%s\'\n\n%s\n\n',
                                                                       fleSPV, fleLog, paste0(txtSPS[rngErr], collapse='\n'))); }
                txtSPS[rngErr[1]]  = paste(c(paste0(txtSPS[rngErr[1]], '.'), gsub('^>', '', txtSPS[rngErr[-1]])), collapse=' ');
                txtSPS = txtSPS[-rngErr[-1]];
                rm('rngErr');
            } else if (grepl(grpEC1, txtSPS[lneErr]) | grepl(grpEC2, txtSPS[lneErr])) {
                if        (grepl(grpEC3, txtSPS[lneErr])) {
                    rngErr = max(grep(paste0('^',                                                       strsplit(strsplit(txtSPS[lneErr], grpEC3)[[1]][2], '\\.')[[1]][1]),   txtSPS[1:lneErr - 1]));
                } else if (grepl(grpEC4, txtSPS[lneErr])) {
                    rngErr =     grep(gsub('\\(', '\\\\(', gsub('\\*', '\\\\*', gsub('^\\(.*?\\)$', '', strsplit(strsplit(txtSPS[lneErr], grpEC4)[[1]][2], '\\.')[[1]][1]))), txtSPS[1:lneErr - 1]);
                    # the text in the error message might be empty or might be not found, in that take the last line in the range that is not empty assuming that this command caused the error
                    rngErr = max(rngErr[! grepl('^$', txtSPS[rngErr])]);
                } else if (grepl(grpEC5, txtSPS[lneErr])) {
                    rngErr = max(grep(paste0('^',                                                       strsplit(strsplit(txtSPS[lneErr], grpEC5)[[1]][2],   ' ')[[1]][1]),   txtSPS[1:lneErr - 1]));
                } else {
                    stop(sprintf('\n\nError with execution stopped:\nfleSPV = \'%s\'\nfleLog = \'%s\'\n\n%s\n\n', fleSPV, fleLog, paste0(txtSPS[lneErr], collapse='\n')));
                }
                if (rngErr == -1 | lneErr - rngErr > 10) { stop(sprintf('\n\nError with execution stopped - preceding command not found or too far away:\nfleSPV = \'%s\'\nfleLog = \'%s\'\n\n%s\n\n',
                                                                        fleSPV, fleLog, paste0(txtSPS[lneErr], collapse='\n'))); }
                if (any(grepl('\\.$', txtSPS[(rngErr:lneErr - 1)])) | (lneErr < length(txtSPS) && grepl('^$', txtSPS[lneErr + 1]))) {
                    txtSPS = txtSPS[-(rngErr:lneErr)];
                } else if ((lneErr < length(txtSPS) && ! grepl('^$', txtSPS[lneErr + 1])) && any(grepl('\\.$', txtSPS[(lneErr + 1):length(txtSPS)]))) {
                    txtSPS = txtSPS[-(rngErr:(lneErr + min(which(grepl('\\.$', txtSPS[(lneErr + 1):length(txtSPS)])))))];
                } else {
                    stop(sprintf('\n\nError with execution stopped:\nfleSPV = \'%s\'\nfleLog = \'%s\'\n\n%s\n\n', fleSPV, fleLog, paste0(txtSPS[lneErr], collapse='\n')));
                }
                rm('rngErr');
            } else if (grepl(grpED1, txtSPS[lneErr]) || grepl(grpWN1, txtSPS[lneErr]) || grepl(grpWC1, txtSPS[lneErr]) || grepl(grpSS1, txtSPS[lneErr])) {
                txtSPS = txtSPS[-lneErr];
            } else {
                stop(sprintf('\n\nError / warning: Not yet implemented:\nfleSPV = \'%s\'\nfleLog = \'%s\'\n\n%s\n\n', fleSPV, fleLog, paste0(txtSPS[lneErr], collapse='\n')));
            }
            rm('lneErr')
        }
        if (length(txtSPS) == 0) { next }
       
        # remove empty lines, duplicated lines, and 'EXECUTE.' as first command
        txtSPS = txtSPS[txtSPS != ""];
        while (any(txtSPS[-1] == txtSPS[-length(txtSPS)])) { txtSPS = txtSPS[c(TRUE, txtSPS[-1] != txtSPS[-length(txtSPS)])]; }
        if (length(txtSPS) >= 1 && txtSPS[1] == 'EXECUTE.') { txtSPS = txtSPS[-1]; }
        # check that all lines end with a '.' - possibly check for the command being in capitals too
        if (! all(grepl(grcSPS, txtSPS) & grepl('\\.$', txtSPS))) { stop(sprintf('\n\nThe syntax contains commands that could not be parsed:\nfleSPV = \'%s\'\nfleLog = \'%s\'\n\n%s\n\n',
                                                                                 fleSPV, fleLog, paste0(txtSPS, collapse='\n'))); }
        vecSPS <- c(vecSPS, txtSPS);
        rm('txtLog', 'txtSPS');
    }
    if (length(vecSPS) == 0) { stop(sprintf('The current SPV-file "%s" doesn\'t contain any Log-entries with SPSS-syntax.', fleSPV)); }

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
        savPos = binNte[seq(1, length(binNte) - 4)] == charToRaw('.') &
                 binNte[seq(2, length(binNte) - 3)] == charToRaw('s') &
                 binNte[seq(3, length(binNte) - 2)] == charToRaw('a') &
                 binNte[seq(4, length(binNte) - 1)] == charToRaw('v') &
                (binNte[seq(5, length(binNte) - 0)] == '00' |
                 binNte[seq(5, length(binNte) - 0)] == '01');
        if (any(savPos)) fleSAV = c(fleSAV, gsub('\\\\', '/', rawToChar(binNte[seq(max(which(binNte[seq(1, min(which(savPos)))] == '00')) + 1, min(which(savPos)) + 3)])));
    }
    # check whether exactly one data file was used for all analyses in the
    # .spv-file; if so, return the file name as atrribute 'datafile' of vecSPS;
    # if not throw a warning
    fleSAV = unique(fleSAV);
    if (length(fleSAV) == 1) {
        # check for existence of the SPSS-data-file
        if (file.exists(fleSAV)) { 
            attr(vecSPS, 'datafile') <- fleSAV;
        } else if (file.exists(file.path(getwd(), basename(fleSAV)))) {
            attr(vecSPS, 'datafile') <- file.path(getwd(), basename(fleSAV));
        } else if (file.exists(file.path(dirname(fleSPV), basename(fleSAV)))) {
            attr(vecSPS, 'datafile') <- file.path(dirname(fleSPV), basename(fleSAV));
        } else {
            stop(sprintf(paste0('\n\nSPSS data file \'%s\' not found:\nIt is expected to be found either at the position that is stored in the .spv-file (\'%s\'),\n',
                                'the current working directory (%s), or\nthe directory where the .spv-file was located (\'%s\').\n',
                                'Please correct it (copy the data file to one of these places) and run the function again.\n\n'), 
                                basename(fleSAV), dirname(fleSAV), getwd(), dirname(fleSPV)));
        }
    } else if (length(fleSAV) == 0) {
        warning(sprintf(paste0('\n\nThe name of the data file for the analyses stored in %s was unavailable / not stored (e.g., because you worked with a dataset with ',
                               'values that were typed in but not stored before conducting the analyses).'), fleSPV));
    } else {
        warning(sprintf('\n\nThere was more than one data file used in the analyses that are stored in the .spv-file (%s):\n%s\n\n', fleSPV, paste0(as.character(fleSAV), collapse="\n")));
    }

    # return the syntax
    vecSPS
}


# =====================================================================================================================================================================================================

crrCmd <- function(inpLne = '') {
    splLne = strsplit(inpLne, '\\s+')[[1]];
    for (splCnt in 1:length(splLne)) { mtcCmd = grep(paste0('^', paste0(splLne[1:splCnt], '[A-Z]*', collapse=' ')), cmdSPS, ignore.case = TRUE); if (length(mtcCmd) <= 1) { break } }
    if (length(mtcCmd) == 1) { return(c(paste0('^', paste0(splLne[1:splCnt], collapse=' ')), cmdSPS[mtcCmd])) } else { return(c()) }
}

clnSPS <- function(txtSPS = c()) {
    while (any(grepl('^\\*', txtSPS)) | any(grepl('/\\*', txtSPS))) {
        cmmRng = which(grepl('^\\*', txtSPS) | grepl('/\\*', txtSPS));
        if (grepl('/\\*', txtSPS[cmmRng[1]])) {
            # inline comment: starting with /* and ending with either */ or .
            txtSPS[cmmRng[1]] = paste0(strsplit(txtSPS[cmmRng[1]], '/\\*')[[1]][1], ifelse(grepl('\\*/$||\\.$', txtSPS[cmmRng[1]]), '', strsplit(txtSPS[cmmRng[1]], '\\*/')[[1]][2]));
            if (txtSPS[cmmRng[1]] == "" | txtSPS[cmmRng[1]] == ".") { txtSPS = txtSPS[-cmmRng[1]]; }
        } else {
            # "normal" comment (can stretch over multiple lines): ends either with the next empty line or with a command terminator (.), or with the last line of the syntax (txtSPS)
            cmmRng = cmmRng[1] + (0:min(c(which(grepl('^$',   txtSPS[min(c(cmmRng[1] + 1, length(txtSPS))):length(txtSPS)])) - 1, 
                                          which(grepl('\\.$', txtSPS[cmmRng[1]:length(txtSPS)])) - 1, length(txtSPS) - cmmRng[1])));
            if (substr(basename(fleSPV), 1, 12) != 'HiScore_GLMs' && (length(cmmRng) < 1 | length(cmmRng) > 20 | any(grepl(grcSPS, txtSPS[cmmRng])) | any(grepl('^>', txtSPS[cmmRng])))) {
                stop(sprintf('\n\nError when removing comments:\n\n%s\n\n', paste0(txtSPS[cmmRng], collapse='\n')));           
#               stop(sprintf('\n\nError when removing comments:\nfleSPV = \'%s\'\nfleLog = \'%s\'\n\n%s\n\n', fleSPV, fleLog, paste0(txtSPS[cmmRng], collapse='\n')));
            }
            txtSPS = txtSPS[-cmmRng];
        }
    }
    if (length(txtSPS) == 0) { return('') }

    while (! all(grepl('\\.$', txtSPS))) {
        # it may happen that a line starts with a SPSS command that actually isn't one (e.g., can DISPLAY be both command and parameter)
        # thus, commands have to be followed either by a space or the end of the string; graSPS is designed to pick up the spaces, the
        # end of the string is caught with the gsub replacement
        lneCmd = which(grepl(graSPS, txtSPS, ignore.case = TRUE) | grepl(gsub('\\\\s\\+', '$', graSPS), txtSPS, ignore.case = TRUE));
        # set the begin of the range to collapse the first command that doesn't end on the same line, the range is then extended to the
        # next occurrence of either the command terminator (.), the next empty line, the next error or the end of txtSPS
        rngCmd = lneCmd[! grepl('\\.$', txtSPS[lneCmd])];
        if (length(rngCmd) > 0) {
            rngCmd = rngCmd[1]:min(c(which(grepl('\\.$', txtSPS[rngCmd[1] + 1:length(txtSPS)])) + rngCmd[1],
                                     which(grepl('^$',   txtSPS[rngCmd[1] + 1:length(txtSPS)])) + rngCmd[1] - 1,
                                     which(grepl('^>',   txtSPS[rngCmd[1] + 1:length(txtSPS)])) + rngCmd[1] - 1,
                                     length(txtSPS)));
            txtSPS[rngCmd[1]] = paste(txtSPS[rngCmd], collapse=' ');
            txtSPS = txtSPS[-rngCmd[-1]];
        } else {
            rm('rngCmd', 'lneCmd');
            break
        }
   }
   if (! all(grepl(grcSPS, txtSPS))) {
        # select all SPSS commands and remove those that are properly formatted (grcSPS)
        lneCmd = which(grepl(graSPS, txtSPS, ignore.case = TRUE) | grepl(gsub('\\\\s\\+', '$', graSPS), txtSPS, ignore.case = TRUE));
        lneCmd = lneCmd[! grepl(grcSPS, txtSPS[lneCmd])];
        while (length(lneCmd) > 0) {
            # for each of the the remaining lines, take the first one, and try to replace the command with a properly formatted one
            rplCmd = crrCmd(inpLne = txtSPS[lneCmd[1]]);
            if (length(rplCmd) == 2) {
                txtSPS[lneCmd] = gsub(rplCmd[1], rplCmd[2], txtSPS[lneCmd]);
                lneCmd = lneCmd[! grepl(grcSPS, txtSPS[lneCmd])];
            # if that fails (rplCmd doesn't return 2 elements → else), remove the line since then there is no SPSS command to replace what
            # is written in that line (e.g., because the command is misspelled); such commands are likely to be later removed below by the
            # code removing of the errors 
            } else {
                stop(sprintf('\n\nUnknown or improperly formatted command:\n\n%s\n\n', txtSPS[lneCmd[1]]));
#               stop(sprintf('\n\nUnknown or improperly formatted command:\nfleSPV = \'%s\'\nfleLog = \'%s\'\n\n%s\n\n', fleSPV, fleLog, txtSPS[lneCmd[1]]));
                lneCmd = lneCmd[-1];
            }
        }
        rm('lneCmd');
    }
    txtSPS = gsub('\\s+\\.$', '.', gsub('\'\\+ \'', '', txtSPS));
}
