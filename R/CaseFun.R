CaseNamesUpdate <- function(CaseNamesWidget = .rqda$.CasesNamesWidget, 
                            sortByTime = FALSE, decreasing = FALSE, ...)
{
    if (is_projOpen()) {
        ## CaseName <- rqda_sel("select name, id, date from cases where status = 1 order by lower(name)")
        CaseName <- rqda_sel("select name, id, date from cases where status = 1")
        if (nrow(CaseName) == 0) {
            case <- NULL
        } else {
            case <- CaseName$name
            Encoding(case) <- "UTF-8"
            if (!sortByTime) {
                case <- sort(case, decreasing = decreasing)
            } else {
                case <- case[OrderByTime(CaseName$date, decreasing = decreasing)]
            }
        }
        tryCatch(CaseNamesWidget[] <- case, error = function(e) {
        })
    }
}

#################
###############
AddCase <- function(name, conName = "qdacon", assignenv = .rqda, ...) {
    if (name != "") {
        con <- get(conName, assignenv)
        maxid <- rqda_sel("select max(id) from cases")[[1]]
        nextid <- ifelse(is.na(maxid), 0 + 1, maxid + 1)
        write <- FALSE
        if (nextid == 1) {
            write <- TRUE
        } else {
            dup <- rqda_sel(sprintf("select name from cases where name='%s'", enc(name)))
            if (nrow(dup) == 0) write <- TRUE
        }
        if (write) {
            rqda_exe(sprintf("insert into cases (name, id, status, date, owner)
                                            values ('%s', %i, %i, %s, %s)", 
                             enc(name), nextid, 1, shQuote(date()), shQuote(.rqda$owner)))
        }
    }
}


AddFileToCaselinkage <- function(Widget = .rqda$.fnames_rqda) {
    ## filenames -> fid -> selfirst = 0; selend = nchar(filesource)
    filename <- svalue(Widget)
    ## Encoding(filename) <- "unknown"
    query <- rqda_sel(sprintf("select id, file from source where name in (%s) and status = 1", 
                              paste("'", enc(filename), "'", sep = "", collapse = ", ")
                              ))
    fid <- query$id
    Encoding(query$file) <- "UTF-8"
    selend <- nchar(query$file)

    ## select a case name -> caseid
    cases <- rqda_sel("select id, name from cases where status = 1")
    if (nrow(cases) != 0) {
        Encoding(cases$name) <- "UTF-8"
        Selected <- gselect.list(cases$name, multiple = TRUE, x = getOption("widgetCoordinate")[1])
        if (length(Selected) > 0 && Selected != "") {
            Encoding(Selected) <- "UTF-8"
            caseid <- cases$id[cases$name %in% Selected]
            for (i in caseid) {
                exist <- rqda_sel(sprintf("select fid from caselinkage where status = 1 and fid in (%s) and caseid=%i", paste("'", fid, "'", sep = "", collapse = ", "), i))
                if (nrow(exist) != length(fid)) {
                    ## write only when the selected file associated with specific case is not in the caselinkage table
                    DAT <- data.frame(caseid = caseid, fid = fid[!fid %in% exist$fid], selfirst = 0, selend = selend[!fid %in% exist$fid], status = 1, owner = .rqda$owner, date = date(), memo="")
                    success <- rqda_wrt("caselinkage", DAT)
                    if (!success) gmessage(sprintf(gettext("Fail to write to database for case with id %i", domain = "R-RQDA"), i))
                }}
        }
    }
}



UpdateFileofCaseWidget <- function(con = .rqda$qdacon, Widget = .rqda$.FileofCase, sortByTime = FALSE, ...) {
    Selected <- svalue(.rqda$.CasesNamesWidget)
    if (length(Selected) != 0) {
        caseid <- rqda_sel(sprintf("select id from cases where status = 1 and name='%s'", 
                                   enc(Selected)))[, 1]
        Total_fid <- rqda_sel(sprintf("select fid from caselinkage where status = 1 and caseid=%i", caseid))
        if (nrow(Total_fid) != 0) {
            items <- rqda_sel("select name, id, date from source where status = 1")
            if (nrow(items) != 0) {
                if (sortByTime) {
                    items <- items[items$id %in% Total_fid$fid, c("name", "date")]
                    items <- items$name[OrderByTime(items$date)]
                    Encoding(items) <- "UTF-8"}
                else{
                        items <- items[items$id %in% Total_fid$fid, c("name")]
                        Encoding(items) <- "UTF-8"
                        items <- sort(items)
                    }
            } else items <- NULL
        } else items <- NULL
    } else items <- NULL
    tryCatch(Widget[] <- items, error = function(e) {
    })
}

HL_Case <- function() {
    if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        SelectedFile <- svalue(.rqda$.root_edit)
        currentFid <-  rqda_sel(sprintf("select id from source where name='%s'", 
                                        enc(SelectedFile)))[, 1]
        if (length(currentFid) != 0) {
            caseName <- svalue(.rqda$.CasesNamesWidget)
            caseid <- rqda_sel(sprintf("select id from cases where name='%s'", enc(caseName)))[, 1]
            idx <- rqda_sel(sprintf("select selfirst, selend from caselinkage where fid=%i and status = 1 and caseid=%i", currentFid, caseid))
            coding.idx <- rqda_sel(sprintf("select selfirst, selend from coding where fid=%i and status = 1", currentFid))
            anno.idx <- rqda_sel(sprintf("select position from annotation where fid=%i and status = 1", currentFid))$position
            allidx <- unlist(coding.idx, anno.idx)
            if (nrow(idx) != 0) {
                if (!is.null(allidx)) {
                    idx[, "selfirst"] <- sapply(idx[, "selfirst"], FUN = function(x) x + sum(allidx <= x))
                    idx[, "selend"] <- sapply(idx[, "selend"], FUN = function(x) x + sum(allidx <= x))
                }
                ClearMark(.rqda$.openfile_gui, 0, max(idx$selend), clear.fore.col = FALSE, clear.back.col = TRUE)
                HL(.rqda$.openfile_gui, index = idx, fore.col = NULL, back.col = .rqda$back.col)
            }
        }
    }
}


