#' @importFrom stringi stri_enc_detect stri_encode
ImportFile <- function(paths, encoding = .rqda$encoding, con= .rqda$qdacon, ...) {

    ## import a file into a DBI connection _con_.
    for (path in paths) {
        ## Fname is in locale Encoding Now.
        Fname <- gsub("\\.[[:alpha:]]*$", "", basename(path))
        FnameUTF8 <- iconv(Fname, to = "UTF-8")
        ## remove the suffix such as .txt
        if (Fname != "") {
            file_con <- file(path, open = "r")
            if (isTRUE(.rqda$BOM)) seek(file_con, 3)
            content <- readLines(file_con, warn = FALSE, encoding = encoding)
            close(file_con)
            content <- paste(content, collapse = "\n")
                                        #content <- enc(content, encoding = Encoding(content))

                                        # detect encoding and convert
            dtct <- stri_enc_detect(paste(content, collapse = "\n"))[[1]]
            enc_hat <- dtct$Encoding[dtct$Confidence == max(dtct$Confidence)]
            cat("File imported with the following encoding: ", enc_hat, "\n")
            content <- stri_encode(content, from = enc_hat, to = "UTF-8")

                                        # guard against single quote possibly theres more, but lets wait and see
            content <- gsub("'", "''", content)

            maxid <- rqda_sel("select max(id) from source")[[1]]
            nextid <- ifelse(is.na(maxid), 0 + 1, maxid + 1)
            write <- FALSE
            ## check if the content should be written into con.
            if (nextid == 1) {
                write <- TRUE
                ## if this is the first file, no need to worry about the duplication issue.
            } else {
                if (nrow(rqda_sel(sprintf("select name from source where name='%s'", FnameUTF8))) == 0) {
                    ## no duplication file exists, then write.
                    write <- TRUE
                } else {
                    gmessage(gettext("A file with the same name exists in the database!", domain = "R-RQDA"))
                }
            }
            if (write) {
                rqda_exe(sprintf("insert into source (name, file, id, status, date, owner)
                             values ('%s', '%s', %i, %i, '%s', '%s')",
                             Fname, content, nextid, 1, date(), .rqda$owner))
            }
        }
        isTRUE(.rqda$isLaunched)
        FileNamesUpdate()
    }
}


FileNamesUpdate <- function(FileNamesWidget = .rqda$.fnames_rqda, sortByTime = TRUE, decreasing = FALSE, ...) {
    ##update file names list in the FileNamesWidget
    wopt <- options(warn = -2)
    on.exit(options(wopt))
    source <- rqda_sel("select name, date, id from source where status = 1 order by lower(name)")
    if (nrow(source) != 0) {
        fnames <- source$name
        Encoding(fnames) <- "UTF-8"
        if (sortByTime) {
            fnames <- fnames[OrderByTime(source$date, decreasing = decreasing)]
        }
        tryCatch(FileNamesWidget[] <- fnames, error = function(e) {
        })
    }
}

LineNumber.expose <- function(da, event, data) {
    ## translated from http://www.pygtk.org/pygtk2tutorial/sec-TextViewExample.html
    textView <- da
    textView$SetBorderWindowSize("GTK_TEXT_WINDOW_LEFT", 30)
    vis <- textView$GetVisibleRect()
    heightVis <- vis$visible.rect$height
    firstY <- vis$visible.rect$y
    lastY <- firstY + heightVis
    posFirst <- gtkTextViewWindowToBufferCoords(textView, "GTK_TEXT_WINDOW_LEFT", 0, firstY)
    posLast <- gtkTextViewWindowToBufferCoords(textView, "GTK_TEXT_WINDOW_LEFT", 0, lastY)
    windowL <- textView$GetWindow("GTK_TEXT_WINDOW_LEFT")
    atTop <- textView$GetLineAtY(firstY)
    iter  <- atTop$target.iter
    top <- atTop$line.top
    count <- 0
    pixels <- numbers <- c()
    while (!iter$IsEnd()) {
        tmp <- textView$GetLineYrange(iter)
        y <- tmp$y
        line_num <- gtkTextViewGetLineAtY(textView, y)$target.iter$GetLine() + 1
        numbers <- c(numbers, line_num)
        height <- tmp$height
        count <- count + 1
        pixels <- c(pixels, y)
        if ((y + height) >= lastY) {
            break
        }
        iter$ForwardLine()
    }
    pixels <- pixels - min(pixels)

    pango <- gtkWidgetCreatePangoLayout(textView, NULL)
    for (i in 1:count) {
        pango$SetText(as.character(numbers[i]))
        gtkPaintLayout(textView$Style, windowL, textView$State(), FALSE, NULL, widget = textView, x = 2, y = pixels[i], layout = pango)
    }
}


ViewFileFun <- function(FileNameWidget, hightlight = TRUE) {
    ## FileNameWidget = .rqda$.fnames_rqda in Files Tab
    ## FileNameWidget = .rqda$.FileofCat in F-CAT Tab
    if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        if (length(svalue(FileNameWidget)) == 0) {
            gmessage(gettext("Select a file first.", domain = "R-RQDA"), icon = "error", con = TRUE)
        } else {
            SelectedFileName <- svalue(FileNameWidget)
            ViewFileFunHelper(SelectedFileName, hightlight = TRUE)
        }}}


ViewFileFunHelper <- function(FileName, hightlight = TRUE, codingTable = .rqda$codingTable, annotation = TRUE) {
    if (exists(".root_edit", envir = .rqda) && isExtant(.rqda$.root_edit)) {
        dispose(.rqda$.root_edit)
    }
    SelectedFileName <- FileName
    wnh <- size(.rqda$.root_rqdagui)
    if (grepl("apple", R.version$platform)) {
        gw <- gwindow(title = SelectedFileName, parent = c(0, 0),
                      width = getOption("widgetSize")[1], height = getOption("widgetSize")[2])
    } else {
        gw <- gwindow(title = SelectedFileName, parent = wnh, ## .rqda$.root_rqdagui,
                      width = getOption("widgetSize")[1],
                      height = getOption("widgetSize")[2]
                      )
    }

    addHandlerKeystroke(gw, function(h, ...) {
        if (h$key == "\027") dispose(gw)
    })

    mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
    gw$set_icon(mainIcon)
                                        # getToolkitWidget(gw)$Move(getOption("widgetCoordinate")[1],
                                        #                           getOption("widgetCoordinate")[2])
    assign(".root_edit", gw, envir = .rqda)
    .root_edit <- get(".root_edit", .rqda)
    tmp <- gtext(container = .root_edit)
    font <- pangoFontDescriptionFromString(.rqda$font)
    gtkWidgetModifyFont(tmp$widget, font)
    tmp$widget$SetPixelsBelowLines(5) ## set the spacing
    tmp$widget$SetPixelsInsideWrap(5) ## so the text looks more confortable.
    assign(".openfile_gui", tmp, envir = .rqda)
    Encoding(SelectedFileName) <- "unknown"
    IDandContent <- rqda_sel(sprintf("select id, file from source where name='%s'",
                                     enc(SelectedFileName))
                             )
    content <- IDandContent$file
    Encoding(content) <- "UTF-8"
    W <- get(".openfile_gui", .rqda)
    insert(W, content)
    W$set_editable(FALSE)
    markidx <- rqda_sel(sprintf("select %s.rowid, selfirst, selend, freecode.name, freecode.color, freecode.id from %s, freecode where fid=%i and %s.status = 1 and freecode.id = cid and freecode.status = 1", codingTable, codingTable, IDandContent$id, codingTable))
    if (annotation) {
        anno <- rqda_sel(sprintf("select position, rowid from annotation where status = 1 and fid=%s", IDandContent$id))
    }
    buffer <- W$buffer
    fore.col <- .rqda$fore.col
    back.col <- .rqda$back.col


                                        # checks if the tag exists otherwise Gtk will complain
    if (is.null(gtkTextTagTableLookup(buffer$`tag-table`, "underline")))
        buffer$createTag("underline", underline = "single")

    if (is.null(gtkTextTagTableLookup(buffer$`tag-table`, fore.col)))
        buffer$createTag(fore.col, foreground = fore.col)

    if (is.null(gtkTextTagTableLookup(buffer$`tag-table`, sprintf("%s.background", back.col))))
        buffer$createTag(sprintf("%s.background", back.col), background = back.col)

    ## create buffer tag, which is created by default since gwidgetRGtk2 changes its API
    N <- nrow(markidx)

    if (nrow(markidx) != 0) { ## make sense only when there is coding there
        for (i in 1:N) {
            iter <- gtkTextBufferGetIterAtOffset(buffer, markidx[i, "selfirst"]) ## index to iter
            buffer$CreateMark(sprintf("%s.1", markidx[i, "rowid"]), where = iter$iter) ## insert marks
            iter <- gtkTextBufferGetIterAtOffset(buffer, markidx[i, "selend"])
            buffer$CreateMark(sprintf("%s.2", markidx[i, "rowid"]), where = iter$iter)
            ## the second iter is used to HL coding
        }
    } ## create marks

    if (annotation) {
        if (nrow(anno) != 0) {
            for (i in 1:nrow(anno)) {
                iter <- gtkTextBufferGetIterAtOffset(buffer, anno[i, "position"]) ## index to iter
                buffer$CreateMark(sprintf("%s.3", anno[i, "rowid"]), where = iter$iter) ## insert marks
            }} ## creat marks for annotation
    }

    if (nrow(markidx) != 0) {
        sapply(markidx[, "rowid"], FUN = function(x) {
            code <- markidx[markidx$rowid == x, "name"]
            Encoding(code) <- "UTF-8"
            codeColor <- markidx[markidx$rowid == x, "color"]
            if (is.na(codeColor)) {
                codeColor <- DefaultCodeColor[as.numeric(markidx[markidx$rowid == x, "id"]) %% length(DefaultCodeColor) + 1]
            }
            m1 <- buffer$GetMark(sprintf("%s.1", x))
            iter1 <- buffer$GetIterAtMark(m1)
            idx1 <- gtkTextIterGetOffset(iter1$iter)
            m2 <- buffer$GetMark(sprintf("%s.2", x))
            iter2 <- buffer$GetIterAtMark(m2)
            idx2 <- gtkTextIterGetOffset(iter2$iter)
            InsertAnchor(.rqda$.openfile_gui, label = sprintf("<%s>", code), index = idx1, label.col = codeColor,
                         handler = TRUE, EndMarkName = sprintf("%s.2", x))
        }) ## end of sapply -> insert code label

        if (hightlight) {
            idx <- sapply(markidx[, "rowid"], FUN = function(x) {
                m1 <- buffer$GetMark(sprintf("%s.1", x))
                iter1 <- buffer$GetIterAtMark(m1)
                idx1 <- gtkTextIterGetOffset(iter1$iter)
                m2 <- buffer$GetMark(sprintf("%s.2", x))
                iter2 <- buffer$GetIterAtMark(m2)
                idx2 <- gtkTextIterGetOffset(iter2$iter)
                return(c(idx1, idx2))
            })## get offset for HL.
            idx <- t(idx)
            HL(W, idx, fore.col = .rqda$fore.col, back.col = NULL)
        }}

    if (annotation) {
        if (nrow(anno) != 0) {
            apply(anno, 1, function(x) {
                m <- buffer$GetMark(sprintf("%s.3", x["rowid"]))
                iter <- buffer$GetIterAtMark(m)
                idx <- gtkTextIterGetOffset(iter$iter)
                InsertAnnotation(index = idx, fid = IDandContent$id, rowid = x["rowid"])
            })}}

    buffer$PlaceCursor(buffer$getIterAtOffset(0)$iter) ## place cursor at the beginning

    ## gSignalConnect(tmp, "expose_event", LineNumber.expose) ## add line number to the widget
    ## does not work well yet
    enabled(button$AnnB) <- TRUE
    enabled(button$MarCodB1) <- (length(svalue(.rqda$.codes_rqda)) == 1)
    ## enabled(button$UnMarB1) <- (length(svalue(.rqda$.codes_rqda)) == 1)
    enabled(button$MarCodB2) <- (length(svalue(.rqda$.CodeofCat)) == 1)
    enabled(button$UnMarB2) <- (length(svalue(.rqda$.CodeofCat)) == 1)
    ## enabled(button$c2memobutton) <- TRUE
    addHandlerUnrealize(gw, handler = function(h, ...) {
        enabled(button$AnnB) <- FALSE
        enabled(button$MarCodB1) <- FALSE
        enabled(button$UnMarB1) <- FALSE
        enabled(button$MarCodB2) <- FALSE
        enabled(button$UnMarB2) <- FALSE
        ## enabled(button$c2memobutton) <- FALSE
        enabled(button$CasMarB) <- FALSE
        enabled(button$CasUnMarB) <- FALSE
        return(FALSE)
    })
}


EditFileFun <- function(FileNameWidget = .rqda$.fnames_rqda) {
    ## FileNameWidget = .rqda$.fnames_rqda in Files Tab
    ## FileNameWidget = .rqda$.FileofCat in F-CAT Tab
    if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        SelectedFileName <- svalue(FileNameWidget)
        if (length(svalue(FileNameWidget)) == 0) {
            gmessage(gettext("Select a file first.", domain = "R-RQDA"), icon = "error", con = TRUE)
        }
        else {
            tryCatch(dispose(.rqda$.root_edit), error = function(e) {
            })
            gw <- gwindow(title = SelectedFileName, #parent = getOption("widgetCoordinate"),
                          width = getOption("widgetSize")[1], height = getOption("widgetSize")[2]
                          )

            addHandlerKeystroke(gw, function(h, ...) {
                if (h$key == "\027") dispose(gw)
            })
            mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
            gw$set_icon(mainIcon)
            assign(".root_edit", gw, envir = .rqda)
            assign(".root_edit2", gpanedgroup(horizontal = FALSE, container = .rqda$.root_edit), envir = .rqda)
            EdiFilB <- gbutton(gettext("Save File", domain = "R-RQDA"), container = .rqda$.root_edit2, handler = function(h, ...) {
                content <- svalue(.rqda$.openfile_gui)
                rqda_exe(sprintf("update source set file='%s', dateM='%s' where name='%s'",
                                 enc(content, "UTF-8"), date(), enc(svalue(.rqda$.root_edit), "UTF-8"))) ## update source table
                if (nrow(mark_index) != 0) { ## only manipulate the coding when there is one.
                    idx <- apply(mark_index, 1, FUN = function(x) {
                        m1 <- buffer$GetMark(sprintf("%s.1", x[3]))
                        iter1 <- buffer$GetIterAtMark(m1)
                        idx1 <- gtkTextIterGetOffset(iter1$iter)
                        m2 <- buffer$GetMark(sprintf("%s.2", x[3]))
                        iter2 <- buffer$GetIterAtMark(m2)
                        idx2 <- gtkTextIterGetOffset(iter2$iter)
                        ans <- c(selfirst = idx1, selend = idx2, x[3])## matrix of 3x N (N = nrow(mark_index))
                    }) ## end of apply
                    apply(idx, 2, FUN = function(x) {
                        if (x[1] == x[2])  rqda_exe(sprintf("update coding set status = 0 where rowid=%i", x[3])) else {
                                                                                                                      Encoding(content) <- "UTF-8"
                                                                                                                      rqda_exe(sprintf("update coding set seltext='%s', selfirst=%i, selend=%i where rowid=%i",
                                                                                                                                       enc(substr(content, x[1], x[2]), "UTF-8"), x[1], x[2], x[3]))
                                                                                                                  }
                    })## update the coding table (seltext, selfirst, selend), on the rowid (use rowid to name the marks)
                }

                if (nrow(mark_indexS) != 0) { ## only manipulate coding2
                    idxS <- apply(mark_indexS, 1, FUN = function(x) {
                        m1 <- buffer$GetMark(sprintf("%s.1S", x[3]))
                        iter1 <- buffer$GetIterAtMark(m1)
                        idx1 <- gtkTextIterGetOffset(iter1$iter)
                        m2 <- buffer$GetMark(sprintf("%s.2S", x[3]))
                        iter2 <- buffer$GetIterAtMark(m2)
                        idx2 <- gtkTextIterGetOffset(iter2$iter)
                        ans <- c(selfirst = idx1, selend = idx2, x[3])## matrix of 3x N (N = nrow(mark_index))
                    }) ## end of apply
                    apply(idxS, 2, FUN = function(x) {
                        if (x[1] == x[2])  rqda_exe(sprintf("update coding2 set status = 0 where rowid=%i", x[3])) else {
                                                                                                                       Encoding(content) <- "UTF-8"
                                                                                                                       rqda_exe(sprintf("update coding2 set seltext='%s', selfirst=%i, selend=%i where rowid=%i",
                                                                                                                                        enc(substr(content, x[1], x[2]), "UTF-8"), x[1], x[2], x[3]))
                                                                                                                   }
                    })
                } ## end of updating coding2

                if (nrow(mark_idx_case) != 0) {
                    idx_case <- apply(mark_idx_case, 1, FUN = function(x) {
                        m1 <- buffer$GetMark(sprintf("c%s.1", x["rowid"]))
                        iter1 <- buffer$GetIterAtMark(m1)
                        idx1 <- gtkTextIterGetOffset(iter1$iter)
                        m2 <- buffer$GetMark(sprintf("c%s.2", x["rowid"]))
                        iter2 <- buffer$GetIterAtMark(m2)
                        idx2 <- gtkTextIterGetOffset(iter2$iter)
                        ans <- c(selfirst = idx1, selend = idx2, x["rowid"])
                    }) ## end of apply
                    apply(idx_case, 2, FUN = function(x) {
                        if (x[1] == x[2])  rqda_exe(sprintf("update caselinkage set status = 0 where rowid=%i", x["rowid"])) else {
                                                                                                                                 rqda_exe(sprintf("update caselinkage set selfirst=%i, selend=%i where rowid=%i", x[1], x[2], x[3]))
                                                                                                                             }
                    })## end of apply
                }
                enabled(button$EdiFilB) <- FALSE
            })## end of save button

            assign("EdiFilB", EdiFilB, envir = button)
            enabled(EdiFilB) <- FALSE
            tmp <- gtext(container = .rqda$.root_edit2)
            font <- pangoFontDescriptionFromString(.rqda$font)
            gtkWidgetModifyFont(tmp$widget, font)
            assign(".openfile_gui", tmp, envir = .rqda)
            Encoding(SelectedFileName) <- "unknown"
            IDandContent <- rqda_sel(sprintf("select id, file from source where name='%s'", enc(SelectedFileName)))
            content <- IDandContent$file
            Encoding(content) <- "UTF-8"
            W <- get(".openfile_gui", .rqda)
            insert(W, content)
            buffer <- W$buffer ## get text buffer.

            fore.col <- .rqda$fore.col
            back.col <- .rqda$back.col

                                        # checks if the tag exists otherwise Gtk will complain
            if (is.null(gtkTextTagTableLookup(buffer$`tag-table`, "underline")))
                buffer$createTag("underline", underline = "single")

            if (is.null(gtkTextTagTableLookup(buffer$`tag-table`, fore.col)))
                buffer$createTag(fore.col, foreground = fore.col)

            if (is.null(gtkTextTagTableLookup(buffer$`tag-table`, sprintf("%s.background", back.col))))
                buffer$createTag(sprintf("%s.background", back.col), background = back.col)


            mark_index <- rqda_sel(sprintf("select selfirst, selend, rowid from coding where fid=%i and status = 1",
                                           IDandContent$id))
            if (nrow(mark_index) != 0) {## make sense only when there is coding there
                ClearMark(W, 0, max(mark_index$selend), TRUE, FALSE)
                HL(W, index = mark_index[, c("selfirst", "selend")], .rqda$fore.col, NULL)
                ## insert marks according to mark_index (use rowid to name the marks)
                apply(mark_index, 1, function(x) {
                    iter <- gtkTextBufferGetIterAtOffset(buffer, x[1]) ## index to iter
                    mark <- buffer$CreateMark(sprintf("%s.1", x[3]), where = iter$iter)         ## insert marks
                    ## gtkTextMarkSetVisible(mark, TRUE)                   ## set itvisible
                    iter <- gtkTextBufferGetIterAtOffset(buffer, x[2]) ## index to iter
                    mark <- buffer$CreateMark(sprintf("%s.2", x[3]), where = iter$iter)         ## insert marks
                    ## gtkTextMarkSetVisible(mark, TRUE)                   ## set itvisible
                }) ## end of apply
            }

            mark_indexS <- rqda_sel(sprintf("select selfirst, selend, rowid from coding2 where fid=%i and status = 1", IDandContent$id))
            if (nrow(mark_indexS) != 0) {
                apply(mark_indexS, 1, function(x) {
                    iter <- gtkTextBufferGetIterAtOffset(buffer, x[1]) ## index to iter
                    mark <- buffer$CreateMark(sprintf("%s.1S", x[3]), where = iter$iter)
                    iter <- gtkTextBufferGetIterAtOffset(buffer, x[2]) ## index to iter
                    mark <- buffer$CreateMark(sprintf("%s.2S", x[3]), where = iter$iter)
                }) ## end of apply
            }

            mark_idx_case <- rqda_sel(sprintf("select selfirst, selend, rowid from caselinkage where fid=%i and status = 1",
                                              IDandContent$id))
            if (nrow(mark_idx_case) != 0) {
                ClearMark(W, 0, max(mark_idx_case$selend), FALSE, TRUE)
                HL(W, index = mark_idx_case[, c("selfirst", "selend")], NULL, .rqda$back.col)
                apply(mark_idx_case, 1, function(x) {
                    iter <- gtkTextBufferGetIterAtOffset(buffer, x["selfirst"])
                    mark <- buffer$CreateMark(sprintf("c%s.1", x["rowid"]), where = iter$iter)
                    gtkTextMarkSetVisible(mark, TRUE)
                    iter <- gtkTextBufferGetIterAtOffset(buffer, x["selend"])
                    mark <- buffer$CreateMark(sprintf("c%s.2", x["rowid"]), where = iter$iter)
                    gtkTextMarkSetVisible(mark, TRUE)
                }) ## end of apply
            }
            gSignalConnect(.rqda$.openfile_gui$buffer, "changed",
                           function(h, ...) {
                               enabled(button$EdiFilB) <- TRUE
                           })
            addHandlerUnrealize(.rqda$.openfile_gui, handler = function(h, ...) {
                rm("EdiFilB", envir = button)
                rm(".root_edit", ".root_edit2", ".openfile_gui", envir = .rqda)
                FALSE
            })
        } ## end of else
    }
}

#' @export
write.FileList <- function(FileList, encoding = .rqda$encoding, con = .rqda$qdacon, ...) {
    ## import a list of files into the source table
    ## FileList is a list of file content, with names(FileList) the name of the files.
    WriteToTable <- function(Fname, content) {
        ## helper function
        ## FnameUTF8 <- iconv(Fname, to = "UTF-8")
        FnameUTF8 <- enc(Fname, encoding = encoding)
        content <- enc(content, encoding = encoding) ## adjust encoding argument.
        if (Encoding(content) != "UTF-8") {
            content <- iconv(content, to = "UTF-8", sub = "byte") ## UTF-8 file content
        }
        maxid <- rqda_sel("select max(id) from source")[[1]]
        nextid <- ifelse(is.na(maxid), 0 + 1, maxid + 1)
        write <- FALSE
        ## check if the content should be written into con.
        if (nextid == 1) {
            write <- TRUE
            ## if this is the first file, no need to worry about the duplication issue.
        } else {
            if (nrow(rqda_sel(sprintf("select name from source where name='%s'", FnameUTF8))) == 0) {
                ## no duplication file exists, then write.
                write <- TRUE
            } else {
                cat(sprintf("%s exists in the database!\n", Fname))
            }
        }
        if (write) {
            rqda_exe(sprintf("insert into source (name, file, id, status, date, owner)
                             values ('%s', '%s', %i, %i, '%s', '%s')",
                             FnameUTF8, content, nextid, 1, date(), .rqda$owner))
        }
    }
    FileNames <- names(FileList)
    FileNames[FileNames == ""] <- as.character(1:sum(FileNames == ""))

    if (is_projOpen()) {
        for (i in 1:length(FileList)) {
            WriteToTable(FileNames[i], FileList[[i]])
        }
        FileNamesUpdate(FileNamesWidget = .rqda$.fnames_rqda)
    } else gmessage(gettext("Open a project first.", domain = "R-RQDA"), container = TRUE)
}

#' @export
addFilesFromDir <- function(dir, pattern = "*.txt$") {
    oldDir <- getwd()
    setwd(dir)
    Files <- list.files(pattern = pattern)

    cat("importing: ", length(Files), "file(s)\n")
    cat(paste(Files, collapse = "\n"), "\n")

                                        # FixMe: make encoding optional why do we need this anyway?
    ImportFile(Files, .rqda$qdacon, encoding = "UTF-8")
    on.exit(setwd(oldDir))
}

FileNameWidgetUpdate <- function(FileNamesWidget = .rqda$.fnames_rqda, sort = TRUE, decreasing = FALSE, FileId = NULL, ...) {
    ##update file names list in the FileNamesWidget
    wopt <- options(warn = -2)
    on.exit(options(wopt))
    source <- rqda_sel("select name, date, id from source where status = 1")
    if (nrow(source) == 0) {
        fnames <- NULL
    } else {
        Encoding(source$name) <- "UTF-8"
        if (!is.null(FileId)) {
            source <- source[source$id %in% FileId, ]
            fnames <- source$name##when FileId is not in source$id, fnames is character(0), still works.
            date <- source$date
        } else{
            fnames <- source$name
            date <- source$date
        }
        if (sort) {
            fnames <- fnames[OrderByTime(date, decreasing = decreasing)]
        }
    }
    tryCatch(FileNamesWidget[] <- fnames, error = function(e) {
    })
}

#' @export
getFileIds <- function(condition = c("unconditional", "case", "filecategory", "both"), type = c("all", "coded", "uncoded", "selected")) {
    ## helper function
    unconditionalFun <- function(type) {
        if (type == "selected") {
            selected <- svalue(.rqda$.fnames_rqda)
            ans <- rqda_sel(
                sprintf("select id from source where status = 1 and name in (%s)",
                        paste(paste("'", enc(selected), "'", sep = ""), collapse = ", ")
                        ))$id
        } else {
            allfid <- rqda_sel("select id from source where status = 1 group by id")$id
            if (type != "all") {
                fid_coded <- rqda_sel("select fid from coding where status = 1 group by fid")$fid
            }
            if (type == "all") {
                ans <- allfid
            } else if (type == "coded") {
                ans <- fid_coded
            } else if (type == "uncoded") {
                ans <- allfid[! (allfid %in% fid_coded)]
            }
        }
        ans
    }

    FidOfCaseFun <- function(type) {
        if (type == "selected") {
            selected <- svalue(.rqda$.FileofCase)
            ans <- rqda_sel(
                sprintf("select id from source where status = 1 and name in (%s)",
                        paste(paste("'", enc(selected), "'", sep = ""), collapse = ", ")
                        ))$id
        } else {
            Selected <- svalue(.rqda$.CasesNamesWidget)
            if (length(Selected) == 0) {
                ans <- NULL
            } else {
                if (length(Selected) > 1) {
                    gmessage(gettext("select one file category only.", domain = "R-RQDA"), container = TRUE)
                    stop("more than one file categories are selected", domain = "R-RQDA")

                }
                caseid <- rqda_sel(sprintf("select id from cases where status = 1 and name='%s'",
                                           enc(Selected)))$id
                fidofcase <- rqda_sel(sprintf("select fid from caselinkage where status = 1 and caseid=%i", caseid))$fid
                ##         caseid <- rqda_sel(sprintf("select id from cases where status = 1 and name in (%s)",
                ##                                                  paste(paste("'", Selected, "'", sep = ""), collapse = ", ")))$id
                ##         fidofcase <- rqda_sel(sprintf("select fid from caselinkage where status == 1 and caseid in (%s)",
                ##                                                     paste(paste("'", caseid, "'", sep = ""), collapse = ", ")))$fid
                ## roll back to rev 90
                allfid <- unconditionalFun(type = type)
                ans <- intersect(fidofcase, allfid)
            }
        }
        ans
    }

    FidOfCatFun <- function(type) {
        if (type == "selected") {
            selected <- svalue(.rqda$.FileofCat)
            ans <- rqda_sel(
                sprintf("select id from source where status = 1 and name in (%s)",
                        paste(paste("'", enc(selected), "'", sep = ""), collapse = ", ")
                        ))$id
        }
        allfid <- getFileIdsSets("filecategory", "intersect")
        if (type == "all") {
            ans <- allfid
        } else {
            codedfid <- rqda_sel(sprintf("select fid from coding where status = 1 and fid in (%s) group by fid", paste(shQuote(allfid), collapse = ", ")))$fid
            if (type == "coded") {
                ans <- codedfid
            }
            if (type == "uncoded") {
                ans <- setdiff(allfid, codedfid)
            }
        }
        ans
    }

    bothFun <- function(type) {
        ans <- intersect(getFileIds("case", type), getFileIds("file", type))
        ans
    }

    condition <- match.arg(condition)
    type <- match.arg(type)
    fid <- switch(condition,
                  unconditional = unconditionalFun(type = type),
                  case = FidOfCaseFun(type = type),
                  filecategory = FidOfCatFun(type = type),
                  both = bothFun(type = type)
                  )
    if (is.null(fid)) fid <- integer(0)
    class(fid) <- c("RQDA.vector", "fileId")
    fid
}


#' @export
getFileIdsSets <- function(set = c("case", "filecategory"), relation = c("union", "intersect")) {
    set <- match.arg(set)
    relation <- match.arg(relation)
    if (set == "case") {
        Selected <- svalue(.rqda$.CasesNamesWidget)
        if (length(Selected) == 0) {
            ans <- NULL
        } else {
            Selected <- gsub("'", "''", Selected)
            if (relation == "union") {
                ans <- rqda_sel(sprintf("select fid from caselinkage where status = 1 and caseid in (select id from cases where status = 1 and name in (%s)) group by fid", paste(paste("'", Selected, "'", sep = ""), collapse = ", ")))$fid
            } else if (relation == "intersect") {
                ans <- rqda_sel(sprintf("select fid, count(fid) as n from caselinkage where status = 1 and caseid in (select id from cases where status = 1 and name in (%s)) group by fid having n= %i", paste(paste("'", Selected, "'", sep = ""), collapse = ", "), length(Selected)))$fid
            }
        }
    }## end of set == "case"
    if (set == "filecategory") {
        Selected <- svalue(.rqda$.FileCatWidget)
        if (length(Selected) == 0) {
            ans <- NULL
        } else {
            Selected <- gsub("'", "''", Selected)
            if (relation == "union") {
                ans <- rqda_sel(sprintf("select fid from treefile where status = 1 and catid in (select catid from filecat where status = 1 and name in (%s)) group by fid", paste(paste("'", Selected, "'", sep = ""), collapse = ", ")))$fid
            } else if (relation == "intersect") {
                ans <- rqda_sel(sprintf("select fid, count(fid) as n from treefile where status = 1 and catid in (select catid from filecat where status = 1 and name in (%s)) group by fid having n= %i", paste(paste("'", Selected, "'", sep = ""), collapse = ", "), length(Selected)))$fid
            }
        }
    } ## end of set == "filecategory"
    if (is.null(ans)) ans <- integer(0)
    class(ans) <- c("RQDA.vector", "fileId")
    ans
}

AddToFileCategory <- function(Widget = .rqda$.fnames_rqda, updateWidget = TRUE) {
    ## filenames -> fid -> selfirst = 0; selend = nchar(filesource)
    filename <- svalue(Widget)
    Encoding(filename) <- "unknown"
    query <- rqda_sel(sprintf("select id, file from source where name in(%s) and status = 1", paste("'", enc(filename), "'", sep = "", collapse = ", "))) ## multiple fid
    fid <- query$id
    Encoding(query$file) <- "UTF-8"
    ## select a F-cat name -> F-cat id
    Fcat <- rqda_sel("select catid, name from filecat where status = 1")
    if (nrow(Fcat) == 0) {
        gmessage(gettext("Add File Category first.", domain = "R-RQDA"), container = TRUE)
    } else{
        Encoding(Fcat$name) <- "UTF-8"
        Selecteds <- gselect.list(Fcat$name, multiple = TRUE)
        if (length(Selecteds) > 0 && Selecteds != "") {
            Encoding(Selecteds) <- "UTF-8"
            for (Selected in Selecteds) {
                Fcatid <- Fcat$catid[Fcat$name %in% Selected]
                exist <- rqda_sel(sprintf("select fid from treefile where status = 1 and fid in (%s) and catid=%i", paste("'", fid, "'", sep = "", collapse = ", "), Fcatid))
                if (nrow(exist) != length(fid)) {
                    ## write only when the selected file associated with specific f-cat is not there
                    DAT <- data.frame(fid = fid[!fid %in% exist$fid], catid = Fcatid, date = date(), dateM = date(), memo = "", status = 1, owner = .rqda$owner)
                    ## should pay attention to the var order of DAT, must be the same as that of treefile table
                    success <- rqda_wrt("treefile", DAT)
                    ## write to caselinkage table
                    if (success && updateWidget) {
                        UpdateFileofCatWidget()
                    }
                    if (!success) gmessage(sprintf(gettext("Fail to write to file category of %s", domain = "R-RQDA"), Selected))
                }
            }
        } else {
            invisible(FALSE)
        }
    }
}


## library(RGtk2)
searchWord <- function(str, widget, from = 0, col = "green", verbose = FALSE) {
    tview <- widget$widget
    buffer <- tview$buffer
    Iter0 <- buffer$GetIterAtOffset(from)$iter
    ans <- gtkTextIterForwardSearch(Iter0, str, "GTK_TEXT_SEARCH_VISIBLE_ONLY")
    if (ans$retval) {
        gtkTextViewScrollToIter(tview, ans$match.start, 0.47)

        if (is.null(gtkTextTagTableLookup(buffer$`tag-table`, sprintf("%s.background", col))))
            buffer$createTag(sprintf("%s.background", col), background = col)

        buffer$ApplyTagByName(sprintf("%s.background", col), ans$match.start, ans$match.end)
        ans$match.end$GetOffset()
    } else {
        if (verbose) gmessage(gettext("Reach the end.", domain = "R-RQDA"))
        invisible(NULL)
    }
}

SearchButton <- function(widget) {
    ## widget = .rqda$.openfile_gui)
    assign("searchFrom", 0, envir = .rqda)
    group <- ggroup(horizontal = FALSE, container = gwindow(
                                            width = getOption("widgetSize")[1], height = getOption("widgetSize")[2], title = "Search a word"))
    kwdW <- gedit("", container = group)
    gbutton(gettext("Search next", domain = "R-RQDA"), container = group, handler = function(h, ...) {
        if (!is.null(.rqda$searchFrom)) {
            str <- svalue(h$action)
            Encoding(str) <- "UTF-8"
            res <- searchWord(str, widget = widget, from = .rqda$searchFrom, verbose = TRUE)
            assign("searchFrom", res, envir = .rqda)
        }}, action = kwdW)
    gbutton(gettext("Restart", domain = "R-RQDA"), container = group, handler = function(h, ...) {
        assign("searchFrom", 1, envir = .rqda)
    })
}


#' @export
viewPlainFile <- function(FileNameWidget = .rqda$.fnames_rqda) {
    if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        if (length(svalue(FileNameWidget)) == 0) {
            gmessage(gettext("Select a file first.", domain = "R-RQDA"), icon = "error", con = TRUE)
        } else {
            SelectedFileName <- svalue(FileNameWidget)

            wnh <- size(.rqda$.root_rqdagui) ## size of the main window
            gw <- gwindow(title = SelectedFileName, parent = wnh, ## .rqda$.root_rqdagui,
                          width = getOption("widgetSize")[1], height = getOption("widgetSize")[2])

            addHandlerKeystroke(gw, function(h, ...) {
                if (h$key == "\027") dispose(gw)
            })
            mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
            gw$set_icon(mainIcon)
                                        # getToolkitWidget(gw)$Move(getOption("widgetCoordinate")[1], getOption("widgetCoordinate")[2])
            tmp <- gtext(container = gw)
            font <- pangoFontDescriptionFromString(.rqda$font)
            gtkWidgetModifyFont(tmp$widget, font)
            tmp$widget$SetPixelsBelowLines(5) ## set the spacing
            tmp$widget$SetPixelsInsideWrap(5) ## so the text looks more confortable.
            Encoding(SelectedFileName) <- "unknown"
            IDandContent <- rqda_sel(sprintf("select id, file from source where name='%s'",
                                             enc(SelectedFileName))
                                     )
            content <- IDandContent$file
            Encoding(content) <- "UTF-8"
            insert(tmp, content)
            tmp$widget$set_editable(FALSE)
        }}}
