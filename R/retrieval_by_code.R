retrieval_by_code <- function(Fid = NULL, order = c("fname", "ftime", "ctime"), code, codingTable = "coding") {
    currentCode2 <- code
    if (length(currentCode2) != 0) {
        currentCode <- enc(currentCode2, "UTF-8")
        Encoding(currentCode2) <- "UTF-8"
        currentCid <- rqda_sel(sprintf("select id from freecode where name= '%s' ",
                                       currentCode))[1, 1]
        order <- match.arg(order)
        order <- switch(order, fname = "order by source.name",
                        ftime = "order by source.id", ctime = "")
        if (is.null(Fid)) {
            retrieval <- rqda_sel(sprintf("select cid, fid, selfirst, selend, seltext, %s.rowid, source.name, source.id from %s, source where %s.status = 1 and cid=%i and source.id = fid %s",
                                          codingTable, codingTable, codingTable, currentCid,
                                          order))
        }
        else {
            retrieval <- rqda_sel(sprintf("select cid, fid, selfirst, selend, seltext, %s.rowid, source.name, source.id from %s, source where %s.status = 1 and cid=%i and source.id = fid and fid in (%s) %s",
                                          codingTable, codingTable, codingTable, currentCid,
                                          paste(Fid, collapse = ", "), order))
        }
        if (nrow(retrieval) == 0)
            gmessage(gettext("No Coding associated with the selected code.", domain = "R-RQDA"),
                     container = TRUE)
        else {
            fid <- unique(retrieval$fid)
            retrieval$fname <- ""
            Nfiles <- length(fid)
            Ncodings <- nrow(retrieval)
            if (Ncodings == 1) {
                title <- sprintf(ngettext(Nfiles,
                                          "1 retrieved coding: \"%s\" from %i file",
                                          "1 retrieved coding: \"%s\" from %i files", domain = "R-RQDA"),
                                 currentCode2, Nfiles)
            } else {
                title <- sprintf(ngettext(Nfiles,
                                          "%i retrieved codings: \"%s\" from %i file",
                                          "%i retrieved codings: \"%s\" from %i files", domain = "R-RQDA"),
                                 Ncodings, currentCode2, Nfiles)
            }
            tryCatch(eval(parse(text = sprintf("dispose(.rqda$.codingsOf%s)",
                                               currentCid))), error = function(e) {
                                               })
            wnh <- size(.rqda$.root_rqdagui)
            .gw <- gwindow(title = title, parent = c(wnh[1] +
                                                     10, 2),
                           width = getOption("widgetSize")[1], height = getOption("widgetSize")[2]
                           )

            addHandlerKeystroke(.gw, function(h, ...) {
                if (h$key == "\027") dispose(.gw)
            })

            mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
            .gw$set_icon(mainIcon)
            assign(sprintf(".codingsOf%s", currentCid), .gw,
                   envir = .rqda)
            .retreivalgui <- gtext(container = .gw)
            font <- pangoFontDescriptionFromString(.rqda$font)
            gtkWidgetModifyFont(.retreivalgui$widget,
                                font)
            .retreivalgui$widget$SetPixelsBelowLines(5)
            .retreivalgui$widget$SetPixelsInsideWrap(5)
            for (i in fid) {
                FileName <- rqda_sel(sprintf("select name from source where status = 1 and id=%i",
                                             i))[["name"]]
                if (!is.null(FileName)) {
                    Encoding(FileName) <- "UTF-8"
                    retrieval$fname[retrieval$fid == i] <- FileName
                }
                else {
                    retrieval <- retrieval[retrieval$fid != i,
                                           ]
                    rqda_exe(sprintf("update %s set status = 0 where fid=%i",
                                     codingTable, i))
                }
            }
            Encoding(retrieval$seltext) <- Encoding(retrieval$fname) <- "UTF-8"
            ComputeCallbackFun <- function(FileName, rowid) {
                CallBackFUN <- function(widget, event, ...) {
                    ViewFileFunHelper(FileName, hightlight = FALSE)
                    textView <- .rqda$.openfile_gui$widget
                    buffer <- textView$buffer
                    mark1 <- gtkTextBufferGetMark(buffer, sprintf("%s.1",
                                                                  rowid))
                    gtkTextViewScrollToMark(textView, mark1, 0)
                    iter1 <- buffer$GetIterAtMark(mark1)$iter
                    idx1 <- gtkTextIterGetOffset(iter1)
                    mark2 <- buffer$GetMark(sprintf("%s.2", rowid))
                    gtkTextMarkSetVisible(mark2, TRUE)
                    iter2 <- buffer$GetIterAtMark(mark2)$iter
                    idx2 <- gtkTextIterGetOffset(iter2)
                    HL(.rqda$.openfile_gui, data.frame(idx1, idx2),
                       fore.col = .rqda$fore.col, back.col = NULL)
                }
                CallBackFUN
            }
            buffer <- .retreivalgui$buffer
            if (is.null(gtkTextTagTableLookup(buffer$`tag-table`, "red")))
                buffer$createTag("red", foreground = "red")
            iter <- buffer$getIterAtOffset(0)$iter
            apply(retrieval, 1, function(x) {
                metaData <- sprintf("%s [%i:%i]", x[["fname"]],
                                    as.numeric(x[["selfirst"]]), as.numeric(x[["selend"]]))
                buffer$InsertWithTagsByName(iter, metaData, "red")
                anchorcreated <- buffer$createChildAnchor(iter)
                iter$BackwardChar()
                anchor <- iter$getChildAnchor()
                lab <- gtkLabelNew(gettext("Back", domain = "R-RQDA"))
                widget <- gtkEventBoxNew()
                widget$Add(lab)
                gSignalConnect(widget, "button-press-event",
                               ComputeCallbackFun(x[["fname"]], as.numeric(x[["rowid"]])))
                .retreivalgui$widget$addChildAtAnchor(widget,
                                                      anchor)
                widget$showAll()
                iter$ForwardChar()
                buffer$insert(iter, "\n")
                buffer$InsertWithTagsByName(iter, x[["seltext"]])
                buffer$insert(iter, "\n\n")
            })
            buffer$PlaceCursor(buffer$getIterAtOffset(0)$iter)
        }
    }
}
