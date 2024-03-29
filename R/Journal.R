AddJournalButton <- function(label = gettext("Add", domain = "R-RQDA")) {
    AddJouB <- gbutton(label, handler = function(h, ...) {
        AddNewJournalFun()
    }
    )
    assign("AddJouB", AddJouB, envir = button)
    enabled(AddJouB) <- FALSE
    AddJouB
}

DeleteJournalButton <- function(label = gettext("Delete", domain = "R-RQDA")) {
    DelJouB <- gbutton(label, handler = function(h, ...) {
        del <- gconfirm(gettext("Really delete the journal?", domain = "R-RQDA"), icon = "question")
        if (isTRUE(del)) {
            Selected <- svalue(.rqda$.JournalNamesWidget)
            Encoding(Selected) <- "UTF-8"
            rqda_exe(sprintf("update journal set status = 0 where name='%s'", enc(Selected)))
            JournalNamesUpdate()
        }
    }
    )
    assign("DelJouB", DelJouB, envir = button)
    enabled(DelJouB) <- FALSE
    DelJouB
}

RenameJournalButton <- function(label = gettext("Rename", domain = "R-RQDA")) {
    RenJouB <- gbutton(label, handler = function(h, ...) {
        selected <- svalue(.rqda$.JournalNamesWidget)
        NewName <- ginput(gettext("Enter new journal name. ", domain = "R-RQDA"), text = substring(selected, 20), icon = "info")
        Encoding(NewName) <- "UTF-8"
        NewName <- paste(substring(selected, 0, 19), NewName, sep = " ")

        if (!identical(NewName, character(0))) {
            if (!is.na(NewName)) {
                rename(from = selected, to = NewName, "journal")
                JournalNamesUpdate()
            }
        }
    }
    )
    assign("RenJouB", RenJouB, envir = button)
    enabled(RenJouB) <- FALSE
    RenJouB
}


OpenJournalButton <- function(label = gettext("Open", domain = "R-RQDA")) {
    OpeJouB <- gbutton(label, handler = function(h, ...) {
        ViewJournalWidget()
    })
    assign("OpeJouB", OpeJouB, envir = button)
    enabled(OpeJouB) <- FALSE
    OpeJouB
}

JournalNamesUpdate <- function(Widget = .rqda$.JournalNamesWidget, decreasing = FALSE, ...) {
    if (is_projOpen()) {
        journal <- rqda_sel("select name from journal where status = 1")
        if (nrow(journal) == 0) {
            journal <- NULL
        } else {
            journal <- journal[, 1]
            Encoding(journal) <- "UTF-8"
            journal <- journal[OrderByTime(substring(journal, 0, 20), decreasing = decreasing)]
        }
        tryCatch(Widget[] <- journal, error = function(e) {
        })
    }
}

AddNewJournalFun <- function() {
    if (is_projOpen(envir = .rqda, "qdacon")) {
        tryCatch(eval(parse(text = "dispose(.rqda$.AddNewJournalWidget")), error = function(e) {
        }) ## close the widget if open
        gw <- gwindow(title = "Add New Journal.", parent = getOption("widgetCoordinate"),
                      width = getOption("widgetSize")[1], height = getOption("widgetSize")[2]
                      )

        addHandlerKeystroke(gw, function(h, ...) {
            if (h$key == "\027") dispose(gw)
        })

                                        # get size of root gui as width and height
        wdh <- size(.rqda$.root_rqdagui)
        head_s <- c(wdh["width"], wdh["height"] * .1)
        body_s <- c(wdh["width"], wdh["height"] * .9)

        mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
        gw$set_icon(mainIcon)
        assign(".AddNewJournalWidget", gw, envir = .rqda)
        assign(".AddNewJournalWidget2", gpanedgroup(horizontal = FALSE, container = get(".AddNewJournalWidget", envir = .rqda)), envir = .rqda)
        jbut <- gbutton(gettext("Save Journal", domain = "R-RQDA"), container = get(".AddNewJournalWidget2", envir = .rqda), handler = function(h, ...) {
            ## title <- ginput(gettext("Enter new file name. ", domain = "R-RQDA"), text = Sys.time(), icon = "info")
            title <- as.character(Sys.time())
            if (!is.na(title)) {
                if (nrow(rqda_sel(sprintf("select name from journal where name='%s'", enc(title)))) != 0) {
                    title <- paste("New", title)
                }## Make sure it is unique
                content <- svalue(textW)
                content <- enc(content, encoding = "UTF-8") ## take care of double quote.
                ans <- rqda_exe(sprintf("insert into journal (name, journal, date, owner, status)
                             values ('%s', '%s', '%s', '%s', %i)",
                             enc(title), content, date(), .rqda$owner, 1))
                if (is.null(ans)) {
                    dispose(.rqda$.AddNewJournalWidget)
                    ##gmessage(gettext("Succeed.", domain = "R-RQDA"), container = TRUE)
                }
                ## must put here rather than in AddJournalButton()
                JournalNamesUpdate()
            }}) ## end of save button

        size(jbut) <- head_s
        tmp <- gtext(container = get(".AddNewJournalWidget2", envir = .rqda))
        size(tmp) <- body_s
        font <- pangoFontDescriptionFromString(.rqda$font)
        gtkWidgetModifyFont(tmp$widget, font)
        assign(".AddNewJournalWidgetW", tmp, envir = .rqda)
        textW <- get(".AddNewJournalWidgetW", envir = .rqda)
    }}

ViewJournalWidget <- function(prefix = "Journal", widget = .rqda$.JournalNamesWidget, dbTable = "journal") {
    if (is_projOpen(envir = .rqda, "qdacon")) {
        Selected <- svalue(widget)
        if (length(Selected) == 0) {
            gmessage(gettext("Select first.", domain = "R-RQDA"), icon = "error", container = TRUE)
        }
        else {
            tryCatch(eval(parse(text = sprintf("dispose(.rqda$.%smemo)", prefix))), error = function(e) {
            })
            gw <- gwindow(title = sprintf("%s:%s", prefix, Selected), parent = getOption("widgetCoordinate"),
                          width = getOption("widgetSize")[1], height = getOption("widgetSize")[2]
                          )

            addHandlerKeystroke(gw, function(h, ...) {
                if (h$key == "\027") dispose(gw)
            })
            mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
            gw$set_icon(mainIcon)
            assign(sprintf(".%smemo", prefix), gw, envir = .rqda)
            assign(sprintf(".%smemo2", prefix),
                   gpanedgroup(horizontal = FALSE, container = get(sprintf(".%smemo", prefix), envir = .rqda)),
                   envir = .rqda)
            saveJournalButton <- gbutton(gettext("Save Journal", domain = "R-RQDA"), container = get(sprintf(".%smemo2", prefix), envir = .rqda), handler = function(h, ...) {
                newcontent <- svalue(W)
                newcontent <- enc(newcontent, encoding = "UTF-8") ## take care of double quote.
                Encoding(Selected) <- "UTF-8"
                rqda_exe(sprintf("update %s set journal='%s' where name='%s'", dbTable, newcontent, enc(Selected)))
                enabled(button$saveJournalB) <- FALSE
            }
            )## end of save button
            assign("saveJournalB", saveJournalButton, envir = button)
            enabled(saveJournalButton) <- FALSE
            tmp <- gtext(container = get(sprintf(".%smemo2", prefix), envir = .rqda))
            font <- pangoFontDescriptionFromString(.rqda$font)
            gtkWidgetModifyFont(tmp$widget, font)## set the default fontsize
            assign(sprintf(".%smemoW", prefix), tmp, envir = .rqda)
            prvcontent <- rqda_sel(sprintf("select journal from %s where name='%s'", dbTable, enc(Selected)))[1, 1]
            if (is.na(prvcontent)) prvcontent <- ""
            Encoding(prvcontent) <- "UTF-8"
            W <- get(sprintf(".%smemoW", prefix), envir = .rqda)
            insert(W, prvcontent, do.newline = FALSE, where = "beginning")
            addHandlerKeystroke(tmp, handler = function(h, ...) {
                enabled(button$saveJournalB) <- TRUE
            })
            addHandlerUnrealize(get(sprintf(".%smemo", prefix), envir = .rqda), handler = function(h, ...) {
                withinWidget <- svalue(get(sprintf(".%smemoW", prefix), envir = .rqda))
                InRQDA <- rqda_sel(sprintf("select journal from %s where name='%s'", dbTable, enc(Selected)))[1, 1]
                if (isTRUE(all.equal(withinWidget, InRQDA))) {
                    return(FALSE)
                } else {
                    val <- gconfirm(gettext("The Journal has been changed. Close anyway?", domain = "R-RQDA"), container = TRUE)
                    return(!val)
                }
            }
            )
        }
    }
}
