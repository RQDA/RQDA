new_proj <- function(path, conName = "qdacon", assignenv = .rqda, ...) {
    ## success <- file.create(tmpNamme <- tempfile(pattern = "file"
    ## , tmpdir = dirname(path)))
    success <- (file.access(names = dirname(path), mode = 2) == 0)
    if (!success) {
        gmessage(gettext("No write permission.", domain = "R-RQDA"),
                 icon = "error", container = TRUE)
    }
    else{
        ## unlink(tmpNamme)
        ## deal with the ".rqda"
        path <- paste(gsub("\\.rqda$", "", path), "rqda", sep = ".")
        override <- FALSE
        if (fexist <- file.exists(path)) {
            ## if there exists a file, should ask; and test if have write access
            ## to overwrite it.
            override <- gconfirm(
                gettext("Overwrite existing project?", domain = "R-RQDA"),
                icon = "warning")
            if (file.access(path, 2) != 0 && override) {
                override <- FALSE
                gmessage(
                    gettext("You have no write permission to overwrite it.",
                            domain = "R-RQDA"),
                    con = TRUE, icon = "error")
            }
        }
        if (!fexist | override) {
            ## close con in assignmenv first.
            tryCatch(closeProject(conName = conName, assignenv = assignenv),
                     error = function(e) {
                     })
            if (Encoding(path) == "UTF-8") {
                Encoding(path) = "unknown"
                ## otherwise, it is illegible under windows when path contains
                ## chinese because it is in utf8 encoding
                assign(conName, dbConnect(drv = dbDriver("SQLite"),
                                          dbname = path), envir = assignenv)
                Encoding(path) <- "UTF-8"
            } else {
                assign(conName, dbConnect(drv = dbDriver("SQLite"),
                                          dbname = path), envir = assignenv)
            }
            con <- get(conName, assignenv)

            if (dbExistsTable(con, "source")) dbRemoveTable(con, "source")
            ## interview record
            rqda_exe(
                paste("create table source (name text, id integer, file text, ",
                      "memo text, owner text, date text, dateM text, status integer)")
            )
            ## dateM means modified date
            if (dbExistsTable(con, "freecode")) dbRemoveTable(con, "freecode")
            ## list of free codes
            rqda_exe(
                paste("create table freecode (name text, memo text, owner text, ",
                      "date text, dateM text, id integer, status integer, color text)")
            )
            if (dbExistsTable(con, "treecode")) dbRemoveTable(con, "treecode")
            ## tree-like strcuture of code (relationship between code and
            ## code-category[codecat])
            rqda_exe(
                paste("create table treecode  (cid integer, catid integer, date text, ",
                      "dateM text, memo text, status integer, owner text)")
            )
            if (dbExistsTable(con, "treefile")) dbRemoveTable(con, "treefile")
            ## tree-like structure of interview record  (relationship between file
            ## and file category [filecat])
            rqda_exe(
                paste("create table treefile  (fid integer, catid integer, date text, ",
                      "dateM text, memo text, status integer, owner text)")
            )
            if (dbExistsTable(con, "filecat")) dbRemoveTable(con, "filecat")
            ## file category
            rqda_exe(
                paste("create table filecat  (name text, fid integer, catid integer, ",
                      "owner text, date text, dateM text, memo text, status integer)")
            )
            if (dbExistsTable(con, "codecat")) dbRemoveTable(con, "codecat")
            ## code category
            rqda_exe(
                paste("create table codecat  (name text, cid integer, catid integer, ",
                      "owner text, date text, dateM text, memo text, status integer)")
            )
            if (dbExistsTable(con, "coding")) dbRemoveTable(con, "coding")
            ## coding: code and its coded text chunks
            rqda_exe(
                paste("create table coding  (cid integer, fid integer, seltext text, ",
                      "selfirst real, selend real, status integer, owner text, ",
                      "date text, memo text)")
            )
            if (dbExistsTable(con, "coding2")) dbRemoveTable(con, "coding2")
            ## second coding
            rqda_exe(
                paste("create table coding2  (cid integer, fid integer, seltext text, ",
                      "selfirst real, selend real, status integer, owner text, ",
                      "date text, memo text)")
            )
            if (dbExistsTable(con, "project")) dbRemoveTable(con, "project")
            ## rqda_exe("create table project
            ## (encoding text, databaseversion text, date text, dateM text,
            ## memo text, BOM integer)")
            rqda_exe(
                paste("create table project  (databaseversion text, date text, ",
                      "dateM text, memo text, about text)")
            )
            rqda_exe(
                sprintf(paste("insert into project (databaseversion, date, about, memo)",
                              "values ('0.2.2', '%s', 'Database created by RQDA",
                              "(http://rqda.r-forge.r-project.org/)', '')"), date()))
            if (dbExistsTable(con, "cases")) dbRemoveTable(con, "cases")
            rqda_exe(
                paste("create table cases  (name text, memo text, owner text, ",
                      "date text, dateM text, id integer, status integer)")
            )
            if (dbExistsTable(con, "caselinkage")) dbRemoveTable(con, "caselinkage")
            rqda_exe(
                paste("create table caselinkage  (caseid integer, fid integer, ",
                      "selfirst real, selend real, status integer, owner text, ",
                      "date text, memo text)")
            )

            if (dbExistsTable(con, "attributes")) dbRemoveTable(con, "attributes")
            rqda_exe(
                paste("create table attributes (name text, status integer, date text, ",
                      "dateM text, owner text, memo text)")
            )
            if (dbExistsTable(con, "caseAttr")) dbRemoveTable(con, "caseAttr")
            rqda_exe(
                paste("create table caseAttr (variable text, value text, ",
                      "caseID integer, date text, dateM text, owner text)")
            )
            if (dbExistsTable(con, "fileAttr")) dbRemoveTable(con, "fileAttr")
            rqda_exe(
                paste("create table fileAttr (variable text, value text, ",
                      "fileID integer, date text, dateM text, owner text)")
            )
            if (dbExistsTable(con, "journal")) dbRemoveTable(con, "journal")
            rqda_exe(
                paste("create table journal (name text, journal text, date text, ",
                      "dateM text, owner text, status integer)")
            )
            rqda_exe("alter table project add column imageDir text")
            try(rqda_exe("alter table attributes add column class text"), TRUE)
            rqda_exe("alter table caseAttr add column status integer")
            rqda_exe("alter table fileAttr add column status integer")
            try(rqda_exe(
                paste("create table annotation (fid integer, position integer, ",
                      "annotation text, owner text, date text, dateM text, ",
                      "status integer)")), TRUE)
            if (dbExistsTable(con, "image")) dbRemoveTable(con, "image")
            rqda_exe(
                paste("create table image (name text, id integer, date text, ",
                      "dateM text, owner text, status integer)"))
            if (dbExistsTable(con, "imageCoding")) dbRemoveTable(con, "imageCoding")
            rqda_exe(
                paste("create table imageCoding (cid integer, iid integer, ",
                      "x1 integer, y1 integer, x2 integer, y2 integer, ",
                      "memo text, date text, dateM text, owner text, status integer)"))
        }
    }
}

UpgradeTables <- function() {
    Fields <- dbListFields(.rqda$qdacon, "project")
    if (!"databaseversion" %in% Fields) {
        rqda_exe("alter table project add column databaseversion text")
        rqda_exe("update project set databaseversion='0.1.5'")
    }
    currentVersion <- rqda_sel("select databaseversion from project")[[1]]
    if (currentVersion == "0.1.5") {
        ##from = "0.1.5"
        rqda_exe(
            paste("create table caseAttr (variable text, value text, caseID integer, ",
                  "date text, dateM text, owner text)"))
        ## caseAttr table
        rqda_exe(
            paste("create table fileAttr (variable text, value text, fileID integer, ",
                  "date text, dateM text, owner text)"))
        ## fileAttr table
        rqda_exe(
            paste("create table attributes (name text, status integer, date text, ",
                  "dateM text, owner text, memo text)"))
        ## attributes table
        rqda_exe(
            paste("create table journal (name text, journal text, date text, ",
                  "dateM text, owner text, status integer)"))
        ## journal table
        rqda_exe("alter table project add column about text")
        rqda_exe(
            paste("update project set about='Database created by RQDA",
                  "(http://rqda.r-forge.r-project.org/)'"))
        rqda_exe("update project set databaseversion='0.1.9'")
        ## reset the version.
        ## added for version 0.1.8
        ## (no version 0.1.7 to make the version number consistent with RQDA
        ## version)
        rqda_exe("alter table project add column imageDir text")
        try(rqda_exe("alter table attributes add column class text"), TRUE)
        rqda_exe("alter table caseAttr add column status integer")
        rqda_exe("alter table fileAttr add column status integer")
        rqda_exe("alter table freecode add column color text")
        rqda_exe("update caseAttr set status = 1")
        rqda_exe("update fileAttr set status = 1")
        try(rqda_exe(
            paste("create table annotation (fid integer, position integer, ",
                  "annotation text, owner text, date text, dateM text, ",
                  "status integer)")), TRUE)
        rqda_exe(
            paste("create table image (name text, id integer, date text, ",
                  "dateM text, owner text, status integer)"))
        rqda_exe(
            paste("create table imageCoding (cid integer, iid integer, x1 integer, ",
                  "y1 integer, x2 integer, y2 integer, memo text, date text, ",
                  "dateM text, owner text, status integer)"))
    }
    if (currentVersion == "0.1.6") {
        rqda_exe("alter table project add column about text")
        rqda_exe(
            paste("update project set about='Database created by RQDA",
                  "(http://rqda.r-forge.r-project.org/)'"))
        rqda_exe("update project set databaseversion='0.1.9'")
        rqda_exe("alter table project add column imageDir text")
        try(rqda_exe("alter table attributes add column class text"), TRUE)
        rqda_exe("alter table caseAttr add column status integer")
        rqda_exe("update caseAttr set status = 1")
        rqda_exe("alter table fileAttr add column status integer")
        rqda_exe("alter table freecode add column color text")
        rqda_exe("update fileAttr set status = 1")
        try(rqda_exe(
            paste("create table annotation (fid integer, position integer, ",
                  "annotation text, owner text, date text, dateM text, ",
                  "status integer)")), TRUE)
        rqda_exe(
            paste("create table image (name text, id integer, date text, ",
                  "dateM text, owner text, status integer)"))
        rqda_exe(
            paste("create table imageCoding (cid integer, iid integer, x1 integer, ",
                  "y1 integer, x2 integer, y2 integer, memo text, date text, ",
                  "dateM text, owner text, status integer)"))
    }
    if (currentVersion == "0.1.8") {
        rqda_exe("update project set databaseversion='0.1.9'")
        rqda_exe("alter table freecode add column color text")
    }
    if (currentVersion < "0.2.0") {
        if (dbExistsTable(.rqda$qdacon, "coding2"))
            dbRemoveTable(.rqda$qdacon, "coding2")

        rqda_exe(
            paste("create table coding2  (cid integer, fid integer, seltext text, ",
                  "selfirst real, selend real, status integer, owner text, ",
                  "date text, memo text)"))
        rqda_exe("update project set databaseversion='0.2.0'")
    }
    if (currentVersion < "0.2.2") {
        rqda_exe("alter table treecode add column owner text")
        rqda_exe("alter table treefile add column owner text")
        rqda_exe("update project set databaseversion='0.2.2'")
    }
}

open_proj <- function(path, conName = "qdacon", assignenv = .rqda, ...) {
    tryCatch({
        con <- get(conName, assignenv)
        pkg <- attr(attr(con, "class"), "package")
        Open <- getFunction("dbIsValid",
                            where = sprintf("package:%s", pkg))(con)
        if (open) dbDisconnect(con)
    },
    error = function(e) {
    })
    ## Fist close the con if it exist, then open a new con.
    if (file.access(path, 2) == 0) {
        Encoding(path) <- "unknown"
        assign(conName, dbConnect(drv = dbDriver("SQLite"), dbname = path),
               envir = assignenv)
    } else if (file.access(path, 4) == 0) {
        Encoding(path) <- "unknown"
        assign(conName, dbConnect(drv = dbDriver("SQLite"), dbname = path),
               envir = assignenv)
        gmessage(
            gettext(paste("You don't have write access to the *.rqda file.",
                          "You can only read the project."), domain = "R-RQDA"),
            container = TRUE, icon = "warning")
    } else {
        gmessage(
            gettext("You don't have read access to the *.rqda file. Fail to open.",
                    domain = "R-RQDA"), container = TRUE, icon = "error")
    }
}



closeProject <- function(conName = "qdacon", assignenv = .rqda, ...) {
    tryCatch({
        con <- get(conName, assignenv)
        if (is_projOpen(message = FALSE)) {
            tryCatch(dispose(.rqda$.sfp), error = function(e) {
            })
            tryCatch(dispose(.rqda$.root_edit), error = function(e) {
            })
            WidgetList <- ls(envir = .rqda, pattern = "^[.]codingsOf", all.names = TRUE)
            for (i in WidgetList)
                tryCatch(dispose(get(i, envir = .rqda)), error = function(e) {
                })
            closeProjBF() ## update all widgets
            if (!dbDisconnect(con)) {
                gmessage(gettext("Closing project failed.", domain = "R-RQDA"),
                         icon = "waring", container = TRUE)
            }
        }
    }, error = function(e) {
    })
}



is_projOpen <- function(envir = .rqda, conName = "qdacon", message = TRUE) {
    ## test if any project is open.
    open <- FALSE
    tryCatch({
        con <- get(conName, envir)
        pkg <- attr(attr(con, "class"), "package")
        Open2 <- getFunction("dbIsValid", where = sprintf("package:%s", pkg))(con)
        open <- open + Open2
    }, error = function(e) {
    })
    if (!open & message) gmessage(gettext("No Project is Open.",
                                          domain = "R-RQDA"), icon = "warning",
                                  container = TRUE)
    return(open)
}

backup_proj <- function(con) {
    ## con = .rqda$qdacon
    dbname <- con@dbname
    Encoding(dbname) <- "UTF-8"
    backupname <- sprintf("%s%s.rqda", gsub("rqda$", "", dbname),
                          format(Sys.time(), "%H%M%S%d%m%Y"))
    success <- file.copy(from = dbname, to = backupname, overwrite = FALSE)
    if (success) {
        gmessage(gettext("Succeeded!", domain = "R-RQDA"),
                 container = TRUE, icon = "info")
    } else {
        gmessage(gettext("Fail to back up the project.", domain = "R-RQDA"),
                 container = TRUE, icon = "error")
    }
}

                                        # ToDo: use MemoWidget for this as well? Why are
                                        # there two MemoWidgets after all
ProjectMemoWidget <- function() {
    if (is_projOpen(envir = .rqda, "qdacon")) {
        ## use enviroment, so you can refer to the same object easily, this is
        ## the beauty of environment
        ## if project is open, then continue
        tryCatch(dispose(.rqda$.projmemo), error = function(e) {
        })
        ## Close the open project memo first, then open a new one
        ## .projmemo is the container of .projmemocontent, widget for the content
        ## of memo

                                        # get size of root gui as width and height
        wdh <- size(.rqda$.root_rqdagui)
        head_s <- c(wdh["width"], wdh["height"] * .1)
        body_s <- c(wdh["width"], wdh["height"] * .9)

        gw <- gwindow(title = "Project Memo",
                      width = getOption("widgetSize")[1],
                      height = getOption("widgetSize")[2])

        addHandlerKeystroke(gw, function(h, ...) {
            if (h$key == "\027") dispose(gw)
        })
        mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
        gw$set_icon(mainIcon)
        assign(".projmemo", gw, envir = .rqda)
        .projmemo <- get(".projmemo", .rqda)
        .projmemo2 <- gpanedgroup(horizontal = FALSE, container = .projmemo)
        ## use .projmemo2, so can add a save button to it.
        proj_memoB <- gbutton(
            gettext("Save memo", domain = "R-RQDA"),
            container = .projmemo2, handler = function(h, ...) {
                ## send the new content of memo back to database
                newcontent <- svalue(W)
                ## Encoding(newcontent) <- "UTF-8"
                ## take care of double quote.
                newcontent <- enc(newcontent, encoding = "UTF-8")

                ## only one row is needed
                rqda_exe(
                    sprintf("update project set memo='%s' where rowid = 1",
                            newcontent)
                    ## have to quote the character in the sql expression
                )
                mbut <- get("proj_memoB", envir = button)
                enabled(mbut) <- FALSE ## grey out the  button
            }
        )## end of save memo button
        size(proj_memoB) <- head_s
        assign("proj_memoB", proj_memoB, envir = button)
        tmp <- gtext(container = .projmemo2, font.attr = list(size = "large"))
        size(tmp) <- body_s
        gSignalConnect(tmp$buffer, "changed",
                       function(h, ...) {
                           mbut <- get("proj_memoB", envir = button)
                           enabled(mbut) <- TRUE
                       })##
        font <- pangoFontDescriptionFromString(.rqda$font)
        gtkWidgetModifyFont(tmp$widget, font)
        assign(".projmemocontent", tmp, envir = .rqda)
        prvcontent <- rqda_sel("select memo from project")[1, 1]
        ## [1, 1]turn data.frame to 1-length character. Existing content of memo
        if (length(prvcontent) == 0) {
            rqda_exe("replace into project (memo) values('')")
            prvcontent <- ""
            ## if there is no record in project table, it fails to save memo,
            ## so insert sth into it
        }

        W <- .rqda$.projmemocontent
        Encoding(prvcontent) <- "UTF-8"

        insert(W, prvcontent, do.newline = FALSE, where = "beginning",
               font.attr = list(size = "large"))

        ## do.newline:do not add a \n (new line) at the beginning
        ## push the previous content to the widget.
        size(proj_memoB) <- head_s
        enabled(proj_memoB) <- FALSE
        addHandlerUnrealize(get(".projmemo", envir = .rqda), handler = function(h, ...) {
            withinWidget <- svalue(get(".projmemocontent", envir = .rqda))
            InRQDA <- rqda_sel("select memo from project where rowid = 1")[1, 1]
            if (isTRUE(all.equal(withinWidget, InRQDA))) {
                return(FALSE)
            } else {
                val <- gconfirm(
                    gettext(
                        "The memo has bee change. Close anyway?", domain = "R-RQDA"),
                    container = TRUE)
                return(!val)
            }
        }
        )
    }
}

close_AllCodings <- function() {
    obj <- ls(.rqda, all.names = TRUE, pattern = "^.codingsOf")
    if (length(obj) != 0) {
        for (i in obj) {
            tryCatch(dispose(get(i, envir = .rqda)), error = function(e) {

            })
        }
    }
}
