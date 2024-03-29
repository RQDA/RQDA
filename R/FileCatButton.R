#################
AddFileCatButton <- function(label = gettext("Add", domain = "R-RQDA")) {
    AddFilCatB <- gbutton(label, handler = function(h, ...) {
        item <- ginput(gettext("Enter new File Category. ", domain = "R-RQDA"), icon = "info")
        if (!is.na(item)) {
            Encoding(item) <- "UTF-8"
            AddTodbTable(item, "filecat", Id = "catid") ## FILE CATegory
            UpdateTableWidget(Widget = .rqda$.FileCatWidget, FromdbTable = "filecat")
        }
    }
    )
    assign("AddFilCatB", AddFilCatB, button)
    enabled(AddFilCatB) <- FALSE
    AddFilCatB
}


DeleteFileCatButton <- function(label = gettext("Delete", domain = "R-RQDA")) {
    DelFilCatB <- gbutton(label, handler = function(h, ...) {
        del <- gconfirm(gettext("Really delete the File Category?", domain = "R-RQDA"), icon = "question")
        if (isTRUE(del)) {
            Selected <- svalue(.rqda$.FileCatWidget)
            Encoding(Selected) <- "UTF-8"
            catid <- rqda_sel(sprintf("select catid from filecat where status = 1 and name='%s'", enc(Selected)))[, 1]
            if (length(catid) == 1) {
                rqda_exe(sprintf("update filecat set status = 0 where name='%s'", enc(Selected)))
                ## set status in table freecode to 0
                UpdateTableWidget(Widget = .rqda$.FileCatWidget, FromdbTable = "filecat")
                tryCatch(rqda_exe(sprintf("update treefile set status = 0 where catid='%s'", catid)), error = function(e) {
                })
                ## should delete all the related codelists
                UpdateFileofCatWidget() ## update files of file cat widget
            } else gmessage(gettext("The Category Name is not unique.", domain = "R-RQDA"), con = TRUE)
        }
    }
    )
    assign("DelFilCatB", DelFilCatB, button)
    enabled(DelFilCatB) <- FALSE
    DelFilCatB
}


FileCat_RenameButton <- function(label = gettext("Rename", domain = "R-RQDA"), Widget = .rqda$.FileCatWidget, ...) {
    ## rename of selected file cat.
    FilCatRenB <- gbutton(label, handler = function(h, ...) {
        OldName <- svalue(Widget)
        ## get the new file names
        NewName <- ginput(gettext("Enter new Category name. ", domain = "R-RQDA"), text = OldName, icon = "info")

        if (!identical(NewName, character(0))) {
            if (!is.na(NewName)) {
                Encoding(NewName) <- "UTF-8"
                rename(OldName, NewName, "filecat")
                UpdateTableWidget(Widget = .rqda$.FileCatWidget, FromdbTable = "filecat")
            }
        }
    }
    )
    assign("FilCatRenB", FilCatRenB, button)
    enabled(FilCatRenB) <- FALSE
    FilCatRenB
}

UpdateFileofCatWidget <- function(con = .rqda$qdacon, Widget = .rqda$.FileofCat, sortByTime = FALSE, ...) {
    SelectedFileCat <- svalue(.rqda$.FileCatWidget)
    if (length(SelectedFileCat) != 0) {
        Encoding(SelectedFileCat) <- "UTF-8"
        catid <- rqda_sel(sprintf("select catid from filecat where status = 1 and name='%s'", enc(SelectedFileCat)))[, 1]
        Total_fid <- rqda_sel(sprintf("select fid from treefile where status = 1 and catid=%i", catid))
        if (nrow(Total_fid) != 0) {
            items <- rqda_sel("select name, id, date from source where status = 1")
            if (nrow(items) != 0) {
                items <- items[items$id %in% Total_fid$fid, c("name", "date")]
                items <- items$name[OrderByTime(items$date)] ## sort by date
                Encoding(items) <- "UTF-8"
                if (!sortByTime) items <- sort(items)
            } else items <- NULL
        } else items <- NULL
    } else items <- NULL
    tryCatch(Widget[] <- items, error = function(e) {
    })
}

UpdateFileofCatWidget2 <- function(con = .rqda$qdacon, Widget = .rqda$.FileofCat, sortByTime = FALSE, ...) {
    Total_fid <- getFileIdsSets("filecategory", "intersect")
    if (length(Total_fid) != 0) {
        items <- rqda_sel("select name, id, date from source where status = 1")
        if (nrow(items) != 0) {
            items <- items[items$id %in% Total_fid, c("name", "date")]
            items <- items$name[OrderByTime(items$date)] ## sort by date
            Encoding(items) <- "UTF-8"
            if (!sortByTime) items <- sort(items)
        } else items <- NULL
    } else items <- NULL
    tryCatch(Widget[] <- items, error = function(e) {
    })
}



FileCatMemoButton <- function(label = gettext("Memo", domain = "R-RQDA")) {
    ans <- gbutton(label, handler = function(h, ...) {
        MemoWidget(gettext("File Category", domain = "R-RQDA"), .rqda$.FileCatWidget, "filecat")
    }
    )
    gtkWidgetSetTooltipText(getToolkitWidget(ans), gettext("Memo of file category.", domain = "R-RQDA"))
    assign("FilCatMemB", ans, button)
    enabled(ans) <- FALSE
    ans
}


FileCatAddToButton <- function(label = gettext("AddTo", domain = "R-RQDA"), Widget = .rqda$.FileCatWidget, ...) {
    ans <- gbutton(label, handler = function(h, ...) {
        SelectedFileCat <- svalue(.rqda$.FileCatWidget)
        catid <- rqda_sel(sprintf("select catid from filecat where status = 1 and name='%s'", enc(SelectedFileCat)))[, 1]
        freefile <- rqda_sel("select name, id from source where status = 1")
        if (nrow(freefile) == 0) {
            gmessage(gettext("No files Yet.", domain = "R-RQDA"), cont = .rqda$.FileCatWidget)
        } else {
            Encoding(SelectedFileCat) <- Encoding(freefile[["name"]]) <- "UTF-8"
            fileofcat <- rqda_sel(sprintf("select fid from treefile where status = 1 and catid=%i", catid))
            if (nrow(fileofcat) != 0) {
                fileoutofcat <- subset(freefile, !(freefile$id %in% fileofcat$fid))
            } else  fileoutofcat <- freefile
            Selected <- gselect.list(fileoutofcat[["name"]], multiple = TRUE, x = getOption("widgetCoordinate")[1])
            if (Selected != "") {
                ## Selected <- iconv(Selected, to = "UTF-8") ## already Encoded as UTF-8.
                fid <- fileoutofcat[fileoutofcat$name %in% Selected, "id"]
                Dat <- data.frame(fid = fid, catid = catid, date = date(), dateM = date(), memo = NA, status = 1, owner = .rqda$owner)
                rqda_wrt("treefile", Dat)
                UpdateFileofCatWidget()
            }
        }
    }
    )
    gtkWidgetSetTooltipText(getToolkitWidget(ans), gettext("Add file(s) to the selected file category.", domain = "R-RQDA"))
    assign("FilCatAddToB", ans, button)
    enabled(ans) <- FALSE
    return(ans)
}

FileCatDropFromButton <- function(label = gettext("DropFrom", domain = "R-RQDA"), Widget = .rqda$.FileofCat, ...) {
    ans <- gbutton(label, handler = function(h, ...) {
        FileOfCat <- svalue(Widget)
        ## Give a confirm msg
        del <- gconfirm(sprintf(gettext("Delete %i file(s) from this category. Are you sure?", domain = "R-RQDA"), length(FileOfCat)), con = TRUE, icon = "question")
        if (isTRUE(del)) {
            SelectedFileCat <- svalue(.rqda$.FileCatWidget)
            Encoding(SelectedFileCat) <- Encoding(FileOfCat) <- "UTF-8"
            catid <- rqda_sel(sprintf("select catid from filecat where status = 1 and name='%s'", enc(SelectedFileCat)))[, 1]
            for (i in FileOfCat) {
                fid <- rqda_sel(sprintf("select id from source where status = 1 and name='%s'", enc(i)))[, 1]
                rqda_exe(sprintf("update treefile set status = 0 where catid=%i and fid=%i", catid, fid))
            }
            ## update .CodeofCat Widget
            ## .rqda$.FileofCat[] <- setdiff(.rqda$.FileofCat[], FileOfCat)
            UpdateWidget(".FileofCat", from = FileOfCat, to = NULL)
            ## UpdateFileofCatWidget()
        }
    }
    )
    gtkWidgetSetTooltipText(getToolkitWidget(ans), gettext("Drop selected file(s) from file category.", domain = "R-RQDA"))
    assign("FilCatDroFromB", ans, button)
    enabled(ans) <- FALSE
    return(ans)
}

GetFileCatWidgetMenu <- function() {
    FileCatWidgetMenu <- list()

    FileCatWidgetMenu[[1]] <- gaction(gettext("Memo", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon")) {
            MemoWidget(gettext("File Category", domain = "R-RQDA"), .rqda$.FileCatWidget, "filecat")
            ## see CodeCatButton.R  for definition of MemoWidget
        }
    })

    FileCatWidgetMenu[[2]] <- gaction(gettext("Delete all files of selected category", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon")) {
            fid <- getFileIds("file")
            if (length(fid) > 0) {
                rqda_exe(sprintf("update source set status = 0 where id in (%s)", paste(shQuote(fid), collapse = ", ")))
                rqda_exe(sprintf("update coding set status = 0 where fid in (%s)", paste(shQuote(fid), collapse = ", ")))
                rqda_exe(sprintf("update caselinkage set status = 0 where fid in (%s)", paste(shQuote(fid), collapse = ", ")))
                rqda_exe(sprintf("update treefile set status = 0 where fid in (%s)", paste(shQuote(fid), collapse = ", ")))
                UpdateFileofCatWidget()
            }
        }
    })

    FileCatWidgetMenu[[2]] <- gaction(gettext("Sort by created time", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon")) {
            UpdateTableWidget(Widget = .rqda$.FileCatWidget, FromdbTable = "filecat")
        }
    })

    FileCatWidgetMenu
}


## popup menu for files of this category
GetFileofCatWidgetMenu <- function() {
    FileofCatWidgetMenu <- list()

    FileofCatWidgetMenu[[1]] <- gaction(gettext("Add To Case ...", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
            AddFileToCaselinkage(Widget = .rqda$.FileofCat)
            UpdateFileofCaseWidget()
        }
    })

    FileofCatWidgetMenu[[2]] <- gaction(gettext("Add To File Category ...", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
            AddToFileCategory(Widget = .rqda$.FileofCat, updateWidget = FALSE)
        }
    })

    FileofCatWidgetMenu[[3]] <- gaction(gettext("Move To File Category ...", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
            fcatname <- svalue(.rqda$.FileCatWidget) ## should select one only
            fcatid <- rqda_sel(sprintf("select catid from filecat where name='%s'",
                                       enc(fcatname)))$catid
            fid <- getFileIds("file", "select")
            ans <- AddToFileCategory(Widget = .rqda$.FileofCat, updateWidget = FALSE)
            if (isTRUE(ans)) {
                rqda_exe(sprintf("update treefile set status = 0 where fid in (%s) and catid='%s'",
                                 paste(shQuote(fid), collapse = ", "),
                                 fcatid))
                .rqda$.FileofCat[] <- setdiff(.rqda$.FileofCat[], svalue(.rqda$.FileofCat))
            }}
    })

    FileofCatWidgetMenu[[4]] <- gaction(gettext("File Memo", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon")) {
            MemoWidget(gettext("File", domain = "R-RQDA"), .rqda$.FileofCat, "source")
        }
    })

    FileofCatWidgetMenu[[5]] <- gaction(gettext("Open Selected File", domain = "R-RQDA"), handler = function(h, ...) {
        ViewFileFun(FileNameWidget = .rqda$.FileofCat)
    })

    FileofCatWidgetMenu[[6]] <- gaction(gettext("Edit Selected File", domain = "R-RQDA"), handler = function(h, ...) {
        EditFileFun(FileNameWidget = .rqda$.FileofCat)
    })

    FileofCatWidgetMenu[[7]] <- gaction(gettext("Search Files Within Category", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon")) {
            fid <- getFileIds(condition = "filecategory", type = "all")
            pattern <- ifelse(is.null(.rqda$lastsearch), "file like '%%'", .rqda$lastsearch)
            pattern <- ginput(gettext("Please input a search pattern.", domain = "R-RQDA"), text = pattern)
            Encoding(pattern) <- "UTF-8"
            if (!is.na(pattern) && length(fid) != 0) {
                tryCatch(searchFiles(sprintf("(%s) and id in (%s)", pattern, paste(shQuote(fid), collapse = ", ")),
                                     Widget = ".FileofCat", is.UTF8 = TRUE),
                         error = function(e) gmessage(gettext("Error~~~.", domain = "R-RQDA")), con = TRUE)
                assign("lastsearch", pattern, envir = .rqda)
            }
        }
    })

    FileofCatWidgetMenu[[8]] <- gaction(gettext("Delete selected File(s)", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon")) {
            SelectedFile <- svalue(.rqda$.FileofCat)
            Encoding(SelectedFile) <- "UTF-8"
            for (i in SelectedFile) {
                i <- enc(i)
                fid <- rqda_sel(sprintf("select id from source where name='%s'", i))$id
                rqda_exe(sprintf("update source set status = 0 where name='%s'", i))
                rqda_exe(sprintf("update caselinkage set status = 0 where fid=%i", fid))
                rqda_exe(sprintf("update treefile set status = 0 where fid=%i", fid))
                rqda_exe(sprintf("update coding set status = 0 where fid=%i", fid))
            }
            ## UpdateFileofCatWidget()
            ## .rqda$.FileofCat[] <- setdiff(.rqda$.FileofCat[], SelectedFile)
            UpdateWidget(".FileofCat", from = SelectedFile, to = NULL)
        }
    })

    FileofCatWidgetMenu[[9]] <- gaction(gettext("Rename selected File", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon")) {
            selectedFN <- svalue(.rqda$.FileofCat)
            if (length(selectedFN) == 0) {
                gmessage(gettext("Select a file first.", domain = "R-RQDA"), icon = "error", con = TRUE)
            }
            else {
                NewFileName <- ginput(gettext("Enter new file name. ", domain = "R-RQDA"), text = selectedFN, icon = "info")

                if (!identical(NewFileName, character(0))) {
                    if (!is.na(NewFileName)) {
                        Encoding(NewFileName) <- "UTF-8"
                        rename(selectedFN, NewFileName, "source")
                        ## UpdateFileofCatWidget()
                        Fnames <- .rqda$.FileofCat[]
                        Fnames[Fnames == selectedFN] <- NewFileName
                        .rqda$.FileofCat[] <- Fnames
                    }
                }
            }}
    })

    show_lst <- vector("list", 3)

    show_lst[[1]] <- gaction(gettext("Show All By Imported Time", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon")) {
            fid <- getFileIds(condition = "filecategory", type = "all")
            FileNameWidgetUpdate(FileNamesWidget = .rqda$.FileofCat, FileId = fid)
        }
    })

    show_lst[[2]] <- gaction(gettext("Show Coded Files Sorted by Imported time", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon")) {
            FileNameWidgetUpdate(FileNamesWidget = .rqda$.FileofCat, FileId = getFileIds(condition = "filecat", type = "coded"))
        }
    })

    show_lst[[3]] <- gaction(gettext("Show Uncoded Files Sorted by Imported time", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
            ## UncodedFileNamesUpdate(FileNamesWidget = .rqda$.fnames_rqda)
            FileNameWidgetUpdate(FileNamesWidget = .rqda$.FileofCat, FileId = getFileIds(condition = "filecat", type = "uncoded"))
            ## By default, the file names in the widget will be sorted.
        }
    })

    FileofCatWidgetMenu[[gettext("Show ...", domain = "R-RQDA")]] <- show_lst

    FileofCatWidgetMenu[[9]] <- gaction(gettext("Show Selected File Property", domain = "R-RQDA"), handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
            ShowFileProperty(Fid = getFileIds("filecat", "selected"))
        }
    })

    FileofCatWidgetMenu
}
