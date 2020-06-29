ImportFileButton <- function(label = rqda_txt("Import"), container, ...) {
  ImpFilB <- gbutton(label, container = container, handler = function(h, ...) {
    paths  <- gfile(type = "open",
                    filter = list("text files" =
                                    list(mime.types = c("text/plain")),
                                  "All files" = list(patterns = c("*"))),
                    multi = TRUE)
    if (!identical(paths, character(0)))
    {
      if (all(paths != "")) {
        ## have to convert, otherwise, can not find the file.
        Encoding(paths) <- "UTF-8"
        ImportFile(paths, container = .rqda$qdacon)
      }
    }
  })
  assign("ImpFilB", ImpFilB, envir = button)
  gtkWidgetSetSensitive(button$ImpFilB$widget, FALSE)
}

NewFileButton <- function(label = rqda_txt("New"), container, ...) {
  NewFilB <- gbutton(label, container = container, handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      AddNewFileFun()
    }
  })
  assign("NewFilB", NewFilB, envir = button)
  enabled(NewFilB) <- FALSE
}

DeleteFileButton <- function(label = rqda_txt("Delete"), container, ...) {
  DelFilB <- gbutton(label, container = container, handler = function(h, ...) {
    SelectedFile <- svalue(.rqda$.fnames_rqda)
    Encoding(SelectedFile) <- "UTF-8"
    ## if the project open and a file is selected, then continue the action
    del <- gconfirm(ngettext(length(SelectedFile),
                             "Really delete the file?",
                             "Really delete the files?"), icon = "question")
    if (isTRUE(del)) {
      ## con <- .rqda$qdacon
      for (i in SelectedFile) {
        i <- enc(i)
        fid <- rqda_sel(
          sprintf("select id from source where name = '%s'",
                  i))$id
        rqda_exe(
          sprintf("update source set status = 0 where name = '%s'",
                  i))
        ## set the status of the selected file to 0
        rqda_exe(
          sprintf("update caselinkage set status = 0 where fid = %i",
                  fid))
        rqda_exe(
          sprintf("update treefile set status = 0 where fid = %i",
                  fid))
        rqda_exe(
          sprintf("update coding set status = 0 where fid = %i",
                  fid))
        ## set the status of the related case/f-cat to 0
      }
      UpdateWidget(".fnames_rqda", from = SelectedFile, to = NULL)
      # FileNamesUpdate()
    }
  }, action = list(envir = .rqda, conName = "qdacon")
  )
  assign("DelFilB", DelFilB, envir = button)
  gtkWidgetSetSensitive(button$DelFilB$widget, FALSE)
}

ViewFileButton <-  function(label = rqda_txt("Open"), container, ...) {
  VieFilB <- gbutton(label, container = container,
                     handler = function(h, ...) {
                       ViewFileFun(FileNameWidget = .rqda$.fnames_rqda)
                     })
  assign("VieFilB", VieFilB, envir = button)
  gtkWidgetSetSensitive(button$VieFilB$widget, FALSE)
}

File_MemoButton <- function(label = rqda_txt("Memo"),
                            container = .rqda$.files_button,
                            FileWidget = .rqda$.fnames_rqda, ...) {
  ## memo of selected file.
  FilMemB <- gbutton(label, container = container, handler = function(h, ...) {
    MemoWidget(rqda_txt("File"), FileWidget, "source")
  })
  assign("FilMemB", FilMemB, envir = button)
  gtkWidgetSetSensitive(button$FilMemB$widget, FALSE)
}

File_RenameButton <- function(label = rqda_txt("Rename"),
                              container = .rqda$.files_button,
                              FileWidget = .rqda$.fnames_rqda, ...) {
  ## rename of selected file.
  FilRenB <- gbutton(label, container = container, handler = function(h, ...) {
    selectedFN <- svalue(FileWidget)

    if (length(selectedFN) == 0) {
      gmessage(rqda_txt("Select a file first."), icon = "error",
               container = TRUE)
    } else {
      ## get the new file names
      NewFileName <- ginput(rqda_txt("Enter new file name. "),
                            text = selectedFN, icon = "info")

      if (!identical(NewFileName, character(0)))
      {
        if (!is.na(NewFileName)) {
          Encoding(NewFileName) <- "UTF-8"
          ## otherwise, R transform it into local Encoding rather
          ## than keep it as UTF-8
          ## Newfilename <- iconv(codename, from = "UTF-8") ## now use UTF-8
          ## for SQLite data set.
          ## update the name in source table by a function
          rename(selectedFN, NewFileName, "source")
          FileNamesUpdate()
          ## speed it up by bypassing access the database.
          UpdateWidget(".fnames_rqda", from = selectedFN, to = NewFileName)
          ## (name is the only field should be modified, as other table
          ##  use fid rather than name)

          FileNamesUpdate()
        }
      }
    }
  })

  FilRenB
  assign("FilRenB", FilRenB, envir = button)
  gtkWidgetSetSensitive(button$FilRenB$widget, FALSE)
}

FileAttribute_Button <- function(label = rqda_txt("Attribute"),
                                 container = .rqda$.files_button,
                                 FileWidget = .rqda$.fnames_rqda, ...) {
  FileAttrB <- gbutton(label, container = container, handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon")) {
      Selected <- svalue(FileWidget)
      if (length(Selected != 0 )) {
        fileId <- rqda_sel(
          sprintf("select id from source where status = 1 and name = '%s'",
                  enc(Selected)))[, 1]
        FileAttrFun(fileId = fileId, title = Selected)
      }
    }
  })

  FileAttrB
  assign("FileAttrB", FileAttrB, envir = button)
  enabled(FileAttrB) <- FALSE
}

AddNewFileFun <- function() {
  if (is_projOpen(envir = .rqda, "qdacon")) {

    if (exists(".AddNewFileWidget", envir = .rqda) &&
        isExtant(.rqda$.AddNewFileWidget)) {
      dispose(.rqda$.AddNewFileWidget)
    } ## close the widget if open

    # get size of root gui as width and height
    wdh <- size(.rqda$.root_rqdagui)
    head_s <- c( wdh["width"], wdh["height"] * .1)
    body_s <- c( wdh["width"], wdh["height"] * .9)

    gw <- gwindow(title = "Add a new file",
                  parent = getOption("widgetCoordinate"),
                  width = getOption("widgetSize")[1],
                  height = getOption("widgetSize")[2])

    addHandlerKeystroke(gw, function(h, ...){
      if(h$key=="\027") dispose(gw)
    })

    mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
    gw$set_icon(mainIcon)
    assign(".AddNewFileWidget", gw, envir = .rqda)
    assign(".AddNewFileWidget2",
           gpanedgroup(horizontal = FALSE,
                       container = get(".AddNewFileWidget", envir = .rqda)),
           envir = .rqda)

    saveFileFun <- function() {
      ## require a title for the file
      Ftitle <- ginput(rqda_txt("Enter the title"),
                       icon = "info")
      if (!is.na(Ftitle)) {
        Ftitle <- enc(Ftitle, "UTF-8")
        if (nrow(
          rqda_sel(
            sprintf("select name from source where name = '%s'",
                    Ftitle))) != 0) {
          Ftitle <- paste("New", Ftitle)
        } ## Make sure it is unique
        content <- svalue(textW)
        content <- enc(content, encoding = "UTF-8")
        ## the current one
        maxid <- rqda_sel( "select max(id) from source")[[1]]
        ## the new one/ for the new file
        nextid <- ifelse(is.na(maxid), 0+1, maxid+1)
        ans <- rqda_exe(
          sprintf(
            paste("insert into source (name, file, id, status, date, owner )",
                  "values ('%s', '%s', %i, %i, '%s', '%s')"),
            Ftitle, content, nextid, 1, date(), .rqda$owner))
        if (is.null(ans)) {
          svalue(textW) <- "" ## clear the content.
          FileNamesUpdate()
          enabled(button$AddNewFilB) <- FALSE
          enabled(button$AddNewFilB2) <- FALSE
        }
        return(TRUE)
      } else {
        return(FALSE)
      }
    } ## end of saveFileFun

    gl <- glayout(homogeneous = T,
                  container = get(".AddNewFileWidget2", envir = .rqda))
    size(gl) <- head_s

    # ToDo: Make a save button and a close button, like in any other application
    AddNewFilB <- gbutton(rqda_txt("Save To Project"),
                          handler = function(h, ...) {
                            saveFileFun()
                            FileNamesUpdate()
                          })
    enabled(AddNewFilB) <- FALSE
    assign("AddNewFilB", AddNewFilB, envir = button)

    gl[1, 1] <- AddNewFilB

    tmp <- gtext(container = get(".AddNewFileWidget2", envir = .rqda))
    size(tmp) <- body_s
    font <- pangoFontDescriptionFromString(.rqda$font)
    gtkWidgetModifyFont(tmp$widget, font) ## set the default fontsize
    assign(".AddNewFileWidgetW", tmp, envir = .rqda)
    textW <- get(".AddNewFileWidgetW", envir = .rqda)

    addHandlerKeystroke(.rqda$.AddNewFileWidgetW, handler = function(h, ...) {
      enabled(button$AddNewFilB) <- TRUE
      enabled(button$AddNewFilB2) <- TRUE
    })

    addHandlerUnrealize(.rqda$.AddNewFileWidgetW, handler = function(h, ...) {
      rm("AddNewFilB", envir = button)
      rm(".AddNewFileWidgetW", ".AddNewFileWidget",
         ".AddNewFileWidget2", envir = .rqda)
      FALSE
    })

    ## svalue(.rqda$.AddNewFileWidget2) <- 0.03999
  }
}


## pop-up menu of add to case and F-cat from Files Tab
## The translations must be created at run time, otherwise they will not work.
GetFileNamesWidgetMenu <- function()
{
  FileNamesWidgetMenu <- list()

  FileNamesWidgetMenu[[1]] <- gaction(
    rqda_txt("Add New File ..."), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        AddNewFileFun()
      }
    })

  FileNamesWidgetMenu[[2]] <- gaction(
    rqda_txt("Add To Case ..."), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        AddFileToCaselinkage()
        UpdateFileofCaseWidget()
      }
    })

  FileNamesWidgetMenu[[3]] <- gaction(
    rqda_txt("Add To File Category ..."), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        AddToFileCategory()
        UpdateFileofCatWidget()
      }
    })

  FileNamesWidgetMenu[[4]] <- gaction(
    rqda_txt("Add/modify Attributes of The Open File..."),
    handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        Selected <- tryCatch(svalue(.rqda$.root_edit),
                             error = function(e) {NULL})
        if (!is.null(Selected)) {
          fileId <- rqda_sel(
            sprintf("select id from source where status = 1 and name = '%s'",
                    enc(Selected)))[, 1]
          FileAttrFun(fileId = fileId, title = Selected)
        }
      }})

  FileNamesWidgetMenu[[5]] <- gaction(
    rqda_txt("View Attributes"), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        viewFileAttr()
      }
    })

  FileNamesWidgetMenu[[6]] <- gaction(
    rqda_txt("Codings of selected file(s)"), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        fid = getFileIds(type = "selected")
        if (length(fid)>0) {
          getCodingsFromFiles(Fid = fid)
        } else {
          gmessage(rqda_txt("No coded file is selected."))
        }
      }
    })

  FileNamesWidgetMenu[[7]] <- gaction(
    rqda_txt("Export File Attributes"), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        fName <- gfile(type = 'save',
                       filter = list("csv" = list(pattern = c("*.csv"))))
        Encoding(fName) <- "UTF-8"
        if (length(grep(".csv$", fName)) == 0)
          fName <- sprintf("%s.csv", fName)

        write.csv(getAttr("file"), row.names = FALSE, file = fName, na = "")
      }
    })

  FileNamesWidgetMenu[[8]] <- gaction(
    rqda_txt("Edit Selected File"), handler = function(h, ...) {
      EditFileFun()
    })

  FileNamesWidgetMenu[[9]] <- gaction(
    rqda_txt("Export Coded file as HTML"), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        path = gfile(
          type = "save",
          text = rqda_txt("Type a name for the exported codings and click OK."))
        if (!identical(path, character(0)))
        {
          if (!is.na(path)) {
            Encoding(path) <- "UTF-8"
            path <- sprintf("%s.html", path)
            exportCodedFile(file = path, getFileIds(type = "selected")[1])
          }
        }}})

  FileNamesWidgetMenu[[10]] <- gaction(
    rqda_txt("File Annotations"), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        print(getAnnos())
      }})

  FileNamesWidgetMenu[[11]] <- gaction(
    rqda_txt("File Memo"), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        MemoWidget(rqda_txt("File"), .rqda$.fnames_rqda, "source")
      }
    })

  FileNamesWidgetMenu[[12]] <- gaction(
    rqda_txt("Import PDF Highlights via rjpod (selector)"),
    handler = function(h, ...) {
      importPDFHL(engine = "rjpod")
    })

  FileNamesWidgetMenu[[13]] <- gaction(
    rqda_txt("Import PDF Highlights via rjpod (file path)"),
    handler = function(h, ...) {
      fpath = ginput(rqda_txt("Enter a pdf file path"), con = T)
      importPDFHL(file = fpath, engine = "rjpod")
    })

  FileNamesWidgetMenu[[14]] <- gaction(
    rqda_txt("Open Selected File"), handler = function(h, ...) {
      ViewFileFun(FileNameWidget = .rqda$.fnames_rqda)
    })

  FileNamesWidgetMenu[[15]] <- gaction(
    rqda_txt("Open Previous Coded File"), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        fname <- rqda_sel(
          paste("select name from source where id in ( select fid from",
                "coding where rowid in (select max(rowid) from coding",
                "where status = 1))"))$name
        if (length(fname) != 0)  fname <- enc(fname, "UTF-8")
        ViewFileFunHelper(FileName = fname)
      }})

  FileNamesWidgetMenu[[16]] <- gaction(
    rqda_txt("Search for a Word"), handler = function(h, ...) {
      if (exists(".openfile_gui", envir = .rqda) &&
          isExtant(.rqda$.openfile_gui)) {
        SearchButton(.rqda$.openfile_gui)
      }
    })

  FileNamesWidgetMenu[[17]] <- gaction(
    rqda_txt("Search all files ..."), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        pattern <- ifelse(is.null(.rqda$lastsearch),
                          "file like '%%'",
                          .rqda$lastsearch)
        pattern <- ginput(rqda_txt("Please input a search pattern."),
                          text = pattern)
        if (!is.na(pattern)) {
          tryCatch(
            searchFiles(pattern, Widget = ".fnames_rqda", is.UTF8 = TRUE),
            error = function(e) gmessage(rqda_txt("Error~~~.")),
            container = TRUE)

          Encoding(pattern) <- "UTF-8"
          assign("lastsearch", pattern, envir = .rqda)
        }
      }
    })

  # submenu
  show_lst <- vector("list", 9)

  show_lst[[1]] <- gaction(
    rqda_txt("Show All Sorted By Imported Time"),
    handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        FileNamesUpdate(FileNamesWidget = .rqda$.fnames_rqda)
        FileNameWidgetUpdate(FileNamesWidget = .rqda$.fnames_rqda,
                             FileId = getFileIds(condition = "unconditional",
                                                 type = "all"))
      }
    })

  show_lst[[2]] <- gaction(
    rqda_txt("Show Coded Files Sorted by Imported time"),
    handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        FileNameWidgetUpdate(FileNamesWidget = .rqda$.fnames_rqda,
                             FileId = getFileIds(condition = "unconditional",
                                                 type = "coded"))
      }
    })

  show_lst[[3]] <- gaction(
    rqda_txt("Show Uncoded Files Sorted by Imported time"),
    handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        FileNameWidgetUpdate(FileNamesWidget = .rqda$.fnames_rqda,
                             FileId = getFileIds(condition = "unconditional",
                                                 type = "uncoded"))
      }
    })

  show_lst[[4]] <- gaction(
    rqda_txt("Show Files With Annotation"),
    handler = function(h, ...) {
      fileid <-
        rqda_sel(
          "select fid from annotation where status = 1 group by fid")$fid
      if (length(fileid) != 0) {
        FileNameWidgetUpdate(FileNamesWidget = .rqda$.fnames_rqda,
                             FileId = fileid)
      } else gmessage(rqda_txt("No file with memo."),
                      container = TRUE)
    })

  show_lst[[5]] <- gaction(
    rqda_txt("Show Files Without Annotation"),
    handler = function(h, ...) {
      fileid <- rqda_sel(
        paste("select id from source where status = 1 and id not in",
              "(select fid from annotation where status = 1 group by fid)"))$id
      if (length(fileid) != 0) {
        FileNameWidgetUpdate(FileNamesWidget = .rqda$.fnames_rqda,
                             FileId = fileid)
      } else gmessage(rqda_txt("All files have annotation."),
                      container = TRUE)
    })

  show_lst[[6]] <- gaction(
    rqda_txt("Show Files With Memo"),
    handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        fileid <-
          rqda_sel(
            "select id from source where memo is not null")
        if (nrow(fileid) != 0) {
          fileid <- fileid[[1]]
          FileNameWidgetUpdate(FileNamesWidget = .rqda$.fnames_rqda,
                               FileId = fileid)
        } else gmessage(rqda_txt("No file with memo."),
                        container = TRUE)
      }
    })

  show_lst[[7]] <- gaction(
    rqda_txt("Show Files Without Memo"),
    handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        fileid <- rqda_sel(
          "select id from source where memo is null")
        if (nrow(fileid) != 0) {
          fileid <- fileid[[1]]
          FileNameWidgetUpdate(FileNamesWidget = .rqda$.fnames_rqda,
                               FileId = fileid)
        } else gmessage(rqda_txt("No file is found."),
                        container = TRUE)
      }
    })

  show_lst[[8]] <- gaction(
    rqda_txt("Show Files Without File Category"),
    handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        fileid <- rqda_sel(
          paste("select id from source where status = 1 and id not in",
                "(select fid from treefile where status = 1)"))
        if (nrow(fileid) != 0) {
          fileid <- fileid[[1]]
          FileNameWidgetUpdate(FileNamesWidget = .rqda$.fnames_rqda,
                               FileId = fileid)
        } else {
          gmessage(rqda_txt("All are linked with file category."),
                   container = TRUE)}
      }
    })

  show_lst[[9]] <- gaction(
    rqda_txt("Show Files With No Case"),
    handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        fileid <- rqda_sel(
          paste("select id from source where status = 1 and id not in",
                "(select fid from caselinkage where status = 1)"))
        if (nrow(fileid) != 0) {
          fileid <- fileid[[1]]
          FileNameWidgetUpdate(FileNamesWidget = .rqda$.fnames_rqda,
                               FileId = fileid)
        } else gmessage(rqda_txt("All are linked with cases."),
                        container = TRUE)
      }
    })

  # [[18]]
  FileNamesWidgetMenu[[rqda_txt("Show ...")]] <- show_lst

  FileNamesWidgetMenu[[19]] <- gaction(
    rqda_txt("Show Selected File Property"), handler = function(h, ...) {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        ShowFileProperty()
      }
    })

  FileNamesWidgetMenu
}
