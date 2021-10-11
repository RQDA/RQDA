#' @export
RQDA <- function() {


### GUI FOR ROOT #############################################################

    options("guiToolkit" = "RGtk2")
    ## avoid manual selection when more than one toolkit have been installed
    if (isTRUE(.rqda$isLaunched)) {
        message("RQDA has been launched.")
    } else {
        ".root_rqdagui" <- gwindow(
            title = rqda_txt("RQDA: Qualitative Data Analysis"),
            parent = c(2, 2),
            width = gdkScreenWidth() * .1,
            height = gdkScreenHeight() * .1,
            visible = FALSE,
            handler = function(h, ...) {
                closeProject(assignenv = .rqda)
            }
        )
        addHandlerKeystroke(.root_rqdagui, function(h, ...) {
            if (h$key == "\021") {
                dispose(.root_rqdagui)
                assign("isLaunched", FALSE, envir = .rqda)
            }
        })
        if (is.null(getOption("widgetSize")))
            options(widgetSize = c(gdkScreenWidth() * .5,
                                   gdkScreenHeight() * .5))


        wdh <- size(.root_rqdagui)
        head_s <- c(wdh["width"], wdh["height"] * .1)
        body_s <- c(wdh["width"], wdh["height"] * .9)

        mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
        .root_rqdagui$set_icon(mainIcon)
        ## set an icon for the main program.
        ".nb_rqdagui" <- gnotebook(2, # tab.pos
                                   container = .root_rqdagui,
                                   closebuttons = FALSE)



#### GUI FOR PROJECT #######################################################
        ".proj_gui" <- ggroup(
            container = .nb_rqdagui,
            horizontal = FALSE,
            label = rqda_txt("Project\n"))

        NewProjectButton(container = .proj_gui)
        OpenProjectButton(container = .proj_gui)
        CloseProjectButton(container = .proj_gui)

        Proj_MemoButton(label = rqda_txt("Project Memo"),
                        container = .proj_gui)

        ## project memo button
        BackupProjectButton(container = .proj_gui)
        saveAsButt(label = rqda_txt("Save Project As"),
                   container = .proj_gui)
        CleanProjButton(container = .proj_gui)
        CloseAllCodingsButton(container = .proj_gui)
        ##gbutton(rqda_txt("About"),
        ## container = .proj_gui, handler = function(h, ...) {
        ## browseURL("http://rqda.r-forge.r-project.org/")})

        gseparator(container = .proj_gui)
        glabel(rqda_txt("Path of current project:"),
               container = .proj_gui)

        ".currentProj" <- glabel(rqda_txt("No project is open."),
                                 container = .proj_gui)

        gseparator(container = .proj_gui)
        glabel(
            rqda_txt("Author: <ronggui.huang@gmail.com>"),
            container = .proj_gui)
        glabel(
            rqda_txt("Help: click to join rqda-help mailing list"),
            container = .proj_gui, handler = function(h, ...) {
                browseURL(paste0("https://lists.r-forge.r-project.org/cgi-bin/",
                                 "mailman/listinfo/rqda-help"))
            })

        gseparator(container = .proj_gui)
        glabel(
            rqda_txt("License: BSD"),
            container = .proj_gui, handler = function(h, ...) {
                gtext(
                    paste(readLines((system.file("License", package = "RQDA")),
                                    warn = FALSE), collapse = "\n"),
                    container = gwindow(title = "License"))
            })
        glabel(
            paste(rqda_txt("Version:"),
                  packageDescription("RQDA")$Version,
                  rqda_txt(" Year:"),
                  substr(packageDescription("RQDA")$Date, 1, 4)),
            container = .proj_gui, handler = function(h, ...) {
                gtext(format(citation("RQDA"), "textVersion"),
                      container = gwindow(
                          title = rqda_txt("Please cite this package.")))
            })
        glabel(rqda_txt("About"),
               container = .proj_gui, handler = function(h, ...) {
                   browseURL("http://rqda.r-forge.r-project.org/")
               })


#### GUI for FILES #########################################################

        ##".fnames_rqda"<- gtable("Click Here to see the File list.",
        ## container = .files_pan, multiple = TRUE)
        ##.fnames_rqda[] <- NULL # get around of the text argument.

        ".files_pan" <- gpanedgroup(container = .nb_rqdagui,
                                    horizontal = FALSE,
                                    label = rqda_txt("Files\n"))

        ".files_button" <- ggroup(container = .files_pan, horizontal = TRUE)
        ".fnames_rqda" <- gtable(character(0), container = .files_pan,
                                 multiple = TRUE)
        names(.fnames_rqda) <- rqda_txt("Files")

        size(.files_button) <- head_s
        size(.fnames_rqda)  <- body_s


        ImportFileButton(rqda_txt("Import"),
                         container = .files_button)
        NewFileButton(rqda_txt("New"),
                      container = .files_button)
        DeleteFileButton(rqda_txt("Delete"),
                         container = .files_button)
        ViewFileButton(rqda_txt("Open"),
                       container = .files_button)
        File_MemoButton(label = rqda_txt("Memo"),
                        container = .files_button,
                        FileWidget = .fnames_rqda)
        File_RenameButton(label = rqda_txt("Rename"),
                          container = .files_button,
                          FileWidget = .fnames_rqda)
        FileAttribute_Button(label = rqda_txt("Attribute"),
                             container = .files_button,
                             FileWidget = .fnames_rqda)


### GUI for CODES ##########################################################

        ".codes_pan" <- gpanedgroup(container = .nb_rqdagui,
                                    horizontal = FALSE,
                                    label = rqda_txt("Codes\n"))

        ".codes_button" <- glayout(container = .codes_pan)
        ".codes_rqda" <- gtable(character(0), container = .codes_pan)
        names(.codes_rqda) <- rqda_txt("Codes List")

        size(.codes_button) <- head_s
        size(.codes_rqda)   <- body_s

        .codes_button[1, 1] <- AddCodeButton()
        .codes_button[1, 2] <- DeleteCodeButton()
        .codes_button[1, 3] <- FreeCode_RenameButton(label = rqda_txt("Rename"),
                                                     CodeNamesWidget = .codes_rqda)
        .codes_button[1, 4] <- CodeMemoButton(label = rqda_txt("Memo"))
        .codes_button[2, 1] <- AnnotationButton(rqda_txt("Anno"))
        .codes_button[2, 2] <- RetrievalButton(rqda_txt("Coding"))
        .codes_button[2, 3] <- Unmark_Button(name = "UnMarB1")
        .codes_button[2, 4] <- Mark_Button(name = "MarCodB1")


### GUI  for code categories ###############################################

        ".codecat_pan" <- gpanedgroup(container = .nb_rqdagui,
                                      horizontal = FALSE,
                                      label = rqda_txt("Code\nCategories"))

        ".codecat_buttons" <- glayout(container = .codecat_pan)
        ".Ccat_PW" <- ggroup(container = .codecat_pan, horizontal = FALSE)

        size(.codecat_buttons) <- head_s
        size(.Ccat_PW)         <- body_s

        ".CodeCatWidget" <- gtable(character(0), container = .Ccat_PW,
                                   expand = TRUE, multiple = TRUE)
        names(.CodeCatWidget) <- rqda_txt("Code Category")

        ".CodeofCat" <- gtable(character(0), container = .Ccat_PW,
                               expand = TRUE, multiple = TRUE)
        names(.CodeofCat) <- rqda_txt("Codes of This Category")

        .codecat_buttons[1, 1] <- AddCodeCatButton(rqda_txt("Add"))
        .codecat_buttons[1, 2] <- DeleteCodeCatButton(rqda_txt("Delete"))
        .codecat_buttons[1, 3] <- CodeCat_RenameButton(rqda_txt("Rename"))
        .codecat_buttons[2, 1] <- CodeCatAddToButton(rqda_txt("Add To"))
        .codecat_buttons[2, 2] <- CodeCatDropFromButton(rqda_txt("Drop From"))
        .codecat_buttons[1, 4] <- CodeCatMemoButton()
        .codecat_buttons[2, 3] <- Unmark_Button(label = rqda_txt("UnMark"),
                                                codeListWidget = .rqda$.CodeofCat,
                                                name = "UnMarB2")
        .codecat_buttons[2, 4] <- Mark_Button(label = rqda_txt("Mark"),
                                              codeListWidget = ".CodeofCat",
                                              name = "MarCodB2")


### GUI  for cases #########################################################

        ".case_pan" <- gpanedgroup(container = .nb_rqdagui,
                                   horizontal = FALSE,
                                   label = rqda_txt("Cases\n"))

        ".case_buttons" <- glayout(container = .case_pan)
        ".case_PW" <- ggroup(container = .case_pan, horizontal = FALSE)

        size(.case_buttons) <- head_s
        size(.case_PW)      <- body_s

        ".CasesNamesWidget" <- gtable(character(0), container = .case_PW,
                                      expand = TRUE, multiple = TRUE)
        names(.CasesNamesWidget) <- rqda_txt("Cases")

        ".FileofCase" <- gtable(character(0), container = .case_PW,
                                expand = TRUE, multiple = TRUE)
        names(.FileofCase) <- rqda_txt("Files of This Case")

        .case_buttons[1, 1] <- AddCaseButton()
        .case_buttons[1, 2] <- DeleteCaseButton()
        .case_buttons[1, 3] <- Case_RenameButton()
        .case_buttons[1, 4] <- CaseUnMark_Button(rqda_txt("Unlink"))
        .case_buttons[1, 5] <- CaseMark_Button(rqda_txt(" Link "))
        .case_buttons[2, 1] <- CaseAttribute_Button(rqda_txt("Attribute"))
        .case_buttons[2, 2] <- prof_mat_Button(rqda_txt("Profile"))


### GUI for Attributes #####################################################

        ".attr_pan" <- gpanedgroup(container = .nb_rqdagui,
                                   horizontal = FALSE,
                                   label = rqda_txt("Attributes\n"))

        ".attr_buttons" <- glayout(container = .attr_pan)
        ".attr_PW" <- ggroup(container = .attr_pan, horizontal = FALSE)

        size(.attr_buttons) <- head_s
        size(.attr_PW)      <- body_s

        ".AttrNamesWidget" <- gtable(character(0), container = .attr_PW,
                                     expand = TRUE, multiple = TRUE)
        names(.AttrNamesWidget) <- rqda_txt("Attributes")

        .attr_buttons[1, 1] <- AddAttrButton()
        .attr_buttons[1, 2] <- DeleteAttrButton()
        .attr_buttons[1, 3] <- RenameAttrButton()
        .attr_buttons[1, 4] <- AttrMemoButton()
        .attr_buttons[1, 5] <- SetAttrClsButton()


### GUI  for File categories ###############################################

        ".filecat_pan" <- gpanedgroup(container = .nb_rqdagui,
                                      horizontal = FALSE,
                                      label = rqda_txt("File\nCategories"))

        ".filecat_buttons" <- glayout(container = .filecat_pan)
        ".Fcat_PW" <- ggroup(container = .filecat_pan, horizontal = FALSE)

        size(.filecat_buttons) <- head_s
        size(.Fcat_PW)         <- body_s


        ".FileCatWidget" <- gtable(character(0), container = .Fcat_PW,
                                   expand = TRUE, multiple = TRUE)
        names(.FileCatWidget) <- rqda_txt("File Category")

        ".FileofCat" <- gtable(character(0), container = .Fcat_PW,
                               expand = TRUE, multiple = TRUE)
        names(.FileofCat) <- rqda_txt("Files of This Category")

        .filecat_buttons[1, 1] <- AddFileCatButton(rqda_txt("Add"))
        .filecat_buttons[1, 2] <- DeleteFileCatButton(rqda_txt("Delete"))
        .filecat_buttons[1, 3] <- FileCat_RenameButton(rqda_txt("Rename"))
        .filecat_buttons[2, 3] <- FileCatMemoButton()
        .filecat_buttons[2, 1] <- FileCatAddToButton(rqda_txt("Add To"))
        .filecat_buttons[2, 2] <- FileCatDropFromButton(rqda_txt("Drop From"))


        ## ### GUI for Search ######################################################
        ##   ".fsearch_pan" <- gpanedgroup(container = .nb_rqdagui,
        ##    horizontal = FALSE, label = "F-Search")
        ##  ".fsearch_rqda" <- glabel(
        ##  rqda_txt("Use searchFiles function to search files.\n
        ##  See ?searchFiles for more."), container = .fsearch_pan)
        ##  ".fsearch_rqda" <- gtable(rqda_txt("Click Here to see the File list.",
        ##   domain = "R-RQDA"), container = .fsearch_pan, multiple = TRUE,
        ##    expand = TRUE)
        ##  .fsearch_rqda[] <- NULL # get around of the text argument.
        ##  names(.fsearch_rqda) <- "Files Search"


### GUI for Journal ########################################################

        ".journal_pan" <- gpanedgroup(container = .nb_rqdagui,
                                      horizontal = FALSE,
                                      label = rqda_txt("Journals\n"))

        ".journal_buttons" <- glayout(container = .journal_pan)
        ".journal_PW" <- ggroup(container = .journal_pan, horizontal = FALSE)

        size(.journal_buttons) <- head_s
        size(.journal_PW)      <- body_s

        ".JournalNamesWidget" <- gtable(character(0), container = .journal_PW,
                                        expand = TRUE, multiple = FALSE)
        names(.JournalNamesWidget) <- rqda_txt("Journals")

        .journal_buttons[1, 1] <- AddJournalButton()
        .journal_buttons[1, 2] <- DeleteJournalButton()
        .journal_buttons[1, 3] <- OpenJournalButton()
        .journal_buttons[1, 4] <- RenameJournalButton()


### GUI  for settings ######################################################

        ".settings_gui" <- ggroup(container = .nb_rqdagui, horizontal = FALSE,
                                  label = rqda_txt("Settings\n"))
        addSettingGUI(container = .settings_gui)

        ## Put them together
        if (is.null(getOption("widgetCoordinate"))) {
            options(widgetCoordinate = c(size(.root_rqdagui$widget)[1] + 12, 2))
        }

        visible(.root_rqdagui) <- TRUE
        svalue(.nb_rqdagui) <- 1 ## make sure the project tab gain the focus.

##########################
        ## add documentation here
        assign(".root_rqdagui", .root_rqdagui, envir = .rqda)
        assign(".files_button", .files_button, envir = .rqda)
        assign(".codes_rqda", .codes_rqda, envir = .rqda)
        assign(".fnames_rqda", .fnames_rqda, envir = .rqda)
        ##assign(".fsearch_rqda", .fsearch_rqda, envir = .rqda)
        assign(".CasesNamesWidget", .CasesNamesWidget, envir = .rqda)
        assign(".AttrNamesWidget", .AttrNamesWidget, envir = .rqda)
        assign(".JournalNamesWidget", .JournalNamesWidget, envir = .rqda)
        assign(".FileofCase", .FileofCase, envir = .rqda)
        assign(".CodeCatWidget", .CodeCatWidget, envir = .rqda)
        assign(".CodeofCat", .CodeofCat, envir = .rqda)
        assign(".FileCatWidget", .FileCatWidget, envir = .rqda)
        assign(".FileofCat", .FileofCat, envir = .rqda)
        assign(".currentProj", .currentProj, envir = .rqda)
        assign(".SettingsGui", .settings_gui, envir = .rqda)
        assign("font", "Sans 11", envir = .rqda)
        assign("isLaunched", TRUE, envir = .rqda)

##########################
        gtkWidgetSetSensitive(.fnames_rqda$widget, FALSE)
        enabled(.JournalNamesWidget) <- FALSE
        enabled(.rqda$.codes_rqda) <- FALSE
        enabled(.rqda$.SettingsGui) <- FALSE
        enabled(.rqda$.CodeCatWidget) <- FALSE
        enabled(.rqda$.CodeofCat) <- FALSE
        enabled(.rqda$.CasesNamesWidget) <- FALSE
        enabled(.rqda$.FileofCase) <- FALSE
        enabled(.rqda$.AttrNamesWidget) <- FALSE
        enabled(.rqda$.FileCatWidget) <- FALSE
        enabled(.rqda$.FileofCat) <- FALSE
        enabled(button$profmatB) <- FALSE

##########################
### set the positions
###  svalue(.codes_pan) <- 0.09
###  svalue(.codecat_pan)<- 0.09
###  svalue(.filecat_pan)<- 0.09
###  svalue(.case_pan)<- 0.04
###  svalue(.attr_pan)<- 0.04
###  svalue(.journal_pan)<- 0.04
### The effect depends on the screen size, which makes
###  it difficult to look elegant for all PCs.
##########################

        AddHandler()
    }}
## end of function RQDA


AddHandler <- function() {
    ## add handler function for GUIs

    addHandlerUnrealize(.rqda$.root_rqdagui, handler = function(h, ...) {
        ## handler for Root
        ## make sure is the project should be closed by issuing a confirm window.
        val <- gconfirm(
            rqda_txt(paste("Really EXIT?\n\n",
                           "You can use RQDA() to start this program again.")), parent = h$obj)
        if (as.logical(val)) {
            assign("isLaunched", FALSE, envir = .rqda)
            return(FALSE) # destroy
        } else {
            return(TRUE) # don't destroy
        }
    })

    ## handler for .fnames_rqda (gtable holding the file names)
    addHandlerClicked(.rqda$.fnames_rqda, handler = function(h, ...) {
        if (isTRUE(.rqda$SFP))
            ShowFileProperty(focus = FALSE)

        Fid <- getFileIds(type = "selected")
        if (!is.null(Fid) && length(Fid) == 1) {
            names(.rqda$.fnames_rqda) <- sprintf(
                rqda_txt("Selected File id is %s"), Fid)
            gtkWidgetSetSensitive(button$DelFilB$widget, TRUE)
            gtkWidgetSetSensitive(button$VieFilB$widget, TRUE)
            gtkWidgetSetSensitive(button$FilMemB$widget, TRUE)
            gtkWidgetSetSensitive(button$FilRenB$widget, TRUE)
            ## dynamically change the label of attribute(s)
            if ((nattr <- length(.rqda$.AttrNamesWidget[])) !=  0) {
                enabled(button$FileAttrB) <- TRUE
                if (length(svalue(.rqda$.AttrNamesWidget)) > 1 || nattr > 1) {
                    svalue(button$FileAttrB) <- rqda_txt("Attributes")
                }
            }
        }
    })

    ## right click to add file to a case category
    addRightclickPopupMenu(.rqda$.fnames_rqda, GetFileNamesWidgetMenu())

    addHandlerDoubleclick(.rqda$.fnames_rqda, handler = function(h, ...) {
        ViewFileFun(FileNameWidget = .rqda$.fnames_rqda)
    })

    ## handler for .codes_rqda
    addHandlerDoubleclick(.rqda$.codes_rqda, handler = function(h, ...) {
        if (is_projOpen(envir = .rqda, conName = "qdacon")) {
            if (length(Fid <- getFileIds(condition = .rqda$TOR, type = "coded")) > 0) {
                retrieval(Fid = Fid, CodeNameWidget = .rqda$.codes_rqda)
            } else {
                gmessage(
                    rqda_txt("No coding associated with this code."),
                    container = TRUE)
            }
        }
    })

    addRightclickPopupMenu(.rqda$.codes_rqda, GetCodesNamesWidgetMenu())

    addHandlerClicked(.rqda$.codes_rqda, handler = function(h, ...) {
        ClickHandlerFun(.rqda$.codes_rqda, buttons = c("MarCodB1"),
                        codingTable = .rqda$codingTable)
        if (length(svalue(.rqda$.codes_rqda)) == 1) {
            enabled(button$RetB) <- TRUE
            enabled(button$DelCodB) <- TRUE
            enabled(button$codememobuton) <- TRUE
            enabled(button$FreCodRenB) <- TRUE
        }
    })

    ## handler for .CodeofCat
    addHandlerClicked(.rqda$.CodeofCat, handler = function(h, ...) {
        ClickHandlerFun(.rqda$.CodeofCat, buttons = c("MarCodB2", "UnMarB2"),
                        codingTable = .rqda$codingTable)
        if (length(svalue(.rqda$.CodeofCat)) > 0) {
            enabled(button$CodCatADroFromB) <- TRUE
        }
    })

    addHandlerDoubleclick(.rqda$.CasesNamesWidget, handler = function(h, ...) {
        MemoWidget(rqda_txt("Case"),
                   .rqda$.CasesNamesWidget, "cases")
    })

    addHandlerClicked(.rqda$.CasesNamesWidget, handler = function(h, ...) {

        SelectedCase <- svalue(.rqda$.CasesNamesWidget)

        if (length(SelectedCase) !=  0) {
            enabled(button$DelCasB) <- TRUE
            enabled(button$CasRenB) <- TRUE
            enabled(button$profmatB) <- TRUE
            if ((nattr <- length(.rqda$.AttrNamesWidget[])) !=  0) {
                enabled(button$CasAttrB) <- TRUE
                if (length(svalue(.rqda$.AttrNamesWidget)) > 1 || nattr > 1) {
                    svalue(button$CasAttrB) <- rqda_txt("Attributes")
                }
            }
            enabled(.rqda$.FileofCase) <- TRUE
            enabled(button$CasMarB) <-
                (exists(".root_edit", envir = .rqda) && isExtant(.rqda$.root_edit))
            Encoding(SelectedCase) <- "UTF-8"
            currentCid <- rqda_sel(sprintf("select id from cases where name = '%s'",
                                           enc(SelectedCase)))[, 1]

            freq <- rqda_sel(
                sprintf(paste("select count(distinct fid) as freq from caselinkage",
                              "where status = 1 and caseid = %s"), currentCid))$freq
            names(.rqda$.CasesNamesWidget) <- sprintf(
                rqda_txt("Selected case id is %i__%i files"),
                currentCid, freq)

            if (exists(".root_edit", envir = .rqda) && isExtant(.rqda$.root_edit)) {
                SelectedFile <- svalue(.rqda$.root_edit)
                Encoding(SelectedFile) <- "UTF-8"
                currentFid <- rqda_sel(sprintf(
                    "select id from source where name = '%s'",
                    enc(SelectedFile)))[, 1]

                ## following code: Only mark the text chuck according to
                ## the current code.
                coding.idx <- rqda_sel(
                    sprintf(paste("select selfirst, selend from coding where",
                                  "fid = %i and status = 1"), currentFid))
                anno.idx <- rqda_sel(
                    sprintf(paste("select position from annotation where",
                                  "fid = %i and status = 1"), currentFid))$position
                allidx <- unlist(coding.idx, anno.idx)

                sel_index <- rqda_sel(
                    sprintf(paste("select selfirst, selend from caselinkage where",
                                  "caseid = %i and fid = %i and status = 1"),
                            currentCid, currentFid))

                Maxindex <- rqda_sel(
                    sprintf("select max(selend) from caselinkage where fid = %i",
                            currentFid))[1, 1]

                if (!is.null(allidx) && length(allidx) > 0)
                    Maxindex <- Maxindex + sum(allidx <= Maxindex)

                ClearMark(.rqda$.openfile_gui, min = 0, max = Maxindex,
                          clear.fore.col = FALSE, clear.back.col = TRUE)

                if (nrow(sel_index) > 0) {
                    if (!is.null(allidx)) {
                        sel_index[, "selfirst"] <- sapply(
                            sel_index[, "selfirst"], FUN = function(x) {
                                x + sum(allidx <= x)
                            })
                        sel_index[, "selend"] <- sapply(
                            sel_index[, "selend"], FUN = function(x) {
                                x + sum(allidx <= x)
                            })
                    }
                    HL(.rqda$.openfile_gui, index = sel_index,
                       fore.col = NULL, back.col = .rqda$back.col)

                    enabled(button$CasUnMarB) <-
                        (exists(".root_edit", envir = .rqda) && isExtant(.rqda$.root_edit))
                    ## end of mark text chuck
                }}

            UpdateFileofCaseWidget()
        }
    })

    addHandlerClicked(.rqda$.CodeCatWidget, handler = function(h, ...) {

        Selected <- svalue(.rqda$.CodeCatWidget)
        if (identical(Selected, character(0))) {
            return(invisible(NULL))
        }

        if ((ncc <- length(Selected)) != 0) {
            enabled(.rqda$.CodeofCat)    <- TRUE
            enabled(button$DelCodCatB)   <- TRUE
            enabled(button$CodCatMemB)   <- TRUE
            enabled(button$CodCatRenB)   <- TRUE
            enabled(button$CodCatAddToB) <- TRUE
            enabled(button$MarCodB2)     <- FALSE
            enabled(button$UnMarB2)      <- FALSE

                                        # obtain one or more category ids
            sql <- paste0("select catid from codecat where name in ('",
                          paste(enc(Selected), collapse = "', '"), "')")

            catid <- rqda_sel(sql)$catid

            if (!is.null(catid) && length(catid) == 1) {
                names(.rqda$.CodeCatWidget) <- sprintf(
                    rqda_txt("Selected category id is %s"), catid)

                UpdateCodeofCatWidget(con = .rqda$qdacon, Widget = .rqda$.CodeofCat)
            }

        }


        ## if (ncc > 1) {
        ##     psccItem <- CodeCatWidgetMenu$"Plot Selected Code Category"
        ##     svalue(psccItem) <- "Plot Selected Code Categories"
        ## }

    })

    addHandlerDoubleclick(.rqda$.AttrNamesWidget, handler = function(h, ...) {
        MemoWidget(rqda_txt("Attributes"), .rqda$.AttrNamesWidget, "attributes")
    })

    addHandlerClicked(.rqda$.AttrNamesWidget, handler = function(h, ...) {
        if (length(svalue(.rqda$.AttrNamesWidget)) !=  0) {
            enabled(button$DelAttB) <- TRUE
            enabled(button$RenAttB) <- TRUE
            enabled(button$AttMemB) <- TRUE
            enabled(button$SetAttClsB) <- TRUE
            if (length(svalue(.rqda$.AttrNamesWidget)) > 1) {
                svalue(button$CasAttrB) <-
                    svalue(button$FileAttrB) <-
                    rqda_txt("Attributes")
            } else {
                svalue(button$CasAttrB) <-
                    svalue(button$FileAttrB) <-
                    rqda_txt("Attribute")
            }
        }
    })

    addHandlerDoubleclick(.rqda$.CodeCatWidget, handler = function(h, ...) {
        MemoWidget(rqda_txt("Code Category"), .rqda$.CodeCatWidget, "codecat")
    })

    addRightclickPopupMenu(.rqda$.CodeCatWidget, GetCodeCatWidgetMenu())

    addHandlerDoubleclick(.rqda$.CodeofCat, handler = function(h, ...) {
        retrieval(Fid = getFileIds(condition = .rqda$TOR, type = "coded"),
                  CodeNameWidget = .rqda$.CodeofCat)
    })

    addRightclickPopupMenu(.rqda$.CodeofCat, GetCodeofCatWidgetMenu())

    addHandlerClicked(.rqda$.FileCatWidget, handler = function(h, ...) {

        if (length(svalue(.rqda$.FileCatWidget)) > 0) {

            UpdateFileofCatWidget2(con = .rqda$qdacon, Widget = .rqda$.FileofCat)

            enabled(button$DelFilCatB) <- TRUE
            enabled(button$FilCatRenB) <- TRUE
            enabled(button$FilCatMemB) <- TRUE
            enabled(button$FilCatAddToB) <- TRUE
            enabled(.rqda$.FileofCat) <- TRUE
        }})

    addHandlerDoubleclick(.rqda$.FileCatWidget, handler = function(h, ...) {
        MemoWidget(rqda_txt("File Category"), .rqda$.FileCatWidget, "filecat")
    })

    addRightclickPopupMenu(.rqda$.FileCatWidget, GetFileCatWidgetMenu())

    addHandlerDoubleclick(.rqda$.FileofCat, handler = function(h, ...) {
        ViewFileFun(FileNameWidget = .rqda$.FileofCat)
    })

    addHandlerClicked(.rqda$.FileofCat, handler = function(h, ...) {
        if (length(svalue(.rqda$.FileofCat)) > 0) {
            enabled(button$FilCatDroFromB) <- TRUE
            names(.rqda$.FileofCat) <- sprintf(rqda_txt("Selected file id is %s"),
                                               getFileIds("filecat", "selected"))
            if (isTRUE(.rqda$SFP)) {
                ShowFileProperty(Fid = getFileIds("file", "selected"), focus = FALSE)
            }
        }
    })

    addRightclickPopupMenu(.rqda$.FileofCat, GetFileofCatWidgetMenu())

    addRightclickPopupMenu(.rqda$.CasesNamesWidget, GetCaseNamesWidgetMenu())
    ## popup menu by right-click on CaseNamesWidget

    addRightclickPopupMenu(.rqda$.FileofCase, GetFileofCaseWidgetMenu())

    addHandlerDoubleclick(.rqda$.FileofCase, handler = function(h, ...) {
        ViewFileFun(FileNameWidget = .rqda$.FileofCase)
        HL_Case()
        enabled(button$CasUnMarB) <- TRUE
        enabled(button$CasMarB) <- TRUE
    })

    addHandlerClicked(.rqda$.FileofCase, handler = function(h, ...) {
        if (length(svalue(.rqda$.FileofCase)) > 0) {
            names(.rqda$.FileofCase) <- sprintf(rqda_txt("Selected File id is %s"),
                                                getFileIds("case", "selected"))
        }
        if (isTRUE(.rqda$SFP))
            ShowFileProperty(Fid = getFileIds("case", "selected"), focus = FALSE)
    })

    addHandlerDoubleclick(.rqda$.JournalNamesWidget, handler = function(h, ...) {
        ViewJournalWidget()
    })

    addHandlerClicked(.rqda$.JournalNamesWidget, handler = function(h, ...) {
        if (length(svalue(.rqda$.JournalNamesWidget)) !=  0) {
            enabled(button$DelJouB) <- TRUE
            enabled(button$RenJouB) <- TRUE
            enabled(button$OpeJouB) <- TRUE
        }
    })

} ## end of AddHandler()
