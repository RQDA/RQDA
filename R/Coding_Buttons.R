AddCodeButton <- function(label=gettext("Add", domain = "R-RQDA")) {
  AddCodB <- gbutton(label, 
                     handler= function(h, ...) {
                       if (is_projOpen(envir=.rqda, conName="qdacon")) {
                         codename <- ginput(gettext("Enter new code. ", domain = "R-RQDA"), icon="info")
                         if (!identical(codename, character(0)))
                         {
                         if (!is.na(codename)) {
                           codename <- enc(codename, encoding="UTF-8")
                           addcode(codename)
                           CodeNamesUpdate(sortByTime=FALSE)
                         }
                         }
                       }
                     })

  assign("AddCodB", AddCodB, envir=button)
  enabled(AddCodB) <- FALSE
  AddCodB
}


DeleteCodeButton <- function(label=gettext("Delete", domain = "R-RQDA")) {
  DelCodB <- gbutton(
    label, handler= function(h, ...) {

      if (is_projOpen(envir=.rqda, conName="qdacon") &
          length(svalue(.rqda$.codes_rqda)) != 0) {
        ## if project is open and one code is selected, then continue
        del <- gconfirm(gettext("Really delete the code?", domain = "R-RQDA"), 
                        icon="question")

        if (isTRUE(del)) {
          SelectedCode <- svalue(.rqda$.codes_rqda)
          SelectedCode2 <- enc(SelectedCode, encoding="UTF-8")
          cid <- rqda_sel(
                            sprintf("select id from freecode where name='%s'", 
                                    SelectedCode2))$id
          rqda_exe(
                    sprintf("update freecode set status=0 where name='%s'", 
                            SelectedCode2))
          ## set status in table freecode to 0
          rqda_exe(
                    sprintf("update coding set status=0 where cid=%i", cid))
          ## set status in table coding to 0
          ## CodeNamesUpdate(sortByTime=FALSE)

          UpdateWidget(".codes_rqda", from=SelectedCode, to=NULL)
        }

      }
    }
  )

  assign("DelCodB", DelCodB, envir=button)
  enabled(DelCodB) <- FALSE
  DelCodB
}

RetrievalButton <- function(label) {
  RetB <- gbutton(label, 
                  handler= function(h, ...) {
                    if (is_projOpen(envir=.rqda, conName="qdacon")) {
                      Fid <- getFileIds(condition=.rqda$TOR, type="coded")
                      if (length(Fid)>0) {
                        retrieval(Fid=Fid, CodeNameWidget=.rqda$.codes_rqda)
                      } else {gmessage(gettext("No codings associated with this code.", domain = "R-RQDA"), cont=TRUE)}
                    }
                  }
  )
  gtkWidgetSetTooltipText(getToolkitWidget(RetB), gettext("Retrieve codings of the selected code.", domain = "R-RQDA"))
  assign("RetB", RetB, envir=button)
  enabled(RetB) <- FALSE
  return(RetB)
}

Mark_Button<-function(label=gettext("Mark", domain = "R-RQDA"), codeListWidget=".codes_rqda", name="MarCodB1") {
  ans <- gbutton(label, handler= function(h, ...) {
    MarkCodeFun(codeListWidget=codeListWidget, codingTable=.rqda$codingTable)
  })
  enabled(ans) <- FALSE
  assign(name, ans, envir=button)
  return(ans)
}


MarkCodeFun <- function(codeListWidget=".codes_rqda", codingTable="coding") {
  ## insert lable as mark when data is written to database.
  if (is_projOpen(envir=.rqda, conName="qdacon")) {
    currentFile <- tryCatch(svalue(.rqda$.root_edit), error= function(e) {NULL})
    if (is.null(currentFile)) gmessage(gettext("Open a file first.", domain = "R-RQDA"), con=TRUE) else{
      W <- .rqda$.openfile_gui$widget
      con <- .rqda$qdacon
      codeListWidget <- get(codeListWidget, envir=.rqda)
      idx <- sindex(.rqda$.openfile_gui, includeAnchor=FALSE, codingTable=codingTable)
      ans <- list(start=idx$startN, end=idx$endN, text=idx$seltext)
      if (ans$start != ans$end) { ## when selected no text, makes on sense to do anything.
        SelectedCode <- svalue(codeListWidget)
        if (length(SelectedCode) != 0) {
          Encoding(SelectedCode) <- "UTF-8"
          SelectedCode2 <- enc(SelectedCode, encoding="UTF-8")
          codeInfo<-  rqda_sel(sprintf("select id, color from freecode where name='%s'", SelectedCode2))
          currentCid <- codeInfo[, 1]
          codeCol <- codeInfo[, 2] ## select color for the code
          ## if (is.na(codeCol)) codeCol <-  c("antiquewhite1", "green", "aquamarine2", "bisque1", "brown1")[as.numeric(currentCid) %% 5 + 1] ## specification of default color for codemark
          if (is.na(codeCol)) codeCol <-  DefaultCodeColor[as.numeric(currentCid) %% 11 + 1] ## specification of default color for codemark
          SelectedFile <- svalue(.rqda$.root_edit)
          SelectedFile <- enc(SelectedFile, encoding="UTF-8")
          currentFid <-  rqda_sel(sprintf("select id from source where name='%s'", SelectedFile))[, 1]

          # JS Test for coherency!
          #sourcetext <- rqda_sel(sprintf("select substr(file, %s, %s) from source where name='%s'", ans$start + 1, ans$end-ans$start, SelectedFile))[, 1]
          #if (sourcetext != ans$text) {
          #  print(sprintf("Start: %s  End: %s", ans$start, ans$end))
          #  print(sprintf("Source <%s>", sourcetext))
          #  print(sprintf("Selection <%s>", ans$text))
          #  gmessage(gettext("CONSISTENCY ERROR 1 - See console for details", domain = "R-RQDA"))
          #}

          ## Exist <-  rqda_sel(sprintf("select rowid, selfirst, selend from coding where cid=%i and fid=%i and status=1", currentCid, currentFid))
          Exist1 <-  rqda_sel(sprintf("select %s.rowid, selfirst, selend, freecode.name from %s, freecode where cid=%i and fid=%i and %s.status=1 and cid=freecode.id", codingTable, codingTable, currentCid, currentFid, codingTable))
          DAT <- data.frame(cid=currentCid, fid=currentFid, seltext=ans$text, selfirst=ans$start, selend=ans$end, status=1, owner=.rqda$owner, date=date(), memo=NA, stringsAsFactors=FALSE)
          DAT$seltext <- enc(DAT$seltext)
          if (nrow(Exist1) == 0) {
            rowid <- NextRowId(codingTable)
            success <- try(rqda_exe(sprintf("insert into %s (cid, fid, seltext, selfirst, selend, status, owner, date) values (%s, %s, '%s', %s, %s, %s, '%s', '%s') ", 
                                                           codingTable, DAT$cid, DAT$fid, DAT$seltext, DAT$selfirst, DAT$selend, 1, .rqda$owner, as.character(date()))), silent=TRUE) > 0
            if (success) {
              markRange(widget=.rqda$.openfile_gui, from=ans$start, to=ans$end, rowid=rowid, addButton=TRUE, buttonLabel=SelectedCode, buttonCol=codeCol, codingTable=codingTable)}
            else{
              gmessage(gettext("Fail to write to database.", domain = "R-RQDA"))
            }
          } else {
            Exist <- Exist1[, c("selfirst", "selend", "rowid")]
            Relations <- apply(Exist, 1, FUN= function(x) relation(x[c("selfirst", "selend")], c(ans$start, ans$end)))
            Exist$Relation <- sapply(Relations, FUN= function(x)x$Relation)
            if (!any(Exist$Relation == "exact")) {
              ## if they are axact, do nothing; -> if they are not exact, do something.
              Exist$WhichMin <- sapply(Relations, FUN= function(x)x$WhichMin)
              Exist$Start <- sapply(Relations, FUN= function(x)x$UnionIndex[1])
              Exist$End <- sapply(Relations, FUN= function(x)x$UnionIndex[2])
              if (all(Exist$Relation == "proximity")) {
                rowid <- NextRowId(codingTable)
                success <- try(rqda_exe(sprintf("insert into %s (cid, fid, seltext, selfirst, selend, status, owner, date) values (%s, %s, '%s', %s, %s, %s, '%s', '%s') ", 
                                                               codingTable, DAT$cid, DAT$fid, DAT$seltext, DAT$selfirst, DAT$selend, 1, .rqda$owner, as.character(date()))), silent=TRUE) > 0
                if (success) {
                  markRange(widget=.rqda$.openfile_gui, from=ans$start, to=ans$end, rowid=rowid, addButton=TRUE, 
                            buttonLabel=SelectedCode, buttonCol=codeCol, codingTable=codingTable)
                } else {gmessage(gettext("Fail to write to data base.", domain = "R-RQDA"), con=TRUE)}
                ## if there are no overlap in any kind, just write to database; otherwise, pass to else{}.
              } else {
                del1 <-(Exist$Relation == "inclusion" & (is.na(Exist$WhichMin) | Exist$WhichMin == 2))
                ## if overlap or inclusion [old nested in new]
                ## then the original coding should be deleted
                ## then write the new coding to table
                del2 <- Exist$Relation == "overlap"
                del <- (del1 | del2)
                if (any(del)) {
                  Sel <- c(min(Exist$Start[del]), max(Exist$End[del]))
                  memo <- rqda_sel(sprintf("select memo from %s where rowid in (%s)", codingTable, paste(Exist$rowid[del], collapse=", ", sep="")))$memo
                  memo <- paste(memo, collapse="", sep="")
                  rqda_exe(sprintf("delete from %s where rowid in (%s)", codingTable, paste(Exist$rowid[del], collapse=", ", sep="")))
                  buffer <- W$buffer
                  for (i in Exist1$rowid[del]) {
                    code <- Exist1[Exist1$rowid == i, "name"]
                    m <- buffer$GetMark(sprintf("%s.1", i))
                    pos <- buffer$GetIterAtMark(m)$iter$GetOffset()
                    DeleteButton(widget=W, label=sprintf("<%s>", code), index=pos, direction="backward")
                    m <- buffer$GetMark(sprintf("%s.2", i))
                    pos <- buffer$GetIterAtMark(m)$iter$GetOffset()
                    ##DeleteButton(widget=W, label=sprintf(">%s", code), index=pos, direction="forward")
                  }
                  tt <- svalue(W)
                  Encoding(tt) <- "UTF-8"
                  DAT <- data.frame(cid=currentCid, fid=currentFid, seltext=substr(tt, Sel[1] + 1, Sel[2]), selfirst=Sel[1], selend=Sel[2], status=1, owner=.rqda$owner, date=date(), memo=memo, stringsAsFactors=FALSE)

                  # JS Test for coherency!
                  #sourcetext <- rqda_sel(sprintf("select substr(file, %s, %s) from source where name='%s'", Sel[1] + 1, Sel[2]-Sel[1], SelectedFile))[, 1]
                  #seltext = substr(tt, Sel[1] + 1, Sel[2])
                  #if (sourcetext != seltext) {
                  #  print(sprintf("Start: %s  End: %s", Sel[1], Sel[2]))
                  #  print(sprintf("Source <%s>", sourcetext))
                  #  print(sprintf("Selection <%s>", seltext))
                  #  gmessage(gettext("CONSISTENCY ERROR 2 - See console for details", domain = "R-RQDA"))
                  #}

                  DAT$seltext <- enc(DAT$seltext)
                  rowid <- NextRowId(codingTable)
                  success <- is.null(try(rqda_exe(sprintf("insert into %s (cid, fid, seltext, selfirst, selend, status, owner, date, memo) values (%s, %s, '%s', %s, %s, %s, '%s', '%s', '%s') ", 
                                                           codingTable, DAT$cid, DAT$fid, DAT$seltext, DAT$selfirst, DAT$selend, 1, .rqda$owner, as.character(date()), DAT$memo)), silent=TRUE))
                  if (success) {
                    markRange(widget=.rqda$.openfile_gui, from=Sel[1], to=Sel[2], rowid=rowid, addButton=TRUE, buttonLabel=SelectedCode, buttonCol=codeCol, codingTable=codingTable)}else{gmessage(gettext("Fail to write to database.", domain = "R-RQDA"))}
                }
              }}}}}}}}


Unmark_Button <- function(label=gettext("Unmark", domain = "R-RQDA"), codeListWidget=.rqda$.codes_rqda, name="UnMarB1") {
  ans <- gbutton(gettext("Unmark", domain = "R-RQDA"), handler= function(h, ...) {
    UnMarkCodeFunByRowid(codeListWidget=codeListWidget, codingTable=.rqda$codingTable)
    enabled(button$UnMarB1) <- FALSE
  }
  )
  enabled(ans) <- FALSE
  assign(name, ans, envir=button)
  ans
}

UnMarkCodeFun <- function(codeListWidget=.rqda$.codes_rqda, codingTable="coding") {
  ## this function is superseded by MarkCodeFunByRowid
  if (is_projOpen(envir=.rqda, conName="qdacon")) {
    con <- .rqda$qdacon
    W <- tryCatch(get(".openfile_gui", envir=.rqda), error= function(e) {})
    ## get the widget for file display. If it does not exist, then return NULL.
    idx1 <- tryCatch(sindex(W, includeAnchor=FALSE, codingTable=codingTable), error= function(e) {})
    idx2 <- tryCatch(sindex(W, includeAnchor=TRUE, codingTable=codingTable), error= function(e) {})
    ## if the not file is open, unmark doesn't work.
    if (!is.null(idx1)) {
      ## codeListWidget <- get(codeListWidget, envir=.rqda)
      SelectedCode <- svalue(codeListWidget)
      if (length(SelectedCode) == 0) {
        gmessage(gettext("Select a code first.", domain = "R-RQDA"), con=TRUE)
      } else {
        Encoding(SelectedCode) <- "UTF-8"
        SelectedCode2 <- enc(SelectedCode, "UTF-8")
        currentCid <-  rqda_sel(
                                  sprintf("select id from freecode where name='%s'", 
                                          SelectedCode2))[, 1]
        SelectedFile <- svalue(.rqda$.root_edit)
        SelectedFile <- enc(SelectedFile, "UTF-8") ## Encoding(SelectedFile) <- "UTF-8"
        currentFid <-  rqda_sel(sprintf("select id from source where name='%s'", 
                                              SelectedFile))[, 1]
        codings_index <-  rqda_sel(sprintf("select rowid, cid, fid, selfirst, selend from %s where cid=%i and fid=%i and status=1", codingTable, currentCid, currentFid))
        ## should only work with those related to current code and current file.
        rowid <- codings_index$rowid[(codings_index$selfirst  >= idx1$startN) &
                                       (codings_index$selend  <= idx1$endN)]
        if (length(rowid) == 0) {
          gmessage(gettext("Select a code and one of its codings exactly first.", domain = "R-RQDA"), con=TRUE)
        } else {
          for (j in rowid) {
            ClearMark(W, min=idx2$startN, max=idx2$endN)
            ## This clear all the marks in the gtext window
            buffer <- .rqda$.openfile_gui$widget$buffer
            startIter <- buffer$GetIterAtMark(idx2$startMark)$iter
            startN <- startIter$GetOffset()
            isRemoved <- DeleteButton(.rqda$.openfile_gui, label=sprintf("<%s>", svalue(codeListWidget)), 
                                      index=startN, direction="backward")
            if (isRemoved) {
              rqda_exe(sprintf("update %s set status=-1 where rowid=%i", codingTable, j))
              ## better to get around the loop by sqlite condition expression.
              ## status=-1 to differentiate the result of delete button
              endMark <- buffer$GetMark(sprintf("%s.2", j))
              ## gtkTextMarkSetVisible(endMark, FALSE)
              gtkTextBufferDeleteMarkByName(buffer, sprintf("%s.2", j))
              ##endIter <- buffer$GetIterAtMark(idx2$endMark)$iter
              ##endN <- endIter$GetOffset()
              ##DeleteButton(.rqda$.openfile_gui, label=sprintf(">%s", svalue(codeListWidget)), index=endN, direction="forward")
              ## even for the non-current code. can improve.
            }
          }
        }
      }
    }
  }
}


UnMarkCodeFunByRowid <- function(codeListWidget=.rqda$.codes_rqda, codingTable="coding") {
  W <- tryCatch(get(".openfile_gui", envir=.rqda), error= function(e) {})
  ## get the widget for file display. If it does not exist, then return NULL.
  if (!is.null(W)) {
    rowid <- .codingEnv$selectedRowid
    coding_index <- rqda_sel(sprintf("select cid, fid, selfirst, selend from %s where rowid=%s and status=1", 
                                      codingTable, rowid))
    nshift1 <- nrow(rqda_sel(sprintf("select selfirst from %s where status=1 and selfirst<= %s and fid=%s", 
                                      codingTable, coding_index$selfirst, coding_index$fid)))
    nshift2 <- nrow(rqda_sel(sprintf("select fid, position from annotation where status=1 and position <= %s and fid=%s", 
                                      coding_index$selfirst, coding_index$fid)))
    nshift <- nshift1 + nshift2
    ClearMark(W, min=coding_index$selfirst + nshift, max=coding_index$selend + nshift)
    ## clear mark of the selected coding
    codeName <- rqda_sel(sprintf("select name from freecode where status=1 and id = %s", coding_index$cid))$name
    Encoding(codeName) <- "UTF-8"
    buffer <- .rqda$.openfile_gui$widget$buffer
    isRemoved <- DeleteButton(.rqda$.openfile_gui, label=sprintf("<%s>", codeName), 
                              index=coding_index$selfirst + nshift, direction="backward")
    if (isRemoved) {
      rqda_exe(sprintf("update %s set status=-1 where rowid=%s", codingTable, rowid))
      endMark <- buffer$GetMark(sprintf("%s.2", rowid))
      gtkTextBufferDeleteMarkByName(buffer, sprintf("%s.2", rowid))
    }
  }
}

CodeMemoButton <- function(label=gettext("C-Memo", domain = "R-RQDA"), ...) {
  codememobuton <- gbutton(label, handler= function(h, ...) {
    MemoWidget(gettext("code", domain = "R-RQDA"), .rqda$.codes_rqda, "freecode")
  }
  )
  gtkWidgetSetTooltipText(getToolkitWidget(codememobuton), gettext("Memo for selected code.", domain = "R-RQDA"))
  assign("codememobuton", codememobuton, envir=button)
  enabled(codememobuton) <- FALSE
  return(codememobuton)
}


CodingMemoButton <- function(label=gettext("C2Memo", domain = "R-RQDA"))
{

  InsertCodingMemoAnchor <- function(index, rowid, label=gettext("[Coding Memo]", domain = "R-RQDA"), title)
  {
    ## don't use this function.
    ## use Annotation to add anno to a file.
    widget=.rqda$.openfile_gui
    lab <- gtkLabelNew(label)
    label <- gtkEventBoxNew()
    label$ModifyBg("normal", gdkColorParse("yellow")$color)
    label$Add(lab)
    buffer <- widget$buffer
    button_press <- function(widget, event, moreArgs) {
      OpenCodingMemo(rowid=moreArgs$rowid, title=moreArgs$title)
    }
    gSignalConnect(label, "button-press-event", button_press, data =list(rowid=rowid, title=title))
    iter <- gtkTextBufferGetIterAtOffset(buffer, index)$iter
    anchorcreated <- buffer$createChildAnchor(iter)
    iter$BackwardChar()
    anchor <- iter$getChildAnchor()
    anchor <- gtkTextIterGetChildAnchor(iter)
    widget$widget$addChildAtAnchor(label, anchor)
    return(TRUE)
  }

  OpenCodingMemo <- function(rowid, AnchorPos=NULL, title=NULL) {
    ##  open a widget for memo, and take care of the save memo function
    tryCatch(dispose(.rqda$.codingmemo), error= function(e) {})
    ## Close the coding memo first, then open a new one
    if (is.null(title)) title <- "Coding Memo"
    .codingmemo <- gwindow(title=title, getOption("widgetCoordinate"), 
    width = getOption("widgetSize")[1], height = getOption("widgetSize")[2])
    
        addHandlerKeystroke(.codingmemo, function(h, ...) {
        if(h$key == "\027") dispose(.codingmemo)
        })
    assign(".codingmemo", .codingmemo, envir=.rqda)
    .codingmemo <- get(".codingmemo", envir=.rqda)
    .codingmemo2 <- gpanedgroup(horizontal = FALSE, container=.codingmemo)
    gbutton(gettext("Save Coding Memo", domain = "R-RQDA"), container=.codingmemo2, handler= function(h, ...) {
      newcontent <- svalue(.rqda$.cdmemocontent)
      newcontent <- enc(newcontent, encoding="UTF-8") ## take care of double quote.
      rqda_exe(sprintf("update coding set memo='%s' where rowid=%i", newcontent, rowid))
      ## if (isTRUE(.rqda$NewCodingMemo)) {
      ## if (InsertCodingMemoAnchor(index=AnchorPos, rowid=rowid, title=title)) assign("NewCodingMemo", FALSE, envir=.rqda)
      ## }
      ## if newcontent is "", should delete the codingMemoAnchor (not add memoanchor here)
    })## end of save memo button
    assign(".cdmemocontent", gtext(container=.codingmemo2, font.attr=list(size="large")), envir=.rqda)
    prvcontent <- rqda_sel(sprintf("select memo from coding where rowid=%i", rowid))[1, 1]
    if (is.na(prvcontent)) prvcontent <- ""
    Encoding(prvcontent) <- "UTF-8"
    if (prvcontent == "") assign("NewCodingMemo", TRUE, envir=.rqda)
    W <- get(".cdmemocontent", envir=.rqda)
    insert(W, prvcontent, do.newline = FALSE, where = "beginning", 
           font.attr=list(size="large"))
  } ## end of OpenCodingMemo

  c2memobutton <- gbutton(label, handler= function(h, ...) {
    con <- .rqda$qdacon
    if (is_projOpen(envir=.rqda, conName="qdacon")) {
      W <- tryCatch(get(".openfile_gui", envir=.rqda), error= function(e) {})
      ## get the widget for file display. If it does not exist, then return NULL.
      sel_index <- tryCatch(sindex(W, includeAnchor=FALSE), error= function(e) {})
      AnchorPos <- tryCatch(sindex(W, includeAnchor=TRUE)$startN, error= function(e) {})
      ## if the not file is open, it doesn't work.
      if (is.null(sel_index)) {gmessage(gettext("Open a file first!", domain = "R-RQDA"), container=TRUE)}
      else {
        SelectedCode <- svalue(.rqda$.codes_rqda)
        if (length(SelectedCode) == 0) gmessage(gettext("select a code first.", domain = "R-RQDA"), container=TRUE) else {
          Encoding(SelectedCode) <- "UTF-8"
          SelectedCode2 <- enc(SelectedCode, "UTF-8")
          currentCid <-  rqda_sel(sprintf("select id from freecode where name='%s'", SelectedCode2))[, 1]
          SelectedFile <- svalue(.rqda$.root_edit)
          SelectedFile <- enc(SelectedFile, encoding="UTF-8")
          currentFid <-  rqda_sel(sprintf("select id from source where name='%s'", SelectedFile))[, 1]
          codings_index <-  rqda_sel(sprintf("select rowid, cid, fid, selfirst, selend from coding where
                                                   cid=%i and fid=%i and status=1 ", currentCid, currentFid))
          ## should only work with those related to current code and current file.
          rowid <- codings_index$rowid[(codings_index$selfirst  >= sel_index$startN) &
                                         (codings_index$selfirst  <= sel_index$startN + 2) &
                                         (codings_index$selend  <= sel_index$endN)&
                                         (codings_index$selend  >= sel_index$endN - 2)
          ] ## determine which one is the current text chunk?
          if (length(rowid) !=  1) {gmessage(gettext("Select the exact CODING AND the corresponding CODE first.", domain = "R-RQDA"), container=TRUE)}
          else {
            OpenCodingMemo(rowid=rowid, AnchorPos=AnchorPos, title=sprintf("Coding Memo: %s", SelectedCode))
          }
        }
      }}})
  gtkWidgetSetTooltipText(getToolkitWidget(c2memobutton), gettext("Memo for a Coding.", domain = "R-RQDA"))
  assign("c2memobutton", c2memobutton, envir=button)
  enabled(c2memobutton) <- FALSE
  return(c2memobutton)
}



FreeCode_RenameButton <- function(label=gettext("Rename", domain = "R-RQDA"), CodeNamesWidget=.rqda$.codes_rqda, ...)
{
  ## rename of selected file.
  FreCodRenB <- gbutton(label, handler= function(h, ...) {
    if (is_projOpen(envir=.rqda, "qdacon")) {
      ## if project is open, then continue
      selectedCodeName <- svalue(CodeNamesWidget)
      if (length(selectedCodeName) == 0) {
        gmessage(gettext("Select a code first.", domain = "R-RQDA"), icon="error", container=TRUE)
      }
      else {
        ## get the new file names
        NewCodeName <- ginput(gettext("Enter new code name. ", domain = "R-RQDA"), text=selectedCodeName, icon="info")

        if (!identical(NewCodeName, character(0)))
        {
        if (!is.na(NewCodeName)) {
          Encoding(NewCodeName) <- Encoding(selectedCodeName) <- "UTF-8"
          ## update the name in source table by a function
          rename(selectedCodeName, NewCodeName, "freecode")
          ## (name is the only field should be modifed, as other table use ID rather than name)
          ## CodeNamesUpdate(sortByTime=FALSE)
          UpdateWidget(".codes_rqda", from=selectedCodeName, to=NewCodeName)
        }
        }
      }
    }
  }
  )
  assign("FreCodRenB", FreCodRenB, envir=button)
  enabled(FreCodRenB) <- FALSE
  FreCodRenB
}


AnnotationButton <- function(label=gettext("Add Anno", domain = "R-RQDA")) {
  AnnB <- gbutton(label, handler= function(h, ...) {
    if (is_projOpen(envir=.rqda, conName="qdacon")) {
      Annotation()
    }})
  gtkWidgetSetTooltipText(getToolkitWidget(AnnB), gettext("Add new annotation to the open file\nat position of cursor.", domain = "R-RQDA"))
  assign("AnnB", AnnB, envir=button)
  enabled(AnnB) <- FALSE
  return(AnnB)
}


## popup-menu
GetCodesNamesWidgetMenu <- function()
{
  CodesNamesWidgetMenu <- list()

  CodesNamesWidgetMenu[[1]] <- gaction(gettext("Add To Code Category...", domain = "R-RQDA"), handler = function(h, ...) {
    AddToCodeCategory()
  })

  CodesNamesWidgetMenu[[2]] <- gaction(gettext("All Code Memos", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      print(getMemos())
    }
  })

  CodesNamesWidgetMenu[[3]] <- gaction(gettext("All Annotations", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      print(getAnnos())
    }
  })

  CodesNamesWidgetMenu[[4]] <- gaction(gettext("Code Memo", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      MemoWidget(gettext("code", domain = "R-RQDA"), .rqda$.codes_rqda, "freecode")
    }
  })

  CodesNamesWidgetMenu[[5]] <- gaction(gettext("Codings of Multiple Codes", domain = "R-RQDA"), handler = function(h, ...) {
    ct <- getCodingsOfCodes(fid=getFileIds(condition=.rqda$TOR))
    print.codingsByOne(ct)
  })

  CodesNamesWidgetMenu[[6]] <- gaction(gettext("Export Codings as HTML", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      path=gfile(type="save", text = gettext("Type a name for the exported codings and click OK.", domain = "R-RQDA"))
     if (!identical(path, character(0)))
     {
      if (!is.na(path)) {
        Encoding(path) <- "UTF-8"
        path <- sprintf("%s.html", path)
        if (.rqda$TOR == "uncondition") fid <- NULL else fid <- getFileIds(condition=.rqda$TOR)
        exportCodings(file=path, Fid=fid)
      }
      }}
  })

  CodesNamesWidgetMenu[[7]] <- gaction(gettext("Highlight All Codings", domain = "R-RQDA"), handler = function(h, ...) {
    HL_AllCodings()
  })

  CodesNamesWidgetMenu[[8]] <- gaction(gettext("Highlight Codings with Memo", domain = "R-RQDA"), handler = function(h, ...) {
    HL_CodingWithMemo()
  })

  CodesNamesWidgetMenu[[9]] <- gaction(gettext("Merge Selected with...", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      Selected1 <- svalue(.rqda$.codes_rqda)
      cid1 <- rqda_sel(sprintf("select id from freecode where name='%s'", Selected1))[1, 1]
      Selected2 <- gselect.list(as.character(.rqda$.codes_rqda[]), x=getOption("widgetCoordinate")[1])
      if (Selected2 != "" && Selected1 != Selected2) cid2 <- rqda_sel(sprintf("select id from freecode where name='%s'", Selected2))[1, 1]
      mergeCodes(cid1, cid2)
      CodeNamesWidgetUpdate()
    }
  })

  CodesNamesWidgetMenu[[10]] <- gaction(gettext("Show Codes With Codings", domain = "R-RQDA"), handler = function(h, ...) {
    CodeWithCoding(.rqda$TOR)
  })

  CodesNamesWidgetMenu[[11]] <- gaction(gettext("Show Codes Without Codings", domain = "R-RQDA"), handler = function(h, ...) {
    CodeWithoutCoding(condition=.rqda$TOR)
  })

  CodesNamesWidgetMenu[[12]] <- gaction(gettext("Show Codes With Code Category", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      cid <- rqda_sel("select id from freecode where status=1 and id in (select cid from treecode where status=1)")
      if (nrow(cid) != 0) {
        cid <- cid[[1]]
        CodeNamesWidgetUpdate(CodeNamesWidget=.rqda$.codes_rqda, CodeId=cid, sortByTime=FALSE)
      } else gmessage(gettext("All codes are assigned to code category.", domain = "R-RQDA"), container=TRUE)
    }
  })

  CodesNamesWidgetMenu[[13]] <- gaction(gettext("Show Codes Without Code Category", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      cid <- rqda_sel("select id from freecode where status=1 and id not in (select cid from treecode where status=1)")
      if (nrow(cid) != 0) {
        cid <- cid[[1]]
        CodeNamesWidgetUpdate(CodeNamesWidget=.rqda$.codes_rqda, CodeId=cid, sortByTime=FALSE)
      } else gmessage(gettext("All codes are assigned to code category.", domain = "R-RQDA"), container=TRUE)
    }
  })

  CodesNamesWidgetMenu[[14]] <- gaction(gettext("Show Codes With Memo", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      cid <- rqda_sel("select id from freecode where memo is not null and memo != ''")
      if (nrow(cid) != 0) {
        cid <- cid[[1]]
        CodeNamesWidgetUpdate(CodeNamesWidget=.rqda$.codes_rqda, CodeId=cid, sortByTime=FALSE)
      } else gmessage(gettext("No Code with memo.", domain = "R-RQDA"), container=TRUE)
    }
  })

  CodesNamesWidgetMenu[[15]] <- gaction(gettext("Show Codes Without Memo", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      cid <- rqda_sel("select id from freecode where memo is null or memo = ''")
      if (nrow(cid) != 0) {
        cid <- cid[[1]]
        CodeNamesWidgetUpdate(CodeNamesWidget=.rqda$.codes_rqda, CodeId=cid, sortByTime=FALSE)
      } else gmessage(gettext("No Code with memo.", domain = "R-RQDA"), container=TRUE)
    }
  })

  CodesNamesWidgetMenu[[16]] <- gaction(gettext("Show Codes With Selected File", domain = "R-RQDA"), handler = function(h, ...) {
    fname <- svalue(.rqda$.fnames_rqda)
    if (length(fname) > 0) {
      fname <- enc(fname)
      codes <- rqda_sel(sprintf("select freecode.name from freecode where freecode.status=1 and freecode.id in (
                                 select coding.cid from coding where coding.status=1 and coding.fid in (
                                                                                                        select source.id from source where source.status=1 and source.name='%s'
                                                                                                        )
                                 )", fname))
      if (nrow(codes) > 0) {
        Encoding(codes$name) <- "UTF-8"
        .rqda$.codes_rqda[] <- codes$name
      }
    }
  })

  CodesNamesWidgetMenu[[17]] <- gaction(gettext("Set Coding Mark Color", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      Selected <- svalue(.rqda$.codes_rqda)
      Selected <- enc(Selected, "UTF-8")
      codeInfo <- rqda_sel(sprintf("select id, color from freecode where name='%s'", Selected))[1, ]
      cid <- codeInfo[, 1]
      codeColor <- codeInfo[, 2]
      if (is.na(codeColor)) title <- "Change color to..." else title <- sprintf("Change from '%s' to...", codeColor)
      newCol <- gselect.list(colors(), multiple = FALSE, title = title, x=getOption("widgetCoordinate")[1])
      if (newCol != "" && length(newCol) != 0) {
        if (!identical(codeColor, newCol)) {
          rqda_exe(sprintf("update freecode set color='%s' where id =%i", newCol, cid))
        }
      }}
  })

  CodesNamesWidgetMenu[[18]] <- gaction(gettext("Sort: By Created Time", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      CodeNamesUpdate(sortByTime=TRUE)
    }
  })

  CodesNamesWidgetMenu[[19]] <- gaction(gettext("Sort: Most to Least Frequently Used", domain = "R-RQDA"), handler = function(h, ...) {
    freq <- rqda_sel("select count(cid) as freq, freecode.name, freecode.status from freecode left join coding on cid=freecode.id group by freecode.name order by freq desc")
    if (nrow(freq)>0) {
      Encoding(freq$name) <- "UTF-8"
      #freq <- subset(freq, status == 1)
      freq <- freq[freq$status == 1, ]
      CodeNamesWidget=.rqda$.codes_rqda
      CodeNamesWidget[] <- freq$name
    }
  })

  CodesNamesWidgetMenu[[20]] <- gaction(gettext("Sort: Least to Most Frequently Used", domain = "R-RQDA"), handler = function(h, ...) {
    freq <- rqda_sel("select count(cid) as freq, freecode.name, freecode.status from freecode left join coding on cid=freecode.id group by freecode.name order by freq asc")
    if (nrow(freq)>0) {
      Encoding(freq$name) <- "UTF-8"
      #freq <- subset(freq, status == 1)
      freq <- freq[freq$status == 1, ]
      CodeNamesWidget=.rqda$.codes_rqda
      CodeNamesWidget[] <- freq$name
    }
  })

  CodesNamesWidgetMenu
}
