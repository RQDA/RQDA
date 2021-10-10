EditVarWidget <- function(ExistingItems = NULL, container = NULL, title = NULL, ID = NULL, saveFUN = NULL, ...) {
  ## modified from RGtk2 package
  ## ExistingItems: existing data set for a case/file etc. It is data frame of 2 columns, the first is Variable
  ## saveFUN is character.
  ## container: similar to that of gWidget package.
  COLUMN <- c(Variable = 0, Value = 1, editable = 2)
  articles <- NULL

  create.model <- function()
    {
      ## create the array of data
      articles <<- list()
      ##  create list store
      model <- gtkListStoreNew("gchararray", "gchararray", "gboolean")
      ## add item from ExistingItems
      ## needs modification
      if (!is.null(ExistingItems)) {
        articles <<- c(articles, unlist(apply(ExistingItems, 1, function(x) list(list(Variable = x[1], Value = x[2], editable = TRUE))), FALSE))
        for (i in 1:length(articles))
          {
            iter <- model$append()$iter
            model$set(iter, COLUMN["Variable"], articles[[i]]$Variable,
                      COLUMN["Value"], articles[[i]]$Value,
                      COLUMN["editable"], articles[[i]]$editable)
          }
      }
      return(model)
    }

  cell.edited <- function(cell, path.string, new.text, data)
    {
      Encoding(new.text) <- 'UTF-8' ## now atrribute displays correctly for non-english character
      checkPtrType(data, "GtkListStore")
      model <- data
      path <- gtkTreePathNewFromString(path.string)
      column <- cell$getData("column")
      iter <- model$getIter(path)$iter
      if (column == 1) {
               i <- path$getIndices()[[1]] + 1
               articles[[i]]$Value <<- new.text
               model$set(iter, column, articles[[i]]$Value)
             }
    }

  add.columns <- function(treeview)
    {
      model <- treeview$getModel()
      ## Variable column
      renderer <- gtkCellRendererTextNew()
      gSignalConnect(renderer, "edited", cell.edited, model)
      renderer$setData("column", COLUMN["Variable"])
      treeview$insertColumnWithAttributes(-1, "Variable", renderer, text = COLUMN[["Variable"]], editable = COLUMN[["editable"]])
      ## Value column
      renderer <- gtkCellRendererTextNew()
      gSignalConnect(renderer, "edited", cell.edited, model)
      renderer$setData("column", COLUMN["Value"])
      treeview$insertColumnWithAttributes(-1, "Value", renderer, text = COLUMN[["Value"]], editable = COLUMN[["editable"]])
    }

    saveFUN <- get(saveFUN, mode = "function")

  ## create window, etc
  window <- gtkWindowNew("toplevel", show = F)
  Encoding(title) <- 'UTF-8'
  window$setTitle(paste(gettext("Attribute of:", domain = "R-RQDA"), title))
  #window$`border-width` <- 5
  vbox <- gtkVBoxNew(FALSE, 5)
  window$add(vbox)
  sw <- gtkScrolledWindowNew(NULL, NULL)
  sw$setShadowType("etched-in")
  sw$setPolicy("automatic", "automatic")
  vbox$packStart(sw, TRUE, TRUE, 0)
  ## create model
  model <- create.model()
  ## create tree view
  treeview <- gtkTreeViewNewWithModel(model)
  treeview$setRulesHint(TRUE)
  treeview$getSelection()$setMode("single")
  add.columns(treeview)
  sw$add(treeview)
  ## some buttons
  hbox <- gtkHBoxNew(TRUE, 4)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel(gettext("Save and Close", domain = "R-RQDA"))
  gSignalConnect(button, "clicked", saveFUN, list(model, window, ExistingItems, list(...)))
  hbox$packStart(button, TRUE, TRUE, 0)
  window$setDefaultSize(300, 350)
  # window$Move(size(.rqda$.root_rqdagui)[1], 2)
  window$showAll()
  invisible(window)
}

saveFUN4CaseAttr <- function(button, data) {
  ## the first arg must button, and data as second.
  ## push dataset into project file.
  model <- data[[1]]
  window <- data[[2]]
  ExistingItems <- data[[3]]
  MoreArgs <- data[[4]]
  IterFirst <- model$getIterFirst()
  cond <- IterFirst[[1]]
  iter <- IterFirst$iter
  ans <- c()
  while(cond) {
    dat <- unlist(model$get(iter, 0, 1))
    ans <- c(ans, dat)
    cond <- model$iterNext(iter)
  }
  n <- length(ans)
  if (n >= 2) {
    idx1 <- seq(1, to = n, by = 2)
    idx2 <- seq(2, to = n, by = 2)
    ans <- data.frame(Variable = ans[idx1], Value = ans[idx2], stringsAsFactors = FALSE)
    ## cal which variable is added and which is modified
    ExistingItems$value[which(is.na(ExistingItems$value))] <- "NA" ## add this line to address NA.
    change_idx <- ans$Value != ExistingItems$value
    mod_idx <- change_idx & (ExistingItems$value !=  "NA")
    new_idx <- change_idx & (! mod_idx)
    if (any(mod_idx)) {
    ## alter the table for the modified variable
    vars <- ans[mod_idx, ]
    apply(vars, 1, FUN = function(x) rqda_exe(sprintf("update caseAttr set value = '%s' where variable = '%s' and caseID ='%s' and status =1", x[2], x[1], MoreArgs$caseId)))
    }
    if (any(new_idx)) {
    ## add the new variable to table
    vars <- data.frame(variable = ans[new_idx, 1], value = ans[new_idx, 2], caseID = MoreArgs$caseId, date = date(), dateM = NA, owner=.rqda$owner, status = 1)
    rqda_wrt("caseAttr", vars)
  }
  }
  window$Destroy()## close
}

CaseAttrFun <- function(caseId, title = NULL, attrs = svalue(.rqda$.AttrNamesWidget)) {
  if (length(attrs) == 0) attrs <-  rqda_sel("select name from attributes where status = 1")$name
  if (is.null(attrs)) {
   gmessage(gettext("add attribute in Attrs Table first.", domain = "R-RQDA"), container = TRUE)
   } else {
    attrs2 <- data.frame(variable = attrs, value = "NA", stringsAsFactors = FALSE)
    variables <- rqda_sel(sprintf("select variable, value from caseAttr where caseID=%i and variable in (%s) and status = 1", caseId, paste(shQuote(attrs), collapse = ", ")))
    if (nrow(variables) != 0) {
      Encoding(variables$variable) <- Encoding(variables$value) <- 'UTF-8'
      idx <- match(variables[[1]], attrs2[[1]])
      attrs2[idx, ] <- variables
    }
    EditVarWidget(ExistingItems = attrs2, saveFUN = "saveFUN4CaseAttr", title = title, caseId = caseId)
    ## get attrs list and turn it to a data frame, pass it to ExistingItems, then call EditVarWidget
  }
}

saveFUN4FileAttr <- function(button, data) {
  ## the first arg must button, and data as second.
  ## push dataset into project file.
  model <- data[[1]]
  window <- data[[2]]
  ExistingItems <- data[[3]]
  MoreArgs <- data[[4]]
  IterFirst <- model$getIterFirst()
  cond <- IterFirst[[1]]
  iter <- IterFirst$iter
  ans <- c()
  while(cond) {
    dat <- unlist(model$get(iter, 0, 1))
    ans <- c(ans, dat)
    cond <- model$iterNext(iter)
  }
  n <- length(ans)
  if (n >= 2) {
    idx1 <- seq(1, to = n, by = 2)
    idx2 <- seq(2, to = n, by = 2)
    ans <- data.frame(Variable = ans[idx1], Value = ans[idx2], stringsAsFactors = FALSE)
    ## cal which variable is added and which is modified
    change_idx <- ans$Value != ExistingItems$value
    mod_idx <- change_idx & (ExistingItems$value !=  "NA")
    new_idx <- change_idx & (! mod_idx)
    if (any(mod_idx)) {
    ## alter the table for the modified variable
    vars <- ans[mod_idx, ]
    apply(vars, 1, FUN = function(x) rqda_exe(sprintf("update fileAttr set value = '%s' where variable = '%s' and fileID ='%s'and status = 1", x[2], x[1], MoreArgs$fileId)))
    }
    if (any(new_idx)) {
    ## add the new variable to table
    vars <- data.frame(variable = ans[new_idx, 1], value = ans[new_idx, 2], fileID = MoreArgs$fileId, date = date(), dateM = NA, owner=.rqda$owner, status = 1)
    rqda_wrt("fileAttr", vars)
    }
  }
  window$Destroy()## close
}

FileAttrFun <- function(fileId, title = NULL, attrs = svalue(.rqda$.AttrNamesWidget)) {
  if (length(attrs) == 0) attrs <-  rqda_sel("select name from attributes where status = 1")$name
  if (is.null(attrs)) gmessage(gettext("add attribute in Attrs Table first.", domain = "R-RQDA"), container = TRUE) else{
    Encoding(attrs) <- 'UTF-8'
    attrs2 <- data.frame(variable = attrs, value = "NA", stringsAsFactors = FALSE)
    variables <- rqda_sel(sprintf("select variable, value from fileAttr where fileID=%i and variable in (%s) and status = 1", fileId, paste(shQuote(attrs), collapse = ", ")))
    if (nrow(variables) != 0) {
      Encoding(variables$variable) <- Encoding(variables$value) <- 'UTF-8'
      idx <- match(variables[[1]], attrs2[[1]])
      attrs2[idx, ] <- variables
    }
    EditVarWidget(ExistingItems = attrs2, saveFUN = "saveFUN4FileAttr", title = title, fileId = fileId)
    ## get attrs list and turn it to a data frame, pass it to ExistingItems, then call EditVarWidget
  }
}


## change the name of Variables.R to Attributes.R

AttrNamesUpdate <- function(Widget=.rqda$.AttrNamesWidget, sortByTime = FALSE, decreasing = FALSE, ...)
{
  if (is_projOpen()) {
    attr <- rqda_sel(
                       "select name, date from attributes where status = 1")
    if (nrow(attr) == 0) {
      attr <- NULL
    } else {
      attr <- attr$name
      Encoding(attr) <- "UTF-8"
      if (!sortByTime) {attr <- sort(attr)} else {
        attr <- attr[OrderByTime(attr$date, decreasing = decreasing)]
      }
    }
    tryCatch(Widget[] <- attr, error = function(e) {
})
  }
}

AddAttrNames <- function(name, ...) {
  if (name != "") {
    con <- .rqda$qdacon
    dup <- rqda_sel(sprintf("select name from attributes where name='%s'", name))
    if (nrow(dup) == 0) {
      rqda_exe(sprintf("insert into attributes (name, status, date, owner) values ('%s', %i, %s, %s)",
                             name, 1, shQuote(date()), shQuote(.rqda$owner)))
    }
  }
}

AddAttrButton <- function(label = gettext("Add", domain = "R-RQDA")) {
  AddAttB <- gbutton(label, handler = function(h, ...) {
    AttrName <- ginput(gettext("Enter new Attr Name. ", domain = "R-RQDA"), icon = "info")
    if (!identical(AttrName, character(0)))
    {
    if (!is.na(AttrName)) {
      Encoding(AttrName) <- "UTF-8"
      invalid <- grepl("'", AttrName)
      if (invalid) {
        gmessage(gettext("Attribute should NOT contain '.", domain = "R-RQDA"), container = TRUE)
      } else {
        if (AttrName %in% c("fileID", "caseID")) {
          gmessage(gettext("This is a reserved keyword.", domain = "R-RQDA"), container = TRUE)
        } else{
          AddAttrNames(AttrName)
          AttrNamesUpdate()
        }
      }
    }
    }
  }
                     )
  assign("AddAttB", AddAttB, envir = button)
  enabled(AddAttB) <- FALSE
  AddAttB
}


DeleteAttrButton <- function(label = rqda_txt("Delete")) {
  DelAttB <- gbutton(label, handler = function(h, ...) {

    del <- gconfirm(
      rqda_txt("Really delete the Attribute?"),
      icon = "question")

    if (isTRUE(del)) {
      sel <- svalue(.rqda$.AttrNamesWidget)
      sel <- enc(sel, "UTF-8")
      rqda_exe(sprintf("update attributes set status = 0 where name='%s'", sel))

      found <- rqda_sel(
        sprintf("select * from caseAttr where variable = '%s' ", sel))

      if (nrow(found) > 0)
        rqda_exe(
          sprintf("update caseAttr set status = 0 where variable='%s'", sel))

      found <- rqda_sel(
        sprintf("select * from fileAttr where variable = '%s' ", sel))


      if (nrow(found) > 0)
        rqda_exe(sprintf("update fileAttr set status = 0 where variable='%s'",
                         sel))

      AttrNamesUpdate()
    }
  })

  assign("DelAttB", DelAttB, envir = button)
  enabled(DelAttB) <- FALSE
  DelAttB
}


RenameAttrButton <- function(label = gettext("Rename", domain = "R-RQDA")) {
  RenAttB <- gbutton(label, handler = function(h, ...) {
    selected <- svalue(.rqda$.AttrNamesWidget)
    NewName <- ginput(gettext("Enter new attribute name. ", domain = "R-RQDA"), text = selected, icon = "info")

    if (!identical(NewName, character(0)))
    {
    if (!is.na(NewName)) {
      Encoding(NewName) <- "UTF-8"
      selected <- enc(selected, encoding = "UTF-8")
      invalid <- grepl("'", NewName)
      if (invalid) {
        gmessage(gettext("Attribute should NOT contain '.", domain = "R-RQDA"), container = TRUE)
      } else {
        exists <- rqda_sel(sprintf("select * from attributes where name = '%s' ", NewName))
        if (nrow(exists) > 0) {
          gmessage(gettext("Name duplicated. Please use another name.", domain = "R-RQDA"), cont = TRUE)
        } else {
          rqda_exe(sprintf("update attributes set name = '%s' where name = '%s' ", NewName, selected))
          rqda_exe(sprintf("update caseAttr set variable = '%s' where variable = '%s' ", NewName, selected))
          rqda_exe(sprintf("update fileAttr set variable = '%s' where variable = '%s' ", NewName, selected))
          AttrNamesUpdate()
        }
      }
    }
    }
  }
                     )
  assign("RenAttB", RenAttB, envir = button)
  enabled(RenAttB) <- FALSE
  RenAttB
}

AttrMemoButton <- function(label = gettext("Memo", domain = "R-RQDA")) {
  AttMemB <- gbutton(label, handler = function(h, ...) {
    MemoWidget(gettext("Attributes", domain = "R-RQDA"), .rqda$.AttrNamesWidget, "attributes")
  }
                     )
  assign("AttMemB", AttMemB, envir = button)
  enabled(AttMemB) <- FALSE
  AttMemB
}

viewCaseAttr <- function() {
  DF <- rqda_sel("select variable, value, caseId from caseAttr where status = 1")
  DF <- reshape(DF, v.names = "value", idvar = "caseID", direction = "wide", timevar = "variable")
  names(DF) <- gsub("^value.", "", names(DF))
  caseName <- rqda_sel("select name, id from cases where status = 1")
  if (nrow(caseName) != 0) {
    names(caseName) <- c("case", "caseID")
    Encoding(caseName$case) <- "UTF-8"
    DF <- merge(caseName, DF)
    gtable(DF, container = TRUE)
  }
}

viewFileAttr <- function() {
  DF <- rqda_sel("select variable, value, fileId from fileAttr where status = 1")
  DF <- reshape(DF, v.names = "value", idvar = "fileID", direction = "wide", timevar = "variable")
  names(DF) <- gsub("^value.", "", names(DF))
  fileName <- rqda_sel("select name, id from source where status = 1")
  if (nrow(fileName) != 0) {
    names(fileName) <- c("file", "fileID")
    Encoding(fileName$file) <- "UTF-8"
    DF <- merge(fileName, DF)
    gtable(DF, container = TRUE)
  }
}


#' @aliases getAttr showSubset
#' @title attributes
#' @description Get the attributes of case or file.
#' @usage
#' getAttr(type = c("case", "file"),
#'         attrs = svalue(.rqda$.AttrNamesWidget),
#'         subset)
#' showSubset(x, ...)
#'
#' @param type Type of attributes.
#' @param attrs character vector, subset of attributes to retrieve.
#' @param subset when subset is not missing, return subset only.
#' @param x an object from \code{getAttr}
#' @param ... Not used currently.
#'
#' @details
#' You can add and modify the attributes of cases or files.
#' \code{getAttr} returns this attributes as a data frame.
#'
#' Sometimes, you only want to show a subset of files or cases according to
#' their attributes. You can do the subset operation of the result from
#' \code{getAttr} and pass it to \code{showSubset}, or you can pass a
#' subset argument to \code{GetAttr}. The meaning of subset is the same as
#' that in \code{subset} function.
#'
#' @return
#' For \code{getAttr}, when type is "case", it is a data frame with class
#' of "CaseAttr"; when type is "file", it is a data frame with class of
#' "FileAttr". For \code{showSubset}, no value is returned, the
#' side-effect is to change the file list or case list in the respective
#' widget.
#'
#' @note
#' All the variables in the data frame is of class "character", you need to
#' convert to suitable class when conducting statistical analysis.
#'
#' @examples
#' \dontrun{
#'   attr <- getAttr("case")
#'   ## assuming there is a variable named atttribute1 in attr.
#'   showSubset(subset(attr, attribute1 == 1))
#' }
#' @export
getAttr <- function(type = c("case", "file"), attrs = svalue(.rqda$.AttrNamesWidget), subset) {
  if (is_projOpen()) {
  type <-  match.arg(type)
  if (length(attrs) == 0) attrs <- NULL
  inClause <- ifelse(is.null(attrs), "", sprintf("where status = 1 and variable in (%s)", paste(shQuote(attrs), collapse = ", ")))
  if (type == "case") {
    rqda_exe("delete from caseAttr where value='NA'")
    rqda_exe("delete from caseAttr where value=''") ## clean the table
    DF <- rqda_sel(sprintf("select variable, value, caseId from caseAttr %s", inClause))
    if (nrow(DF) > 0) {
    Encoding(DF$variable) <- Encoding(DF$value) <- "UTF-8"
    DF <- reshape(DF, v.names = "value", idvar = "caseID", direction = "wide", timevar = "variable")
    names(DF) <- gsub("^value.", "", names(DF))
    caseName <- rqda_sel("select name, id from cases where status = 1")
    if (nrow(caseName) != 0) {
      names(caseName) <- c("case", "caseID")
      Encoding(caseName$case) <- "UTF-8"
      DF <- merge(caseName, DF)
      class(DF) <- c("CaseAttr", "data.frame")
    }}
  } else if (type == "file") {
    rqda_exe("delete from fileAttr where value='NA'")
    rqda_exe("delete from fileAttr where value=''") ## clean the table
    DF <- rqda_sel(sprintf("select variable, value, fileId from fileAttr %s", inClause))
    if (nrow(DF) > 0) {
    Encoding(DF$variable) <- Encoding(DF$value) <- "UTF-8"
    DF <- reshape(DF, v.names = "value", idvar = "fileID", direction = "wide", timevar = "variable")
    names(DF) <- gsub("^value.", "", names(DF))
    fileName <- rqda_sel("select name, id from source where status = 1")
    if (nrow(fileName) != 0) {
      names(fileName) <- c("file", "fileID")
      Encoding(fileName$file) <- "UTF-8"
      DF <- merge(fileName, DF)
      class(DF) <- c("FileAttr", "data.frame")
    }}
  }
  tt <- rqda_sel("select name, class from attributes")
  attrs <- tt[tt$class == "numeric", "name"]
  idx <- which(names(DF) %in% attrs)
  DF[, idx]<-as.data.frame(apply(DF[, idx, drop = FALSE], 2, as.numeric))
  if (missing(subset)) DF else {
      r <- eval(substitute(subset), DF)
      if (!is.logical(r))
          stop("'subset' must evaluate to logical", domain = "R-RQDA")
      r <- r & !is.na(r)
      DF <- DF[r, , drop = FALSE]
      DF
  }
}}

SetAttrClsButton <- function(label = gettext("Class", domain = "R-RQDA")) {
  ans <- gbutton(label, handler = function(h, ...) {
    setAttrType()
  }
                 )
  gtkWidgetSetTooltipText(getToolkitWidget(ans), gettext("Set class of selected attribute.\nIt can be 'numeric' or 'character'.", domain = "R-RQDA"))
  assign("SetAttClsB", ans, envir = button)
  enabled(ans) <- FALSE
  ans
}


setAttrType <- function() {
    Selected <- enc(svalue(.rqda$.AttrNamesWidget), encoding = "UTF-8")
    oldCls <- tryCatch(rqda_sel(sprintf("select class from attributes where status = 1 and name='%s'", Selected))[1, 1],
                       error = function(e) {
                         rqda_exe("alter table attributes add column class text")
                         rqda_sel(sprintf("select class from attributes where status = 1 and name='%s'", Selected))[1, 1]
                       })
    if (is.null(oldCls)||is.na(oldCls)) {
      items <- c("unspecified", "numeric", "character")
      idx <- 1
    } else {
      items <- c("numeric", "character")
      idx <- which (items %in%  oldCls)
    }
    w <- gwindow(gettext("Type of attribute", domain = "R-RQDA"), width = getOption("widgetSize")[1], height = getOption("widgetSize")[2])
    addHandlerKeystroke(w, function(h, ...) {
      if (h$key == "\027") dispose(w)
    })
    gp <- ggroup(horizontal = FALSE, container = w)
    rb <- gradio(items, idx, horizontal = TRUE, container = gp)
    gbutton("OK", container = gp, handler = function(h, ...) {
      if ((newCls <- svalue(rb)) !=  "unspecified") {
        rqda_exe(sprintf("update attributes set class='%s' where status = 1 and name='%s'", newCls, Selected))
      }
      dispose(w)
    })}


importAttr <- function(data, type='file', filename) {
  idx <- match(filename, names(data))
  dat <- data[, -idx, drop = FALSE]
  fn <- getFiles()
  fid <- getFiles(names = F)
  fnuser <- data[, filename]
  if (!all(fnuser %in% fn)) stop("some files are not in the rqda project.", domain = "R-RQDA")
  fid <- fid[match(data[[filename]], fn)]
  allAtt <- rqda_sel("select name from attributes where status = 1")$name
  if (!all(names(dat) %in% allAtt)) stop("some attributes are in not the rqda project.", domain = "R-RQDA")
  for (att in names(dat)) {
    attval <- dat[[att]]
    if (mode(attval) == "character" && Encoding(attval) != "UTF-8") attval <- iconv(attval, to='UTF-8')
    for (i in 1:nrow(dat)) {
      exist <- rqda_sel(sprintf("select value from fileAttr where variable='%s' and fileID=%i and status = 1", att, fid[i]))
      if (nrow(exist) == 0 && !is.na(attval[i])) {
        rqda_exe(sprintf("insert into fileAttr (variable, value, fileID, date, owner, status)
                          values ('%s', '%s', '%s', '%s', 'rghuang', 1)", att, attval[i], fid[i], as.character(date())))
        }
    }
    }
}
