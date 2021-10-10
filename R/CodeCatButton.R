### UpdateTableWidget() and AddTodbTable() are general versions of the previous
### functions
UpdateTableWidget <- function(Widget, FromdbTable, con = .rqda$qdacon,
                              sortByTime = FALSE, decreasing = FALSE, ...)
{
  if (is_projOpen()) {
    items <- rqda_sel(sprintf("select name,date from %s where status=1",
                              FromdbTable))
    if (nrow(items) != 0) {
      Encoding(items$name) <- "UTF-8"
      if (!sortByTime) {
        items <- sort(items$name, decreasing = decreasing)
      } else {
        items <- items$name[OrderByTime(items$date, decreasing = decreasing)]
      }
    } else {
      items <- NULL
    }
    tryCatch(eval(substitute(W[] <- items,
                             list(W = quote(Widget)))),
             error=function(e) {})
  }
}


AddTodbTable <- function(item,dbTable,Id="id",field="name",con=.rqda$qdacon,...) {
    ## now handles ' in item
    if (item != "") {
        maxid <- rqda_sel(sprintf("select max(%s) from %s",Id, dbTable))[[1]]
        nextid <- ifelse(is.na(maxid),0+1, maxid+1)
        write <- FALSE
        if (nextid == 1) {
            write <- TRUE
        } else {
            dup <- rqda_sel(sprintf("select %s from %s where name='%s'",field, dbTable, enc(item)))
            if (nrow(dup) == 0) write <- TRUE
        }
        if (write ) {
            rqda_exe(sprintf("insert into %s (%s, %s, status,date,owner)
                                            values ('%s', %i, %i,%s, %s)",dbTable,field,Id,
                                   enc(item),nextid, 1, shQuote(date()),shQuote(.rqda$owner)))
        }
    }
}


#################
AddCodeCatButton <- function(label = rqda_txt("Add")) {
  AddCodCatB <- gbutton(label, handler = function(h, ...) {
    item <- ginput(rqda_txt("Enter new Code Category. "), icon="info")
    if (!identical(item, character(0))) {
      Encoding(item) <- "UTF-8"
      AddTodbTable(item, "codecat", Id = "catid") ## CODE CATegory
      UpdateTableWidget(Widget = .rqda$.CodeCatWidget, FromdbTable = "codecat")
    }
  })
  assign("AddCodCatB", AddCodCatB, envir = button)
  enabled(AddCodCatB) <- FALSE
  AddCodCatB
}



DeleteCodeCatButton <- function(label = rqda_txt("Delete"))
{

  DelCodCatB <- gbutton(label, handler=function(h, ...) {

    Selected <- svalue(.rqda$.CodeCatWidget)
    Encoding(Selected) <- "UTF-8"

    if (identical (Selected, character(0))) {
      gmessage(rqda_txt("Select a Code Category first."),
               icon = "error", container = TRUE)
      return(invisible(NULL))
    }

    del <- gconfirm(rqda_txt("Really delete the Code Category?"),
                    icon="question")

    if (isTRUE(del)) {
      catid <- rqda_sel(
        sprintf("select catid from codecat where status=1 and name='%s'",
                enc(Selected)))[ , 1]
      if (length(catid) == 1 ) {
        rqda_exe(
          sprintf("update codecat set status=0 where name='%s'",
                  enc(Selected)))
        ## set status in table freecode to 0
        UpdateTableWidget(Widget = .rqda$.CodeCatWidget,
                          FromdbTable = "codecat")
        tryCatch(
          rqda_exe(
            sprintf("update treecode set status=0 where catid='%s'",
                    catid)),
          error=function(e) {})

        ## should delete all the related codelists
        UpdateCodeofCatWidget() ## update the code of cat widget
      } else {
        gmessage(rqda_txt("The Category Name is not unique."),
                 container = TRUE)
      }

    }
  }
  )
  assign("DelCodCatB", DelCodCatB, envir = button)
  enabled(DelCodCatB) <- FALSE
  DelCodCatB
}


CodeCat_RenameButton <- function(label=gettext("Rename", domain = "R-RQDA"),Widget=.rqda$.CodeCatWidget,...)
{
  ## rename of selected code cat.
  CodCatRenB <- gbutton(label,handler=function(h,...) {
      OldName <- svalue(Widget)
      if (length(OldName) == 0) {
          gmessage(gettext("Select a Code Category first.", domain = "R-RQDA"),icon="error",container=TRUE)
      }
      else {
          ## get the new file names
          NewName <- ginput(gettext("Enter new Category name. ", domain = "R-RQDA"), text=OldName, icon="info")

          if (!identical(NewName, character(0)))
          {
          if (!is.na(NewName)) {
              Encoding(NewName) <- "UTF-8"
              rename(OldName,NewName,"codecat")
              UpdateTableWidget(Widget=.rqda$.CodeCatWidget,FromdbTable="codecat")
          }
          }
      }
  })
  assign("CodCatRenB",CodCatRenB,envir=button)
  enabled(CodCatRenB) <- FALSE
  CodCatRenB
}

UpdateCodeofCatWidget <- function(con=.rqda$qdacon,Widget=.rqda$.CodeofCat,sort=TRUE)
{
  SelectedCodeCat <- svalue(.rqda$.CodeCatWidget)
  # print(SelectedCodeCat)
  # print(length(SelectedCodeCat))

  if (length(SelectedCodeCat) != 0) {
    ## if code cat is selected, then continue
    Encoding(SelectedCodeCat) <- "UTF-8"
    catid <- rqda_sel(
      sprintf("select catid from codecat where status=1 and name='%s'",
              enc(SelectedCodeCat)))[ , 1]

    Total_cid <- rqda_sel(
      sprintf("select cid from treecode where status=1 and catid=%i",
              catid))

    if (nrow(Total_cid) != 0) {
      items <- rqda_sel("select name,id,date from freecode where status=1")

      if (nrow(items) != 0) {
        items <- items[items$id %in% Total_cid$cid, c("name","date")]
        items <- items$name[OrderByTime(items$date)] ## sort accoding to date

        Encoding(items) <- "UTF-8"
        if (sort)
          items <- sort(items)

      } else {
        items <- NULL
      }
    } else {
      items <- NULL
    }
  } else {
    items <- NULL
  }

  tryCatch(Widget[] <- items, error=function(e) {})
}

CodeCatAddToButton <- function(label = rqda_txt("Add To"),
                               Widget = .rqda$.CodeCatWidget, ...) {

  ans <- gbutton(label, handler = function(h, ...) {

    SelectedCodeCat <- svalue(.rqda$.CodeCatWidget)
    if (identical (SelectedCodeCat, character(0))) {
      gmessage(rqda_txt("Select a Code Category first."),
               icon = "error", container = TRUE)
      return(invisible(NULL))
    }

    catid <- rqda_sel(
      sprintf("select catid from codecat where status=1 and name='%s'",
              enc(SelectedCodeCat)))[ , 1]

    freecode <-  rqda_sel("select name, id from freecode where status=1")

    if (nrow(freecode) == 0) {
      gmessage(rqda_txt("No free codes yet."),
               cont = .rqda$.CodeCatWidget)

    } else {
      Encoding(SelectedCodeCat) <- Encoding(freecode[['name']]) <- "UTF-8"

      codeofcat <- rqda_sel(
        sprintf("select cid from treecode where status=1 and catid=%i",
                catid))

      if (nrow(codeofcat) != 0) {

        codeoutofcat <- subset(freecode,
                               !(freecode$id %in% codeofcat$cid))
      } else {
        codeoutofcat <- freecode
      }

      Selected <- gselect.list(codeoutofcat[['name']],
                               multiple = TRUE,
                               x = getOption("widgetCoordinate")[1])

      if (length(Selected) >1 || Selected != "") {

        cid <- codeoutofcat[codeoutofcat$name %in% Selected,"id"]
        Dat <- data.frame(cid = cid,
                          catid = catid,
                          date = date(),
                          dateM = date(),
                          memo = "",
                          status = 1,
                          owner = .rqda$owner)

        ## Push selected codeList to table treecode
        rqda_wrt("treecode", Dat)

        UpdateCodeofCatWidget()
      }
    }})
  gtkWidgetSetTooltipText(
    getToolkitWidget(ans),
    rqda_txt("Add code(s) to the selected code category."))

  assign("CodCatAddToB",ans, envir=button)
  enabled(ans) <- FALSE
  return(ans)
}

  ## update .rqda$.CodeofCat[] by click handler on .rqda$.CodeCatWidget

CodeCatDropFromButton <- function(label=gettext("Drop From", domain = "R-RQDA"),Widget=.rqda$.CodeofCat,...)
{
    ans <- gbutton(label,handler=function(h,...) {
        ## Get CodeList already in the category (table treecode): svalue()
        CodeOfCat <- svalue(Widget)
        if ((NumofSelected <- length(CodeOfCat)) == 0) {
            gmessage(gettext("Please select the Codes you want to delete.", domain = "R-RQDA"),container=TRUE)
        } else {
            ## Give a confirm msg
            del <- gconfirm(sprintf(gettext("Delete %i code(s) from this category. Are you sure?", domain = "R-RQDA"),NumofSelected),container=TRUE,icon="question")
            if (isTRUE(del)) {
                ## set status == 0 for those selected CodeList (table treecode)
                SelectedCodeCat <- svalue(.rqda$.CodeCatWidget)
                ## Encoding(SelectedCodeCat) <- "UTF-8"
                catid <- rqda_sel(sprintf("select catid from codecat where status=1 and name='%s'",enc(SelectedCodeCat)))[,1]
                for (i in CodeOfCat) {
                    cid <- rqda_sel(sprintf("select id from freecode where status=1 and name='%s'",enc(i)))[,1]
                    rqda_exe(sprintf("update treecode set status=0 where catid=%i and cid=%i",catid,cid))
                }
                ## update .CodeofCat Widget
                UpdateCodeofCatWidget()
            }
        }
    }
                   )
    gtkWidgetSetTooltipText(getToolkitWidget(ans),gettext("Drop selected code(s) from code category.", domain = "R-RQDA"))
    assign("CodCatADroFromB",ans, envir=button)
    enabled(ans) <- FALSE
    return(ans)
}

CodeCatMemoButton <- function(label=gettext("Memo", domain = "R-RQDA"),...) {
    CodCatMemB <- gbutton(label,handler=function(h,...) {
        MemoWidget(
          gettext("Code Category", domain = "R-RQDA"),
          .rqda$.CodeCatWidget,
          "codecat")
        }
                          )
    assign("CodCatMemB", CodCatMemB,envir=button)
    enabled(CodCatMemB) <- FALSE
    CodCatMemB
}
## MemoWidget() is moved to utils.R

plotCodeCategory <-function(parent=NULL) {
  if (is.null(parent))
    parent <- svalue(.rqda$.CodeCatWidget)
  ans <- rqda_sel(
  sprintf("select
            codecat.name as parent,
            freecode.name as child from treecode,
            codecat,freecode
            where treecode.status=1 and
                  codecat.status=1 and
                  freecode.status=1 and
                  treecode.catid = codecat.catid and
                  freecode.id = treecode.cid and
                  codecat.name in (%s)",
          paste(shQuote(parent), collapse=",")))

  Encoding(ans$parent) <- "UTF-8"
  Encoding(ans$child) <- "UTF-8"

  g <- igraph::graph.data.frame(ans)
  tryCatch(
    igraph::tkplot(g, vertex.label = igraph::V(g)$name), error=function(e) {
    igraph::plot.igraph(g,vertex.label=igraph::V(g)$name)
  })
}

d3CodeCategory <-function(parent=NULL) {
  if (is.null(parent)) parent <- svalue(.rqda$.CodeCatWidget)
  ans <- rqda_sel(sprintf("select codecat.name as parent,freecode.name as child from treecode, codecat,freecode
where treecode.status=1 and codecat.status=1 and freecode.status=1
and treecode.catid=codecat.catid and freecode.id=treecode.cid and codecat.name in (%s)",paste(shQuote(parent),collapse=",")))
  Encoding(ans$parent) <- "UTF-8"
  Encoding(ans$child) <- "UTF-8"
  file = paste(tempfile(), "html", sep=".")
  d3Network::d3SimpleNetwork(ans, width = gdkScreenWidth(), height = gdkScreenHeight(), file=file(file, encoding="UTF-8"))
  browseURL(file)
}

getCodingsByCategory <- function(catid=NULL, fid = NULL, codingTable = c("coding", "coding2")) {
    if (is.null(catid)) catid <- rqda_sel(sprintf("select catid from codecat where name = '%s'", enc(svalue(.rqda$.CodeCatWidget))))$catid
    cid <- rqda_sel(sprintf("select cid from treecode where catid == %s and status == 1",catid))$cid
    codingTable <- match.arg(codingTable)
    if (codingTable == "coding") {
        ct <- rqda_sel(sprintf("select coding.rowid as rowid, coding.cid, coding.fid,
freecode.name as codename, source.name as filename, coding.selfirst as index1,
coding.selend as index2, coding.seltext as coding, coding.selend - coding.selfirst as CodingLength from coding
 left join freecode on (coding.cid=freecode.id) left join source on (coding.fid=source.id)
where coding.status=1 and source.status=1 and freecode.status=1 and coding.cid in (%s)", paste(cid,collapse=",")))
    }
    if (codingTable == "coding2") {
        ct <- rqda_sel(sprintf("select coding.rowid as rowid, coding.cid, coding.fid,
freecode.name as codename, source.name as filename, coding2.selfirst as index1,
coding2.selend as index2, coding2.seltext as coding2, coding2.selend - coding2.selfirst as CodingLength from coding2
 left join freecode on (coding2.cid=freecode.id) left join source on (coding2.fid=source.id)
where coding2.status=1 and source.status=1 and freecode.status=1 and coding2.cid in (%s)", paste(cid,collapse=",")))
    }
    if (nrow(ct) != 0) {
        Encoding(ct$codename) <- Encoding(ct$filename) <- Encoding(ct$coding) <- "UTF-8"
        if (!is.null(fid))
            ct <- ct[ct$fid %in% fid, ]
    }
    ## class(ct) <- c("codingsByOne", "data.frame")
    ct
}
GetCodeCatWidgetMenu <- function()
{
  CodeCatWidgetMenu <- list()

  CodeCatWidgetMenu[[1]] <- gaction(rqda_txt("Add New Code to Selected Category"), handler = function(h, ...) {
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
      codename <- ginput(rqda_txt("Enter new code."), icon="info")
      if (!identical(codename, character(0)))
      {
      if (!is.na(codename)) {
        codename <- enc(codename,encoding="UTF-8")
        addcode(codename)
        CodeNamesUpdate(sortByTime=FALSE)
        cid <- rqda_sel(sprintf("select id from freecode where status=1 and name='%s'",codename))$id
        ## end of add a new code to free code.
        SelectedCodeCat <- svalue(.rqda$.CodeCatWidget)
        if (length(SelectedCodeCat) == 0) {gmessage(rqda_txt("Select a code category first."),container=TRUE)} else{
          catid <- rqda_sel(sprintf("select catid from codecat where status=1 and name='%s'",SelectedCodeCat))[,1]
          ## CodeList and the id (table freecode): sql -> name and id where status == 1
          Dat <- data.frame(cid=cid,catid=catid,date=date(),dateM=date(),memo="",
                            status=1, owner=.rqda$owner)
          ## Push selected codeList to table treecode
          ok <- rqda_wrt("treecode", Dat)
          if (ok) {
            ## update .CodeofCat Widget
            UpdateCodeofCatWidget()
          } else gmessage(rqda_txt("Failed to assign code category"))
        }
      }
      }
    }
  })

  CodeCatWidgetMenu[[2]] <- gaction(rqda_txt("Codings of selected category"), handler = function(h, ...) {
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
      ct <- getCodingsByCategory(fid=getFileIds(condition=.rqda$TOR))
      print.codingsByOne(ct)
    }
  })

  CodeCatWidgetMenu[[3]] <- gaction(rqda_txt("Memo"), handler = function(h, ...) {
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
      MemoWidget("Code Category",.rqda$.CodeCatWidget,"codecat")
    }
  })

  CodeCatWidgetMenu[[4]] <- gaction(rqda_txt("Plot Selected Code Categories"), handler = function(h, ...) {
    plotCodeCategory()
  })

  CodeCatWidgetMenu[[5]] <- gaction(rqda_txt("Plot Selected Code Categories with d3"), handler = function(h, ...) {
    d3CodeCategory()
  })

  CodeCatWidgetMenu[[6]] <- gaction(rqda_txt("Sort by created time"), handler = function(h, ...) {
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
      UpdateTableWidget(Widget=.rqda$.CodeCatWidget,FromdbTable="codecat")
      ## UpdateCodeofCatWidget() ## wrong function
    }
  })

  CodeCatWidgetMenu
}


##
GetCodeofCatWidgetMenu <- function()
{
  CodeofCatWidgetMenu <- list()

  CodeofCatWidgetMenu[[1]] <- gaction(gettext("Rename Selected Code", domain = "R-RQDA"), handler = function(h, ...) {
    selectedCodeName <- svalue(.rqda$.CodeofCat)
    if (length(selectedCodeName) == 0) {
      gmessage(gettext("Select a code first.", domain = "R-RQDA"),icon="error",container=TRUE)
    }
    else {
      NewCodeName <- ginput(gettext("Enter new code name. ", domain = "R-RQDA"), text=selectedCodeName, icon="info")

    if (!identical(NewCodeName, character(0)))
    {
      if (!is.na(NewCodeName)) {
        Encoding(NewCodeName) <- Encoding(selectedCodeName) <- "UTF-8"
        rename(selectedCodeName,NewCodeName,"freecode")
        UpdateWidget(".codes_rqda",from=selectedCodeName,to=NewCodeName)
        UpdateWidget(".CodeofCat",from=selectedCodeName,to=NewCodeName)
      }
    }
    }
  })

  CodeofCatWidgetMenu[[2]] <- gaction(gettext("Code Memo", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
      MemoWidget(gettext("Code", domain = "R-RQDA"),.rqda$.CodeofCat,"freecode")
    }
  })

  CodeofCatWidgetMenu[[3]] <- gaction(gettext("Sort by created time", domain = "R-RQDA"), handler = function(h, ...) {
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
      UpdateCodeofCatWidget()
    }
  })

  CodeofCatWidgetMenu
}
