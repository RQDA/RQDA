addSettingGUI <- function(container,width=12){
  colorsList <- colors()
  if (Sys.info()["user"]!="") assign("owner",Sys.info()["user"],envir=.rqda)
  
  
  container <- ggroup(horizontal = FALSE, container = container)
  
  ans <- gbutton(gettext("Click to set font", domain = "R-RQDA"),
                container = container,
                handler=function(h,...)
                  setFont(default=.rqda$font))
  ## set font for widget
  gtkWidgetSetTooltipText(getToolkitWidget(ans),
                          gettext("Set fonts for memo widgets.", domain = "R-RQDA"))

  SettingFL <- gformlayout(align = "default", spacing = 5,
                           container = container, expand=TRUE)

  gedit(name = "owner",
        label = gettext("Name of Coder", domain = "R-RQDA"),
        text = .rqda$owner, container = SettingFL
  )
  gedit(name = "encoding",
        label = gettext("File Encoding", domain = "R-RQDA"),
        text = .rqda$encoding, container = SettingFL
  )
  gcombobox(name = "fore.col",
            label = gettext("Color for Coding", domain = "R-RQDA"),
            ## type = "gedit",width=width,
            ## text = .rqda$fore.col
            items=c(.rqda$fore.col,colorsList),
            container = SettingFL
  )
  gcombobox(name = "back.col",
            label = gettext("Color for Case", domain = "R-RQDA"),
            ## type = "gedit",width=width,
            ## text = .rqda$back.col
            items=c(.rqda$back.col,colorsList),
            container = SettingFL
  )
  gcombobox(name = "codingTable",
            label = gettext("Current coding table", domain = "R-RQDA"),
            items=c(.rqda$codingTable,"coding2"),
            container = SettingFL
  )
  gcombobox(name = "BOM",
            label = gettext("Byte Order Mark", domain = "R-RQDA"),
            items = c(FALSE, TRUE),
            container = SettingFL
  )
  gcombobox(name = "SFP",
            label = gettext("Show File Property", domain = "R-RQDA"),
            items = c(FALSE, TRUE),
            container = SettingFL
  )
  gcombobox(name = "TOR",
            label = gettext("Type of Retrieval", domain = "R-RQDA"),
            items = c(gettext("unconditional", domain = "R-RQDA"),
                      gettext("case", domain = "R-RQDA"),
                      gettext("filecategory", domain = "R-RQDA"),
                      gettext("both", domain = "R-RQDA")),
            container = SettingFL
  )
  
  ButtonContainer <- ggroup(container = container) ##, width=100) ## not necessary to set width here
  addSpring(ButtonContainer)
  resetButton <- gbutton(gettext("Default", domain = "R-RQDA"),
                         container = ButtonContainer)
  okButton <- gbutton(gettext("OK", domain = "R-RQDA"),
                      container = ButtonContainer)
  
  addHandlerChanged(okButton, function(h,...) {
    out <- svalue(SettingFL)
    ## Untranslate Type Of Retrieve:
    if(out["TOR"] == gettext("unconditional", domain = "R-RQDA"))
      out["TOR"] <- "unconditional"
    else if(out["TOR"] == gettext("case", domain = "R-RQDA"))
      out["TOR"] <- "case"
    else if(out["TOR"] == gettext("filecategory", domain = "R-RQDA"))
      out["TOR"] <- "filecategory"
    else if(out["TOR"] == gettext("both", domain = "R-RQDA"))
      out["TOR"] <- "both"
    
    tryCatch(ClearMark(.rqda$.root_edit,0,nchar(svalue(.rqda$.openfile_gui)),TRUE,TRUE),error=function(e){})
    for (i in names(out)) assign(i,out[[i]],envir=.rqda)
  })
  
  addHandlerChanged(resetButton, function(h,...) {
    tryCatch(ClearMark(.rqda$.root_edit,0,
                       nchar(svalue(.rqda$.openfile_gui)),TRUE,TRUE),
             error=function(e){})
    tryCatch(svalue(SettingFL[]$BOM) <- FALSE,error=function(e){})
    tryCatch(svalue(SettingFL[]$SFP) <- FALSE,error=function(e){})
    tryCatch(svalue(SettingFL[]$encoding) <- "unknown",error=function(e){})
    tryCatch(svalue(SettingFL[]$owner) <- "default",error=function(e){})
    tryCatch(svalue(SettingFL[]$back.col) <- "gold",error=function(e){})
    tryCatch(svalue(SettingFL[]$fore.col) <- "blue",error=function(e){})
    tryCatch(svalue(SettingFL[]$codingTable) <- "coding",error=function(e){})
    tryCatch(svalue(SettingFL[]$TOR) <- gettext("unconditional",
                                                domain = "R-RQDA"),
             error=function(e){})
    assign("BOM",FALSE,envir=.rqda)
    assign("SFP",FALSE,envir=.rqda)
    assign("encoding","unknown",envir=.rqda)
    assign("owner","default",envir=.rqda)
    assign("back.col","gold",envir=.rqda)
    assign("fore.col","blue",envir=.rqda)
    assign("codingTable","coding",envir=.rqda)
    assign("TOR", "unconditional",envir=.rqda)
    assign("font","Sans 11",envir=.rqda)
    
  })}

setFont <- function(default="Sans 11"){
  font <- gtkFontButtonNew()
  gtkFontButtonSetFontName(font,default)
  g <-glayout(container=gwindow(width=(gdkScreenWidth()/10),
                                height=(gdkScreenHeight()/10),
                                parent=getOption("widgetCoordinate")),
              homogeneous=TRUE)
  g[1,1:2] <- font
  g[2,1] <- gbutton("Ok",handler=function(h,...){
    ans <- font$GetFontName()
    assign("font",ans, envir=.rqda)
    dispose(g)
  })
  g[2,2] <- gbutton("Cancel",handler=function(h,...) dispose(g))
}
