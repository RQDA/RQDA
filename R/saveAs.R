saveAsButt <- function(label = rqda_txt("Save Project As ..."),
                       container) {

    saveAsB <- gbutton(text = label, container = container,
                       handler = function(h, ...) {
                           saveAs()
                       })

    enabled(saveAsB) <- FALSE
    assign("saveAsB", saveAsB, envir = button)
}

saveAs <- function(newpath = NULL) {
    oldpath <- dbGetInfo(.rqda$qdacon)$dbname

    if (Encoding(oldpath) == "unknown")
        Encoding(oldpath) <- "UTF-8"

    if (is.null(newpath)) {
        newpath <- gfile(type = "save",
                         text = rqda_txt("Type a new file name and click OK."),
                         filter = list("RQDA" = list(patterns = c("*.rqda$"))))

                                        # if no file selected, return nothing
        if (identical(newpath, character(0)))
            return(0)

        if (Encoding(newpath) != "UTF-8")
            Encoding(newpath) <- "UTF-8"

        newpath <- sprintf("%s.rqda", newpath)
    }

    override <- TRUE
    if (fexist <- file.exists(newpath)) {
        override <- gconfirm(rqda_txt("Overwrite existing project?"),
                             icon = "warning")
        if (file.access(newpath, 2) != 0 && override) {
            override <- FALSE
            gmessage(rqda_txt("You have no write permission to overwrite it."),
                     container = TRUE, icon = "error")
        }
    }

    if (!fexist | override) {
        succeeded <- file.copy(from = oldpath, to = newpath, overwrite = override)
    }

    if (!succeeded)
        gmessage(rqda_txt("Failed to save the project to the new location."),
                 container = TRUE, icon = "error")

    closeProjBF()
    ## this must be placed before closeProject() because .fnames_rqda[] <- NULL
    ## will triger Clicked handler
    closeProject()
    enabled(button$cloprob) <- FALSE
    openProject(path = newpath, updateGUI = TRUE)
}
