## part of RQDA project
## by ronggui HUANG


.onAttach <- function(...) {
    ## use .onLoad/.onAttach rather than .First.lib when there is namespace
    ## Refer R news 2003-1 for details about name space

    optOld <- options()

    options(andMethod = c("overlap", "exact", "inclusion"))
    assign("optOld", optOld, envir = .rqda)
    if (interactive()) {
        packageStartupMessage(
            "\nUse 'RQDA()' to start the programme.\n", domain = "R-RQDA")
        RQDA()
    }
}

.onUnload <- function(...) {
    cat("Bye, RQDA is unloaded.\n")
    options(.rqda$optOld)
}
