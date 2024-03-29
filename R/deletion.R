#' @export
list.deleted <- function(type = c("file", "code", "case", "codecategory", "filecategory")) {
    ## list the tmp deleted file/code/case/codecategory/filecategory
    ## no need to list deleted coding
    if (!is_projOpen()) print("No project is open!")
    else {
        type <- match.arg(type)
        if (type == "file") {
            ans <- rqda_sel("select name from source where status = 0")
        }
        else if (type == "code") {
            ans <- rqda_sel("select name from freecode where status = 0")
        } else if (type == "case") {
            ans <- rqda_sel("select name from cases where status = 0")
        } else if (type == "codecategory") {
            ans <- rqda_sel("select name from codecat where status = 0")
        } else if (type == "filecategory") {
            ans <- rqda_sel("select name from filecat where status = 0")
        }
        if (nrow(ans) == 0) {
            sprintf("No %s is deleted.", type)
        }  else {
            Encoding(ans[, 1]) <- "UTF-8"
            ans
        }
    }
}

#' @export
pdelete <- function(type = c("file", "code", "case", "codecategory", "filecategory", "coding"), ask = FALSE) {
    ## permanantly delete all the "deleted" files/codes/codings (those with status = 0)

    codingFun <- function(ask) {
        ## erase deleted coding by unmark button
        if (ask) {
            del <- gconfirm(gettext("Are you sure to clean the coding table?", domain = "R-RQDA"), icon = "question")
        } else del <- TRUE
        if (del) {
            rqda_exe("delete from coding where status = -1")
            ## this is codings deleted by unmark button
        }
    }

    fileFun <- function(del) {
        ## delete files
        fid <- rqda_sel(sprintf("select id from source where status = 0 AND name in (%s)",
                                paste(paste("'", del, "'", sep = ""), collapse = ", ")))[, 1]
        if (length(fid) != 0) {
            rqda_exe(sprintf("delete from source where status = 0 and id in (%s)",
                             paste(paste(fid, sep = ""), collapse = ", ")))
            ## delete from source table
            rqda_exe(sprintf("delete from coding where status = 0 and fid in (%s)",
                             paste(paste(fid, sep = ""), collapse = ", ")))
            ## delete associated coding
            rqda_exe(sprintf("delete from caselinkage where status = 0 and fid in (%s)",
                             paste(paste(fid, sep = ""), collapse = ", ")))
            ## delete case linkage
            rqda_exe(sprintf("delete from treefile where status = 0 and fid in (%s)",
                             paste(paste(fid, sep = ""), collapse = ", ")))
            ## delete associated file-category
        }
    }

    codeFun <- function(del) {
        cid <- rqda_sel(sprintf("select id from freecode where status = 0 AND name in (%s)",
                                paste(paste("'", del, "'", sep = ""), collapse = ", ")))[, 1]
        if (length(cid) != 0) {
            rqda_exe(sprintf("delete from freecode where status = 0 and id in (%s)",
                             paste(paste(cid, sep = ""), collapse = ", ")))
            ## delete from freecode table
            rqda_exe(sprintf("delete from coding where status = 0 and cid in (%s)",
                             paste(paste(cid, sep = ""), collapse = ", ")))
            ## delete associated coding
        }
    }

    caseFun <- function(del) {
        caseid <- rqda_sel(sprintf("select id from cases where status = 0 AND name in (%s)",
                                   paste(paste("'", del, "'", sep = ""), collapse = ", ")))[, 1]
        if (length(caseid) != 0) {
            rqda_exe(sprintf("delete from cases where status = 0 and id in (%s)",
                             paste(paste(caseid, sep = ""), collapse = ", ")))
            rqda_exe(sprintf("delete from caselinkage where status = 0 and caseid in (%s)",
                             paste(paste(caseid, sep = ""), collapse = ", ")))
            ## caselinkage table
        }
    }

    CcatFun <- function(del) {
        catid <- rqda_sel(sprintf("select catid from codecat where status = 0 AND name in (%s)",
                                  paste(paste("'", del, "'", sep = ""), collapse = ", ")))[, 1]
        if (length(catid) != 0) {
            rqda_exe(sprintf("delete from codecat where status = 0 and catid in (%s)",
                             paste(paste(catid, sep = ""), collapse = ", ")))
            rqda_exe(sprintf("delete from treecode where status = 0 and catid in (%s)",
                             paste(paste(catid, sep = ""), collapse = ", ")))
            ## caselinkage table
        }
    }

    FcatFun <- function(del) {
        catid <- rqda_sel(sprintf("select catid from filecat where status = 0 AND name in (%s)",
                                  paste(paste("'", del, "'", sep = ""), collapse = ", ")))[, 1]
        if (length(catid) != 0) {
            rqda_exe(sprintf("delete from filecat where status = 0 and catid in (%s)",
                             paste(paste(catid, sep = ""), collapse = ", ")))
            rqda_exe(sprintf("delete from treefile where status = 0 and catid in (%s)",
                             paste(paste(catid, sep = ""), collapse = ", ")))
            ## caselinkage table
        }
    }

    ## end of helper functions

    if (!is_projOpen()) {
        print("No project is open!")
    }  else {
        type <- match.arg(type)
        if (type == "coding") {
            codingFun(ask = ask)
        } else {
            del <- list.deleted(type)
            if (!is.data.frame(del)) {
                print("Nothing to clear.")
            } else {
                if (ask) {
                    del <- gselect.list(del[, 1], multiple = TRUE) ##select.list(del[, 1], multiple = TRUE)
                } else {
                    del <- del[, 1]
                }
                del <- enc(del, encoding = "UTF-8")
                switch(type,
                       ## use switch to run Fun
                       file = fileFun(del = del),
                       code = codeFun(del = del),
                       case = caseFun(del = del),
                       codecategory = CcatFun(del = del),
                       filecategory = FcatFun(del = del)
                       )
            }
        }
    }
}

CleanProject <- function(ask = FALSE) {
    pdelete("file", ask = ask)
    pdelete("code", ask = ask)
    pdelete("case", ask = ask)
    pdelete("codecategory", ask = ask)
    pdelete("filecategory", ask = ask)
    pdelete("coding", ask = ask)
}


#' @export
undelete <- function(type = c("file", "code", "case", "codecategory", "filecategory"), ask = TRUE) {
    ## set the status  back to 1

    ## beginning of helper functions
    fileFun <- function(del) {
        ## delete files
        fid <- rqda_sel(sprintf("select id from source where status = 0 AND name in (%s)",
                                paste(paste("'", del, "'", sep = ""), collapse = ", ")))[, 1]
        if (length(fid) != 0) {
            rqda_exe(sprintf("update source set status = 1 where status = 0 and id in (%s)",
                             paste(paste(fid, sep = ""), collapse = ", ")))
            rqda_exe(sprintf("update coding set status = 1 where status = 0 and fid in (%s)",
                             paste(paste(fid, sep = ""), collapse = ", ")))
            rqda_exe(sprintf("update caselinkage set status =1 where status = 0 and fid in (%s)",
                             paste(paste(fid, sep = ""), collapse = ", ")))
            rqda_exe(sprintf("update treefile set status = 1 where status = 0 and fid in (%s)",
                             paste(paste(fid, sep = ""), collapse = ", ")))
        }
    }

    codeFun <- function(del) {
        cid <- rqda_sel(sprintf("select id from freecode where status = 0 AND name in (%s)",
                                paste(paste("'", del, "'", sep = ""), collapse = ", ")))[, 1]
        if (length(cid) != 0) {
            rqda_exe(sprintf("update freecode set status = 1  where status = 0 and id in (%s)",
                             paste(paste(cid, sep = ""), collapse = ", ")))
            rqda_exe(sprintf("update coding set status = 1 where status = 0 and cid in (%s)",
                             paste(paste(cid, sep = ""), collapse = ", ")))
        }
    }

    caseFun <- function(del) {
        caseid <- rqda_sel(sprintf("select id from cases where status = 0 AND name in (%s)",
                                   paste(paste("'", del, "'", sep = ""), collapse = ", ")))[, 1]
        if (length(caseid) != 0) {
            rqda_exe(sprintf("update cases set status =1  where status = 0 and id in (%s)",
                             paste(paste(caseid, sep = ""), collapse = ", ")))
            rqda_exe(sprintf("update caselinkage set status = 1 where status = 0 and caseid in (%s)",
                             paste(paste(caseid, sep = ""), collapse = ", ")))
        }
    }

    CcatFun <- function(del) {
        catid <- rqda_sel(sprintf("select catid from codecat where status = 0 AND name in (%s)",
                                  paste(paste("'", del, "'", sep = ""), collapse = ", ")))[, 1]
        if (length(catid) != 0) {
            rqda_exe(sprintf("update codecat set status = 1 where status = 0 and catid in (%s)",
                             paste(paste(catid, sep = ""), collapse = ", ")))
            UpdateTableWidget(Widget = .rqda$.CodeCatWidget, FromdbTable = "codecat")
            rqda_exe(sprintf("update treecode set status = 1 where status = 0 and catid in (%s)",
                             paste(paste(catid, sep = ""), collapse = ", ")))
        }
    }

    FcatFun <- function(del) {
        catid <- rqda_sel(sprintf("select catid from filecat where status = 0 AND name in (%s)",
                                  paste(paste("'", del, "'", sep = ""), collapse = ", ")))[, 1]
        if (length(catid) != 0) {
            rqda_exe(sprintf("update filecat set status = 1 where status = 0 and catid in (%s)",
                             paste(paste(catid, sep = ""), collapse = ", ")))
            UpdateTableWidget(Widget = .rqda$.FileCatWidget, FromdbTable = "filecat")
            rqda_exe(sprintf("update treefile set status = 1 where status = 0 and catid in (%s)",
                             paste(paste(catid, sep = ""), collapse = ", ")))
        }
    }
    ## end of helper functions

    if (!is_projOpen()) {
        print("No project is open!")
    }  else {
        type <- match.arg(type)
        del <- list.deleted(type)
        if (!is.data.frame(del)) {
            print("Nothing to clear.")
        } else {
            if (ask) {
                del <- gselect.list(del[, 1], multiple = TRUE) ##select.list(del[, 1], multiple = TRUE)
            } else {
                del <- del[, 1]
            }
            if (del != "") {
                del <- enc(del, encoding = "UTF-8")
                switch(type,
                       ## use switch to run Fun
                       file = fileFun(del = del),
                       code = codeFun(del = del),
                       case = caseFun(del = del),
                       codecategory = CcatFun(del = del),
                       filecategory = FcatFun(del = del)
                       )
            }
        }
    }
}
