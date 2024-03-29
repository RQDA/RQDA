prof_mat <- function(unit = c("coding", "file"), case_ids = NULL, case_names = NULL) {
    unit <- match.arg(unit)

    if (is.null(case_ids) & is.null(case_names)) {
        case_ids <- getCaseIds()
        case_names <- getCaseNames(case_ids)
    } else if (!is.null(case_ids)) {
        case_names <- getCaseNames(case_ids)
    } else {
        case_ids <- rqda_sel(sprintf("select id from cases where name in (%s)",
                                     paste(shQuote(case_names), collapse = ", ")))$id
    }

    codes <- rqda_sel(paste("select name, id, cid from freecode, coding where",
                            "freecode.id = coding.cid and freecode.status = 1",
                            "group by cid order by name"))
    Encoding(codes$name) <- "UTF-8"

    wnh <- size(.rqda$.root_rqdagui)
    w <- gwindow(title = sprintf(rqda_txt("Profile Matrix - %s"), unit),
                 width = getOption("widgetSize")[1],
                 height = getOption("widgetSize")[2],
                 visible = FALSE, parent = c(wnh[1] + 10, 2))

    addHandlerKeystroke(w, function(h, ...) {
        if (h$key == "\027") dispose(w)
    })
    gf <- ggroup(container = w, use.scrollwindow = TRUE)
    tbl <- glayout(container = gf, expand = FALSE)

    for (i in 1:nrow(codes)) {
        tbl[i + 1, 1] <- glabel(codes$name[i], container = tbl,
                                action = list(code = codes$name[i]),
                                handler = function(h, ...) {
                                    retrieval_by_code(code = h$action$code)
                                })
    }

    for (i in 1:length(case_names)) {
        tbl[1, i + 1] <- glabel(case_names[i], container = tbl)
    }

    for (i in 1:nrow(codes)) {
        ncoded <- rqda_sel(sprintf(
            paste("select count(fid) as n, fid, status from",
                  "coding where status = 1 and cid=%s group",
                  "by fid"), codes$cid[i]))

        for (col in 1:length(case_names)) {
            fid <- rqda_sel(
                sprintf("select fid from caselinkage where caseid=%s",
                        case_ids[col]))$fid
            if (nrow(ncoded) == 0) ncodings <- 0 else {
                                                     ncodings <- switch(unit,
                                                                        coding = sum(ncoded$n[ncoded$fid %in% fid]),
                                                                        file = sum(ncoded$fid %in% fid)
                                                                        )
                                                 }
            tbl[i + 1, col + 1] <- gcheckbox(
                formatC(ncodings, width = 4),
                container = tbl, use.togglebutton = TRUE,
                action = list(code = codes$name[i], fid = fid),
                handler = function(h, ...) {
                                        #cat("The widget is checked?", svalue(h$obj), "\n")
                    retrieval_by_code(Fid = h$action$fid, code = h$action$code)
                })
        }
    }

    visible(w) <- TRUE
}
