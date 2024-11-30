
box::use(
    dplyr[mutate, filter, across],
    lubridate[dmy], 
)

#' @export
process_raw <- function(raw, include, n = 5) {
    raw <- raw[raw != ""]
    
    ## calculate position indexes for RESULT TABLE:
    ## pri = position result index
    ## prt = position result table
    pri <- c(grep("RESULT TABLE", raw), length(raw))
    prt <- mapply(`:`, pri[-length(pri)], pri[-1], 
                  SIMPLIFY = TRUE)
    
    ## create a character vector to remove irrelevant texts
    texts_included <- c("Patient ID|Sample Type,|Test Result,|Analyte Name|HPV 16|HPV 18_45|P3,|P4,|P5,|SAC,|Start Time,")
    
    ## if prt is not a list, convert it to a list
    if (!is.list(prt))
        prt <- list(prt)
    
    ## run a loop to process individual RESULT TABLE `irt`
    irt <- lapply(prt[1:100], function(x) {
        t <- raw[x]
        t <- t[grepl(texts_included, t)]
        t <- t[!is.na(t)]
        hpv <- any(grepl("Test Result,HPV", t))
        
        if (hpv) {
            pid <- unlist(strsplit(t[grepl("Patient ID,", t)], ","))[2]
            type <- unlist(strsplit(t[grepl("Sample Type,", t)], ","))[2]
            sttime <- unlist(strsplit(t[grepl("Start Time,", t)], ","))[2]
            sttime <- unlist(strsplit(sttime, " "))[1]
            
            
            analyte <- c("Analyte Name,", "HPV 16,", "HPV 18_45,",
                         "P3,", "P4,", "P5,", "SAC,")
            analyte <- paste0(analyte, collapse = "|")
            analyte <- t(do.call(rbind, strsplit(t[grepl(analyte, t)], ",")))
            
            pos <- any(analyte[4, 2:7] %in% "POS")
            
            ct_val <- which(analyte[, 1] == "Ct")[1]
            
            res <- cbind(start_time = sttime,
                         PID = pid)
            bin <- unlist(strsplit(grep("Test Result,HPV", t, value = TRUE), split = ","))[2]
            bin <- unlist(strsplit(bin, split = "\\|"))
            bin <- gsub("HPV 16 |HPV 18_45 |OTHER HR HPV ", "", bin)
            
            if (include == "type") res <- cbind(res, type = type)
            res <- cbind(
                res,
                result = ifelse(pos, "Positive", "Negative"),
                hpv_16 = bin[1], 
                hpv_18_45 = bin[2], 
                hpv_hr = bin[3], 
                ct_hpv_16 = analyte[ct_val, which(analyte[1, ] == "HPV 16")[1]],
                ct_hpv_18_45 = analyte[ct_val, which(analyte[1, ] == "HPV 18_45")[1]],
                ct_p3 = analyte[ct_val, which(analyte[1, ] == "P3")[1]],
                ct_p4 = analyte[ct_val, which(analyte[1, ] == "P4")[1]],
                ct_p5 = analyte[ct_val, which(analyte[1, ] == "P5")[1]],
                ct_sac = analyte[ct_val, which(analyte[1, ] == "SAC")[1]]
            ) 
            
            if (ncol(res) == 13) {
                if (grepl("TSH", pid)) data.frame(res) else NA
            } else {NA}
        } else {NA}
        
    })
    
    # incProgress(1/n)
    
    eligible <- (is.na(irt))
    cat("\n ***** Removing", sum(eligible), "non-HPV Gates Study resuts for HPV *****\n")
    irt <- irt[!eligible]
    
    ## combine all results into one table
    output <- do.call(rbind, irt)
    if (is.null(output)) {
        hpv <- data.frame()
    } else {
        output <- output |> 
            mutate(across(ct_hpv_16:ct_sac, ~ as.numeric(.x)))
        
        # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4355361/
        output$vl_hpv_16 <- output$ct_hpv_16 / (output$ct_sac / 2)
        output$vl_hpv_18_45 <- output$ct_hpv_18_45 / (output$ct_sac / 2)
        output$vl_hpv_p3 <- output$ct_p3 / (output$ct_sac / 2)
        output$vl_hpv_p4 <- output$ct_p4 / (output$ct_sac / 2)
        output$vl_hpv_p5 <- output$ct_p5 / (output$ct_sac / 2)
        names(output)
        output[is.na(output) | sapply(output, is.nan)] <- 0
        output[which(output$result == "Negative"), c(
            "vl_hpv_16", "vl_hpv_18_45", "vl_hpv_p3", "vl_hpv_p4", "vl_hpv_p5"
        )] <- 0
        
        hpv <- output |> 
            mutate(start_time = dmy(start_time),
                          PID = gsub("\"", "", PID)) |> 
            filter(!is.na(start_time))
    }
    
    # incProgress(1/n)
    
    
    ## run a loop to process individual RESULT TABLE `irt`
    irt <- lapply(prt, function(x) {
        t <- raw[x]
        t <- t[grepl(texts_included, t)]
        t <- t[!is.na(t)]
        ctng <- any(grepl("Test Result,CT", t))
        
        if (ctng) {
            
            pid <- grep("Patient ID,", t, value = TRUE)
            pid <- gsub("\"|Patient ID,", "", pid)
            pid <- paste(pid, collapse = ", ")
            
            type <- unlist(strsplit(t[grepl("Sample Type,", t)], ","))[2]
            sttime <- unlist(strsplit(t[grepl("Start Time,", t)], ","))[2]
            sttime <- unlist(strsplit(sttime, " "))[1]
            
            bin <- unlist(strsplit(grep("Test Result,CT", t, value = TRUE), split = ","))[2]
            bin <- unlist(strsplit(bin, split = "\\|"))
            bin <- gsub("CT |NG ", "", bin)
            bin <- ifelse(bin == "NOT DETECTED", "NEG", 
                          ifelse(bin == "DETECTED", "POS", NA))
            
            res <- cbind(
                start_time = sttime, PID = pid, 
                type = type, 
                ct = bin[1], 
                ng = bin[2]
            )
            
            if (ncol(res) == 5) {
                if (grepl("TSH", pid)) data.frame(res) else NA
            } else {NA}
        } else {NA}
        
    })
    
    
    # incProgress(1/n)
    
    
    eligible <- (is.na(irt))
    cat("\n ***** Removing", sum(eligible), "non-HPV Gates Study resuts for CTNG *****\n")
    irt <- irt[!eligible]
    
    ## combine all results into one table
    output <- do.call(rbind, irt)
    if (is.null(output)) {
        ctng <- data.frame()
    } else {
        ctng <- output |> 
            dplyr::mutate(start_time = dmy(start_time), 
                          PID = gsub("\"", "", PID)) |> 
            dplyr::filter(!is.na(start_time))
    }
    
    list(hpv = hpv, ctng = ctng)
}

