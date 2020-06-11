# libraries----
library(tidyverse)
library(readxl)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.lpsolve) # not in conda
library(ompr) # not in conda
library(ompr.roi) # not in conda

# ----------------- Data Extraction Function --------------------------
func_data_extract <- function(xl_file_path) {
    
    ##import data from excel template
    divisor <- 1000000
    
    # 1 Data Import ----

    

    xl_lt_data    <- readxl::read_excel(path = xl_file_path, 
                                          sheet ='ltdata') 

    xl_bd_data <- readxl::read_excel(path = xl_file_path,  
                                          sheet ='bddata')


    xl_ent_data  <- readxl::read_excel(path = xl_file_path,  
                                          sheet ='entdata') 

 
    xl_p_score    <- readxl::read_excel(path = xl_file_path,
                                          sheet ='pscore')
    
 
    xl_earamt_data <- readxl::read_excel(xl_file_path, 
                                                   sheet ='earamt') 
    xl_earamt_data <- xl_earamt_data[order(xl_earamt_data$entid, 
                                           xl_earamt_data$ltid),] %>% 
        dplyr::mutate(earamt = earamt/divisor) 
    
    # Extracting ID's  as list -----
    bds <-  sort(c(unique(xl_p_score$bdid)))
    lts <-   sort(c(unique(xl_p_score$ltid)))
    ents <- sort(c(unique(xl_ent_data$entid))) 
    
    # lenghts of bds, lts and ents ----
    noofbds  <- length(bds)
    nooflts     <- length(lts)
    noofents <- length(ents)
    
    # load lts cnt values ----
    lot_cntvalue <- c()
    lot_ms     <- c()
    
    for (i in lts){
        lot_cntvalue  <- c(lot_cntvalue, 
                                pull(subset(xl_lt_data, 
                                            ltid == i)['cntvalue'])/divisor
        )
        
        lot_ms      <- c(lot_ms, 
                                pull(subset(xl_lt_data, 
                                            ltid == i)['ms'])*100
        )
        
    }
    

    # ents total cap ----
    ent_cap <- c()
    
    for (i in ents){
        
        ent_cap    <- c(ent_cap, 
                               pull(subset(xl_ent_data, 
                                           entid == i)['entcap'])/divisor
        )
        
        
    }
    

    ## Matrices -----
    # P Score Matrix
    p_score_mtx   <-  as.matrix(spread(subset(xl_p_score,
                                                select = c(bdid, 
                                                           ltid,
                                                           p_score)
    ),
    ltid, 
    p_score)[,-1])
    
    p_fill_na <- max(p_score_mtx, na.rm = T)+100000
    
    p_score_mtx[is.na(p_score_mtx)] <- p_fill_na
    

    # Q Score
    q_score_mtx   <-  as.matrix(spread(subset(xl_p_score,
                                                    select = c(bdid, 
                                                               ltid,
                                                               q_score)
    ),
    ltid, 
    q_score)[,-1])
    
    q_score_mtx[is.na(q_score_mtx)] <- 0
    
    
    ## ent Matrix
    
    ncol <- length(colnames(xl_bd_data))
    
    consortia_data_tbl <- xl_bd_data[,c(1,3:ncol)] %>% 
        tidyr::pivot_longer( - bdid,
                             names_to = "entid",
                             values_to = "ent_pct"
        ) %>% 
        dplyr::mutate(entid = entid %>% 
                          as.numeric()) %>% 
        # dplyr::left_join(xl_ent_data) %>% 
        dplyr::select(bdid,entid, ent_pct)
    
    
    consortia_mtx <- as.matrix(spread(subset(consortia_data_tbl,
                                             select = c(bdid, 
                                                        entid,
                                                        ent_pct)),
                                      bdid, 
                                      ent_pct)[,-1])
    
    consortia_mtx[is.na(consortia_mtx)] <- 0
    
    # consortial cap per lot: earamt Matrix

    consortia_cap_per_lot_mtx   <-  as.matrix(spread(subset(xl_earamt_data,
                                                                select = c(entid, 
                                                                           ltid,
                                                                           earamt)),
                                                         ltid, 
                                                         earamt)[,-1])
    
    consortia_cap_per_lot_mtx[is.na(consortia_cap_per_lot_mtx)] <- 0
    
    
    
    
    return(
        list(
            noofbds,
            nooflts, 
            noofents, 
            consortia_mtx,
            lot_ms, 
            lot_cntvalue,
            ent_cap, 
            p_score_mtx, 
            q_score_mtx, 
            consortia_cap_per_lot_mtx,
            divisor, 
            consortia_data_tbl, 
            # Outputs for Excel Download
            xl_lt_data, 
            xl_bd_data, 
            xl_ent_data, 
            xl_p_score, 
            xl_earamt_data, 
            bds, # bds Vector
            lts # lts Vector
        )
    )
}

