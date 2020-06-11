rm(list = ls())
source("01_scripts/functions.R")

# excel path and  thresholds ----
excelfile <- "00_data/data.xlsx"

data <- func_data_extract(xl_file_path = excelfile)



noofbds <- data[[1]]
nooflts <- data[[2]]
noofents <- data[[3]]
consortia_mtx <- data[[4]]
lot_ms <- data[[5]]
lot_cntvalue <- data[[6]]
ent_cap <- data[[7]]
p_score_mtx <- data[[8]]
q_score_mtx <- data[[9]]
consortia_capital_per_lot_mtx <- data[[10]]
asset_divisor <- data[[11]]
consortia_data_tbl <- data[[12]]
# Outputs for Excel Download
# xl_lots_data <- dt[[13]]
# xl_bidders_data <- dt[[14]]
# xl_entity_data <- dt[[15]]
# xl_pqp_score <- dt[[16]]
# xl_earmarking_data_excel <- dt[[17]]
# bidders <- dt[[18]]
# lots <- dt[[19]]

cap_req <- 25
lt_ms_threshold <- 31
cap_req <- cap_req/100

bigN <- max(lot_cntvalue * cap_req) * 100 #more than max lot capital * 100


###################################################################################################
#       MIP Model Begins Here (Below model is to identify overall minimum P Score)
###################################################################################################

p_score_min_model <- MIPModel() %>% 
    add_variable(
        x[i, j], 
        i = 1:noofbds, 
        j = 1:nooflts, 
        type = "binary"
    ) %>% 
    add_variable(
        y[k, j], 
        k = 1:noofents, 
        j = 1:nooflts, 
        type = "continuous"
    ) %>%
    
    add_constraint(
        sum_expr(x[i, j], 
                 i = 1:noofbds) == 1 , 
        j = 1:nooflts
    ) %>% 
    add_constraint(
        sum_expr(lot_ms[j] * consortia_mtx [k, i] * x[i ,j] ,
                 j = 1:nooflts, i = 1:noofbds) <= lt_ms_threshold,
        k = 1:noofents
        
    ) %>%
    add_constraint(
        y[k,j] >= 0,
        k = 1:noofents,
        j = 1:nooflts
    ) %>% 
    add_constraint(
        y[k,j] <=  sum_expr(consortia_mtx [k, i] * x[i ,j] * bigN, i = 1:noofbds),
        k = 1:noofents,
        j = 1:nooflts
    ) %>%
    add_constraint(
        sum_expr(y[k,j], k = 1:noofents) >= (lot_cntvalue[j] * cap_req) ,
        j = 1:nooflts
    )%>%
    add_constraint(
        sum_expr(y[k,j], j = 1:nooflts) <= ent_cap[k],
        k = 1:noofents
    )%>%
    
    add_constraint(
        y[k,j] <= consortia_capital_per_lot_mtx[k,j],
        k = 1:noofents,
        j = 1:nooflts
    ) %>%
    set_objective(
        sum_expr(
            p_score_mtx[i, j]  * x[i ,j],  
            i = 1:noofbds,
            j = 1:nooflts) , 
        "min"
    )


p_score_min_solver <- p_score_min_model %>% 
    solve_model(with_ROI(solver = "lpsolve", 
                         verbose = TRUE,
                         control = list("epsb" = 1e-20,
                                        "epsel" = 1e-20)) 
    )

p_score_boundry <- p_score_min_solver %>% 
    objective_value()


p_score_output <- p_score_min_solver %>% 
    get_solution(x[i , j]) %>% 
    dplyr::filter(value > 0)


p_score_output # this output is correct and it is as expected 


###############################################################################################################################################################
#       MIP Model for Max Q-Score begin (this model will remove any ties i.e. where there is same PQP score for two bds) using pscore_boundry as constraint
###############################################################################################################################################################


q_score_max_model <- p_score_min_model %>% 
    add_constraint(
        sum_expr(p_score_mtx[i,j] * x[i,j],
                 i = 1:noofbds,
                 j = 1:nooflts) == p_score_boundry 
        
    ) %>% 
    set_objective(
        sum_expr(
            q_score_mtx[i, j]  * x[i ,j],
            i = 1:noofbds,
            j = 1:nooflts) ,
        "max"
    )


q_score_max_solver <- q_score_max_model %>% 
    solve_model(with_ROI(solver = "lpsolve", 
                         verbose = TRUE,
                         control = list("epsb" = 1e-20,
                                        "epsel" = 1e-20)
    ) 
    )

q_score_boundry <- q_score_max_solver %>% 
    objective_value()


q_score_output <- q_score_max_solver %>% 
    get_solution(x[i , j]) %>% 
    dplyr::filter(value > 0)


# Below results are correct, however this could be complete coincidence. If data changes then results could change

q_score_output 





# --------------------------------- Funny Ties: Model ---------------------------------------------------------------------------------------------------------------------------------
# Funny Ties : When there is either or solution to choose from i.e. more than one outcome for the same overall P score (Result should be same as P-score model for this data)



initial_result_tbl <- q_score_max_model %>% 
    solve_model(with_ROI(solver = "lpsolve", 
                         verbose = TRUE,
                         control = list("epsb" = 1e-20,
                                        "epsel" = 1e-20)
    )
    )  %>%
    get_solution(x[i , j]) %>% 
    # dplyr::filter(value > 0) %>% 
    dplyr::as_tibble()


output_results_list <- list( initial_result_tbl)

model_status <- "optimal"

data <- initial_result_tbl # initialisation of data matrix


#---- Start of while loop ------

while(model_status == "optimal" ) {
    
    
    duplicate_test_mxt <- as.matrix(spread(subset(data,
                                                  select = c(i,j,value)
    ), j,value
    )[,-1]
    )
    
    tmp_out_solve <- q_score_max_model %>% 
        add_constraint(sum_expr(x[i,j] * duplicate_test_mxt[i,j] , i = 1:noofbds, j= 1:nooflts) <= nooflts - 1 ) %>%
        add_constraint(
            sum_expr(p_score_mtx[i,j] * x[i,j],
                     i = 1:noofbds,
                     j = 1:nooflts)  == p_score_boundry
            
        ) %>%
        add_constraint(
            sum_expr(q_score_mtx[i,j] * x[i,j],
                     i = 1:noofbds,
                     j = 1:nooflts) == q_score_boundry
            
        ) %>%
        
        solve_model(with_ROI(solver = "lpsolve", 
                             verbose = TRUE,
                             control = list("epsb" = 1e-20,
                                            "epsel" = 1e-20)
        )
        )
    
    model_status <- tmp_out_solve %>% 
        solver_status()
    
    if (model_status == "optimal") {
        
        temp_result_tbl <- tmp_out_solve %>% 
            get_solution(x[i , j]) %>%
            dplyr::as_tibble()
        output_results_list <- c(output_results_list,list(temp_result_tbl))
        
    } else {
        output_results_list <- c(output_results_list)
    }
    
    output_tbl <- output_results_list %>% 
        purrr::reduce(rbind) %>% 
        dplyr::distinct() %>% 
        dplyr::select(i, j, value) %>% 
        dplyr::arrange(j)
    
    data <- output_tbl %>% dplyr::select(i, j, value) %>% 
        dplyr::group_by(i,j) %>% 
        dplyr::summarise(value = max(value)) %>% 
        dplyr::ungroup()
    
}
#---- end of while loop ------

output_tbl <- output_tbl %>%
    dplyr::filter(value >0) 


output_tbl # This output is not as expected for some reason the constrains for P score and Q score doesn't seems to work: IN this data set the p score is not same for any of the elements in column I


# Outputs ------
# p_score_output
p_score_output

# Q Score output
q_score_output

# Final Output
output_tbl
