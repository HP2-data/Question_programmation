

data("diamonds")

Table_stat_desc_R(diamonds %>% head(4999) %>% mutate_if(is.factor,~factor(., ordered = FALSE )),"median","color",Test_group = TRUE)


require(tidyverse)
require(caret)

`%notin%` <- Negate(`%in%`) 
##########################################################################################
# table descriptive des variables
##################################################################################
#################### Descriptive stat + Test function #########################
Table_stat_desc_R <- function(df_func,Numeric_func,name_expo_fonc,Test_group = TRUE){
  
  test_string <- df_func %>%  select_if(function(col) !is.factor(col) & !is.numeric(col)) 
  
  drop_col <- test_string %>%
    colnames %>% 
    unlist(use.names = FALSE)
  
  if (ncol(test_string) != 0) {
    
    warning("following columns are not factor or numeric and are droped : ",
            paste0(drop_col,
                   collapse = ", "))
  }
  
  df_func_num <- df_func %>%  select(-all_of(drop_col)) %>% rename(grouping_var = !!sym(name_expo_fonc))
  
  
  
  factor_df <- df_func_num %>% 
    select(grouping_var,where(is.factor)) %>% 
    mutate(grouping_var = "all") %>% 
    rbind(df_func_num %>% select(grouping_var,where(is.factor)))
  
  
  
  
  
  #####################################
  # Generation of a table for numeric
  
  DESC_Stats <- stat_dec_fun(df_func_num,factor_df,Numeric_func)
  
  ################################################################################
  ########################## TEST ################################################
  if (Test_group){
    P_value_Stats <- pair_wise_test(df_func_num,factor_df,Numeric_func)
    table_res <- DESC_Stats %>%  left_join(P_value_Stats, by = c("name","value"))
    
  } else {
    table_res <- DESC_Stats 
  }
  
  
  ###############################################################################
  ###################### TEST ####################################################
  table_res_pval <- table_res %>% arrange(name) #%>% 
  #  select(sort(tidyselect::peek_vars()))  
  
  tbl_nb_ind_grp <-  table(df_func_num[,"grouping_var"])
  nb_pat_par_grp <- suppressWarnings(c(nrow(df_func_num),
                                       tbl_nb_ind_grp[order(as.numeric(names(tbl_nb_ind_grp)))]))
  
  names_group <- c("name","value","all" ,names(nb_pat_par_grp)[-1])
  nb_pat_par_grp <-  paste0(nb_pat_par_grp , "(" , round(nb_pat_par_grp*100/nrow(df_func_num),1) ,"%)")
  
  
  nb_pat_par_grp <- c("Number of patient",NA,nb_pat_par_grp,rep(NA,(ncol(table_res_pval) - (2 + length(nb_pat_par_grp)))  ))
  
  
  
  names(nb_pat_par_grp) <- c(names_group,
                             colnames(table_res_pval)[colnames(table_res_pval) %notin% names_group])
  
  res <- plyr::rbind.fill(as.data.frame(t(nb_pat_par_grp)),
                          table_res_pval)
  
  
  
  return(res)
  
  
}


###############################################################################
###################### Create descriptive statistique table ###################
stat_dec_fun <- function(df_func_num,factor_df,Numeric_func){
  
  num_df <- df_func_num %>% select(grouping_var,where(is.numeric)) %>% 
    recup_var_table_res("grouping_var") %>%
    mutate_at(vars(-key),~round(.,1)) %>% 
    mutate(nb_NA = ifelse(nb_NA == 0,"",paste0("NA:", nb_NA ))) 
  
  name_all_grp <- paste0("all")
  nb_grp <- df_func_num %>%
    select(grouping_var) %>% 
    unique() %>% 
    unlist(use.names = FALSE) %>% 
    sort
  
  if (Numeric_func == "median") {
    column_num <- c("med","quart1","quart2")
  } else if (Numeric_func == "mean") {
    column_num <- c("moy","std")
  } else (stop("not support func for numeric support are : ",
               paste0(c("median","mean"),
                      collapse = ", ")))
  
  
  
  if(ncol(df_func_num %>%
          select(grouping_var,where(is.numeric))) > 1) {
    group_cluster_numeric_descript_var <- lapply(nb_grp, function(x) {
      col_name <- paste0(#nom_grp,"_",
        x)
      
      res <- df_func_num %>%
        select(grouping_var,where(is.numeric)) %>%
        filter(grouping_var == x) %>% 
        recup_var_table_res("grouping_var")  %>% 
        mutate_at(vars(-key),~round(.,1)) %>% 
        mutate(nb_NA = ifelse(nb_NA == 0,"",paste0("NA:", nb_NA ))) %>% 
        select(key,all_of(column_num),nb_NA) %>%
        rowwise() %>% 
        mutate( {{col_name}} := paste0(!!rlang::sym(column_num[1]) ,"(",
                                       
                                       paste(!!!rlang::syms(column_num[-1]),sep = ";") , ")"  ,
                                       nb_NA) )   %>% 
        mutate_all(~str_replace(.,"NA\\([NA\\;]+NA\\)","NA:")) %>% # corriger pour match n pattern
        select(key,all_of(col_name)) 
      return(res)
    }
    ) 
    
    table_res_numeric <- num_df  %>% 
      select(key,all_of(column_num),nb_NA) %>% rowwise() %>% 
      mutate( {{name_all_grp}} := paste0(!!rlang::sym(column_num[1]) ,"(",
                                         paste(!!!rlang::syms(column_num[-1]),sep = ";") , ")"  ,
                                         nb_NA) )   %>% 
      select(key,all_of(name_all_grp)) 
    
    
    Table_num_Total <- inner_join(table_res_numeric,group_cluster_numeric_descript_var %>% 
                                    reduce(inner_join,"key"), "key")  %>% 
      rename(name = key)
  }
  
  ################################################################################
  ################## Generation of table with count for factor ###################
  
  
  if(ncol(factor_df) > 1) {
    Table_factor_Total <- factor_df %>%  
      pivot_longer(-grouping_var) %>%
      {. ->> temp_full_list_facotr} %>% 
      group_by(name,grouping_var) %>% 
      mutate(percent_temp = sum(!is.na(grouping_var))) %>% 
      ungroup() %>% 
      group_by(name,grouping_var,value) %>% 
      #{ . ->> tmp_nb_bygrp } %>% 
      summarise(n =n(),percent = n()*100/percent_temp,.groups = "drop") %>% 
      { . ->> tmp_na } %>% 
      filter(is.na(value)) %>% 
      select(-value,-percent) %>%
      rename(nb_NA = n) %>% 
      distinct(.keep_all = TRUE) %>%
      right_join(temp_full_list_facotr %>% 
                   select(-grouping_var) %>% 
                   distinct(name,value) %>% 
                   filter(!is.na(value)) %>%
                   merge(unique(factor_df$grouping_var)) %>% rename(grouping_var = y)
                 ,by =c("name","grouping_var")) %>% 
      mutate(nb_NA = ifelse(is.na(nb_NA),0,nb_NA)) %>% 
      left_join(tmp_na %>% distinct,by = c("name","grouping_var","value")) %>%
      mutate( nb_NA = ifelse(0==nb_NA,"",paste0("NA:", nb_NA )),
              n = ifelse(is.na(n),"",n ), 
              percent = ifelse(is.na(percent),"",paste0("(",round(percent,1), "%)"))
      ) %>% 
      mutate(res = paste0(n,percent, nb_NA)) %>% 
      select(-nb_NA,-n,-percent) %>% 
      pivot_wider(names_from = grouping_var , values_from = res)
    res <- rbind(Table_factor_Total,tibble(Table_num_Total,value = NA))
    
  } else {res <- data.frame(Table_num_Total,value = NA)}
  
  if(ncol(df_func_num %>%
          select(grouping_var,where(is.numeric))) == 1) {res <- Table_factor_Total}
  
  return(res)
}





################################################################################
########################## TEST ################################################
################################################################################

pair_wise_test <- function(df_func_num,factor_df,Numeric_func) {
  
  near_zero_var_col <- colnames(df_func_num)[df_func_num %>%  
                                               caret::checkConditionalX(df_func_num$grouping_var)]
  
  
  zero_var_NA_omit <- df_func_num %>% group_by(grouping_var) %>%
    summarise_all(~length(unique(na.omit(.)))) %>%
    select(-grouping_var)%>%
    summarise_all(~min(.)) %>% 
    select_if(function(col) col == 1) %>%
    colnames()
  
  near_zero_var_col <- c(near_zero_var_col,zero_var_NA_omit[zero_var_NA_omit %notin% near_zero_var_col])
  
  
  near_zero_var_col <- near_zero_var_col[near_zero_var_col != "grouping_var"]
  
  
  if (length(near_zero_var_col) != 0 ){
    Cbn_var_near_zero_var <- rbind(
      data.frame(name = df_func_num[,near_zero_var_col] %>% 
                   select(where(is.numeric)) %>% 
                   colnames(), 
                 value = NA),
      df_func_num[,near_zero_var_col] %>%
        select(where(is.factor)) %>% 
        mutate_all(as.character) %>% 
        pivot_longer(everything())  %>% 
        distinct(name,value) )
  } 
  
  
  
  ################################################################################
  ####################### Qualitative ############################################
  if (ncol(factor_df) > 1){
    Test_factor_df_extra_all_grp_rename <- factor_df %>% select(-any_of(near_zero_var_col))
    
    Cbn_var_chisq_test <- Test_factor_df_extra_all_grp_rename %>% 
      pivot_longer(-grouping_var) %>% 
      distinct(grouping_var,name,value) %>% filter(!is.na(value))
    
    # Globale testing 
    global_quali_list <- Test_factor_df_extra_all_grp_rename %>% 
      pivot_longer(-grouping_var) %>%
      filter(grouping_var !="all") %>% 
      count(grouping_var,name,value) %>% 
      group_by(name) %>% 
      pivot_wider(names_from = grouping_var, values_from = n) %>% 
      mutate_at(vars(any_of(Test_factor_df_extra_all_grp_rename$grouping_var)),
                ~ifelse(is.na(.),
                        0,
                        .)
      ) %>% ungroup() %>% 
      group_split(name, .keep = TRUE)
    
    global_quali <- suppressWarnings(lapply(global_quali_list, function(x){
      test <- x %>% select(-name) %>% mutate(value = ifelse(is.na(value),"NA",as.character(value))) %>% column_to_rownames("value") %>% as.matrix %>% chisq.test
      x %>% mutate(p_val = test$p.value,
                   possible = min(test$expected) >=5,
                   p_val = ifelse(possible,p_val,NA),
                   Test = ifelse(possible,"chisq","impossible"))
    }
    ) %>% bind_rows()  %>%
      select(-possible,-any_of(Test_factor_df_extra_all_grp_rename$grouping_var)) %>% 
      mutate(grouping_var.x = "global",
             grouping_var.y = "global",
             var.x.x = NA,
             var.y.x = NA))
    
    
    
    
    
    
    
    
    chisq_and_fisher_test <- suppressWarnings(Cbn_var_chisq_test %>%
                                                inner_join(Cbn_var_chisq_test, by = c("name","value")) %>%
                                                filter(grouping_var.x>grouping_var.y) %>%
                                                rowwise() %>%
                                                mutate(var.x = list(Test_factor_df_extra_all_grp_rename %>%
                                                                      filter(grouping_var == grouping_var.x | grouping_var == grouping_var.y) %>%
                                                                      select(grouping_var) %>%
                                                                      unlist(use.names = FALSE) %>%
                                                                      as.factor ),
                                                       var.y = list(Test_factor_df_extra_all_grp_rename %>%
                                                                      filter(grouping_var == grouping_var.x | grouping_var == grouping_var.y) %>%
                                                                      select(name) %>%
                                                                      unlist(use.names = FALSE) ))  %>%
                                                mutate(min_expected_chisq = min(chisq.test(var.x, var.y)$expected)) %>%
                                                mutate(chisq_possible = min_expected_chisq >=5 ) %>%
                                                mutate(fisher_possible = (!chisq_possible & nlevels(var.y) < 3)))
    
    
    
    
    need_one_hot_fact <- chisq_and_fisher_test %>%
      filter(!chisq_possible ,!fisher_possible) %>%
      {. ->> fisher_grp_temp_table }%>% 
      select(name) %>%
      unlist(use.names = FALSE)
    
    
    if (length(need_one_hot_fact) > 0) {
      Test_factor_df_one_hot <- cbind(grouping_var = Test_factor_df_extra_all_grp_rename$grouping_var,
                                      predict(
                                        caret::dummyVars(" ~ .",
                                                         data=Test_factor_df_extra_all_grp_rename %>%
                                                           select(all_of(need_one_hot_fact)),
                                                         sep = "_._"),
                                        newdata = Test_factor_df_extra_all_grp_rename %>%
                                          select(all_of(need_one_hot_fact))
                                      ) ) %>% as.tibble()
      
      near_zero_var_col_on_one_hot <- colnames(Test_factor_df_one_hot)[Test_factor_df_one_hot %>%
                                                                         caret::checkConditionalX(Test_factor_df_one_hot$grouping_var)]
      near_zero_var_col_on_one_hot <- near_zero_var_col_on_one_hot[near_zero_var_col_on_one_hot != "grouping_var"]
      
      
      
      Cbn_var_chisq_one_hot <- Test_factor_df_one_hot %>%
        pivot_longer(-grouping_var) %>%
        separate(name, c("prefix", "suffix"),sep = "_._", remove = FALSE) %>%  
        distinct(grouping_var,name, .keep_all = TRUE ) %>% 
        select(-value)
      
      
      fisher_table <- Cbn_var_chisq_one_hot %>%
        inner_join(Cbn_var_chisq_one_hot %>% 
                     select(grouping_var,name), by = c("name")) %>%
        inner_join(fisher_grp_temp_table %>% 
                     select(name,contains("grouping_var")),
                   by = c("prefix"="name","grouping_var.x","grouping_var.y") ) %>% 
        filter(grouping_var.x>grouping_var.y) %>%
        rowwise() %>%
        mutate(var.x = list(Test_factor_df_one_hot %>%
                              filter(grouping_var == grouping_var.x | grouping_var == grouping_var.y) %>%
                              select(grouping_var) %>%
                              unlist(use.names = FALSE) %>%
                              as.factor ),
               var.y = list(Test_factor_df_one_hot %>%
                              filter(grouping_var == grouping_var.x | grouping_var == grouping_var.y) %>%
                              select(name) %>%
                              unlist(use.names = FALSE) %>% as.factor ))  %>%
        mutate(fisher_possible = (nlevels(var.y) < 3)) %>% 
        select(-name) %>% 
        rename(name = prefix, value = suffix  )
      
      Table_P_val_Quali_without_global <- fisher_table %>% 
        right_join(chisq_and_fisher_test,
                   by =  c("grouping_var.x", "name" ,"value","grouping_var.y")) %>% 
        mutate(var.x.x = ifelse(is.null(var.x.x),
                                list(var.x.y),
                                list(var.x.x)),
               var.y.x = ifelse(is.null(var.y.x),
                                list(var.y.y),
                                list(var.y.x)),
               Test = ifelse(chisq_possible,
                             "chisq",
                             "fisher"),
               Test = ifelse(paste0(name,"_._",value) %in% near_zero_var_col_on_one_hot,
                             "impossible",
                             Test)
        ) %>%
        select(-contains("fisher_possible"),
               -chisq_possible,
               -var.x.y,
               -var.y.y,
               -min_expected_chisq) %>% 
        mutate(p_val = ifelse(Test =="impossible",
                              NA,
                              ifelse(Test == "fisher",
                                     fisher.test(var.x.x, var.y.x)$p.value,
                                     ifelse(Test == "chisq",
                                            chisq.test(var.x.x, var.y.x)$p.value)
                              )
        )
        )
      
      
      
    } else {
      Table_P_val_Quali_without_global <- chisq_and_fisher_test %>% 
        mutate(var.x.x = list(var.x),
               var.y.x = list(var.y),
               Test = ifelse(chisq_possible,
                             "chisq",
                             "fisher")
        ) %>%
        select(-contains("fisher_possible"),
               -chisq_possible,
               -var.x,
               -var.y,
               -min_expected_chisq) %>% 
        mutate(p_val = ifelse(Test =="impossible",
                              NA,
                              ifelse(Test == "fisher",
                                     fisher.test(var.x.x, var.y.x)$p.value,
                                     ifelse(Test == "chisq",
                                            chisq.test(var.x.x, var.y.x)$p.value)
                              )
        )
        )
    }
    
    Table_P_val_Quali <- Table_P_val_Quali_without_global %>% rbind(global_quali)
  } 
  
  ################################################################################
  ####################### Quantitatif ############################################
  
  if (ncol(df_func_num  %>%
           select(grouping_var,where(is.numeric))) > 1){
    Test_numeric_df_all_grp_rename <- df_func_num  %>%
      select(grouping_var,where(is.numeric)) %>% 
      mutate(grouping_var = "all") %>% 
      rbind(df_func_num %>% select(grouping_var,where(is.numeric))
      ) %>% 
      select(-any_of(near_zero_var_col))
    
    
    
    
    Cbn_var_t_test_test <- Test_numeric_df_all_grp_rename %>% 
      pivot_longer(-grouping_var) %>% 
      distinct(grouping_var,name ) %>%
      mutate(grouping_var = as.character(grouping_var))
    
    
    global_quanti_kruskal <-
      Test_numeric_df_all_grp_rename %>% 
      pivot_longer(-grouping_var) %>%
      filter(grouping_var !="all") %>% 
      group_by(name) %>% 
      nest(data = c(grouping_var, value))  %>% 
      mutate(kruskal_raw = map(data, ~ kruskal.test(.x$value, .x$grouping_var)),
             kruskal = map(kruskal_raw, broom::tidy)) %>%
      select(-data) %>%
      unnest(kruskal) %>% 
      select(name,p.value) %>%
      rename(p_val = p.value) %>% 
      mutate(Test = "kruskal test",
             grouping_var.x = "global",
             grouping_var.y = "global")
    
    if (Numeric_func == "median"){
      Table_P_val_Quanti_without_global <- suppressWarnings(Cbn_var_t_test_test %>%
                                                              inner_join(Cbn_var_t_test_test, by = c("name")) %>%
                                                              filter(grouping_var.x>grouping_var.y) %>%
                                                              rowwise() %>%
                                                              mutate(var.x = list(Test_numeric_df_all_grp_rename %>%
                                                                                    filter(grouping_var == grouping_var.x) %>%
                                                                                    select(name) %>%
                                                                                    unlist(use.names = FALSE)
                                                              ),
                                                              var.y = list(Test_numeric_df_all_grp_rename %>%
                                                                             filter( grouping_var == grouping_var.y) %>%
                                                                             select(name) %>%
                                                                             unlist(use.names = FALSE) ))  %>%
                                                              mutate(number_of_ind = min(length(var.x),length(var.y))>= 30 ) %>%
                                                              mutate(normality = (shapiro.test(var.x)$p.value >= 0.05 &
                                                                                    shapiro.test(var.y)$p.value >= 0.05)
                                                              ) %>%
                                                              mutate(Test = "Mann–Whitney") %>% 
                                                              mutate(p_val = wilcox.test(var.x, var.y)$p.value)
      )
      
    } else {
      Table_P_val_Quanti_without_global <- suppressWarnings(Cbn_var_t_test_test %>%
                                                              inner_join(Cbn_var_t_test_test, by = c("name")) %>%
                                                              filter(grouping_var.x>grouping_var.y) %>%
                                                              rowwise() %>%
                                                              mutate(var.x = list(Test_numeric_df_all_grp_rename %>%
                                                                                    filter(grouping_var == grouping_var.x) %>%
                                                                                    select(name) %>%
                                                                                    unlist(use.names = FALSE)
                                                              ),
                                                              var.y = list(Test_numeric_df_all_grp_rename %>%
                                                                             filter( grouping_var == grouping_var.y) %>%
                                                                             select(name) %>%
                                                                             unlist(use.names = FALSE) ))  %>%
                                                              mutate(number_of_ind = min(length(var.x),length(var.y))>= 30 ) %>%
                                                              mutate(normality = (shapiro.test(var.x)$p.value >= 0.05 &
                                                                                    shapiro.test(var.y)$p.value >= 0.05)
                                                              ) %>%
                                                              mutate(Test = ifelse(number_of_ind | normality,"t-test","Mann–Whitney")) %>% 
                                                              mutate(p_val = ifelse(Test == "t-test",
                                                                                    t.test(var.x, var.y)$p.value,
                                                                                    ifelse(Test == "Mann–Whitney",
                                                                                           wilcox.test(var.x, var.y)$p.value)
                                                              )
                                                              
                                                              ))
      
    }
    
    
    Table_P_val_Quanti <- Table_P_val_Quanti_without_global %>% select(name,grouping_var.x,grouping_var.y,Test,p_val) %>% 
      plyr::rbind.fill(global_quanti_kruskal ) 
  } 
  
  
  if (ncol(df_func_num  %>%
           select(grouping_var,where(is.numeric))) == 1){
    P_value_without_zero_var <- Table_P_val_Quali %>% 
      select(name,value,grouping_var.x,grouping_var.y,Test,p_val)  %>% 
      {. ->> check_PVAL} %>% select(-starts_with("check_")) %>% 
      rowwise() %>% 
      mutate(grouping_var = ifelse(grouping_var.x== "global" |
                                     grouping_var.y == "global",
                                   "global" ,
                                   paste0(grouping_var.x, " vs ",grouping_var.y))
      ) %>% 
      select(-grouping_var.x,-grouping_var.y) %>% distinct() %>% 
      pivot_wider(names_from = grouping_var, 
                  values_from = c(p_val, Test),
                  names_sep = " ")    
    
  } else if(ncol(factor_df) == 1){
    P_value_without_zero_var <- Table_P_val_Quanti  %>% 
      {. ->> check_PVAL} %>% select(-starts_with("check_")) %>% 
      rowwise() %>% 
      mutate(grouping_var = ifelse(grouping_var.x== "global" |
                                     grouping_var.y == "global",
                                   "global" ,
                                   paste0(grouping_var.x, " vs ",grouping_var.y))
      ) %>% 
      select(-grouping_var.x,-grouping_var.y) %>% distinct() %>% 
      pivot_wider(names_from = grouping_var, 
                  values_from = c(p_val, Test),
                  names_sep = " ")    
    
  } else {
    
    P_value_without_zero_var <- full_join(Table_P_val_Quali %>% 
                                            select(name,value,grouping_var.x,grouping_var.y,Test,p_val),
                                          Table_P_val_Quanti, #%>% 
                                          # data.frame(value = NA)
                                          by = c("name","grouping_var.x","grouping_var.y") ) %>% 
      mutate(Test = ifelse(!is.na(Test.x),Test.x,Test.y),
             check_test = (!is.na(Test.x) & !is.na(Test.y)),
             p_val =ifelse(!is.na(p_val.x),p_val.x,p_val.y),
             check_p_val = (!is.na(p_val.x) & !is.na(p_val.y))
      ) %>% 
      {. ->> check_PVAL} %>% select(-starts_with("check_"),-Test.x,-p_val.x,-Test.y,-p_val.y ) %>% 
      rowwise() %>% 
      mutate(grouping_var = ifelse(grouping_var.x== "global" |
                                     grouping_var.y == "global",
                                   "global" ,
                                   paste0(grouping_var.x, " vs ",grouping_var.y))
      ) %>% 
      select(-grouping_var.x,-grouping_var.y) %>% distinct() %>% 
      pivot_wider(names_from = grouping_var, 
                  values_from = c(p_val, Test),
                  names_sep = " ")    
  }
  
  
  
  
  if (length(near_zero_var_col) != 0 ){
    Cbn_var_near_zero_var[,P_value_without_zero_var %>% select(-any_of(colnames(Cbn_var_near_zero_var))) %>% colnames()] <- NA
    if (any(check_PVAL %>% select(starts_with("check_")) %>% unlist() )) {
      stop("erreur P val")
    }
    
    res <- rbind(Cbn_var_near_zero_var %>% 
                   mutate_at(vars(contains("Test")),
                             ~ifelse(is.na(.),
                                     "impossible",
                                     .) 
                   ),
                 P_value_without_zero_var) 
  } else {res <- P_value_without_zero_var }
  
  
  return(res)
}

# transpose dataframe
t.df <- function(df,pivot=NULL){
  if (is.null(pivot)){
    pivot <- "row_id"
    df <- df %>% mutate(row_id=paste0("col_",1:nrow(df) ))
  }
  res <- df %>% pivot_longer(cols = -!!pivot,"key","value") %>%
    pivot_wider(names_from = !!pivot,values_from = value)
  return(res) 
}


##########################################################################################
# function récupérant les variables pour table descriptive des variables

# df_one_hot_encode_fonc généré avec one_hot_fb

recup_var_table_res <- function(df_one_hot_encode_fonc,name_expo_fonc){
  res <- df_one_hot_encode_fonc %>% 
    select(-all_of(name_expo_fonc)) %>% 
    mutate_if(is.factor, ~as.numeric(as.character(.))) %>% # points litigieux j'utilise cette méthode pour convertir mes facteur booleen en numeric a surveilllé a l'avenir
    summarise_all(list(fonc_moy = ~mean(.,na.rm = TRUE),
                       fonc_std =~sd(.,na.rm = TRUE),
                       fonc_med = ~median(.,na.rm = TRUE),
                       fonc_quart1 = ~quantile(.,0.25,na.rm = TRUE),
                       fonc_quart2 = ~quantile(.,0.75,na.rm = TRUE),
                       fonc_n = ~sum(.==1,na.rm = TRUE), 
                       fonc_pourcent = ~mean(.==1,na.rm = TRUE)*100,
                       fonc_nb_NA = ~sum(is.na(.))
    )
    ) %>% 
    pivot_longer(cols =  everything(),
                 names_to = c(".value", "level"),
                 names_pattern = "(.*)_fonc_(.*)") %>% 
    t.df(.,"level") 
  return(res)}