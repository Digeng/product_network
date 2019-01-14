library(tidyverse)
library(docstring)
Rcpp::sourceCpp("./src/customer/platform_waterfall.cpp")

get_customer_trending_df <- function() {

  customer_trending_file <-
    "./data/customer/cust_trending_10_16_2018.csv"
  
  df_cust <-
    read_csv(customer_trending_file,
             col_types = cols(`Internal ID` = col_character())) %>%
    janitor::clean_names() %>%
    mutate(snapshot_per_ending = as.Date(snapshot_per_ending, "%m/%d/%Y"))
  
  class(df_cust) <- append(class(df_cust), "df_cust_raw")
  
  df_cust
}

save_data <- function(data_obj, path_name) {
  feather::write_feather(data_obj$preprocess_df_cust(), path_name)
}

get_customer_trending_df_memoise <-
  memoise::memoise(get_customer_trending_df)



Customer_Trending <- function(data) {
  #'closure as a 'pseudo' class.
  #'Transforms raw customer_trending dataset to create customer & product flows
  #'@param data raw customer trending file. 
  #'@return function that transforms data into desired form. 
  
  assertthat::assert_that(inherits(data, "df_cust_raw"))
  
  PRODUCT_COLS <-
    data %>% select(metasploit:insightops) %>% colnames()
  ATTRIBUTE_COLS <- c(
    "internal_id",
    "company_name",
    "snapshot_per_ending",
    "csm_tier",
    "segment",
    "reporting_division",
    "director_level_team",
    "platform_waterfall_type"
  )
  
  ## HELPERS------------------------------------------------------------------------
  consolidate_product_cols <- function(df_cust) {
    #'consolidates or regroups product columns into broader product category.   
    df_cust %>%
      mutate(
        nexpose = pmin(
          1,
          nexpose_license + nexpose_all_nx - managed_service_nexpose
        ),
        idr = pmin(insightidr + insightuba, 1),
        services = pmin(sas_services + services + tem_services + idr_services, 1)
      ) %>%
      
      select(
        -appspider_license,
        
        -nexpose_all_nx,
        -nexpose_license,
        
        -insightidr,
        -insightuba,
        -uba_idr_ar,
        
        -pss_asv,
        
        -sas_services,
        -tem_services,
        -idr_services
      )
  }
  
  
  calculate_platform_waterfall_type <- function(df_cust) {
    #'looks at each customer's quarter over quarter product composition flow to determine platform waterfall type. 
    #'Either platform add, platform loss, or platform no change. 
    #'@return transformed input data.
    
    #---
    df_cust$snapshot_per_ending <- as.Date(df_cust$snapshot_per_ending)
    assertthat::assert_that(lubridate::is.Date(df_cust$snapshot_per_ending))
    #---
    
    df_cust %>%
      group_by(internal_id) %>%
      mutate(platform = ifelse(is.na(platform), 0, platform)) %>%
      mutate(platform_lag = lag(platform, order_by = snapshot_per_ending)) %>%
      ungroup() %>%
      mutate(platform_lag = ifelse(is.na(platform_lag), 0, platform_lag)) %>%
      mutate(platform_waterfall_type = calculate_platform_waterfall_type_cpp(platform, platform_lag))
  }
  
  
  create_product_super_group <- function(df_cust) {
    supergroup_map <- new.env()
    
    supergroup_map[["nexpose"]] <- "nx_ivm"
    supergroup_map[["insightvm"]] <- "nx_ivm"
    supergroup_map[["managed_service_nexpose"]] <- "nx_ivm"
    supergroup_map[["mobilisafe"]] <- "nx_ivm"
    
    supergroup_map[["insightidr"]] <- "idr"
    supergroup_map[["analytic_response"]] <- "idr"
    supergroup_map[["insight_uba"]] <- "idr"
    
    supergroup_map[["it_search"]] <- "insightops"
    
    supergroup_map[["appspider"]] <- "app_sec"
    supergroup_map[["insightappsec"]] <- "app_sec"
    supergroup_map[["managed_service_appspider"]] <- "app_sec"
    
    
    df_cust$product_super_group <- lapply(df_cust$product,
                                          function(x) {
                                            res <- supergroup_map[[x]]
                                            ifelse(is_null(res), x, res)
                                          }) %>% unlist()
    
    df_cust %>%
      mutate(snapshot_per_ending = as.character(snapshot_per_ending))
  }
  
  
  calc_platform <- function(df_cust) {
    #'calls calculate_platform_waterfall_type and then filters for only necessary rows and columns.
    df_cust <- df_cust %>%
      calculate_platform_waterfall_type() %>%
      filter(customer == 1) %>%
      select_at(vars(c(ATTRIBUTE_COLS,
                       PRODUCT_COLS))) %>%
      distinct()
  }
  
  
  gather_product_cols <- function(df_cust) {
    #'transforms dataset into long format. (i.e, unpivots on product cols).
    df_cust %>% gather_(
      key = "product",
      value = "owns_product",
      gather_cols = setdiff(names(.),
                            ATTRIBUTE_COLS)
    )
  }
  
  
  summarize_super_group <- function(df_cust) {
    df_cust %>%
      group_by_at(vars(c(
        "product_super_group", ATTRIBUTE_COLS
      ))) %>%
      summarize(owns_product = max(owns_product, na.rm = TRUE)) %>%
      mutate(owns_product = ifelse(owns_product < 0, 0, owns_product)) %>%
      ungroup()
  }
  
  
  calc_owns_services <- function(df_cust) {
    #'set owns_product to zero for services. 
    df_cust %>%
      mutate(owns_service = if_else(product_super_group == "services" &
                                      owns_product == 1, 1, 0)) 
  }
  
  adjust_services_owns_product <- function(df_cust) {
    df_cust %>% 
      mutate(owns_product = if_else(product_super_group == "services", 0, owns_product))
  }
  
  adjust_platform_type <- function(df_cust) {
    #'adjusts for idr incorrectly labeled as non-platform. 
    
    df_cust %>% group_by(internal_id, snapshot_per_ending) %>%
      mutate(temp_sum = sum(owns_product)) %>%
      ungroup() %>%
      mutate(
        owns_product = if_else(
          product_super_group == "idr" &
            temp_sum == 0 &
            platform_waterfall_type == "customer_add",
          1,
          owns_product
        )
      ) %>%
      select(-temp_sum)
  }
  
  
  calc_quarter_of_date <- function(df_cust){
    date_to_quarter <-
      memoise::memoise(utilsDigeng::calculate_quarter_of_date)
    
    df_cust <- df_cust %>%
      mutate(quarter = date_to_quarter(as.Date(snapshot_per_ending)))
  }
  
  set_na_to_unidentified <- function(df_cust) {
    df_cust %>% map_df( ~ {
      if (is.character(.x)) {
        .x[is.na(.x)] <- "unidentified"
      }
      .x
    })
  }
  
  
  remove_zero_prod_cust <- function(df_cust) {
    #'remove customers that have zero products in all quarters.
    df_zero_prod <- df_cust %>%
      group_by(internal_id, snapshot_per_ending) %>%
      summarize(n_prod = sum(owns_product[product_super_group != "services"], na.rm = TRUE)) %>%
      ungroup() %>%
      filter(n_prod == 0) %>%
      select(internal_id, snapshot_per_ending)
    
    df_cust <- df_cust %>% anti_join(df_zero_prod)
  }
  
  get_customer_attributes <- function(df_cust){
    utilsDigeng::connect_to_Domo()
    waterfall_late_adjusted <- "55a2a1a9-f297-478d-9989-f11738eb402a"
    df_wf <- DomoR::fetch(waterfall_late_adjusted) %>% 
      select(customerid, cohort, cohort_quarter) %>% 
      distinct()
    
    df_cust <- df_cust %>% left_join(df_wf, by = c("internal_id" = "customerid"))
    df_cust[, c("cohort", "cohort_quarter")] <- lapply(df_cust[,c("cohort", "cohort_quarter")], 
                                                       function(x) { 
                                                         ifelse(is.na(x), "unidentified", x)
                                                         })
    df_cust
  }
  

  ## PUBLIC -----------------------------------------------------------------------
  preprocess_df_cust <- function(df_cust_raw) {
    df_cust <- purrr::reduce(
      .init = df_cust_raw,
      .x = list( 
        calc_platform,
        consolidate_product_cols,
        gather_product_cols,
        create_product_super_group,
        summarize_super_group,
        calc_owns_services,
        adjust_services_owns_product,
        adjust_platform_type,
        calc_quarter_of_date,
        set_na_to_unidentified,
        remove_zero_prod_cust,
        get_customer_attributes
      ),
      .f = function(df, fun)
        fun(df)
    )
    
    class(df_cust) <- append(class(df_cust), "preprocessed_df_cust")
    
    df_cust
  }
  
  preprocess_df_cust_memo <- memoise::memoise(preprocess_df_cust)
  
  
  
  structure(list(preprocess_df_cust = purrr::partial(preprocess_df_cust_memo, df_cust_raw = data)), 
            class = "preprocessed_customer_trending")
}