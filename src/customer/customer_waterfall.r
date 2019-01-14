Customer_Waterfall <- function() {
  #this dataset, when compared to domo waterfall dataset, removes previous_quarter type, and includes 'no change' type.
  data <- feather::read_feather("./data/customer/waterfall_12_03_2018.feather") 
  
  data <- data %>% rename(internal_id = customerid, 
                  csm_tier = csm_customer_group) %>% 
    filter(arr > 0) %>%
    filter(!(type %in% c("previous_quarter", "customer_churn"))) %>%
    filter(!(product_super_group %in% c("other", "pso", "unidentified")))
  
  data <- data %>% 
    mutate(owns_product = ifelse(arr > 0, 1, 0)) %>%
    mutate(product_super_group = recode(product_super_group, userinsight = "idr")) 
  
  class(data) <- purrr::reduce(.init = class(data), list("preprocessed_df_cust", "preprocessed_waterfall"), append)  
  data
}

save_waterfall_data <- function(waterfall_data) {
  assertthat::assert_that(inherits(waterfall_data, "preprocessed_waterfall"))
  
  feather::write_feather(waterfall_data, "./data/customer/preprocessed_waterfall.feather")
} 