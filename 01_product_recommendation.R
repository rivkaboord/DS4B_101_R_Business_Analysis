# DS4B 101-R ----
# PRODUCT RECOMMENDATION FUNCTIONS

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)
library(parsnip)
library(plotly)

source("../Documents/Programming and Data Science/R/DS4B_101_R_Business_Analysis/00_scripts/separate_bikes_and_outlier_detection.R")

bike_orderlines_tbl <- read_rds("../Documents/Programming and Data Science/R/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

models_tbl <- read_rds("../Documents/Programming and Data Science/R/DS4B_101_R_Business_Analysis/00_models/parsnip_models_tbl.rds")


# 2.0 BIKE FEATURES ----

get_bike_features <- function() {
    
    bike_features_tbl <- bike_orderlines_tbl %>% 
        
        select(price, model, category_1, category_2, frame_material) %>% 
        
        distinct() %>% 
        
        mutate(id = row_number()) %>% 
        
        select(id, everything()) %>% 
        
        separate_bike_model(keep_model_column = TRUE, append = TRUE)
    
    return(bike_features_tbl)
}

get_bike_features()

plot_bike_features <- function(interactive = TRUE) {
    
    # DATA MANIPULATION
    bike_features_tbl <- get_bike_features()
    
    # VISUALIZATION
    g <- bike_features_tbl %>% 
        
        mutate(category_2 = fct_reorder(category_2, price)) %>% 
        mutate(label_text = str_glue("Model: {model}
                                     Price: {scales::dollar(price)}")) %>% 
        
        ggplot(aes(x = category_2, y = price)) +
        geom_violin() +
        geom_jitter(aes(text = label_text), width = 0.1, color = "#2c3e50", alpha = 0.5) +
        facet_wrap(~ frame_material) +
        coord_flip() +
        
        theme_tq() +
        theme(strip.text.x = element_text(margin = margin(5,5,5,5, unit = "pt"))) +
        labs(title = "Product Gap Analysis",
             x = "",
             y = "") +
        scale_y_continuous(labels = scales::dollar_format())
    
    # INTERACTIVE VS. STATIC
    if(interactive) {
        
        ggplotly(g, tool = "text")
        
    } else {
        return(g)
    }
}

plot_bike_features()
plot_bike_features(interactive = FALSE)


# 3.0 SAVE FUNCTIONS ----

function_names <- c("get_bike_features", "plot_bike_features")

dump(function_names, file = "../Documents/Programming and Data Science/R/DS4B_101_R_Business_Analysis/00_scripts/plot_product_recommendation.R")
