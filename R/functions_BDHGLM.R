## This one just puts A/E ratios in a good format. 
### df: dataframe, e.g., ilecData
### rf: risk factor of interest, e.g., products
### model: GLM model, e.g., modelCnt
aeSummary <- function(df, rf,resp,offset) {
  resp_v <- as.symbol(eval(resp))
  off_v <- as.symbol(eval(offset))
  rf_v <- as.symbol(eval(rf))
  
  df %>% 
    group_by(across({{rf}})) %>% 
    summarize(
      actual = sum({{resp_v}}, na.rm = TRUE),
      basis  = sum({{off_v}}, na.rm = TRUE)
    ) %>% 
    mutate(
      `A/E` = actual/basis,
      actual = NULL,
      basis = NULL
    ) %>% 
    pivot_wider(
      names_from = {{rf_v}}, 
      values_from = `A/E`
    ) %>%
    mutate(
      rowname = offset
    ) %>%
    select(
      rowname,
      everything()
    ) %>%
    as.data.table()
}



## get the coefficients from GLM models

allCoef <- function(df, model) {
  df %>% 
    Filter(is.factor, .) %>% 
    map(levels) %>% 
    enframe() %>% 
    unnest(
      cols = c(value)
    ) %>% 
    mutate(
      predictor = name,
      observation = value,
      value = paste0(name, value)
    ) %>% 
    merge(
      coefficients(model) %>% 
        enframe( 
          name = "predictor", 
          value = "coefficient"
        ) 
      , by.x = c('value'), by.y = c('predictor'), all.x = TRUE, na.action()
    ) %>% 
    mutate(
      coefficient = if_else(is.na(coefficient), 1, exp(coefficient),2)
    ) 
}

### dfCoef: coefficients returned from allCoef function
get_coef_row <- function(dfCoef, rf) {
  dfCoef %>% 
    filter(predictor == rf) %>% 
    select(observation, coefficient) %>%
    pivot_wider(
      names_from = observation, 
      values_from = coefficient
    ) %>% mutate(
      rowname = paste0("Factor: ", rf)
    ) %>% select(rowname, names(.)) %>%
    as.data.table()
}  

## Create a table of aggregate expected (e.g., 15vbt) in two dimensions
### dimension: largely means other predictors than "rf", the predictor of interest.
mExpected <- function(df, dfCoef, rf, dimension, resp, offset) {
  resp_v <- as.symbol(eval(resp))
  off_v <- as.symbol(eval(offset))
  rf_v <- as.symbol(eval(rf))
  dim_v <- as.symbol(eval(dimension))
  
  df1 <- df %>%
    group_by(across(all_of(c(!!! rf,!!! dimension)))) %>%
    summarize(
      measure = sum({{off_v}}, na.rm = TRUE), 
    )  %>% 
    pivot_wider(
      names_from = {{rf_v}}, 
      values_from = measure,
      values_fill = 0
    ) %>% as.data.table()
  
  df1 %>% mutate(
      predictor :=  dimension,
      observation =  {{dim_v}}
    ) %>% select(-c({{dimension}})) %>% 
    merge(
      dfCoef, 
      by = c("predictor", "observation")
    ) %>% 
    as.data.table()
}

## Calculate average factors
avgCoef <- function(df, dfCoef, rf, resp, offset) {
  resp_v <- as.symbol(eval(resp))
  off_v <- as.symbol(eval(offset))
  rf_v <- as.symbol(eval(rf))
  
  tibble(
    others = setdiff(Filter(is.factor,df) %>% names(),rf)
  ) %>% 
    mutate (
      expMatrix = map(
        others, 
        function(predictor) {
          mExpected(df, dfCoef, rf, predictor, resp, offset)}
      )  
    ) %>% 
    unnest(cols = expMatrix)  %>% 
    select(-c("name", "others", "value")) %>%
    pivot_longer(
      cols = -c(predictor, observation, coefficient), 
      names_to = "dimension", 
      values_to = "value"#, 
    ) %>%
    group_by(
      dimension, predictor
    ) %>% 
    summarize(
      avg = sum(coefficient * value, na.rm = TRUE)/sum(value, na.rm = TRUE)
    ) %>% pivot_wider (
      names_from = dimension, 
      values_from = avg
    ) %>%
    rename(
      rowname = predictor
    )%>%
    mutate(rowname = paste0("Ave Fac: ", rowname)) %>%
    as.data.table()
}

# this is the main funtion to produce tables for further analysis
mainF <- function(df, model, rf, resp, offset) {
  coefs <- allCoef(df, model)
  rbind(
    aeSummary(df, rf,resp,offset),
    get_coef_row(coefs, rf),
    avgCoef(df, coefs, rf, resp, offset)
  ) 
}