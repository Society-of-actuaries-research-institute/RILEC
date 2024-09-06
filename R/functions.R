#==================================================================================================#
#### Section 0: Functions ####
#==================================================================================================#

#-----------------------------------------#
##### Weighted Poisson Deviance       #####
#-----------------------------------------#

poisson_dev <- function(y, u, w) {
  ## returns weighted poisson deviance
  ## y = actual 
  ## u = predicted mean
  ## w = weights
  
  ind_loss = poisson()$dev.resid(y,u,w)
  agg_loss = 2*(sum(w*ind_loss))
  
  return(agg_loss)
}

#-----------------------------------------#
##### Validation Metrics #####
#-----------------------------------------#

val <- function(y, u, w) {
  ## returns Weighted MSE, MAE, Deviance as a vector
  ## y = actual 
  ## u = predicted mean
  ## w = weights
  mse = sum(w*(y-u)^2)/sum(w)
  mae = sum(w*abs(y-u))/sum(w)
  dev = poisson_dev(y, u, w)/sum(w)
  
  return(data.frame(mse=mse,mae=mae,dev=dev))
}

#-----------------------------------------#
##### Lorenz Plot #####
#-----------------------------------------#

lorenz <- function(actual, prediction, weight) {
  ## returns a lorenz curve: cum exposures vs cum claims
  ## requires dplyr library
  ## actual = actual response value
  ## prediction = model prediction
  ## weights = model weights / exposures
  
  # df <- data.frame(actual=actual, prediction=prediction, weight=weight)
  # 
  # df <- df %>% mutate(rank = dense_rank(prediction))
  # df <- df %>% arrange(rank) %>% mutate(sum_exp = cumsum(weight)/sum(weight),
  #                                       sum_claims = cumsum(actual)/sum(actual))
  # 
  # plot(df$sum_exp, df$sum_claims, xlab='cumulative exposures', 
  #      ylab='cumulative claims', main = 'Lorenz Curve', type='l')
  # abline(0,1)
  
  dt <- data.table(actual=actual, prediction=prediction, weight=weight) %>%
    arrange(dense_rank(prediction)) %>%
    mutate(sum_exp=cumsum(as.numeric(weight))/sum(as.numeric(weight)),
           sum_claims=cumsum(as.numeric(actual))/sum(as.numeric(actual))) %>%
    as.data.table()
  
  # Trapezoidal integration
  curve_int <- sum(
    ((dt[1:(nrow(dt)-1),sum_claims] + dt[2:nrow(dt),sum_claims])/2)*
      dt[,diff(sum_exp)]
    )
  
  dt %>%
    ggplot(aes(x=sum_exp,y=sum_claims)) +
    geom_line() +
    ggtitle("Lorenz Curve") +
    scale_x_continuous(name="% cumulative exposures",
                       labels=scales::percent) + 
    scale_y_continuous(name="% cumulative response",
                       labels=scales::percent) +
    geom_segment(x=0,y=0,xend=1,yend=1) +
    annotate("text", x=0.25,y=0.75,label=paste0("Gini Coeff: ",round((0.5-curve_int)/curve_int,2))) +
    theme_minimal()
  
}

#-----------------------------------------#
##### Decile Table #####
#-----------------------------------------#

decile.table <- function(actual, prediction, weight) {
  ## returns a decile table of values for lift chart
  ## actual = actual response value
  ## prediction = model prediction
  ## weights = model weights / exposures
  
  # df <- data.frame(actual=actual, prediction=prediction, weight=weight)
  # 
  # df <- df %>% mutate(rank = dense_rank(prediction)) %>% arrange(rank)
  # df <- df %>% mutate(cumweight=cumsum(weight)/sum(weight))
  # df$decile <- ceiling(df$cumweight*10)
  # 
  # decile <- df %>% group_by(decile) %>% 
  #   summarize(exposures = sum(weight), 
  #             actual = sum(actual),
  #             table = sum(weight),
  #             model = sum(weight*prediction))
  # return(decile)
  
  return(
    data.table(actual=actual, prediction=prediction, weight=weight) %>%
      arrange(dense_rank(prediction)) %>%
      mutate(cumweight=cumsum(weight)/sum(weight)) %>%
      mutate(decile=ceiling(cumweight*10)) %>%
      group_by(decile) %>%
      summarize(exposures=sum(weight),
                actual=sum(actual),
                table=sum(weight),
                model=sum(weight*prediction)) %>%
      as.data.table()
  )
  
}

#-----------------------------------------#
##### Generate formatting for tabsets #####
#-----------------------------------------#

generate_tabset <- function(tabs,
                            tabtitle = "",
                            tablevel = 1) {
  # markdown <- paste0(
  #   paste(rep("#",tablevel),collapse=""),
  #   " ",
  #   tabtitle,
  #   " ",
  #   "{.tabset}\n\n")
  
  # markdown <- "::: panel-tabset"
  markdown <- ""
  
  for (i in seq_along(tabs)) {
    title <- names(tabs)[i]
    content <- tabs[[i]]
    markdown <- paste0(markdown, 
                       paste(rep("#",tablevel + 1),collapse=""),
                       " ", 
                       title, 
                       "\n\n", 
                       content, 
                       "\n\n")
  }
  # markdown <- paste0(markdown,"\n",":::")
  return(markdown)
}

export_tables_to_excel <- function(output_tables,
                                   file) {
  wb <- wb_workbook()
  
  
  for(i in seq_along(output_tables)) {
    sheetName <- substr(names(output_tables)[i],1,31)
    wb$add_worksheet( sheetName )
    wb <- wb_add_flextable(
      wb,
      sheetName,
      output_tables[[i]]
    )
  }
  
  wb$save(file)
}