# Preps the data for use in elastic net modeling
#
# formula - formula for the model matrix
# data - data frame from which to build the model matrix, response, offset, and weights
# predictors - predictor columns to use
# offset - offset to use
# weights - weights to use
# foldid - for supplying a custom fold id for elastic net
# useSpace - uses sparse matrices when building the model matrix
# dropunused - drops unused columns

prepELData <- function( formula, data, predictors, response, offset=NULL, weights=NULL, 
                        foldid=NULL, useSparse=FALSE, dropunused=FALSE ) {
  
  if(is.null(formula))
    stop("formula must be provided")
  if(is.null(data))
    stop("data must be provided")
  if(is.null(response))
    stop("response must be provided")
  
  # if(useSparse) {
  #   outMat <- sparse.model.matrix(
  #     formula,
  #     data[,..predictors]
  #   )
  # } else {
  #   outMat <- model.matrix(
  #     formula,
  #     data[,..predictors]
  #   )
  # }
  
  outMat <- model.Matrix(
    formula,
    data[,..predictors],
    sparse=useSparse,
    drop.unused.levels = dropunused
  )

  outResp <- as.matrix(data[[response]])
  
  if(is.null(offset))
    outOffset <- NULL
  else
    outOffset <- as.matrix(data[[offset]])
  
  if(is.null(weights))
    outWeights <- NULL
  else
    outWeights <- as.matrix(data[[weights]])
  
  if(is.null(foldid))
    outFoldID <- NULL
  else
    outFoldID <- as.matrix(data[[foldid]])
  
  list(input.matrix=outMat,
       response=outResp,
       offset = outOffset,
       weights = outWeights,
       foldid = outFoldID)
}

# Fits the cross-validated elastic net model
#
# datalist - data list from prepELData
# alpha - alpha parameter for glmnet
# nfolds - specification of fold id information

fitCVGLMNet <- function(datalist,alpha=0.5,
                        nfolds=ifelse(is.null(datalist$foldid),5,max(datalist$foldid))) {
  cl <- makeCluster(nfolds)
  registerDoParallel(cl)
  
  #glmnet.control(itrace = 1)
  
  cvfit <- try(
    {
      cv.glmnet(
        x=datalist$input.matrix,
        y=datalist$response,
        offset=datalist$offset,
        weights=datalist$weights,
        family="poisson",
        alpha=alpha,
        nfolds=nfolds,
        foldid = datalist$foldid,
        parallel=T,
        standardize=F)
    }
  )

  
  stopCluster(cl)
  
  cvfit
}

# https://stackoverflow.com/questions/6457290/how-to-check-the-amount-of-ram
# Queries the available memory in R
# Retained from prior work, no longer used
available_memory <- function()
{
  
  # Get operating system
  OS <- tolower(Sys.info()["sysname"])
  
  # Branch based on OS
  if(OS == "windows"){ # Windows
    
    # System information
    system_info <- system("systeminfo", intern = TRUE)
    
    # Get available memory
    value <- system_info[
      grep("Available Physical Memory", system_info)
    ]
    
    # Remove extraneous information
    value <- gsub("Available Physical Memory: ", "", value)
    value <- gsub("\\,", "", value)
    
    # Convert to bytes
    value_split <- unlist(strsplit(value, split = " "))
    
    # Check for second value
    bytes <- as.numeric(value_split[1]) * switch(
      value_split[2],
      "KB" = 1e03,
      "MB" = 1e06,
      "GB" = 1e09
    )
    
  }else if(OS == "linux"){ # Linux
    
    # Split system information
    info_split <- strsplit(system("free -b", intern = TRUE), split = " ")
    
    # Remove "Mem:" and "Swap:"
    info_split <- lapply(info_split, function(x){gsub("Mem:", "", x)})
    info_split <- lapply(info_split, function(x){gsub("Swap:", "", x)})
    
    # Get actual values
    info_split <- lapply(info_split, function(x){x[x != ""]})
    
    # Bind values
    info_split <- do.call(rbind, info_split[1:2])
    
    # Get free values
    bytes <- as.numeric(info_split[2, info_split[1,] == "free"])
    
  }else{ # Mac
    
    # System information
    system_info <- system("top -l 1 -s 0 | grep PhysMem", intern = TRUE)
    
    # Get everything after comma
    unused <- gsub(" .*,", "", system_info)
    
    # Get values only
    value <- gsub("PhysMem: ", "", unused)
    value <- gsub(" unused.", "", value)
    
    # Check for bytes
    if(grepl("M", value)){
      bytes <- as.numeric(gsub("M", "", value)) * 1e06
    }else if(grepl("G", value)){
      bytes <- as.numeric(gsub("G", "", value)) * 1e09
    }else if(grepl("K", value)){
      bytes <- as.numeric(gsub("K", "", value)) * 1e03
    }
    
  }
  
  # Return bytes
  return(bytes)
  
}

# Reformats the coefficient table from the CV glm fit object,
# splitting the coefficient name into factor name and level
reformatCoefs <- function(cvfit,pred.cols,s="lambda.min") {
  coef(cvfit,s=s) %>%
    as.matrix() %>%
    as.data.table(keep.rownames=T) %>%
    setnames(c("rn","s1"),c("CoefName","Coef")) ->
    coefs 
  
  coefs[,c("Feature1","Feature2") := tstrsplit(CoefName,":")]

  walk(pred.cols,\(x) 
       coefs[startsWith(Feature1,x),
                        `:=`(Feature1Name=x
                        )]
  )
  
  coefs[,
                   Feature1Level:=mapply(
                     function(f,fname) {
                       str_sub(
                         f,
                         end=-1,
                         start=nchar(fname)-nchar(f)
                       )
                     },
                     Feature1,
                     Feature1Name,
                     SIMPLIFY = "vector"
                   )]
  
  walk(pred.cols,\(x) 
       coefs[startsWith(Feature2,x),
                        `:=`(Feature2Name=x
                        )]
  )
  
  coefs[,
                   Feature2Level:=mapply(
                     function(f,fname) {
                       str_sub(
                         f,
                         end=-1,
                         start=nchar(fname)-nchar(f)
                       )
                     },
                     Feature2,
                     Feature2Name,
                     SIMPLIFY = "vector"
                   )]
  
  coefs[Feature1=="(Intercept)",
                   `:=`(Feature1Name="(Intercept)",
                        Feature1Level="(Intercept)")]
  
  coefs[,`:=`(Feature1=NULL, Feature2=NULL)]
  
  coefs
}

# Which vector level should we pick, useful when building the
# plots and tables of coefficients where we need to fix
# a factor level for variables outside the plot or table.
#
# x - vector of values
# whichlevel - "mid" chooses the midpoint, "first" chooses the first, and
#              "last" chooses the last; parameter is required to be one of these
pluckVectorLevel <- function(
    x,
    whichlevel = "mid"
) {
  
  stopifnot(whichlevel %in% c("mid","first","last"))
  
  if(whichlevel == "first") 
    retval <- x[1]
  else if(whichlevel == "last")
    retval <- x[length(x)]
  else if(whichlevel == "mid")
    retval <- x[(1+length(x))/2]
  
  retval
}

# Build a frame to filter the big table of coefficients
#
# model.grid - the grid of coefficients to work over
# vars - the variables we wish to retain all levels for
# pred.cols  - the columns to restrict to when working with the model.grid
# levellist  - list of levels to manually specify for external variables not
#             mentioned in vars; must be in the levels for the factor,
#             otherwise just uses the choice for whichlevel
# whichlevel - default choice of external variable not mentioned in vars and not
#             mentioned in levellist
buildFilterFrame <- function(
    model.grid,
    vars,
    pred.cols,
    levellist = NULL,
    whichlevel = "mid"
) {
  model.grid[,..pred.cols] %>%
    lapply(levels) %>%
    imap(.f=\(x,idx) {
      if(idx %in% vars) { # Want all the levels for the main variables
        return(x)
      }
      
      if(inherits(levellist,"list")) {
        if( !is.null( levellist[[idx]] ) ) {
          if( levellist[[idx]] %in% x ) 
            return( levellist[[idx]] )
        }
      }
      
      return(pluckVectorLevel(x,whichlevel))
      
    }) %>%
    expand.grid() %>%
    setDT() ->
    filter.frame
  
  filter.frame
}

# Generates a plot for the coefficients, fixing levels at specified points for
# factors not mentioend in vars, only performs bivariate plots
#
# model.grid - the grid of coefficients to work over
# vars       - the variables we wish to retain all levels for, must be a vector of
#              length 2
# pred.cols  - the columns to restrict to when working with the model.grid
# levellist  - list of levels to manually specify for external variables not
#             mentioned in vars; must be in the levels for the factor,
#             otherwise just uses the choice for whichlevel
# whichlevel - default choice of external variable not mentioned in vars and not
#             mentioned in levellist

plotCVNetCoefs <- function(
    model.grid,
    vars,
    factorcol,
    pred.cols,
    levellist = NULL,
    whichlevel = "mid"
) {
  
  v1 <- sym(vars[1])
  v2 <- sym(vars[2])
  vf <- sym(factorcol)
  
  filter.frame <- buildFilterFrame(model.grid,
                                   vars,
                                   pred.cols,
                                   levellist,
                                   whichlevel)
  
  filter.frame %>%
    select(!c(!!v1,!!v2)) %>%
    distinct() %>%
    imap( .f = \(x,idx) paste0(idx,": ",x)) %>%
    paste(collapse=", ") %>%
    paste0("Other fixed variables - ",.) ->
    st
  
  model.grid %>%
    inner_join(filter.frame) %>%
    ggplot(aes(x=!!v1,y=!!vf)) +
    geom_point() +
    facet_wrap(vars(!!v2)) +
    scale_y_continuous(labels = scales::percent) +
    geom_hline(yintercept=1,linetype=2) +
    ggtitle(label=paste0("Coefficients for ",vars[1], " by ", vars[2]),
            subtitle = stringr::str_wrap(st,60,exdent=5,whitespace_only = F) ) +
    theme_minimal() + 
    theme(
      axis.text.x = element_text(angle=90)
    )
}

# Generates a table for the coefficients, fixing levels at specified points for
# factors not mentioend in vars, only performs bivariate table generation
#
# model.grid - the grid of coefficients to work over
# vars       - the variables we wish to retain all levels for, must be a vector of
#              length 2
# pred.cols  - the columns to restrict to when working with the model.grid
# levellist  - list of levels to manually specify for external variables not
#             mentioned in vars; must be in the levels for the factor,
#             otherwise just uses the choice for whichlevel
# whichlevel - default choice of external variable not mentioned in vars and not
#             mentioned in levellist
tableCVNetCoefs <- function(
    model.grid,
    vars,
    factorcol,
    pred.cols,
    levellist = NULL,
    whichlevel = "mid"
) {
  
  v1 <- sym(vars[1])
  v2 <- sym(vars[2])
  vf <- sym(factorcol)
  
  filter.frame <- buildFilterFrame(model.grid,
                                   vars,
                                   pred.cols,
                                   levellist,
                                   whichlevel)
  
  filter.frame %>%
    select(!c(!!v1,!!v2)) %>%
    distinct() %>%
    imap( .f = \(x,idx) paste0(idx,": ",x)) %>%
    paste(collapse=", ") %>%
    paste0("Other fixed variables - ",.) ->
    st
  
  model.grid %>%
    inner_join(filter.frame) %>%
    select(!!v1,!!v2,Factor) %>%
    pivot_wider(names_from=!!v2,
                values_from=Factor)
}
