ilec_shap_plot <- function(shp, 
                           maineffect,
                           interactions,
                           resp_var,
                           resp_offset,
                           train.data,
                           which.shap=maineffect,
                           plotActualOffsetRatio=TRUE,
                           plotModelOffsetRatio=TRUE,
                           trans="identity")
{
  retVal <- list()
  
  shp_x_cols <- c(maineffect,interactions)
  
  data.table(
    cbind(
      shp$X[,..shp_x_cols],
      shap=shp$S[,c(which.shap)] + shp$baseline,
      response=train.data[[resp_var]],
      offset=train.data[[resp_offset]]
    )
  ) -> 
    shp.plot.dat
  
  v1 <- sym(maineffect)
  
  pmain <- shp.plot.dat %>%
    ggplot(aes(x=!!v1)) +
    geom_boxplot(aes(y=exp(shap)),
                 outlier.size = 0.1)
  
  if(plotActualOffsetRatio) {
    pmain <- pmain + geom_point(data=train.data[,.(a_e=sum(get(resp_var))/sum(get(resp_offset))),
                               by=c(maineffect)],
               aes(y=a_e),
               position=position_dodge(width=0.75),
               shape=15, #square
               size=4,
               color="blue"
    )
  }
  
  if(plotModelOffsetRatio) {
    pmain <- pmain + geom_point(data=shp.plot.dat[,.(mean_shap=sum(exp(shap)*offset)/sum(offset)),
                                 by=c(maineffect)],
               aes(y=mean_shap),
               position=position_dodge(width=0.75),
               shape=18, #diamond
               size=4,
               color="red"
    )
  }
    
  pmain <- pmain +
    scale_color_viridis_d(guide="none") +
    scale_fill_viridis_d(guide="none") +
    scale_y_continuous(limits = c(0,NA),
                       labels = scales::percent,
                       name="Exponentiated SHAP Value (% relative to Offset)",
                       trans=trans) +
    #geom_hline(yintercept=1, linetype=2) +
    ggtitle(label=paste0("Boxplot of ", which.shap ," SHAP values for Effect ",maineffect)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle=45),
      
    )
  
  retVal[[paste0("main effect: ",maineffect)]] <- pmain
  
  for(i in seq_along(interactions)) {
    vint <- sym(interactions[i])
    
    retVal[[ paste0(maineffect," x ", interactions[i]) ]] <- shp.plot.dat %>%
      ggplot(aes(x=!!v1,color=!!vint)) +
      geom_boxplot(aes(y=exp(shap)),
                   outlier.size = 0.1) +  
      scale_color_viridis_d() +
      scale_fill_viridis_d(guide="none") +
      scale_y_continuous(limits = c(0,NA),
                         labels = scales::percent,
                         name="Exponentiated SHAP Value (% relative to Offset)",
                         trans=trans) +
      ggtitle(label=paste0("Boxplot of ", which.shap ," SHAP values for Effect ",maineffect),
              subtitle = paste0("by ",interactions[i])) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle=45),
        legend.position = "bottom"
      )
  }
  
  return(retVal)
  
}
