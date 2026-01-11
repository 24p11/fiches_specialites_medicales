filter_df<-function(df,filters){
  
  for(arg in names(filters)){
    
    if(length(filters[[arg]])==1){
      
        if(is.na(filters[[arg]])){

            filter_text ="is.na(" %+% arg %+% ")"
          
        }else{
        
          filter_text = arg %+% '== "' %+% filters[[arg]] %+% '"'
        }
      
    }else{
      
      filter_text = arg %+% '%in%c("' %+% paste0(filters[[arg]],collapse = '","') %+% '")'
    }
    
    df<- df %>% dplyr::filter(eval(parse( text = filter_text )))
   
     
  }
  
  return(df)
} 


get_df_hist_ghu<-function(df,ghu_ref,age_ref,indicateur_ref,specialite,groups = NULL){
  
  if(specialite == "A-Total"){
    
    niveau_filter = c("Indicateurs - GHU","Indicateurs - Categ")
    
  }else{
    
    niveau_filter = c("Spéclialité - GHU","Spéclialité - Categ")
    
  }
  df |> dplyr::filter(age2 == age_ref,
                      niveau %in% niveau_filter,
                      ghu == ghu_ref | is.na(ghu) ,
                      lib_spe_uma == specialite,
                      indicateur == indicateur_ref) |> 
    dplyr::mutate(categ = ifelse(!is.na(ghu),ghu,categ)) |> 
    dplyr::select(categ,p,annee) |> 
    tidyr::pivot_wider(names_from = annee,values_from = p)-> df_hist
  
  if(!is.null(groups)){
    df_hist |> dplyr::mutate(categ = factor(categ,levels = groups)) -> df_hist
  }
  
  return(df_hist)
}

get_hist<-function(df_hist){
  
  fig <- plot_ly(df_hist, x = ~categ, y = ~`2018`, type = 'bar', name = '2018',
                 text = df_hist$`2018`, textposition = 'auto')
  fig <- fig %>% add_trace(y = ~`2024`, name = '2024',
                           text = df_hist$`2024`, textposition = 'auto')
  
  return(fig)
  
  
}



get_bar_plot<-function(d_groupe,d_groupe_tot,d_groupe_limites,colors_define){

  if(length(colors_define)<5){
    
    y_leg = max(d_groupe_tot$tot)
    
  }else{
    
    y_leg = max(d_groupe_tot$tot)*1.1
    
  }
  
  
  plot <- ggplot() +
    geom_col(data = d_groupe, aes(x = annee, fill = categ, y = nb), alpha = 0.7) +
    geom_text(data = d_groupe,
              aes(x = annee, group = categ,y = nb, label = fl),
              size = 3,
              position = position_stack(vjust = 0.5)) +
    geom_text(data = d_groupe_tot,
              aes(x = annee, y = y_leg, label = paste0(prettyNum(tot, big.mark = " "), "\nséjours" )),
              size = 3,
              position = position_stack(vjust = 1.05)) +
    coord_flip() +
    theme_light(9) +
    guides(fill = guide_legend(reverse = T)) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ylab('') + xlab('') + ylim(0, max(d_groupe_limites$tot)*1.12)
  plot <- plot + scale_fill_manual(name = "", values = colors_define)
  
  return(plot)
}


get_df_ambu<-function(df,criteres){
  #En colonnes :
  # - Nombre total de séjours par an
  # - Nombre de séjours d'HP
  # - Nombre de séjours d'HC
  # - Taux ambu
  #Critères
  
  
}

get_line_ghu<-function(df_graph){
  
  colors =c('#377EB8','#a4c7e4','darkgreen','#984EA3','#E41A1C','#FFFF33','#FF7F00')
  
  fig <- plot_ly(
    data = df_graph ,
    x = ~ annee,
    y =  ~ `APHP` ,
    mode = "lines+markers",
    type = "scatter",
    line = list(color = colors[1], width = 3),
    marker =list(color = colors[1], size = 10),
    name = "APHP")
  
  if(ghu_ref %in% names(df_graph)){
    
    fig <- fig %>% add_trace(y =  as.formula( "~ " %+% ghu_ref) ,
                             type = 'scatter',
                             mode = "lines+markers",
                             line = list(color = colors[2],
                                         width = 1),
                             marker =list(color = colors[2], size = 8), name = ghu_ref)
  }
  
  if("ESPIC" %in% names(df_graph)){
    fig <- fig %>% add_trace(y =  ~ESPIC,
                             type = 'scatter',
                             mode = "lines+markers",
                             line = list(color = colors[3],
                                         width = 2),
                             marker =list(color = colors[3], size = 8), name = "ESPIC")
  }
  
  if("CH" %in% names(df_graph)){
    
    fig <- fig %>% add_trace(y =  ~ CH,
                             type = 'scatter',
                             mode = "lines+markers",
                             line = list(color = colors[4],
                                         width = 1),
                             marker =list(color = colors[4], size = 8), name = "CH")
  }
  
  if("Cliniques" %in% names(df_graph)){
    fig <- fig %>% add_trace(y =  ~Cliniques,
                             type = 'scatter',
                             mode ="lines+markers",
                             line = list(color = colors[5], width = 1),
                             marker =list(color = colors[5], size = 8),
                             name = "Cliniques")
  }
  
  if("CLCC" %in% names(df_graph)){
    fig <- fig %>% add_trace(y =  ~CLCC,
                             type = 'scatter',
                             mode = "lines+markers",
                             line = list(color = colors[6], width = 1),
                             marker =list(color = colors[6], size = 8),
                             name = "CLCC")
  }

  
  fig<-  fig %>% layout(yaxis = list(title = 'Taux ambulatoire'),
                        title = "Evolution du taux d'ambulatoire",
                        xaxis=list(title='Année'))
  
  
  return(fig)
  
}

