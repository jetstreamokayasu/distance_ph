##ggplotによる実験結果描画をまとめた関数-----
#white=白抜きするか否か
plot_success_rates<-
  function(data, sumry, ylim = c(0, 1), aes_x = "n_points", aes_y = "dim2rate",
           col_vals = c("black", "royalblue1", "red"), fill_alpha = 0.1, point_size = 3, line_size = 1,
           xlab = "Data density", ylab = "Success rate", axis_text_size = 20, axis_title_size = 25, 
           legend_name = "method", legend_labels = c("conventional", "proposed 1", "proposed 2"), 
           legend_text_size = 20, legend_title_size = 25,
           scale_label = sapply(seq(30, 35, by=1), function(i){bquote(.(i)/pi^2)}),
           plot_ave = T, plot_sd = T, white = F, show_plot = T){
    
  if(!is_list(data[[1]])){data<-lst(data)}
  if(!is_list(sumry[[1]])){sumry<-lst(sumry)}
    
  names(col_vals)<-as.character(seq_along(col_vals))
  
  {
  gplt_base<-ggplot() + ylim(ylim) + xlab(xlab) + ylab(ylab) +
      scale_color_manual(values = col_vals, guide = "legend", name = legend_name, labels = legend_labels)
  }
  
  #白抜き背景の場合
  if(white){gplt_theme<-gplt_base + theme_bw() + theme(axis.text = element_text(size=axis_text_size), axis.title = element_text(size=axis_title_size), 
                                        legend.text = element_text(size=legend_text_size), legend.title = element_text(size=legend_title_size), 
                                        panel.border = element_rect(size=2))}
  
  else{gplt_theme<-gplt_base + theme(axis.text = element_text(size=axis_text_size), axis.title = element_text(size=axis_title_size), 
                                                legend.text = element_text(size=legend_text_size), legend.title = element_text(size=legend_title_size))}
  
  gplt_scale<-gplt_theme + scale_x_continuous(breaks = unique(data[[1]][[aes_x]]), labels = scale_label)
  
  # if(plot_ave){
  #   
  #   gplt_theme<-lapply(seq_along(sumry), function(i){
  #     
  #     gplt_ave<-geom_line(data = sumry[[i]], aes(x = sumry[[i]][[aes_x]], y = sumry[[i]][[paste0(aes_y, "_mean")]]), color = col_vals[i], size = line_size)
  #     return(gplt_ave)
  #     
  #     }) %>% reduce(append(lst(gplt_scale), .), `+`)
  #   
  # }
  # if(plot_sd){
  #   
  #   gplt_theme<-lapply(seq_along(sumry), function(i){
  #     
  #     gplt_sd<-geom_ribbon(data = sumry[[i]], aes(ymin = sumry[[i]][[paste0(aes_y, "_mean")]] - sumry[[i]][[paste0(aes_y, "_sd")]], ymax = sumry[[i]][[paste0(aes_y, "_mean")]] + sumry[[i]][[paste0(aes_y, "_sd")]]), fill = col_vals[i], alpha = fill_alpha)
  #     return(gplt_sd)
  #     
  #   }) %>% reduce(append(lst(gplt_scale), .), `+`)
  #   
  #   }
  
  
  gplt_lst<-lapply(seq_along(data), function(i){

    gplt_points<-geom_point(data = data[[i]], aes(x = aes_x, y = aes_y, color = as.character(i)), size = point_size)

    return(gplt_points)

  })
  
  gplt_whole<-reduce(append(lst(gplt_scale), gplt_lst), `+`)
  
  if(show_plot){gplt_whole}
  
  return(gplt_whole)
  
}

plot_success_rates_test<-
  function(data, sumry, ylim = c(0, 1), aes_x = "n_points", aes_y = "dim2rate",
          col_vals = c("black", "royalblue1", "red"), fill_alpha = 0.1, point_size = 3, line_size = 1,
          xlab = "Data density", ylab = "Success rate", axis_text_size = 20, axis_title_size = 25, 
          legend_name = "method", legend_labels = c("conventional", "proposed 1", "proposed 2"), 
          legend_text_size = 20, legend_title_size = 25,
          scale_label = sapply(seq(30, 35, by=1), function(i){bquote(.(i)/pi^2)}),
          plot_ave = T, plot_sd = T, white = F, show_plot = T){
    #-----------------------------------------------------------------
    
    if(!is_list(data[[1]])){data<-lst(data)}
    if(!is_list(sumry[[1]])){sumry<-lst(sumry)}
    
    names(col_vals)<-as.character(seq_along(col_vals))
    
    {
      gplt_base<-ggplot() + ylim(ylim) + xlab(xlab) + ylab(ylab) +
        scale_color_manual(values = col_vals, guide = "legend", name = legend_name, labels = legend_labels)
    }
    
    #白抜き背景の場合
    if(white){gplt_theme<-gplt_base + theme_bw() + theme(axis.text = element_text(size=axis_text_size), axis.title = element_text(size=axis_title_size), 
                                                         legend.text = element_text(size=legend_text_size), legend.title = element_text(size=legend_title_size), 
                                                         panel.border = element_rect(size=2))}
    
    else{gplt_theme<-gplt_base + theme(axis.text = element_text(size=axis_text_size), axis.title = element_text(size=axis_title_size), 
                                       legend.text = element_text(size=legend_text_size), legend.title = element_text(size=legend_title_size))}
    
    gplt_scale<-gplt_theme + scale_x_continuous(breaks = unique(data[[1]][[aes_x]]), labels = scale_label)

    gplt_ave_lst<-lapply(seq_along(sumry), function(i){

      gplt_ave<-geom_line(data = sumry[[i]], aes(x = sumry[[i]][[aes_x]], y = sumry[[i]][[paste0(aes_y, "_mean")]]), color = col_vals[i], size = line_size)
      return(gplt_ave)

    }) 

    gplt_sd_lst<-lapply(seq_along(sumry), function(i){

      gplt_sd<-geom_ribbon(data = sumry[[i]], aes(x = sumry[[i]][[aes_x]], ymin = sumry[[i]][[paste0(aes_y, "_mean")]] - sumry[[i]][[paste0(aes_y, "_sd")]], ymax = sumry[[i]][[paste0(aes_y, "_mean")]] + sumry[[i]][[paste0(aes_y, "_sd")]]), fill = col_vals[i], alpha = fill_alpha)
      return(gplt_sd)

    }) 
    
    gplt_point_lst<-lapply(seq_along(data), function(i){
      
      gplt_point<-geom_point(data = data[[i]], aes(x = data[[i]][[aes_x]], y = data[[i]][[aes_y]], color = as.character(i)), size = point_size)
      return(gplt_point)
      
    })
    
    gplt_whole<-reduce(c(lst(gplt_scale), gplt_ave_lst, gplt_sd_lst, gplt_point_lst), `+`)
    
    gplt_whole
    return(gplt_whole)
  
  }
  
