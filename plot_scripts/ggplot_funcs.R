##ggplotによる実験結果描画をまとめた関数-----
#data=点をプロットするデータ、sumry=dataの平均・標準偏差を入れたDF、aes_x=横軸にする値、aes_y=縦軸にする値
#ylim=縦軸の描画範囲、col_vals=色として用いる値、fill_alpha=標準偏差の領域を塗る色の透明度、
#point_size=点の大きさ、line_size=平均値を結ぶ線の太さ、xlab=横軸ラベルタイトル、ylab=縦軸ラベルタイトル、
#axis_text_size=軸目盛の文字サイズ、axis_title_size=軸ラベルの文字サイズ、
#legend_name=凡例のタイトル、legend_labs=凡例のラベル、legend_text_size=凡例のラベルの文字サイズ、
#legend_title_size=凡例タイトルの文字サイズ、scale_label=横軸目盛りのラベル、
#white=白抜きするか否か、plot_ave=平均値を結ぶ直線を描くか否か、plot_sd=標準偏差の領域を描くか否か、
#show_plot=関数実行時にグラフ全体を描画するか否か

plot_success_rates<-
  function(data, sumry, aes_x = "n_points", aes_y, ylim = c(0, 1), 
          col_vals = c("black", "royalblue1", "red"), fill_alpha = 0.1, point_size = 3, line_size = 1,
          xlab = "Data density", ylab = "Success rate", axis_text_size = 20, axis_title_size = 25, 
          legend_name = "method", legend_labels = c("conventional", "proposed 1", "proposed 2"), 
          legend_text_size = 20, legend_title_size = 25,
          scale_label = sapply(seq(30, 35, by=1), function(i){parse(text=paste0(i, "/pi^2"))}),
          white = F, plot_ave = T, plot_sd = T, show_plot = T){
    #-----------------------------------------------------------------
    
    if(!is_list(data[[1]])){data<-lst(data)}
    
    #平均や分散が入ったsummryがない場合、計算する
    if(missing(sumry)){
      
      sumry<-lapply(data, function(d){
        
        smz<-d %>% group_by(n_points) %>% 
          dplyr::summarise(across(.fns = lst(mean, sd)))
        
        return(smz)
        
      })
      
    }
    
    if(!is_list(sumry[[1]]) || length(sumry) == 1){sumry<-lst(sumry)}
    
    names(col_vals)<-as.character(seq_along(col_vals))
    
    if(missing(aes_y)){aes_y<-colnames(data[[1]])[2]}
    
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

    if(plot_ave){
      
      gplt_ave_lst<-lapply(seq_along(sumry), function(i){
        
        if(ncol(sumry[[i]])==3){
          
          colnames(sumry[[i]])[2] %<>% paste0(aes_y, "_", .)
          
        }        

        gplt_ave<-geom_line(data = sumry[[i]], aes(x = sumry[[i]][[aes_x]], y = sumry[[i]][[paste0(aes_y, "_mean")]]), color = col_vals[i], size = line_size)
        return(gplt_ave)

      })
      
      gplt_scale<-reduce(c(lst(gplt_scale), gplt_ave_lst), `+`)
      
    }

    
    if(plot_sd && !( some(sumry, ~{some(.x, ~{NaN %in% .})}) )){
      
      gplt_sd_lst<-lapply(seq_along(sumry), function(i){
        
        if(ncol(sumry[[i]])==3){
          
          colnames(sumry[[i]])[3] %<>% paste0(aes_y, "_",  .)
          
        }
  
        gplt_sd<-geom_ribbon(data = sumry[[i]], aes(x = sumry[[i]][[aes_x]], ymin = sumry[[i]][[paste0(aes_y, "_mean")]] - sumry[[i]][[paste0(aes_y, "_sd")]], ymax = sumry[[i]][[paste0(aes_y, "_mean")]] + sumry[[i]][[paste0(aes_y, "_sd")]]), fill = col_vals[i], alpha = fill_alpha)
        return(gplt_sd)
  
      })
      
      gplt_scale<-reduce(c(lst(gplt_scale), gplt_sd_lst), `+`)
    }
    
    gplt_point_lst<-lapply(seq_along(data), function(i){
      
      gplt_point<-geom_point(data = data[[i]], aes(x = data[[i]][[aes_x]], y = data[[i]][[aes_y]], color = as.character(i)), size = point_size)
      return(gplt_point)
      
    })
    
    
    
    gplt_whole<-reduce(c(lst(gplt_scale), gplt_point_lst), `+`)
    
    #gplt_whole<-reduce(c(lst(gplt_scale), gplt_ave_lst, gplt_sd_lst, gplt_point_lst), `+`)
    
    if(show_plot){gplt_whole}
    
    return(gplt_whole)
  
}
T  
