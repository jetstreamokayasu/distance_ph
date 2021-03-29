#計算時間のまとめ

#-----------------------------
#2次元トーラス------------------

#CTIC2019手法
trs350_ctic_time<-map_dbl(c(list(trs350_colle1_ctic_aggr), trs350_list2to5_aggr), ~sum(.[["times"]]))
trs340_ctic_time<-map_dbl(trs340_list1to5_aggr, ~sum(.[["times"]]))
trs330_ctic_time<-map_dbl(trs330_list1to5_aggr, ~sum(.[["times"]]))
trs320_ctic_time<-map_dbl(trs320_list1to5_aggr, ~sum(.[["times"]]))
trs310_ctic_time<-map_dbl(trs310_list1to5_aggr, ~sum(.[["times"]]))
trs300_ctic_time<-map_dbl(c(list(trs300_colle1_ctic_aggr), trs300_list2to5_aggr), ~sum(.[["times"]]))

t2_ctic_time<-tibble("300"=trs300_ctic_time,
                     "310"=trs310_ctic_time,
                     "320"=trs320_ctic_time,
                     "330"=trs330_ctic_time,
                     "340"=trs340_ctic_time,
                     "350"=trs350_ctic_time) %>% 
  pivot_longer(cols = as.character(seq(300, 350, by=10)), names_to = "n_points", values_to = "time") %>% 
  dplyr::arrange(n_points) 
  
t2_ctic_time$n_points %<>% as.numeric()

t2_ctic_time_smz<-t2_ctic_time %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("time"))

#ggplotによるCTIC2019手法計算時間のプロット
t2_ctic_time_plt_ave<-ggplot(data = t2_ctic_time_smz, mapping = aes(x = n_points, y = mean)) + geom_line() 
t2_ctic_time_plt_sd<-t2_ctic_time_plt_ave + geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.1)
{
  t2_ctic_time_plt_all<-t2_ctic_time_plt_sd + geom_point(data = t2_ctic_time, aes(x = n_points, y = time, color = "Futagami"), size = 2) +
    scale_color_manual(breaks = c("Futagami", "Yamada", "Proposed"), values = c("black","royalblue1", "red"), guide = "legend", name = "method", 
                                    labels = c("Futagami", "Yamada", "Proposed"))
}

{
  t2_ctic_time_plt_all<-t2_ctic_time_plt_all + 
  labs(x = "Data density", y = "Computation time [sec]") + 
    theme(axis.text = element_text(size=25), axis.title = element_text(size=30), 
          legend.text = element_text(size=25), legend.title = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size = 5))) +
    scale_x_continuous(breaks = seq(300, 350, by=10), labels = c(expression(30/pi^2), expression(31/pi^2), expression(32/pi^2), expression(33/pi^2), expression(34/pi^2), expression(35/pi^2)) )
  
  
}

#補間手法
trs350_inted_time<-map_dbl(trs350_list1to5_inted_aggr, ~sum(.[["times"]]))
trs340_inted_time<-map_dbl(trs340_list1to5_inted_aggr, ~sum(.[["times"]]))
trs330_inted_time<-map_dbl(trs330_list1to5_inted_aggr, ~sum(.[["times"]]))
trs320_inted_time<-map_dbl(trs320_list1to5_inted_aggr, ~sum(.[["times"]]))
trs310_inted_time<-map_dbl(trs310_list1to5_inted_aggr, ~sum(.[["times"]]))
trs300_inted_time<-map_dbl(trs300_list1to5_inted_aggr, ~sum(.[["times"]]))

t2_inted_time<-tibble("300"=trs300_inted_time,
                     "310"=trs310_inted_time,
                     "320"=trs320_inted_time,
                     "330"=trs330_inted_time,
                     "340"=trs340_inted_time,
                     "350"=trs350_inted_time) %>% 
  pivot_longer(cols = as.character(seq(300, 350, by=10)), names_to = "n_points", values_to = "time") %>% 
  dplyr::arrange(n_points) 

t2_inted_time$n_points %<>% as.numeric()

t2_inted_time_smz<-t2_inted_time %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("time"))

#ggplotによる補間手法計算時間のプロット
t2_inted_time_plt_ave<-t2_ctic_time_plt_all + geom_line(data = t2_inted_time_smz, aes(x = n_points, y = mean), color = "royalblue1")
t2_inted_time_plt_sd<-t2_inted_time_plt_ave + geom_ribbon(data = t2_inted_time_smz, aes(ymin = mean - sd, ymax = mean + sd), fill = "royalblue1", alpha = 0.1)
t2_inted_time_plt_all<-t2_inted_time_plt_sd + geom_point(data = t2_inted_time, aes(x = n_points, y = time, color = "Yamada"), size = 2)

#補間点数をまとめる
torus300_colle_setB_inted_points<-map(torus300_colle_setB_inted, ~map_int(., nrow))
torus300_colle_setB_inted_points_mean<-map_dbl(trs300_col_setB_inted_points, mean)

for (j in seq(300, 350, by=10)) {
  assign(paste0("torus", j, "_colle_setB_inted_points"), map(get(paste0("torus", j, "_colle_setB_inted")), ~map_int(., nrow)))
  assign(paste0("torus", j, "_colle_setB_inted_points_mean"), map_dbl(get(paste0("torus", j, "_colle_setB_inted_points")), mean))
}

trs300to350_col_setB_inted_points_mean<-map(seq(300, 350, by=10), ~get(paste0("torus", .x, "_colle_setB_inted_points_mean"))) %>% unlist()

t2_inted_time %<>% add_column(total_points=trs300to350_col_setB_inted_points_mean, .after = 1) 

trs300to350_inted_points_mean<-map_dbl(seq(300, 350, by=10), ~{
  
  get(paste0("torus", .x, "_colle_setB_inted_points")) %>% unlist() %>% mean()
  
})

t2_inted_time_smz %<>% add_column(total_mean=trs300to350_inted_points_mean, .after = 1) 

#-----------------------
#CTIC手法と補間手法の計算時間プロット------
ggsave("./pics/compute_time_ctic_interp.pdf", plot = t2_inted_time_plt_all, height = 8.5, width = 14, units = "in")

t2_ctic_time_smz %<>% dplyr::rename(time_mean=mean, time_sd=sd)
t2_inted_time_smz %<>% dplyr::rename(time_mean=mean, time_sd=sd)

t2_ctic_intrp_cmpt_plt<-plot_success_rates(data = lst(t2_ctic_time, t2_inted_time), sumry = lst(t2_ctic_time_smz, t2_inted_time_smz), aes_y = "time", plot_ave = T, plot_sd = T, 
                                  point_size = 3, xlab = "Data density", ylab = "Computation time [sec]", ylim = c(0, NA), 
                                  legend_labels = c("Futagami", "Yamada"), axis_text_size = 25, 
                                  axis_title_size = 30, legend_text_size = 25, legend_title_size = 30)

ggsave("./pics/compute_time_t2_ctic_intr.pdf", plot = t2_ctic_intrp_cmpt_plt, height = 8.5, width = 14, units = "in")

#--------------------------
#exp距離変化手法----------
trs350_wvr_time<-c(trs350_colle1_wvr_time[3], map_dbl(trs350_colle2to3_wvr_aggr, ~{.[["time"]][3]}), map_dbl(trs350_colle4to5_wvr_aggr, ~{.[["time"]][3]}))
trs340_wvr_time<-c(trs340_colle1_wvr_time[3], map_dbl(trs340_colle2to3_wvr_aggr, ~{.[["time"]][3]}), map_dbl(trs340_colle4to5_wvr_aggr, ~{.[["time"]][3]}))
trs330_wvr_time<-c(trs330_colle1_wvr_time[3], map_dbl(trs330_colle2to3_wvr_aggr, ~{.[["time"]][3]}), map_dbl(trs330_colle4to5_wvr_aggr, ~{.[["time"]][3]}))
trs320_wvr_time<-c(trs320_colle1_wvr_time[3], map_dbl(trs320_colle2to3_wvr_aggr, ~{.[["time"]][3]}), map_dbl(trs320_colle4to5_wvr_aggr, ~{.[["time"]][3]}))
trs310_wvr_time<-c(trs310_colle1_wvr_time[3], map_dbl(trs310_colle2to3_wvr_aggr, ~{.[["time"]][3]}), map_dbl(trs310_colle4to5_wvr_aggr, ~{.[["time"]][3]}))
trs300_wvr_time<-c(trs300_colle1_wvr_time[3], map_dbl(trs300_colle2to3_wvr_aggr, ~{.[["time"]][3]}), map_dbl(trs300_colle4to5_wvr_aggr, ~{.[["time"]][3]}))

t2_wvr_time<-tibble("300"=trs300_wvr_time,
                      "310"=trs310_wvr_time,
                      "320"=trs320_wvr_time,
                      "330"=trs330_wvr_time,
                      "340"=trs340_wvr_time,
                      "350"=trs350_wvr_time) %>% 
  pivot_longer(cols = as.character(seq(300, 350, by=10)), names_to = "n_points", values_to = "time") %>% 
  dplyr::arrange(n_points) 

t2_wvr_time$n_points %<>% as.numeric()

t2_wvr_time_smz<-t2_wvr_time %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("time"))

#ggplotによるexp距離変化手法計算時間のプロット
t2_wvr_time_plt_ave<-t2_inted_time_plt_all + geom_line(data = t2_wvr_time_smz, aes(x = n_points, y = mean), color = "red")
t2_wvr_time_plt_sd<-t2_wvr_time_plt_ave + geom_ribbon(data = t2_wvr_time_smz, aes(ymin = mean - sd, ymax = mean + sd), fill = "red", alpha = 0.1)
t2_wvr_time_plt_all<-t2_wvr_time_plt_sd + geom_point(data = t2_wvr_time, aes(x = n_points, y = time, colour = "Proposed"), size = 2)

#ggplotによる300~350点2次元トーラスの計算時間グラフ
#軸ラベルや凡例の調整
{t2_time_plt_whole<-t2_wvr_time_plt_all +labs(x = "Data density", y = "Computation time [sec]") + 
    theme(axis.text = element_text(size=25), axis.title = element_text(size=30), 
          legend.text = element_text(size=25), legend.title = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size = 5))) +
    scale_x_continuous(breaks = seq(300, 350, by=10), labels = c(expression(30/pi^2), expression(31/pi^2), expression(32/pi^2), expression(33/pi^2), expression(34/pi^2), expression(35/pi^2)) )
}

#ggplotによる300~350点2次元トーラスの計算時間グラフ
#軸ラベルや凡例の調整
#スライド用白抜きグラフ
{t2_time_plt_whole2<-t2_wvr_time_plt_all +labs(x = "The number of points", y = "Computational time [sec]") + theme_bw() +
    theme(axis.text = element_text(size=25), axis.title = element_text(size=30), 
          legend.text = element_text(size=25), legend.title = element_text(size=30), panel.border = element_rect(size=2)) + guides(color = guide_legend(override.aes = list(size = 5))) #+
    #scale_x_continuous(breaks = seq(300, 350, by=10), labels = c(expression(30/pi^2), expression(31/pi^2), expression(32/pi^2), expression(33/pi^2), expression(34/pi^2), expression(35/pi^2)) )
}

ggsave("./pics/compute_time.pdf", plot = t2_time_plt_whole2, height = 8.5, width = 14, units = "in")

t2_time_plt_whl3<-plot_success_rates(data = lst(t2_ctic_time, t2_inted_time, t2_wvr_time), aes_y = "time", ylim = c(0, NA),
                   fill_alpha = 0.1, scale_label = seq(300, 350, by = 10), xlab = "The number of points", ylab = "Computation time", legend_labels = c("Futagami", "Yamada", "Proposed"), 
                   point_size = 3, axis_text_size = 25, axis_title_size = 30, legend_text_size = 25, legend_title_size = 30)
ggsave(filename = "./pics/t2_cmpt_time_ctic_intrp_wvr.pdf", plot = t2_time_plt_whl3, width = 14, height = 10, units = "in")

t2_ctic_time_summary<-t2_ctic_time %>% group_by(n_points) %>% dplyr::summarise(across(.fns = lst(mean, sd)))

t2_ctic_time_summary_lst<-list(t2_ctic_time_summary, t2_ctic_time_summary)

#----------------------------
#横軸を補間点も含めた全データ点数とした場合の計算時間-----
#ggplotによるCTIC2019手法計算時間のプロット
t2_ctic_time_plt_ave2<-ggplot(data = t2_ctic_time_smz, mapping = aes(x = n_points, y = mean)) + geom_line() 
t2_ctic_time_plt_sd2<-t2_ctic_time_plt_ave2 + geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.1)
{
  t2_ctic_time_plt_whl<-t2_ctic_time_plt_sd2 + geom_point(data = t2_ctic_time, aes(x = n_points, y = time, color = "futagami"), size = 2) +
    scale_color_manual(breaks = c("futagami", "yamada", "proposed"), values = c("black","royalblue1", "red"), guide = "legend", name = "method", 
                       labels = c("Futagami", "Yamada", "Proposed"))
}

{
  t2_ctic_time_plt_whl2<-t2_ctic_time_plt_whl + 
    labs(x = "The number of points", y = "Computational time [sec]") + 
    theme(axis.text = element_text(size=25), axis.title = element_text(size=30), 
          legend.text = element_text(size=25), legend.title = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size = 5))) #+
    #scale_x_continuous(breaks = seq(300, 350, by=10), labels = c(expression(30/pi^2), expression(31/pi^2), expression(32/pi^2), expression(33/pi^2), expression(34/pi^2), expression(35/pi^2)) )
  
  
}

#ggplotによる補間手法計算時間のプロット
t2_inted_time_plt_ave2<-t2_ctic_time_plt_whl2 + geom_line(data = t2_inted_time_smz, aes(x = total_mean, y = mean), color = "royalblue1")
t2_inted_time_plt_sd2<-t2_inted_time_plt_ave2 + geom_ribbon(data = t2_inted_time_smz, aes(x = total_mean, ymin = mean - sd, ymax = mean + sd), fill = "royalblue1", alpha = 0.1)
t2_inted_time_plt_whl<-t2_inted_time_plt_sd2 + geom_point(data = t2_inted_time, aes(x = total_points, y = time, color = "yamada"), size = 2)

#ggplotによるexp距離変化手法計算時間のプロット
t2_wvr_time_plt_ave2<-t2_inted_time_plt_whl + geom_line(data = t2_wvr_time_smz, aes(x = n_points, y = mean), color = "red")
t2_wvr_time_plt_sd2<-t2_wvr_time_plt_ave2 + geom_ribbon(data = t2_wvr_time_smz, aes(ymin = mean - sd, ymax = mean + sd), fill = "red", alpha = 0.1)
t2_wvr_time_plt_whl<-t2_wvr_time_plt_sd2 + geom_point(data = t2_wvr_time, aes(x = n_points, y = time, colour = "proposed"), size = 2)

ggsave("./pics/torus_compute_time_ctic_intrp_wvr.pdf", plot = t2_wvr_time_plt_whl, height = 8.2, width = 17, units = "in")

#一括プロット関数
t2_wvr_time_smz %<>% dplyr::rename(time_mean=mean, time_sd=sd)

t2_ctic_intrp_cmpt_wvr_plt<-plot_success_rates(data = lst(t2_ctic_time, t2_inted_time, t2_wvr_time), sumry = lst(t2_ctic_time_smz, t2_inted_time_smz, t2_wvr_time_smz), aes_y = "time", plot_ave = T, plot_sd = T, 
                                           point_size = 3, xlab = "Data density", ylab = "Computation time [sec]", ylim = c(0, NA), 
                                           legend_labels = c("Futagami", "Yamada", "Proposed"), axis_text_size = 25, 
                                           axis_title_size = 30, legend_text_size = 25, legend_title_size = 30)

#------------------------------
#補間後の平均点を示す棒グラフと、成功率の折れ線グラフを一緒に描く----------
y1_lim<-c(0, 5500)
y2_lim<-c(0, 610)

y_scaler<-(y1_lim[2]-y1_lim[1])/(y2_lim[2]-y2_lim[1])

t2_ctic_intrp_cmpt_wvr_plt2<-
t2_ctic_intrp_cmpt_wvr_plt + geom_bar(data = t2_inted_time_smz, mapping = aes(x = n_points, y = total_mean*y_scaler/2), stat = "identity", col = "chartreuse4", fill = "chartreuse4", alpha = 0.1) +
  scale_y_continuous(limits = y1_lim, sec.axis = sec_axis(trans = ~ (./y_scaler)*2, name = "The mean of the total of \n original and added points"))


#補間後の平均点の棒グラフ
inted_p_bar<-ggplot()+geom_bar(data = t2_inted_time_smz, mapping = aes(x = n_points, y = total_mean*y_scaler/2), 
                               stat = "identity", fill = "orange", alpha = 0.1, width = 5, col = "orange") + 
  geom_text(data = t2_inted_time_smz, mapping = aes(x = n_points, label = total_mean, y = total_mean*y_scaler/2 + 30), position = position_dodge(0.9), vjust = 0)
#inted_p_bar
#ctic手法の計算時間
t2_ctic_time_plt_ave2<-inted_p_bar + geom_line(data = t2_ctic_time_smz, mapping = aes(x = n_points, y = time_mean))
t2_ctic_time_plt_sd2<-t2_ctic_time_plt_ave2 + geom_ribbon(data = t2_ctic_time_smz, aes(x = n_points, ymin = time_mean - time_sd, ymax = time_mean + time_sd), alpha = 0.3)
#t2_ctic_time_plt_sd2
{
  t2_ctic_time_plt_all2<-t2_ctic_time_plt_sd2 + geom_point(data = t2_ctic_time, aes(x = n_points, y = time, color = "Futagami"), size = 2) +
    scale_color_manual(breaks = c("Futagami", "Yamada", "Proposed"), values = c("black","royalblue1", "red"), guide = "legend", name = "method", 
                       labels = c("Futagami", "Yamada", "Proposed"))
}

{
  t2_ctic_time_plt_all2<-t2_ctic_time_plt_all2 + 
    labs(x = "The number of points", y = "Computation time [sec]") + 
    theme(axis.text = element_text(size=25), axis.title = element_text(size=30), 
          legend.text = element_text(size=25), legend.title = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size = 5))) +
    scale_x_continuous(breaks = seq(300, 350, by=10), labels = seq(300, 350, by=10) )
  
  
}

#補間手法の計算時間
t2_inted_time_plt_ave2<-t2_ctic_time_plt_all2 + geom_line(data = t2_inted_time_smz, aes(x = n_points, y = time_mean), color = "royalblue1")
t2_inted_time_plt_sd2<-t2_inted_time_plt_ave2 + geom_ribbon(data = t2_inted_time_smz, aes(x = n_points, ymin = time_mean - time_sd, ymax = time_mean + time_sd), fill = "royalblue1", alpha = 0.3)
t2_inted_time_plt_all2<-t2_inted_time_plt_sd2 + geom_point(data = t2_inted_time, aes(x = n_points, y = time, color = "Yamada"), size = 2)

#結合時刻早期化手法の計算時間
t2_wvr_time_plt_ave2<-t2_inted_time_plt_all2 + geom_line(data = t2_wvr_time_smz, aes(x = n_points, y = time_mean), color = "red")
t2_wvr_time_plt_sd2<-t2_wvr_time_plt_ave2 + geom_ribbon(data = t2_wvr_time_smz, aes(x = n_points, ymin = time_mean - time_sd, ymax = time_mean + time_sd), fill = "red", alpha = 0.3)
t2_wvr_time_plt_all2<-t2_wvr_time_plt_sd2 + geom_point(data = t2_wvr_time, aes(x = n_points, y = time, colour = "Proposed"), size = 2)

#右縦軸を作成
t2_ctic_intr_wvr_cmpt_pbar<-t2_wvr_time_plt_all2 + scale_y_continuous(limits = c(0, 5500), sec.axis = sec_axis(trans = ~ (./y_scaler)*2, name = "The mean of the total of \n original and added points"))

#t2_ctic_intr_wvr_cmpt_pbar
ggsave("./pics/t2_ctic_intr_wvr_cmpt_pbar2.pdf", plot = t2_ctic_intr_wvr_cmpt_pbar, height = 8.5, width = 14, units = "in")

#ggplot()+geom_ribbon(data = t2_inted_time_smz, aes(x = n_points, ymin = time_mean - time_sd, ymax = time_mean + time_sd), fill = "royalblue1", alpha = 0.1)

#------------------- 
#楕円体--------------

#-------------------
#ctic手法--------
ellip150_ctic_time<-map_dbl(c(lst(ellip150_aggr1), ellip150_list2to5_aggr), ~sum(.[["times"]]))
ellip160_ctic_time<-map_dbl(ellip160_list1to5_aggr, ~sum(.[["times"]]))
ellip170_ctic_time<-map_dbl(ellip170_list1to5_aggr, ~sum(.[["times"]]))
ellip180_ctic_time<-map_dbl(ellip180_list1to5_aggr, ~sum(.[["times"]]))
ellip190_ctic_time<-map_dbl(ellip190_list1to5_aggr, ~sum(.[["times"]]))
ellip200_ctic_time<-map_dbl(ellip200_list1to5_aggr, ~sum(.[["times"]]))

ellip_ctic_time<-tibble("150"=ellip150_ctic_time,
                     "160"=ellip160_ctic_time,
                     "170"=ellip170_ctic_time,
                     "180"=ellip180_ctic_time,
                     "190"=ellip190_ctic_time,
                     "200"=ellip200_ctic_time) %>% 
  pivot_longer(cols = as.character(seq(150, 200, by=10)), names_to = "n_points", values_to = "time") %>% 
  dplyr::arrange(n_points) 

ellip_ctic_time$n_points %<>% as.numeric()

ellip_ctic_time_smz<-ellip_ctic_time %>% group_by(n_points) %>% 
  dplyr::summarise(across(.fns = lst(mean, sd)))

#-------------------
#補間手法--------
ellip150_intrp_time<-map_dbl(c(lst(ellip150_inted_aggr1), ellip150_list2to5_inted_aggr), ~sum(.[["times"]]))
ellip160_intrp_time<-map_dbl(ellip160_list1to5_inted_aggr, ~sum(.[["times"]]))
ellip170_intrp_time<-map_dbl(ellip170_list1to5_inted_aggr, ~sum(.[["times"]]))
ellip180_intrp_time<-map_dbl(ellip180_list1to5_inted_aggr, ~sum(.[["times"]]))
ellip190_intrp_time<-map_dbl(ellip190_list1to5_inted_aggr, ~sum(.[["times"]]))
ellip200_intrp_time<-map_dbl(ellip200_list1to5_inted_aggr, ~sum(.[["times"]]))

ellip_intrp_time<-tibble("150"=ellip150_intrp_time,
                        "160"=ellip160_intrp_time,
                        "170"=ellip170_intrp_time,
                        "180"=ellip180_intrp_time,
                        "190"=ellip190_intrp_time,
                        "200"=ellip200_intrp_time) %>% 
  pivot_longer(cols = as.character(seq(150, 200, by=10)), names_to = "n_points", values_to = "time") %>% 
  dplyr::arrange(n_points) 

ellip_intrp_time$n_points %<>% as.numeric()

ellip_intrp_time_smz<-ellip_intrp_time %>% group_by(n_points) %>% 
  dplyr::summarise(across(.fns = lst(mean, sd)))

#補間点数をまとめる
ellip150_lst1to5_inted_npoints<-map(c(lst(ellip150_list1_inted), ellip150_list2to5_inted), ~{map_int(.x, ~{nrow(.)})})
ellip150_lst1to5_inted_npoints_mean<-map_dbl(ellip150_lst1to5_inted_npoints, mean)

ellip160_lst1to5_inted_npoints<-map(ellip160_list1to5_inted, ~{map_int(.x, nrow)})
ellip160_lst1to5_inted_npoints_mean<-map_dbl(ellip160_lst1to5_inted_npoints, mean)

ellip170_lst1to5_inted_npoints<-map(ellip170_list1to5_inted, ~{map_int(.x, nrow)})
ellip170_lst1to5_inted_npoints_mean<-map_dbl(ellip170_lst1to5_inted_npoints, mean)

ellip180_lst1to5_inted_npoints<-map(ellip180_list1to5_inted, ~{map_int(.x, nrow)})
ellip180_lst1to5_inted_npoints_mean<-map_dbl(ellip180_lst1to5_inted_npoints, mean)

ellip190_lst1to5_inted_npoints<-map(ellip190_list1to5_inted, ~{map_int(.x, nrow)})
ellip190_lst1to5_inted_npoints_mean<-map_dbl(ellip190_lst1to5_inted_npoints, mean)

ellip200_lst1to5_inted_npoints<-map(ellip200_list1to5_inted, ~{map_int(.x, nrow)})
ellip200_lst1to5_inted_npoints_mean<-map_dbl(ellip200_lst1to5_inted_npoints, mean)

ellip_intrp_time %<>% add_column(total_points = c(ellip150_lst1to5_inted_npoints_mean, ellip160_lst1to5_inted_npoints_mean, ellip170_lst1to5_inted_npoints_mean, ellip180_lst1to5_inted_npoints_mean, ellip190_lst1to5_inted_npoints_mean, ellip200_lst1to5_inted_npoints_mean), 
           .after = 1)

ellip150to200_inted_points_mean<-map_dbl(seq(150, 200, by=10), ~{
  
  get(paste0("ellip", .x, "_lst1to5_inted_npoints")) %>% unlist() %>% mean()
  
})

ellip_intrp_time_smz %<>% add_column(total_mean=ellip150to200_inted_points_mean, .after = 1)

#--------------------
#結合時刻変化手法--------------
ellip150_wvr_time<-map_dbl(c(lst(ellip150_wvr_aggr1), ellip150_list2to5_wvr_aggr), ~sum(.[["times"]]))
ellip160_wvr_time<-map_dbl(ellip160_list1to5_wvr_aggr, ~sum(.[["times"]]))
ellip170_wvr_time<-map_dbl(ellip170_list1to5_wvr_aggr, ~sum(.[["times"]]))
ellip180_wvr_time<-map_dbl(ellip180_list1to5_wvr_aggr, ~sum(.[["times"]]))
ellip190_wvr_time<-map_dbl(ellip190_list1to5_wvr_aggr, ~sum(.[["times"]]))
ellip200_wvr_time<-map_dbl(ellip200_list1to5_wvr_aggr, ~sum(.[["times"]]))

ellip_wvr_time<-tibble("150"=ellip150_wvr_time,
                         "160"=ellip160_wvr_time,
                         "170"=ellip170_wvr_time,
                         "180"=ellip180_wvr_time,
                         "190"=ellip190_wvr_time,
                         "200"=ellip200_wvr_time) %>% 
  pivot_longer(cols = as.character(seq(150, 200, by=10)), names_to = "n_points", values_to = "time") %>% 
  dplyr::arrange(n_points) 

ellip_wvr_time$n_points %<>% as.numeric()

ellip_wvr_time_smz<-ellip_wvr_time %>% group_by(n_points) %>% 
  dplyr::summarise(across(.fns = lst(mean, sd)))


#-------------------------------
#楕円体計算時間全体プロット---------

#補間後の平均点を示す棒グラフと、成功率の折れ線グラフを一緒に描く----------
y3_lim<-c(0, 2000)
y2_lim<-c(0, 610)

y_scaler2<-(y3_lim[2]-y3_lim[1])/(y2_lim[2]-y2_lim[1])

#補間後の平均点の棒グラフ
ellip_inted_bar<-ggplot()+geom_bar(data = ellip_intrp_time_smz, mapping = aes(x = n_points, y = total_mean*y_scaler2/2), 
                               stat = "identity", fill = "orange", alpha = 0.1, width = 5, col = "orange") + 
  geom_text(data = ellip_intrp_time_smz, mapping = aes(x = n_points, label = total_mean, y = total_mean*y_scaler2/2 + 10), position = position_dodge(0.9), vjust = 0)

ellip_inted_bar
# ellip_cmpt_time_plt<-plot_success_rates(data = lst(ellip_ctic_time, ellip_intrp_time, ellip_wvr_time), sumry = lst(ellip_ctic_time_smz, ellip_intrp_time_smz, ellip_wvr_time_smz), 
#                    aes_y = "time", ylim = c(0, NA), scale_label = seq(150, 200, by=10), xlab = "The number of points", ylab = "Computation time", legend_labels = c("Futagami", "Yamada", "Proposed"))
# 
# ggsave("./pics/ellip_compute_time_ctic_intrp_wvr.pdf", plot = ellip_cmpt_time_plt, height = 10, width = 14, units = "in")


#ggplotによるCTIC2019手法計算時間のプロット
ellip_ctic_time_plt_ave<-ellip_inted_bar + geom_line(data = ellip_ctic_time_smz, mapping = aes(x = n_points, y = time_mean))
ellip_ctic_time_plt_sd<-ellip_ctic_time_plt_ave + geom_ribbon(data = ellip_ctic_time_smz, mapping = aes(x = n_points, ymin = time_mean - time_sd, ymax = time_mean + time_sd), alpha = 0.3)
ellip_ctic_time_plt_sd
{
  ellip_ctic_time_plt_whole<-ellip_ctic_time_plt_sd + geom_point(data = ellip_ctic_time, aes(x = n_points, y = time, color = "futagami"), size = 2) +
    scale_color_manual(breaks = c("futagami", "yamada", "proposed"), values = c("black","royalblue1", "red"), guide = "legend", name = "method", 
                       labels = c("Futagami", "Yamada", "Proposed"))
}

{
  ellip_ctic_time_plt_whole2<-ellip_ctic_time_plt_whole + 
    labs(x = "The number of points", y = "Computation time [sec]") + 
    theme(axis.text = element_text(size=25), axis.title = element_text(size=30), 
          legend.text = element_text(size=25), legend.title = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size = 5))) + 
    scale_x_continuous(breaks = seq(150, 200, by=10), labels = seq(150, 200, by=10))
  
}
ellip_ctic_time_plt_whole2
#ggplotによる補間手法計算時間のプロット
#横軸を補間後の全データ点数とした場合の計算時間
# ellip_intrp_time_ttlp_mean<-ellip_ctic_time_plt_whole2+geom_line(data = ellip_intrp_time_smz, mapping = aes(x = total_mean, y = time_mean), color = "royalblue1")
# ellip_intrp_time_ttlp_sd<-ellip_intrp_time_ttlp_mean + geom_ribbon(data = ellip_intrp_time_smz, mapping = aes(x = total_mean, ymin = time_mean-time_sd, ymax = time_mean+time_sd), fill = "royalblue1", alpha = 0.1)
# ellip_intrp_time_ttlp1<-ellip_intrp_time_ttlp_sd+geom_point(data = ellip_intrp_time, mapping = aes(x = total_points, y = time, colour = "yamada"), size = 2)

ellip_intrp_time_ttlp_mean<-ellip_ctic_time_plt_whole2+geom_line(data = ellip_intrp_time_smz, mapping = aes(x = n_points, y = time_mean), color = "royalblue1")
ellip_intrp_time_ttlp_sd<-ellip_intrp_time_ttlp_mean + geom_ribbon(data = ellip_intrp_time_smz, mapping = aes(x = n_points, ymin = time_mean-time_sd, ymax = time_mean+time_sd), fill = "royalblue1", alpha = 0.3)
ellip_intrp_time_ttlp1<-ellip_intrp_time_ttlp_sd+geom_point(data = ellip_intrp_time, mapping = aes(x = n_points, y = time, colour = "yamada"), size = 2)
ellip_intrp_time_ttlp1

#ggplotによるexp距離変化手法計算時間のプロット
ellip_wvr_time_plt_ave<-ellip_intrp_time_ttlp1 + geom_line(data = ellip_wvr_time_smz, aes(x = n_points, y = time_mean), color = "red")
ellip_wvr_time_plt_sd<-ellip_wvr_time_plt_ave + geom_ribbon(data = ellip_wvr_time_smz, aes(x = n_points, ymin = time_mean - time_sd, ymax = time_mean + time_sd), fill = "red", alpha = 0.1)
ellip_wvr_time_plt_whl<-ellip_wvr_time_plt_sd + geom_point(data = ellip_wvr_time, aes(x = n_points, y = time, colour = "proposed"), size = 2)

ellip_wvr_time_plt_whl

#右縦軸を作成
ellip_ctic_intr_wvr_cmpt_pbar<-ellip_wvr_time_plt_whl + scale_y_continuous(limits = y3_lim, sec.axis = sec_axis(trans = ~ (./y_scaler2)*2, name = "The mean of the total of \n original and added points"))

ggsave("./pics/ellip_compute_time2.pdf", plot = ellip_ctic_intr_wvr_cmpt_pbar, height = 8.5, width = 14, units = "in")

#--------------------------------
#3次元トーラスの計算時間----

#500点
t3rs_lst4_times<-c(t3orus4_list3_1to30aggrs4$times, t3orus4_list3_31to100_wvr_aggrs3$times, t3orus4_list3_1to30_wvrH2_aggrs6$times, t3orus4_list3_31to100_wvrH2_aggrs6$times)

t3rs450_times<-c(t3orus450_list1_1to30_wvr_aggrs1$times, t3orus450_list1_31to100_wvr_aggrs1$times, t3orus450_list1_1to30_wvrH2_aggrs2$times, t3orus450_list1_31to100_wvrH2_aggrs2$times)
t3rs460_times<-c(t3orus460_list1_1to30_wvr_aggrs1$times, t3orus460_list1_31to100_wvr_aggrs1$times, t3orus460_list1_1to30_wvrH2_aggrs2$times, t3orus460_list1_31to100_wvrH2_aggrs2$times)
t3rs470_times<-c(t3orus470_list1_1to30_wvr_aggrs1$times, t3orus470_list1_31to100_wvr_aggrs1$times, t3orus470_list1_1to30_wvrH2_aggrs2$times, t3orus470_list1_31to100_wvrH2_aggrs2$times)
t3rs480_times<-c(t3orus480_list1_1to30_wvr_aggrs1$times, t3orus480_list1_31to100_wvr_aggrs1$times, t3orus480_list1_1to30_wvrH2_aggrs2$times, t3orus480_list1_31to100_wvrH2_aggrs2$times)
t3rs490_times<-c(t3orus490_list1_1to30_wvr_aggrs1$times, t3orus490_list1_31to100_wvr_aggrs1$times, t3orus490_list1_1to30_wvrH2_aggrs2$times, t3orus490_list1_31to100_wvrH2_aggrs2$times)

t3_maxtime<-max(t3rs_lst4_times, t3rs450_times, t3rs460_times, t3rs470_times, t3rs480_times, t3rs490_times)
