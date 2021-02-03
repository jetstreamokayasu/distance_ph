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
  t2_ctic_time_plt_all<-t2_ctic_time_plt_sd + geom_point(data = t2_ctic_time, aes(x = n_points, y = time, color = "conventional"), size = 2) +
    scale_color_manual(breaks = c("conventional", "proposed1", "proposed2"), values = c("black","royalblue1", "red"), guide = "legend", name = "method", 
                                    labels = c("conventional", "proposed 1", "proposed 2"))
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
t2_inted_time_plt_all<-t2_inted_time_plt_sd + geom_point(data = t2_inted_time, aes(x = n_points, y = time, color = "proposed1"), size = 2)

#exp距離変化手法
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
t2_wvr_time_plt_all<-t2_wvr_time_plt_sd + geom_point(data = t2_wvr_time, aes(x = n_points, y = time, colour = "proposed2"), size = 2)

#ggplotによる300~350点2次元トーラスの計算時間グラフ
#軸ラベルや凡例の調整
{t2_time_plt_whole<-t2_wvr_time_plt_all +labs(x = "Data density", y = "Computational time [sec]") + 
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

# plot_success_rates(data = lst(t2_ctic_time, t2_inted_time, t2_wvr_time), aes_y = "time", ylim = c(0, NA),
#                    fill_alpha = 0.1, scale_label = seq(300, 350, by = 10), xlab = "The number of points", ylab = "Computation time", white = T)
