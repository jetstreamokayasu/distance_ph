#tibble試し

#-----------------
#2次元トーラスの成功率をまとめてみる--------
#expを掛ける距離変化手法

wvr_rates_tbl<-tibble("300"=torus300_wvr_rate,
                   "310"=torus310_wvr_rate,
                   "320"=torus320_wvr_rate,
                   "330"=torus330_wvr_rate,
                   "340"=torus340_wvr_rate,
                   "350"=torus350_wvr_rate) %>% 
  pivot_longer(cols = as.character(seq(300, 350, by=10)), names_to = "n_points", values_to = "rate") %$%
  tibble("n_points"=n_points, 
         "dim1rate"=unlist(rate[,1]),
         "dim2rate"=unlist(rate[,2])) %>% 
  dplyr::arrange(n_points)
  
wvr_rates_tbl$n_points %<>% as.numeric()

wvr_rates_smz<-wvr_rates_tbl %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("dim"))

#-------------------------
#CTIC2019手法-------------

sucs_rates_tbl<-tibble("300"=sucs300_rate,
                 "310"=sucs310_rate,
                 "320"=sucs320_rate,
                 "330"=sucs330_rate,
                 "340"=sucs340_rate,
                 "350"=sucs350_rate) %>% 
  pivot_longer(cols = as.character(seq(300, 350, by=10)), names_to = "n_points", values_to = "rate") %$%
  tibble("n_points"=n_points, 
         "dim1rate"=unlist(rate[,1]),
         "dim2rate"=unlist(rate[,2])) %>% 
  dplyr::arrange(n_points)

sucs_rates_tbl$n_points %<>% as.numeric() 

sucs_rates_smz<-sucs_rates_tbl %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("dim"))

#--------------------------------
#補間手法--------

inter_rates_tbl<-tibble("300"=in300_rates,
                        "310"=in310_rates,
                        "320"=in320_rates,
                        "330"=in330_rates,
                        "340"=in340_rates,
                        "350"=in350_rates) %>% 
  pivot_longer(cols = as.character(seq(300, 350, by=10)), names_to = "n_points", values_to = "rate") %$%
  tibble("n_points"=n_points, 
         "dim1rate"=unlist(rate[,1]),
         "dim2rate"=unlist(rate[,2])) %>% 
  dplyr::arrange(n_points)

inter_rates_tbl$n_points %<>% as.numeric() 

inter_rates_smz<-inter_rates_tbl %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("dim"))

#--------------------
#ggplotを使って描画---------

#何故かリボンがかけない
suc_plot<-ggplot(data = sucs_rates_smz) + geom_point(data = sucs_rates_tbl, aes(x = n_points, y = dim2rate)) + ylim(0, 1)
suc_plot_ave<-suc_plot + geom_line(aes(x = n_points, y = dim2rate_mean), group=1)
suc_plot_sd<-suc_plot_ave + geom_ribbon(data=sucs_rates_smz, aes(x = sucs_rates_smz$n_points, ymin = sucs_rates_smz$dim2rate_mean+0.1, ymax = sucs_rates_smz$dim2rate_mean+0.1))

#こっちの場合は出る
suc_plot_ave<-ggplot(data = sucs_rates_smz, mapping = aes(x = n_points, y = dim2rate_mean)) + geom_line() 
suc_plot_sd<-suc_plot_ave + geom_ribbon(aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), alpha = 0.1)
suc_plot_all<-suc_plot_sd + geom_point(data = sucs_rates_tbl, aes(x = n_points, y = dim2rate, color = "conventional"), size = 1.8) 
suc_plot_all2<-suc_plot_all + ylim(0, 1) + scale_color_manual(breaks = c("conventional", "proposed1", "proposed2"), values = c("black","royalblue1", "red"), guide = "legend", name = "method", 
                                                              labels = c("conventional", "poprosed 1", "proposed 2"))

ctic_plt<-suc_plot_all2 +labs(x = "Data density", y = "Success rate") + theme(axis.text = element_text(size=14), axis.title = element_text(size=16), legend.text = element_text(size=12), legend.title = element_text(size=15))
ctic_plt2<-ctic_plt + scale_x_continuous(breaks = seq(300, 350, by=10), labels = c(expression(30/pi^2), expression(31/pi^2), expression(32/pi^2), expression(33/pi^2), expression(34/pi^2), expression(35/pi^2)) )
ggsave("./pics/conventional_T2H2_plot.pdf", plot = ctic_plt2)

#expを掛ける距離変化手法のプロット
wvr_plot1<-ctic_plt2 + geom_line(data = wvr_rates_smz, aes(x = n_points, y = dim2rate_mean), color = "red")
wvr_plot2<-wvr_plot1 + geom_ribbon(data = wvr_rates_smz, aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), fill = "red", alpha = 0.1)
wvr_plot3<-wvr_plot2 + geom_point(data = wvr_rates_tbl, aes(x = n_points, y = dim2rate, colour = "proposed2"))


#補間手法のプロット
inter_plot_ave<-wvr_plot3 + geom_line(data = inter_rates_smz, aes(x = n_points, y = dim2rate_mean), color = "royalblue1")
inter_plot_sd<-inter_plot_ave + geom_ribbon(data = inter_rates_smz, aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), fill = "royalblue1", alpha = 0.1)
inter_plot_all<-inter_plot_sd + geom_point(data = inter_rates_tbl, aes(x = n_points, y = dim2rate, color = "proposed1"))

ggsave("./pics/success_T2H2_plot.pdf", plot = inter_plot_all)

#成功率グラフ全体
#success_plt<-inter_plot_all + labs(x = "Number of points", y = "Success rate")

#------------------------------
#3次元トーラス2次ベッチ数推定成功率グラフ------

t3_H2rate<-tibble("500"=t3rs500_rate_H2,
                  "490"=t3rs490_rate_H2,
                  "480"=t3rs480_rate_H2,
                  "470"=t3rs470_rate_H2,
                  "460"=t3rs460_rate_H2,
                  "450"=t3rs450_rate_H2) %>% 
  pivot_longer(cols = as.character(seq(450, 500, by=10)), names_to = "n_points", values_to = "dim2rate")

t3_H2rate$n_points %<>% as.numeric()

#CTIC2019手法
t3_suc_plt<-ggplot(data = t3_H2rate, aes(x = n_points)) + geom_point(aes(y = dim2rate, color = "conventional")) + geom_line(aes(y = dim2rate))
t3_suc_plt_arng<-t3_suc_plt + ylim(0, 1) + scale_color_manual(breaks = c("conventional", "proposed2A", "proposed2B"), values = c("black","darkorange1", "maroon1"), guide = "legend", name = "method", 
                                                              labels = c("conventional", expression(paste(eta==6.5, ~~epsilon==0.5)), expression(paste(eta==4.0, ~~epsilon==0.5))))

#exp距離変化。eta=6.5, l_rate=0.5
t3_wvr_H2rateA<-tibble("450"=t3rs450_wvr_rate_H2,
                       "460"=t3rs460_wvr_rate_H2,
                       "470"=t3rs470_wvr_rate_H2,
                       "480"=t3rs480_wvr_rate_H2,
                       "490"=t3rs490_wvr_rate_H2,
                       "500"=t3rs500_wvr_rate_H2) %>% 
  pivot_longer(cols = as.character(seq(450, 500, by=10)), names_to = "n_points", values_to = "dim2rate")

t3_wvr_H2rateA$n_points %<>% as.numeric()

t3_wvr_plt<-t3_suc_plt_arng + geom_point(data = t3_wvr_H2rateA, aes(y = dim2rate, color = "proposed2A")) + geom_line(data = t3_wvr_H2rateA, aes(y = dim2rate, color = "proposed2A"))

##exp距離変化。eta=4.0, l_rate=0.5
t3_wvr_H2rateB<-tibble("450"=cycle_number(t3orus450_list1_1to30_wvrH2_aggrs2, 2)[4]/30,
                       "460"=cycle_number(t3orus460_list1_1to30_wvrH2_aggrs2, 2)[4]/30,
                       "470"=cycle_number(t3orus470_list1_1to30_wvrH2_aggrs2, 2)[4]/30,
                       "480"=cycle_number(t3orus480_list1_1to30_wvrH2_aggrs2, 2)[4]/30,
                       "490"=cycle_number(t3orus490_list1_1to30_wvrH2_aggrs2, 2)[4]/30,
                       "500"=cycle_number(t3orus4_list3_1to30_wvrH2_aggrs6, 2)[4]/30) %>% 
  pivot_longer(cols = as.character(seq(450, 500, by=10)), names_to = "n_points", values_to = "dim2rate")

t3_wvr_H2rateB$n_points %<>% as.numeric()

t3_wvr_pltB<-t3_wvr_plt + geom_point(data = t3_wvr_H2rateB, aes(y = dim2rate, color = "proposed2B")) + geom_line(data = t3_wvr_H2rateB, aes(y = dim2rate, color = "proposed2B"))
t3_plt<-t3_wvr_pltB + labs(x = "Data density", y = "Success rate") + theme(axis.text = element_text(size=14), axis.title = element_text(size=16), legend.text = element_text(size=11), legend.title = element_text(size=15))
t3_T3H2_plt2<-t3_plt + scale_x_continuous(breaks = seq(450, 500, by=10), labels = c(expression(450/(64*pi^3)), expression(460/(64*pi^3)), expression(470/(64*pi^3)), expression(480/(64*pi^3)), expression(490/(64*pi^3)), expression(500/(64*pi^3))))

ggsave("./pics/success_T3H2_plot.pdf", plot = t3_T3H2_plt2)

#------------------------------
#3次元トーラス3次ベッチ数推定成功率グラフ------
t3_H3rate<-tibble("500"=t3rs500_rate,
                  "490"=t3rs490_rate,
                  "480"=t3rs480_rate,
                  "470"=t3rs470_rate,
                  "460"=t3rs460_rate,
                  "450"=t3rs450_rate) %>% 
  pivot_longer(cols = as.character(seq(450, 500, by=10)), names_to = "n_points", values_to = "dim3rate")

t3_H3rate$n_points %<>% as.numeric()

#CTIC2019手法
t3_H3_plt<-ggplot(data = t3_H3rate, aes(x = n_points)) + geom_point(aes(y = dim3rate, color = "conventional")) + geom_line(aes(y = dim3rate))
t3_H3_plt_arng<-t3_H3_plt + ylim(0, 1) + scale_color_manual(breaks = c("conventional", "proposed2A", "proposed2B"), values = c("black","darkorange1", "maroon1"), guide = "legend", name = "method", 
                                                              labels = c("conventional", expression(paste(eta==6.5, ~~epsilon==0.5)), expression(paste(eta==4.0, ~~epsilon==0.5))))

#exp距離変化。eta=6.5, l_rate=0.5
t3_wvr_H3rateA<-tibble("450"=t3rs450_wvr_rate,
                       "460"=t3rs460_wvr_rate,
                       "470"=t3rs470_wvr_rate,
                       "480"=t3rs480_wvr_rate,
                       "490"=t3rs490_wvr_rate,
                       "500"=t3rs500_wvr_rate) %>% 
  pivot_longer(cols = as.character(seq(450, 500, by=10)), names_to = "n_points", values_to = "dim3rate")

t3_wvr_H3rateA$n_points %<>% as.numeric()

t3_wvrH3_plt<-t3_H3_plt_arng + geom_point(data = t3_wvr_H3rateA, aes(y = dim3rate, color = "proposed2A")) + geom_line(data = t3_wvr_H3rateA, aes(y = dim3rate, color = "proposed2A"))

#exp距離変化。eta=4.0, l_rate=0.5
t3_wvr_H3rateB<-tibble("450"=cycle_number(t3orus450_list1_1to30_wvrH2_aggrs2, 3)[2]/30,
                       "460"=cycle_number(t3orus460_list1_1to30_wvrH2_aggrs2, 3)[2]/30,
                       "470"=cycle_number(t3orus470_list1_1to30_wvrH2_aggrs2, 3)[2]/30,
                       "480"=cycle_number(t3orus480_list1_1to30_wvrH2_aggrs2, 3)[2]/30,
                       "490"=cycle_number(t3orus490_list1_1to30_wvrH2_aggrs2, 3)[2]/30,
                       "500"=cycle_number(t3orus4_list3_1to30_wvrH2_aggrs6, 3)[2]/30) %>% 
  pivot_longer(cols = as.character(seq(450, 500, by=10)), names_to = "n_points", values_to = "dim3rate")

t3_wvr_H3rateB$n_points %<>% as.numeric()

t3_wvrH3_pltB<-t3_wvrH3_plt + geom_point(data = t3_wvr_H3rateB, aes(y = dim3rate, color = "proposed2B")) + geom_line(data = t3_wvr_H3rateB, aes(y = dim3rate, color = "proposed2B"))
t3_H3_plt<-t3_wvrH3_pltB + labs(x = "Data density", y = "Success rate") + theme(axis.text = element_text(size=14), axis.title = element_text(size=16), legend.text = element_text(size=11), legend.title = element_text(size=15))

t3_H3_plt2<-t3_H3_plt + scale_x_continuous(breaks = seq(450, 500, by=10), labels = c(expression(450/(64*pi^3)), expression(460/(64*pi^3)), expression(470/(64*pi^3)), expression(480/(64*pi^3)), expression(490/(64*pi^3)), expression(500/(64*pi^3))))

ggsave("./pics/success_T3H3_plot.pdf", plot = t3_H3_plt2)
