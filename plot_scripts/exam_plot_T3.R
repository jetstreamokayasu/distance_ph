#3次元トーラスの成功率グラフ
#実験結果のプロット
#プロット領域準備
oldpar <- par(no.readonly = TRUE)
par(mgp=c(2.5, 1, 0))
plot(500, t3rs500_rate, pch=16, cex.axis=1.6, xlab="Data Density", ylab="Success Rates", cex.lab=1.6, xlim=c(450, 500), ylim=c(0.0, 1.0), xaxt="n", type="n")
axis(side=1, at=seq(450, 500, by=10), labels=c(paste0(seq(450, 500, by=10), "/(64*pi^3)")), cex.axis=1.1)

t3rs500_rate<-map_dbl(list(t3orus4_list3_1to30_normal_aggrs, t3orus4_list3_31to100_normal_aggrs), ~cycle_number(., 3)[2]) %>% sum()/100
t3rs490_rate<-map_dbl(list(t3orus490_list1_1to30_normal_aggrs, t3orus490_list1_31to100_normal_aggrs), ~cycle_number(., 3)[2]) %>% sum()/100
t3rs480_rate<-map_dbl(list(t3orus480_list1_1to30_normal_aggrs, t3orus480_list1_31to100_normal_aggrs), ~cycle_number(., 3)[2]) %>% sum()/100
t3rs470_rate<-map_dbl(list(t3orus470_list1_1to30_normal_aggrs, t3orus470_list1_31to100_normal_aggrs), ~cycle_number(., 3)[2]) %>% sum()/100
t3rs460_rate<-map_dbl(list(t3orus460_list1_1to30_normal_aggrs, t3orus460_list1_31to100_normal_aggrs), ~cycle_number(., 3)[2]) %>% sum()/100
t3rs450_rate<-map_dbl(list(t3orus450_list1_1to30_normal_aggrs, t3orus450_list1_31to100_normal_aggrs), ~cycle_number(., 3)[2]) %>% sum()/100

#CTIC2019手法
#3次ベッチ数推定成功率プロット
black<-"gray22"
points(500, t3rs500_rate, pch=16, col=black)
points(490, t3rs490_rate, pch=16, col=black)
points(480, t3rs480_rate, pch=16, col=black)
points(470, t3rs470_rate, pch=16, col=black)
points(460, t3rs460_rate, pch=16, col=black)
points(450, t3rs450_rate, pch=16, col=black)

lines(seq(450, 500, by=10), c(t3rs450_rate, t3rs460_rate, t3rs470_rate, t3rs480_rate, t3rs490_rate, t3rs500_rate), col=black)

#CTIC2019手法
#2次ベッチ数推定成功率プロット
t3rs500_rate_H2<-map_dbl(list(t3orus4_list3_1to30_normal_aggrs, t3orus4_list3_31to100_normal_aggrs), ~cycle_number(., 2)[4]) %>% sum()/100
t3rs490_rate_H2<-map_dbl(list(t3orus490_list1_1to30_normal_aggrs, t3orus490_list1_31to100_normal_aggrs), ~cycle_number(., 2)[4]) %>% sum()/100
t3rs480_rate_H2<-map_dbl(list(t3orus480_list1_1to30_normal_aggrs, t3orus480_list1_31to100_normal_aggrs), ~cycle_number(., 2)[4]) %>% sum()/100
t3rs470_rate_H2<-map_dbl(list(t3orus470_list1_1to30_normal_aggrs, t3orus470_list1_31to100_normal_aggrs), ~cycle_number(., 2)[4]) %>% sum()/100
t3rs460_rate_H2<-map_dbl(list(t3orus460_list1_1to30_normal_aggrs, t3orus460_list1_31to100_normal_aggrs), ~cycle_number(., 2)[4]) %>% sum()/100
t3rs450_rate_H2<-map_dbl(list(t3orus450_list1_1to30_normal_aggrs, t3orus450_list1_31to100_normal_aggrs), ~cycle_number(., 2)[4]) %>% sum()/100

points(500, t3rs500_rate_H2, pch=16, col=black)
points(490, t3rs490_rate_H2, pch=16, col=black)
points(480, t3rs480_rate_H2, pch=16, col=black)
points(470, t3rs470_rate_H2, pch=16, col=black)
points(460, t3rs460_rate_H2, pch=16, col=black)
points(450, t3rs450_rate_H2, pch=16, col=black)

lines(seq(450, 500, by=10), c(t3rs450_rate_H2, t3rs460_rate_H2, t3rs470_rate_H2, t3rs480_rate_H2, t3rs490_rate_H2, t3rs500_rate_H2), col=black)


#-------------------------------
#espを掛ける距離行列変化手法----
t3rs500_wvr_rate<-map_dbl(list(t3orus4_list3_1to30aggrs3, t3orus4_list3_31to100_wvr_aggrs3), ~cycle_number(., 3)[2]) %>% sum()/100
t3rs490_wvr_rate<-map_dbl(list(t3orus490_list1_1to30_wvr_aggrs1, t3orus490_list1_31to100_wvr_aggrs1), ~cycle_number(., 3)[2]) %>% sum()/100
t3rs480_wvr_rate<-map_dbl(list(t3orus480_list1_1to30_wvr_aggrs1, t3orus480_list1_31to100_wvr_aggrs1), ~cycle_number(., 3)[2]) %>% sum()/100
t3rs470_wvr_rate<-map_dbl(list(t3orus470_list1_1to30_wvr_aggrs1, t3orus470_list1_31to100_wvr_aggrs1), ~cycle_number(., 3)[2]) %>% sum()/100
t3rs460_wvr_rate<-map_dbl(list(t3orus460_list1_1to30_wvr_aggrs1, t3orus460_list1_31to100_wvr_aggrs1), ~cycle_number(., 3)[2]) %>% sum()/100
t3rs450_wvr_rate<-map_dbl(list(t3orus450_list1_1to30_wvr_aggrs1, t3orus450_list1_31to100_wvr_aggrs1), ~cycle_number(., 3)[2]) %>% sum()/100



#espを掛ける距離行列変化手法
#3次ベッチ数推定成功率プロット
red<-"darkorange1"
points(500, t3rs500_wvr_rate, pch=16, col=red)
points(490, t3rs490_wvr_rate, pch=16, col=red)
points(480, t3rs480_wvr_rate, pch=16, col=red)
points(470, t3rs470_wvr_rate, pch=16, col=red)
points(460, t3rs460_wvr_rate, pch=16, col=red)
points(450, t3rs450_wvr_rate, pch=16, col=red)

lines(seq(450, 500, by=10), c(t3rs450_wvr_rate, t3rs460_wvr_rate, t3rs470_wvr_rate, t3rs480_wvr_rate, t3rs490_wvr_rate, t3rs500_wvr_rate), col=red)




#espを掛ける距離行列変化手法
#2次ベッチ数推定成功率プロット
t3rs500_wvr_rate_H2<-map_dbl(list(t3orus4_list3_1to30aggrs3, t3orus4_list3_31to100_wvr_aggrs3), ~cycle_number(., 2)[4]) %>% sum()/100
t3rs490_wvr_rate_H2<-map_dbl(list(t3orus490_list1_1to30_wvr_aggrs1, t3orus490_list1_31to100_wvr_aggrs1), ~cycle_number(., 2)[4]) %>% sum()/100
t3rs480_wvr_rate_H2<-map_dbl(list(t3orus480_list1_1to30_wvr_aggrs1, t3orus480_list1_31to100_wvr_aggrs1), ~cycle_number(., 2)[4]) %>% sum()/100
t3rs470_wvr_rate_H2<-map_dbl(list(t3orus470_list1_1to30_wvr_aggrs1, t3orus470_list1_31to100_wvr_aggrs1), ~cycle_number(., 2)[4]) %>% sum()/100
t3rs460_wvr_rate_H2<-map_dbl(list(t3orus460_list1_1to30_wvr_aggrs1, t3orus460_list1_31to100_wvr_aggrs1), ~cycle_number(., 2)[4]) %>% sum()/100
t3rs450_wvr_rate_H2<-map_dbl(list(t3orus450_list1_1to30_wvr_aggrs1, t3orus450_list1_31to100_wvr_aggrs1), ~cycle_number(., 2)[4]) %>% sum()/100


points(500, t3rs500_wvr_rate_H2, pch=16, col=red)
points(490, t3rs490_wvr_rate_H2, pch=16, col=red)
points(480, t3rs480_wvr_rate_H2, pch=16, col=red)
points(470, t3rs470_wvr_rate_H2, pch=16, col=red)
points(460, t3rs460_wvr_rate_H2, pch=16, col=red)
points(450, t3rs450_wvr_rate_H2, pch=16, col=red)

lines(seq(450, 500, by=10), c(t3rs450_wvr_rate_H2, t3rs460_wvr_rate_H2, t3rs470_wvr_rate_H2, t3rs480_wvr_rate_H2, t3rs490_wvr_rate_H2, t3rs500_wvr_rate_H2), col=red)

#-----------------------------
#ggplotでプロット
#計算時間が求められている結果でプロット
t3rs500_wvr_rate<-map_dbl(list(t3orus4_list3_1to30aggrs4, t3orus4_list3_31to100_wvr_aggrs3), ~cycle_number(., 3)[2]) %>% sum()/100
t3rs500_wvr_rate_H2<-map_dbl(list(t3orus4_list3_1to30aggrs4, t3orus4_list3_31to100_wvr_aggrs3), ~cycle_number(., 2)[4]) %>% sum()/100

#結合時刻変化。eta=6.5, l_rate=0.5
#3次ベッチ数
t3_wvr_H3rateA<-tibble("450"=t3rs450_wvr_rate,
                       "460"=t3rs460_wvr_rate,
                       "470"=t3rs470_wvr_rate,
                       "480"=t3rs480_wvr_rate,
                       "490"=t3rs490_wvr_rate,
                       "500"=t3rs500_wvr_rate) %>% 
  pivot_longer(cols = as.character(seq(450, 500, by=10)), names_to = "n_points", values_to = "dim3rate")

t3_wvr_H3rateA$n_points %<>% as.numeric()

#2次ベッチ数
t3_wvr_H2rateA<-tibble("450"=t3rs450_wvr_rate_H2,
                       "460"=t3rs460_wvr_rate_H2,
                       "470"=t3rs470_wvr_rate_H2,
                       "480"=t3rs480_wvr_rate_H2,
                       "490"=t3rs490_wvr_rate_H2,
                       "500"=t3rs500_wvr_rate_H2) %>% 
  pivot_longer(cols = as.character(seq(450, 500, by=10)), names_to = "n_points", values_to = "dim2rate")

t3_wvr_H2rateA$n_points %<>% as.numeric()


t3_H3_ctic_wvr_plt<-plot_success_rates(data = lst(t3_H3rate, t3_wvr_H3rateA, t3_wvr_H3rateB), aes_y = "dim3rate", plot_sd = F, col_vals = c("black","darkorange1", "maroon1"), 
                                            legend_labels = c("Futagami", expression(paste(eta==6.5, ~~epsilon==0.5)), expression(paste(eta==4.0, ~~epsilon==0.5))), point_size = 3, axis_text_size = 25, axis_title_size = 30, legend_text_size = 25, legend_title_size = 30, 
                                       scale_label = seq(450, 500, by=10), xlab = "The number of points")


ggsave("./pics/success_ctic_intr_T3H3_2.pdf", plot = t3_H3_ctic_wvr_plt, height = 8.5, width = 14, units = "in")


t3_H2_ctic_wvr_plt<-plot_success_rates(data = lst(t3_H2rate, t3_wvr_H2rateA, t3_wvr_H2rateB), aes_y = "dim2rate", plot_sd = F, col_vals = c("black","darkorange1", "maroon1"), 
                   legend_labels = c("Futagami", expression(paste(eta==6.5, ~~epsilon==0.5)), expression(paste(eta==4.0, ~~epsilon==0.5))), point_size = 3, axis_text_size = 25, axis_title_size = 30, legend_text_size = 25, legend_title_size = 30, 
                   scale_label = seq(450, 500, by=10), xlab = "The number of points")

ggsave("./pics/success_ctic_intr_T3H2_2.pdf", plot = t3_H2_ctic_wvr_plt, height = 8.5, width = 14, units = "in")
