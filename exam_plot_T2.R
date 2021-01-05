#2次元トーラスの実験結果を、超低密度領域を含めて改めて整理

trs250_rate<-aggr_success_rates(trs250_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs260_rate<-aggr_success_rates(trs260_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs270_rate<-aggr_success_rates(trs270_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs280_rate<-aggr_success_rates(trs280_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs290_rate<-aggr_success_rates(trs290_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)

t2_low_dens_ctic_rate<-tibble("250"=trs250_rate,
                     "260"=trs260_rate,
                     "270"=trs270_rate,
                     "280"=trs280_rate,
                     "290"=trs290_rate) %>% 
  pivot_longer(cols = as.character(seq(250, 290, by=10)), names_to = "n_points", values_to = "rate") %$%
  tibble("n_points"=n_points, 
         "dim1rate"=unlist(rate[,1]),
         "dim2rate"=unlist(rate[,2])) %>% 
  dplyr::arrange(n_points) 

t2_ctic_rate$n_points %<>% as.numeric() 
t2_ctic_rate %<>% bind_rows(sucs_rates_tbl)

t2_ctic_rate_smz<-t2_ctic_rate %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("dim"))

# trs290_list1_2_inst<-TDAdataset$new(trs290_list1to5[[1]][[2]])
# calculate_homology(mat = trs290_list1to5[[1]][[2]], dim = 2) %>%  trs290_list1_2_inst$input_pd()
t2_plt_ave<-ggplot(data = t2_ctic_rate_smz, mapping = aes(x = n_points, y = dim2rate_mean)) + geom_line() 
t2_plt_sd<-t2_plt_ave + geom_ribbon(aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), alpha = 0.1)
{
t2_plt_all<-t2_plt_sd + geom_point(data = sucs_rates_tbl, aes(x = n_points, y = dim2rate, color = "conventional"), size = 2) +
     ylim(0, 1) + scale_color_manual(breaks = c("conventional", "proposed1", "proposed2"), values = c("black","royalblue1", "red"), guide = "legend", name = "method", 
                                      labels = c("conventional", "proposed 1", "proposed 2"))
}                 

#--------------------------------------------
#2次元トーラスの成功率をまとめる-------------

#CTIC2019手法
trs300_ctic_rate<-aggr_success_rates(c(list(trs300_colle1_ctic_aggr), trs300_list2to5_aggr), c(2, 1)) %>% do.call(rbind, .)
trs310_ctic_rate<-aggr_success_rates(trs310_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs320_ctic_rate<-aggr_success_rates(trs320_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs330_ctic_rate<-aggr_success_rates(trs330_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs340_ctic_rate<-aggr_success_rates(trs340_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs350_ctic_rate<-aggr_success_rates(c(list(trs350_colle1_ctic_aggr), trs350_list2to5_aggr), c(2, 1)) %>% do.call(rbind, .)

t2_300lv_ctic_rate<-tibble("300"=trs300_ctic_rate,
                     "310"=trs310_ctic_rate,
                     "320"=trs320_ctic_rate,
                     "330"=trs330_ctic_rate,
                     "340"=trs340_ctic_rate,
                     "350"=trs350_ctic_rate) %>% 
  pivot_longer(cols = as.character(seq(300, 350, by=10)), names_to = "n_points", values_to = "rate") %$%
  tibble("n_points"=n_points, 
         "dim1rate"=unlist(rate[,1]),
         "dim2rate"=unlist(rate[,2])) %>% 
  dplyr::arrange(n_points) 

t2_300lv_ctic_rate$n_points %<>% as.numeric() 
#t2_ctic_rate %<>% bind_rows(sucs_rates_tbl)

t2_300lv_ctic_smz<-t2_300lv_ctic_rate %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("dim"))

#ggplotによるCTIC2019手法のプロット
t2_300lv_ctic_plt_ave<-ggplot(data = t2_300lv_ctic_smz, mapping = aes(x = n_points, y = dim2rate_mean)) + geom_line() 
t2_300lv_ctic_plt_sd<-t2_300lv_ctic_plt_ave + geom_ribbon(aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), alpha = 0.1)
{
  t2_300lv_ctic_plt_all<-t2_300lv_ctic_plt_sd + geom_point(data = t2_300lv_ctic_rate, aes(x = n_points, y = dim2rate, color = "conventional"), size = 2) +
    ylim(0, 1) + scale_color_manual(breaks = c("conventional", "proposed1", "proposed2"), values = c("black","royalblue1", "red"), guide = "legend", name = "method", 
                                    labels = c("conventional", "proposed 1", "proposed 2"))
}

#ggplotによる300~350点2次元トーラスの成功率グラフ
#軸ラベルや凡例の調整
{t2_300lv_ctic_plt_whole<-t2_300lv_ctic_plt_all +labs(x = "Data density", y = "Success rate") + theme(axis.text = element_text(size=25), axis.title = element_text(size=30), legend.text = element_text(size=25), legend.title = element_text(size=30)) +
    scale_x_continuous(breaks = seq(300, 350, by=10), labels = c(expression(30/pi^2), expression(31/pi^2), expression(32/pi^2), expression(33/pi^2), expression(34/pi^2), expression(35/pi^2)) )
}

ggsave("./pics/conventional_T2H2_plot3.pdf", plot = t2_300lv_ctic_plt_whole)

#補間手法
trs300_inted_rate<-aggr_success_rates(trs300_list1to5_inted_aggr, c(2, 1)) %>% do.call(rbind, .)
trs310_inted_rate<-aggr_success_rates(trs310_list1to5_inted_aggr, c(2, 1)) %>% do.call(rbind, .)
trs320_inted_rate<-aggr_success_rates(trs320_list1to5_inted_aggr, c(2, 1)) %>% do.call(rbind, .)
trs330_inted_rate<-aggr_success_rates(trs330_list1to5_inted_aggr, c(2, 1)) %>% do.call(rbind, .)
trs340_inted_rate<-aggr_success_rates(trs340_list1to5_inted_aggr, c(2, 1)) %>% do.call(rbind, .)
trs350_inted_rate<-aggr_success_rates(trs350_list1to5_inted_aggr, c(2, 1)) %>% do.call(rbind, .)

t2_300lv_inted_rate<-tibble("300"=trs300_inted_rate,
                           "310"=trs310_inted_rate,
                           "320"=trs320_inted_rate,
                           "330"=trs330_inted_rate,
                           "340"=trs340_inted_rate,
                           "350"=trs350_inted_rate) %>% 
  pivot_longer(cols = as.character(seq(300, 350, by=10)), names_to = "n_points", values_to = "rate") %$%
  tibble("n_points"=n_points, 
         "dim1rate"=unlist(rate[,1]),
         "dim2rate"=unlist(rate[,2])) %>% 
  dplyr::arrange(n_points) 

t2_300lv_inted_rate$n_points %<>% as.numeric() 
#t2_ctic_rate %<>% bind_rows(sucs_rates_tbl)

t2_300lv_inted_smz<-t2_300lv_inted_rate %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("dim"))

#ggplotによる補間手法のプロット
t2_300lv_inted_plt_ave<-t2_300lv_ctic_plt_whole + geom_line(data = t2_300lv_inted_smz, aes(x = n_points, y = dim2rate_mean), color = "royalblue1")
t2_300lv_inted_plt_sd<-t2_300lv_inted_plt_ave + geom_ribbon(data = t2_300lv_inted_smz, aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), fill = "royalblue1", alpha = 0.1)
t2_300lv_inted_plt_all<-t2_300lv_inted_plt_sd + geom_point(data = t2_300lv_inted_rate, aes(x = n_points, y = dim2rate, color = "proposed1"), size = 2)

#ggplotによるexp距離変化手法のプロット
t2_300lv_wvr_plt_ave<-t2_300lv_inted_plt_all + geom_line(data = wvr_rates_smz, aes(x = n_points, y = dim2rate_mean), color = "red")
t2_300lv_wvr_plt_sd<-t2_300lv_wvr_plt_ave + geom_ribbon(data = wvr_rates_smz, aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), fill = "red", alpha = 0.1)
t2_300lv_wvr_plt_all<-t2_300lv_wvr_plt_sd + geom_point(data = wvr_rates_tbl, aes(x = n_points, y = dim2rate, colour = "proposed2"), size = 2)

ggsave("./pics/success_T2H2_plot3.pdf", plot = t2_300lv_wvr_plt_all)
