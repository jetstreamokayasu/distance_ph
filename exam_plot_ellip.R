#楕円体実験まとめ

#---------------------------------------
#CTIC2019手法-------------
ellip150_rate<-aggr_success_rates(c(list(ellip150_aggr1), ellip150_list2to5_aggr), c(0, 1)) %>% do.call(rbind, .)
ellip160_rate<-aggr_success_rates(ellip160_list1to5_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip170_rate<-aggr_success_rates(ellip170_list1to5_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip180_rate<-aggr_success_rates(ellip180_list1to5_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip190_rate<-aggr_success_rates(ellip190_list1to5_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip200_rate<-aggr_success_rates(ellip200_list1to5_aggr, c(0, 1)) %>% do.call(rbind, .)

ellip_rates_tbl<-tibble("150"=ellip150_rate,
                        "160"=ellip160_rate,
                        "170"=ellip170_rate,
                        "180"=ellip180_rate,
                        "190"=ellip190_rate,
                        "200"=ellip200_rate) %>% 
  pivot_longer(cols = as.character(seq(150, 200, by=10)), names_to = "n_points", values_to = "rate") %$%
  tibble("n_points"=n_points, 
         "dim1rate"=unlist(rate[,1]),
         "dim2rate"=unlist(rate[,2])) %>% 
  dplyr::arrange(n_points)

ellip_rates_tbl$n_points %<>% as.numeric()

ellip_rates_smz<-ellip_rates_tbl %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("dim"))


ellip_plt_ave<-ggplot(data = ellip_rates_smz, mapping = aes(x = n_points, y = dim2rate_mean)) + geom_line()
ellip_plt_sd<-ellip_plt_ave + geom_ribbon(aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), alpha = 0.1)
{ellip_plt_all<-ellip_plt_sd + geom_point(data = ellip_rates_tbl, aes(x = n_points, y = dim2rate, color = "conventional"), size = 2) +
  ylim(0, 1) + 
    scale_color_manual(breaks = c("conventional", "proposed1", "proposed2"), values = c("black","royalblue1", "red"), guide = "legend", name = "method", 
                                                              labels = c("conventional", "proposed 1", "proposed 2"))
}

#--------------------------------------
#補間手法---------
ellip150_inted_rate<-aggr_success_rates(c(list(ellip150_inted_aggr1), ellip150_list2to5_inted_aggr), c(0, 1)) %>% do.call(rbind, .)
ellip160_inted_rate<-aggr_success_rates(ellip160_list1to5_inted_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip170_inted_rate<-aggr_success_rates(ellip160_list1to5_inted_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip180_inted_rate<-aggr_success_rates(ellip170_list1to5_inted_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip190_inted_rate<-aggr_success_rates(ellip180_list1to5_inted_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip200_inted_rate<-aggr_success_rates(ellip200_list1to5_inted_aggr, c(0, 1)) %>% do.call(rbind, .)

ellip_inted_rates_tbl<-tibble("150"=ellip150_inted_rate,
                              "160"=ellip160_inted_rate,
                              "170"=ellip170_inted_rate,
                              "180"=ellip180_inted_rate,
                              "190"=ellip190_inted_rate,
                              "200"=ellip200_inted_rate) %>% 
  pivot_longer(cols = as.character(seq(150, 200, by=10)), names_to = "n_points", values_to = "rate") %$%
  tibble("n_points"=n_points, 
         "dim1rate"=unlist(rate[,1]),
         "dim2rate"=unlist(rate[,2])) %>% 
  dplyr::arrange(n_points)

ellip_inted_rates_tbl$n_points %<>% as.numeric()

ellip_inted_rates_smz<-ellip_inted_rates_tbl %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("dim"))

ellip_inted_plt_ave<-ellip_plt_all + geom_line(data = ellip_inted_rates_smz, aes(x = n_points, y = dim2rate_mean), color = "royalblue1")
ellip_inted_plt_sd<-ellip_inted_plt_ave + geom_ribbon(data = ellip_inted_rates_smz, aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), fill = "royalblue1", alpha = 0.1)
ellip_inted_plt_all<-ellip_inted_plt_sd + geom_point(data = ellip_inted_rates_tbl, aes(x = n_points, y = dim2rate, color = "proposed1"), size = 2)


#-------------------------------------------
#expを掛ける距離変化手法
ellip150_wvr_rate<-aggr_success_rates(c(list(ellip150_wvr_aggr1), ellip150_list2to5_wvr_aggr), c(0, 1)) %>% do.call(rbind, .)
ellip160_wvr_rate<-aggr_success_rates(ellip160_list1to5_wvr_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip170_wvr_rate<-aggr_success_rates(ellip160_list1to5_wvr_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip180_wvr_rate<-aggr_success_rates(ellip170_list1to5_wvr_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip190_wvr_rate<-aggr_success_rates(ellip180_list1to5_wvr_aggr, c(0, 1)) %>% do.call(rbind, .)
ellip200_wvr_rate<-aggr_success_rates(ellip200_list1to5_wvr_aggr, c(0, 1)) %>% do.call(rbind, .)

ellip_wvr_rates_tbl<-tibble("150"=ellip150_wvr_rate,
                            "160"=ellip160_wvr_rate,
                            "170"=ellip170_wvr_rate,
                            "180"=ellip180_wvr_rate,
                            "190"=ellip190_wvr_rate,
                            "200"=ellip200_wvr_rate) %>% 
  pivot_longer(cols = as.character(seq(150, 200, by=10)), names_to = "n_points", values_to = "rate") %$%
  tibble("n_points"=n_points, 
         "dim1rate"=unlist(rate[,1]),
         "dim2rate"=unlist(rate[,2])) %>% 
  dplyr::arrange(n_points)

ellip_wvr_rates_tbl$n_points %<>% as.numeric()

ellip_wvr_rates_smz<-ellip_wvr_rates_tbl %>% group_by(n_points) %>% 
  dplyr::summarise_each(lst(mean, sd), starts_with("dim"))

ellip_wvr_plt_ave<-ellip_inted_plt_all + geom_line(data = ellip_wvr_rates_smz, aes(x = n_points, y = dim2rate_mean), color = "red")
ellip_wvr_plt_sd<-ellip_wvr_plt_ave + geom_ribbon(data = ellip_wvr_rates_smz, aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), fill = "red", alpha = 0.1)
ellip_wvr_plt_all<-ellip_wvr_plt_sd + geom_point(data = ellip_wvr_rates_tbl, aes(x = n_points, y = dim2rate, colour = "proposed2"), size = 2)

#楕円体表面積(近似値)
e_surf<-ellip_surface(5, 1, 1)

{
  ellip_whole_plt<-ellip_wvr_plt_all + labs(x = "Data density", y = "Success rate") + theme(axis.text = element_text(size=20), axis.title = element_text(size=25), legend.text = element_text(size=20), legend.title = element_text(size=25)) +
  scale_x_continuous(breaks = seq(150, 200, by=10), labels =  round(seq(150, 200, by=10)/e_surf, digits = 3))
}

plot(sapply(seq(450, 500, by=10), function(k){expression(k/(64*pi^3))}), ellip_wvr_rates_tbl$dim2rate)
