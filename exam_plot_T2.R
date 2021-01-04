#2次元トーラスの実験結果を、超低密度領域を含めて改めて整理

trs250_rate<-aggr_success_rates(trs250_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs260_rate<-aggr_success_rates(trs260_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs270_rate<-aggr_success_rates(trs270_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs280_rate<-aggr_success_rates(trs280_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)
trs290_rate<-aggr_success_rates(trs290_list1to5_aggr, c(2, 1)) %>% do.call(rbind, .)

t2_ctic_rate<-tibble("250"=trs250_rate,
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
