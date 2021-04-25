#4次元直方体の実験結果プロット
cb4d_250_wvr_rate<-map_dbl(lst(cube4d_250_lst1_1to20_wvr_aggr2, cube4d_250_lst1_21to30_wvr_aggr, cube4d_250_lst1_31to40_wvr_aggr, cube4d_250_lst1_41to50_wvr_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50
cb4d_240_wvr_rate<-map_dbl(lst(cube4d_240_lst1_1to20_wvr_aggr, cube4d_240_lst1_21to30_wvr_aggr, cube4d_240_lst1_31to40_wvr_aggr, cube4d_240_lst1_41to50_wvr_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50
cb4d_230_wvr_rate<-map_dbl(lst(cube4d_230_lst1_1to20_wvr_aggr, cube4d_230_lst1_21to30_wvr_aggr, cube4d_230_lst1_31to40_wvr_aggr, cube4d_230_lst1_41to50_wvr_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50
cb4d_220_wvr_rate<-map_dbl(lst(cube4d_220_lst1_1to20_wvr_aggr, cube4d_220_lst1_21to30_wvr_aggr, cube4d_220_lst1_31to40_wvr_aggr, cube4d_220_lst1_41to50_wvr_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50
cb4d_210_wvr_rate<-map_dbl(lst(cube4d_210_lst1_1to20_wvr_aggr, cube4d_210_lst1_21to30_wvr_aggr, cube4d_210_lst1_31to40_wvr_aggr, cube4d_210_lst1_41to50_wvr_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50
cb4d_200_wvr_rate<-map_dbl(lst(cube4d_200_lst1_1to20_wvr_aggr, cube4d_200_lst1_21to30_wvr_aggr, cube4d_200_lst1_31to40_wvr_aggr, cube4d_200_lst1_41to50_wvr_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50

cb4d_250_ctic_rate<-map_dbl(lst(cube4d_250_lst1_1to20_aggr, cube4d_250_lst1_21to30_aggr, cube4d_250_lst1_31to40_aggr, cube4d_250_lst1_41to50_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50
cb4d_240_ctic_rate<-map_dbl(lst(cube4d_240_lst1_1to20_aggr, cube4d_240_lst1_21to30_aggr, cube4d_240_lst1_31to40_aggr, cube4d_240_lst1_41to50_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50
cb4d_230_ctic_rate<-map_dbl(lst(cube4d_230_lst1_1to20_aggr, cube4d_230_lst1_21to30_aggr, cube4d_230_lst1_31to40_aggr, cube4d_230_lst1_41to50_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50
cb4d_220_ctic_rate<-map_dbl(lst(cube4d_220_lst1_1to20_aggr, cube4d_220_lst1_21to30_aggr, cube4d_220_lst1_31to40_aggr, cube4d_220_lst1_41to50_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50
cb4d_210_ctic_rate<-map_dbl(lst(cube4d_210_lst1_1to20_aggr, cube4d_210_lst1_21to30_aggr, cube4d_210_lst1_31to40_aggr, cube4d_210_lst1_41to50_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50
cb4d_200_ctic_rate<-map_dbl(lst(cube4d_200_lst1_1to20_aggr, cube4d_200_lst1_21to30_aggr, cube4d_200_lst1_31to40_aggr, cube4d_200_lst1_41to50_aggr), ~{cycle_number(., 4)[2]}) %>% sum()/50

cb4d_wvr_rate<-tibble("250"=cb4d_250_wvr_rate,
                      "240"=cb4d_240_wvr_rate,
                      "230"=cb4d_230_wvr_rate,
                      "220"=cb4d_220_wvr_rate,
                      "210"=cb4d_210_wvr_rate,
                      "200"=cb4d_200_wvr_rate) %>% 
  pivot_longer(cols = as.character(seq(200, 250, by=10)), names_to = "n_points", values_to = "rate") 


cb4d_wvr_rate$n_points %<>% as.numeric() 

cb4d_ctic_rate<-tibble("250"=cb4d_250_ctic_rate,
                      "240"=cb4d_240_ctic_rate,
                      "230"=cb4d_230_ctic_rate,
                      "220"=cb4d_220_ctic_rate,
                      "210"=cb4d_210_ctic_rate,
                      "200"=cb4d_200_ctic_rate) %>% 
  pivot_longer(cols = as.character(seq(200, 250, by=10)), names_to = "n_points", values_to = "rate")


cb4d_ctic_rate$n_points %<>% as.numeric() 

cb4d_rate_plt<-plot_success_rates(data = lst(cb4d_ctic_rate, cb4d_wvr_rate), aes_y = "rate", plot_ave = T, plot_sd = F, 
                   point_size = 3, xlab = "The number of points", scale_label = seq(200, 250, by=10), 
                   legend_labels = c("Futagami", "Proposed"), col_vals = c("black", "red"), axis_text_size = 25, 
                   axis_title_size = 30, legend_text_size = 25, legend_title_size = 30)

ggsave("./pics/cb4d_rate_plt3.pdf", plot = cb4d_rate_plt, height = 8.5, width = 14, units = "in")

