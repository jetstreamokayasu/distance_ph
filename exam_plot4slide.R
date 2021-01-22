#スライド用のグラフを作成する
#大本のデータ、グラフ設定は修論原稿用のグラフを作成した各スクリプトに有り

#---------------------------
#図形のプロット-------

#3次元トーラス
figurePlot3d(t3orus450_list2[[2]][, 1:3])
spheres3d(t3orus450_list2[[2]][, 1:3], radius = 0.2)
rgl.snapshot("./pics/3d_torus.png")

#楕円体
figurePlot3d(ellip200_list1to5[[1]][[1]])
spheres3d(ellip200_list1to5[[1]][[1]], radius = 0.08)
rgl.snapshot("./pics/ellip.png")

#2次元トーラス
figurePlot3d(torus300_colle_setB[[1]][[1]])
spheres3d(torus300_colle_setB[[1]][[1]], radius = 0.08)
rgl.snapshot("./pics/2d_torus.png")

#---------------------------
#2次元トーラスの2次ベッチ数推定成功率グラフ-----


#ggplotによるCTIC2019手法のプロット
t2_300lv_ctic_plt_ave2<-ggplot(data = t2_300lv_ctic_smz, mapping = aes(x = n_points, y = dim2rate_mean)) + geom_line() 
t2_300lv_ctic_plt_sd2<-t2_300lv_ctic_plt_ave2 + geom_ribbon(aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), alpha = 0.4)
{
  t2_300lv_ctic_plt_all2<-t2_300lv_ctic_plt_sd2 + geom_point(data = t2_300lv_ctic_rate, aes(x = n_points, y = dim2rate, color = "conventional"), size = 3) +
    ylim(0, 1) + scale_color_manual(breaks = c("conventional", "proposed1", "proposed2"), values = c("black","royalblue1", "red"), guide = "legend", name = "method", 
                                    labels = c("conventional", "proposed 1", "proposed 2"))
}

#ggplotによる300~350点2次元トーラスの成功率グラフ
#軸ラベルや凡例の調整
#スライド用の白抜き背景
{t2_300lv_ctic_plt_whole2<-t2_300lv_ctic_plt_all2 +labs(x = "Data density", y = "Success rate") + theme_bw() + 
    theme(axis.text = element_text(size=30), axis.title = element_text(size=35), legend.text = element_text(size=30), legend.title = element_text(size=35),
          panel.border = element_rect(size=2)) +
    scale_x_continuous(breaks = seq(300, 350, by=10), labels = sapply(seq(30, 35, by=1), function(i){bquote(.(i)/pi^2)}) )
    
}

#補間手法の実験結果ggplot
t2_300lv_inted_plt_ave2<-t2_300lv_ctic_plt_all2 + geom_line(data = t2_300lv_inted_smz, aes(x = n_points, y = dim2rate_mean), color = "royalblue1")
t2_300lv_inted_plt_sd2<-t2_300lv_inted_plt_ave2 + geom_ribbon(data = t2_300lv_inted_smz, aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), fill = "royalblue1", alpha = 0.4)
t2_300lv_inted_plt_all2<-t2_300lv_inted_plt_sd2 + geom_point(data = t2_300lv_inted_rate, aes(x = n_points, y = dim2rate, color = "proposed1"), size = 3)

#exp距離変化手法のggplot
t2_300lv_wvr_plt_ave2<-t2_300lv_inted_plt_all2 + geom_line(data = wvr_rates_smz, aes(x = n_points, y = dim2rate_mean), color = "red")
t2_300lv_wvr_plt_sd2<-t2_300lv_wvr_plt_ave2 + geom_ribbon(data = wvr_rates_smz, aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), fill = "red", alpha = 0.4)
t2_300lv_wvr_plt_all2<-t2_300lv_wvr_plt_sd2 + geom_point(data = wvr_rates_tbl, aes(x = n_points, y = dim2rate, colour = "proposed2"), size = 3)

#ggplotによる300~350点2次元トーラスの成功率グラフ
#横軸をデータ点数に
#スライド用の白抜き背景
{t2_300lv_ctic_plt_whole3<-t2_300lv_wvr_plt_all2 +labs(x = "The number of points", y = "Success rate") + theme_bw() + 
    theme(axis.text = element_text(size=30), axis.title = element_text(size=35), legend.text = element_text(size=30), legend.title = element_text(size=35),
          panel.border = element_rect(size=2)) 
}
t2_300lv_ctic_plt_whole3
ggsave("./pics/success_T2H2_slide.pdf", plot = t2_300lv_ctic_plt_whole3, height = 8.5, width = 14, units = "in")


#---------------------------------
#楕円体の2次ベッチ数推定成功率グラフ

#ggplotによるCTIC2019手法のプロット
ellip_plt_ave2<-ggplot(data = ellip_rates_smz, mapping = aes(x = n_points, y = dim2rate_mean)) + geom_line()
ellip_plt_sd2<-ellip_plt_ave2 + geom_ribbon(aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), alpha = 0.4)
{ellip_plt_all2<-ellip_plt_sd2 + geom_point(data = ellip_rates_tbl, aes(x = n_points, y = dim2rate, color = "conventional"), size = 3) +
    ylim(0, 1) + 
    scale_color_manual(breaks = c("conventional", "proposed1", "proposed2"), values = c("black","royalblue1", "red"), guide = "legend", name = "method", 
                       labels = c("conventional", "proposed 1", "proposed 2"))
}

#補間手法の実験結果ggplot
ellip_inted_plt_ave2<-ellip_plt_all2 + geom_line(data = ellip_inted_rates_smz, aes(x = n_points, y = dim2rate_mean), color = "royalblue1")
ellip_inted_plt_sd2<-ellip_inted_plt_ave2 + geom_ribbon(data = ellip_inted_rates_smz, aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), fill = "royalblue1", alpha = 0.4)
ellip_inted_plt_all2<-ellip_inted_plt_sd2 + geom_point(data = ellip_inted_rates_tbl, aes(x = n_points, y = dim2rate, color = "proposed1"), size = 3)


#exp距離変化手法のggplot
ellip_wvr_plt_ave2<-ellip_inted_plt_all2 + geom_line(data = ellip_wvr_rates_smz, aes(x = n_points, y = dim2rate_mean), color = "red")
ellip_wvr_plt_sd2<-ellip_wvr_plt_ave2 + geom_ribbon(data = ellip_wvr_rates_smz, aes(ymin = dim2rate_mean - dim2rate_sd, ymax = dim2rate_mean + dim2rate_sd), fill = "red", alpha = 0.4)
ellip_wvr_plt_all2<-ellip_wvr_plt_sd2 + geom_point(data = ellip_wvr_rates_tbl, aes(x = n_points, y = dim2rate, colour = "proposed2"), size = 3)

#ggplotによる150~200点楕円体の成功率グラフ
#横軸をデータ点数に
#スライド用の白抜き背景
{
  ellip_whole_plt2<-ellip_wvr_plt_all2 + labs(x = "The number of points", y = "Success rate") + theme_bw() + 
    theme(axis.text = element_text(size=30), axis.title = element_text(size=35), 
          legend.text = element_text(size=30), legend.title = element_text(size=35), 
          panel.border = element_rect(size=2))
}
ellip_whole_plt2
ggsave("./pics/ellip_success_plt_slide.pdf", plot = ellip_whole_plt2, height = 8.5, width = 14, units = "in")

#----------------------------------------
#3次元トーラスの2次ベッチ数推定成功率グラフ-----

#CTIC2019手法ggplot
t3_suc_plt2<-ggplot(data = t3_H2rate, aes(x = n_points)) + geom_point(aes(y = dim2rate, color = "conventional"), size=5) + geom_line(aes(y = dim2rate), size = 2)
t3_suc_plt_arng2<-t3_suc_plt2 + ylim(0, 1) + scale_color_manual(breaks = c("conventional", "proposed2A", "proposed2B"), values = c("black","darkorange1", "maroon1"), guide = "legend", name = "method", 
                                                              labels = c("conventional", expression(paste(eta==6.5, ~~epsilon==0.5)), expression(paste(eta==4.0, ~~epsilon==0.5))))
#exp距離変化ggplot。eta=6.5, l_rate=0.5
t3_wvr_plt2<-t3_suc_plt_arng2 + geom_point(data = t3_wvr_H2rateA, aes(y = dim2rate, color = "proposed2A"), size=5) + geom_line(data = t3_wvr_H2rateA, aes(y = dim2rate, color = "proposed2A"), size = 2)

#exp距離変化ggplot。eta=4.0, l_rate=0.5
t3_wvr_pltB2<-t3_wvr_plt2 + geom_point(data = t3_wvr_H2rateB, aes(y = dim2rate, color = "proposed2B"), size=5) + geom_line(data = t3_wvr_H2rateB, aes(y = dim2rate, color = "proposed2B"), size = 2)
{t3_plt2<-t3_wvr_pltB2 + labs(x = "The number of points", y = "Success rate") + theme_bw() + 
    theme(axis.text = element_text(size=30), axis.title = element_text(size=35), 
          legend.text = element_text(size=30), legend.title = element_text(size=35), 
          panel.border = element_rect(size=2))
}
t3_plt2
ggsave("./pics/successT3H2_plt_slide.pdf", plot = t3_plt2, height = 8.5, width = 14, units = "in")

#----------------------------------------
#3次元トーラスの3次ベッチ数推定成功率グラフ-----

#CTIC2019手法ggplot
t3_H3_plt2<-ggplot(data = t3_H3rate, aes(x = n_points)) + geom_point(aes(y = dim3rate, color = "conventional"), size=5) + geom_line(aes(y = dim3rate), size = 2)
t3_H3_plt_arng2<-t3_H3_plt2 + ylim(0, 1) + scale_color_manual(breaks = c("conventional", "proposed2A", "proposed2B"), values = c("black","darkorange1", "maroon1"), guide = "legend", name = "method", 
                                                            labels = c("conventional", expression(paste(eta==6.5, ~~epsilon==0.5)), expression(paste(eta==4.0, ~~epsilon==0.5))))

#exp距離変化ggplot。eta=6.5, l_rate=0.5
t3_wvrH3_plt2<-t3_H3_plt_arng2 + geom_point(data = t3_wvr_H3rateA, aes(y = dim3rate, color = "proposed2A"), size=5) + geom_line(data = t3_wvr_H3rateA, aes(y = dim3rate, color = "proposed2A"), size = 2)

#exp距離変化ggplot。eta=4.0, l_rate=0.5
t3_wvrH3_pltB2<-t3_wvrH3_plt2 + geom_point(data = t3_wvr_H3rateB, aes(y = dim3rate, color = "proposed2B"), size=5) + geom_line(data = t3_wvr_H3rateB, aes(y = dim3rate, color = "proposed2B"), size = 2)
{t3_H3_plt2<-t3_wvrH3_pltB2 + labs(x = "The number of points", y = "Success rate") + theme_bw() +
    theme(axis.text = element_text(size=30), axis.title = element_text(size=35), 
          legend.text = element_text(size=30), legend.title = element_text(size=35), 
          panel.border = element_rect(size=2))
}
t3_H3_plt2
ggsave("./pics/successT3H3_plt_slide.pdf", plot = t3_H3_plt2)                            

#----------------------
#距離変化グラフ----

domain<-seq(0, 30, length=100)
{
modified_dist_plt<-ggplot()+geom_line(aes(x = c(0, max(domain)), y = c(0, max(domain))), size = 7, alpha = 0.5, linetype = "dotted") +
    geom_line(mapping = aes(domain, domain*( 1-exp( -(domain/12)^2) ), colour = brewer.pal(3, "Set1")[1]), size = 10, alpha = 0.9) +
    theme_bw()+theme(axis.text = element_text(size=35), axis.title = element_text(size=45), panel.border = element_rect(size=2))+guides(colour=FALSE) +
    labs(x = "Original distance", y = "Modified distance")
}

modified_dist_plt
ggsave("./pics/modified_dist_plt4.png", height = 20, width = 20, plot = modified_dist_plt, units = "cm")


domain2<-seq(0, 7, length=100)
{
  modified_dist_plt2<-ggplot()+geom_line(aes(x = c(0, max(domain2)), y = c(0, max(domain2))), size = 5, alpha = 0.5, linetype = "dotted") +
    geom_line(mapping = aes(domain2, domain2*( 1-exp( -(domain2/3.0)^2) ), colour = brewer.pal(3, "Set1")[1]), size = 10, alpha = 0.9) +
    theme_bw()+theme(axis.text = element_text(size=30), axis.title = element_text(size=40), panel.border = element_rect(size=2))+guides(colour=FALSE) +
    labs(x = "Original distance", y = "Modified distance")
}

modified_dist_plt2
ggsave("./pics/modified_dist_plt_2B.png", height = 20, width = 20, plot = modified_dist_plt2, units = "cm")


plot(domain2, domain2*( 1-exp( -(domain2/3.0)^2)) )
     