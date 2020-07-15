#実験結果のプロット
#プロット領域準備
oldpar <- par(no.readonly = TRUE)
par(mgp=c(2.5, 1, 0))
plot(rep(300, 5), torus300_colle_rates[,1], pch=16, cex.axis=1.6, xlab="Data Density", ylab="Success Rates", cex.lab=1.6, xlim=c(300, 350), ylim=c(0.0, 1.0), xaxt="n", type="n")
axis(side=1, at=seq(300, 350, by=10), labels=c(paste0(seq(30, 35), "/(pi^2)")), cex.axis=1.1)


#---------------------------------------------------
#CTIC2019手法の推定成功率まとめ
torus300_colle_rates<-aggr_success_rates(torus300_colle_aggrs, c(2,1)) %>% do.call(rbind, .)
torus310_colle_rates<-aggr_success_rates(torus310_colle_aggrs, c(2,1)) %>% do.call(rbind, .)
torus320_colle_rates<-aggr_success_rates(torus320_colset_aggrs, c(2,1)) %>% do.call(rbind, .)
torus330_colle_rates<-aggr_success_rates(torus330_colset_aggrs, c(2,1)) %>% do.call(rbind, .)
torus340_colle_rates<-aggr_success_rates(torus340_colset_aggrs, c(2,1)) %>% do.call(rbind, .)
torus350_colle_rates<-aggr_success_rates(torus350_colle_aggrs, c(2,1)) %>% do.call(rbind, .)

sucs_rates<-list("300"=torus300_colle_rates,
                 "310"=torus310_colle_rates,
                 "320"=torus320_colle_rates,
                 "330"=torus330_colle_rates,
                 "340"=torus340_colle_rates,
                 "350"=torus350_colle_rates)

#CTIC2019手法
#2次ベッチ数推定成功率プロット
points(rep(300, 5), torus300_colle_rates[,2], pch=16, col='#7F7F7F')
points(rep(310, 5), torus310_colle_rates[,2], pch=16, col='#7F7F7F')
points(rep(320, 5), torus320_colle_rates[,2], pch=16, col='#7F7F7F')
points(rep(330, 5), torus330_colle_rates[,2], pch=16, col='#7F7F7F')
points(rep(340, 5), torus340_colle_rates[,2], pch=16, col='#7F7F7F')
points(rep(350, 5), torus350_colle_rates[,2], pch=16, col='#7F7F7F')

sucs_mean_dim2<-sapply(sucs_rates, function(rate)mean(unlist(rate[,2])))
lines(seq(300, 350, by=10), sucs_mean_dim2, col='#7F7F7F')

sucs_sd_dim2<-sapply(sucs_rates, function(rate)sd(unlist(rate[,2])))
lines(seq(300, 350, by=10), sucs_mean_dim2-sucs_sd_dim2, lty="dashed", col='#7F7F7F')
lines(seq(300, 350, by=10), sucs_mean_dim2+sucs_sd_dim2, lty="dashed", col='#7F7F7F')

#CTIC2019手法
#1次ベッチ数推定成功率プロット
points(rep(300, 5), torus300_colle_rates[,1], pch=16, col='#7F7F7F')
points(rep(310, 5), torus310_colle_rates[,1], pch=16, col='#7F7F7F')
points(rep(320, 5), torus320_colle_rates[,1], pch=16, col='#7F7F7F')
points(rep(330, 5), torus330_colle_rates[,1], pch=16, col='#7F7F7F')
points(rep(340, 5), torus340_colle_rates[,1], pch=16, col='#7F7F7F')
points(rep(350, 5), torus350_colle_rates[,1], pch=16, col='#7F7F7F')

sucs_mean_dim1<-sapply(sucs_rates, function(rate)mean(unlist(rate[,1])))
lines(seq(300, 350, by=10), sucs_mean_dim1, col='#7F7F7F')

sucs_sd_dim1<-sapply(sucs_rates, function(rate)sd(unlist(rate[,1])))
lines(seq(300, 350, by=10), sucs_mean_dim1-sucs_sd_dim1, lty="dashed", col='#7F7F7F')
lines(seq(300, 350, by=10), sucs_mean_dim1+sucs_sd_dim1, lty="dashed", col='#7F7F7F')

#-------------------------------------------------------------
#補間手法推定成功率まとめ
torus350_incolle_rate<-aggr_success_rates(torus350_incolle_aggrs, c(2,1)) %>% do.call(rbind, .)
torus340_incolle_rate<-aggr_success_rates(append(torus340_incolle13_aggrs, torus340_incolle45_aggrs), c(2,1)) %>% do.call(rbind, .)
torus330_incolle_rate<-aggr_success_rates(append(torus330_incolle13_aggrs, torus330_incolle45_aggrs), c(2,1)) %>% do.call(rbind, .)
torus320_incolle_rate<-aggr_success_rates(append(torus320_incolle13_aggrs, torus320_incolle45_aggrs), c(2,1)) %>% do.call(rbind, .)
torus310_incolle_rate<-aggr_success_rates(append(torus310_incolle13_aggrs, torus310_incolle45_aggrs), c(2,1)) %>% do.call(rbind, .)
torus300_incolle_rate<-aggr_success_rates(append(torus300_incolle13_aggrs2, torus300_incolle45_aggrs), c(2,1)) %>% do.call(rbind, .)

insuc_rates<-list("300"=torus300_incolle_rate,
                  "310"=torus310_incolle_rate,
                  "320"=torus320_incolle_rate,
                  "330"=torus330_incolle_rate,
                  "340"=torus340_incolle_rate,
                  "350"=torus350_incolle_rate)

#PCA原点平行移動補間後2次ベッチ数
points(rep(300, 5), torus300_incolle_rate[,2], col='#4C4CFF', pch=16)
points(rep(310, 5), torus310_incolle_rate[,2], col='#4C4CFF', pch=16)
points(rep(320, 5), torus320_incolle_rate[,2], col='#4C4CFF', pch=16)
points(rep(330, 5), torus330_incolle_rate[,2], col='#4C4CFF', pch=16)
points(rep(340, 5), torus340_incolle_rate[,2], col='#4C4CFF', pch=16)
points(rep(350, 5), torus350_incolle_rate[,2], col='#4C4CFF', pch=16)

insucs_mean_dim2<-sapply(insuc_rates, function(rate)mean(unlist(rate[,2])))
lines(seq(300, 350, by=10), insucs_mean_dim2, col='#4C4CFF')

insucs_sd_dim2<-sapply(insuc_rates, function(rate)sd(unlist(rate[,2])))
lines(seq(300, 350, by=10), insucs_mean_dim2-insucs_sd_dim2, lty="dashed", col='#4C4CFF')
lines(seq(300, 350, by=10), insucs_mean_dim2+insucs_sd_dim2, lty="dashed", col='#4C4CFF')


#PCA原点平行移動手法補間後1次ベッチ数
points(rep(300, 5), torus300_incolle_rate[,1], col='#4C4CFF', pch=16)
points(rep(310, 5), torus310_incolle_rate[,1], col='#4C4CFF', pch=16)
points(rep(320, 5), torus320_incolle_rate[,1], col='#4C4CFF', pch=16)
points(rep(330, 5), torus330_incolle_rate[,1], col='#4C4CFF', pch=16)
points(rep(340, 5), torus340_incolle_rate[,1], col='#4C4CFF', pch=16)
points(rep(350, 5), torus350_incolle_rate[,1], col='#4C4CFF', pch=16)

insucs_mean_dim1<-sapply(insuc_rates, function(rate)mean(unlist(rate[,1])))
lines(seq(300, 350, by=10), insucs_mean_dim1, col='#4C4CFF')

insucs_sd_dim1<-sapply(insuc_rates, function(rate)sd(unlist(rate[,1])))
lines(seq(300, 350, by=10), insucs_mean_dim1-insucs_sd_dim1, lty="dashed", col='#4C4CFF')
lines(seq(300, 350, by=10), insucs_mean_dim1+insucs_sd_dim1, lty="dashed", col='#4C4CFF')




#-------------------------------------------------
#距離行列変化手法推定成功率まとめ
torus300_colle13_dc_rate<-aggr_success_rates(torus300_colle13_dc_aggrs, c(2,1)) %>% do.call(rbind, .)
torus310_colle13_dc_rate<-aggr_success_rates(torus310_colle13_dc_aggrs, c(2,1)) %>% do.call(rbind, .)
torus320_colle13_dc_rate<-aggr_success_rates(torus320_colle13_dc_aggrs, c(2,1)) %>% do.call(rbind, .)
torus330_colle13_dc_rate<-aggr_success_rates(torus330_colle13_dc_aggrs, c(2,1)) %>% do.call(rbind, .)
torus340_colle13_dc_rate<-aggr_success_rates(torus340_colle13_dc_aggrs, c(2,1)) %>% do.call(rbind, .)
torus350_colle13_dc_rate<-aggr_success_rates(torus350_colle13_dc_aggrs, c(2,1)) %>% do.call(rbind, .)

dcsuc_rates<-list("300"=torus300_colle13_dc_rate,
                  "310"=torus310_colle13_dc_rate,
                  "320"=torus320_colle13_dc_rate,
                  "330"=torus330_colle13_dc_rate,
                  "340"=torus340_colle13_dc_rate,
                  "350"=torus350_colle13_dc_rate)

#距離行列変化手法2次ベッチ数
points(rep(300, 3), torus300_colle13_dc_rate[,2], col="red", pch=16, cex=1.5)
points(rep(310, 3), torus310_colle13_dc_rate[,2], col="red", pch=16, cex=1.5)
points(rep(320, 3), torus320_colle13_dc_rate[,2], col="red", pch=16, cex=1.5)
points(rep(330, 3), torus330_colle13_dc_rate[,2], col="red", pch=16, cex=1.5)
points(rep(340, 3), torus340_colle13_dc_rate[,2], col="red", pch=16, cex=1.5)
points(rep(350, 3), torus350_colle13_dc_rate[,2], col="red", pch=16, cex=1.5)

dcsucs_mean_dim2<-sapply(dcsuc_rates, function(rate)mean(unlist(rate[,2])))
lines(seq(300, 350, by=10), dcsucs_mean_dim2, col='red', lwd=2)

dcsucs_sd_dim2<-sapply(dcsuc_rates, function(rate)sd(unlist(rate[,2])))
lines(seq(300, 350, by=10), dcsucs_mean_dim2-dcsucs_sd_dim2, lty="dashed", col='red', lwd=2)
lines(seq(300, 350, by=10), dcsucs_mean_dim2+dcsucs_sd_dim2, lty="dashed", col='red', lwd=2)

#距離行列変化手法1次ベッチ数
points(rep(300, 3), torus300_colle13_dc_rate[,1], col="red", pch=16, cex=1.5)
points(rep(310, 3), torus310_colle13_dc_rate[,1], col="red", pch=16, cex=1.5)
points(rep(320, 3), torus320_colle13_dc_rate[,1], col="red", pch=16, cex=1.5)
points(rep(330, 3), torus330_colle13_dc_rate[,1], col="red", pch=16, cex=1.5)
points(rep(340, 3), torus340_colle13_dc_rate[,1], col="red", pch=16, cex=1.5)
points(rep(350, 3), torus350_colle13_dc_rate[,1], col="red", pch=16, cex=1.5)

dcsucs_mean_dim1<-sapply(dcsuc_rates, function(rate)mean(unlist(rate[,1])))
lines(seq(300, 350, by=10), dcsucs_mean_dim1, col='red', lwd=2)

dcsucs_sd_dim1<-sapply(dcsuc_rates, function(rate)sd(unlist(rate[,1])))
lines(seq(300, 350, by=10), dcsucs_mean_dim1-dcsucs_sd_dim1, lty="dashed", col='red', lwd=2)
lines(seq(300, 350, by=10), dcsucs_mean_dim1+dcsucs_sd_dim1, lty="dashed", col='red', lwd=2)

#------------------------------------------
#距離行列変化手法計算時間まとめ
torus300_colle13_dc_times<-sapply(torus300_colle13_dc_aggrs, function(x)x[["time"]])
