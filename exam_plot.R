#実験結果のプロット
#プロット領域準備
oldpar <- par(no.readonly = TRUE)
par(mgp=c(2.5, 1, 0))
plot(rep(300, 5), sucs300_rate[,2], pch=16, cex.axis=1.6, xlab="Data Density", ylab="Success Rates", cex.lab=1.6, xlim=c(300, 350), ylim=c(0.0, 1.0), xaxt="n", type="n")
axis(side=1, at=seq(300, 350, by=10), labels=c(paste0(seq(30, 35), "/(pi^2)")), cex.axis=1.1)

search_load("torus300_colle_aggrs", path = "~/R/ph_jikkeb/data")
old_path<-"~/R/ph_jikkeb/data"



#---------------------------------------------------
#CTIC2019手法の推定成功率まとめ-------
#NewDeepになってからの成功率の図表化---
##補間前
sucs350_rate<-do.call(rbind, aggr_success_rates(torus350_colle_aggrs, c(2,1)))
sucs340_rate<-do.call(rbind, aggr_success_rates(torus340_colset_aggrs, c(2,1)))
sucs330_rate<-do.call(rbind, aggr_success_rates(torus330_colset_aggrs, c(2,1)))
sucs320_rate<-do.call(rbind, aggr_success_rates(torus320_colset_aggrs, c(2,1)))
sucs310_rate<-do.call(rbind, aggr_success_rates(torus310_colle_aggrs, c(2,1)))
sucs300_rate<-do.call(rbind, aggr_success_rates(torus300_colle_aggrs, c(2,1)))


sucs_rates<-list("300"=sucs300_rate,
                 "310"=sucs310_rate,
                 "320"=sucs320_rate,
                 "330"=sucs330_rate,
                 "340"=sucs340_rate,
                 "350"=sucs350_rate)

sucs_rates_tidy<-sucs_rates %>% bind_cols() %>% gather(data, value)


black<-"gray22"
points(rep(300, 5), sucs300_rate[,2], pch=16)
points(rep(310, 5), sucs310_rate[,2], pch=16)
points(rep(320, 5), sucs320_rate[,2], pch=16)
points(rep(330, 5), sucs330_rate[,2], pch=16)
points(rep(340, 5), sucs340_rate[,2], pch=16)
points(rep(350, 5), sucs350_rate[,2], pch=16)

sucsrate_mean<-sapply(sucs_rates, function(rate)mean(unlist(rate[,2])))
lines(seq(300, 350, by=10), sucsrate_mean)

sucsrate_sd<-sapply(sucs_rates, function(rate)sd(unlist(rate[,2])))
lines(seq(300, 350, by=10), sucsrate_mean-sucsrate_sd, lty="dashed")
lines(seq(300, 350, by=10), sucsrate_mean+sucsrate_sd, lty="dashed")

##補間前1次ベッチ数
plot(sucs_rates_tidy, pch=16, cex.axis=1.6, xlab="Data Density", ylab="Success Rates", cex.lab=1.6, xlim=c(300, 350), ylim=c(0.0, 1.0), xaxt="n", type="n")
axis(side=1, at=seq(300, 350, by=10), labels=c(paste0(seq(30, 35), "/(pi^2)")), cex.axis=1.1)

points(rep(300, 5), sucs300_rate[,1], pch=16, col='#7F7F7F')
points(rep(310, 5), sucs310_rate[,1], pch=16)
points(rep(320, 5), sucs320_rate[,1], pch=16)
points(rep(330, 5), sucs330_rate[,1], pch=16)
points(rep(340, 5), sucs340_rate[,1], pch=16)
points(rep(350, 5), sucs350_rate[,1], pch=16)

sucsrate_mean<-sapply(sucs_rates, function(rate)mean(unlist(rate[,1])))
lines(seq(300, 350, by=10), sucsrate_mean)

sucsrate_sd<-sapply(sucs_rates, function(rate)sd(unlist(rate[,1])))
lines(seq(300, 350, by=10), sucsrate_mean-sucsrate_sd, lty="dashed")
lines(seq(300, 350, by=10), sucsrate_mean+sucsrate_sd, lty="dashed")


#補間後
torus350_incolle_rate<-aggr_success_rates(torus350_incolle_aggrs, c(2,1))
torus340_incolle13_rate<-aggr_success_rates(torus340_incolle13_aggrs, c(2,1))
torus330_incolle13_rate<-aggr_success_rates(torus330_incolle13_aggrs, c(2,1))
torus320_incolle13_rate<-aggr_success_rates(torus320_incolle13_aggrs, c(2,1))
torus310_incolle13_rate<-aggr_success_rates(torus310_incolle13_aggrs, c(2,1))
torus300_incolle13_rate<-aggr_success_rates(torus320_incolle13_aggrs, c(2,1))

torus340_incolle45_rate<-aggr_success_rates(torus340_incolle45_aggrs, c(2,1))
torus330_incolle45_rate<-aggr_success_rates(torus330_incolle45_aggrs, c(2,1))
torus320_incolle45_rate<-aggr_success_rates(torus320_incolle45_aggrs, c(2,1))
torus310_incolle45_rate<-aggr_success_rates(torus320_incolle45_aggrs, c(2,1))
torus300_incolle45_rate<-aggr_success_rates(torus320_incolle45_aggrs, c(2,1))


in350_rates<-do.call(rbind, torus350_incolle_rate)
in340_rates<-do.call(rbind, append(torus340_incolle13_rate, torus340_incolle45_rate))
in330_rates<-do.call(rbind, append(torus330_incolle13_rate, torus330_incolle45_rate))
in320_rates<-do.call(rbind, append(torus320_incolle13_rate, torus320_incolle45_rate))
in310_rates<-do.call(rbind, append(torus310_incolle13_rate, torus310_incolle45_rate))
in300_rates<-do.call(rbind, append(torus300_incolle13_rate, torus300_incolle45_rate))


in300_1_rate<-do.call(rbind, torus300_1_1_rate)

insub_rates<-list("300"=in300_rates,
                  "310"=in310_rates,
                  "320"=in320_rates,
                  "330"=in330_rates,
                  "340"=in340_rates,
                  "350"=in350_rates)

#2次ベッチ数新手法補間後
points(rep(300, 5), in300_rates[,2], col='#4C4CFF', pch=16)
points(rep(310, 5), in310_rates[,2], col='#4C4CFF', pch=16)
points(rep(320, 5), in320_rates[,2], col='#4C4CFF', pch=16)
points(rep(330, 5), in330_rates[,2], col='#4C4CFF', pch=16)
points(rep(340, 5), in340_rates[,2], col='#4C4CFF', pch=16)
points(rep(350, 5), in350_rates[,2], col='#4C4CFF', pch=16)

in_dim2_mean<-sapply(insub_rates, function(rate)mean(unlist(rate[,2])))
lines(seq(300, 350, by=10), in_dim2_mean, col=4)

in_dim2_sd<-sapply(insub_rates, function(rate)sd(unlist(rate[,2])))
lines(seq(300, 350, by=10), in_dim2_mean-in_dim2_sd, lty="dashed", col=4)
lines(seq(300, 350, by=10), in_dim2_mean+in_dim2_sd, lty="dashed", col=4)


##補間後1次ベッチ数
points(rep(300, 5), in300_rates[,1], col=4, pch=16)
points(rep(310, 5), in310_rates[,1], col=4, pch=16)
points(rep(320, 5), in320_rates[,1], col=4, pch=16)
points(rep(330, 5), in330_rates[,1], col=4, pch=16)
points(rep(340, 5), in340_rates[,1], col=4, pch=16)
points(rep(350, 5), in350_rates[,1], col=4, pch=16)

in_dim2_mean<-sapply(insub_rates, function(rate)mean(unlist(rate[,1])))
lines(seq(300, 350, by=10), in_dim2_mean, col=4)

in_dim2_sd<-sapply(insub_rates, function(rate)sd(unlist(rate[,1])))
lines(seq(300, 350, by=10), in_dim2_mean-in_dim2_sd, lty="dashed", col=4)
lines(seq(300, 350, by=10), in_dim2_mean+in_dim2_sd, lty="dashed", col=4)

#-------------------------------------------------
#球体化距離行列変化手法推定成功率まとめ------
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
points(rep(300, 3), torus300_colle13_dc_rate[,2], col=red, pch=16, cex=1.5)
points(rep(310, 3), torus310_colle13_dc_rate[,2], col=red, pch=16, cex=1.5)
points(rep(320, 3), torus320_colle13_dc_rate[,2], col=red, pch=16, cex=1.5)
points(rep(330, 3), torus330_colle13_dc_rate[,2], col=red, pch=16, cex=1.5)
points(rep(340, 3), torus340_colle13_dc_rate[,2], col=red, pch=16, cex=1.5)
points(rep(350, 3), torus350_colle13_dc_rate[,2], col=red, pch=16, cex=1.5)

dcsucs_mean_dim2<-sapply(dcsuc_rates, function(rate)mean(unlist(rate[,2])))
lines(seq(300, 350, by=10), dcsucs_mean_dim2, col='red', lwd=2)

dcsucs_sd_dim2<-sapply(dcsuc_rates, function(rate)sd(unlist(rate[,2])))
lines(seq(300, 350, by=10), dcsucs_mean_dim2-dcsucs_sd_dim2, lty="dashed", col='red', lwd=2)
lines(seq(300, 350, by=10), dcsucs_mean_dim2+dcsucs_sd_dim2, lty="dashed", col='red', lwd=2)

#距離行列変化手法1次ベッチ数
points(rep(300, 3), torus300_colle13_dc_rate[,1], col=red, pch=16, cex=1.5)
points(rep(310, 3), torus310_colle13_dc_rate[,1], col=red, pch=16, cex=1.5)
points(rep(320, 3), torus320_colle13_dc_rate[,1], col=red, pch=16, cex=1.5)
points(rep(330, 3), torus330_colle13_dc_rate[,1], col=red, pch=16, cex=1.5)
points(rep(340, 3), torus340_colle13_dc_rate[,1], col=red, pch=16, cex=1.5)
points(rep(350, 3), torus350_colle13_dc_rate[,1], col=red, pch=16, cex=1.5)

dcsucs_mean_dim1<-sapply(dcsuc_rates, function(rate)mean(unlist(rate[,1])))
lines(seq(300, 350, by=10), dcsucs_mean_dim1, col='red', lwd=2)

dcsucs_sd_dim1<-sapply(dcsuc_rates, function(rate)sd(unlist(rate[,1])))
lines(seq(300, 350, by=10), dcsucs_mean_dim1-dcsucs_sd_dim1, lty="dashed", col='red', lwd=2)
lines(seq(300, 350, by=10), dcsucs_mean_dim1+dcsucs_sd_dim1, lty="dashed", col='red', lwd=2)

#------------------------------------------
#距離行列変化手法計算時間まとめ
torus300_colle13_dc_times<-sapply(torus300_colle13_dc_aggrs, function(x)x[["time"]])

#--------------------------------------
#expを掛ける距離行列変化手法推定成功率まとめ---

torus350_wvr_rate<-aggr_success_rates(c(list(trs350_colle1_wvr_aggr), trs350_colle2to3_wvr_aggr, trs350_colle4to5_wvr_aggr), c(2,1)) %>% do.call(rbind, .)
torus340_wvr_rate<-aggr_success_rates(c(list(trs340_colle1_wvr_aggr), trs340_colle2to3_wvr_aggr, trs340_colle4to5_wvr_aggr), c(2,1)) %>% do.call(rbind, .)
torus330_wvr_rate<-aggr_success_rates(c(list(trs330_colle1_wvr_aggr), trs330_colle2to3_wvr_aggr, trs330_colle4to5_wvr_aggr), c(2,1)) %>% do.call(rbind, .)
torus320_wvr_rate<-aggr_success_rates(c(list(trs320_colle1_wvr_aggr), trs320_colle2to3_wvr_aggr, trs320_colle4to5_wvr_aggr), c(2,1)) %>% do.call(rbind, .)
torus310_wvr_rate<-aggr_success_rates(c(list(trs310_colle1_wvr_aggr), trs310_colle2to3_wvr_aggr, trs310_colle4to5_wvr_aggr), c(2,1)) %>% do.call(rbind, .)
torus300_wvr_rate<-aggr_success_rates(c(list(trs300_colle1_wvr_aggr), trs300_colle2to3_wvr_aggr, trs300_colle4to5_wvr_aggr), c(2,1)) %>% do.call(rbind, .)

wvrsuc_rates<-list("300"=torus300_wvr_rate,
                  "310"=torus310_wvr_rate,
                  "320"=torus320_wvr_rate,
                  "330"=torus330_wvr_rate,
                  "340"=torus340_wvr_rate,
                  "350"=torus350_wvr_rate)

#expを掛ける距離行列変化手法2次ベッチ数
red<-"darkorange1"
points(rep(300, 5), torus300_wvr_rate[,2], col=red, pch=16)
points(rep(310, 5), torus310_wvr_rate[,2], col=red, pch=16)
points(rep(320, 5), torus320_wvr_rate[,2], col=red, pch=16)
points(rep(330, 5), torus330_wvr_rate[,2], col=red, pch=16)
points(rep(340, 5), torus340_wvr_rate[,2], col=red, pch=16)
points(rep(350, 5), torus350_wvr_rate[,2], col=red, pch=16)

wvrsucs_mean_dim2<-sapply(wvrsuc_rates, function(rate)mean(unlist(rate[,2])))
lines(seq(300, 350, by=10), wvrsucs_mean_dim2, col=red)

wvrsucs_sd_dim2<-sapply(wvrsuc_rates, function(rate)sd(unlist(rate[,2])))
lines(seq(300, 350, by=10), wvrsucs_mean_dim2-wvrsucs_sd_dim2, lty="dashed", col=red)
lines(seq(300, 350, by=10), wvrsucs_mean_dim2+wvrsucs_sd_dim2, lty="dashed", col=red)


#expを掛ける距離行列変化手法2次ベッチ数
points(rep(300, 5), torus300_wvr_rate[,1], col=red, pch=16)
points(rep(310, 5), torus310_wvr_rate[,1], col=red, pch=16)
points(rep(320, 5), torus320_wvr_rate[,1], col=red, pch=16)
points(rep(330, 5), torus330_wvr_rate[,1], col=red, pch=16)
points(rep(340, 5), torus340_wvr_rate[,1], col=red, pch=16)
points(rep(350, 5), torus350_wvr_rate[,1], col=red, pch=16)

insucs_mean_dim1<-sapply(insuc_rates, function(rate)mean(unlist(rate[,1])))
lines(seq(300, 350, by=10), insucs_mean_dim1, col=red)

insucs_sd_dim1<-sapply(insuc_rates, function(rate)sd(unlist(rate[,1])))
lines(seq(300, 350, by=10), insucs_mean_dim1-insucs_sd_dim1, lty="dashed", col=red)
lines(seq(300, 350, by=10), insucs_mean_dim1+insucs_sd_dim1, lty="dashed", col=red)
