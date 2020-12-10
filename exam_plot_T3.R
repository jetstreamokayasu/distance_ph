#3次元トーラスの成功率グラフ
#実験結果のプロット
#プロット領域準備
oldpar <- par(no.readonly = TRUE)
par(mgp=c(2.5, 1, 0))
plot(500, t3rs500_rate, pch=16, cex.axis=1.6, xlab="Data Density", ylab="Success Rates", cex.lab=1.6, xlim=c(450, 500), ylim=c(0.0, 1.0), xaxt="n", type="n")
axis(side=1, at=seq(450, 500, by=10), labels=c(paste0(seq(450, 500, by=10), "/(64*pi^3)")), cex.axis=1.1)

t3rs500_rate<-cycle_number(t3orus4_list3_1to30_normal_aggrs, 3)[2] /30
t3rs490_rate<-cycle_number(t3orus490_list1_1to30_normal_aggrs, 3)[2] /30
t3rs480_rate<-cycle_number(t3orus480_list1_1to30_normal_aggrs, 3)[2] /30
t3rs470_rate<-cycle_number(t3orus470_list1_1to30_normal_aggrs, 3)[2] /30
t3rs460_rate<-cycle_number(t3orus460_list1_1to30_normal_aggrs, 3)[2] /30
t3rs450_rate<-cycle_number(t3orus450_list1_1to30_normal_aggrs, 3)[2] /30


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
t3rs500_rate_H2<-cycle_number(t3orus4_list3_1to30_normal_aggrs, 2)[4] /30
t3rs490_rate_H2<-cycle_number(t3orus490_list1_1to30_normal_aggrs, 2)[4] /30
t3rs480_rate_H2<-cycle_number(t3orus480_list1_1to30_normal_aggrs, 2)[4] /30
t3rs470_rate_H2<-cycle_number(t3orus470_list1_1to30_normal_aggrs, 2)[4] /30
t3rs460_rate_H2<-cycle_number(t3orus460_list1_1to30_normal_aggrs, 2)[4] /30
t3rs450_rate_H2<-cycle_number(t3orus450_list1_1to30_normal_aggrs, 2)[4] /30

points(500, t3rs500_rate_H2, pch=16, col=black)
points(490, t3rs490_rate_H2, pch=16, col=black)
points(480, t3rs480_rate_H2, pch=16, col=black)
points(470, t3rs470_rate_H2, pch=16, col=black)
points(460, t3rs460_rate_H2, pch=16, col=black)
points(450, t3rs450_rate_H2, pch=16, col=black)

lines(seq(450, 500, by=10), c(t3rs450_rate_H2, t3rs460_rate_H2, t3rs470_rate_H2, t3rs480_rate_H2, t3rs490_rate_H2, t3rs500_rate_H2), col=black)


#-------------------------------
#espを掛ける距離行列変化手法----
t3rs500_wvr_rate<-cycle_number(t3orus4_list3_1to30aggrs3, 3)[2] /30
t3rs490_wvr_rate<-cycle_number(t3orus490_list1_1to30_wvr_aggrs1, 3)[2] / 30
t3rs480_wvr_rate<-cycle_number(t3orus480_list1_1to30_wvr_aggrs1, 3)[2] / 30
t3rs470_wvr_rate<-cycle_number(t3orus470_list1_1to30_wvr_aggrs1, 3)[2] / 30
t3rs460_wvr_rate<-cycle_number(t3orus460_list1_1to30_wvr_aggrs1, 3)[2] / 30
t3rs450_wvr_rate<-cycle_number(t3orus450_list1_1to30_wvr_aggrs1, 3)[2] / 30

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
t3rs500_wvr_rate_H2<-cycle_number(t3orus4_list3_1to30aggrs3, 2)[4] /30
t3rs490_wvr_rate_H2<-cycle_number(t3orus490_list1_1to30_wvr_aggrs1, 2)[4] / 30
t3rs480_wvr_rate_H2<-cycle_number(t3orus480_list1_1to30_wvr_aggrs1, 2)[4] / 30
t3rs470_wvr_rate_H2<-cycle_number(t3orus470_list1_1to30_wvr_aggrs1, 2)[4] / 30
t3rs460_wvr_rate_H2<-cycle_number(t3orus460_list1_1to30_wvr_aggrs1, 2)[4] / 30
t3rs450_wvr_rate_H2<-cycle_number(t3orus450_list1_1to30_wvr_aggrs1, 2)[4] / 30


points(500, t3rs500_wvr_rate_H2, pch=16, col=red)
points(490, t3rs490_wvr_rate_H2, pch=16, col=red)
points(480, t3rs480_wvr_rate_H2, pch=16, col=red)
points(470, t3rs470_wvr_rate_H2, pch=16, col=red)
points(460, t3rs460_wvr_rate_H2, pch=16, col=red)
points(450, t3rs450_wvr_rate_H2, pch=16, col=red)

lines(seq(450, 500, by=10), c(t3rs450_wvr_rate_H2, t3rs460_wvr_rate_H2, t3rs470_wvr_rate_H2, t3rs480_wvr_rate_H2, t3rs490_wvr_rate_H2, t3rs500_wvr_rate_H2), col=red)
