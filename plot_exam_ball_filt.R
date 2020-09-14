#ボール化した場合のフィルトレーションをプロットして確認する
a0<-c(0, -0.5)
a1<-c(0.5*sqrt(3)/2, 0.25)
a2<-c(0, 0.5)
a3<-c(-0.5*sqrt(3)/2, 0.25)
plot(rbind(a0, a1, a2, a3), xlim = c(-1, 1), ylim = c(-1, 1), pch=16, cex=1.5)

#MHPによって距離を1-exp(r_ij^2)で更新した場合の点をプロットする
#その場合の点をつないだポリゴンを表示
r_01<-sum((a1 - a0)^2) 
r_02<-sum((a2 - a0)^2) 
r_03<-sum((a3 - a0)^2)
r_12<-sum((a2 - a1)^2) %>% sqrt()
r_23<-sum((a3 - a2)^2) %>% sqrt()
r_13<-sum((a3 - a1)^2) %>% sqrt()


d_01<-1-exp(-r_01)
d_02<-1-exp(-r_02)
d_03<-1-exp(-r_03)

a01<-((sqrt(r_01) - d_01)/sqrt(r_01))* (a1 - a0) + a0
points(x = a01[1], y = a01[2], col=3, pch=16)
a02<-((sqrt(r_02) - d_02)/sqrt(r_02))* (a2 - a0) + a0
points(x = a02[1], y = a02[2], col=3, pch=16)
a03<-((sqrt(r_03) - d_03)/sqrt(r_03))* (a3 - a0) + a0
points(x = a03[1], y = a03[2], col=3, pch=16)

polygon(c(a0[1], a01[1], a02[1], a03[1]), c(a0[2], a01[2], a02[2], a03[2]), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
lines(c(a1[1], a2[1], a3[1], a0[1], a1[1]), c(a1[2], a2[2], a3[2], a0[2], a1[2]))
#穴に相当する円周の描画
theta <- seq(-pi, pi, length=100)
r<-0.5
polygon(r*cos(theta), r*sin(theta))

R<-2*sqrt(3)
R<-8*sqrt(3)/5
R<-8/2.5
R<-3.2228
R<-3.25
R<-2*sqrt(3)-1
R<-3.0551
R<-4
#a0を中心に半径2の円とみなした時のbirthとdeath
R<-(4*sqrt(3)-v)/2
#v=2
R<-1/11*(6*sqrt(55)-11)
#v=1
R<-1/94*(12*sqrt(987)-47)
#v=3
R<-4*sin(pi/6)
R<-1/26*(4*sqrt(715)-39)

#a0の固定円の半径
v<-3

polygon(a0[1]+(R+v)*cos(theta), a0[2]+(R+v)*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
#a0固定円なし
polygon(a0[1]+R*cos(theta), a0[2]+R*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
#polygon(a0[1]+1.5*R*cos(theta), a0[2]+1.5*R*sin(theta))
polygon(a1[1]+R*cos(theta), a1[2]+R*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
polygon(a2[1]+R*cos(theta), a2[2]+R*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
polygon(a3[1]+R*cos(theta), a3[2]+R*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))

#a0の固定円の描画
polygon(a0[1]+v*cos(theta), a0[2]+v*sin(theta), col="pink")
points(a0[1], a0[2], pch=16, cex=1.5)

polygon(1+R*cos(theta), (sqrt(3)-4)+R*sin(theta))
polygon(-1+R*cos(theta), (sqrt(3)-4)+R*sin(theta))
polygon(R*cos(theta), -2+R*sin(theta))
points(rbind(c(1, sqrt(3)-4),c(-1, sqrt(3)-4), c(0, -2)))
lines(c(0, 2*sqrt(3)), c(-4, 2))
lines(c(a1[1], a2[1], a3[1], a0[1]), c(a1[2], a2[2], a3[2], a0[2]))

#----------------------------------------
#点が満遍なく分布している場合

#点のプロット
for (i in 1:(360/30)) {
  
  points(4*cos((i-1)*pi/6), 4*sin((i-1)*pi/6), pch=16, cex=1.5)
  
}

#円のプロット
x<-((2*sqrt(3)-4)^2 + 2^2)/2
x<-4*sin(pi/12)
x<-4
for (i in 1:(360/30)) {
  
  polygon(4*cos((i-1)*pi/6)+x*cos(theta), 4*sin((i-1)*pi/6)+x*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
  
}

#---------------------------------
#密度が低いが均等に分布している場合
plot(rbind(a0, a1, a2, a3), xlim = c(-5, 5), ylim = c(-5, 5), type = "n")

#円のプロット
x<-4*sin(pi/4)
x<-4
for (i in 1:(360/90)) {
  
  polygon(4*cos((i-1)*pi/2)+x*cos(theta), 4*sin((i-1)*pi/2)+x*sin(theta))
  
}

#点のプロット
for (i in 1:(360/90)) {
  
  points(4*cos((i-1)*pi/2), 4*sin((i-1)*pi/2), pch=16)
  
}

#--------------------------------------
#擬似的なPDを出力
#密度が高い場合
plot(4*sin(pi/12), 4, pch=2, cex=2, xlim = c(0, 6), ylim = c(0, 6), col=2, lwd=2)
abline(0, 1)

#密度が低い場合
plot(2*sqrt(3), 4, pch=2, cex=2, xlim = c(0, 6), ylim = c(0, 6), col=2, lwd=2)
abline(0, 1)

plot(2*sqrt(3), 4, pch=2, cex=2, xlim = c(0, 6), ylim = c(0, 6), col=2, lwd=2)

#密度が低い場合
#固定円有り
plot(4*sin(pi/6), 1/26*(4*sqrt(715)-39), pch=2, cex=2, xlim = c(0, 6), ylim = c(0, 6), col=2, lwd=2)
abline(0, 1)

#----------------------------------------
#点が満遍なく分布している場合
#小さい円

#穴に相当する円
polygon(2*cos(theta), 2*sin(theta))

#点のプロット
for (i in 1:(360/30)) {
  
  points(2*cos((i-1)*pi/6), 2*sin((i-1)*pi/6), pch=16, cex=1.5)
  
}

#円のプロット
x<-((2*sqrt(3)-4)^2 + 2^2)/2
x<-2*sin(pi/12)
x<-2
for (i in 1:(360/30)) {
  
  polygon(2*cos((i-1)*pi/6)+x*cos(theta), 2*sin((i-1)*pi/6)+x*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
  
}

#擬似的なPDを出力
#小さい穴の場合
plot(2*sin(pi/12), 2, pch=2, cex=2, xlim = c(0, 6), ylim = c(0, 6), col=2, lwd=2)
abline(0, 1)

#-----------------------------------
#連結成分を表すためのプロット

b1<-c(1.5+sqrt(3), 0)
b2<-c(1.5, -1)
b3<-c(1.5, 1)

#穴の生成
r2<-1
#穴の消滅
r2<-sqrt(3)*2/3
#連結成分が1に
#r2<-1.5
r2<-sqrt(13)/2

r2<-3

plot(rbind(b1, b2, b3, b4, b5, b6), xlim = c(-5, 5), ylim = c(-5, 5), pch=16, cex=2)

#b1円
polygon(1.5+sqrt(3)+r2*cos(theta), r2*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
#b2円
polygon(1.5+r2*cos(theta), -1+r2*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
#b3円
polygon(1.5+r2*cos(theta), 1+r2*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))


b4<-c(-1.5-sqrt(3), 0)
b5<-c(-1.5, -1)
b6<-c(-1.5, 1)

#b4円
polygon(-1.5-sqrt(3)+r2*cos(theta), r2*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
#b5円
polygon(-1.5+r2*cos(theta), -1+r2*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
#b6円
polygon(-1.5+r2*cos(theta), 1+r2*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))

#単体プロット
#穴の消滅時
polygon(rbind(b1, b2, b3), density = 10)
polygon(rbind(b4, b5, b6), density = 10)

#連結成分が1になるとき
polygon(rbind(b5, b2, b3), density = 10)
polygon(rbind(b6, b2, b3), density = 10)
polygon(rbind(b5, b6, b3), density = 10)

lines(rbind(b1, b2), lwd=2)
lines(rbind(b1, b3), lwd=2)
lines(rbind(b3, b2), lwd=2)
lines(rbind(b3, b5), lwd=2)
lines(rbind(b5, b2), lwd=2)
lines(rbind(b6, b2), lwd=2)
lines(rbind(b3, b6), lwd=2)
lines(rbind(b5, b6), lwd=2)
lines(rbind(b4, b5), lwd=2)
lines(rbind(b4, b6), lwd=2)

#擬似的なPDを出力
#穴の消滅時の連結成分
plot(0, sqrt(3)*2/3, pch=16, cex=2, xlim = c(0, 4), ylim = c(0, 4))
abline(0, 1)
#擬似的なPDを出力
#連結成分が1になるとき
plot(rbind(c(0, sqrt(3)*2/3),c(0, sqrt(13)/2)), pch=16, cex=2, xlim = c(0, 4), ylim = c(0, 4))
abline(0, 1)

#フィルトレーション終了
plot(rbind(c(0, sqrt(3)*2/3),c(0, sqrt(13)/2), c(0, 3)), pch=16, cex=2, xlim = c(0, 4), ylim = c(0, 4))
abline(0, 1)
