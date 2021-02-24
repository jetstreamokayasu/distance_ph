#密度高低差によるパーシステンスの差、距離変化によるタイムライン用の図を作成する

#密度が低い場合の点群
d_t<-cbind(c(1.2, 0, -1.1, 0, 0.5), c(0, 0.9, 0, -1.3, 0.4))
pdf(file = "./sample_data3.pdf")
pdf(file = "./pics/sample_data3_death.pdf")

plot(d_t_inst$data, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), pch = 16, xlab = "", ylab = "", cex=1.5)
points(d_t_inst$data[c(1,3),], col = 2, pch = 16, cex = 1.5)
text(c(1.2, 0, -1.1, 0, 0.5), c(0, 0.9, 0, -1.3, 0.4), labels = 1:5, pos=3, cex = 1.5)

dev.off()

plot_circle(x = 0, y = -0.2, r = 1)

#----------------------------------------
#点が満遍なく分布している場合----

#点の準備
#単位演習場に30度ずつ置き、ガウスノイズを加える
#0, pi/2, pi, (3/2)piの分はd_tを使う
set.seed(42)
d_x<-cos((0:11)*pi/6) + rnorm(n = 12, sd = 0.05)
d_y<-sin((0:11)*pi/6) + rnorm(n = 12, sd = 0.05)

d_t2<-cbind(d_x, d_y)
d_t2[1, ]<-d_t[1, ]
d_t2[4, ]<-d_t[2, ]
d_t2[7, ]<-d_t[3, ]
d_t2[10, ]<-d_t[4, ]
d_t2<-rbind(d_t2, d_t[5, ])

#密度が高い場合の点群のプロット
points(d_t2, pch = 16, cex = 1.5)

d_t2_inst<-TDAdataset$new(d_t2)
d_t2_inst$calc_pd(maxdim = 1, maxscale = 2)

#円のプロット
theta <- seq(-pi, pi, length=100)
r<-d_t2_inst$get_pd()[13, 2]/2 #生成時の円の半径。
r<-d_t2_inst$get_pd()[13, 3]/2 #消滅時の円の半径。

for (i in 1:nrow(d_t2)) {
  
  polygon(d_t2[i,1]+r*cos(theta), d_t2[i,2]+r*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
  
}

#VR複体における消滅時ではなく、アルファ複体における消滅時を描画する
#d_t2[8, ], d_t2[9, ], d_t[5,]を中心とした3円の交点を求める

#----------------------------
#補間した場合のフィルトレーションを示す-------------

#密度の低いデータ
a0<-c(0, -0.5)*8
a1<-c(0.5*sqrt(3)/2, 0.25)*8
a2<-c(0, 0.5)*8
a3<-c(-0.5*sqrt(3)/2, 0.25)*8
plot(rbind(a0, a1, a2, a3), xlim = c(-5, 5), ylim = c(-5, 5), pch=16, ylab = "", xlab = "", cex = 5)
plot_circle(x = 0, y = 0, r = 4)

pdf(file = "./pics/interpolated_data.pdf")
pdf(file = "./pics/interpolated_birth.pdf")
pdf(file = "./pics/interpolated_death.pdf")

pdf(file = "./pics/low_density_data.pdf")
pdf(file = "./pics/low_density_birth.pdf")
pdf(file = "./pics/low_density_death.pdf")
dev.off()

a4<-(a1+a0)/2
a5<-(a3+a0)/2
points(a1_set$data, pch = 16, cex = 5)
points(rbind(a4, a5), pch = 16, col = 4, cex = 5)

#a1_set<-TDAdataset$new(rbind(a0, a1, a2, a3, a4, a5))
#a1_set$calc_pd(maxdim = 1, maxscale = 8)

#円のプロット
r<-a1_set$get_pd()[6, 2]/2 #生成半径
r<-a1_set$get_pd()[6, 3]/2 #消滅半径
r<-14/5 #アルファ複体における消滅半径?

r<-2*sqrt(3)#密度が低い場合(補間点無し)の生成半径
r<-4#密度が低い場合(補間点無し)の消滅半径

for (i in 1:a1_set$n_points) {
  
  polygon(a1_set$data[i,1]+r*cos(theta), a1_set$data[i,2]+r*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
  
}

for (i in 1:(a1_set$n_points-2)) {
  
  polygon(a1_set$data[i,1]+r*cos(theta), a1_set$data[i,2]+r*sin(theta), col=rgb(255/255, 153/255, 153/255, alpha = 0.3))
  
}

points(a1_set$data[1:4, ], pch = 16, cex = 5)

#-----------------------
#3つの円が交わるときの半径を求める------


#p1, p2, p3を各円の中心としたときの円の方程式を求める
#xyr[1]=交点のx座標(未知変数), xyr[2]=交点のy座標(未知変数), xyr[3]=半径(未知変数)
triple_circle_eq<-function(xyr, p1, p2, p3){
  
  x<-xyr[1]
  y<-xyr[2]
  r<-xyr[2]
  
  #方程式1
  f<-(x - p1[1])^2 + (y - p1[2])^2 - r^2
  
  #方程式2
  g<-(x - p2[1])^2 + (y - p2[2])^2 - r^2
  
  #方程式2
  h<-(x - p3[1])^2 + (y - p3[2])^2 - r^2
  
  return(c(f, g, h))
  
}

#p1, p2, p3を入力して解を求める連立方程式を立式
triple_circle_specify<-function(x){triple_circle_eq(xyr = x, a2, a4, a5)}

#解を求める
library(nleqslv)

ans<-nleqslv(x = c(1, 1, 1), fn = triple_circle_specify, control = list(allowSingular = T))

#--------------------------------
#AppendixのPH説明用フィルトレーション図作成
#点を反時計回り順で配置
y1<-c(1, 0)
y2<-c(0, 1)
y3<-c(-1/sqrt(3), 0)
y4<-c(0, -1)

plot(rbind(y1, y2, y3, y4), pch=16, cex=4, ann = F, axes = F, ylim = c(-1.5, 1.5), xlim = c(-1, 1.5))
#text(rbind(y1, y2, y3, y4), labels = c(expression(v[3]), expression(v[4]), expression(v[0]), expression(v[2])), pos = 1, cex = 3)

draw_line(y2, y3, lwd = 4)
draw_line(y4, y3, lwd = 4)
draw_line(y2, y1, lwd = 4)
draw_line(y1, y4, lwd = 4)
draw_line(y2, y4, lwd = 4)

polygon(rbind(y2, y3, y4), density = 10)

pdf("./pics/appen_filt0.pdf", height = 8, width = 7)
pdf("./pics/appen_y2y3_y3y4.pdf", height = 8, width = 7)
pdf("./pics/appen_y1y2_y1y4.pdf", height = 8, width = 7)
pdf("./pics/appen_y2y4.pdf", height = 8, width = 7)
pdf("./pics/appen_y2y3y4.pdf", height = 8, width = 7)
dev.off()
