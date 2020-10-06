#データセットを作る関数群

#--------------------------------------
#立方体の各面に一様分布
#立方体全体でみて一様分布にはなっていない
#l=一辺の長さ、fp=一つの面上にある点の個数
cube_xunif<-function(l=1, fp=100){
  
  faces<-lapply(1:6, function(i){
    
    x<-runif(fp, 0, l)
    y<-runif(fp, 0, l)
    
    return(cbind(x, y))
    
  })
  
  bottom<-cbind(faces[[1]], rep(0, fp))
  right<-cbind(rep(l, fp), faces[[2]][, 2], l-faces[[2]][, 1])
  front<-cbind(faces[[3]][,1], rep(0, fp), faces[[3]][,2])
  behind<-cbind(faces[[4]][,1], rep(l, fp), l-faces[[4]][,2])
  left<-cbind(rep(0, fp), faces[[5]][,2], faces[[5]][,1])
  top<-cbind(faces[[6]], rep(l, fp))
  cube<-rbind(bottom, right, front, behind, left, top)
  
  return(cube)
  
}

#-------------------------------------------------
#楕円体状の一様分布(仮)
#ヤコビアンを使っている(一応)
#n=データ点数、a=x軸方向の径、b=y軸方向の径、c=z軸方向の径、

xEllip_unif<-function(n, a, b, c){
  
  p<-runif(n, -1, 1)
  theta<-acos(-p)
  phi<-runif(n, 0, 2*pi)
  
  x<-a*sin(theta)*cos(phi)
  y<-b*sin(theta)*sin(phi)
  z<-c*cos(theta)
  
  return(cbind(x, y, z))
  
}

#----------------------------------------
#半径Rの円内に一様分布する点群を作る関数
#N=データ点数、R=円盤の半径

uniDisMake<-function(N, R){
  
  theta <- runif(N, min=0, max=2*pi)
  r <- sqrt(2*runif(N, min=0, max=0.5*R^2))
  df <- data.frame(x=r*cos(theta), y=r*sin(theta))
  return(df)
  
}

#-----------------------------------------------
#3Dトーラスの角度一様分布
rad_distribute<-function(r, R1, R2, theta, phi){
  
  g<-1/R1*(1 + (r/R2)*sin(theta))*(1/R2 + (1/R1)*sin(phi)*(1 + (r/R2)*sin(theta)))
  
  return(g)
  
}


#-------------------------------------------
#3Dトーラス一様分布を作る。暫定版
#n=データ点数、r, R1, R2=半径
x3Dtorus_unif<-function(n, r, R1, R2){
  
  theta<-c()
  phi<-c()
  
  tp<-expand.grid(seq(0, 2*pi, length=100), seq(0, 2*pi, length=100))
  
  g<-apply(tp, 1, function(i){
    
    g0<-rad_distribute(r, R1, R2, i[1], i[2])
    
    return(g0)
    
  })
  
  max_g<-rad_distribute(r, R1, R2, pi/2, pi/2)
  min_g<-min(g)
  
  while(length(theta) < n){
    
    xvec<-runif(1, 0, 2*pi)
    yvec<-runif(1, 0, 2*pi)
    zvec<-runif(1, min_g, max_g)
    
    fxy<-rad_distribute(r, R1, R2, xvec, yvec)
    
    if(zvec <= fxy){
      theta<-c(theta, xvec)
      phi<-c(phi, yvec)
    }
    
  }
  
  delta<-runif(n, 0, 2*pi)
  
  x<-sin(delta)*(R1 + sin(phi)*(R2 + r*sin(theta)))
  y<-cos(delta)*(R1 + sin(phi)*(R2 + r*sin(theta)))
  z<-cos(phi)*(R2 + r*sin(theta))
  w<-r*cos(theta)
  
  out<-cbind(x, y, z, w)
  
  return(out)
  
}
