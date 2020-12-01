#3次元トーラスを生成するための角度の分布生成関数のテスト

thph<-expand.grid(seq(0, 2*pi, length=100), seq(0, 2*pi, length=100))

t3_rads<-apply(thph, 1, function(i){
  
  g0<-rad_distribute(r = 2, R1=8, R2=4, i[1], i[2])
  
  return(g0)
  
})

fr<-function(rad, r, R1, R2){
  
  theta<-rad[1]
  phi<-rad[2]
  
  g<-1/R1*(1 + (r/R2)*sin(theta))*(1/R2 + (1/R1)*sin(phi)*(1 + (r/R2)*sin(theta)))
  
  return(g)
  
}

grr<-function(rad, r, R1, R2){
  
  theta<-rad[1]
  phi<-rad[2]
  
  f_t<-(r/(R1*R2)) * cos(theta)*( 2*cos(phi)*( (1/R1)+(r/R1*R2)*sin(theta) ) + 1/R2 )
  f_p<-cos(phi) * ( (1/R1) + (r/(R1*R2))*sin(theta) )
  
  return(c(f_t, f_p))
  
}

opt_fr<-optim(par = c(0, 0), fn = fr, control = list(fnscale = -1), r=2, R1=8, R2=4)