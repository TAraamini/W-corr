#library(readxl)
install.packages("Rssa")
library(Rssa)

#Book5 <- read_excel("C:/Users/amini/Desktop/real data (1)/real data/Book 5.xlsx")
#require(graphics)

#View(Book5)
#x<-ts(Book5,start=c(1385,1),end=c(1401,2), frequency=4)
#X<- x[,c(2, 5)]
#X
#plot(X, y = NULL, plot.type = c("single"),
#    xy.labels, xy.lines, panel = lines, col=c(1,6),
#   ylab=" GDP(constant prices (base year: 2019)) ", ylim=c(1000,990000), xlim= c(1386,1401),
#  lty=c(1,3), xaxt="n") 
#axis(side=1, at=c(1385:1401), labels=c(1385:1401), las=2)
#legend("topright",-20, 70, title= "time series", 
#      legend= c("Agriculture", "Indusrtial"),
#     lty=c(1,3), cex=0.4)
#############
install.packages("plotly")
library(plotly)
install.packages("ggplot2")
library(ggplot2)
install.packages("TSstudio")
library(TSstudio)
data("Coffee_Prices")
ts_info(Coffee_Prices)
ts_plot(Coffee_Prices)
Coffee_Prices$Arabica
#@@@@@@@@@@@
ts_plot(Coffee_Prices,
        title = "US Monthly n",
        Xtitle = "Time",
        Ytitle = " Feet",
        color =  "pink",
        Xgrid = TRUE,
        Ygrid = TRUE) %>%
  layout(paper_bgcolor ="black",
         plot_bgcolor = "black",
         font = list(color = "white"),
         yaxis = list(linecolor = "#6b6b6b",
                      zerolinecolor = "#6b6b6b",
                      gridcolor= "#444444"),
         xaxis = list(linecolor = "#6b6b6b",
                      zerolinecolor = "#6b6b6b",
                      gridcolor= "#444444"))

####################
data("Coffee_Prices")
View(Coffee_Prices)
xxx<-ts(Coffee_Prices,start=c(1960,1),end=c(2018,5), frequency=12)
View(xxx)
library(Rssa)

library(urca)

s <- ssa(co2)
w <- wcor(s, groups = 1:20)
plot(w)
plot(w, grid = c(2,4, 5,7))
data(Mars)
s <- ssa(Mars, mask = Mars != 0, wmask = circle(50), kind = "2d-ssa")
aa<- wcor(s, groups = 1:25)
plot(wcor(s, groups = 1:25), grid = c(13, 15, 17,19))

FS.series <- ssa(xxx, L=468, kind="mssa")
?ssa()
??wcor(FS.series)
?wcor() 
library(forecast)
plot(FS.series, type = "vectors", idx=1:35)
plot(FS.series, type = "paired", idx=1:34)
plot(FS.series, type = "values", idx=1:34)
plot(wcor(FS.series)) 
R.series <- reconstruct(S.series, groups=list(signal=c(1:50)))
R.series1 <- R.series $signal
plot(R.series,add.residuals=TRUE, add.original=TRUE, plot.method ="xyplot", superpose=TRUE)
############################L1 method ###################
#x<-ts(Book5,start=c(1385,1),end=c(1401,2), frequency=4)
#Y<- x[, c(5, 12)]

#signal1<- Book5$Agriculturegroup
#signal2<- Book5$Industrialgroup
#Y1<- list(signal1, signal2)

XXX<-ts(Coffee_Prices,start=c(1960,1),end=c(2018,5), frequency=12)
View(XXX)
U<- as.data.frame(XXX)
signal1<- U $ Robusta
signal2<- U$Arabica
YY<- list(signal1, signal2)

VMSSA.Hankel<-function(Y,k){
  X<-NULL
  if(is.list(Y)){
    M<-length(Y)
    for(i in 1:M){
      Li<-length(Y[[i]])-k+1 
      Xi<-outer((1:Li),(1:k),function(x,y) Y[[i]][(x+y-1)])
      X<-rbind(X,Xi)} 
  } 
  else{
    Y<-as.matrix(Y)
    M<-ncol(Y)
    for(i in 1:M){
      Li<-length(Y[,i])-k+1 
      Xi<-outer((1:Li),(1:k),function(x,y) Y[(x+y-1),i])
      X<-rbind(X,Xi)} 
  }
  X
}

X<- VMSSA.Hankel(YY ,189)
##################   SVD  ###################
V.SVD<- function(Y,k){
  X <-  VMSSA.Hankel(Y,k)
  svd(X)
}
X1<-V.SVD(YY , 128)

#############scree plot########################

Sing.plt<-function(Y,K){
  lambda<-log(V.SVD(Y,K)$d)
  d<-length(lambda) 
  win<-1:d
  plot.new() 
  plot.window(xlim=range(win),ylim=range(lambda))
  usr=par("usr") 
  rect(usr[1],usr[3],usr[2],usr[4])
  lines(win,lambda,lwd=2)
  points(win,lambda,pch=21,bg="gray") 
  axis(1) 
  axis(2) 
  box()
  title(xlab="Number") 
  title(ylab="Log. Singular Values")}

??par()
Sing.plt(exam1_Y, 128)
#############################
###############BASIc  VMSSA grouping R code################
V.Group<-function(Y,k,r){ 
  I<-r;p<-length(I) 
  SVD<-V.SVD(Y,k) 
  LambdaI<-matrix(diag(SVD$d)[I,I],p,p)
  SVD$u[,I]%*%LambdaI%*%t(SVD$v[,I])
}
X2<-V.Group(exam1_Y, 5,2)

#####Basic VMSSA hankelization R code############
DiagAver<-function (X){
  L<- nrow(X); K<- ncol(X); N<- K+L-1
  D<- NULL
  for(j in 1:N){
    s1<- max(1, (j-N+L))
    s2<- min (L,j)
    place<- (s1:s2)+L* (((j+1-s1):(j+1-s2))-1)
    D[j]<-mean(X[place])
  }
  D
}
########VDiagAver
VDiagAver<-function(X,series.lengths){
  M<-length(series.lengths)
  k<-ncol(X);
  L<-series.lengths-k+1 
  rows=cumsum(c(0,L))
  D<-NULL 
  for(j in 1:(M)){
    D[[j]]<-DiagAver(X[((rows[j]+1):rows[(j+1)]),])}
  D 
}
#######Basic VMSSA reconstruction R code############
VMSSA.Rec<-function(Y,k,r){ 
  N<-NULL 
  if(is.list(Y)){
    M<-length(Y)
    for(i in 1:M) 
      N[i]<-length(Y[[i]])} 
  else{ 
    Y<-as.matrix(Y) 
    M<-ncol(Y) 
    N<-rep(nrow(Y),M)}
  X<-VMSSA.Hankel(Y,k) 
  XI<-V.Group(Y,k,r) 
  Approx<-VDiagAver(XI,N)
  if(is.list(Y)){
    Resid<-vector("list",M)
    for(i in 1:M)
      Resid[[i]]<-Y[[i]]-Approx[[i]]} 
  else{
    Approx<-matrix(unlist(Approx),ncol=M) 
    Resid<-Y-Approx} 
  list(Approximation=Approx,Residual=Resid)}

X3<- VMSSA.Rec(exam1_Y, 5, 2)
#############################W _ Corraolation #############

w.corr<-function(k,Y1,Y2){
  N<-length(Y1) 
  w.corr<-NULL 
    w<-((N+1)-abs((N+1)/2-(N-k+1))-abs((N+1)/2-1:N)-
        abs(abs((N+1)/2-(N-k+1)) -abs((N+1)/2-1:N)))/2
  sum((w*Y1)*Y2)/sqrt(sum(w*Y1^2)*sum(w*Y2^2))
  } 

signal1<- U $ Robusta
signal2<- U$Arabica
Yt<- YY
Y1<- signal1
Y2<- signal2
w.corr(128,Y1, Y2 )



W.corr<-function(Y,k,r){
  m<-length(r);w.corr<-diag(m)
  if(is.list(Y)){
    M<-length(Y)
    for(p in 1:M){  
    N<-length(Y[[p]])}}
  else{
    Y<-as.matrix(Y)
    M<-ncol(Y)
    N<-rep(nrow(Y),M)}
    w<-((N+1)-abs((N+1)/2-(N-k+1))-abs((N+1)/2-1:N)-
            abs(abs((N+1)/2-(N-k+1))- abs((N+1)/2-1:N)))/2
   wcorr<-function(i,j){
     for(i in 1:(m-1)){
      Y1<-VMSSA.Rec(Y,k,r[[i]])$Approximation
    Y2<-VMSSA.Rec(Y,k,r[[j]])$Approximation 
    for(p in 1:M){
    sum(w*Y1[[p]]*Y2[[p]])/sqrt(sum(w*Y1[[p]]^2)*sum(w*Y2[[p]]^2))}} }
  for(i in 1:(m-1)){
    for(j in (i+1):m){ 
  w.corr[i,j]=w.corr[j,i]=wcorr(i,j)}}
  rownames(w.corr)<-colnames(w.corr)<-r
  w.corr
}

exam1_signal1 <- 3* sin(2*pi * (1:200) / 12)
exam1_signal2 <- 2* sin (2*pi * (1:200)/12 + pi/4)
exam1_Y <- list(exam1_signal1, exam1_signal2)
aaa<- W.corr(exam1_Y,96,seq(1:6))
install.packages("graphics")

library(graphics)
Plt.Img(W.corr(exam1_signal1,128,seq(1:20)))
plot(W.corr(exam1_signal1,128,seq(1:20)))
title(main="w-correlation")
##########################################

plot(X3)
S.series <- ssa(X, L=13, kind="mssa")
plot(S.series,type = "vectors", idx=1:13)
plot(S.series,type = "paired", idx=1:10)
plot(S.series,type = "values", idx=1:10)
plot(wcor(S.series)) 
R.series <- reconstruct(S.series, groups= list(signal=c(1:6)))
R.series1 <- R.series $signal
plot(R.series,add.residuals=TRUE, add.original=TRUE, plot.method ="xyplot", superpose=TRUE)

??plot.window


#############Basic Recurrent VMSSA forecasting R code#########
forecastVMSSA.R<-function(k,r,h,Y){
  rr<-length(r)
  Dec<-V.SVD(Y,k)
  sigma<-Dec$d
  d<-length(sigma[sigma>0])
  U<-matrix(Dec$u,ncol=d)
  V<-matrix(Dec$v,d,k)
  hx<-VMSSA.Rec(Y,k,r)$Approximation
  if(is.list(Y)){
    M<-length(Y);L<-NULL 
    forecasts<-array(0,dim=c(h,M)) 
    for(i in 1:M) L[i]<-length(Y[[i]])-k+1
    Ld<-c(0,cumsum(L-1))
    W<-array(U[cumsum(L),r],dim=c(M,rr))
    Ud<-array(U[-cumsum(L),r],dim=c((sum(L)-M),rr)) 
    R<-solve(diag(M)-W%*%t(W))%*%W%*%t(Ud)
    for(i in 1:h){
      for(j in 1:M){
        for(m in 1:M){
          Zh<-hx[[m]][(k+i):(k+L[m]+i-2)] 
          jm<-(Ld[m]+1):Ld[(m+1)]
          forecasts[i,j]<-forecasts[i,j]+R[j,jm]%*%Zh}
        hx[[j]]<-c(hx[[j]],forecasts[i,j])}}
    forecasts} 
  else{
    M<-ncol(Y);L<-rep((nrow(Y)-k+1),M) 
    forecasts<-array(0,dim=c(h,M))
    W<-array(U[cumsum(L),r],dim=c(M,rr))
    Ud<-array(U[-cumsum(L),r],dim=c((sum(L)-M),rr))
    R<-solve(diag(M)-W%*%t(W))%*%W%*%t(Ud)
    for(i in 1:h){
      Zh<-as.vector(hx[(k+i):(k+L[1]+i-2),])
      forecasts[i,]<-R%*%Zh 
      hx<-rbind(hx,forecasts[i,])} 
    forecasts}}

forecastVMSSA.R1<-forecastVMSSA.R(k=5,r=2,h=6,Y1)

##############################################################################
L1.VMSSA.Hankel<-function(Y,k){
  X<-NULL
  if(is.list(Y)){
    M<-length(Y)
    for(i in 1:M){
      Li<-length(Y[[i]])-k+1 
      Xi<-outer((1:Li),(1:k),function(x,y) Y[[i]][(x+y-1)])
      X<-rbind(X,Xi)} 
  } 
  else{
    Y<-as.matrix(Y)
    M<-ncol(Y)
    for(i in 1:M){
      Li<-length(Y[,i])-k+1 
      Xi<-outer((1:Li),(1:k),function(x,y) Y[(x+y-1),i])
      X<-rbind(X,Xi)} 
  }
  X
}

X<- L1.VMSSA.Hankel(Y1 , 5)
##################   SVD  ###################
L1.V.SVD<- function(Y,K){
  X <-  L1.VMSSA.Hankel(Y,K)
  svd(X)
}
X1<-L1.V.SVD(Y1, 5)
####################
B.t <- function(Y, k,r){
  X <-  L1.VMSSA.Hankel(Y, k)
  SVD <- svd(X)
  lambda <- diag(SVD$d[1:r])
  V <- SVD$v[,1:r]
  Bt <- V %*% t(lambda)
  return(Bt)
}

B_t<- B.t(Y1, 5, 2)
dim(B_t)
#################### function calculation LAD #######
install.packages("quantreg")
library(quantreg)
LAD <- function(B, Xj){
  
  data <- data.frame(B, Xj)
  aj <- rq(Xj ~ .-1, data, tau = 0.5)
  aj <- unname(aj$coefficients)
  return(aj)
}

A.t <- apply (t(X), 2, LAD, B= B_t)
A <- t( A.t)
RealData_signal<- A%*% t( B_t)
#################
VDiagMed<-function (X){
  L<- nrow(X); K<- ncol(X); N<- K+L-1
  D<- NULL
  for(j in 1:N){
    s1<- max(1, (j-N+L))
    s2<- min (L,j)
    place<- (s1:s2)+L* (((j+1-s1):(j+1-s2))-1)
    D[j]<-median(X[place])
  }
  D
}
Z<- VDiagMed(RealData_signal)
########## Diagonal median function for MSSA #####
VDiagMedian<-function(X,series.lengths){
  M<-length(series.lengths)
  k<-ncol(X); L<-series.lengths-k+1 
  rows=cumsum(c(0,L)) 
  D<-NULL 
  for(j in 1:(M)){
    D[[j]]<-VDiagMed(X[((rows[j]+1):rows[(j+1)]),])}
  D 
}

ZZ<-VDiagMedian(RealData_signal,10)
########################################################################################
Replace_L <- function(X, size, delta_factor){
  signal <- X
  index = which(signal==c(signal))
  ZZ <- data.frame (index, signal)
  (SAMPLE <- ZZ[sample(nrow(ZZ), size= size), ]$ index)
  QW <- (delta_factor)* signal[c(SAMPLE)]
  Replace <- replace(signal, c(SAMPLE), c(QW))
  return(Replace)
}
S<- Replace_L(X, 5, 2)
######################################### function Reconstruct################

L1.Reconstructsignal<- function(ts_list, k, r, st.me=1, size, delta_factor){
  N<- NULL
  signal1 <- ts_list[[1]]
  signal2 <- ts_list[[2]]
  N1 <- length(signal1)
  N2 <- length(signal2)
  rep_signal1 <- Replace_L(signal1, size=size, delta_factor = delta_factor)
  rep_signal2 <- Replace_L(signal2, size=size, delta_factor = delta_factor)
  Re_series1 <- rep_signal1 + rnorm(N1, mean = 0, sd = st.me )
  Re_series2 <- rep_signal2 + rnorm(N2, mean = 0, sd = st.me )
  list.ts <- list(Re_series1,Re_series2)
  X <- L1.VMSSA.Hankel(list.ts, k=k)
  B_t <- B.t (list.ts,  k=k, r =r)
  A.t <- apply (t(X), 2, LAD, B=B_t)
  A <- t(A.t)
  Signal_main <- A%*% t(B_t)
  VDmedian <- VDiagMedian (Signal_main, series.lengths=c(N1,N2))
  output1 <- VDmedian
  return(output1)  
}
#######################forercasting_R function #################################

VRec_RMSE.L1.signal<- function(ts_list, k, r,  st.me, size, delta_factor){
  signal1 <- ts_list[[1]]
  signal2 <- ts_list[[2]]
  N1 <- length(signal1)
  N2 <- length(signal2)
  rep_signal1 <- Replace_L(signal1,size=size, delta_factor = delta_factor)
  rep_signal2 <- Replace_L(signal2,size=size, delta_factor = delta_factor)
  Re_series1 <- rep_signal1 + rnorm(N1, mean = 0, sd = st.me )
  Re_series2 <- rep_signal2 + rnorm(N2, mean = 0, sd = st.me )
  list.ts <- list(Re_series1,Re_series2)
  X <- L1.VMSSA.Hankel(list.ts, k=k)
  B_t <- B.t (list.ts,  k=k, r =r)
  A.t <- apply (t(X), 2, LAD, B=B_t)
  A <- t(A.t)
  Signal_main <- A%*% t(B_t)
  VDmedian <- VDiagMedian (Signal_main, series.lengths=c(N1,N2))
  hhx <- VDmedian 
  Dec<-L1.V.SVD(ts_list, k)
  hx<-L1.Reconstructsignal(list.ts, k,r,st.me, size, delta_factor)
  Rec_RMSE.1 <- sqrt(mean((signal1- hx[[1]])^2))
  Rec_RMSE.2 <- sqrt (mean((signal2 - hx[[2]])^2))
  Rec_RMSE_L1 <- c(Rec_RMSE.1, Rec_RMSE.2)
  Rec_MAD.L1.1 <- mean(abs(signal1- hx[[1]]))
  Rec_MAD.L1.2 <- mean(abs(signal1- hx[[2]]))
  Rec_MAD_L1 <- c(Rec_MAD.L1.1, Rec_MAD.L1.2)
  S <-VMSSA.Hankel(Y,k)
  rec_1 <- VMSSA.Rec(Y,k,r)$Approximation
  Rec_MAD.B.1 <- mean(abs(signal1-rec_1[[1]]))
  Rec_MAD.B.2 <- mean(abs(signal1-rec_1[[2]]))
  Rec_MAD_Basic <- c(Rec_MAD.B.1, Rec_MAD.B.2)
  Rec_RMSE_B1 <- sqrt(mean((signal1- rec_1[[1]])^2))
  Rec_RMSE_B2 <- sqrt(mean((signal2 - rec_1[[2]])^2))
  Rec_RMSE_Basic <- c(Rec_RMSE_B1, Rec_RMSE_B2)
  Rec_MAD.B.1 <- mean(abs(signal1- rec_1[[1]]))
  Rec_MAD.B.2 <- mean(abs(signal2- rec_1[[2]]))
  Rec_MAD_Basic <- c(Rec_MAD.B.1, Rec_MAD.B.2)
  output1 <- c( (Rec_RMSE.1 + Rec_RMSE.2), (Rec_RMSE_B1 + Rec_RMSE_B2),
                ( Rec_MAD.L1.1+ Rec_MAD.L1.2), (Rec_MAD.B.1 + Rec_MAD.B.2))
  output2 <- c(Rec_RMSE_L1, Rec_RMSE_Basic , Rec_MAD_L1, Rec_MAD_Basic)
  meansumoferrors <- c(( (Rec_RMSE.1 + Rec_RMSE.2)/2), ((Rec_RMSE_B1 + Rec_RMSE_B2)/2),
                       ((Rec_MAD.L1.1+ Rec_MAD.L1.2)/2), ((Rec_MAD.B.1 + Rec_MAD.B.2)/2))
  names(output1) <- c("f_RL1", "f_RB", "f_ML1", "f_MB")
  names(output2) <- c("f_RL1_signal1", "f_RL1_signal2" ,
                      "f_RB_signal1","f_RB_signal2", 
                      "f_M.L1.signal1", "f_M.L1.signal2",
                      "f_M.B.signal1","f_M.B.signal2")
  return(c(output1, output2))  
}

S.series <- ssa(X, L=13, kind="mssa")
plot(S.series,type = "vectors", idx=1:13)
plot(S.series,type = "paired", idx=1:10)
plot(S.series,type = "values", idx=1:10)
plot(wcor(S.series)) 
R.series <- reconstruct(S.series, groups= list(signal=c(1:6)))
R.series1 <- R.series $signal
plot(R.series,add.residuals=TRUE, add.original=TRUE, plot.method ="xyplot", superpose=TRUE)



########determination parameters for vertical##########



VRec_RMSE.L1.signal(ts_list, k, r,  st.me, size, delta_factor)


L12r5<- VRec_RMSE.L1.signal(Y1, 12, 5, st.me=1, 5, 1) 
L12r5

L13r5<- VRec_RMSE.L1.signal(Y1, 13, 5, st.me=1, 5, 1)
L13r5

L14r5<- VRec_RMSE.L1.signal(Y1, 14, 5, st.me=1, 5, 1)
L14r5

L15r5<- VRec_RMSE.L1.signal(Y1, 15, 5, st.me=1, 5, 1)
L15r5

L16r5<- VRec_RMSE.L1.signal(Y1, 16, 5, st.me=1, 5, 1)
L16r5

L17r5<- VRec_RMSE.L1.signal(Y1, 17,5, st.me=1, 5, 1)
L17r5

L18r5<- VRec_RMSE.L1.signal(Y1, 18, 5, st.me=1, 5, 1)
L18r5

L19r6<- VRec_RMSE.L1.signal(Y1, 19, 6, st.me=1, 5, 1)
L19r6

L20r7<- VRec_RMSE.L1.signal(Y1, 20,7, st.me=1, 5, 1)
L20r7

L21r7<- VRec_RMSE.L1.signal(Y1, 21, 7, st.me=1, 5, 1)
L21r7

L22r7<- VRec_RMSE.L1.signal(Y1, 22,7, st.me=1, 5, 1)
L22r7

L23r7<- VRec_RMSE.L1.signal(Y1, 23, 7, st.me=1, 5, 1)
L23r7

L24r7<- VRec_RMSE.L1.signal(Y1, 24, 7, st.me=1, 5, 1)
L24r7

L25r7<- VRec_RMSE.L1.signal(Y1, 25, 7, st.me=1, 5, 1)
L25r7

L26r7<- VRec_RMSE.L1.signal(Y1, 26, 7, st.me=1, 5, 1)
L26r7

L27r7<- VRec_RMSE.L1.signal(Y1, 27, 7, st.me=1, 5, 1)
L27r7

L28r7<- VRec_RMSE.L1.signal(Y1, 28, 7, st.me=1, 5, 1)
L28r7

L29r7<- VRec_RMSE.L1.signal(Y1, 29, 7, st.me=1, 5, 1)
L29r7

L30r7<- VRec_RMSE.L1.signal(Y1, 30, 7, st.me=1, 5, 1)
L30r7

L31r7<- VRec_RMSE.L1.signal(Y1, 31, 7, st.me=1, 5, 1)
L31r7

L32r7<- VRec_RMSE.L1.signal(Y1, 32, 7, st.me=1, 5, 1)
L32r7

L33r7<- VRec_RMSE.L1.signal(Y1, 33, 7, st.me=1, 5, 1)
L33r7

L34r7<- VRec_RMSE.L1.signal(Y1, 34, 7, st.me=1, 5, 1)
L34r7

L35r7<- VRec_RMSE.L1.signal(Y1, 35, 7, st.me=1, 5, 1)
L35r7

L36r7<- VRec_RMSE.L1.signal(Y1, 36, 7, st.me=1, 5, 1)
L36r7

L37r7<- VRec_RMSE.L1.signal(Y1, 37, 7, st.me=1, 5, 1)
L37r7

L38r7<- VRec_RMSE.L1.signal(Y1, 38, 7, st.me=1, 5, 1)
L38r7

L39r7<- VRec_RMSE.L1.signal(Y1, 39, 7, st.me=1, 5, 1)
L39r7

L40r7<- VRec_RMSE.L1.signal(Y1, 40, 7, st.me=1, 5, 1)
L40r7

L41r8<- VRec_RMSE.L1.signal(Y1, 41, 8, st.me=1, 5, 1)
L41r8

L42r8<- VRec_RMSE.L1.signal(Y1, 42, 8, st.me=1, 5, 1)
L42r8

L43r8<- VRec_RMSE.L1.signal(Y1, 43, 8, st.me=1, 5, 1)
L43r8

L44r8<- RMSE.L1.signal(Y1, 44, 8, st.me=1, 5, 1)
L44r8
Dr.Amini <- data.frame(L36r6h=L36r6h, L72r8h=L72r8h, L108r11h=L108r11h,
                       L216r6h=L216r6h, L252r6h=L252r6h, L288r3h=L288r3h, L324r9h=L324r9h,
                       L360r9h=L360r9h, L396r9h=L396r9h, L432r8h=L432r8h, 
                       L468r6h=L468r6h, L504r6h=L504r6h, L576r15h=L576r15h, L612r9h=L612r9h,
                       L648r11h=L648r11h)

as.matrix(Dr.Amini)
Dr.Amini
write.csv(Dr.Amini, "outputfer4020505.csv")
