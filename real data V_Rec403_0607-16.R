#library(readxl)
install.packages("Rssa")
library(Rssa)

#Book5 <- read_excel("C:/Users/amini/Desktop/real data (1)/real data/Book 5.xlsx")
#require(graphics)

TRUE, plot.method ="xyplot", superpose=TRUE)



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
