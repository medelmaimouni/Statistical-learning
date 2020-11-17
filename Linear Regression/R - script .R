#==============================================================================================================

# Author: Mohamed EL MAIMOUNI
#==============================================================================================================

# # 1-) Matrice X (n,p) de plein range
p = 2
n = 100
r <- 0
while (r != p)
 {
X = matrix( randomNumbers(n = n , min = -n/2, max = n/2, check = TRUE) , nrow = n, ncol = p, byrow = TRUE)
r = qr(X)$rank
W = cbind(rep(1,n), X)
f = qr(W)$rank
if (f != p+1) {
  r = 0
}
}
## A ce stage, nous sommes assurre que X est de plein rang et que (1,,1) n'est pas dans
# son image




# 
# # X est une matrice nxp remplie par la méthode randomSequence qui renvoie des nombres sans duplicate
# #dans l'intervalle [-100, 100] de plein rang.
# 
# ## Pour creer une matrice avec que des valeurs positives on fixe l'intervalle [min, max] = [n, 3n] par exemple
# 
# X_posti = matrix( randomNumbers(n = n , min = n, max = 2*n, check = TRUE) , nrow = n, ncol = p, byrow = TRUE)
# 
# #============================================================================================================
# 
# 
# # 2-) Vecteur Y bruité
# 
Beta = c(-2, 7)
eps = rnorm(n,  mean = 0 , sd = sqrt(2))
# 

# Y = X%*%Beta + eps
# 
# # plot(X[,1],Y)
# 
# #============================================================================================================
# 
# # 3-) Plot de Y par rapport a X1 et X2
# 
# #=========  les coefficients de X sont tous positifs   =================================
# 
c <- 0
while (r != p ){
C = matrix( randomNumbers(n = n , min = 0, max = n, check = TRUE) , nrow = n, ncol = p, byrow = TRUE)
c = qr(C)$rank
r = qr(X)$rank
}
#
Y_c = C%*%Beta + eps
# 
# #
plot(C[,1],Y_c,
     xlab = "X1",
     ylab = "Y",
     main = "les coefficients de X sont tous positifs",
     col="blue")


x = (max(C[,1])-min(C[,1]))*(1:n)/(n+min(C[,1]))
y =-2*x
points(x,y,type='l', col = "red")
legend(1, 95,legend = c(" y = -2x", "X"), col=c("red", "blue"), lty=1:2, yjust = -2)
#
# 
# #=========  les coefficients de X sont negatifs et postifs   =================================
# 
# 
X = matrix( randomNumbers(n = n , min = -n/2, max = n/2 +1, check = TRUE) , nrow = n, ncol = p, byrow = TRUE)
Y = X%*%Beta + eps
plot(X[,2],Y,
     xlab = "X2",
     main = "les coefficients de X sont negatifs et positifs",
      col="blue")
x = (max(X[,2])-min(X[,2]))*(-n:n)/(n+min(X[,2]))

y = 7*x
points(x,y,type='l', col = "red")
legend(1, 95,legend = c(" y = 7x", "X"), col=c("red", "blue"), lty=1:2, yjust = -1, xjust = 1)

# 
# 
# 
# 
# #============================================================================================================
# 
# # 4 -) Calcul de Beta estimation
# 
# # on utilise la fonction solve() pour inverser t(X)X
# #
Beta_est =  solve(t(X)%*%X)%*%t(X)%*%Y
# 
# 
# # #============================================================================================================
# #
# # # 5-)
# #
res=lm(Y~X[,1]+X[,2])
summary(res)
# # #============================================================================================================
# #
p = 3

# # # 6-) Ajout de colonne 1
# # #
Z = cbind(rep(1,n), X)

#
Beta_est_3 =  solve(t(Z)%*%Z)%*%t(Z)%*%Y
# #
# # #============================================================================================================
# # # 7-)
# 
inv_Xt_X = solve(t(Z)%*%Z)

Y_est = Z%*%Beta_est_3

var_bet_2 = t(rep(0,p))
T_bet = t(rep(0,p))

Y_bar = 0       #On suppose que qu'on aura au min un element pourque Y_bar ne restera pas 0
for (i in 1:n){
  Y_bar = Y_bar + (1/n)*(Y[i])
}


var_2 = (t(Y-Y_est)%*%(Y-Y_est))/(n - p)

R_2 = (t(Y_est-Y_bar)%*%(Y_est-Y_bar))/((t(Y-Y_bar)%*%(Y-Y_bar)))

for (j in 1:p){
  var_bet_2[j] = var_2*inv_Xt_X[j,j]
  T_bet[j] = Beta_est_3[j]/sqrt(var_bet_2[j])
}

F_1 = ((R_2)*(n-p))/((1-R_2)*((p-1)))
# 
# 
# # #============================================================================================================
# #
# # # 8-) p-values and Pr(>|t|)
# #
p_value = pf(F,p-1,n-p)
Pr_t = pt(T_bet,n-p)
# 
# 
#============================================================================================================
#                                        Real Data                                                          
#============================================================================================================

#================================= Boston =================================================

housing_data = read_excel("./Real_data/housing.xlsx")
Y_housing = housing_data[,1]
X_housing= housing_data[, 2:14]

# corr_matrix = cor(housing_data, method = 'pearson')


dropList = c('CHAS')
housing_data = housing_data[, !colnames(housing_data) %in% dropList]
formule = 'ZN'
for (i in 3:length(housing_data)){

  formule = paste(formule, colnames(housing_data)[i], sep = ' + ')

}
res = lm(CRIM ~ ZN + INDUS  + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + B + LSTAT + MEDV , data = housing_data)

summary(reg)



#============================ Forest fire ==========================================================


dataset = read.csv('./Real_data/forestfires.csv')
droped = c('area')

formule_F = 'X'

for (i in 3:length(dataset)-1){
  formule_F = paste(formule_F, colnames(dataset)[i], sep = ' + ')
  
}

regg = lm(log(1+area) ~ X + Y + month + day + FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = dataset)
summary(regg)



