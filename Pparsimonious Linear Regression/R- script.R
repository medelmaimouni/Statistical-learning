
#==============================================================================================================

# Author: Mohamed EL MAIMOUNI
#==============================================================================================================

# ================= Generer une matrice qui n'est pas de plein rang =============================


n = 10
p = 100
dist = rnorm(n*p, mean = 0, sd = 2)
X = matrix(dist, nrow = n, ncol = p, byrow = TRUE)

eps = rnorm(n, mean = 0 , sd = sqrt(2))
beta = c(-2,7, rep(0,p-2))
Y = X%*%beta + eps
#

#================= Calcule de l'estimateur des moindres carrees =================

beta_hat = solve(t(X)%*%X)%*%t(X)%*%Y


#================= Estimer beta Ridge   =================

lamda = 1
Ridge = t(X)%*%X+lamda*diag(p)
beta_Ridge = solve(Ridge)%*%t(X)%*%Y

#================= Elastic Net   =================

res = glmnet(X,Y, alpha = 0.5, lambda = seq(1,10,0.1), intercept = FALSE, standardize = FALSE )
Beta_elsatic = matrix(coef(res, s=1))

plot(res)
title(c('Elastic Net - alpha = 0.5', cex = '',
       color = ''))


#================ LASSO ==================

lasso = glmnet(X, Y, alpha = 1 , lambda = seq(0.5,10,0.1), intercept = FALSE, standardize = FALSE)
Beta_lasso = matrix(coef(lasso, s = 1))


plot(lasso)

plot(lasso, xvar = 'norm', label = TRUE)
title(c('LASSO', cex = '',
        color = ''))
vnat=coef(lasso)
vnat=vnat[-1,ncol(vnat)]
vn=paste("Beta",1:100)

axis(4, at=vnat,line=-.5,label=vn,las=1,tick=FALSE, cex.axis=0.5)


#============== X avec colonnes correlees =======================

gauss_2 = rnorm(n*p/2, mean = 0 , sd = 10)

X_2 = matrix(gauss_2, nrow = n, ncol = p, byrow = FALSE)
X_2[,51:100] = X_2[,51:100] + rnorm(1, mean = 0 , sd = sqrt(0.01))


Y_2 = X_2%*%beta +eps

alphas = c(0,0.5,1)
# 
Beta_corr = data.frame(
  'alpha: 0' = matrix(coef(glmnet(X_2,Y_2, alpha = 0, lambda = 1, intercept = FALSE, standardize = FALSE ))),
  'alpha: 0.5' = matrix(coef(glmnet(X_2,Y_2, alpha = 0.5, lambda = 1, intercept = FALSE, standardize = FALSE ))),
  'alpha: 1' = matrix(coef(glmnet(X_2,Y_2, alpha = 1, lambda = 1, intercept = FALSE, standardize = FALSE )))

)


#================ Cross val ==================================
# 
resu = cv.glmnet(X, Y, alpha = 0.5, intercept = FALSE, standardize = FALSE )
l = resu$lambda[which.min(resu$cvm)]

Beta_cv = matrix(coef(resu, s= l ))


#============= Real data  ============================================================

#===============Boston =========================
# Nous avons ajoute le nom des colonnes et converti notre dataset en xlsx format

# 
housing = read_excel('./Real_data/housing.xlsx')
Y = housing$CRIM
X = housing[,!names(housing) %in% c('CRIM')]

res_hous = cv.glmnet(as.matrix(X),Y, alpha = 1)

lamda_opt = res_hous$lambda[which.min(res_hous$cvm)]

beta_hous = matrix(coef(res_hous, s = lamda_opt))
rownames(beta_hous) = c('Inter', colnames(X))

res_hous1 = glmnet(as.matrix(X),Y, alpha = 1, lambda = 1)
beta_hous1 = matrix(coef(res_hous1))
rownames(beta_hous1) = c('Inter', colnames(X))



#=================== Forest Fire =========================

dataset = read.csv('./Real_data/forestfires.csv')
droplist = c('month', 'day', 'area')
Y = dataset[,13]
X = dataset[, !colnames(dataset) %in% droplist]
formule = 'X'
for (elt in colnames(X)){
  if (elt != 'X'){
  formule = paste(formule, elt, sep = ' + ')
}
}
#========== Elastic Net alpha  = 0.5 ====================================

forest_cv = cv.glmnet(as.matrix(X),Y, alpha = 0.5)

forest_lambda = forest_cv$lambda[which.min(forest_cv$cvm)]

beta_forest1  = matrix(coef(forest_cv, s = forest_lambda))
rownames(beta_forest1) = c('Intercept', colnames(X))
colnames(beta_forest1) = c("Elastic Net alpha  = 0.5")

#========== LASSO alpha  = 1 ====================================

lasso_cv = cv.glmnet(as.matrix(X),Y, alpha = 1)

lasso_lambda = lasso_cv$lambda[which.min(lasso_cv$cvm)]

beta_lasso_forest = matrix(coef(lasso_cv, s = lasso_lambda))
rownames(beta_lasso_forest) = c('Interept', colnames(X))
colnames(beta_lasso_forest) = c("LASSO alpha  = 1")


#=========== Ordinary Regression =====================

res = lm( area ~ X + Y + FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = dataset)

summary(res)








