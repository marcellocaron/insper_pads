library(tidyverse)
library(FNN)

# amostra gerada ----------------------------------------------------------

n_obs <- 100

set.seed(123)

dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

dados %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  theme_bw()


# KNN ---------------------------------------------------------------------

knn.reg(train = dados$x, 
        test = matrix(dados$x[1:10]), 
        y =  dados$y, 
        k = 10)$pred


n_obs <- 100

set.seed(123)

dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

validacao <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

x_pred <- seq(8, 18, 0.1)

tibble(ajuste = x_pred,
       y_fit = knn.reg(train = dados$x, 
                       test = matrix(x_pred), 
                       y =  dados$y, 
                       k = 15)$pred) %>% 
  ggplot(aes(ajuste, y_fit)) + 
    geom_point(data = dados, aes(x, y)) +
    geom_step(color = "red")


# KNN validacao cruzada 10 ----------------------------------------------------------

n_obs <- 100

set.seed(124)

dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

#eqm_aux <- vector("numeric", 10) # 
eqm_aux <- rep(0, 10)

lotes <- sample(rep(1:10, each = 10))

resultados <- tibble(gl = 1:90, 
                     eqm = NA_real_)

for (i in 1:nrow(resultados)) {
  
  
  for (j in 1:10) {
    
    
    # calcula o EQM do lote j e guarda em um vetor
    
    y_fit <- knn.reg(train = dados$x[lotes != j], 
                   test = matrix(x_pred), 
                   y =  dados$y[lotes != j], 
                   k = resultados$gl[i])$pred
    
    eqm_aux[j] <- mean((dados$y[lotes == j] - y_fit)^2)
    
    
  }
  
  resultados$eqm[i] <- mean(eqm_aux)
  
  # guarda a media do vetor com os EQM em resultados$eqm[i]
  
  
}


resultados %>% 
  
  arrange(eqm)



y_fit <- knn.reg(train = dados$x[lotes != 1], 
                 test = matrix(x_pred), 
                 y =  dados$y[lotes != 1], 
                 k = resultados$gl[1])$pred

eqm_aux[1] <- mean((dados$y[lotes == 1] - y_fit)^2)