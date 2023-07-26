library(tidyverse)

# psi(x) - modelo de base -------------------------------------------------

ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
  stat_function(fun = function(x) 45*tanh(x/1.9 - 7) + 57, linewidth = 1.2) +
  labs(x = "anos de educação", y = "renda anual") + 
  xlim(8, 18) + 
  theme_bw()


# amostra gerada ----------------------------------------------------------

n_obs <- 100

set.seed(123)

dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

dados %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  theme_bw()


resultados <- tibble(gl = 2:100, 
                     eqm = NA_real_)



for (i in 1:nrow(resultados)) {
  
  # ajusta spline com i graus de liberdade
  fit <- smooth.spline(dados$x, dados$y, df = resultados$gl[i], all.knots = TRUE)
  
  # calcula o EQM e guarda em resultados$eqm[i]
  resultados$eqm[i] <- mean((dados$y - predict(fit, dados$x)$y)^2)
  
}

resultados





fit <- smooth.spline(dados$x, dados$y, df = 10, all.knots = TRUE)
predict(fit, dados$x)$y



resultados <- tibble(gl = 2:100, 
                     eqm = NA_real_)



for (i in 1:nrow(resultados)) {
  
  # ajusta spline com i graus de liberdade
  fit <- smooth.spline(dados$x, dados$y, df = resultados$gl[i], all.knots = TRUE)
  
  # calcula o EQM e guarda em resultados$eqm[i]
  resultados$eqm[i] <- mean((dados$y - predict(fit, dados$x)$y)^2)
  
}

resultados

?smooth.spline()


?knots


[21:38] Tiago Santos

# validação cruzada em 10 lotes -------------------------------------------



n_obs <- 100



set.seed(123)



dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))



eqm_aux <- vector("numeric", 10) # eqm_aux <- rep(0, 10)



lotes <- sample(rep(1:10, each = 10))



resultados <- tibble(gl = 2:90, 
                     
                     eqm = NA_real_)





for (i in 1:nrow(resultados)) {
  
  
  for (j in 1:10) {
    
    
    # calcula o EQM do lote j e guarda em um vetor
    
    fit <- smooth.spline(dados$x[lotes != j], 
                         
                         dados$y[lotes != j], 
                         
                         df = resultados$gl[i], 
                         
                         all.knots = TRUE)
    
    
    eqm_aux[j] <- mean((dados$y[lotes == j] - predict(fit, dados$x[lotes == j])$y)^2)
    
    
  }
  
  
  resultados$eqm[i] <- mean(eqm_aux)
  
  # guarda a media do vetor com os EQM em resultados$eqm[i]
  
  
}





resultados %>% 
  
  arrange(eqm)



resultados %>% 
  
  ggplot(aes(gl, eqm)) +
  
  geom_point() +
  
  geom_line()





fit <- smooth.spline(dados$x, 
                     
                     dados$y, 
                     
                     df = 7, 
                     
                     all.knots = TRUE)

[21:38] Tiago Santos

# validação cruzada em 10 lotes -------------------------------------------



n_obs <- 100



set.seed(123)



dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))



eqm_aux <- vector("numeric", 10) # eqm_aux <- rep(0, 10)



lotes <- sample(rep(1:10, each = 10))



resultados <- tibble(gl = 2:90, 
                     
                     eqm = NA_real_)





for (i in 1:nrow(resultados)) {
  
  
  for (j in 1:10) {
    
    
    # calcula o EQM do lote j e guarda em um vetor
    
    fit <- smooth.spline(dados$x[lotes != j], 
                         
                         dados$y[lotes != j], 
                         
                         df = resultados$gl[i], 
                         
                         all.knots = TRUE)
    
    
    eqm_aux[j] <- mean((dados$y[lotes == j] - predict(fit, dados$x[lotes == j])$y)^2)
    
    
  }
  
  
  resultados$eqm[i] <- mean(eqm_aux)
  
  # guarda a media do vetor com os EQM em resultados$eqm[i]
  
  
}





resultados %>% 
  
  arrange(eqm)



resultados %>% 
  
  ggplot(aes(gl, eqm)) +
  
  geom_point() +
  
  geom_line()





fit <- smooth.spline(dados$x, 
                     
                     dados$y, 
                     
                     df = 7, 
                     
                     all.knots = TRUE)

[21:39] Tiago Santos

library(tidyverse)



# install.packages("tidyverse")



# psi(x) - modelo de base -------------------------------------------------



ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
  
  stat_function(fun = function(x) 45*tanh(x/1.9 - 7) + 57, linewidth = 1.2) +
  
  labs(x = "anos de educação", y = "renda anual") + 
  
  xlim(8, 18) + 
  
  theme_bw()





# amostra gerada ----------------------------------------------------------



?runif



set.seed(31)



runif(n = 5, min = 8, max = 18)



runif(n = 5, min = 8, max = 18)





n_obs <- 100



set.seed(123)



dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))



dados %>% 
  
  ggplot(aes(x, y)) + 
  
  geom_point() + 
  
  theme_bw()



# spline ------------------------------------------------------------------



fit <- smooth.spline(dados$x, dados$y, df = 10, all.knots = TRUE)

predict(fit, dados$x)$y



resultados <- tibble(gl = 2:100, 
                     
                     eqm = NA_real_)



for (i in 1:nrow(resultados)) {
  
  
  # ajusta spline com i graus de liberdade
  
  fit <- smooth.spline(dados$x, dados$y, df = resultados$gl[i], all.knots = TRUE)
  
  
  # calcula o EQM e guarda em resultados$eqm[i]
  
  resultados$eqm[i] <- mean((dados$y - predict(fit, dados$x)$y)^2)
  
  
}



# observado <- c(1, 2, 3)

# predito <- c(10, 5, 7)

# mean((observado - predito)^2)



resultados %>% 
  
  ggplot(aes(x = gl, y = eqm)) +
  
  geom_point() +
  
  geom_line()







# treinamento e validação -------------------------------------------------



n_obs <- 100



set.seed(123)



dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))



validacao <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                    
                    y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))





resultados <- tibble(gl = 2:100, 
                     
                     eqm = NA_real_)



for (i in 1:nrow(resultados)) {
  
  
  # ajusta spline com i graus de liberdade
  
  fit <- smooth.spline(dados$x, dados$y, df = resultados$gl[i], all.knots = TRUE)
  
  
  # calcula o EQM e guarda em resultados$eqm[i]
  
  resultados$eqm[i] <- mean((validacao$y - predict(fit, validacao$x)$y)^2)
  
  
}



resultados %>% 
  
  arrange(eqm)



resultados %>% 
  
  ggplot(aes(x = gl, y = eqm)) +
  
  geom_point() +
  
  geom_line()







# validação cruzada em 10 lotes -------------------------------------------



n_obs <- 100



set.seed(123)



dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))



eqm_aux <- vector("numeric", 10) # eqm_aux <- rep(0, 10)



lotes <- sample(rep(1:10, each = 10))



resultados <- tibble(gl = 2:90, 
                     
                     eqm = NA_real_)





for (i in 1:nrow(resultados)) {
  
  
  for (j in 1:10) {
    
    
    # calcula o EQM do lote j e guarda em um vetor
    
    fit <- smooth.spline(dados$x[lotes != j], 
                         
                         dados$y[lotes != j], 
                         
                         df = resultados$gl[i], 
                         
                         all.knots = TRUE)
    
    
    eqm_aux[j] <- mean((dados$y[lotes == j] - predict(fit, dados$x[lotes == j])$y)^2)
    
    
  }
  
  
  resultados$eqm[i] <- mean(eqm_aux)
  
  # guarda a media do vetor com os EQM em resultados$eqm[i]
  
  
}





resultados %>% 
  
  arrange(eqm)



resultados %>% 
  
  ggplot(aes(gl, eqm)) +
  
  geom_point() +
  
  geom_line()





fit <- smooth.spline(dados$x, 
                     
                     dados$y, 
                     
                     df = 7, 
                     
                     all.knots = TRUE)


















