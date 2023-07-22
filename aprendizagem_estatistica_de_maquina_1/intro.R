#install.packages("tidyverse")
library("tidyverse") # conjunto de libs, tipo anaconda

dados <- read_csv2("https://raw.githubusercontent.com/tiagomendonca/educacao/main/estado.csv")

head(dados)

dados %>% 
  ggplot(aes(x = ano, y = nota_matematica)) + 
  geom_point()

dados %>% 
  ggplot(aes(x = ano, y = nota_matematica, color = regiao)) + 
  geom_point()


dados %>% 
  ggplot(aes(x = ano, y = nota_matematica, group = estado, color = regiao)) + 
  geom_point() + geom_line()

head(mtcars)
?mtcars

summary(mtcars)

class(mtcars)

str(mtcars)

vetor1 <- c(1,2,3)
vetor1

1:10

vetor1[1]

tail(vetor1, n=1)

vetor1[c(3)]
vetor1[c(2,3)]

vetor1 + 1

vetor2 <- c(3,4,5)

vetor1 + vetor2

a <- TRUE
b <- FALSE

a == b

a == TRUE

a != TRUE

3 %in% c(1,2,3)

minha_coluna <- c(1,3,0,10)

minha_coluna[minha_coluna > 3]

which(minha_coluna > 3)

mtcars$mpg

dim(mtcars)

mtcars[2,3]

mtcars[,1]

mtcars[1,]

mtcars[, c(1,2)]

mtcars[, c("mpg","am")]

mtcars$cyl == 4

mtcars[mtcars$cyl == 4, ] # WHERE do SQL

sum(1,3)

sum(c(1,3))

mean(1,3) # incorreto

mean(c(1,3)) # correto

seq(from=4, to=10, by=2)

seq(4,10,2)

tail(mtcars)

names(mtcars)

cbind() # append colunas
rbind() # append linhas


minha_soma <- function(x, y) {
  soma <- x + y
  
  soma
}
  
  
minha_soma(1,2)

