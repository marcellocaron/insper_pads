#install.packages("tidyverse")
library("tidyverse")

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