#install.packages("gapminder")
library(gapminder)
?gapminder
library(tidyverse)

# a) Quantas observações há no total?

str(gapminder)

# 1.704 observações

# b) Para quais anos (year) há registros na base de dados? 

b <- gapminder %>% 
      group_by(year) %>% 
        summarise(n())

b[1]

# Para os seguintes anos:

# 1  1952
# 2  1957
# 3  1962
# 4  1967
# 5  1972
# 6  1977
# 7  1982
# 8  1987
# 9  1992
# 10  1997
# 11  2002
# 12  2007

# c) Quantas observações há para cada continente? 
  
gapminder %>% 
  group_by(continent) %>% 
    summarise(n())

# Aqui está a resposta:

# 1 Africa      624
# 2 Americas    300
# 3 Asia        396
# 4 Europe      360
# 5 Oceania      24

# d) Quantas observações há para cada continente em cada um dos anos considerados?
  
gapminder %>% 
  group_by(continent,year) %>% 
    summarise(n()) %>% 
      print(n=Inf)

# Aqui está a resposta:

# 1 Africa     1952    52
# 2 Africa     1957    52
# 3 Africa     1962    52
# 4 Africa     1967    52
# 5 Africa     1972    52
# 6 Africa     1977    52
# 7 Africa     1982    52
# 8 Africa     1987    52
# 9 Africa     1992    52
# 10 Africa     1997    52
# 11 Africa     2002    52
# 12 Africa     2007    52
# 13 Americas   1952    25
# 14 Americas   1957    25
# 15 Americas   1962    25
# 16 Americas   1967    25
# 17 Americas   1972    25
# 18 Americas   1977    25
# 19 Americas   1982    25
# 20 Americas   1987    25
# 21 Americas   1992    25
# 22 Americas   1997    25
# 23 Americas   2002    25
# 24 Americas   2007    25
# 25 Asia       1952    33
# 26 Asia       1957    33
# 27 Asia       1962    33
# 28 Asia       1967    33
# 29 Asia       1972    33
# 30 Asia       1977    33
# 31 Asia       1982    33
# 32 Asia       1987    33
# 33 Asia       1992    33
# 34 Asia       1997    33
# 35 Asia       2002    33
# 36 Asia       2007    33
# 37 Europe     1952    30
# 38 Europe     1957    30
# 39 Europe     1962    30
# 40 Europe     1967    30
# 41 Europe     1972    30
# 42 Europe     1977    30
# 43 Europe     1982    30
# 44 Europe     1987    30
# 45 Europe     1992    30
# 46 Europe     1997    30
# 47 Europe     2002    30
# 48 Europe     2007    30
# 49 Oceania    1952     2
# 50 Oceania    1957     2
# 51 Oceania    1962     2
# 52 Oceania    1967     2
# 53 Oceania    1972     2
# 54 Oceania    1977     2
# 55 Oceania    1982     2
# 56 Oceania    1987     2
# 57 Oceania    1992     2
# 58 Oceania    1997     2
# 59 Oceania    2002     2
# 60 Oceania    2007     2

# e) Quantas observações há para cada continente em cada um dos anos 
# considerados levando em conta apenas os dados a partir de 2000?
  
gapminder %>% 
  filter(year >= 2000) %>% 
    group_by(continent,year) %>% 
      summarise(n()) %>% 
        print(n=Inf)

# Aqui está a resposta:

# 1 Africa     2002    52
# 2 Africa     2007    52
# 3 Americas   2002    25
# 4 Americas   2007    25
# 5 Asia       2002    33
# 6 Asia       2007    33
# 7 Europe     2002    30
# 8 Europe     2007    30
# 9 Oceania    2002     2
# 10 Oceania    2007     2

# f) Qual a média e desvio-padrão do PIB per capita (gdpPercap) dos continentes de
# acordo com o ano levando em conta apenas dados a partir de 2000?
  
gapminder %>% 
  filter(year >= 2000) %>% 
    group_by(continent,year) %>% 
      summarise(
          média = mean(gdpPercap),
          desvio_padrão = sd(gdpPercap)) %>% 
        print(n=Inf)

# Aqui está a resposta:

#    continent  year  média desvio_padrão
#    <fct>     <int>  <dbl>         <dbl>
#  1 Africa     2002  2599.         2973.
#  2 Africa     2007  3089.         3618.
#  3 Americas   2002  9288.         8896.
#  4 Americas   2007 11003.         9713.
#  5 Asia       2002 10174.        11151.
#  6 Asia       2007 12473.        14155.
#  7 Europe     2002 21712.        11197.
#  8 Europe     2007 25054.        11800.
#  9 Oceania    2002 26939.         5302.
# 10 Oceania    2007 29810.         6541.

# g) Faça o gráfico de linhas de expectativa de vida (lifeExp) de acordo com o ano (year)
# para os países (country).

gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_line()  + 
  guides(color="none")
  

# h) Obtenha a expectativa de vida média por continente ao longo dos anos. 

gapminder %>% 
  group_by(continent,year) %>% 
    summarise(
        média = mean(lifeExp)) %>% 
      print(n=Inf)
      
# Aqui está a resposta:

#    continent  year média
#    <fct>     <int> <dbl>
#  1 Africa     1952  39.1
#  2 Africa     1957  41.3
#  3 Africa     1962  43.3
#  4 Africa     1967  45.3
#  5 Africa     1972  47.5
#  6 Africa     1977  49.6
#  7 Africa     1982  51.6
#  8 Africa     1987  53.3
#  9 Africa     1992  53.6
# 10 Africa     1997  53.6
# 11 Africa     2002  53.3
# 12 Africa     2007  54.8
# 13 Americas   1952  53.3
# 14 Americas   1957  56.0
# 15 Americas   1962  58.4
# 16 Americas   1967  60.4
# 17 Americas   1972  62.4
# 18 Americas   1977  64.4
# 19 Americas   1982  66.2
# 20 Americas   1987  68.1
# 21 Americas   1992  69.6
# 22 Americas   1997  71.2
# 23 Americas   2002  72.4
# 24 Americas   2007  73.6
# 25 Asia       1952  46.3
# 26 Asia       1957  49.3
# 27 Asia       1962  51.6
# 28 Asia       1967  54.7
# 29 Asia       1972  57.3
# 30 Asia       1977  59.6
# 31 Asia       1982  62.6
# 32 Asia       1987  64.9
# 33 Asia       1992  66.5
# 34 Asia       1997  68.0
# 35 Asia       2002  69.2
# 36 Asia       2007  70.7
# 37 Europe     1952  64.4
# 38 Europe     1957  66.7
# 39 Europe     1962  68.5
# 40 Europe     1967  69.7
# 41 Europe     1972  70.8
# 42 Europe     1977  71.9
# 43 Europe     1982  72.8
# 44 Europe     1987  73.6
# 45 Europe     1992  74.4
# 46 Europe     1997  75.5
# 47 Europe     2002  76.7
# 48 Europe     2007  77.6
# 49 Oceania    1952  69.3
# 50 Oceania    1957  70.3
# 51 Oceania    1962  71.1
# 52 Oceania    1967  71.3
# 53 Oceania    1972  71.9
# 54 Oceania    1977  72.9
# 55 Oceania    1982  74.3
# 56 Oceania    1987  75.3
# 57 Oceania    1992  76.9
# 58 Oceania    1997  78.2
# 59 Oceania    2002  79.7
# 60 Oceania    2007  80.7

# i) Faça um gráfico de linhas de expectativa de vida média ao longo dos anos para os
# continentes.

gapminder %>% 
  group_by(continent,year) %>% 
    summarise(
        média = mean(lifeExp)) %>% 
      ggplot(aes(x = year, y = média, color = continent)) +
      geom_line() +
      geom_point()

                