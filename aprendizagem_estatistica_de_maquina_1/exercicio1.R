vetor <- c(1:10)
vetor

length(vetor)

mean(vetor) # 5.5

sd(vetor) # 3.02765

length(vetor[vetor > 5]) # 5

# Dados de filmes
titulo <- c("Barbie", "Matrix", "O Senhor dos Anéis", "Interestelar", "Pantera Negra")
genero <- c("Animação", "Ficção Científica", "Fantasia", "Ficção Científica", "Ação")
ano_lancamento <- c(2023, 1999, 2001, 2014, 2018)
avaliacao <- c(7.5, 8.7, 9.0, 8.6, 7.3)

# Crie o DataFrame usando a função data.frame
cinema_df <- data.frame(Titulo = titulo,
                        Genero = genero,
                        Ano_de_Lancamento = ano_lancamento,
                        Avaliacao = avaliacao)

# Visualizar o DataFrame
print(cinema_df)

class(cinema_df)


mean(cinema_df$Avaliacao) # 8.22

min(cinema_df$Ano_de_Lancamento) # 1999

max(cinema_df$Ano_de_Lancamento) # 2023

n_distinct(cinema_df$Genero) # 4

# ou

table(cinema_df$Genero)

teste <- cinema_df %>% 
  group_by(Genero) %>% 
    summarise(mean_avaliacao = mean(Avaliacao), 
              std_avaliacao = replace_na(sd(Avaliacao),0),
              count = n()
              ) %>% 
      arrange(desc(mean_avaliacao))

# 1 Fantasia                    9   
# 2 Ficção Científica           8.65
# 3 Animação                    7.5 
# 4 Ação                        7.3 

class(teste)

?group_by