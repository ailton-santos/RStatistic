# Definição dos dados
n <- 2000 # tamanho da amostra
p1 <- 0.2 # proporção que fará o vestibular
p2 <- 0.3 # proporção que realizará o ensino técnico primeiro
p3 <- 0.1 # proporção que não prosseguirá com os estudos
p0 <- 0.6 # proporção hipotética
tempo_max <- 4 # tempo máximo em anos

# Cálculo da proporção amostral
x <- 1400 # número de alunos que completaram o curso com sucesso até o tempo_max
prop_am <- x / n

# Cálculo da função de sobrevivência
S <- function(t) (1 - p1 - p2 - p3)^(n * (1 - prop_am) * t)

# Estimativa da proporção de alunos que completarão o curso com sucesso até um tempo grande
P <- S(Inf)

# Intervalo de confiança para P
z_alpha <- qnorm(1 - alpha/2)
erro_padrao <- sqrt(P * (1 - P) / n)
intervalo_conf <- P + c(-1, 1) * z_alpha * erro_padrao

# Imprimir resultados
cat("Proporção amostral de alunos que completaram o curso com sucesso até", tempo_max, "anos:", prop_am, "\n")
cat("Estimativa da proporção de alunos que completarão o curso com sucesso:", P, "\n")
cat("Intervalo de confiança para a proporção de alunos que completarão o curso com sucesso:", intervalo_conf, "\n")

# Gráfico de tendência
t <- seq(0, tempo_max, by = 0.1)
prob_t <- S(t)
plot(t, prob_t, type = "l", xlab = "Tempo (anos)", ylab = "Probabilidade de sucesso")
