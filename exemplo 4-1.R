#Testando diretamente as verossimilhanças.
#?family familias que podem ser usadas e as funçoes de ligação padrão

# Conjunto de dados
data(trees)

# Ajuste do modelo completo e nulo
mod_gama_completo <- glm(Volume ~ Height + Girth, family = Gamma(link = "log"), data = trees)

mod_gama_nulo <- glm(Volume ~ 1, family = Gamma(link = "log"), data = trees)

# Extração das log-verossimilhanças
( l_gama_completo <- logLik(mod_gama_completo) )
#'log Lik.' -71.50404 (df=4)

( l_gama_nulo <- logLik(mod_gama_nulo) )
'log Lik.' -125.7405 (df=2)

# Estatística TRV
( T_rv <- as.numeric(2 * (l_gama_completo - l_gama_nulo)) )
#108.4729

# Cálculo do p-valor via distribuição Qui-Quadrado (df = p = 2)
( p_valor <- pchisq(T_rv, df = 2, lower.tail = FALSE) )
# 2.78873e-24


##Rejeita a hipotese de que a diferença não é signifcativa, existe diferença, ou seja, pelo menos uma das váriaveis consegue explicar a variável resposta. 



# Usando a função nativa ----

# Teste da razão de verossimilhanças usando a função
# lrtest do pacote lmtext
lmtest::lrtest(mod_gama_completo, mod_gama_nulo)

# p-valor exato
lmtest::lrtest(mod_gama_completo, mod_gama_nulo)$`Pr(>Chisq)`[2]
#2.78873e-24 < 5% #pelo menos uma variavel explica a variável resposta. 




