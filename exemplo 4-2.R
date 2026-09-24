library(MASS)

data(birthwt)

# Ajuste do modelo completo e nulo
mod_logistico_completo <- glm(low ~ lwt + smoke, family = binomial(link = "logit"), data = birthwt)

mod_logistico_nulo <- glm(low ~ 1, family = binomial(link = "logit"), data = birthwt)


# Extração das log-verossimilhanças
( l_logistico_completo <- logLik(mod_logistico_completo) )
( l_logistico_nulo <- logLik(mod_logistico_nulo) )


# Estatística TRV
( T_rv <- as.numeric(2 * (l_logistico_completo - l_logistico_nulo)) )


# Cálculo do p-valor via distribuição Qui-Quadrado (df = p = 2)
( p_valor <- pchisq(T_rv, df = 2, lower.tail = FALSE) )
#0.005709221 < 0,05, logo existe diferença, pelo menos uma variável consegue explicar a variável resposta.  


#Ou

lmtest::lrtest(mod_logistico_completo, mod_logistico_nulo)



