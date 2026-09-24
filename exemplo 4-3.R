
data("warpbreaks")

# Ajuste do modelo completo e nulo
mod_poisson_completo <- glm(breaks ~ wool + tension, family = poisson(link = "log"), data = warpbreaks)

mod_poisson_nulo <- glm(breaks ~ 1, family = poisson(link = "log"), data = warpbreaks)


# Extração das log-verossimilhanças
( l_poisson_completo <- logLik(mod_poisson_completo) )
( l_poisson_nulo <- logLik(mod_poisson_nulo) )


# Estatística TRV
( T_rv <- as.numeric(2 * (l_poisson_completo - l_poisson_nulo)) )


# Cálculo do p-valor via distribuição Qui-Quadrado (df = p = 2)
( p_valor <- pchisq(T_rv, df = 2, lower.tail = FALSE) )

#Ou

lmtest::lrtest(mod_poisson_completo, mod_poisson_nulo)
#< 2.2e-16 ***


