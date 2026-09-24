library(MASS)

data("Pima.tr")


# Ajuste do modelo completo e nulo
mod_logistico_completo <- glm(type ~ glu + bmi + age, family = binomial(link = "logit"), data = Pima.tr)

mod_logistico_nulo <- glm(type ~ 1, family = binomial(link = "logit"), data = Pima.tr)

lmtest::lrtest(mod_logistico_completo, mod_logistico_nulo)




