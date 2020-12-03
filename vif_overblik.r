# Data ----------------------------------------------------------------------------------------
library(magrittr)

# Funktion ------------------------------------------------------------------------------------
# Nedenstående funktion tager en enkelt uevalueret model som input og konstruerer en tibble
# med modellen, antallet af parametre, VIF information og dummies der indikerer hvilke
# parametre der er med i modellen.
summarize_model = function(unevaluated_model) {
  
  mod = eval(unevaluated_model)
  vif = car::vif(mod)
  n_params = length(mod$coefficients)
  
  tbl = tibble::tibble(
    mod = list(mod), 
    n_params = n_params,
    max_vif = max(vif),
    max_vif_name = which.max(vif) %>% names()
  )
  
  coefs = matrix(rep(TRUE, n_params), nrow = 1)
  colnames(coefs) <- names(mod$coefficients)
  
  return(
    cbind(tbl, coefs) %>% 
      tibble::as_tibble()
  )
}

# Konstruktion af alle modeller ---------------------------------------------------------------
# Modellen der indeholder alle forklarende variable
global_model <- lm(Deaths ~ . -Country -Deaths , data = my.data, na.action = "na.fail")

# Tillader R at sprede komputation ud på flere kerner, for at forøge hastigheden.
future::plan(future::multiprocess()) 

# dredge konstruerer liste bestående af alle permutation af den globale model.
all_permutations <- MuMIn::dredge(
  global_model, 
  evaluate = FALSE, 
  m.lim = c(2, ncol(my.data)) # Grænser for antallet af parametre
  ) %>%
  furrr::future_map_dfr(   # Benytter funktionen på alle elementer i listen lavet af dredge.
    .f = summarize_model,  # Hvert output bliver koblet samme i en tibble (_dfr = data.frame).
    .progress = TRUE
  ) %>%
  dplyr::mutate_all(~ tidyr::replace_na(., FALSE)) 
  # Sidste funktion indsætter FALSE hvis en parameter ikke indgår i modellen.

# Behandling af output ------------------------------------------------------------------------

# Modellen der har den laveste maksimale VIF
all_permutations %>%
  dplyr::slice(which.min(max_vif)) %>%
  purrr::pluck("mod", 1)

# Modeller der overholder at VIF er under en tolerance, og indeholder x2 men ikke x3
tol <- 5
all_permutations %>%
  dplyr::filter(max_vif < tol )

# Modellerne med flest parametre, der stadig overholder VIF tolerancen. 
all_permutations %>%
  dplyr::filter(
    max_vif < tol,
    n_params == max(n_params)
  )

# glimpse() funktionen fra dplyr kan være brugbar til at se jeres tibble med mange variable.
mod <- all_permutations %>%
  dplyr::filter(
    max_vif < tol,
    n_params == max(n_params)
    ) %>%
  dplyr::filter(max_vif == min(max_vif)) %>%
  purrr::pluck("mod", 1)

# Pls slet mine kommentarer hvis i forstår koden. Det ser klamt ud (:
#Code belonging to "vif_overblik.r"
standres.mod <- rstandard(mod)
qqnorm(standres.mod); qqline(standres.mod, col = "red")
plot(predict(mod), standres.mod) #Trumpet shape, y is log-transformed
mod1 <- lm(log(Deaths) ~ Cancer + Diabetes + GDP + Obesity + Pop.density + Smokers, data = my.data)
res.mod1 <- rstandard(mod1)
plot(predict(mod1), res.mod1) #No more trumpet shape 
par(mfrow = c(3,3))
plot(my.data$Pop.density, res.mod1)
plot(my.data$GDP, res.mod1)
plot(my.data$Diabetes, res.mod1)
plot(my.data$Smokers, res.mod1)
plot(my.data$Obesity, res.mod1)
plot(my.data$Cancer, res.mod1) #Dependence between pop.density and the residuals, i.e. log-transform

mod2 <- lm(log(Deaths) ~ log(Pop.density) + GDP + Diabetes + Smokers + Obesity + log(Cancer),
           data = my.data)
res.mod2 <- rstandard(mod2)
plot(predict(mod2), res.mod2) #No trumpetshape 
qqnorm(res.mod2); qqline(res.mod2, col = "red")
par(mfrow = c(3,2))
plot(log(my.data$Pop.density), res.mod2)
plot(my.data$GDP, res.mod2)
plot(my.data$Diabetes, res.mod2)
plot(my.data$Smokers, res.mod2)
plot(my.data$Obesity, res.mod2)
plot(log(my.data$Cancer), res.mod1) #Dependence is no longer visible 
