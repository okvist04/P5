# Data ----------------------------------------------------------------------------------------
library(magrittr)
data <- tibble::tibble(
  y = rnorm(100),
  x1 = rnorm(100),
  x2 = runif(100),
  x3 = x1 - rnorm(100),
  x4 = x2 + rnorm(100),
  to_remove = rnorm(100)
)

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
global_model <- lm(y ~ . -to_remove , data = data, na.action = "na.fail")

# Tillader R at sprede komputation ud på flere kerner, for at forøge hastigheden.
future::plan(future::multiprocess()) 

# dredge konstruerer liste bestående af alle permutation af den globale model.
all_permutations <- MuMIn::dredge(
  global_model, 
  evaluate = FALSE, 
  m.lim = c(2, ncol(data)) # Grænser for antallet af parametre
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
tol <- 1.1
all_permutations %>%
  dplyr::filter(max_vif < tol, x2, !x3 )

# Modellerne med flest parametre, der stadig overholder VIF tolerancen. 
all_permutations %>%
  dplyr::filter(
    max_vif < tol,
    which.max(n_params)
  )

# glimpse() funktionen fra dplyr kan være brugbar til at se jeres tibble med mange variable.
all_permutations %>%
  dplyr::filter(
    max_vif < tol,
    which.max(n_params)
  ) %>%
  dplyr::glimpse()

# Pls slet mine kommentarer hvis i forstår koden. Det ser klamt ud (:
