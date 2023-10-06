# Customize geeglm function to handle different missing rows for each variable
my_geeglm <- function(formula, data, id, ...) {

  # capture id input (since it's unquoted)
  id <- rlang::enexpr(id)
  
  # keep compelte cases amoung the variables needed in the model
  data <-
    select(data, all_of(all.vars(formula)), !!id) %>%
    dplyr::filter(complete.cases(.))
  
  # build GEE model
  rlang::inject(geepack::geeglm(
    formula = formula,
    data = data,
    id = !!id,
    ...
  ))

}

# GEE multivariate function
geemulti <- function(vars) {
  dftb %>%
    select(lam0, h_code, all_of(vars)) %>%
    drop_na(any_of(c("lam0", "h_code", vars))) %>%
    geeglm(
      formula = vars %>%
        str_c(collapse = "+") %>%
        str_c("lam0 ~ ", .) %>%
        as.formula(),
      family = binomial,
      data = .,
      id = h_code,
      corstr = "independence"
    )
}
