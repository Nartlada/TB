# Backwards step 0 - include all variables that are significant univariately at p < 0.2
step0 <- geemulti(
  c(
    "sex",
    "nationgrp",
    "cd4grp",
    "fever",
    "weight_loss",
    "has_oi",
    "patient_type",
    "cxr",
    "site"
  )
)
summary(step0)
wald.test(Sigma=vcov(step0),b=coef(step0),Terms=2)
wald.test(Sigma=vcov(step0),b=coef(step0),Terms=3)
wald.test(Sigma=vcov(step0),b=coef(step0),Terms=4:5)
wald.test(Sigma=vcov(step0),b=coef(step0),Terms=6)
wald.test(Sigma=vcov(step0),b=coef(step0),Terms=7)
wald.test(Sigma=vcov(step0),b=coef(step0),Terms=8)
wald.test(Sigma=vcov(step0),b=coef(step0),Terms=9)
wald.test(Sigma=vcov(step0),b=coef(step0),Terms=10)
wald.test(Sigma=vcov(step0),b=coef(step0),Terms=11)

# Backwards step 1 - drop site
step1 <- geemulti(
  c(
    "sex",
    "nationgrp",
    "cd4grp",
    "fever",
    "weight_loss",
    "has_oi",
    "patient_type",
    "cxr"
  )
)
summary(step1)
wald.test(Sigma=vcov(step1),b=coef(step1),Terms=2)
wald.test(Sigma=vcov(step1),b=coef(step1),Terms=3)
wald.test(Sigma=vcov(step1),b=coef(step1),Terms=4:5)
wald.test(Sigma=vcov(step1),b=coef(step1),Terms=6)
wald.test(Sigma=vcov(step1),b=coef(step1),Terms=7)
wald.test(Sigma=vcov(step1),b=coef(step1),Terms=8)
wald.test(Sigma=vcov(step1),b=coef(step1),Terms=9)
wald.test(Sigma=vcov(step1),b=coef(step1),Terms=10)

# Backwards step 2 - drop fever
step2 <- geemulti(
  c(
    "sex",
    "nationgrp",
    "cd4grp",
    "weight_loss",
    "has_oi",
    "patient_type",
    "cxr"
  )
)
summary(step2)
wald.test(Sigma=vcov(step2),b=coef(step2),Terms=2)
wald.test(Sigma=vcov(step2),b=coef(step2),Terms=3)
wald.test(Sigma=vcov(step2),b=coef(step2),Terms=4:5)
wald.test(Sigma=vcov(step2),b=coef(step2),Terms=6)
wald.test(Sigma=vcov(step2),b=coef(step2),Terms=7)
wald.test(Sigma=vcov(step2),b=coef(step2),Terms=8)
wald.test(Sigma=vcov(step2),b=coef(step2),Terms=9)

# Backwards step 3 - drop nationgrp
step3 <- geemulti(
  c(
    "sex",
    "cd4grp",
    "weight_loss",
    "has_oi",
    "patient_type",
    "cxr"
  )
)
summary(step3)
wald.test(Sigma=vcov(step3),b=coef(step3),Terms=2)
wald.test(Sigma=vcov(step3),b=coef(step3),Terms=3:4)
wald.test(Sigma=vcov(step3),b=coef(step3),Terms=5)
wald.test(Sigma=vcov(step3),b=coef(step3),Terms=6)
wald.test(Sigma=vcov(step3),b=coef(step3),Terms=7)
wald.test(Sigma=vcov(step3),b=coef(step3),Terms=8)
