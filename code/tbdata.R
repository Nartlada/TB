# Load required packages
pacman::p_load(
  rio,
  janitor,
  labelled
)

dftb <- 
  
  # Import & merge all excel files
  import(here('data', 'table1.xlsx')) %>%
  select(-c(Remark:updated_at)) %>%
  mutate(id = as.numeric(id)) %>%
  left_join(import(here('data', 'table2.xlsx')) %>%
              select(-c(id, Remark:updated_at), id = patient_id),
            by = 'id') %>%
  left_join(import(here('data', 'table3.xlsx')) %>%
              select(-c(id, Remark:updated_at), id = patient_id), 
            by = 'id') %>%
  left_join(import(here('data', 'table4.xlsx')) %>%
              select(-c(id, Remark:updated_at), id = patient_id), 
            by = 'id') %>%
  left_join(import(here('data', 'table5.xlsx')) %>%
              select(-c(id, Remark:updated_at), id = patient_id), 
            by = 'id') %>%
  left_join(import(here('data', 'table6.xlsx')) %>%
              select(-c(id, Remark:updated_at), id = patient_id), 
            by = 'id') %>%
  left_join(import(here('data', 'table7.xlsx')) %>%
              select(-c(id, created_at:isDeleted), id = patient_id) %>%
              group_by(id) %>%
              mutate(visit = row_number()) %>%
              ungroup() %>%
              pivot_wider(
                names_from = visit,
                values_from = VirologyCheck:ViralLoad,
                names_vary = 'slowest'), 
            by = 'id') %>%
  left_join(import(here('data', 'table8.xlsx')) %>%
              select(-c(id, Remark:updated_at), id = patient_id), 
            by = 'id') %>%
  left_join(import(here('data', 'table843.xlsx')) %>%
              select(-c(id, Remark:updated_at), id = patient_id), 
            by = 'id') %>%
  left_join(import(here('data', 'table9.xlsx')) %>%
              select(-c(id, Remark:updated_at), id = patient_id), 
            by = 'id') %>%
  left_join(import(here('data', 'table10.xlsx')) %>%
              select(-c(id, Remark:updated_at), id = patient_id), 
            by = 'id') %>%
  left_join(import(here('data', 'table11.xlsx')) %>%
              select(-c(id, Remark:updated_at), id = patient_id), 
            by = 'id') %>%
  
  # Simplify variable names
  clean_names() %>%
  
  # Sort by h_code
  arrange(h_code) %>% 
  
  # Create key variables (tb_definition, tb_diag, tb, lam, ageatvisit)
  mutate(
    culturepos = case_when(
      tb_culture_check_result1 == 2 ~ 1,
      tb_culture_check_result2 == 2 ~ 1,
      tb_culture_check_result3 == 2 ~ 1,
      tb_culture_check_result4 == 2 ~ 1,
      tb_culture_check_result5 == 2 ~ 1,
      TRUE ~ 0
    ),
    xpertpos = case_when(
      xpert_check_mtb_result1 == 1 ~ 1,
      xpert_check_mtb_result2 == 1 ~ 1,
      xpert_check_mtb_result3 == 1 ~ 1,
      xpert_check_mtb_result4 == 1 ~ 1,
      xpert_check_mtb_result5 == 1 ~ 1,
      xpert_check_rif_result1 == 1 ~ 1,
      xpert_check_rif_result2 == 1 ~ 1,
      xpert_check_rif_result3 == 1 ~ 1,
      xpert_check_rif_result4 == 1 ~ 1,
      xpert_check_rif_result5 == 1 ~ 1,
      TRUE ~ 0
    ),
    symptom = case_when(
      criteria_2_1_1 == 1 ~ 1,
      criteria_2_1_2 == 1 ~ 1,
      criteria_2_1_3 == 1 ~ 1,
      criteria_2_1_4 == 1 ~ 1,
      criteria_2_1_5 == 1 ~ 1,
      TRUE ~ 0
    ),
    afbpos = case_when(
      afb_check_result1 == 2 ~ 1,
      afb_check_result2 == 2 ~ 1,
      afb_check_result3 == 2 ~ 1,
      afb_check_result4 == 2 ~ 1,
      afb_check_result5 == 2 ~ 1,
      TRUE ~ 0
    ),
    cxr = case_when(
      cxr_check_result == 2 ~ 1,
      TRUE ~ 0),
    ct = case_when(
      ct_check_result == 2 ~ 1,
      TRUE ~ 0),
    tbtrt = case_when(
      antituberculosis_drugs_receive == 1 & tb_diagg_result != 5 ~ 1,
      TRUE ~ 0
    ),
    tb_definition = case_when(
      # Definite TB
      culturepos == 1 | xpertpos == 1                                ~ 1,
      # Probable TB
      (symptom == 1 | afbpos == 1 | cxr == 1 | ct == 1) & tbtrt == 1 ~ 2,
      # Not TB
      TRUE                                                           ~ 0 
    ),
    tb_diag = case_when(
      pulmonary_tb == 1 | extra_pulmonary_tb == 1 | disseminated_tb == 1 ~ 1,
      non_tb == 1                                                        ~ 0,
      TRUE                                                               ~ NA_real_
    ),
    tb = factor(
      case_when(
        tb_definition %in% 1:2 ~ 1,
        TRUE                   ~ 0
      ),
      levels = c(1, 0),
      labels = c('Positive', 'Negative')
    ),
    lam = factor(
      active_tb_alere_determine_test_result,
      levels = c(2, 1),
      labels = c('Positive', 'Negative')
    ),
    lam0 = active_tb_alere_determine_test_result - 1,
    across(where(is.POSIXct), ~ as.Date(.)),
    ageatvisit = as.numeric(difftime(service_date, dob, units = "days")) / 365.25
  ) %>% 
  
  # Exclude conflicts between definition & diagnosis
  filter((tb_definition %in% 1:2 & tb_diag == 1) | 
         (tb_definition == 0 & tb_diag == 0) |
         (is.na(tb_diag))) %>%

  # Include only age >= 15
  filter(ageatvisit >= 15) %>%
  # Exclude missing lam 
  filter(!is.na(lam)) %>%
  # Exclude incomplete routine TB diagnosis
  filter(!is.na(tb_diag)) %>%
  # Exclude incomplete treatment information
  filter((!is.na(antituberculosis_drugs_receive) | !tb_diag == 1)) %>% 
  # Exclude non-standard LF-LAM testing
  filter(active_tb_alere_determine_test_result %in% 1:2) %>% 
  # Include only TB positive
  filter(tb == 'Positive') %>% 

  # Factors associated with LF-LAM positivity
  mutate(
    sex = factor(
      sex,
      levels = c(1, 2),
      labels = c('Male', 'Female')
    ),
    agegrp = factor(
      case_when(
        ageatvisit < 30                     ~ 1,
        ageatvisit >= 30 & ageatvisit <= 50 ~ 2,
        ageatvisit > 50                     ~ 3,
        TRUE                                ~ NA_real_
      ),
      levels = c(1, 2, 3),
      labels = c('< 30 years', '30-50 years', '> 50 years')
    ),
    nationgrp = factor(
      case_when(
        nation == 1                ~ 1,
        nation %in% c(2, 3, 4, 99) ~ 2,
        TRUE                       ~ NA_real_
      ),      
      levels = c(1, 2),
      labels = c('Thai', 'Non-Thai')
    ),
    cd4grp = factor(
      case_when(
        absolute_cd4 < 100              ~ 1,
        between(absolute_cd4, 101, 200) ~ 2,
        absolute_cd4 > 200              ~ 3,
        TRUE                            ~ NA_real_
      ),
      levels = c(1, 2, 3),
      labels = c('Less than 100', '101-200', 'More than 200')
    ),
    fever = factor(
      criteria_2_1_1,
      levels = c(0, 1),
      labels = c('No', 'Yes')
    ),
    sweat = factor(
      criteria_2_1_2,
      levels = c(0, 1),
      labels = c('No', 'Yes')
    ),
    cough = factor(
      criteria_2_1_3,
      levels = c(0, 1),
      labels = c('No', 'Yes')
    ),
    hemoptysis = factor(
      criteria_2_1_4,
      levels = c(0, 1),
      labels = c('No', 'Yes')
    ),
    weight_loss = factor(
      criteria_2_1_5,
      levels = c(0, 1),
      labels = c('No', 'Yes')
    ),
    has_oi = factor(
      has_oi,
      levels = c(0, 1),
      labels = c('No', 'Yes')
    ),
    patient_type = factor(
      patient_type,
      levels = c(1, 2),
      labels = c('IPD', 'OPD')
    ),
    cxr = factor(
      cxr,
      levels = c(0, 1),
      labels = c('Normal', 'Abnormal')
    ),
    site = factor(
      case_when(
        disseminated_tb == 1 ~ 2,
        TRUE                 ~ 1
      ),
      levels = c(2, 1),
      labels = c('Disseminated TB', 'Localized TB')
    )
  ) %>% 

  # Variable labels
  set_variable_labels(
    lam          = "LF_LAM",
    sex          = "Sex",
    agegrp       = "Age",
    nationgrp    = "Nationality",
    cd4grp       = "CD4 (cell/μL)",
    fever        = "Fever",
    sweat        = "Night sweating",
    cough        = "Chronic cough",
    hemoptysis   = "Hemoptysis",
    weight_loss  = "Weight loss",
    has_oi       = "Previous OI",
    patient_type = "Patient type at visit",
    cxr          = "Chest radiography",
    site         = "Anatomical site of disease"
  )
