getwd()

#packages needed
install.packages("tidyverse")
install.packages("lubridate")
install.packages("psych")


library(tidyverse)
library(lubridate)
library(readxl)
library(psych)
library(cNORM)

#calculating SDMT normative data

norms_macfims <- read_csv("normative_data/macfims_norms.csv", 
                          na = "NA", 
                          guess_max=Inf, 
)
spec(norms_macfims)

##add new dummy variables for regression
##education (two groups)
norms_macfims$edu_dummy_16  <- ifelse(norms_macfims$edu_years >= 16, 1, 0)
norms_macfims$edu_dummy_13  <- ifelse(norms_macfims$edu_years >= 13, 1, 0)
norms_macfims$sex_dummy <- ifelse(norms_macfims$sex=="Male", 1,0)

#norms SDMT
norms_sdmt <- norms_macfims %>% select(c(origin, record_id, age, sex, sex_dummy, edu_years, edu_dummy_13, edu_dummy_16, bdi_total, sdmt90_total)) %>% filter(!is.na(sdmt90_total))

##preparing dataset for the modeling
norms_sdmt <- rankBySlidingWindow(norms_sdmt, age = "age", raw = "sdmt90_total", width = 10, scale = "z", nGroup = 5, covariate = "edu_dummy_16")
norms_sdmt_powers <- computePowers(norms_sdmt, k = 4, norm = "normValue", age = "age", covariate = "edu_dummy_16")

###working on final model
norms_sdmt_model <- bestModel(norms_sdmt_powers, 
                              raw = "sdmt90_total", 
                              predictors = c(
                                "COV", "L1COV","A1COV","L1A1COV",
                                "L1", "L2", "L3","L4",
                                "A1", "A2", "A3","A4",
                                "L1A1", "L1A2", "L1A3","L1A4",
                                "L2A1", "L2A2","L2A3","L2A4", 
                                "L3A1", "L3A2", "L3A3","L3A4",
                                "L4A1", "L4A2", "L4A3","L4A4"
                              ),
                              plot = F,
                              terms = 4
)

#norms SDMT - withouteducation
norms_sdmt_noedu <- norms_macfims %>% select(c(origin, record_id, age, sex, sex_dummy, edu_years, edu_dummy_13, edu_dummy_16, bdi_total, sdmt90_total)) %>% filter(!is.na(sdmt90_total))

##preparing dataset for the modeling
norms_sdmt_noedu <- rankBySlidingWindow(norms_sdmt_noedu, age = "age", raw = "sdmt90_total", width = 10, scale = "z", nGroup = 5)
norms_sdmt_powers_noedu <- computePowers(norms_sdmt_noedu, k = 4, norm = "normValue", age = "age")

###working on final model
norms_sdmt_model_noedu <- bestModel(norms_sdmt_powers_noedu, 
                              raw = "sdmt90_total", 
                              predictors = c(
                                "L1", "L2", "L3","L4",
                                "A1", "A2", "A3","A4",
                                "L1A1", "L1A2", "L1A3","L1A4",
                                "L2A1", "L2A2","L2A3","L2A4", 
                                "L3A1", "L3A2", "L3A3","L3A4",
                                "L4A1", "L4A2", "L4A3","L4A4"
                              ),
                              plot = F,
                              terms = 4
)

remove(norms_macfims)
remove(norms_sdmt)
remove(norms_sdmt_powers)
remove(norms_sdmt_noedu)
remove(norms_sdmt_powers_noedu)


#main analysis
##loading data

hc_data_untidy <- read_csv("HC_data.csv", 
                            na = "", 
                            col_types= list(birthday = col_date("%Y-%m-%d"), psycho_date = col_date("%Y-%m-%d"))
)
spec(hc_data_untidy)

pac_data_final <- read_excel("Paraple_data.xlsx", 
                           na = "NA" 
                           )

spec(pac_data_final)


##tidy data
###copy basic demographics to all timepoints
hc_data_tidy <- 
  tibble(record_id = hc_data_untidy$record_id, 
         personal_id = hc_data_untidy$personal_id,
         firstname = hc_data_untidy$firstname,
         surname = hc_data_untidy$surname,
         surname_maiden = hc_data_untidy$surname_maiden,
         birthday = hc_data_untidy$birthday, 
         sex = hc_data_untidy$sex,
         sex_other = hc_data_untidy$sex_other,
         email = hc_data_untidy$email,
         phone_number = hc_data_untidy$phone_number,
         account_number = hc_data_untidy$account_number,
         later_hand = hc_data_untidy$later_hand,
         later_hand_other = hc_data_untidy$later_hand_other,
         spu = hc_data_untidy$spu,
         spu_type___1	= hc_data_untidy$spu_type___1,
         spu_type___2	= hc_data_untidy$spu_type___2,
         spu_type___3	= hc_data_untidy$spu_type___3,
         spu_type___4	= hc_data_untidy$spu_type___4,
         spu_type___5	= hc_data_untidy$spu_type___5,
         spu_type___6	= hc_data_untidy$spu_type___6,
         spu_type___7	= hc_data_untidy$spu_type___7,
         spu_type___8	= hc_data_untidy$spu_type___8,
         spu_type_note = hc_data_untidy$spu_type_note,
         psych_normative_yes = hc_data_untidy$psych_normative_yes,
         study___hcr = hc_data_untidy$study___hcr,
         study___gauk = hc_data_untidy$study___gauk,
         basic_demographic_data_complete = hc_data_untidy$basic_demographic_data_complete,
  ) %>%
  filter(is.na(hc_data_untidy$basic_demographic_data_complete)==F) %>%
  left_join(hc_data_untidy, by="record_id", keep = F)

hc_data_tidy <- select(hc_data_tidy, 
                        -personal_id.y,
                        -firstname.y,
                        -surname.y,
                        -surname_maiden.y,
                        -birthday.y, 
                        -sex.y,
                        -sex_other.y,
                        -email.y,
                        -phone_number.y,
                        -account_number.y,
                        -later_hand.y,
                        -later_hand_other.y,
                        -spu.y,
                        -spu_type___1.y,
                        -spu_type___2.y,
                        -spu_type___3.y,
                        -spu_type___4.y,
                        -spu_type___5.y,
                        -spu_type___6.y,
                        -spu_type___7.y,
                        -spu_type___8.y,
                        -spu_type_note.y,
                        -psych_normative_yes.y,
                        -study___hcr.y,
                        -study___gauk.y,
                        -basic_demographic_data_complete.y)
hc_data_tidy <- rename(hc_data_tidy, 
                        personal_id = personal_id.x,
                        firstname = firstname.x,
                        surname = surname.x,
                        surname_maiden = surname_maiden.x,
                        birthday = birthday.x, 
                        sex = sex.x,
                        sex_other = sex_other.x,
                        email = email.x,
                        phone_number = phone_number.x,
                        account_number = account_number.x,
                        later_hand = later_hand.x,
                        later_hand_other = later_hand_other.x,
                        spu = spu.x,
                        spu_type___1	= spu_type___1.x,
                        spu_type___2	= spu_type___2.x,
                        spu_type___3	= spu_type___3.x,
                        spu_type___4	= spu_type___4.x,
                        spu_type___5	= spu_type___5.x,
                        spu_type___6	= spu_type___6.x,
                        spu_type___7	= spu_type___7.x,
                        spu_type___8	= spu_type___8.x,
                        spu_type_note = spu_type_note.x,
                        psych_normative_yes = psych_normative_yes.x,
                        study___hcr = study___hcr.x,
                        study___gauk = study___gauk.x,
                        basic_demographic_data_complete = basic_demographic_data_complete.x)

###filtering only M36 in HCs, where the two fomrs of SDMT were tested
hc_data_final <- filter(hc_data_tidy, redcap_event_name == "m36_arm_1")
hc_data_m48 <- filter(hc_data_tidy, redcap_event_name == "m48_arm_1")


###calculating age of HCs
hc_data_final$age <- as.numeric(lubridate::time_length(difftime(hc_data_final$psycho_date, hc_data_final$birthday, units = "days"),"years"))

###calculate z-scores
hc_data_final$edu_dummy_16  <- ifelse(hc_data_final$edu_level != 0 & 
                                        hc_data_final$edu_level != 1 &
                                        hc_data_final$edu_level != 2 & 
                                        hc_data_final$edu_level != 3, 
                                      1, 0)
pac_data_final$edu_dummy_16  <- ifelse(pac_data_final$edu_level != 0 & 
                                         pac_data_final$edu_level != 1 & 
                                         pac_data_final$edu_level != 2 & 
                                         pac_data_final$edu_level != 3, 
                                       1, 0)

hc_data_final$sdmt90_z <- as.numeric(NA)
for (i in 1:nrow(hc_data_final)) {
    if(!is.na(hc_data_final$edu_dummy_16[[i]])) {
      hc_data_final$sdmt90_z[[i]] <- predictNorm(raw = hc_data_final$sdmt90_total[[i]], A = hc_data_final$age[[i]], model = norms_sdmt_model, minNorm = -5, maxNorm = 5, covariate = hc_data_final$edu_dummy_16[[i]])
    }
    if(is.na(hc_data_final$edu_dummy_16[[i]])) {
      hc_data_final$sdmt90_z[[i]] <- predictNorm(raw = hc_data_final$sdmt90_total[[i]], A = hc_data_final$age[[i]], model = norms_sdmt_model_noedu, minNorm = -5, maxNorm = 5)
    }
}

hc_data_final$sdmt90_hf_z <- as.numeric(NA)
for (i in 1:nrow(hc_data_final)) {
  if(!is.na(hc_data_final$edu_dummy_16[[i]])) {
    hc_data_final$sdmt90_hf_z[[i]] <- predictNorm(raw = hc_data_final$sdmt90_total_handsfree[[i]], A = hc_data_final$age[[i]], model = norms_sdmt_model, minNorm = -5, maxNorm = 5, covariate = hc_data_final$edu_dummy_16[[i]]) 
  }
  if(is.na(hc_data_final$edu_dummy_16[[i]])) {
    hc_data_final$sdmt90_hf_z[[i]] <- predictNorm(raw = hc_data_final$sdmt90_total_handsfree[[i]], A = hc_data_final$age[[i]], model = norms_sdmt_model_noedu, minNorm = -5, maxNorm = 5) 
  }
}


pac_data_final$sdmt90_hf_z <- as.numeric(NA)
for (i in 1:nrow(pac_data_final)) {
  if(!is.na(pac_data_final$edu_dummy_16[[i]])) {
    pac_data_final$sdmt90_hf_z[[i]] <- predictNorm(raw = pac_data_final$sdmt90_total_handsfree[[i]], A = pac_data_final$age[[i]], model = norms_sdmt_model, minNorm = -5, maxNorm = 5, covariate = pac_data_final$edu_dummy_16[[i]]) 
  }
  if(is.na(pac_data_final$edu_dummy_16[[i]])) {
    pac_data_final$sdmt90_hf_z[[i]] <- predictNorm(raw = pac_data_final$sdmt90_total_handsfree[[i]], A = pac_data_final$age[[i]], model = norms_sdmt_model_noedu, minNorm = -5, maxNorm = 5) 
  }
}




###filtering based on the version of SDMT
hc_data_sdmt_hf <- filter(hc_data_final, !is.na(sdmt90_total_handsfree))
hc_data_cogeval <- filter(hc_data_final, !is.na(cogeval_total))


###removing untidy data
remove(hc_data_tidy)
remove(hc_data_untidy)


##basic analysis
###descriptives
descr_hc <- describe(hc_data_final)
descr_sdmt_hf <- describe(hc_data_sdmt_hf)
descr_sdmt_cogeval <- describe(hc_data_cogeval)
descr_pac <- describe(pac_data_final)

descr_hc["age", "mean"]
descr_hc["age", "sd"]
descr_pac["age", "mean"]
descr_pac["age", "sd"]
descr_sdmt_hf["age", "mean"]
descr_sdmt_hf["age", "sd"]
descr_sdmt_cogeval["age", "mean"]
descr_sdmt_cogeval["age", "sd"]

descr_hc["sdmt90_z", "mean"]
descr_hc["sdmt90_z", "sd"]
descr_sdmt_hf["sdmt90_z", "mean"]
descr_sdmt_hf["sdmt90_z", "sd"]
descr_hc["sdmt90_hf_z", "mean"]
descr_hc["sdmt90_hf_z", "sd"]
descr_hc["cogeval_app_zscore", "mean"]
descr_hc["cogeval_app_zscore", "sd"]
descr_pac["sdmt90_hf_z", "mean"]
descr_pac["sdmt90_hf_z", "sd"]

t.test(hc_data_final$sdmt90_z,hc_data_final$sdmt90_hf_z, paired = T)
t.test(hc_data_final$sdmt90_z,hc_data_final$cogeval_app_zscore, paired = T)
t.test(pac_data_final$sdmt90_hf_z,hc_data_final$sdmt90_hf_z, paired = F)
corel_sdmt_hf_z <- corr.test(hc_data_final$sdmt90_z, hc_data_final$sdmt90_hf_z, use =  "pairwise")
corel_sdmt_hf_total <- corr.test(hc_data_final$sdmt90_total, hc_data_final$sdmt90_total_handsfree, use =  "pairwise")
corel_cogeval_z <- corr.test(hc_data_final$sdmt90_z, hc_data_final$cogeval_app_zscore, use =  "pairwise")
corel_cogeval_total <- corr.test(hc_data_final$sdmt90_total, hc_data_final$cogeval_total, use =  "pairwise")
corel_cogeval_within <- corr.test(hc_data_final$cogeval_app_zscore, hc_data_final$cogeval_total, use =  "pairwise")
corel_sdmt_within <- corr.test(hc_data_final$sdmt90_z, hc_data_final$sdmt90_total, use =  "pairwise")

plot(hc_data_final$sdmt90_z, hc_data_final$cogeval_app_zscore)
plot(hc_data_final$sdmt90_total, hc_data_final$cogeval_total)

plot(hc_data_final$sdmt90_z, hc_data_final$sdmt90_hf_z)
hist(hc_data_final$sdmt90_z)
hist(hc_data_final$sdmt90_hf_z)
hist(hc_data_final$cogeval_app_zscore)
hist(pac_data_final$sdmt90_hf_z)