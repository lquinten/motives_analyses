setwd("C:/Users/Rebecca/Documents/GitHub/LPA_analysis")
library(tidyverse)
library(lavaan)
dat <- haven::read_spss(
  "C:/Users/Rebecca/Documents/GitHub/LPA_analysis/data/data raw/dataE_clear5 2209.sav") %>% select(CASE, starts_with("Y")) %>% 
  select(CASE, ends_with("r"))

dat[, c("Y_proclSpeed_r", "Y_proclDui_r", "Y_proclRob_r", "Y_proclKill_r", "Y_proclProstitute_r", 
        "Y_proclRape_r", "Y_proclZoo_r", "Y_proclPublic_r", "Y_proclPornChild_r", 
        "Y_proclPornChildDarknet_r", "Y_proclChatChild_r", "Y_proclGiftChild_r", "Y_proclSexChild_r", 
        "Y_proclPornYp_r", "Y_proclChatYp_r", "Y_proclGiftYp_r", "Y_proclSexYp_r")] <- 
  lapply(dat[, c("Y_proclSpeed_r", "Y_proclDui_r", "Y_proclRob_r", "Y_proclKill_r", "Y_proclProstitute_r", 
                 "Y_proclRape_r", "Y_proclZoo_r", "Y_proclPublic_r", "Y_proclPornChild_r", 
                 "Y_proclPornChildDarknet_r", "Y_proclChatChild_r", "Y_proclGiftChild_r", "Y_proclSexChild_r", 
                 "Y_proclPornYp_r", "Y_proclChatYp_r", "Y_proclGiftYp_r", "Y_proclSexYp_r")], ordered)

mod <- "
child =~ Y_proclPornChild_r  + Y_proclChatChild_r
+ Y_proclGiftChild_r
YP =~ Y_proclPornYp_r + Y_proclChatYp_r + Y_proclGiftYp_r + Y_proclSexYp_r

sex =~ child + YP + Y_proclRape_r + Y_proclZoo_r + Y_proclPublic_r + Y_proclProstitute_r+ Y_proclSexChild_r

child ~~ 0*YP

sex ~~ 1*sex
child ~~0.001*child
"

fit_mod <- sem(mod, dat, ordered=c("Y_proclSpeed_r", "Y_proclDui_r", "Y_proclRob_r", "Y_proclKill_r", "Y_proclProstitute_r", 
                                         "Y_proclRape_r", "Y_proclZoo_r", "Y_proclPublic_r", "Y_proclPornChild_r", 
                                         "Y_proclPornChildDarknet_r", "Y_proclChatChild_r", "Y_proclGiftChild_r", "Y_proclSexChild_r", 
                                         "Y_proclPornYp_r", "Y_proclChatYp_r", "Y_proclGiftYp_r", "Y_proclSexYp_r"))
summary(fit_mod, fit.measures=T, standardized=T)




