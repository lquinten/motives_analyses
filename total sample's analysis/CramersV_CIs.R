#load libraries and packages
library(tidyverse)
library(confintr)
d <- haven::read_spss("./data/dataE_clear5 2209.sav") %>% 
  dplyr::select(CASE, gender, educ, work, sexOrien, single, dating, notLT, rel_LT, engaged, married, 
                divorced, widowed)
#exclude genders other than male & female
d <- d %>% filter(gender == 1 | gender == 2)
#calculate CIs for Cramer's V per variable
chisq.test(d %>% pull(gender), d %>% pull(educ)) %>% ci_cramersv(type="bootstrap")
chisq.test(d %>% pull(gender), d %>% pull(work)) %>% ci_cramersv(type="bootstrap")
chisq.test(d %>% pull(gender), d %>% pull(sexOrien)) %>% ci_cramersv(type="bootstrap")
chisq.test(d %>% pull(gender), d %>% pull(single)) %>% ci_cramersv(type="bootstrap")
chisq.test(d %>% pull(gender), d %>% pull(dating)) %>% ci_cramersv(type="bootstrap")
chisq.test(d %>% pull(gender), d %>% pull(notLT)) %>% ci_cramersv(type="bootstrap")
chisq.test(d %>% pull(gender), d %>% pull(rel_LT)) %>% ci_cramersv(type="bootstrap")
chisq.test(d %>% pull(gender), d %>% pull(engaged)) %>% ci_cramersv(type="bootstrap")
chisq.test(d %>% pull(gender), d %>% pull(married)) %>% ci_cramersv(type="bootstrap")
chisq.test(d %>% pull(gender), d %>% pull(divorced)) %>% ci_cramersv(type="bootstrap")
chisq.test(d %>% pull(gender), d %>% pull(widowed)) %>% ci_cramersv(type="bootstrap")