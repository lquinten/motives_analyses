install.packages("stepmixr")
library("stepmixr")

Y = haven::read_spss("C:/Users/gniel/OneDrive - MSB Medical School Berlin/Project special issue/LPA_analysis/data/data raw/dataE_clear5 2209.sav") %>% 
  filter(gender == 2) %>% select(starts_with("Y") & ends_with("_r"))


#class enumeration 

mm_stepm <- lapply(1:7, function(x){
  
  test_stepm <- stepmix(n_components = x,
                        measurement = "multinoulli", 
                        verbose=1)
  test_stepm.fit <- fit(test_stepm, 
                        X = data_males %>% select(-CASE) %>% lapply(as.integer) %>% as_tibble())
  return(list(model = test_stepm, fit = test_stepm.fit))
                        
})




test_stepm <- stepmix(n_components = 3,
                      n_steps=3, 
                      measurement = "multinoulli", 
                      correction = "ML",
                      structural = "binary",
                      verbose=1)

test_stepm.fit <- fit(test_stepm, 
                      X = data_males %>% select(-CASE) %>% lapply(as.integer) %>% as_tibble(),
                      Y = Y)
