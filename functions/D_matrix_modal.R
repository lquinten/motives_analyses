#The function needs as an input a dataframe with posterior class predictions and a column called predicted in which each modal class prediction can be found.

D_matrix_modal <- function(posteriors){
  classes <- posteriors$predicted %>% unique() %>% sort() #vector with classes
  
  COLSUMS <- lapply(classes, function(x){posteriors %>% filter(predicted==x) %>% select(-"predicted") %>% colSums()}) %>% unlist()
  COLSUMS <- matrix(COLSUMS, ncol=length(classes), nrow=length(classes), byrow=F, dimnames = list( paste("class", classes, sep="."), paste("assigned_class", classes, sep=".")))
  
  ROWSUMS <- 1/rowSums(COLSUMS)
  ROWSUMS <- matrix(vctrs::vec_rep(ROWSUMS, times=length(classes)),
                    ncol = length(classes),
                    byrow=FALSE)
  
  D <- COLSUMS*ROWSUMS
  return(D)
}