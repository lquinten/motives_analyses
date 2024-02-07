#function for computation of modal weights
modal_weights <- function(modal_assignments, inverse_D){
  #Create empty matrix
  nclass=modal_assignments %>% unique() %>% nrow()
  modal<-matrix(nrow=nrow(modal_assignments), ncol=nclass) #modal class assignment 3 dummies for 3 classes
  
  #Fill in the respective row of the inverse D-matrix for each participant.
  for (i in 1:nrow(modal_assignments)){
    
    #modal class assignment per case i
    mclass <- modal_assignments$predicted[i]
    
    modal[i,]<-inverse_D[mclass,]
  }
  #Give columns meaningfull names
  colnames(modal)<- paste("class", 1:nclass, sep=".")
  
  #Return Object
  return(modal)
}