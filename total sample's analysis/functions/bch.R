#We need a different file compared to the one below. 
#But we can extract it from our model objects and I believe it should look pretty much the same. 

posteriors <- class_prob(lca_final_model_m, type="individual")$individual %>% as_tibble()
  

#creating the weights needed for the D matrix calculation
n<-nrow(posteriors) # the length of the data file
n_class <- ncol(posteriors)-1 #number of classes in the lca solution
ctm<-matrix(nrow=n,ncol=n_class)#modal weights 3*3 class
ctp<-matrix(nrow=n,ncol=n_class)#proportional weights 3*3 class
modal<-matrix(nrow=n, ncol=n_class) #modal class assignment 3 dummies for 3 classes
for (j in 1:n_class) # creating dummies for modal posterior class assignment
{
  modal[,j]<- ifelse (posteriors$predicted==j, 1, 0)
}
# obtaining the elements of the D matrix
ctm <- (posteriors %>% select(-predicted) %>% as.matrix())*modal

combined<-cbind(modal, ctm) #collecting all weights to a matrix
COLSUMS<- matrix(c(apply(combined,2,sum)),
                 1,21) #summing all the weights
# creating the modal D matrix
m <- 
DIM<- solve(matrix(c((matrix(c(COLSUMS[,4:12]),3,3,byrow=F))/apply(matrix(c(COLSUMS[,4:12]),
                                                                   3,3,byrow=F),1,sum)),3,3))



#final modal bch weights applied to each case i
wm1<- ((combined[,1]*DIM[1,1]) + ( combined[,2]
                                   *DIM[2,1]) + (combined[,3]*DIM[3,1]))
wm2<- ((combined[,1]*DIM[1,2]) + (combined[,2]
                                  *DIM[2,2]) + (combined[,3]*DIM[3,2]))
wm3<- ((combined[,1]*DIM[1,3]) + ( combined[,2]
                                   *DIM[2,3]) + (combined[,3]*DIM[3,3]))


#create and save long file
class_longa<-data.frame (classa[,1:8],
                         wmodal1=combined[,1], wmodal2=combined[,2],
                         wmodal3=combined[,3], wprop1=classa[,9],
                         wprop2= classa[,10],wprop3=classa[,11],
                         wbchmodal1=wm1, wbchmodal2=wm2,
                         bchmodal3=wm3,wbchprop1=wp1,
                         wbchprop2=wp2, wbchprop3=wp3)
library(Hmisc)
class_long<- reShape(class_longa, base=c("wmodal","wprop","wbchmodal","wbchprop"),reps=3)