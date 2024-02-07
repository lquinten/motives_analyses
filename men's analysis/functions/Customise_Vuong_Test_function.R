#Compute the rowwise loglikelihood of the three class model

lc1 <- attr(mxEval(class1.fitfunction, lca_final_model_m), 'likelihoods')
lc2 <- attr(mxEval(class2.fitfunction, lca_final_model_m), 'likelihoods')
lc3 <- attr(mxEval(class3.fitfunction, lca_final_model_m), 'likelihoods')

wgts <- lca_final_model_m$expectation$output$weights

cc <- cbind(l1=lc1*wgts[1],
            l2=lc2*wgts[2],
            l3=lc3*wgts[3])

ll_final <- log(rowSums(cc))

#Compute the loglikelihood of the four class model

lc1 <- attr(mxEval(class1.fitfunction, lca_models_m[[4]]), 'likelihoods')
lc2 <- attr(mxEval(class2.fitfunction, lca_models_m[[4]]), 'likelihoods')
lc3 <- attr(mxEval(class3.fitfunction, lca_models_m[[4]]), 'likelihoods')
lc4 <- attr(mxEval(class4.fitfunction, lca_models_m[[4]]), 'likelihoods')

wgts <- lca_models_m[[4]]$expectation$output$weights

cc <- cbind(l1=lc1*wgts[1],
            l2=lc2*wgts[2],
            l3=lc3*wgts[3],
            l4=lc4*wgts[4])

ll_4<- log(rowSums(cc))

#np final = 68
#np 4 = 91

#nonnest2::vuongtest needs these converted to a function that will return rowwise loglikelihood vectors. I suppose.

