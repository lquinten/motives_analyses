#####################
## COMMON DISPLAY ##
####################

#Set-up ----
##packages
packages <- c("tidyverse", "readr", "ggpubr")
lapply(packages, require, character.only=T)
rm(packages)
##data & environment
load(".RData") #load a pre-saved .RData file in which the environment is saved

#Prepare gender data ----
##1 = female gender
data_females <- haven::read_spss("./data/dataE_clear5 2209.sav") %>% 
  dplyr::select(gender, CASE, sexdrive2, CSBD, PPCS_6, meffort, socialanx, lon, mvalue, attr) %>% 
  filter(gender == 1) %>% 
  dplyr::select(-gender)
##2 = male gender
data_males <- haven::read_spss("./data/dataE_clear5 2209.sav") %>% 
  dplyr::select(gender, CASE, sexdrive2, CSBD, PPCS_6, meffort, socialanx, lon, mvalue, attr) %>% 
  filter(gender == 2) %>% 
  dplyr::select(-gender)

#Violin plots males ----
##create long data frame for plotting, including only the relevant variables
plotd_m <- data_males %>% pivot_longer(cols=sexdrive2:attr, names_to="var", values_to="score")
##create a table with the quantiles for each variable
quant_m <- as.data.frame(t(do.call(rbind.data.frame, (lapply(c(
  "CSBD", "lon", "meffort", "mvalue", "PPCS_6", "sexdrive2", "socialanx"), function(x){
    parse_number(gsub(",.*$", "", levels(cut_number(as.data.frame(data_males)[, x], 4))))})))))
##rename columns
colnames(quant_m) <- c("CSBD", "lon", "meffort", "mvalue", "PPCS_6", "sexdrive2", "socialanx")
##pivot for plotting
quant_m <- pivot_longer(quant_m, cols=CSBD:socialanx, names_to='var')
##call on function in other R script
source("./functions/flat_violin.R")
##plot
m1 <- ggplot(plotd_m %>% filter(var!="attr"), aes(x=as.factor(var), y=score, fill=var)) +
  geom_flat_violin(scale="width") +
  scale_fill_grey(start=.5, end=.9, aesthetics = "fill") +
  geom_dotplot(binaxis="y", dotsize=.04, stackdir="down", binwidth=.3, position=position_nudge(-.025)) +
  geom_errorbar(data=quant_m, aes(x=as.factor(var), ymin=value, ymax=value, linetype="Quantiles"), 
                inherit.aes=F, linewidth=.5, width=.3, position=position_nudge(x=.15)) +
  scale_linetype_manual(values="solid") +
  guides(linetype=guide_legend(""), fill="none") +
  scale_x_discrete(labels=NULL) +
  labs(x="", y="Value") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=10))
##plot attraction to children separately
m2 <- ggplot(plotd_m %>% filter(var=="attr"), aes(x=score)) +
  geom_bar() +
  theme_classic() +
  labs(x="", y="Count")

#Violin plots females ----
##create long data frame for plotting, including only the relevant variables
plotd_f <- data_females %>% select(CASE, sexdrive2, CSBD, PPCS_6, meffort, socialanx, lon, mvalue, attr) %>% 
  pivot_longer(cols=sexdrive2:attr, names_to="var", values_to="score")
##create a table with the quantiles for each variable (except CSBD & PPCS_6)
quant_f <- as.data.frame(t(do.call(rbind.data.frame, (lapply(c("lon", "meffort", "mvalue", "sexdrive2", "socialanx"), 
                                                             function(x){parse_number(gsub(",.*$", "", levels(cut_number(
                                                               as.data.frame(data_females)[, x], 4))))})))))
##rename columns
colnames(quant_f) <- c("lon", "meffort", "mvalue", "sexdrive2", "socialanx")
##pivot for plotting
quant_f <- pivot_longer(quant_f, cols=lon:socialanx, names_to='var')
##add CSBD & PPCS_6 manually because they can only be dichotomised
quant_f <- rbind(quant_f, pivot_longer(as.data.frame(t(do.call(rbind.data.frame, 
                                                               (lapply(c("CSBD", "PPCS_6"), function(x){
                                                                 parse_number(gsub(",.*$", "",levels(cut_number(as.data.frame(
                                                                   data_females)[, x], 2))))}))))), cols=V1:V2, names_to='var'))
##rename variables
quant_f$var[quant_f$var=="V1"] <- "CSBD"
quant_f$var[quant_f$var=="V2"] <- "PPCS_6"
##plot
f1 <- ggplot(plotd_f %>% filter(var!="attr"), aes(x=as.factor(var), y=score, fill=var)) +
  geom_flat_violin(scale="width") +
  scale_fill_grey(start=.5, end=.9, aesthetics = "fill") +
  geom_dotplot(binaxis="y", dotsize=.04, stackdir="down", binwidth=.3, position=position_nudge(-.025)) +
  geom_errorbar(data=quant_f, aes(x=as.factor(var), ymin=value, ymax=value, linetype="Quantiles"), 
                inherit.aes=F, linewidth=.5, width=.3, position=position_nudge(x=.15)) +
  scale_linetype_manual(values="solid") +
  guides(linetype=guide_legend(""), fill="none") +
  scale_x_discrete(labels=c("CSBD"="Compulsive sex.", "lon"="Loneliness", "meffort"="Mating effort", "mvalue"="Mate value", 
                            "PPCS_6"="Probl. porn use", "sexdrive2"="Sex drive", "socialanx"="Social anxiety")) +
  labs(x="", y="Value") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=10))
##plot attraction to children separately
f2 <- ggplot(plotd_f %>% filter(var=="attr"), aes(x=score)) +
  geom_bar() +
  theme_classic() +
  labs(x="Attraction to children", y="Count")

#Arrange violin plots together ----
ggarrange(m1, m2, f1, f2,
          ncol=2, nrow=2,
          align="hv",
          labels=c('a)', '', 'b)', ''), font.label=list(size=12, face='plain'), vjust=c(1, 1, 0, 0),
          common.legend=T, legend='right')

#Prepare odds data ----
##1 = females
##put relevant results in a table
results_f <- cbind(Proclivity = c("Flirting or having sexual conversations via chat or webcam with a child", #1
                                  "Flirting or having sexual conversations via chat or webcam with a young person", #2
                                  "Driving under the influence of drugs/alcohol", #3
                                  "Paying or giving gifts to a child for online sexual material (for example, videos, images, 
                                  or online streaming)", #4
                                  "Paying or giving gifts to a young person for online sexual material (for example, videos, images, 
                                  or online streaming", #5
                                  "Killing someone", #6
                                  "Watching porn depicting a child", #7
                                  "Watching porn depicting a child on the Darknet", #8
                                  "Watching porn depicting a young person", #9
                                  "Engaging in sexual activity with a prostitute", #10
                                  "Watching porn in public spaces such as a bus or library", #11
                                  "Engaging in sexual activity with an adult who does not agree or is not able to agree, 
                                  for example, due to intoxication", #12
                                  "Robbing a bank", #13
                                  "Having offline sex or sexual contact with a child", #14
                                  "Having offline sex or sexual contact with a young person", #15
                                  "Driving faster than the posted speed limit", #16
                                  "Engaging in sexual activity with an animal"), #17
                   BCH_results_parameters_f %>%
                     select(ends_with("total_0.01"), starts_with("b") & ends_with("0.01"), starts_with("p") & ends_with("0.01")))
##add odds for each class
results_f <- results_f %>% mutate(odds_predicted_class_2_0.01 = exp(results_f$b_intercept_0.01)*
                                    exp(results_f$b_predicted_class_2_0.01), 
                                  odds_predicted_class_3_0.01 = exp(results_f$b_intercept_0.01)*
                                    exp(results_f$b_predicted_class_3_0.01),
                                  odds_predicted_class_1_0.01 = exp(results_f$b_intercept_0.01))
##change order of rows for unity with other tables
results_f <- results_f[c(10, 11, 12, 17, 7, 8, 1, 4, 14, 16, 3, 13, 6),] #exclude YP items 9, 2, 5, 15
##rename proclivities for plotting
procl <- c("Prostitute", "Public porn", "Rape", "Zoophilia", "Porn child", "Porn child darknet", "Chat child", "Gift child", 
           "Sex child", "Speed", "DUI", "Rob", "Kill")
##rename proclivities
plot_results_f <- results_f
plot_results_f$Proclivity <- procl
##factorise items
plot_results_f$Proclivity <- factor(plot_results_f$Proclivity, levels=plot_results_f$Proclivity)
##pivot results table for plotting (only odds < 1 for better scaling)
plot_results_f <- plot_results_f %>% 
  select(Proclivity, starts_with("odds")) %>% 
  filter(odds_predicted_class_1_0.01 < 1 & odds_predicted_class_2_0.01 < 1 & odds_predicted_class_3_0.01 < 1) %>% 
  pivot_longer(cols=starts_with("odds"), names_to="class", values_to="Odds") 
##rename class column
plot_results_f$class <- rep(c(2, 3, 1), nrow(plot_results_f)/3)
##remaining odds > 1
##rename
plot_results_2_f <- results_f
plot_results_2_f$Proclivity <- procl
##factorise
plot_results_2_f$Proclivity <- factor(plot_results_2_f$Proclivity, levels=plot_results_2_f$Proclivity)
##pivot
plot_results_2_f <- plot_results_2_f %>% 
  select(Proclivity, starts_with("odds")) %>% 
  filter(odds_predicted_class_1_0.01 > 1 | odds_predicted_class_2_0.01 > 1 | odds_predicted_class_3_0.01 > 1) %>% 
  pivot_longer(cols=starts_with("odds"), names_to="class", values_to="Odds") 
##rename class column
plot_results_2_f$class <- rep(c(2, 3, 1), nrow(plot_results_2_f)/3)

##2 = males
##put relevant results in a table
results <- cbind(Proclivity = c("Flirting or having sexual conversations via chat or webcam with a child", #1
                                "Flirting or having sexual conversations via chat or webcam with a young person", #2
                                "Driving under the influence of drugs/alcohol", #3
                                "Paying or giving gifts to a child for online sexual material (for example, videos, images, 
                                or online streaming)", #4
                                "Paying or giving gifts to a young person for online sexual material (for example, videos, images, 
                                or online streaming", #5
                                "Killing someone", #6
                                "Watching porn depicting a child", #7
                                "Watching porn depicting a child on the Darknet", #8
                                "Watching porn depicting a young person", #9
                                "Engaging in sexual activity with a prostitute", #10
                                "Watching porn in public spaces such as a bus or library", #11
                                "Engaging in sexual activity with an adult who does not agree or is not able to agree, for example, 
                                due to intoxication", #12
                                "Robbing a bank", #13
                                "Having offline sex or sexual contact with a child", #14
                                "Having offline sex or sexual contact with a young person", #15
                                "Driving faster than the posted speed limit", #16
                                "Engaging in sexual activity with an animal"), #17
                 BCH_results_parameters %>% 
                   filter(!str_detect(outcomes, "_red")) %>% #don't show the additional analyses for the young persons
                   # filter(!outcomes %in% c("Y_proclChatYp_r", "Y_proclGiftYp_r", "Y_proclPornYp_r", "Y_proclSexYp_r")) %>% 
                   select(ends_with("total_0.01"), starts_with("b") & ends_with("0.01"), starts_with("p") & ends_with("0.01")
                          #, starts_with("ci") & ends_with("0.01")
                   ))
##add odds for each class
results <- results %>% mutate(odds_predicted_class_1_0.01 = exp(results$b_intercept_0.01)*exp(results$b_predicted_class_1_0.01), 
                              odds_predicted_class_2_0.01 = exp(results$b_intercept_0.01)*exp(results$b_predicted_class_2_0.01),
                              odds_predicted_class_3_0.01 = exp(results$b_intercept_0.01))
##change order of rows for unity with other tables
results <- results[c(10, 11, 12, 17, 7, 8, 1, 4, 14, 16, 3, 13, 6),]  #without young person items 9, 2, 5, 15
##rename proclivities
plot_results <- results
plot_results$Proclivity <- procl
#Factorise items
plot_results$Proclivity <- factor(plot_results$Proclivity, levels=plot_results$Proclivity)
##pivot results table for plotting (only odds < 1 for better scaling)
plot_results <- plot_results %>% 
  select(Proclivity, starts_with("odds")) %>% 
  filter(odds_predicted_class_1_0.01 < 1 & odds_predicted_class_2_0.01 < 1 & odds_predicted_class_3_0.01 < 1) %>% 
  pivot_longer(cols=starts_with("odds"), names_to="class", values_to="Odds") 
##rename class column
plot_results$class <- rep(1:3, nrow(plot_results)/3)
##remaining odds > 1
##rename
plot_results_2 <- results
plot_results_2$Proclivity <- procl
##factorise
plot_results_2$Proclivity <- factor(plot_results_2$Proclivity, levels=plot_results_2$Proclivity)
##pivot
plot_results_2 <- plot_results_2 %>% 
  select(Proclivity, starts_with("odds")) %>% 
  filter(odds_predicted_class_1_0.01 > 1 | odds_predicted_class_2_0.01 > 1 | odds_predicted_class_3_0.01 > 1) %>% 
  pivot_longer(cols=starts_with("odds"), names_to="class", values_to="Odds") 
##rename class column
plot_results_2$class <- rep(1:3, nrow(plot_results_2)/3)

#Odds plots males ----
##line plots odds < 1
m3 <- ggplot(plot_results, aes(x=Proclivity, y=Odds, group=as.factor(class), linetype=as.factor(class))) +
  geom_line() +
  geom_point(aes(shape=as.factor(class))) +
  scale_linetype_manual(name=NULL, values=c("dotted", "dashed", "solid"), labels=c("Class 1: \"Mating prowess\"", 
                                                                                   "Class 2: \"Social needs\"", 
                                                                                   "Class 3: \"Multiple motivations\"")) +
  scale_shape_manual(name=NULL, values=c(3, 1, 0), labels=c("Class 1: \"Mating prowess\"", "Class 2: \"Social needs\"", 
                                                            "Class 3: \"Multiple motivations\"")) +
  labs(x="") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8))
#odds > 1
m4 <- ggplot(plot_results_2, aes(x=Proclivity, y=Odds, group=as.factor(class), linetype=as.factor(class))) +
  geom_line() +
  geom_point(aes(shape=as.factor(class))) +
  scale_linetype_manual(name=NULL, values=c("dotted", "dashed", "solid"), labels=c("Class 1: \"Mating prowess\"", 
                                                                                   "Class 2: \"Social needs\"", 
                                                                                   "Class 3: \"Multiple motivations\"")) +
  scale_shape_manual(name=NULL, values=c(3, 1, 0), labels=c("Class 1: \"Mating prowess\"", "Class 2: \"Social needs\"", 
                                                            "Class 3: \"Multiple motivations\"")) +
  labs(x="", y="") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8))

#Odds plots females ----
##line plots odds < 1
f3 <- ggplot(plot_results_f, aes(x=Proclivity, y=Odds, group=as.factor(class), linetype=as.factor(class))) +
  geom_line() +
  geom_point(aes(shape=as.factor(class))) +
  scale_linetype_manual(name=NULL, values=c("solid", "dotted", "dashed"), labels=c("Class 1: \"Multiple motivations\"", 
                                                                                   "Class 2: \"Mating prowess\"", 
                                                                                   "Class 3: \"Social needs\"")) +
  scale_shape_manual(name=NULL, values=c(0, 3, 1), labels=c("Class 1: \"Multiple motivations\"", "Class 2: \"Mating prowess\"", 
                                                            "Class 3: \"Social needs\"")) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8))
#odds > 1
f4 <- ggplot(plot_results_2_f, aes(x=Proclivity, y=Odds, group=as.factor(class), linetype=as.factor(class))) +
  geom_line() +
  geom_point(aes(shape=as.factor(class))) +
  scale_linetype_manual(name=NULL, values=c("solid", "dotted", "dashed"), labels=c("Class 1: \"Multiple motivations\"", 
                                                                                   "Class 2: \"Mating prowess\"", 
                                                                                   "Class 3: \"Social needs\"")) +
  scale_shape_manual(name=NULL, values=c(0, 3, 1), labels=c("Class 1: \"Multiple motivations\"", "Class 2: \"Mating prowess\"", 
                                                            "Class 3: \"Social needs\"")) +
  labs(y="") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8))

#Arrange odds plots together ----
f5 <- ggarrange(f3, f4, ncol=2, align="v", common.legend=T, legend='right')
m5 <- ggarrange(m3, m4, ncol=2, align="v", common.legend=T, legend='right')
ggarrange(m5, f5,
          nrow=2,
          labels=c('a)','b)'), font.label=list(size=12, face='plain'), vjust=c(1, 1))




