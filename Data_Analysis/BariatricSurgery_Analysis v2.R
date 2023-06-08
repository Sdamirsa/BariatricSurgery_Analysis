#install.packages("ggrepel")
library(ggrepel)
library(ggplot2)
#library(extrafont)
library(dplyr)
library(writexl)
library(readxl)
library(readxl)
#install.packages("FactoMineR")
library(FactoMineR)
library(tidyverse)
#remove.packages("rlang")
#install.packages("rlang")
#install.packages("vctrs")
#remove.packages("purrr")
#install.packages("purrr")
#library(viridis)
#library(hrbrthemes)
#baray save pdf bedone az bein raftane text
#install.packages("Cairo")
library(Cairo)
#install.packages("outliers")
library(outliers)
#palet haye rangi dare: https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
#ba in code neshon dade mishe display.brewer.all()
library(RColorBrewer)
#baraye kenar ham gozashtane chandta figure
#install.packages("gridExtra")
library(gridExtra)
#baraye taiin cutoff khob
#library(cutoff)
#for plot of correlation coeffient and ading significance on graph
#install.packages("ggpubr")
library(ggpubr)
#for illustration of correlogram
#install.packages("corrplot")
library(corrplot)
library(grid)
library(lattice)


library(rstatix)


#load dataset
dataset=read_xlsx("D:\\MY WORK\\MaGHALE\\bariatric\\Data E5.xlsx",sheet = "Data E3")
#Ideal weight calculation
dataset$"ideal weight (Kg)"= with(dataset, 55+88*(dataset$`Height (m)`-1.5))
# Excess body weight calculation (EBW)
dataset$"Pre EBW"=dataset$`Weight Pre (kg)`-dataset$"ideal weight (Kg)"
#Excess body weight lost percent calculation (EBWL)
dataset$"EBWL 12"=with(dataset, (dataset$`Weight Pre (kg)`- dataset$`Weight 12 (kg)`) / dataset$`Pre EBW`)
dataset$"EBWL 48"=with(dataset, (dataset$`Weight Pre (kg)`- dataset$`Weight 48 (kg)`)  / dataset$`Pre EBW`)



attach(dataset)

#Corelogram

corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficiens on the diagonal
           diag = diag
  )
}
corrplot2


corrplot2(
  data = dataset,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)




# Is it linear? FBS_Excersice_Pre

plot_FBS_Excersice_Pre<- ggplot(data=dataset, aes(`Exercise min/day Pre (minute)`, `FBS Pre (mg/dL)`))+
  geom_point(position = position_jitter(1), alpha=0.7)+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  theme_classic()

plot_FBS_Excersice_Pre

# Is it linear? FBS_Excersice_12

plot_FBS_Excersice_12<-ggplot(data=dataset, aes(`Exercise min/day 12 (minute)`, `FBS 12 (mg/dL)`))+
  geom_point(position = position_jitter(1), alpha=0.7)+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  theme_classic()

plot_FBS_Excersice_12

# Is it linear? FBS_Excersice_48

plot_FBS_Excersice_48<- ggplot(data=dataset, aes(`Exercise min/day 48 (minute)`, `FBS 48 (mg/dL)`))+
  geom_point(position = position_jitter(1), alpha=0.7)+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  theme_classic()

plot_FBS_Excersice_48

grid.arrange(plot_FBS_Excersice_Pre,
             plot_FBS_Excersice_12,
             plot_FBS_Excersice_48,
             ncol = 3, nrow = 1)



# Is it linear? HbA1c_Excersice_Pre

plot_HbA1c_Excersice_Pre<- ggplot(data=dataset, aes(`Exercise min/day Pre (minute)`, `HbA1c Pre (mg/dL)`))+
  geom_point(position = position_jitter(1), alpha=0.7)+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  theme_classic()

plot_HbA1c_Excersice_Pre

# Is it linear? HbA1c_Excersice_12

plot_HbA1c_Excersice_12<-ggplot(data=dataset, aes(`Exercise min/day 12 (minute)`, `HbA1c 12 (mg/dL)`))+
  geom_point(position = position_jitter(1), alpha=0.7)+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  theme_classic()

plot_HbA1c_Excersice_12

# Is it linear? HbA1c_Excersice_48

plot_HbA1c_Excersice_48<-ggplot(data=dataset, aes(`Exercise min/day 48 (minute)`, `HbA1c 48 (mg/dL)`))+
  geom_point(position = position_jitter(1), alpha=0.7)+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  theme_classic()

plot_HbA1c_Excersice_48



# Is it linear? EWL_HBa1c_12

plot_EWL_HBA1c_12<-ggplot(data=dataset, aes( `Excess weight loss 12 (%)`, `HbA1c 12 (mg/dL)`,))+
  geom_point(position = position_jitter(1),alpha=0.5)+
  geom_smooth(method = "lm", colour="orange")+
  stat_cor(method = "pearson")+
  theme_classic()+
  xlab("Loss of excess body weight (%)")


plot_EWL_HBA1c_12

# Is it linear? EWL_Hba1c_48

plot_EWL_HBA1c_48<-ggplot(data=dataset, aes( `Excess weight loss 48 (%)`, `HbA1c 48 (mg/dL)`))+
  geom_point(position = position_jitter(1),alpha=0.5)+
  geom_smooth(method = "lm", , colour="orange")+
  stat_cor(method = "pearson")+
  theme_classic()+
  xlab("Loss of excess body weight (%)")

plot_EWL_HBA1c_48

png("D:\\MY WORK\\MaGHALE\\bariatric\\Submit - Scientific Report\\Figure nn2.png", 
    width = 9,
    height = 9,
    unit = "in",
    res=500)
grid.arrange(plot_HbA1c_Excersice_Pre,
             plot_HbA1c_Excersice_12,
             plot_HbA1c_Excersice_48,
             plot_FBS_Excersice_Pre,
             plot_FBS_Excersice_12,
             plot_FBS_Excersice_48,
             plot_EWL_HBA1c_12,
             plot_EWL_HBA1c_48,
             ncol = 3, nrow = 3, 
             layout_matrix = cbind(c(1,4,NA),c(2,5,7),c(3,6,8)))
dev.off()

jpeg("D:\\MY WORK\\MaGHALE\\bariatric\\Submit - Scientific Report\\Figure nn2.jpeg", 
    width = 9,
    height = 9,
    unit = "in",
    res=500)
grid.arrange(plot_HbA1c_Excersice_Pre,
             plot_HbA1c_Excersice_12,
             plot_HbA1c_Excersice_48,
             plot_FBS_Excersice_Pre,
             plot_FBS_Excersice_12,
             plot_FBS_Excersice_48,
             plot_EWL_HBA1c_12,
             plot_EWL_HBA1c_48,
             ncol = 3, nrow = 3, 
             layout_matrix = cbind(c(1,4,NA),c(2,5,7),c(3,6,8)))
dev.off()

pdf("D:\\MY WORK\\MaGHALE\\bariatric\\Submit - Scientific Report\\Figure nn2.pdf", 
    width = 9,
    height = 9)
grid.arrange(plot_HbA1c_Excersice_Pre,
             plot_HbA1c_Excersice_12,
             plot_HbA1c_Excersice_48,
             plot_FBS_Excersice_Pre,
             plot_FBS_Excersice_12,
             plot_FBS_Excersice_48,
             plot_EWL_HBA1c_12,
             plot_EWL_HBA1c_48,
             ncol = 3, nrow = 3, 
             layout_matrix = cbind(c(1,4,NA),c(2,5,7),c(3,6,8)))
dev.off()

#----------------------------------------------------------------
  
# Is it linear? EWL_Excersice_12

plot_EWL_Excersice_12<-ggplot(data=dataset, aes(`Exercise min/day 12 (minute)`, `Excess weight loss 12 (%)`))+
  geom_point(position = position_jitter(1),alpha=0.5)+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  ylab("Loss of Excess Body Weight (%)") +
  theme_classic()



plot_EWL_Excersice_12






install.packages("ggalluvial")
library(ggalluvial)




png("D:\\MY WORK\\MaGHALE\\bariatric\\Submit - Scientific Report\\Figure nn3.png", 
    width = 6,
    height = 3,
    unit = "in",
    res=500)

grid.arrange(plot_EWL_Excersice_12,
             plot_EWL_Excersice_48,
             ncol = 2, nrow = 1)
dev.off()

jpeg("D:\\MY WORK\\MaGHALE\\bariatric\\Submit - Scientific Report\\Figure nn3.jpeg", 
    width = 6,
    height = 3,
    unit = "in",
    res=500)

grid.arrange(plot_EWL_Excersice_12,
             plot_EWL_Excersice_48,
             ncol = 2, nrow = 1)
dev.off()

pdf("D:\\MY WORK\\MaGHALE\\bariatric\\Submit - Scientific Report\\Figure nn3.pdf", 
    width = 6,
    height = 3)

grid.arrange(plot_EWL_Excersice_12,
             plot_EWL_Excersice_48,
             ncol = 2, nrow = 1)
dev.off()

#---------------
#Waterfall plot of BMI change stratified by active and low activity cohorts
means <- c(mean(dataset$`BMI Pre (kg/m2)`, na.rm = TRUE),
           mean(dataset$`BMI 12 (kg/m2)`, na.rm = TRUE),
           mean(dataset$`BMI 48 (kg/m2)`, na.rm = TRUE))
sds <- c(sd(dataset$`BMI Pre (kg/m2)`, na.rm = TRUE),
         sd(dataset$`BMI 12 (kg/m2)`, na.rm = TRUE),
         sd(dataset$`BMI 48 (kg/m2)`, na.rm = TRUE))
waterfall <- data.frame(Label = c("BMI Pre", "BMI 12-Month", "BMI-48 Month"),
                 Mean = means,
                 Standard.Deviation = sds)
waterfall

grouped_data <- aggregate(dataset[, c("BMI Pre (kg/m2)", "BMI 12 (kg/m2)", "BMI 48 (kg/m2)")], list(dataset$ActivityClassification), function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
grouped_data <- t(grouped_data[,-1])
rownames(grouped_data) <- grouped_data$ActivityClassification






#classfiieng activiy level
classify_activity <- function(x) {
  ifelse(x <= 29, "Low Activity", "Active")
}
dataset$Activity_Pre <- classify_activity(dataset$`Exercise min/day Pre (minute)`)
dataset$Activity_12 <- classify_activity(dataset$`Exercise min/day 12 (minute)`)
dataset$Activity_48 <- classify_activity(dataset$`Exercise min/day 48 (minute)`)

#mean SD and count in each 2x2x2 matrix based on activity levels
calc_mean_sd_count <- function(x) {
  c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), N = sum(!is.na(x)))
}
grouped_data <- aggregate(dataset[, c("BMI Pre (kg/m2)", "BMI 12 (kg/m2)", "BMI 48 (kg/m2)")], list(dataset$Activity_Pre, dataset$Activity_12, dataset$Activity_48), calc_mean_sd_count)
grouped_data<-do.call(data.frame, grouped_data)
colnames(grouped_data) <- c("Activity_Pre", "Activity_12", "Activity_48", paste0(rep(c("BMI Pre (kg/m2)", "BMI 12 (kg/m2)", "BMI 48 (kg/m2)"), each = 3), rep(c("_mean", "_sd", "_N"), times = 3)))

#add a new column based on 2x2x2 matrix for (8) subgroups
dataset <- dataset %>%
  mutate(Activity_Group_2x2x2matrix = paste(Activity_Pre, Activity_12, Activity_48, sep = "-"))

#filter subgroup of 3x3 with low data for paired t-test
group_counts <- dataset %>% 
  count(Activity_Group_2x2x2matrix)
filtered_dataset <- dataset %>%
  inner_join(group_counts %>% filter(n >= 2), by = "Activity_Group_2x2x2matrix")

#now perfrom t-test grouped by 3x3
grouped_data_ttest <- filtered_dataset %>%
  group_by(Activity_Group_2x2x2matrix) %>%
  summarize(p_value = t.test(`BMI Pre (kg/m2)`, `BMI 48 (kg/m2)`, paired = TRUE)$p.value,
            mean_diff = mean(`BMI 48 (kg/m2)` - `BMI Pre (kg/m2)`),
            lower_CI_95 = t.test(`BMI Pre (kg/m2)`, `BMI 48 (kg/m2)`, paired = TRUE)$conf.int[1],
            upper_CI_95 = t.test(`BMI Pre (kg/m2)`, `BMI 48 (kg/m2)`, paired = TRUE)$conf.int[2]) %>%
  ungroup()

grouped_data <- grouped_data %>%
  unite(Activity_Group_2x2x2matrix, Activity_Pre, Activity_12, Activity_48, sep = "-")

grouped_data_merged <- merge(grouped_data, grouped_data_ttest, by = "Activity_Group_2x2x2matrix", all = TRUE)
grouped_data_merged$p_value <- ceiling(grouped_data_merged$p_value * 10000) / 10000
grouped_data_merged$mean_diff <- round(grouped_data_merged$mean_diff, 2)
grouped_data_merged$lower_CI_95 <- round(grouped_data_merged$lower_CI_95, 2)
grouped_data_merged$upper_CI_95 <- round(grouped_data_merged$upper_CI_95, 2)
grouped_data_merged$`BMI Pre (kg/m2)_mean`=round(grouped_data_merged$`BMI Pre (kg/m2)_mean`, 2)
grouped_data_merged$`BMI 12 (kg/m2)_mean`=round(grouped_data_merged$`BMI 12 (kg/m2)_mean`, 2)
grouped_data_merged$`BMI 48 (kg/m2)_mean`=round(grouped_data_merged$`BMI 48 (kg/m2)_mean`, 2)
grouped_data_merged$`BMI Pre (kg/m2)_sd`=round(grouped_data_merged$`BMI Pre (kg/m2)_sd`, 2)
grouped_data_merged$`BMI 12 (kg/m2)_sd`=round(grouped_data_merged$`BMI 12 (kg/m2)_sd`, 2)
grouped_data_merged$`BMI 48 (kg/m2)_sd`=round(grouped_data_merged$`BMI 48 (kg/m2)_sd`, 2)
#install.packages("GGally")

dataset=read_xlsx("D:\\MY WORK\\MaGHALE\\bariatric\\Data E5.xlsx",sheet = "Data E3")

dataset$`48-Month Failure` <- ifelse(dataset$`48-Month Failure` == 1, "Insufficient", 
                                     ifelse(dataset$`48-Month Failure` == 0, "Good", dataset$`48-Month Failure`))
dataset$`Response 48`=as.factor(dataset$`48-Month Failure`)

dataset$`12-Month Failure` <- ifelse(dataset$`48-Month Failure` == 1, "Insufficient", 
                                     ifelse(dataset$`48-Month Failure` == 0, "Good", dataset$`48-Month Failure`))
dataset$`Response 12`=as.factor(dataset$`12-Month Failure`)
attach(dataset)


parallel_coordinate_EWL<- dataset%>%
  mutate(
         `LEBW Response 48`=factor(`48-Month Failure`, levels=c("Insufficient","Good")),
         `LEBW Response 12`=factor(`Response 12`, levels=c("Insufficient","Good"))) %>%
  pcp_select("Exercise min/day Pre (minute)", "Exercise min/day 12 (minute)",`Excess weight loss 12 (%)`,`LEBW Response 12`, "Exercise min/day 48 (minute)",`Excess weight loss 48 (%)`,  `LEBW Response 48`) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) + 
  geom_pcp_boxes(boxwidth=0.1) + 
  geom_pcp(aes(colour = `Response 48`), alpha = 0.35,axiswidth = c(0,0))+
  geom_pcp_labels()+
  scale_colour_manual(values=c( "darkgreen" ,"darkred"))+
  guides(colour=guide_legend(override.aes = list(alpha=1)))+
  theme_classic2()+
  theme(axis.text.x=element_text(angle=45, hjust = 1))+
  ylab("univariate min-max normalization")+
  xlab("")
parallel_coordinate_EWL

control_category <- function(value) {
  if (value < 6.5) {
    return("Fair Control")
  } else {
    return("Poor Control")
  }
}
dataset$"HbA1c Response Pre" <- cut(dataset$`HbA1c Pre (mg/dL)`, breaks=c(-Inf, 6.5, Inf), labels=c("Good Control", "Poor Control"))
dataset$"HbA1c Response 12" <-  cut(dataset$`HbA1c 12 (mg/dL)`, breaks=c(-Inf, 6.5, Inf), labels=c("Good Control", "Poor Control"))
dataset$"HbA1c Response 48" <-  cut(dataset$`HbA1c 48 (mg/dL)`, breaks=c(-Inf, 6.5, Inf), labels=c("Good Control", "Poor Control"))

dataset$"HbA1c Response Pre" <-as.factor(dataset$"HbA1c Response Pre")
dataset$"HbA1c Response 12"<-as.factor(dataset$"HbA1c Response 12")
dataset$"HbA1c Response 48"<-as.factor(dataset$"HbA1c Response 48")

attach(dataset)
dataset%>%
  mutate(
    `HbA1c Response 48`=factor("HbA1c Response 48", levels=c("Poor Control","Good Control")),
    `HbA1c Response 12`=factor("HbA1c Response 12", levels=c("Poor Control","Good Control")),
    "HbA1c Response Pre"=factor("HbA1c Response Pre", levels=c("Poor Control","Good Control"))
    ) %>%
  pcp_select("HbA1c Response Pre", `HbA1c Pre (mg/dL)`, "Exercise min/day Pre (minute)", "Exercise min/day 12 (minute)",`HbA1c 12 (mg/dL)`,`HbA1c Response 12`, "Exercise min/day 48 (minute)", `HbA1c 48 (mg/dL)`, "HbA1c Response 48") %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) + 
  geom_pcp_boxes(boxwidth=0.1) + 
  geom_pcp(aes(colour = `HbA1c Response 48`), alpha = 0.5,axiswidth = c(0,0))+
  geom_pcp_labels()+
  scale_colour_manual(values=c("darkred", "darkgreen" ))+
  guides(colour=guide_legend(override.aes = list(alpha=1)))+
  theme_classic2()+
  theme(axis.text.x=element_text(angle=45, hjust = 1))+
  ylab("univariate min-max normalization")+
  xlab("")
parallel_coordinate_A1c
#grid.arrange( plot_EWL_Excersice_12,  
#plot_EWL_Excersice_48, 
#parallel_coordinate,
#nrow = 5,
#layout_matrix=cbind(c(1,1,3,3,3),c(2,2,3,3,3)))

png("D:\\MY WORK\\MaGHALE\\bariatric\\Submit - Scientific Report\\Figure nn4.png", 
    width = 8,
    height = 5,
    unit = "in",
    res=500)

parallel_coordinate_EWL

dev.off()

jpeg("D:\\MY WORK\\MaGHALE\\bariatric\\Submit - Scientific Report\\Figure nn4.jpeg", 
     width = 8,
     height = 5,
     unit = "in",
     res=500)


parallel_coordinate

dev.off()

pdf("D:\\MY WORK\\MaGHALE\\bariatric\\Submit - Scientific Report\\Figure nn4.pdf", 
    width = 8,
    height = 5,)

parallel_coordinate

dev.off()



#https://heike.github.io/ggpcp/
dataset%>%
  mutate(`Exersice type Pre`=factor(`Exersice type Pre`),
         `Exersice type 12`=factor(`Exersice type 12`),
         `Exersice type 48`=factor(`Exersice type 48`),
         `48-Month Failure`=factor(`48-Month Failure`, levels=c("Insufficient","Good"))) %>%
  pcp_select(`Exersice type Pre`, `Exersice type 12`,`Excess weight loss 12 (%)`, `Response 12`,`Exersice type 48`,`Excess weight loss 48 (%)`,  `48-Month Failure`) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) + 
  geom_pcp_boxes(boxwidth=0.1) + 
  geom_pcp(aes(colour = `48-Month Failure`), alpha = 0.5,axiswidth = c(0,0))+
  geom_pcp_labels()+
  #scale_colour_manual(values=c("darkgreen", "darkred"))+
  guides(colour=guide_legend(override.aes = list(alpha=1)))+
  theme_classic2()+
  theme(axis.text.x=element_text(angle=45, hjust = 1))
  
dataset%>%
  mutate(`Exersice type in`=factor(`Exersice type Pre`),
         `Exersice type 12`=factor(`Exersice type 12`),
         `Exersice type 48`=factor(`Exersice type 48`),
         `48-Month Failure`=factor(`48-Month Failure`, levels=c("Insufficient","Good"))) %>%
  pcp_select(`Exersice type Pre`, `Exersice type 12`,`Excess weight loss 12 (%)`, `Exersice type 48`,`Excess weight loss 48 (%)`,  `48-Month Failure`) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) + 
  geom_pcp_boxes(boxwidth=0.1) + 
  geom_pcp(aes(colour = `48-Month Failure`), alpha = 0.5,axiswidth = c(0,0))+
  geom_pcp_labels()+
  #scale_colour_manual(values=c("darkgreen", "darkred"))+
  guides(colour=guide_legend(override.aes = list(alpha=1)))+
  theme_classic2()+
  theme(axis.text.x=element_text(angle=45, hjust = 1))

#--------------------------
#Excersice impact on weight loss 



plot_EWL_Excersice_48<- ggplot(data=dataset, aes(`Exercise min/day 48 (minute)`, `Excess weight loss 48 (%)`))+
  geom_point(position = position_jitter(1),alpha=0.5)+
  geom_smooth(method = "lm")+
  ylab("Loss of Excess Body Weight (%)") +
  stat_cor(method = "pearson")+
  theme_classic()


plot_EWL_Excersice_48




grid.arrange(plot_EWL_HBA1c_12,
             plot_EWL_HBA1c_48,
             ncol = 2, nrow = 1)

#------------------------------------------------------------------------------------------
#just for test of EWL48 and HbA1c Pre surgery

ggplot(data=dataset, aes(`Excess weight loss 48 (%)`, `HbA1c Pre (mg/dL)`))+
  geom_point(position = position_jitter(1),alpha=0.5)+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  theme_classic()


plot_HbA1c_pre_EWL_P48

#------------------------------------------------------------------------------------------
  
Model_HbA1c_pre_EWL_P48 <- lm(`Excess weight loss 48 (%)`~`HbA1c Pre (mg/dL)`, data=dataset)
summary(Model_HbA1c_pre_EWL_P48)

Model_EWL_ExcerMin_48 <- lm(`Excess weight loss 48 (%)`~`Exercise min/day 48 (minute)`, data=dataset)
summary(Model_EWL_ExcerMin_48)

Model_EWL_HbA1c_48 <- lm(`Excess weight loss 48 (%)`~`HbA1c 48 (mg/dL)`, data=dataset)
summary(Model_EWL_HbA1c_48)

Model_Mutli_EWL_ExcerMinANDHBA1c_48 <- lm(`Excess weight loss 48 (%)`~`HbA1c 48 (mg/dL)`+`Exercise min/day 48 (minute)`, 
                                          data=dataset)
summary(Model_Mutli_EWL_ExcerMinANDHBA1c_48)


Model_HbA1c_Excer_48 <- lm(`HbA1c 48 (mg/dL)`~`Exercise min/day 48 (minute)`,data=dataset)
summary(Model_HbA1c_Excer_48)
Model_HbA1c_EWL_48<-lm(`HbA1c 48 (mg/dL)`~ `Excess weight loss 48 (%)`,data=dataset)
summary(Model_HbA1c_EWL_48)
Model_Multi_HbA1c_ExcerandEWL<-lm(`HbA1c 48 (mg/dL)`~`Exercise min/day 48 (minute)`+`Excess weight loss 48 (%)`,data=dataset)
summary(Model_Multi_HbA1c_ExcerandEWL)


#---------------------------------

datasetwide=read_xlsx("D:\\MY WORK\\MaGHALE\\bariatric\\Data E4 wide.xlsx")
attach(datasetwide)
datasetwide$Label=as.factor(datasetwide$Label)
datasetwide$Label <- factor(datasetwide$Label, levels=unique(datasetwide$Label))
datasetwide$`Activeity status`=as.factor(datasetwide$`Activeity status`)

datasetwide$HbA1c=datasetwide$`HbA1c (mg/dl)`
datasetwide$BMI= datasetwide$`BMI (kg/m2)`

my_comparisons <- list( c("Pre", "12 Month"), c("12 Month", "48 Month"), c("Pre", "48 Month"))



PLOT_BMI<-ggplot(datasetwide, aes(x=Label, y=datasetwide$`BMI (kg/m2)`))+
  geom_line(aes(group=`Patient No.`),color="Light Gray")+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"), outlier.alpha = 0.0001)+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE, label = "p.signif")+
  theme_classic()
PLOT_BMI


PLOT_Weight<- ggplot(datasetwide, aes(x=Label, y=datasetwide$`Weight (kg)`))+
  geom_line(aes(group=`Patient No.`),color="Light Gray")+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"), outlier.alpha = 0.0001)+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE, label = "p.signif")+
  theme_classic()
PLOT_Weight


# Fat mass was excluded from the figure
PLOT_Fatmass<- ggplot(datasetwide, aes(x=Label, y=datasetwide$"Fat mass (Kg)"))+
  geom_line(aes(group=`Patient No.`),color="Light Gray")+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"), outlier.alpha = 0.0001)+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE, label = "p.signif")+
  theme_classic()
PLOT_Fatmass

 
PLOT_FBS <-ggplot(datasetwide, aes(x=Label, y=datasetwide$"FBS (mmol/L)"))+
  geom_line(aes(group=`Patient No.`),color="Light Gray")+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"), outlier.alpha = 0.0001)+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE, label = "p.signif")+
  theme_classic()
PLOT_FBS


PLOT_HbA1c<- ggplot(datasetwide, aes(x=Label, y=datasetwide$"HbA1c (mg/dl)"))+
  geom_line(aes(group=`Patient No.`),color="Light Gray")+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"))+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE, label = "p.signif")+
  theme_classic()
PLOT_HbA1c


PLOT_ExcMin<-ggplot(datasetwide, aes(x=Label, y=datasetwide$"Exercise min/day (minute)"))+
  geom_line(aes(group=`Patient No.`),color="Light Gray")+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"))+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE, label = "p.signif")+
  theme_classic()
PLOT_ExcMin


PLOT_ExcSess<-ggplot(datasetwide, aes(x=Label, y=datasetwide$"Exercise session/week"))+
  geom_line(aes(group=`Patient No.`),color="Light Gray")+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"))+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE, label = "p.signif")+
  theme_classic()
PLOT_ExcSess



png("Submit - Scientific Report\\Figure 1.png", 
    width = 15,
    height = 8,
    unit = "in",
    res=500)

grid.arrange(PLOT_Weight,
             PLOT_BMI,
             PLOT_FBS,
             PLOT_HbA1c,
             PLOT_ExcMin,
             PLOT_ExcSess,
             ncol = 3, nrow = 2)

dev.off()

#find mean difference and confidence interval to add to figure
dataset=read_xlsx("Data E5.xlsx")
t.test(`Exercise session/week 12`, `Exercise session/week pre`,data=dataset , paired = TRUE)
t.test(`Exercise session/week 48`, `Exercise session/week pre`,data=dataset , paired = TRUE)
ttest= t.test(`Exercise session/week 48`, `Exercise session/week 12`,data=dataset , paired = TRUE)
conf= ttest$conf.int
meandiff=ttest$estimate

#--------------------------------------------------------------------------------------------------
#datasetwideewl<-filter(datasetwide,Label==c("12 Month", "48 Month") )
#my_comparison_ewl=c("12 Month", "48 Month")
#PLOT_ewl<-ggboxplot(datasetwideewl, x="Label", "Excess weight loss (%)")+ 
  #geom_line(aes(group=`Patient No.`,alpha = .01),color="Light Gray")+
  #stat_compare_means(comparisons = my_comparison_ewl, method = "t.test",paired = TRUE,label = "p.signif",)+
  #geom_point(position=position_jitter(0.1),alpha = .1)+
  #geom_boxplot(fill=c( "#0000a7", "#eecc16"))


#-------------------------------------------------


# Comparing active and inactive groupd 
datasetwide=read_xlsx("Data E4 wide.xlsx")
attach(datasetwide)
datasetwide$Label=as.factor(datasetwide$Label)
datasetwide$Label <- factor(datasetwide$Label, levels=unique(datasetwide$Label))
dataset_Prevs48=filter(datasetwide, Label!="12 Month")
dataset_Prevs48$Label=as.factor(dataset_Prevs48$Label)
dataset_Prevs48$`Activeity status`=factor(dataset_Prevs48$`Activeity status`,levels = c("Low Activity Cohort", "Active Cohort"))
dataset_Prevs48$`Activity48Mo`=factor(dataset_Prevs48$`Activity48Mo`,levels = c("Low Activity", "Active"))

dataset_just48=filter(datasetwide, Label=="48 Month")
dataset_just48$`Activity48Mo`=factor(dataset_just48$`Activity48Mo`,,levels = c("Low Activity", "Active"))
#---

Plot_EWLvsActmin<-ggplot(data=dataset_just48, aes(x=`Activity48Mo`, y=`Excess weight loss (%)`, fill=`Activity48Mo`))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("#8491B4FF","#00A087FF" ))
Plot_EWLvsActmin


Plot_Prevs48_BMI<-ggplot(dataset_Prevs48, aes(Label, `BMI (kg/m2)`, fill=Label,))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#c1272d",  "#eecc16"))+
  stat_compare_means( method = "t.test",paired = TRUE, label = "p.signif")+
  theme_classic()+
  facet_grid(~ `Activity48Mo`,margins = T)
Plot_Prevs48_BMI
#BMIvsActmin_stat.test <- dataset_Prevs48 %>%
  #group_by(`Activity48Mo`) %>%
  #t_test(`BMI (kg/m2)` ~ Label , paired = T) %>%
  #add_significance()
#BMIvsActmin_stat.test 

Plot_Prevs48_A1cvsActmin<-ggplot(dataset_Prevs48, aes(Label, `HbA1c (mg/dl)`, fill=Label))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#c1272d",  "#eecc16"))+
  theme_classic()+
  facet_grid(~ `Activity48Mo`,margins = T)    
Plot_Prevs48_A1cvsActmin
#A1cvsActmin_stat.test <- dataset_Prevs48 %>%
  #group_by(`Activity48Mo`) %>%
  #t_test( `HbA1c (mg/dl)` ~ Label , paired = T) %>%
  #add_significance()
#A1cvsActmin_stat.test 



#---
Plot_Prevs48_BMIvsActint<-ggplot(dataset_Prevs48, aes(Label, `BMI (kg/m2)`, fill=Label))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#c1272d",  "#eecc16"))+
  theme_classic()+
  facet_grid(~ `Exercise intensity`,margins = T)
Plot_Prevs48_BMIvsActint


Plot_Prevs48_A1cvsActint<-ggplot(dataset_Prevs48, aes(Label, `HbA1c (mg/dl)`, fill=Label))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#c1272d",  "#eecc16"))+
  theme_classic()+
  facet_grid(~ `Exercise intensity`,margins = T)
Plot_Prevs48_A1cvsActint

Plot_EWLvsActint<-ggplot(data=dataset_just48, aes(x=`Exercise intensity`, y=`Excess weight loss (%)`, fill=`Exercise intensity`))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c( "#8491B4FF", "#00A087FF"))
Plot_EWLvsActint


#---

Plot_Prevs48_BMIvsActsess<-ggplot(dataset_Prevs48, aes(Label, `BMI (kg/m2)`, fill=Label))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#c1272d",  "#eecc16"))+
  theme_classic()+
  facet_grid(~ `Exercise session`,margins = T)
Plot_Prevs48_BMIvsActsess

Plot_Prevs48_A1cvsActsess<-ggplot(dataset_Prevs48, aes(Label, `HbA1c (mg/dl)`, fill=Label))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#c1272d",  "#eecc16"))+
  theme_classic()+
  facet_grid(~ `Exercise session`,margins = T)
Plot_Prevs48_A1cvsActsess

Plot_EWLvsActsess<-ggplot(data=dataset_just48, aes(x=`Exercise session`, y=`Excess weight loss (%)`, fill=`Exercise session`))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("#8491B4FF", "#91D1C2FF", "#00A087FF" ))
Plot_EWLvsActsess

#---

Plot_Prevs48_BMIvsActtype<-ggplot(dataset_Prevs48, aes(Label, `BMI (kg/m2)`, fill=Label))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#c1272d",  "#eecc16"))+
  theme_classic()+
  facet_grid(~ `Exersice type`,margins = T)
Plot_Prevs48_BMIvsActtype

Plot_Prevs48_A1cvsActtype<-ggplot(dataset_Prevs48, aes(Label, `HbA1c (mg/dl)`, fill=Label))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#c1272d",  "#eecc16"))+
  theme_classic()+
  facet_grid(~ `Exersice type`,margins = T,)
Plot_Prevs48_A1cvsActtype

Plot_EWLvsActtype<-ggplot(data=dataset_just48, aes(x=`Exersice type`, y=`Excess weight loss (%)`, fill=`Exersice type`))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c( "#8491B4FF", "#00A087FF"))
Plot_EWLvsActtype


Plot_EWLvsActmin
Plot_Prevs48_BMI
Plot_Prevs48_A1cvsActmin
Plot_EWLvsActint
Plot_Prevs48_BMIvsActint
Plot_Prevs48_A1cvsActint
Plot_EWLvsActsess
Plot_Prevs48_BMIvsActsess
Plot_Prevs48_A1cvsActsess
Plot_EWLvsActtype
Plot_Prevs48_BMIvsActtype
Plot_Prevs48_A1cvsActtype

#---
grid.arrange(Plot_EWLvsActmin,
             Plot_Prevs48_BMI,
             Plot_Prevs48_A1cvsActmin,
             Plot_EWLvsActint,
             Plot_Prevs48_BMIvsActint,
             Plot_Prevs48_A1cvsActint,
             Plot_EWLvsActsess,
             Plot_Prevs48_BMIvsActsess,
             Plot_Prevs48_A1cvsActsess,
             Plot_EWLvsActtype,
             Plot_Prevs48_BMIvsActtype,
             Plot_Prevs48_A1cvsActtype,
             ncol = 3, nrow = 4)



Plot_Prevs48_BMIvsActint<-ggplot(dataset_Prevs48, aes(Label, `BMI (kg/m2)`, fill=Label))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#c1272d",  "#eecc16"))+
  theme_classic()+
  facet_grid(~ `Exercise intensity`,margins = T)

Plot_Prevs48_BMIvsActint


Plot_Prevs48_BMI<-ggplot(dataset_Prevs48, aes(, `BMI (kg/m2)`, fill=Label))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#c1272d",  "#eecc16"))+
  theme_classic()+
  facet_grid(~ `Activity48Mo`,margins = T)
Plot_Prevs48_BMI


Plot_Prevs48_HbA1c<-ggplot(dataset_Prevs48, aes(`Activity48Mo`, `HbA1c (mg/dl)`, fill=Label))+
  geom_point(position=position_jitter(0.1),alpha = .1 )+
  geom_boxplot()+
  scale_fill_manual(values=c("#c1272d",  "#eecc16"))+
  theme_classic()
Plot_Prevs48_HbA1c

dataset=read_xlsx("Data E5.xlsx")
dataset$ActivityClassification=factor(dataset$ActivityClassification, levels = c("Low Activity Cohort", "Active Cohort"))
dataset$IfExcersicetimeequalorhigher30in48Mo=as.factor(dataset$IfExcersicetimeequalorhigher30in48Mo)
dataset$IfExcersicetimeequalorhigher30in12Mo=as.factor(dataset$IfExcersicetimeequalorhigher30in12Mo)

ggplot(dataset, aes(dataset$IfExcersicetimeequalorhigher30in48Mo, dataset$`Excess weight loss 48 (%)`, fill=dataset$IfExcersicetimeequalorhigher30in48Mo))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#ef476f", "#06d6a0"))+
  theme_classic()

#png("Submit - Scientific Report\\Figure 2.png", 
    #width = 15,
    #height = 8,
    #unit = "in",
    #res=500)
grid.arrange(Plot_Prevs48_BMI,
             Plot_Prevs48_HbA1c,
             ncol = 2, nrow = 1)

dataset=read_xlsx("Data E5.xlsx")
dataset

dataBMIinactive=filter(dataset, dataset$ActivityClassification=="Low Activity Cohort")
dataBMIactive=filter(dataset, dataset$ActivityClassification=="Active Cohort")

t.test(data=dataBMIinactive, dataBMIinactive$`BMI 48 (kg/m2)`, dataBMIinactive$`BMI Pre (kg/m2)`, paired = TRUE,)
t.test(data=dataBMIactive, dataBMIactive$`BMI 48 (kg/m2)`, dataBMIactive$`BMI Pre (kg/m2)`, paired = TRUE,)

t.test(dataBMIinactive$`BMI Pre (kg/m2)`, dataBMIactive$`BMI Pre (kg/m2)`, paired = F,var.equal=F)
t.test(dataBMIinactive$`BMI 48 (kg/m2)`, dataBMIactive$`BMI 48 (kg/m2)`, paired = F,var.equal=F)



t.test(data=dataBMIinactive, dataBMIinactive$`HbA1c 48 (mg/dL)`, dataBMIinactive$`HbA1c Pre (mg/dL)`, paired = TRUE,)
t.test(data=dataBMIactive, dataBMIactive$`HbA1c 48 (mg/dL)`, dataBMIactive$`HbA1c Pre (mg/dL)`, paired = TRUE,)

t.test(dataBMIinactive$`HbA1c Pre (mg/dL)`, dataBMIactive$`HbA1c Pre (mg/dL)`, paired = F,var.equal=F)
t.test(dataBMIinactive$`HbA1c 48 (mg/dL)`, dataBMIactive$`HbA1c 48 (mg/dL)`, paired = F,var.equal=F)

dataset$ActivityClassification=factor(dataset$ActivityClassification, levels = c("Low Activity Cohort", "Active Cohort"))
t.test(dataset$`Excess weight loss 48 (%)` ~ dataset$ActivityClassification, paired = F,var.equal=F)

install.packages("waterfalls")
library(waterfalls)

# Create a data frame with the changes in car performance

md <- dres %>%
  group_by(time, ztext) %>%
  summarise(
    m = mean(y),
    s = sd(y),
    n = n(),
    se = s / sqrt(n)
  )

#-----------------------------------
#showin charectristics in three time points of study

#datasetwide=read_xlsx("Data E4 wide.xlsx")
attach(datasetwide)
datasetwide$Label=as.factor(datasetwide$Label)
datasetwide$Label <- factor(datasetwide$Label, levels=unique(datasetwide$Label))
datasetwide$`Activeity status`=as.factor(datasetwide$`Activeity status`)

compare_means(HbA1c~Label,  data = datasetwide, paired=TRUE, method="t.test")

my_comparisons <- list( c("Pre", "12 Month"), c("12 Month", "48 Month"), c("Pre", "48 Month"))

PLOT_BMI<- ggplot(datasetwide, aes(x=Label, y=`BMI (kg/m2)`))+ 
  geom_line(aes(group=`Patient No.`,alpha = .01),color="Light Gray")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE,label = "p.signif",)+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"))
PLOT_BMI

ggplot(datasetwide, aes(x=Label, y=`BMI (kg/m2)`))+ 
  geom_line(aes(group=`Patient No.`,alpha = .01),color="Light Gray")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE,label = "p.signif",)+
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="white") +
  stat_summary(fun.data=mean_sdl, geom="errorbar", width=0.2)+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"))

compare_means("Weight (kg)"~ "Label",  data = datasetwide, paired=TRUE, method="t.test")

PLOT_Weight<- ggboxplot(datasetwide, x="Label", "Weight (kg)")+ 
  geom_line(aes(group=`Patient No.`,alpha = .01),color="Light Gray")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE,label = "p.signif",)+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"))

PLOT_Fatmass<- ggboxplot(datasetwide, x="Label", "Fat mass (Kg)")+ 
  geom_line(aes(group=`Patient No.`,alpha = .01),color="Light Gray")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE,label = "p.signif",)+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"))

PLOT_FBS <- ggboxplot(datasetwide, x="Label", "FBS (mmol/L)")+ 
  geom_line(aes(group=`Patient No.`,alpha = .01),color="Light Gray")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE,label = "p.signif",)+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"))

PLOT_HbA1c<- ggboxplot(datasetwide, x="Label", "HbA1c (mg/dl)")+ 
  geom_line(aes(group=`Patient No.`,alpha = .01),color="Light Gray")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE,label = "p.signif",)+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"))


datasetwideewl<-filter(datasetwide,Label==c("12 Month", "48 Month") )
my_comparison_ewl=c("12 Month", "48 Month")
PLOT_ewl<-ggboxplot(datasetwideewl, x="Label", "Excess weight loss (%)")+ 
  geom_line(aes(group=`Patient No.`,alpha = .01),color="Light Gray")+
  stat_compare_means(comparisons = my_comparison_ewl, method = "t.test",paired = TRUE,label = "p.signif",)+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c( "#0000a7", "#eecc16"))

PLOT_ExcMin<-ggboxplot(datasetwide, x="Label", "Exercise min/day (minute)")+ 
  geom_line(aes(group=`Patient No.`,alpha = .01),color="Light Gray")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE,label = "p.signif",)+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"))

PLOT_ExcSess<-ggboxplot(datasetwide, x="Label", "Exercise session/week")+ 
  geom_line(aes(group=`Patient No.`,alpha = .01),color="Light Gray")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE,label = "p.signif",)+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot(fill=c("#c1272d", "#0000a7", "#eecc16"))



grid.arrange(PLOT_Weight,
             PLOT_BMI,
             PLOT_Fatmass,
             PLOT_FBS,
             PLOT_HbA1c,
             PLOT_ExcMin,
             ncol = 3, nrow = 2)
