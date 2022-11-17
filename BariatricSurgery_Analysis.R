library(ggrepel)
library(ggplot2)
library(extrafont)
library(dplyr)
library(writexl)
library(readxl)
library(FactoMineR)
library(tidyverse)
library(viridis)
library(hrbrthemes)
#baray save pdf bedone az bein raftane text
library(Cairo)
library(extrafont)
library(outliers)
#palet haye rangi dare: https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
#ba in code neshon dade mishe display.brewer.all()
library(RColorBrewer)
#baraye kenar ham gozashtane chandta figure
library(gridExtra)
#baraye taiin cutoff khob
library(cutoff)
#for plot of correlation coeffient and ading significance on graph
library("ggpubr")
#for illustration of correlogram
library(corrplot)


dataset=read_xlsx("Data E5.xlsx")
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

corrplot2(
  data = dataset,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)


#EWL boxplot for outlier detection
ggplot(data = dataset, aes (`Excess weight loss 12 (%)`,`Excess weight loss 12 (%)`))+
  geom_boxplot()
ggplot(data = dataset, aes (`Excess weight loss 48 (%)`,`Excess weight loss 48 (%)`))+
  geom_boxplot()


#FBS boxplot for outlier detection
ggplot(data = dataset, aes (`FBS Pre (mg/dL)`,`FBS Pre (mg/dL)`))+
  geom_boxplot()
FBS_Pre_Threshold = 160
ggplot(data = dataset, aes (`FBS 12 (mg/dL)`,`FBS 12 (mg/dL)`))+
  geom_boxplot()
FBS_12_Threshold =120
ggplot(data = dataset, aes (`FBS 48 (mg/dL)`,`FBS 48 (mg/dL)`))+
  geom_boxplot()
FBS_48_Threshold = 155

#HbA1c boxplot for outlier detection
ggplot(data = dataset, aes (`HbA1c Pre (mg/dL)`,`HbA1c Pre (mg/dL)`))+
  geom_boxplot()
Hba1c_Pre_Threshold=7.5
ggplot(data = dataset, aes (`HbA1c 12 (mg/dL)`,`HbA1c 12 (mg/dL)`))+
  geom_boxplot()
Hba1c_12_Threshold=6.5
ggplot(data = dataset, aes (`HbA1c 48 (mg/dL)`,`HbA1c 48 (mg/dL)`))+
  geom_boxplot()
Hba1c_48_Threshold=6.5

#excercise time box plot for outlier detection
ggplot(data = dataset, aes (`Exercise min/day Pre (minute)`,`Exercise min/day Pre (minute)`))+
  geom_boxplot()
ExcMin_Pre_Threshold = 30
ggplot(data = dataset, aes (`Exercise min/day 12 (minute)`,`Exercise min/day 12 (minute)`))+
  geom_boxplot()
ExcMin_12_Threshold = 50
ggplot(data = dataset, aes (`Exercise min/day 48 (minute)`,`Exercise min/day 48 (minute)`))+
  geom_boxplot()
ExcMin_48_Threshold = 60



# Is it linear? FBS_Excersice_Pre
dataset_FBS_Excersice_Pre= filter(dataset, `FBS Pre (mmol/L)`<FBS_Pre_Threshold,
                                  `Exercise min/day Pre (minute)`<ExcMin_Pre_Threshold)
ggplot(data=dataset_FBS_Excersice_Pre, aes(`Exercise min/day Pre (minute)`, `FBS Pre (mmol/L)`))+
  geom_point(position = position_jitter(1))+
  geom_smooth(method = "lm")

plot_FBS_Excersice_Pre<- ggscatter (data=dataset_FBS_Excersice_Pre, x= "Exercise min/day Pre (minute)", y = "FBS Pre (mmol/L)", 
                                    add = "reg.line", conf.int = TRUE, cor.coef = TRUE,  
                                    cor.method = "pearson",position=position_jitter(1))
           

plot_FBS_Excersice_Pre

# Is it linear? FBS_Excersice_12
dataset_FBS_Excersice_12= filter(dataset, `FBS 12 (mmol/L)`<FBS_12_Threshold,
                                 `Exercise min/day 12 (minute)`<ExcMin_12_Threshold)
ggplot(data=dataset_FBS_Excersice_12, aes(`Exercise min/day 12 (minute)`, `FBS 12 (mmol/L)`))+
  geom_point(position = position_jitter(1))+
  geom_smooth(method = "lm")

plot_FBS_Excersice_12<- ggscatter (data=dataset_FBS_Excersice_12, x= "Exercise min/day 12 (minute)", y = "FBS 12 (mmol/L)", 
                                    add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                                    cor.method = "pearson",position=position_jitter(1))
plot_FBS_Excersice_12

# Is it linear? FBS_Excersice_48
dataset_FBS_Excersice_48= filter(dataset, `FBS 48 (mmol/L)`<FBS_48_Threshold,
                                 `Exercise min/day 48 (minute)`<ExcMin_48_Threshold)
ggplot(data=dataset_FBS_Excersice_48, aes(`Exercise min/day 48 (minute)`, `FBS 48 (mmol/L)`))+
  geom_point(position = position_jitter(1))+
  geom_smooth(method = "lm")

plot_FBS_Excersice_48<- ggscatter (data=dataset_FBS_Excersice_48, x= "Exercise min/day 48 (minute)", y = "FBS 48 (mmol/L)", 
                                   add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                                   cor.method = "pearson",position=position_jitter(1))
plot_FBS_Excersice_48

grid.arrange(plot_FBS_Excersice_Pre,
             plot_FBS_Excersice_12,
             plot_FBS_Excersice_48,
             ncol = 3, nrow = 1)



# Is it linear? HbA1c_Excersice_Pre
dataset_HbA1c_Excersice_Pre= filter(dataset, `HbA1c Pre (mg/dL)`<Hba1c_Pre_Threshold,
                                  `Exercise min/day Pre (minute)`<ExcMin_Pre_Threshold)
ggplot(data=dataset_HbA1c_Excersice_Pre, aes(`Exercise min/day Pre (minute)`, `HbA1c Pre (mg/dL)`))+
  geom_point(position = position_jitter(1))+
  geom_smooth(method = "lm")

plot_HbA1c_Excersice_Pre<- ggscatter (data=dataset_HbA1c_Excersice_Pre, x= "Exercise min/day Pre (minute)", y = "HbA1c Pre (mg/dL)", 
                                   add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                                   cor.method = "pearson",position=position_jitter(1))

plot_HbA1c_Excersice_Pre

# Is it linear? HbA1c_Excersice_12
dataset_HbA1c_Excersice_12= filter(dataset, `HbA1c 12 (mg/dL)`<Hba1c_12_Threshold,
                                 `Exercise min/day 12 (minute)`<ExcMin_12_Threshold)
ggplot(data=dataset_HbA1c_Excersice_12, aes(`Exercise min/day 12 (minute)`, `HbA1c 12 (mg/dL)`))+
  geom_point(position = position_jitter(1))+
  geom_smooth(method = "lm")

plot_HbA1c_Excersice_12<- ggscatter (data=dataset_HbA1c_Excersice_12, x= "Exercise min/day 12 (minute)", y = "HbA1c 12 (mg/dL)", 
                                      add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                                      cor.method = "pearson",position=position_jitter(1))
plot_HbA1c_Excersice_12

# Is it linear? HbA1c_Excersice_48
dataset_HbA1c_Excersice_48= filter(dataset, `HbA1c 48 (mg/dL)`<Hba1c_48_Threshold,
                                 `Exercise min/day 48 (minute)`<ExcMin_48_Threshold)
ggplot(data=dataset_HbA1c_Excersice_48, aes(`Exercise min/day 48 (minute)`, `HbA1c 48 (mg/dL)`))+
  geom_point(position = position_jitter(1))+
  geom_smooth(method = "lm")

plot_HbA1c_Excersice_48<- ggscatter (data=dataset_HbA1c_Excersice_48, x= "Exercise min/day 48 (minute)", y = "HbA1c 48 (mg/dL)", 
                                     add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                                     cor.method = "pearson",position=position_jitter(1))
plot_HbA1c_Excersice_48

grid.arrange(plot_HbA1c_Excersice_Pre,
             plot_HbA1c_Excersice_12,
             plot_HbA1c_Excersice_48,
             plot_FBS_Excersice_Pre,
             plot_FBS_Excersice_12,
             plot_FBS_Excersice_48,
             ncol = 3, nrow = 2)


# Is it linear? EWL_Excersice_12
dataset_EWL_Excersice_12= filter(dataset,
                                   `Exercise min/day 12 (minute)`<ExcMin_12_Threshold)
ggplot(data=dataset_EWL_Excersice_12, aes(`Exercise min/day 12 (minute)`, `Excess weight loss 12 (%)`))+
  geom_point(position = position_jitter(1))+
  geom_smooth(method = "lm")

plot_EWL_Excersice_12<- ggscatter (data=dataset_EWL_Excersice_12, x= "Exercise min/day 12 (minute)", y = "Excess weight loss 12 (%)", 
                                     add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                                     cor.method = "pearson",position=position_jitter(1),alpha=0.5)
plot_EWL_Excersice_12


# Is it linear? EWL_Excersice_48
dataset_EWL_Excersice_48= filter(dataset,
                                 `Exercise min/day 48 (minute)`<ExcMin_48_Threshold)
ggplot(data=dataset_EWL_Excersice_48, aes(`Exercise min/day 48 (minute)`, `Excess weight loss 48 (%)`))+
  geom_point(position = position_jitter(1))+
  geom_smooth(method = "lm")


plot_EWL_Excersice_48<- ggscatter (data=dataset_EWL_Excersice_48, x= "Exercise min/day 48 (minute)", y = "Excess weight loss 48 (%)", 
                                   add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                                   cor.method = "pearson",position=position_jitter(1.5),,alpha=0.5)
plot_EWL_Excersice_48


# Is it linear? EWL_HBa1c_12
dataset_EWL_HBA1c_12= filter(dataset,
                             `HbA1c 12 (mg/dL)`< Hba1c_12_Threshold)
ggplot(data=dataset_EWL_HBA1c_12, aes(`HbA1c 12 (mg/dL)`, `Excess weight loss 12 (%)`))+
  geom_point(position = position_jitter(1))+
  geom_smooth(method = "lm")

plot_EWL_HBA1c_12<- ggscatter (data=dataset_EWL_HBA1c_12, x= "HbA1c 12 (mg/dL)", y = "Excess weight loss 12 (%)", 
                                   add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                                   cor.method = "pearson",position=position_jitter(1),,alpha=0.5)
plot_EWL_HBA1c_12


# Is it linear? EWL_Hba1c_48
dataset_EWL_HBA1c_48= filter(dataset,
                             `HbA1c 48 (mg/dL)`< Hba1c_48_Threshold)
ggplot(data=dataset_EWL_HBA1c_48, aes(`Exercise min/day 48 (minute)`, `Excess weight loss 48 (%)`))+
  geom_point(position = position_jitter(2))+
  geom_smooth(method = "lm")



plot_EWL_HBA1c_48<- ggscatter (data=dataset_EWL_HBA1c_48, x= "HbA1c 48 (mg/dL)", y = "Excess weight loss 48 (%)", 
                                   add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                                   cor.method = "pearson",position=position_jitter(1.5),alpha=0.5)
plot_EWL_HBA1c_48

grid.arrange(plot_EWL_Excersice_12,
             plot_EWL_Excersice_48,
             plot_EWL_HBA1c_12,
             plot_EWL_HBA1c_48,
             ncol = 2, nrow = 2)
------------------------------------------------------------------------------------------
  
dataset_HbA1c_pre_EWL_P48= filter(dataset, `HbA1c Pre (mg/dL)`<Hba1c_Pre_Threshold)

ggplot(data=dataset_HbA1c_pre_EWL_P48, aes(`Excess weight loss 48 (%)`, `HbA1c Pre (mg/dL)`))+
  geom_point(position = position_jitter(2))+
  geom_smooth(method = "lm")


plot_HbA1c_pre_EWL_P48<- ggscatter (data=dataset_HbA1c_pre_EWL_P48, x= "Excess weight loss 48 (%)", y = "HbA1c Pre (mg/dL)", 
                               add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                               cor.method = "pearson",position=position_jitter(1.5),alpha=0.5)
plot_HbA1c_pre_EWL_P48

------------------------------------------------------------------------------------------
  
Model_HbA1c_pre_EWL_P48 <- lm(`Excess weight loss 48 (%)`~`HbA1c Pre (mg/dL)`, data=dataset_HbA1c_pre_EWL_P48)
summary(Model_HbA1c_pre_EWL_P48)





Model_EWL_ExcerMin_48 <- lm(`Excess weight loss 48 (%)`~`Exercise min/day 48 (minute)`, data=dataset_EWL_Excersice_48)
summary(Model_EWL_ExcerMin_48)

Model_EWL_HbA1c_48 <- lm(`Excess weight loss 48 (%)`~`HbA1c 48 (mg/dL)`, data=dataset_EWL_HBA1c_48)
summary(Model_EWL_HbA1c_48)


dataset_Mutli_EWL_ExcerMinANDHBA1c_48= filter(dataset, `Exercise min/day 48 (minute)`<ExcMin_48_Threshold,
                                            `HbA1c 48 (mg/dL)`< Hba1c_48_Threshold)
Model_Mutli_EWL_ExcerMinANDHBA1c_48 <- lm(`Excess weight loss 48 (%)`~`HbA1c 48 (mg/dL)`+`Exercise min/day 48 (minute)`, 
                                          data=dataset_Mutli_EWL_ExcerMinANDHBA1c_48)
summary(Model_Mutli_EWL_ExcerMinANDHBA1c_48)

---------------------------------

datasetwide=read_xlsx("Data E4 wide.xlsx")
attach(datasetwide)
datasetwide$Label=as.factor(datasetwide$Label)
datasetwide$Label <- factor(datasetwide$Label, levels=unique(datasetwide$Label))
datasetwide$`Activeity status`=as.factor(datasetwide$`Activeity status`)

compare_means(HbA1c~Label,  data = datasetwide, paired=TRUE, method="t.test")

my_comparisons <- list( c("Pre", "12 Month"), c("12 Month", "48 Month"), c("Pre", "48 Month"))

PLOT_BMI<- ggboxplot(datasetwide, x="Label", "BMI (kg/m2)")+ 
  geom_line(aes(group=`Patient No.`,alpha = .01),color="Light Gray")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test",paired = TRUE,label = "p.signif",)+
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
             PLOT_ExcSess,
             PLOT_ewl,
             ncol = 3, nrow = 3)
#--------------------------------------------------------------------------------------------------
# Comparing active and inactive groupd 
datasetwide=read_xlsx("/Data E4 wide.xlsx")
attach(datasetwide)
datasetwide$Label=as.factor(datasetwide$Label)
datasetwide$Label <- factor(datasetwide$Label, levels=unique(datasetwide$Label))
dataset_Prevs48=filter(datasetwide, Label==c("Pre", "48 Month"))
dataset_Prevs48$Label=as.factor(dataset_Prevs48$Label)
dataset_Prevs48$`Activeity status`=factor(dataset_Prevs48$`Activeity status`,levels = c("Inactive", "Partially Active", "Active"))


Plot_Prevs48_BMI<-ggplot(dataset_Prevs48, aes(Label, `BMI (kg/m2)`, fill=`Activeity status`))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#ef476f",  "#06d6a0"))+
  theme_classic()

Plot_Prevs48_HbA1c<-ggplot(dataset_Prevs48, aes(Label, `HbA1c (mg/dl)`, fill=`Activeity status`))+
  geom_point(position=position_jitter(0.1),alpha = .1)+
  geom_boxplot()+
  scale_fill_manual(values=c("#ef476f", "#06d6a0"))+
  theme_classic()

grid.arrange(Plot_Prevs48_BMI,
             Plot_Prevs48_HbA1c,
             ncol = 2, nrow = 1)
