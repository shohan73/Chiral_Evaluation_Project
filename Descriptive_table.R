library(tidyverse)
library(xlsx)
library(gtsummary)
library(gt)
library(readxl)
library(MASS)
library(dplyr)
library(nnet)
raw_data<-read_excel("Raw data/AMR_KAP_Data.xlsx")
data<-read_excel("Clean_data/Cleaned_KAP_Data.xlsx")


#Table1.Demonstrates the demographic characteristics of the study participants



raw_data |> 
  select(1:11) |> 
  tbl_summary(
    statistic = list(
      all_continuous() ~"{mean} ({sd})",
      all_categorical()~"{n} ({p}%)"
      
    
    )) |> 
  as_gt() |> 
  gtsave("Tables/Table1.docx")

#Table 2. Major sources of information about antibiotic parents
raw_data |> 
  
  select(41:49) |> 
  tbl_summary(
    statistic = 
  ) |> 
as_gt() |> 
gtsave("Tables/Table2.docx")


#Table3.Knowledge level ,Attitude and Practices

data |> 
  select(15,16,17) |> 
  tbl_summary(
    statistic = 
  ) |> 
  as_gt() |> 
  gtsave("Tables/Table3.docx")






#Table4.Factors associated with the level of knowledge among parents of school-going children


#convert dependent variable to ordered factor


data$knowledge_Level<-factor(data$knowledge_Level,levels =c("Poor","Moderate","Good"),ordered=TRUE)


data |> 
  dplyr::select(1:9,knowledge_Level) |> 
  tbl_uvregression(
    method =polr,
    y=knowledge_Level,
    pvalue_fun = function(x)style_pvalue(x,digits=3),
    exponentiate = TRUE
    
  )|> 
  
  
  bold_p(t=0.05) |> 
  as_gt() |> 
  gtsave("Tables/Table5.docx")






  
  
  
#Table 5. Factors associated with the level of attitudes towards antibiotic resistance among parents of school going children


#convert dependent variable to ordered factor


data$Attitude_Level<-factor(data$Attitude_Level,levels =c("Uncertain","Negative","Positive"),ordered=TRUE)

unique(data$Attitude_Level)

data |> 
  dplyr::select(1:9,Attitude_Level) |> 
  tbl_uvregression(
    method =polr,
    y=Attitude_Level,
    exponentiate = TRUE

    
  )|> 

  as_gt()|> 
  gtsave("Tables/Table5.docx")
  

# Factors associated with the level of practices regarding antibiotic resistance among parents of school going children
 #recode
data <- data |>
  mutate(Practices_level = case_when(
    Practice_Level == "good use" ~ 1,
    Practice_Level == "misuse" ~ 0
  ))

#

data |> 
  dplyr::select(1:9,15:16,Practices_level) |> 
  tbl_uvregression(
    method = glm,
    method.args = list(family=binomial),
   
    y=Practices_level,
    exponentiate=TRUE
  ) |> 

  as_gt() |> 
  gtsave("Tables/Table6.docx")








