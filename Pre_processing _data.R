library(tidyverse)
library(readxl)
library(xlsx)


#Load raw data




data<-read_excel("Raw data/AMR_KAP_Data.xlsx")



#Check missing value

#is.na(data)
#sum(is.na(data))

#Remove missing value

#data <-na.omit(data)


#separate the data


kap_data<- data |> 
  select(12:39)

#change the column name


colnames(kap_data)<-paste0("Q",1:28)
# Knowledge of antibiotics percentage calculation






knowledge_of_antibiotics <- kap_data |> 
  select(Q1:Q12) |> 
  mutate(across(c(Q1,Q2,Q3, Q6, Q8,Q9,Q10,Q11,Q12), ~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0,
    . == "Don't Know" ~ 0,
    TRUE ~ NA_real_
  ))) |>
  mutate(across(c(Q4, Q5, Q7), ~case_when(
    . == "No" ~ 1,
    . == "Yes" ~ 0,
    . == "Don't Know" ~ 0,
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |> 
  mutate(knowledge_of_antibiotics = (sum(c_across(Q1:Q12), na.rm = TRUE) / 12) * 100)  
  



# parents attitudes
Attitude<-kap_data |> 
  select(Q13:Q22) |> 
  mutate(across(Q13:Q22,~case_when(
    .=="Agree"  ~0,
    .=="Disagree"   ~1,
    .=="Neutral" ~0,
    TRUE~NA_real_
    
    
    
  ))) |> 
  rowwise() |> 
  mutate(Attitude=sum(c_across(Q13:Q22),na.rm = TRUE)/10*100) 
  



#parents practices


Practices<-kap_data |> 
  select(Q23:Q28) |> 
  mutate(across(c(Q23,Q26,Q27),~case_when(
    .=="Yes"  ~0,
    .=="No"   ~1,
  
    TRUE~NA_real_
    
    
    
  ))) |>
  mutate(across(c(Q24,Q25,Q28),~case_when(
    .=="Yes"  ~1,
    .=="No"   ~0,
    
    TRUE~NA_real_
    
    
    
  ))) |>
  
  
  
  rowwise() |> 
  mutate(Practices=sum(c_across(Q23:Q28),na.rm = TRUE)/6*100) 




#Selection of demographic data

demographic_data<-data |> 
  select(1:11)

#All domain together

KAP_domain<-cbind(knowledge_of_antibiotics,Attitude,Practices)



#Creating status of every domain

KAP_domain<-KAP_domain |> 
  select(knowledge_of_antibiotics,Attitude,Practices)

KAP_domain <- KAP_domain |> 
  select(knowledge_of_antibiotics, Attitude, Practices) |> 
  
  mutate(knowledge_Level = case_when(
    knowledge_of_antibiotics <= 49 ~ "Poor",          
    knowledge_of_antibiotics >= 50 & knowledge_of_antibiotics <= 79 ~ "Moderate",
    knowledge_of_antibiotics >= 80 ~ "Good",         
  )) |> 
  mutate(Attitude_Level=case_when(
    Attitude <= 49 ~ "Negative",
    Attitude>= 50 & Attitude < 80 ~ "Uncertain",
    Attitude>= 80 ~ "Positive",
    
  ) ) |> 
   
  mutate(Practice_Level=case_when(
    
    Practices<=79~"misuse",
    Practices>=80~"good use",
    
    
  )) |> 
 
  # Step 3: Dichotomize based on the median score for each variable
  mutate(
    # Dichotomize Knowledge: Better (above median), Worse (below median)
    knowledge_dichotomy = if_else(knowledge_of_antibiotics > median(knowledge_of_antibiotics, na.rm = TRUE), "Better", "Worse"),
    
    # Dichotomize Attitude: Better (above median), Worse (below median)
    attitude_dichotomy = if_else(Attitude > median(Attitude, na.rm = TRUE), "Better", "Worse"),
    
    # Dichotomize Practice: Better (above median), Worse (below median)
    practice_dichotomy = if_else(Practices > median(Practices, na.rm = TRUE), "Better", "Worse")
    
  
  )

total_data<-cbind(demographic_data,KAP_domain)


write.xlsx(total_data,"Clean_data/Cleaned_KAP_Data.xlsx",row.names = FALSE)









