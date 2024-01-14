rm(list=ls(all=TRUE))
library(data.table);library(dplyr); library(ggplot2);library(readxl); library(openxlsx); library(openxlsx)
library(lattice); library(mice);library(xgboost);# library(VIM)
df <- fread('/Volumes/MYM/Projet_Scoring_II_2023/train.csv')
df[, `:=`(CODE_GENDER = as.integer(CODE_GENDER == "M"), 
          FLAG_OWN_CAR = as.integer(FLAG_OWN_CAR == "Y"),
          FLAG_OWN_REALTY = as.integer(FLAG_OWN_REALTY == "Y"),
          NAME_CONTRACT_TYPE = as.integer(NAME_CONTRACT_TYPE == "Revolving loans"))]


df$NAME_INCOME_TYPE <- as.factor(df$NAME_INCOME_TYPE)
one_hot <- model.matrix(~ NAME_INCOME_TYPE - 1, data = df)
df <- cbind(df, one_hot)
all_one_hot  <- colnames(one_hot)  
df$NAME_INCOME_TYPE <- NULL
rm(one_hot)

df$NAME_EDUCATION_TYPE <- as.factor(df$NAME_EDUCATION_TYPE)
one_hot <- model.matrix(~ NAME_EDUCATION_TYPE - 1, data = df)
df <- cbind(df, one_hot)
all_one_hot  <- c(all_one_hot,colnames(one_hot))
df$NAME_EDUCATION_TYPE <- NULL
rm(one_hot)

df$NAME_FAMILY_STATUS <- as.factor(df$NAME_FAMILY_STATUS)
one_hot <- model.matrix(~ NAME_FAMILY_STATUS - 1, data = df)
df <- cbind(df, one_hot)
all_one_hot  <- c(all_one_hot,colnames(one_hot))
df$NAME_FAMILY_STATUS <- NULL
rm(one_hot)

df$NAME_HOUSING_TYPE <- as.factor(df$NAME_HOUSING_TYPE)
one_hot <- model.matrix(~ NAME_HOUSING_TYPE - 1, data = df)
df <- cbind(df, one_hot)
all_one_hot  <- c(all_one_hot,colnames(one_hot))
df$NAME_HOUSING_TYPE <- NULL
rm(one_hot)

##这一步维度诅咒 37->94
df$ORGANIZATION_TYPE <- as.factor(df$ORGANIZATION_TYPE)
one_hot <- model.matrix(~ ORGANIZATION_TYPE - 1, data = df)
df <- cbind(df, one_hot)
all_one_hot  <- c(all_one_hot,colnames(one_hot))
df$ORGANIZATION_TYPE <- NULL
rm(one_hot)

##这一步也是维度诅咒 
df$OCCUPATION_TYPE <- as.factor(df$OCCUPATION_TYPE)
one_hot <- model.matrix(~ OCCUPATION_TYPE - 1, data = df)
df <- cbind(df, one_hot)
all_one_hot  <- c(all_one_hot,colnames(one_hot))
df$OCCUPATION_TYPE <- NULL
rm(one_hot)

no_na_columns <- apply(df, 2, function(x) all(complete.cases(x)))
no_na_column_names <- names(df)[no_na_columns]
no_na_column_names <- setdiff(no_na_column_names, "SK_ID_CURR")
rm(no_na_columns)

non_binanry_non_na_cols <- no_na_column_names[!no_na_column_names %in% all_one_hot]

df <- df %>% mutate(across(all_of(non_binanry_non_na_cols), ~ ( . - mean(.)) / sd(.)))
df <- df %>% mutate(across(all_of(non_binanry_non_na_cols), ~ ( . - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

rm(no_na_column_names)

avant_socre <- setdiff(names(df),"EXT_SOURCE_1")
avant_socre <- setdiff(avant_socre,"EXT_SOURCE_2")
avant_socre <- setdiff(avant_socre,"EXT_SOURCE_3")



########################################################################
######################数据存储##########################################
########################################################################

file_path <- '/Volumes/MYM/Projet_Scoring_II_2023/Rapport/tableau_normalisation.csv'
write.csv(df, file = file_path)
rm(file_path)

descriptions <- c("all_one_hot","avant_socre","non_binanry_non_na_cols")
names1_str <- paste(all_one_hot, collapse = ", ")
names2_str <- paste(avant_socre, collapse = ", ")
names3_str <- paste(non_binanry_non_na_cols, collapse = ", ")
name_of_clos <- data.frame(Description = descriptions, 
                 Names = c(names1_str, names2_str, names3_str))
file_path <- '/Volumes/MYM/Projet_Scoring_II_2023/Rapport/索引列.csv'
write.csv(name_of_clos, file = file_path)
rm(file_path)