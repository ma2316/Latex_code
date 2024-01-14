rm(list=ls(all=TRUE))
library(data.table);library(dplyr); library(ggplot2);library(readxl); library(openxlsx); library(openxlsx)
library(lattice); library(mice); library(VIM)
df <- fread('/Volumes/MYM/Projet_Scoring_II_2023/train.csv')
df[, `:=`(OCCUPATION_TYPE = replace(OCCUPATION_TYPE, (OCCUPATION_TYPE==""), NA))]
#df <- fread('/Volumes/MYM/04. IREF - Risque de crédit x Risque climatique/séparation_en_6/subset1.csv')
head(df)

# Compte le nombre de 'NA'
na_counts_dt <- df[, lapply(.SD, function(x) sum(is.na(x)))]
na_counts_vector <- unlist(na_counts_dt)
na_counts_vector

dim(df)
# On trouver que member_id a 100% "NA", et les rests à traiter, ceux sont : 
# 1. loan_amnt 2. funded_amnt 3.  funded_amnt_inv 4.  int_rate 5.  installment 6. annual_inc 7. dti 8. delinq_2yrs

# Visualisation des distribution de 'NA'
na_data <- data.frame(column = names(na_counts_vector), na_count = na_counts_vector)
ggplot(na_data, aes(x = column, y = na_count)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Histogram of NA Counts in Each Column",
       x = "Column", y = "Number of NAs") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



# Comment on traiter le "NA"? 
# Si il y a très peu de "NA", je conseil de le supprimer direct, ici, c'est le cas
# Si le "NA" est important, plus de 5% dans la population, je croix qu'il faut utiliser quelque méthode pour le remplaser
#       #soit on remplaser par la moyenne 
#       #soit on utiliser KNN
#             #Les colonne qui n'ont pas de "NA" sont des train, les rests sont test
library(openxlsx)
file_path <- '/Volumes/MYM/Projet_Scoring_II_2023/Rapport/NaN_table.xlsx'
write.xlsx(na_data, file = file_path, rowNames = FALSE)
##########################################################################
##########################################################################



par(mar = c(5, 8, 4, 0), cex.axis = 0.3, cex.lab = 0.2)
md.pattern(df, plot = TRUE, rotate.names = TRUE)



png("md_pattern.png", width = 1200, height = 800)  # 可以调整 width 和 height 的值
par(mar = c(5, 8, 4, 0), cex.axis = 0.8, cex.lab = 0.8)
md.pattern(df, plot = TRUE, rotate.names = TRUE)
dev.off()

par(mar = c(5, 8, 4, 2) + 0.1)
md_res <- md.pattern(df, plot = TRUE)
md_res_df <- as.data.frame(md_res)
print(md_res_df)
par(mar = c(5, 8, 4, 0))  # 更大的边距 c(bottom, left, top, right)
md.pattern(df, plot = TRUE, rotate.names = TRUE)



