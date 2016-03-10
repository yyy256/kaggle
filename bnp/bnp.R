#  加速BNP Paribas Cardif的索赔管理过程
#  1.claims for which approval could be accelerated leading to faster payments
#  2.在审核索赔之前需要额外的信息
# 
#  目标变量为1代表索赔可以被加速审核
# 
#  使用Log Loss来作为损失函数

setwd('d:/kaggle/bnp/')

library(dplyr)
library(readr)

train <- read_csv('train.csv')

char_vars <- names(train)[sapply(train, class)=="character"]

sapply(select(train, one_of(char_vars)), n_distinct)
unique(train$v22) %>% head(30)

sapply(train, function(x) sum(is.na(x)))
names(train)[sapply(train, function(x) n_distinct(x) == 2)]
