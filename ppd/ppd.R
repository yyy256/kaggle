# Master
# 每一行代表一个样本（一笔成功成交借款），每个样本包含200多个各类字段。
# idx：每一笔贷款的unique key，可以与另外2个文件里的idx相匹配。
# UserInfo_*：借款人特征字段
# WeblogInfo_*：Info网络行为字段
# Education_Info*：学历学籍字段
# ThirdParty_Info_PeriodN_*：第三方数据时间段N字段
# SocialNetwork_*：社交网络字段
# LinstingInfo：借款成交时间
# Target：违约标签（1 = 贷款违约，0 = 正常还款）。测试集里不包含target字段。

# Log_Info 有重复的行为
# 借款人的登陆信息。
# ListingInfo：借款成交时间 一个id对应一个时间 和user_info中一样
# LogInfo1：操作代码 有35种代码
# LogInfo2：操作类别 有15种类别
# LogInfo3：登陆时间
# idx：每一笔贷款的unique key

# Userupdate_Info 有重复的行为
# 借款人修改信息
# ListingInfo1：借款成交时间 一个id对应一个时间
# UserupdateInfo1：修改内容
# UserupdateInfo2：修改时间
# idx：每一笔贷款的unique key

# setwd('d:/kaggle/ppd')
setwd('kaggle/ppd/')
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(xgboost)
library(Matrix)
library(randomForest)
library(rpart)

train_log_info <- read.csv("./Training Set/PPD_LogInfo_3_1_Training_Set.csv", as.is = T)
train_master <- read.csv("./Training Set/PPD_Training_Master_GBK_3_1_Training_Set.csv", fileEncoding='gbk', as.is = T)
train_user_info <- read.csv("./Training Set/PPD_Userupdate_Info_3_1_Training_Set.csv", as.is = T)

test_log_info <- read.csv("./Test Set/PPD_LogInfo_2_Test_Set.csv", as.is = T)
test_master <- read.csv("./Test Set/PPD_Master_GBK_2_Test_Set.csv", fileEncoding='gbk', as.is = T)
test_user_info <- read.csv("./Test Set/PPD_Userupdate_Info_2_Test_Set.csv")

pre_process_func <- function(log_info, master, user_info){
  names(log_info) <- tolower(names(log_info))
  names(master) <- tolower(names(master))
  names(user_info) <- tolower(names(user_info))

  log_info$listinginfo1 <- ymd(log_info$listinginfo1)
  log_info$loginfo3 <- ymd(log_info$loginfo3)
  user_info$listinginfo1 <- ymd(user_info$listinginfo1)
  user_info$userupdateinfo2 <- ymd(user_info$userupdateinfo2)
  # master$listinginfo <- ymd(master$listinginfo)

  # 将修改内容都变成小写，去掉两边空格
  user_info$userupdateinfo1 <- str_trim(tolower(user_info$userupdateinfo1))

  user_info <- user_info %>%
    mutate(update_gap = as.numeric(difftime(listinginfo1, userupdateinfo2, units='days')))
  log_info <- log_info %>%
    mutate(log_gap = as.numeric(difftime(listinginfo1, loginfo3, units='days')))
  return (list(log_info, master, user_info))
}

train_df <- pre_process_func(train_log_info, train_master, train_user_info)
test_df <- pre_process_func(test_log_info, test_master, test_user_info)


# lapply(log_info, n_distinct)
# lapply(user_info, n_distinct)

# char_var <- names(train)[sapply(train, class)=="character"] # 查看字符型的变量
# lapply(char_var, function(x) n_distinct(train[, x]))
# lapply(train, n_distinct)

# train1 <- train %>% left_join(user_info) %>% left_join(log_info %>% select(-Listinginfo1))

# log_info %>% distinct() %>% nrow
# nrow(log_info)
# user_info %>% distinct() %>% nrow
# nrow(user_info)

# # select_vars()

# names(train)[sapply(train, n_distinct)==1] # WeblogInfo_10是没用的

# # train %>% select_("UserInfo_2", "UserInfo_4") %>% select(starts_with("UserInfo")) %>% head()
# train[, names(train) %in% char_var] %>% select(starts_with("WeblogInfo")) %>% head()

# D应该代表缺失
# UserInfo_8 手机运营商 还需要去除空格
# UserInfo_21 婚姻状况 userinfo_22
# UserInfo_22 学历状况
# UserInfo_23 具体地址
# Education_Info2 "E"  "AM" "A"  "AN" "AQ" "U"  "B"
# Education_Info3 "E"    "毕业" "结业" E似乎代表空值
# Education_Info4 "E"  "T"  "AR" "F"  "V"  "AE"
# Education_Info6 "E"  "A"  "AM" "AQ" "U"  "B"
# Education_Info7 "E"    "不详" 没用的字段
# Education_Info8 "E"    "T"    "F"    "V"    "AE"   "80"   "不详"

# WeblogInfo_19 "I" "E" "F" "D" "J" "G" "H" ""
# WeblogInfo_20
# WeblogInfo_21 "D" "C" "A" "B" ""

# 我要借款 我要理财
# 上次修改内容的最大时间间隔，最小时间间隔，平均时间间隔

# 之前修改了几种信息 (无用)
# a1 <- user_info %>% group_by(idx) %>% summarise(f=n_distinct(userupdateinfo1)) %>%
#   left_join(train %>% select(idx, target))
# boxplot(f ~ target, data = a1)
# 修改了那种信息，用one-hot-encoding
# 婚姻
# 学历

compute_var <- function(log_info, master, user_info){
  gap_update_df <- user_info %>% group_by(idx) %>% summarise(min_update_gap=min(update_gap),
                                               max_update_gap=max(update_gap),
                                               mean_update_gap=mean(update_gap))
  # dummy_var <- sparse.model.matrix(target ~ -1,
  #               data = df %>% select(idx, userupdateinfo1, target) %>%
  #                 mutate(target=as.factor(target)) %>% distinct())
  dummy_var <- model.matrix(~ -1 + userupdateinfo1, data = user_info %>%
                              select(idx, userupdateinfo1) %>%
                 mutate(userupdateinfo1=as.factor(userupdateinfo1)) %>% distinct())
  dummy_var <- cbind(user_info %>% select(idx, userupdateinfo1) %>% distinct() %>% select(idx),
                     dummy_var)
  dummy_var <- dummy_var %>% group_by(idx) %>% summarise_each(funs(sum))

  gap_log_df <- log_info %>% group_by(idx) %>% summarise(min_log_gap=min(log_gap),
                                               max_log_gap=max(log_gap),
                                               mean_log_gap=mean(log_gap))
  master[master$userinfo_23=="不详", "userinfo_23"] <- 'D'
  master[master$userinfo_22=="不详", "userinfo_22"] <- 'D'
  master_df <- master %>% select(idx, userinfo_22, userinfo_21, userinfo_23, education_info3) %>%
    mutate(userinfo_22=as.factor(userinfo_22), userinfo_23=as.factor(userinfo_23), education_info3=as.factor(education_info3))

  model_df <- master_df %>% left_join(dummy_var) %>% left_join(gap_log_df) %>%
    left_join(gap_update_df)
  return(model_df)
}

log_info <- bind_rows(train_df[[1]], test_df[[1]])
user_info <- bind_rows(train_df[[3]], test_df[[3]])
master <- bind_rows(train_df[[2]] %>% select(-target), test_df[[2]])

model_df <- compute_var(log_info, master, user_info)

# train_model_df <- compute_var(train_df[[1]], train_df[[2]], train_df[[3]]) %>%
#   left_join(train_master %>% select(Idx, target), by=c("idx"="Idx"))
# test_model_df <- compute_var(test_df[[1]], test_df[[2]], test_df[[3]])

train_model_df <- model_df %>% filter(idx %in% train_master$Idx) %>% 
  left_join(train_master %>% select(Idx, target), by=c("idx"="Idx"))
test_model_df <- model_df %>% filter(idx %in% test_master$Idx)

fit_lm <- glm(target ~ ., family=binomial(link="logit"), data = train_model_df[, -c(1, 4)])
p_lm <- predict(fit_lm, newdata = test_model_df, type = "response")

res <- test_model_df %>% select(idx) %>% mutate(score=round(p_lm, 4))
res[is.na(res)] <- 0
names(res)[1] <- 'Idx'
write.csv(res, 'res0319.csv', row.names=F)

# fit_dt <- rpart(target ~ ., data = train_model_df[, -1], 
#                 control = rpart.control(cp = 0.1))
# p_dt <- predict(fit_dt, newdata = test_model_df)
# 
# fit_rf <- randomForest(target ~ ., data = train_model_df[, -1], na.action=na.roughfix)

# train_sparse_matrix <- sparse.model.matrix(target ~.-1, data = train_model_df)
# test_sparse_matrix <- sparse.model.matrix(~.-1, data = test_model_df)
# 
# dtrain <- xgb.DMatrix(train_sparse_matrix, label = train_model_df$target)
# model <- xgboost(data = dtrain, nrounds = 2, objective = "binary:logistic")

sparse_matrix <- sparse.model.matrix()
