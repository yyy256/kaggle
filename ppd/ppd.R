# setwd('d:/kaggle/ppd')
setwd('kaggle/ppd/')
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(xgboost)
library(Matrix)
# library(randomForest)
# library(rpart)
# library(ROCR)
# library(caret)
# library(glmnet)
# library(DMwR)
load('mz.RData')
load('lola_df.RData')
load('city_info.RData')


train_log_info <- read.csv("./Training Set/PPD_LogInfo_3_1_Training_Set.csv", as.is = T)
train_master <- read.csv("./Training Set/PPD_Training_Master_GBK_3_1_Training_Set.csv", fileEncoding='gbk', as.is = T)
train_user_info <- read.csv("./Training Set/PPD_Userupdate_Info_3_1_Training_Set.csv", as.is = T)

test_log_info <- read.csv("./Test Set/PPD_LogInfo_2_Test_Set.csv", as.is = T)
test_master <- read.csv("./Test Set/PPD_Master_GBK_2_Test_Set.csv", fileEncoding='gbk', as.is = T)
test_user_info <- read.csv("./Test Set/PPD_Userupdate_Info_2_Test_Set.csv")

train_master$ListingInfo <- ymd(train_master$ListingInfo)
test_master$ListingInfo <- dmy(test_master$ListingInfo)

# 将大于10种的分段
binning_name <- names(train_master)[sapply(train_master, function(x) n_distinct(x) > 10 & !is.character(x))]
binning_name <- binning_name[-c(1, length(binning_name))]
binning_func <- function(x) {
  result=smbinning(df=train_master,y="target",x=x,p=0.05) 
  if (result=="No Bins") NULL else result$bands
}
bands <- lapply(binning_name, binning_func)

log_info <- bind_rows(train_log_info, test_log_info)
master <- bind_rows(train_master %>% select(-target), test_master)
user_info <- bind_rows(train_user_info, test_user_info)

clean_city_func <- function(x){
  x <- sub('(市)|(自治州)', '', x)

  for (n in mz){
    x <- sub(n, '', x)
  }
  return (x)
}

clean_province_func <- function(x){
  sub('(省)|(自治区)|(壮族)(回族)|(维吾尔)', '', x)
}

city_name <- c('userinfo_2', 'userinfo_4', 'userinfo_8', 'userinfo_20')
province_name <- c('userinfo_7', 'userinfo_19')

pre_process_func <- function(log_info, master, user_info){
  names(log_info) <- tolower(names(log_info))
  names(master) <- tolower(names(master))
  names(user_info) <- tolower(names(user_info))

  log_info$listinginfo1 <- ymd(log_info$listinginfo1)
  log_info$loginfo3 <- ymd(log_info$loginfo3)
  user_info$listinginfo1 <- ymd(user_info$listinginfo1)
  user_info$userupdateinfo2 <- ymd(user_info$userupdateinfo2)

  master$year <- as.factor(year(master$listinginfo))
  master$month <- as.factor(month(master$listinginfo))
  master$day <- as.factor(day(master$listinginfo))

  # 合并一些类别
  master[master$socialnetwork_1 != 0, 'socialnetwork_1'] <- 1
  master[master$socialnetwork_13 != 0, 'socialnetwork_13'] <- 1
  master[master$socialnetwork_14 != 0, 'socialnetwork_14'] <- 1
  master[master$socialnetwork_17 != 0, 'socialnetwork_17'] <- 1
  master[master$socialnetwork_3 != -1, 'socialnetwork_3'] <- 0
  master[master$socialnetwork_4 != -1, 'socialnetwork_4'] <- 0
  master[master$socialnetwork_5 != -1, 'socialnetwork_5'] <- 0
  master[master$socialnetwork_6 != -1, 'socialnetwork_6'] <- 0
  master[master$socialnetwork_8 != -1, 'socialnetwork_8'] <- 0
  master[master$socialnetwork_9 != -1, 'socialnetwork_9'] <- 0
  master[master$socialnetwork_10 != -1, 'socialnetwork_10'] <- 0
  master[master$socialnetwork_11 != -1, 'socialnetwork_11'] <- 0
  # 要去掉的变量userinfo_3, userinfo_11, userinfo_12, userinfo_13, userinfo_24, 教育只取Education_Info3,
  # WeblogInfo_1 WeblogInfo_3去掉空值太多
  delete_vars <- c('userinfo_3', 'userinfo_11', 'userinfo_12', 'userinfo_13', 'userinfo_24', paste0('education_info', c(1:2, 4:8)),
    'webloginfo_1', 'webloginfo_3')
  master <- master %>% select(-one_of(delete_vars))

  i <- names(master) %in% city_name
  master[i] <- lapply(master[i], clean_city_func)

  i <- names(master) %in% province_name
  master[i] <- lapply(master[i], clean_province_func)

  # # 把UserInfo_19里面的省、市、自治区这几个字去掉
  # master$userinfo_19 <- gsub('(省)|(市)|(自治区)|(维吾尔)|(回族)|(壮族)', '', master$userinfo_19)
  # UserInfo_7是否等于UserInfo_19
  master$is_equal_userinfo7_userinfo19 <- as.numeric(master$userinfo_7==master$userinfo_19)
  master$is_equal_userinfo2_userinfo4 <- as.numeric(master$userinfo_2==master$userinfo_4)

  # UserInfo_3区分度似乎不是很大 UserInfo_11-13没用
  # UserInfo_9里面的两边空格去掉
  master$userinfo_9 <- str_trim(master$userinfo_9)
  # UserInfo_8去掉市
  # UserInfo_22婚姻信息 不详算成D，新增一列初婚、再婚算成已婚，离婚算成未婚
  master[master$userinfo_22=='不详', 'new_userinfo_22'] <- 'D'
  master[master$userinfo_22=='离婚', 'new_userinfo_22'] <- '未婚'
  master[master$userinfo_22 %in% c('初婚', '再婚'), 'new_userinfo_22'] <- '已婚'
  master[is.na(master$new_userinfo_22), 'new_userinfo_22'] <- master[is.na(master$new_userinfo_22), 'userinfo_22']
  # UserInfo_23教育信息，合并一部分信息
  master[master$userinfo_23=='不详', 'userinfo_23'] <- 'D'
  master[!master$userinfo_23 %in% c('D', 'G', 'AB', 'O', 'M', '不详', 'AK', 'H', 'Y', '专科毕业', '大学本科（简称“大学'), 'userinfo_23'] <- 'other'

  # UserInfo_1, UserInfo_5, UserInfo_6, UserInfo_14，UserInfo_15，UserInfo_14-17应该看作因子变量
  i <- names(master) %in% c(paste0('userinfo_', c(1,5,6,14:17)), paste0('socialnetwork', 1:17))
  master[i] <- lapply(master[i], as.factor)

  # 将修改内容都变成小写，去掉两边空格
  user_info$userupdateinfo1 <- str_trim(tolower(user_info$userupdateinfo1))

  user_info <- user_info %>%
    mutate(update_gap = as.numeric(difftime(listinginfo1, userupdateinfo2, units='days')))
  log_info <- log_info %>%
    mutate(log_gap = as.numeric(difftime(listinginfo1, loginfo3, units='days')))
  return (list(log_info, master, user_info))
}

df <- pre_process_func(log_info, master, user_info)



# UserInfo_2,4,8,20是否相等
# 城市等级
# 是否为省会
# SocialNetwork_1去掉, SocialNetwork_3 -1为一类，其它为一类
# 不用第三方数据(thirdparty)做一个模型
# 用最重要的变量做个随机森林

compute_var <- function(log_info, master, user_info){
  # 购买时间间隔；修改过几次lastupdatedate, realname, districtid, provinceid, qq
  gap_update_df <- user_info %>% group_by(idx) %>% summarise(
    # num_lastupdatedate=sum(userupdateinfo1=='lastupdatedate'),
    # num_realname=sum(userupdateinfo1=='realname'),
    # num_districtid=sum(userupdateinfo1=='districtid'),
    # num_provinceid=sum(userupdateinfo1=='provinceid'),
    # num_qq=sum(userupdateinfo1=='qq'),
    m1=max(userupdateinfo2),
    m2=min(userupdateinfo2),
    f_user=n_distinct(userupdateinfo2)-1,
    min_update_gap=min(update_gap),
    max_update_gap=max(update_gap),
    mean_update_gap=mean(update_gap)) %>%
    mutate(m_update_gap=as.numeric(difftime(m1, m2, units='days')/f_user)) %>%
    select(-m1, -m2)
  gap_update_df[is.na(gap_update_df)] <- 0
  # dummy_var <- sparse.model.matrix(target ~ -1,
  #               data = df %>% select(idx, userupdateinfo1, target) %>%
  #                 mutate(target=as.factor(target)) %>% distinct())

  dummy_var <- model.matrix(~ -1 + userupdateinfo1, data = user_info %>%
                              select(idx, userupdateinfo1) %>%
                 mutate(userupdateinfo1=as.factor(userupdateinfo1)) %>% distinct())
  dummy_var <- cbind(user_info %>% select(idx, userupdateinfo1) %>% distinct() %>% select(idx),
                     dummy_var)
  dummy_var <- dummy_var %>% group_by(idx) %>% summarise_each(funs(sum))

  log_dummy_var <- model.matrix(~ -1 + loginfo1 + loginfo2, data = log_info %>%
                              select(idx, loginfo1, loginfo2) %>%
                 mutate(loginfo1=as.factor(loginfo1), loginfo2=as.factor(loginfo2)) %>% distinct())
  log_dummy_var <- cbind(log_info %>% select(idx, loginfo1, loginfo2) %>% distinct() %>% select(idx),
                     log_dummy_var)
  log_dummy_var <- log_dummy_var %>% group_by(idx) %>% summarise_each(funs(sum))

  gap_log_df <- log_info %>% group_by(idx) %>% summarise(min_log_gap=min(log_gap),
                                               max_log_gap=max(log_gap),
                                               mean_log_gap=mean(log_gap),
                                               m1=max(loginfo3),
                                               m2=min(loginfo3),
                                               f_log=n_distinct(loginfo3)-1) %>%
    mutate(m_log_gap=as.numeric(difftime(m1, m2, units='days'))/f_log) %>% select(-m1, -m2)
  gap_log_df[is.na(gap_log_df)] <- 0
  # master_df <- master %>% select(idx, userinfo_22, userinfo_21, userinfo_23, education_info3) %>%
  #   mutate(userinfo_22=as.factor(userinfo_22), userinfo_23=as.factor(userinfo_23), education_info3=as.factor(education_info3))
  vars <- names(master)[sapply(master, function(x) sum(is.na(x))==0)]
  master_df <- master %>% select(one_of(vars), -listinginfo)
  i <- sapply(master_df, is.character)
  master_df[i] <- lapply(master_df[i], as.factor)
  model_df <- master_df %>% left_join(dummy_var) %>% left_join(log_dummy_var) %>% left_join(gap_log_df) %>%
    left_join(gap_update_df)
  model_df$year
  return(model_df)
}

# 标准化
scale_func <- function(x){
  (x-min(x)) / (max(x)-min(x))
}

# log_info <- bind_rows(train_df[[1]], test_df[[1]])
# user_info <- bind_rows(train_df[[3]], test_df[[3]])
# master <- bind_rows(train_df[[2]] %>% select(-target), test_df[[2]])

model_df <- compute_var(df[[1]], df[[2]], df[[3]])

# 填补缺失值
model_df[is.na(model_df$min_log_gap), 'min_log_gap'] <- median(model_df$min_log_gap, na.rm = T)
model_df[is.na(model_df$max_log_gap), 'max_log_gap'] <- median(model_df$max_log_gap, na.rm = T)
model_df[is.na(model_df$mean_log_gap), 'mean_log_gap'] <- median(model_df$mean_log_gap, na.rm = T)
model_df[is.na(model_df$min_update_gap), 'min_update_gap'] <- median(model_df$min_update_gap, na.rm = T)
model_df[is.na(model_df$max_update_gap), 'max_update_gap'] <- median(model_df$max_update_gap, na.rm = T)
model_df[is.na(model_df$mean_update_gap), 'mean_update_gap'] <- median(model_df$mean_update_gap, na.rm = T)

model_df[is.na(model_df$f_log), 'f_log'] <- median(model_df$f_log, na.rm = T)
model_df[is.na(model_df$m_log_gap), 'm_log_gap'] <- median(model_df$m_log_gap, na.rm = T)
model_df[is.na(model_df$f_user), 'f_user'] <- median(model_df$f_user, na.rm = T)
model_df[is.na(model_df$f_log), 'm_update_gap'] <- median(model_df$m_update_gap, na.rm = T)

# model_df$min_log_gap <- scale_func(model_df$min_log_gap)
# model_df$max_log_gap <- scale_func(model_df$max_log_gap)
# model_df$mean_log_gap <- scale_func(model_df$mean_log_gap)
# model_df$min_update_gap <- scale_func(model_df$min_update_gap)
# model_df$max_update_gap <- scale_func(model_df$max_update_gap)
# model_df$mean_update_gap <- scale_func(model_df$mean_update_gap)
# model_df$thirdparty_info_period2_3_log <- log1p(scale_func(model_df$thirdparty_info_period2_3))
# model_df$thirdparty_info_period4_15_log <- log1p(scale_func(model_df$thirdparty_info_period4_15))
# model_df$thirdparty_info_period2_15_log <- log1p(scale_func(model_df$thirdparty_info_period2_15))
# model_df$thirdparty_info_period4_2_log <- log1p(scale_func(model_df$thirdparty_info_period4_2))

# thirdparty_info都应该标准化
# i <- substr(names(model_df), 1, 10) == 'thirdparty'
# model_df[i] <- lapply(model_df[i], scale_func)
model_df[is.na(model_df)] <- 0
# model_df <- model_df %>% left_join(lola_df, by=c('userinfo_2'='city'))
# sparse_matrix_df <- sparse.model.matrix(~.-1, model_df)
names(model_df)[match(c("loginfo1-10", "loginfo1-4"), names(model_df))] <- c('loginfo1_10', 'loginfo1_4')

# i <- sapply(model_df, is.factor)
# model_df[i] <- lapply(model_df[i], as.numeric)
#
# sub_model_df <- model_df %>% select(-starts_with('loginfo'), -starts_with('userupdateinfo'))
# xgb_data <- xgb.DMatrix(data=as.matrix(sub_model_df %>% filter(idx %in% train_master$Idx) %>% select(-idx)),
#                         label = train_master$target)
# bst <- xgb.cv(data=xgb_data, nfold = 10, eta = 0.1,
#               nrounds = 5000, max.depth = 15, objective = "binary:logistic", eval_metric = "auc",
#               early.stop.round = 200, scale_pos_weight =100)
# # train-auc:0.920227+0.001065	test-auc:0.747250+0.013514
# bst <- xgb.train(data=xgb_data, eta = 0.1,
#               nround = 100, max.depth = 15, objective = "binary:logistic", eval_metric = "auc",
#               scale_pos_weight =0.01)
#
# importance_matrix <- xgb.importance(names(model_df), model = bst)

train_model_df <- model_df %>% filter(idx %in% train_master$Idx) %>%
  left_join(train_master %>% select(Idx, target), by=c("idx"="Idx"))
test_model_df <- model_df %>% filter(idx %in% test_master$Idx)

train_sparse_matrix <- sparse.model.matrix(target ~ .-1-idx, train_model_df)
test_sparse_matrix <- sparse.model.matrix(~ .-1-idx, test_model_df)

bst <- xgb.cv(data = train_sparse_matrix, label = train_model_df$target, nfold = 10, eta = 0.1,
              nrounds = 5000, max.depth = 10, objective = "binary:logistic", eval_metric = "auc",
              early.stop.round = 60, scale_pos_weight = 0.01)
# 0.748784
# Best iteration: 331
mst <- xgboost(data = train_sparse_matrix, label = train_model_df$target, eta = 0.1,
              nround = 331, max.depth = 9, objective = "binary:logistic", eval_metric = "auc",
              scale_pos_weight = 0.01, verbose = 0)
importance_matrix <- xgb.importance(train_sparse_matrix@Dimnames[[2]], model = mst)
########################## work
# 经济增长率
# thirdparty_df <- model_df %>% select(starts_with('thirdparty'))
# names(thirdparty_df) <- paste0(names(thirdparty_df), '_init')
#
# num_func <- function(x){
#     quan3 <- quantile(x, 0.8, na.rm=T)
#     as.factor(ifelse(x > quan3, -2, x))
# }
# i <- grepl('thirdparty_info', names(model_df))
# model_df[i] <- lapply(model_df[i], num_func)
#
# model_df <- bind_cols(model_df, thirdparty_df)
# names(model_df) <- gsub('-', '_', names(model_df))
# # model_df$category1 <- as.factor(ifelse(model_df$thirdparty_info_period2_2 > 68, 99999,
# #                              model_df$thirdparty_info_period2_2))
# bst <- xgboost(data = train_sparse_matrix, label = train_model_df$target, eta = 0.1,
#               nround = 100, max.depth = 15, objective = "binary:logistic", eval_metric = "auc",
#               scale_pos_weight = 0.01)
importance_matrix <- xgb.importance(train_sparse_matrix@Dimnames[[2]], model = bst)
View(importance_matrix)
xgb.plot.deepness(model = mst)
p <- predict(mst, test_sparse_matrix)
daily_df <- read.csv('first round test data/daily_test.csv')
final_df <- read.csv('first round test data/final_test.csv')
test_df <- bind_rows(daily_df, final_df) %>% left_join(data_frame(Idx=test_model_df$idx, p=p))
Metrics::auc(test_df %>% filter(idx %in% final_df$idx) %$% target, test_df%>% filter(idx %in% final_df$idx) %$% p) # 0.7500642


########################## work
# # 0.751307
# bstt <- xgb.cv(data = train_sparse_matrix, label = train_model_df$target, nfold = 10, eta = 0.01,
#               nrounds = 500000, max.depth = 20, objective = "binary:logistic", eval_metric = "auc",
#               early.stop.round = 500, scale_pos_weight = 0.01)
# Sys.time()
# bsttt <- xgb.cv(data = train_sparse_matrix, label = train_model_df$target, nfold = 10, eta = 0.01,
#                nrounds = 500000, max.depth = 30, objective = "binary:logistic", eval_metric = "auc",
#                early.stop.round = 500, scale_pos_weight = 0.005)
# # Sys
# bsttt <- xgb.cv(data = train_sparse_matrix, label = train_model_df$target, nfold = 10, eta = 0.01,
#                 nrounds = 500000, max.depth = 25, objective = "binary:logistic", eval_metric = "auc",
#                 early.stop.round = 500, scale_pos_weight = 0.02)
# Sys.time()
#
#
# bst <- xgb.cv(data = train_sparse_matrix, label = train_model_df$target, nfold = 10, eta = 0.1,
#               nrounds = 5000, max.depth = 30, objective = "binary:logistic", eval_metric = "auc",
#               early.stop.round = 100, scale_pos_weight = 0.005)
#
# fitControl <- trainControl(method = "cv", number = 10, repeats = 1, search = "random")
# # train a xgbTree model using caret::train
# model <- train(train_sparse_matrix, train_model_df$target,
#                method = "xgbTree", trControl = fitControl)
#
# # 0.743673+0.007338
# # 0.741335+0.010964
# # 0.747451+0.008305
# # 0.749717+0.018587
# # [2369]	train-auc:0.868506+0.000843	test-auc:0.749920+0.015443
# bst <- xgboost(data = train_sparse_matrix, label = train_model_df$target, max.depth = 9,
#                eta = 0.1, nround = 277,objective = "reg:logistic", eval_metric = "auc",
#                scale_pos_weight = 0.01, seed=824)
# bst1 <- xgboost(data = train_sparse_matrix, label = train_model_df$target, eta = 0.1,
#                 nround = 328, max.depth = 50, objective = "reg:logistic", eval_metric = "auc",
#                scale_pos_weight = 0.01,
#                 subsample=0.9, colsample_bytree=0.8, seed=33)
# bst2 <- xgboost(data = train_sparse_matrix, label = train_model_df$target, eta = 0.05,
#               nrounds = 700, max.depth = 15, objective = "binary:logistic", eval_metric = "auc",
#               scale_pos_weight = 0.01)
#
# bst_rf <- xgb.cv(data = train_sparse_matrix[, importance_matrix$Feature], label = train_model_df$target,  max.depth = 35,
#                num_parallel_tree = 1000, subsample = 0.5, colsample_bytree =0.5, nrounds = 5000,
#                objective = "binary:logistic", early.stop.round = 50, eval_metric = "auc",nfold = 10)
# # xgb.plot.deepness(model = bst)
# p1 <- predict(bst, test_sparse_matrix) # 0.747745+0.00
# p2 <- predict(bst, test_sparse_matrix) # :0.745305+0.017916
# # p3 <- predict(bst, test_sparse_matrix) # 0.747745+0.00
#
# best1 <- read.csv('res0324.csv')
# best2 <- read.csv('res12016-03-27.csv')
#
# p <- best1$score*0.4 + p1*0.4 + best2$score*0.1 + p2 * 0.1
#
# p_bst_train <- predict(bst, train_sparse_matrix)
#
# importance_matrix <- xgb.importance(train_sparse_matrix@Dimnames[[2]], model = bst)
# xgb.plot.importance(importance_matrix)
#
# glm_matrix <- train_sparse_matrix[, importance_matrix$Feature]
#
# # inTrain <- createDataPartition(y = train_model_df$target, p = .75, list = FALSE)
#
#
# cctrl1 <- trainControl(method = "cv", number = 5,
#                        # returnResamp = "all",
#                        classProbs = T, summaryFunction = twoClassSummary)
#                        # sampling = "smote")
#
# # trainY <- factor(train_model_df$target, labels=c('no', 'yes'))
#
# glm_data <- cbind(as.matrix(glm_matrix), trainY=train_model_df$target) %>% as.data.frame
# glm_data$trainY <- factor(glm_data$trainY, labels=c('no', 'yes'))
# smote_train <- SMOTE(trainY ~ ., data  = glm_data)
#
# set.seed(849)
# test_class_cv_model <- train(trainY ~ ., smote_train,
#                              method = "glmnet",
#                              trControl = cctrl1,
#                              metric = "ROC",
#                              # preProc = c("center", "scale"),
#                              tuneGrid = expand.grid(.alpha = seq(.001, 0.005, length = 3),
#                                                     .lambda = c((1:2)/20)))
#
# p_train <- predict(test_class_cv_model, glm_data, type='prob')[, 2]
# p_glm <- predict(test_class_cv_model,
#                  newdata=as.data.frame(as.matrix(test_sparse_matrix)), type = "prob")[, 2]
# p <- p_glm[,2]
# # fit_lm <- glm(target ~ ., family=binomial(link="logit"), data = train_model_df[, -c(1, 4)])
# # train_p_lm <- predict(fit_lm, newdata = train_model_df, type = "response")
# # p_lm <- predict(fit_lm, newdata = test_model_df, type = "response")
#
# # glm_data$trainY
calc_auc_func <- function(p, real) {
  pred <- prediction(p, real)
  perf <- performance(pred, "auc")
  perf@y.values[[1]]
}
#
# p <- p1*0.999 + p_glm*0.001
res <- test_model_df %>% select(idx) %>% mutate(score=round(p, 4))
res[is.na(res)] <- 0
names(res)[1] <- 'Idx'
write.csv(res, 'res.csv', row.names=F)

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

# sparse_matrix <- sparse.model.matrix()
# # 2013年可能技术不完善，违约高
# # 不同月份的违约也是不一样的，12月违约高
# # 填补缺失值改一下，不用的字段去掉
#
# userinfo_19
# userinfo_7
# # gdp数据
# # 经纬度聚类
# 直辖市 省会 人口数量 东部西部 沿海与否