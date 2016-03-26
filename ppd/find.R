train_master %>% select(one_of(paste0('UserInfo_', c(2, 4, 7, 8, 9, 19,20, 24)))) %>%
  filter(UserInfo_24!='D') %>% head(1000) %>% View

train_master %>% select(one_of(paste0('UserInfo_', c(2, 4, 8, 20)))) %>% head(1000) %>% View

train_master %>% select(starts_with('thirdparty_info')) %>% head()
sapply(train_master %>% select(starts_with('thirdparty_info')), n_distinct)

train_master %>% select(starts_with('Weblog')) %>%
  head(1000) %>% View
train_master %>% select(starts_with('Educa')) %>% filter(Education_Info1==0) %$% table(Education_Info3)

train_master %>% select(Education_Info3, UserInfo_23) %>% filter(Education_Info3=="毕业") %$%
  unique(UserInfo_23)

train_master %>% select(starts_with('third')) %>%
  head(1000) %>% View

sapply(master %>% select(starts_with('socialnetwork')), n_distinct)
sapply(master %>% select(starts_with('third')), function(x) sum(is.na(x)) / 30000)

table(train_master$ThirdParty_Info_Period2_6)

prop.table(table(train_master$SocialNetwork_1,
                 train_master$target), margin = 1)

cor(train_master %>% select(starts_with('socialnetwork')))

count(master, thirdparty_info_period3_10) %>% arrange(desc(n))

lola <- readLines('lola.txt')
lola <- lola[grepl('[0-9]', lola)]
lola <- gsub('(北纬)|(东经)', '', lola)
lola <- lapply(lola, function(x) strsplit(str_trim(x), ' ')[[1]])
lola_df <- do.call(rbind, lola) %>% as.data.frame()
names(lola_df) <- c('city', 'latitude', 'longitude')
lola_df <- lola_df %>% mutate(city=sub('市', '', as.character(city)),
                              latitude=as.numeric(as.character(latitude)),
                              longitude=as.numeric(as.character(longitude)))
# save(lola_df, file="lola_df.RData")

# socialnetwork_2 socialnetwork_7 socialnetwork_12 
# socialnetwork_15 socialnetwork_16


