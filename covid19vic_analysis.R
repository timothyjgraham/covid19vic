# Covid19Vic analysis 

require(tweetbotornot2)
require(glue)
require(dplyr)
require(rtweet)
library(tidyverse)    
library(stringr)    

api_key <- "INSERT HERE"
api_secret_key <- "INSERT HERE"
access_token <- "INSERT HERE"
access_token_secret <- "INSERT HERE"

token <- create_token(
  app = "INSERT HERE",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

######################################################
#### COLLECTION - tweets containing #covid19vic
tweet_search_covid19vic <- search_tweets('#covid19vic', n = 18000, include_rts = TRUE, retryonratelimit = TRUE)
saveRDS(tweet_search_covid19vic, paste0(Sys.time()," tweet_search_covid19vic.rds"))
# length(unique(tweet_search_covid19vic$screen_name))

# SAVE TO DISK
library(dplyr)
df_combined_covid19vic <- tweet_search_covid19vic %>% distinct(status_id, .keep_all = TRUE)
dim(df_combined_covid19vic)
# subset only the columns we want to save to disk 
df_combined_covid19vic_TO_DISK <- df_combined_covid19vic[,c(1:6,14:16,48:62,63:66,82:83)]
write.csv(df_combined_covid19vic_TO_DISK,paste0(Sys.time()," tweet_search_covid19vic.csv"),row.names = F)
# write tweet IDs to disk
write.table(df_combined_covid19vic$status_id,paste0(Sys.time(),"_covid19vic_tweet_ids.csv"), row.names = F, col.names = F, sep=",")

# USER AND BOT ANALYSIS

userids_covid19vic2 <- unique(df_combined_covid19vic$user_id)
# collect timeline data (latest 500 tweets), to feed into the bot prediction model
# we have to use a custom function to avoid a curl error with rate limits
# from here: https://github.com/ropensci/rtweet/issues/266#issuecomment-471092678 
get_timeline_unlimited <- function(users, n){
  
  if (length(users) ==0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline")
  
  if (length(users) <= rl$remaining){
    print(glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + 0.1
    print(glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}

# just analyse the first 1000 most active users (by tweet frequency), due to rate limits 
userids_covid19vic_TOP1000_most_active <- sort(table(df_combined_covid19vic$user_id),decreasing = T)
userids_covid19vic_TOP1000_most_active <- names(userids_covid19vic_TOP1000_most_active[1:1000])

covid19vic_user_timelines2 <- get_timeline_unlimited(userids_covid19vic_TOP1000_most_active,n=200)
saveRDS(covid19vic_user_timelines2,paste0(Sys.time(),"_covid19vic_user_timelines.rds")) # save data to disk 

# run tweetbotornot2 predictions 
bot_results_covid19vic2 <- predict_bot(covid19vic_user_timelines2)
bot_results_covid19vic2$screen_name[which(bot_results_covid19vic2$prob_bot > 0.5)]
write.csv(bot_results_covid19vic2,"bot_results_covid19vic2.csv",row.names = F)

# how many tweets did the suspected bot accounts send? 
# length(which(df_combined_covid19vic$screen_name=="Freddyfuddrucke"))
# length(which(df_combined_covid19vic$screen_name=="AppleRTweet"))
# length(which(df_combined_covid19vic$screen_name=="viralvideovlogs"))

#### Construct retweet network edge list (for visualising in Gephi)

df_edgelist_retweet_covid19vic <- data.frame(
  source=df_combined_covid19vic$screen_name,
  target=df_combined_covid19vic$retweet_screen_name, 
  stringsAsFactors = F)

write.csv(df_edgelist_retweet_covid19vic,"df_edgelist_retweet_covid19vic.csv",row.names = F)

#### Construct user to hashtag network edge list

text_cleaned <- tolower(df_combined_covid19vic$text)

text_cleaned <- gsub("\\n"," ",text_cleaned)

hashtag_mentions <- function(x){
  xx <- strsplit(x, " ")
  gsub(':','',unlist(lapply(xx, function(xx)xx[grepl("#[[:alnum:]]", xx)]))) # we remove the colon (which comes after 'RT @someone:')
}

# test on first user 
hashtag_mentions(text_cleaned[1])

require(data.table)
# preallocate dataframe for speed 
df_edgelist_hashtags_covid19vic <- data.table(source=rep("foo",10000000),
                                   target=rep("foo",10000000))

setkey(df_edgelist_hashtags_covid19vic)

currRow <- 1
for (i in 1:nrow(df_combined_covid19vic)) {
  if(i %% 100 == 0) {
    print(i)
  }
  mentions_temp <- hashtag_mentions(text_cleaned[i])
  
  if(length(mentions_temp)==0) {
    next
  }
  
  # tempVar <- unlist(strsplit(x = df_combined_covid19vic$mentions[i],","))
  mentions_temp <- str_trim(mentions_temp, side = c("both")) # remove leading whitespace 
  
  
  for (j in 1:length(mentions_temp)) {
    df_edgelist_hashtags_covid19vic[currRow,source := df_combined_covid19vic$screen_name[i]]
    df_edgelist_hashtags_covid19vic[currRow,target := mentions_temp[j]]
    currRow <- currRow + 1
  }
  
}

dim(df_edgelist_hashtags_covid19vic)
toDel <- which(df_edgelist_hashtags_covid19vic$source=="foo")
length(toDel)
df_edgelist_hashtags_covid19vic <- df_edgelist_hashtags_covid19vic[-toDel,]

df_edgelist_hashtags_covid19vic[1581,]

# remove problematic characters 
df_edgelist_hashtags_covid19vic$target <- gsub("\\\\","",df_edgelist_hashtags_covid19vic$target)
df_edgelist_hashtags_covid19vic$source <- gsub("\\\\","",df_edgelist_hashtags_covid19vic$source)
# remove leading and trailing whitespace 
df_edgelist_hashtags_covid19vic$target <- str_trim(df_edgelist_hashtags_covid19vic$target, side = c("both")) # remove leading whitespace 
df_edgelist_hashtags_covid19vic$source <- str_trim(df_edgelist_hashtags_covid19vic$source, side = c("both")) # remove leading whitespace 

write.csv(df_edgelist_hashtags_covid19vic,"df_edgelist_retweet_covid19vic.csv",row.names = F)
