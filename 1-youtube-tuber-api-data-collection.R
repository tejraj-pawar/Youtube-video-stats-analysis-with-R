devtools::install_github("soodoku/tuber", build_vignettes = TRUE)

# Libraries
if (!require('tidyverse')) install.packages('tidyverse', quiet = TRUE); library('tidyverse')
if (!require('tuber')) install.packages('tuber', quiet = TRUE); library('tuber')
if (!require('httr')) install.packages('httr', quiet = TRUE); library('httr')
if (!require('jsonlite')) install.packages('jsonlite', quiet = TRUE); library('jsonlite')
if (!require('MazamaCoreUtils')) install.packages('MazamaCoreUtils', quiet = TRUE); library('MazamaCoreUtils') # to check null and replace with default value

# STEP 1: Pre-requisites:
client_id <- "**********"
client_secret <- "**********"

yt_oauth(client_id, client_secret)

# STEP 2: Get videos list for a channel:
tedxChannelVideos = list_channel_videos(channel_id = "UCsT0YIqwnpJCM-mx7-gSA4Q", max_results = 1000)
write.csv(tedxChannelVideos, file = "tedxChannelVideos-13May.csv")

#STEP 3: Call video details API in a batch of 50 videos and parse its JSON response explicitly to create a data frame. (Parsing logic is pending)

# Combining video list fetched on 19th April and 13th April
videos_list_df1 = read_csv('tedxChannelVideos-19April.csv', na = c("", NA, "-1"))
videos_list_df2 = read_csv('tedxChannelVideos-13May.csv', na = c("", NA, "-1"))

glimpse(videos_list_df1)

#Renaming columns
videos_list_df1 = videos_list_df1 %>%
  rename(
    videoId = contentDetails.videoId,
    publishedAt = contentDetails.videoPublishedAt
  )
videos_list_df2 = videos_list_df2 %>%
  rename(
    videoId = contentDetails.videoId,
    publishedAt = contentDetails.videoPublishedAt
  )

#Retrieve video ids and request video details in a batch of 50
#videos_list = as.vector(videos_list_df$videoId)
videos_list = c(as.vector(videos_list_df1$videoId), as.vector(videos_list_df2$videoId))
length(videos_list) # 19638

#Removing duplicates
videos_list = unique(videos_list)
length(videos_list)

# Total number of data fields fetched from Video Details API
nos_of_data_fields = 20

#Function to get video details in a batch 
get_video_details_batch <- function(batch_id, videos_list, batch_index, batch_size) {
  
  start_index = batch_index + 1
  end_index = batch_index + batch_size
  
  # Convert list to string with ',' as a delimiter.
  batchVideoIds = paste(unlist(videos_list[start_index : end_index]), collapse=',')
  
  print(paste("Batch Id: ", batch_id, "Start Index: ", start_index, "End Index: ", end_index))
  #print(paste("Batch Video Ids: ", batchVideoIds))
  
  # Fields to be collected from details API:
  # root.items[] >> id(videoId)
  # root.items[].snippet >> publishedAt(timestamp), title, description, tags(array), category, defaultAudioLanguage
  # root.items[].contentDetails >> duration, dimension, caption, licensedContent
  # root.items[].statistics >> viewCount, likeCount, favoriteCount, commentCount
  # root.items[].status >> privacyStatus, license, embeddable, publicStatsViewable, madeForKids
  
  #videoId = "JKY-hohg8wM,SiNyhtMrC1E"
  videoDetailsAPIUrl = paste("https://www.googleapis.com/youtube/v3/videos?id=", batchVideoIds, "&maxResults=50&part=snippet%2C%20statistics%2C%20contentDetails&key=AIzaSyBDdOSuTNSIAgxIn_tn7AUYP_ES5oKkh-M", sep = "")
  print(paste("Video Details API Url: ", videoDetailsAPIUrl))
  
  response = GET(url = videoDetailsAPIUrl)
  response = content(response)
  #print(response)
  
  # Iterating while loop to retrieve data for individual video from json array. 
  video_index = 1
  temp_video_details_vector = c()
  while(video_index <= length(response$items)) {
    video_details = c(
      
      response$items[[video_index]]$id,
      response$items[[video_index]]$snippet$publishedAt,
      setIfNull(response$items[[video_index]]$snippet$title, "TEDx Talk"),
      setIfNull(response$items[[video_index]]$snippet$description, "TEDx Talk"),
      setIfNull(paste(unlist(response$items[[video_index]]$snippet$tags), collapse=','), ""),
      setIfNull(response$items[[video_index]]$snippet$categoryId, "0"),
      setIfNull(response$items[[video_index]]$snippet$defaultAudioLanguage, "en"),
      response$items[[video_index]]$contentDetails$duration,
      response$items[[video_index]]$contentDetails$dimension,
      setIfNull(response$items[[video_index]]$contentDetails$caption, FALSE),
      setIfNull(response$items[[video_index]]$contentDetails$licensedContent, FALSE),
      setIfNull(response$items[[video_index]]$statistics$viewCount, "0"),
      setIfNull(response$items[[video_index]]$statistics$likeCount, "0"),
      setIfNull(response$items[[video_index]]$statistics$favoriteCount, "0"),
      setIfNull(response$items[[video_index]]$statistics$commentCount, "0"),
      setIfNull(response$items[[video_index]]$status$privacyStatus, "public"),
      setIfNull(response$items[[video_index]]$status$license, "youtube"),
      setIfNull(response$items[[video_index]]$status$embeddable, FALSE),
      setIfNull(response$items[[video_index]]$status$publicStatsViewable, TRUE),
      setIfNull(response$items[[video_index]]$status$madeForKids, FALSE)
    )
    
    # Collecting all video details in single vector
    temp_video_details_vector = c(temp_video_details_vector, video_details)
    
    video_index = video_index + 1
  }
  print(length(temp_video_details_vector))
  
  temp_video_details_vector
}

videos_list_len = length(videos_list)
video_details_vector = c()

# Iterating loop to call video details API in a batch of 50
batch_size = 50
batch_index = 0
batch_id = 1

while (batch_index + batch_size <= videos_list_len) {
  start_time = Sys.time()
  
  video_detail_batch = get_video_details_batch(batch_id, videos_list, batch_index, batch_size)
  
  video_details_vector = c(video_details_vector, video_detail_batch)
  
  print(paste("Total number of videos processed till now: ", (length(video_details_vector) / nos_of_data_fields)))
  
  end_time = Sys.time()
  execution_time = paste(round(as.numeric(difftime(time1 = end_time, time2 = start_time, units = "secs")), 1), " Seconds")
  print(paste("Batch Id: ", batch_id, " Completed.", "Execution Time: ", execution_time))
  
  print("=======================================================================")
  print("=======================================================================")
  
  batch_index = batch_index + batch_size
  batch_id = batch_id + 1
}

# Handling corner case
if(videos_list_len - batch_index > 0) { 
  video_details_vector = c(video_details_vector, get_video_details_batch((batch_id), videos_list, batch_index, (videos_list_len - batch_index)))
}

print(paste("Total number of videos processed: ", (length(video_details_vector) / nos_of_data_fields)))

# Here, 20 is the total number of fields we are extracting from API response.
video_details_matrix = array(video_details_vector, dim = c(nos_of_data_fields, (length(video_details_vector) / nos_of_data_fields)))

video_details_data_frame <- as.data.frame(t(video_details_matrix))

# Renaming the data frame column names
video_details_data_frame = video_details_data_frame %>% 
  rename(
    Id = V1,
    Published_Time = V2,
    Title = V3,
    Description = V4,
    Tags = V5,
    Category_Id = V6,
    Default_Audio_Language = V7,
    Duration = V8,
    Dimension = V9,
    Caption = V10,
    Licensed_Content = V11,
    View_Count = V12,
    Like_Count = V13,
    Favorite_Count = V14,
    Comment_Count = V15,
    Privacy_Status = V16,
    License = V17,
    Embeddable = V18,
    Public_Stats_Viewable = V19,
    Made_For_Kids = V20
  )

# Persisting data into CSV
write_csv(video_details_data_frame, "Youtube-Tedx_Channel_Videos_Details.csv")
