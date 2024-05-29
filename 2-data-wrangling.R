if (!require('tidyverse')) install.packages('tidyverse', quiet = TRUE); library('tidyverse')
if (!require('tidyr')) install.packages('tidyr', quiet = TRUE); library('tidyr')
if (!require('dplyr')) install.packages('dplyr', quiet = TRUE); library('dplyr')
if (!require('skimr')) install.packages('skimr', quiet = TRUE); library('skimr')
if (!require('recipes')) install.packages('recipes', quiet = TRUE); library('recipes')
if (!require('lubridate')) install.packages('lubridate', quiet = TRUE); library('lubridate')

# Reading the csv data
video_details_df <- read_csv("TedxChannelVideosUpto13April.csv")
skim(video_details_df)

# Dropping "Favorite_Count" columns
video_details_df <- select(video_details_df, -Favorite_Count)

#checking null values
null_values <- is.na(video_details_df)
skim(null_values)

# Categorize published time and rename it to Utc_Day_Part
Utc_Day_Part <- function(Published_Time) {
  paste(
    c("Night", "Morning", "Afternoon", "Evening", "Night")[
      cut(as.numeric(format(Published_Time, "%H%M")), c(0, 530, 1159, 1700 ,2100, 2359))
    ]
  )
}
video_details_df <- cbind(video_details_df[, 1:2], Utc_Day_Part=Utc_Day_Part(video_details_df$Published_Time), video_details_df[, 3:ncol(video_details_df)])

#Extracting Day of Week from Published Time
Day_Of_Week <- weekdays(video_details_df$Published_Time)
video_details_df <- cbind(video_details_df[, 1:3], Day_Of_Week, video_details_df[, 4:ncol(video_details_df)])

#Extracting Month from Published Time
Month <- months(video_details_df$Published_Time)
video_details_df <- cbind(video_details_df[, 1:3], Month, video_details_df[, 4:ncol(video_details_df)])

#Extracting Minutes from Duration
Duration_Minutes <- as.numeric(substr(video_details_df$Duration, 3, regexpr("M", video_details_df$Duration) - 1))
video_details_df <- cbind(video_details_df[, 1:9], Duration_Minutes, video_details_df[, 10:ncol(video_details_df)])
video_details_df <- select(video_details_df, -Duration)

#Pre-Processing Tags column
clean_tags<-function(tags){
  tag_elements <- unlist(strsplit(tags, ","))
  cleaned_elements <- tag_elements[-grep("TEDxTalks|\\[(.*?)\\]|\\((.*?)\\)", tag_elements)]
  cleaned_tag <- paste(cleaned_elements, collapse = ",")
}
invisible(clean_tags(video_details_df$Tags))
video_details_df$Tags <- sapply(video_details_df$Tags, clean_tags)

# Removing columns with little to no variance
unique(video_details_df$Category_Id)
unique(video_details_df$Privacy_Status)
unique(video_details_df$License)
unique(video_details_df$Licensed_Content)
unique(video_details_df$Embeddable)
unique(video_details_df$Public_Stats_Viewable)
unique(video_details_df$Made_For_Kids)
unique(video_details_df$Dimension)
video_details_df <- select(video_details_df, -c(Id, Category_Id, Privacy_Status, License, Licensed_Content, Dimension, Embeddable, Public_Stats_Viewable, Made_For_Kids))

# Exclude the videos from the last 2 weeks
video_details_df$Published_Time <- as.POSIXct(video_details_df$Published_Time)
# Calculate the date 2 weeks before the maximum date
max_date <- max(video_details_df$Published_Time)
date_cutoff <- max_date - days(14)
filtered_dataset <- video_details_df[!(video_details_df$Published_Time >= date_cutoff & video_details_df$Published_Time <= max_date), ]
video_details_df <- select(filtered_dataset, -c(Published_Time))

# Separating data for text and non text models
skim(video_details_df)
write_rds(video_details_df, "processed_youtube_df.rds")
video_details_text_df <- select(video_details_df, c(Title, Description, Tags, View_Count))
video_details_ml_df <- select(video_details_df, -c(Title, Description, Tags))

# Factorizing character variables
video_details_ml_df_skim = partition(skim(video_details_ml_df))
names(video_details_ml_df_skim)
string_2_factor_names <- video_details_ml_df_skim$character$skim_variable
rec_obj <- recipe(~ ., data = video_details_ml_df) |>
  step_string2factor(all_of(string_2_factor_names)) |>
  step_impute_median(all_numeric()) |> # missing values in numeric columns
  step_impute_mode(all_nominal()) |> # missing values in factor columns
  prep()
video_details_ml_df <- bake(rec_obj, video_details_ml_df)
skim(video_details_ml_df)

# Store processed data
write_rds(video_details_ml_df, "processed_youtube_ml.rds")
write_rds(video_details_text_df, "processed_youtube_text.rds")
