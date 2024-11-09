params <-
list(dataset_selection = 1L, testing_set_percent = 0.1, subset_percent = 0.1, 
    save_files = FALSE, large_dataset = FALSE, remove_obscure = TRUE, 
    expand_genres = TRUE)

# names and urls of MovieLens datasets that have been tested
knitr::opts_chunk$set(echo = FALSE)
names <- list("ml-10m", "ml-32m", "ml-latest", "ml-latest-small",  "ml-25m")
urls <- list("ml-10m.zip", "ml-32m.zip", "ml-latest.zip", "ml-latest-small.zip", "ml-25m.zip")
folders <- list("ml-10M100K", "ml-32m", "ml-latest", "ml-latest-small", "ml-25m")

# load necessary libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("Extra", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")


# ------------------------------------------------------------------------------------------------------
# collection of functions used 
# ------------------------------------------------------------------------------------------------------
# function to perform regularization on the given dataset for a specif lambda
regularization_function <- function(lambda, dataset){
  #calculating average for average training set raring
  mu <- mean(dataset$rating)
  
  #calculating movie bias for a given lambda
  b_movie <- dataset %>% 
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - mu)/(n()+lambda))
 
  #calculating user bias for a given lambda
  b_user <- dataset %>% 
    left_join(b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_user = sum(rating - b_movie - mu)/(n()+lambda))
  
  # calculating genres bias for a given lambda
  b_genres <- dataset %>%
    left_join(b_movie, by="movieId") %>%
    left_join(b_user, by="userId") %>%
    group_by(genres) %>%
    summarize(b_genres = sum(rating - b_movie - b_user - mu)/(n()+lambda))
  
  # calculating release year bias for a given lambda
  b_year <- dataset %>%
    left_join(b_movie, by="movieId") %>%
    left_join(b_user, by="userId") %>%
    left_join(b_genres, by="genres") %>%
    group_by(releaseYear) %>%
    summarize(by = mean(rating - b_movie - b_user - b_genres - mu)/(n()+lambda))
   
    

  #predicting ratings on test_set
  predicted_ratings <- 
    dataset %>% 
    left_join(b_movie, by = "movieId") %>%
    left_join(b_user, by = "userId") %>%
    left_join(b_genres, by= "genres") %>%
    mutate(pred = mu + b_movie + b_user + b_genres) %>%
    pull(pred)

  #calculating RMSE on test set for a given lambda
  return(RMSE(predicted_ratings, dataset$rating))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# check the dataset of NAs and/or nulls
check_for_nas_and_nulls <- function(dataset) {
cnt <- sapply(dataset, function(x) sum(is.na(x)))
if (sum(cnt) == 0)  {
  message("No NAs in dataset")
  } else {
  message("NAs in data set")
  cnt[cnt == 0] %>% kable(col.names = c("nulls present"))
  dataset <- na.omit(dataset)
  }
  
cnt <- sapply(dataset, function(x) sum(is.null(x)))
if (sum(cnt) == 0)  {
  message("No nulls/blanks in dataset")
  } else {
  cnt[cnt == 0] %>% kable(col.names = c("nulls present"))
  }
return (dataset)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# prepare dataset by converting timestamp, and adding release Year and ratingLag columns
dataset_prep <- function(dataset) {
# Transform dataset timestamp column to date
dataset$timestamp <- dataset$timestamp %>% as.POSIXct(origin = "1970-01-01")

dels <<- nrow(edx)
to_delete <- dataset[str_detect(dataset$title, "^.*\\(\\d{4}\\)$", negate=TRUE)]
dataset <- dataset[(!dataset$title %in% to_delete$title),]
dels <<- dels - nrow(edx)

# Create a year of release(assuming a mid-year release) and lag-to-rate columns
releaseYear <- as.Date(paste("07/01/",
                             str_sub(dataset$title, str_length(dataset$title) - 4,
                                     str_length(dataset$title) - 1), sep=""), "%m/%d/%Y")

ratingLag <- as.numeric(as.Date(dataset$timestamp) - releaseYear)
dataset <- dataset %>% cbind(ratingLag, releaseYear)  

check_for_nas_and_nulls(dataset) 

return(dataset)
}     # end of function

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# if selected, expand the dataset genres column into listing only single genre per row
expand_dataset <- function(dataset) {
  if (params$expand_genres) {
    dataset <- dataset %>% separate_rows(genres, sep = "\\|") %>% group_by(genres)
  }
  return (dataset)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# simple function to print the selectable parameters
show_parameters <- function() {
  testing_set_percent <- params$testing_set_percent
  subset_percent <- params$subset_percent
  save_files <- params$save_files
  large_dataset <- params$large_dataset
  remove_obscure <- params$remove_obscure
  expand_genres <- params$expand_genres
  parameters <- data.frame()
  parameters <- rbind(parameters, c("testing_set_percent", params$testing_set_percent))
  parameters <- rbind(parameters, c("subset_percent", params$subset_percent))
  parameters <- rbind(parameters, c("save_files", params$save_files))
  parameters <- rbind(parameters, c("large_dataset", params$large_dataset))
  parameters <- rbind(parameters, c("remove_obscure", params$remove_obscure))
  parameters <- rbind(parameters, c("expand_genres", params$expand_genres))
  parameters <- rbind(parameters, c("dataset name", names[params$dataset_selection]))
  colnames(parameters) <- c("Parameter", "Value")
  return (parameters)
  }
# ------------------------------------------------------------------------------------------------------
# end of functions
# ------------------------------------------------------------------------------------------------------

# keep track of elapsed time
start_time <- Sys.time()

# depending of parameter selected, download working files from website or reload from disk
folder <- folders[params$dataset_selection]
dl <- urls[params$dataset_selection][[1]]
if (!file.exists(dl)) {
 file_url <- paste("https://files.grouplens.org/datasets/movielens/", dl, sep="")
 file_message <- paste('File will be downloaded from ', file_url)
 download.file(file_url, dl)
} else {
  file_message <- paste("Dataset", names[params$dataset_selection], "will be reloaded from disk")
} 

 # selection #1 has different format that the later datasets. Handle as code provided
  if (params$dataset_selection == 1) {
       ratings_file <- paste(folder, "/ratings.dat", sep="")
       if (!file.exists(ratings_file))
         unzip(dl, ratings_file)
       movies_file <- paste(folder, "/movies.dat", sep="")
       if (!file.exists(movies_file))
         unzip(dl, movies_file)
       
       ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
       colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
       ratings <- ratings %>%
         mutate(userId = as.integer(userId),
                movieId = as.integer(movieId), 
                rating = as.numeric(rating),
                timestamp = as.integer(timestamp))
    
      movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify=TRUE), stringsAsFactors=FALSE)
      colnames(movies) <- c("movieId", "title", "genres")
      movies <- movies %>% 
        mutate(movieId = as.integer(movieId))

     } else {
      unzip(dl)
      ratings <- fread(paste(folders[params$dataset_selection], "/ratings.csv", sep=""), header = TRUE)
      movies <- fread(paste(folders[params$dataset_selection], "/movies.csv", sep=""), header = TRUE)
  }

# Indicate which file was loaded and from where
print(file_message)

movielens <- left_join(ratings, movies, by = "movieId")
left_out <- anti_join(ratings, movies, by = "movieId")    #  if movies exist that are not in ratings.

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

# capture size info for complete dataset
size_movielens <- nrow(movielens)
unique_users_movielens <- length(unique(movielens$userId))

# create final_holdout_test dataset
test_index <- createDataPartition(y = movielens$rating, times = 1, p = params$testing_set_percent, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final_holdout_test set back into edx set

removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(ratings, movies, temp, removed, test_index, mmovielens)    # remove temporary data structures

# End of Provided Code

head8 <- head(edx, 8) 
nrows_edx <- nrow(edx)# printed later

edx <- dataset_prep(edx)                        # convert timestamp add columns

# show first 8 rows of the edx dataset
head8 %>% 
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 10, full_width=FALSE, position="center", latex_options = "hold_position", "striped")

column_df <- data.frame(feature = colnames(edx),
                        class = c("integer", "numeric", "numeric", "integer", "character", "character", "numeric", "date"),
                        description = c("unique ID for each movie", "unique ID for each user", "rating (0.5-5) that a user gave a movie",
                                        "timestamp as to when the user rated the movie", "title of the movie and year of release", 
                                        "the genre or genres assigned to the movie", "time between the release date and the date the movie was rated by the user",
                                        "the year of release")
)                                        
column_df %>% 
  kbl(caption = "Description of Features") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"), full_width = FALSE, position="center", latex_options = "hold_position") 

# ------------------------------------------------------------------------------------------------------
# code for calculating and producing the various graphs (each plot is a separate chunk)
# ------------------------------------------------------------------------------------------------------

df <- edx %>% ungroup() %>% group_by(releaseYear) %>% reframe(n=n())                    # extract year from max ratings count
year <- format(as.Date(edx[which.max(df$n),]$releaseYear, format("%Y-%m-%d")), "%Y")
sum_df <- sum(df$n)

df1 <- edx %>% ungroup() %>% mutate(rater_year = as.integer(str_extract(timestamp, "\\d{4}"))) %>%  group_by(rater_year) 
min_rater <- min(df1$rater_year)
max_rater <- max(df1$rater_year)
df1 %>% group_by(userId) %>% summarize(mx = max(timestamp), mn = min(timestamp)) %>% mutate(diff = difftime(mx, mn, units="days"))
df2 <- df1 %>% group_by(userId) %>% summarize(mx = max(timestamp), mn = min(timestamp)) %>% mutate(diff = difftime(mx, mn, units="days"))
df3 <- df1 %>% group_by(rater_year) %>% reframe(n = n())

set.seed(1, sample.kind = "Rounding")
small_edx <- edx %>% ungroup() %>% slice_sample(n=2000)

single_rating <- edx %>%
  group_by(userId) %>% 
  reframe(n_distinct(userId),diff = max(rating) - min(rating) == 0, rating) %>% filter(diff == 1)  %>% 
  group_by(userId, rating) %>% summarize(cnt = n()) %>% arrange(rating) %>% group_by(rating, cnt) %>%  
  ungroup %>% mutate(unit = 1) %>% group_by(userId) %>% 
  reframe(rating, cnt, unit) %>% arrange(rating) %>% reframe(n=n(), rating, Ratings= sum(cnt), Users=sum(unit), .by=rating) %>%
  group_by(rating) %>% reframe(rating, Users, Ratings) %>% unique()

df_average_ratings <- edx %>%
        group_by(userId)%>%
        summarize(avg = mean(rating)) %>%
        arrange(desc(avg))


number_of_raters_plot_per_year <- df1 %>%
  ggplot(aes(rater_year)) + 
  geom_bar(stat="count",bins = 30) + 
  xlab("Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific=FALSE)) +
  ggtitle("Number of Raters by Year") +
  theme(plot.title = element_text(hjust = 0.5))
number_of_ratings_per_rating_year_plot <- df3 %>%
  ggplot(aes(x = rater_year, y = n)) + 
  geom_bar(stat = "identity", bins=30 ) + 
  xlab("Rating Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific=FALSE)) +
  ggtitle("Nbr of Ratings by Rating Year") +
  theme(plot.title = element_text(hjust = 0.5))
number_of_ratings_per_rating_year_plot 

rater_presence_plot <- df2 %>% select(diff) %>% ggplot(aes(diff)) + 
                         geom_histogram() + 
                         xlim(-1, 5*365) + 
                         ylim(0, 4000) +
                         xlab("Days") +
                         ylab("Number of Raters") +
                         ggtitle("Days Between 1st and Last Rating") +
                         geom_vline(xintercept = c(365,2*365, 3*365, 4*365), color="blue")  +    
                         theme(plot.title = element_text(hjust = 0.5))



# plot number of ratings per year
number_of_ratings_per_release_year_plot <- edx %>% select(userId, movieId, releaseYear) %>% mutate(yr = year(releaseYear)) %>% 
  ggplot(aes(yr)) + 
  geom_bar(stat="count") + 
  scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific=FALSE)) +
  xlab("Release Year") +
  ylab("Number of Ratings") +
  ggtitle("Nbr of Ratings by Release Year")
  

# plot distribution of ratings plot
distribution_of_ratings_plot <- edx %>%
         ggplot(aes(rating)) +
         geom_histogram(bins=30, binwidth=0.3, color = "black") +
         scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific=FALSE)) +
         ggtitle("Distribution of Ratings")
# number of ratings per movie
number_of_ratings_per_movie_plot <-edx %>%
         count(movieId) %>%
         ggplot(aes(n)) +
         geom_histogram(bins=30, color="black") +
         scale_x_log10(labels = function(x) format(x, big.mark = ",", scientific=FALSE)) +  
         scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific=FALSE)) +
         xlab("number of ratings") +
         ylab("number of movies") +
         ggtitle("Number of Ratings per Movie")

# plot of movie age vs rating lag
movie_age_against_lagtime_plot <- small_edx %>%
  ggplot(aes(releaseYear, ratingLag, group=releaseYear)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
    xlab("Year of Release") +
    ylab("Days Between Release and Rating") +
    ggtitle("Movie Age vs Rating Lag") 

# plot of frequency of average ratings given by users 
frequency_of_avg_ratings_plot  <- df_average_ratings %>%  ggplot() +
    geom_histogram(aes(avg), bins = 10, binwidth = 0.25) +
    xlab("Average Rating per User") +
    ylab("Frequency of Rating") +
    ggtitle("Frequency of Average Ratings \nGiven By Users")

# plot of the minimum and maximum ratings given by a user
min_max_per_user_plot1 <- edx %>%
  group_by(userId, rating) %>%
  summarize(num = n()) %>%
  group_by(userId) %>%
  summarize(distinct = n_distinct(rating)) %>%
  ggplot() +
    aes(distinct) +
    geom_histogram(binwidth=1) +
    xlab("Number of Distinct Ratings") +
    ylab("Number of Users") +
    ggtitle("Distinct Ratings Per User") 

# does the rater give multiple ratings? Plot of min/max ratings given by a specific user.
# How many different ratings does each user give           
min_max_per_user_plot2 <- edx  %>%
  group_by(userId) %>%
  summarize(min = min(rating), max = max(rating)) %>%
  ggplot() +
    geom_histogram(aes(min), binwidth=0.5, fill="blue", alpha = 0.5) +
    geom_histogram(aes(max), binwidth=0.5, fill="red", alpha=0.5) +
    xlab("Rating") +
    ylab("# of Users") +
    ggtitle("Min/Max Per User")

# plot of the spread of ratings given by a particular user
spread_of_user_ratings_plot <- edx %>% 
  group_by(userId) %>%
  summarize(spread = max(rating) - min(rating))%>%
  ggplot(aes(spread)) +
    geom_histogram(binwidth = 0.5) +
    xlab("Number of Distinct Ratings") +
    ylab("Number of Users") +
    ggtitle("Min/Max Ratings Spread")

# Does a user only give one rating? Plot of those who do.
single_rate_plot <- single_rating %>%
  ggplot() +
  geom_col(aes(x = rating, y = Users)) +
  ggtitle("Single Rating by User") +
  xlab("Rating Given") +
  ylab("Number of Users") 
# expands dataset by separating the genres column value into separate rows
    edx_genres <- expand_dataset(edx)
  
# time-safer option -- polished dataset saved to disk
if (params$save_files) {
  saveRDS(edx, file = "edx.rds")
  saveRDS(final_holdout_test, file = "final_holdout_test.rds")
}


grid.arrange(number_of_raters_plot_per_year,rater_presence_plot, ncol = 2)

grid.arrange(number_of_ratings_per_release_year_plot, number_of_ratings_per_rating_year_plot, ncol = 2, heights=c(4,4))

grid.arrange(distribution_of_ratings_plot, number_of_ratings_per_movie_plot, ncol = 2)

grid.arrange(single_rate_plot, min_max_per_user_plot1, ncol = 2)

grid.arrange(min_max_per_user_plot2, spread_of_user_ratings_plot,  ncol=2)

grid.arrange(frequency_of_avg_ratings_plot, movie_age_against_lagtime_plot, ncol = 2)

# plot the number of movies by genre. One movies may have multiple genres
if (params$expand_genres) {
  edx_genres  %>%
    select(genres, rating) %>%
    group_by(genres) %>%
    ggplot(aes(x = factor(genres))) +
    geom_bar(stat="count") +
    ggtitle("Movie Ratings by Genre") +
    xlab("Genre") +
    ylab("Rating") +
    scale_y_continuous(labels = function(x) format(x, big.mark=",",  scientific=FALSE)) +
    theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1))
    } else {
    edx %>% group_by(genres) %>% reframe( nn = n()) %>% slice_max(order_by=nn, n=25) %>% arrange(genres) %>%
    ggplot(aes(x = factor(genres), y = nn)) +
    geom_bar(stat="identity") +
    xlab("Genre") +
    ylab("Number of Ratings") +
    ggtitle("Movie Ratings by Genre Group") +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    theme(axis.text.x  = element_text(angle=90, vjust=1, hjust=1))
}


# get data for the first movie in the dataset
min_year <- substr(min(edx$releaseYear),1,4)
min_title <- edx[which.min(edx$releaseYear),]$title
# some movie titles have "The" at the end. Move this to the start for aesthetics
min_title <- substr(min_title, 1, nchar(min_title)-7)
if (endsWith(min_title, ", The")) {
  min_title <- paste("The", substr(min_title, 1, nchar(min_title)-5))
}


# table of obscure movies. User has option of excluding these or not from the dataset
obscure_movies <- edx %>% 
  group_by(movieId) %>% 
  reframe(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  reframe(movieId, rating = rating, n_rating =  count) 


obscure_movies %>% select(-movieId) %>% slice(1:15) %>%
  kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 10)
# If desired, obscure movies are removed
if (params$remove_obscure) {
  nrows1 = nrow(edx)
  edx <- edx[!edx$movieId %in% obscure_movies$movieId,]
}

# create table of average rating vs mean
edx %>%
  group_by(userId) %>%
  summarize(Avg_Rating = mean(rating))  %>%
  select(Avg_Rating)%>%
  summary(Avg_Rating) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 12)

# Split the edx dataset into a testing and a training data set. Percentage is determined by knit parameter
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(edx$rating, times = 1, p =     params$subset_percent, list = FALSE)
training_set <- data.frame(edx[-test_index,])
temp <- data.frame(edx[test_index ,])
testing_set <- temp %>%
  semi_join(training_set, by = "movieId") %>%
  semi_join(training_set, by = "userId")

# add back missing rows to the training set and remove extraneous data
withheld <- anti_join(temp, testing_set)
training_set <- rbind(training_set, withheld)
rm (temp, withheld)

# create data frame for holding the results of the calculations
rmse_results <-data.frame(matrix(nrow=0, ncol=3))
colnames(rmse_results) <- c("Method", "RMSE", "Difference")

# ------------------------------------------------------------------------------------------------------
# Modeling
# ------------------------------------------------------------------------------------------------------

mu <- mean(training_set$rating)
zero_rmse <- RMSE(testing_set$rating, mu)
zero_rmse

# save results
rmse_results <- rbind(rmse_results, c("No biases applied - Median Alone", zero_rmse, 0))


# Model 1 -- movie bias
movie_bias_df <- training_set %>% 
              group_by(movieId) %>% 
              summarize(bm = mean(rating - mu))

predicted_ratings <- mu + testing_set %>% 
                     left_join(movie_bias_df, by = "movieId") %>%
                     .$bm
movie_bias_rmse <- RMSE(predicted_ratings, testing_set$rating)
movie_bias_rmse

# save RMSE results
rmse_results <- rbind(rmse_results, c("Median + Movie Effects", movie_bias_rmse, 0))



# Model 2 -- Model1 with user effects
user_bias_df <- training_set %>% 
               left_join(movie_bias_df, by = "movieId") %>%
               group_by(userId) %>% 
               summarize(bu = mean(rating - mu - bm))

predicted_ratings <- testing_set %>% 
                     left_join(movie_bias_df, by='movieId') %>% 
                     left_join(user_bias_df, by="userId") %>%
                     mutate(pred = mu + bm + bu) %>% 
                     .$pred
user_bias_rmse <- RMSE(predicted_ratings, testing_set$rating)
user_bias_rmse

movie_bias_graph <- ggplot(movie_bias_df, aes(x = bm)) +
    geom_histogram(bins=30, fill="yellow", color="lightgray") +
    xlab("Movie Bias") +
    ylab("Number of Movies") +
    ggtitle("Distribution of Movie Bias") +
    theme_linedraw()
user_bias_graph<- ggplot(user_bias_df, aes(x = bu)) +
    geom_histogram(bins=30, fill="yellow", color="lightgray") +
    xlab("User Bias") +
    ylab("Number of Movies") +
    ggtitle("Distribution of User Bias") +
    theme_linedraw()

grid.arrange(movie_bias_graph, user_bias_graph, ncol=2, padding=20) 

# save movie bias RMSE results
rmse_results <- rbind(rmse_results, c("Median + Movie + User Effects", user_bias_rmse, 0))


# Model 3 -- Model 2 plus genre -- for this calculation the expanded data set is used, if selected
genres_bias_df <- training_set %>% 
              left_join(movie_bias_df, by="movieId") %>%
              left_join(user_bias_df, by='userId')  %>% 
              group_by(genres) %>% 
              summarize(bg = mean(rating - mu - bm - bu))

predicted_ratings <- testing_set %>% 
                      left_join(movie_bias_df, by="movieId") %>%
                      left_join(user_bias_df, by='userId')  %>%
                      left_join(genres_bias_df, by="genres") %>%
                      mutate(pred = mu + bu + bm + bg) %>% 
                      .$pred
genres_bias_rmse <- RMSE(predicted_ratings, testing_set$rating)
genres_bias_rmse

# save genres bias RMSE results
rmse_results <- rbind(rmse_results, c("Median + Movie + User + Genres Effects", genres_bias_rmse, 0))


# Model 4 -- Model 3 with release year bias
year_bias_df <- training_set %>% 
              left_join(movie_bias_df, by="movieId") %>%
              left_join(user_bias_df, by='userId') %>% 
              left_join(genres_bias_df, by="genres") %>%
              group_by(releaseYear) %>% 
              summarize(by = mean(rating - mu - bm - bu - bg))

predicted_ratings <- testing_set %>% 
                     left_join(movie_bias_df, by="movieId") %>%
                     left_join(user_bias_df, by='userId') %>%
                     left_join(genres_bias_df, by="genres") %>%
                     left_join(year_bias_df, by = "releaseYear") %>%
                     mutate(pred = mu + bu + bm + bg + by) %>% 
                     .$pred

releaseYear_rmse <- RMSE(predicted_ratings, testing_set$rating)
releaseYear_rmse

# save release year RMSE results
rmse_results <- rbind(rmse_results, c("Median + Movie + User + Genres Effects + Release Year  ", releaseYear_rmse, 0))

genres_bias_graph <- ggplot(genres_bias_df, aes(x = bg)) +
    geom_histogram(bins=30, fill="yellow", color="lightgray") +
    xlab("Genres Bias") +
    ylab("Number of Movies") + 
    ggtitle("Distribution of Genres Bias") +
    theme_linedraw()


year_bias_graph <-ggplot(year_bias_df, aes(x = by)) +
    geom_histogram(bins=30, fill="yellow", color="lightgray") +
    xlab("Year of Release  Bias") +
    ylab("Number of Movies") +
    ggtitle("Distribution of Year of Release Bias") +
    theme_linedraw()

grid.arrange(genres_bias_graph, year_bias_graph, ncol=2, padding=20)

# regularization of Model 4
lambdas <- c(seq(-0.75, 2.5, 0.25))                                 # range of lambdas for testing
rmses <- sapply(lambdas, regularization_function, testing_set)   # test of each lambda specified
min_lambda <- lambdas[which.min(rmses)]                          # select lowest value (best accuracy)

# visualizing where lambda minimizes RMSE
plot_rmses <- ggplot() + 
              aes(x = lambdas, y = rmses) + 
              geom_point() + xlab("Lambda") + 
              ylab("RMSE") + ggtitle("Lambda Tuning") +
              theme(plot.title = element_text(hjust = 0.5)) +
              geom_vline(xintercept = min_lambda, color="blue")
plot_rmses
# print the result of the calculations
message(paste("Minimum lambda: ", min_lambda))


regularization_rmse <- regularization_function(min_lambda, testing_set)
regularization_rmse


# save regularization RMSE results
rmse_results <- rbind(rmse_results, c("Median + Movie + User Effects + regularization", regularization_rmse, 0))


# perform same operations on final_holdout_test dataset as was done on edx
final_holdout_test <- dataset_prep(final_holdout_test)
if (params$remove_obscure) {
  final_holdout_test <- final_holdout_test[!final_holdout_test$movieId %in% obscure_movies$movieId,]
}
final_holdout_test <- expand_dataset(final_holdout_test)

final_holdout_test_rmse <- regularization_function(min_lambda, final_holdout_test)
final_holdout_test_rmse

# save final_holdout_test RMSE results
rmse_results <- rbind(rmse_results, c("final_holdout_test", final_holdout_test_rmse, 0))

# show results of this analysis
colnames(rmse_results) <- c("method", "rmse", "change")

rmse_results %>%
  mutate(change = format(as.numeric(rmse) - first(as.numeric(rmse), digits=4)), rmse = format(as.numeric(rmse), digits=5)) %>% 
  mutate(rmse = cell_spec(rmse, background = ifelse(rmse <= 0.8469, '#90EE90', "white"))) %>% 
  knitr::kable(col.names = c('Method', 'RMSE', 'Change'), align = "lcr", "simple", 
               caption = "Summary of Model RMSEs (goal: 0.86490)") %>%
  kable_styling( )



# Tests of other datasets from movielens
test_dataset <-   c("ml-10M100K", "ml-10M100K", "ml-32M-download", "ml-32M-download", "ml-latest", "ml-latest", "ml-latest-small", "ml-latest-small")
test_size    <-   c("8,999,929",  "10,000,054", "28,749,912",      "TBD",             "33,832,162","30,419,985","87,436",         "91,112"          )
test_obscure <-   c("Yes",        "No",         "Yes",             "No",              "Yes",       "No",        "Yes",             "No"             )
test_training <-  c(0.82029,      0.81992,      0.83488,           0.81598,           0.80875,     0.80875,     0.65521,           0.65892          )
test_final_holdout_test <-c(0.81796,      0.81796,      0.81451,           0.81446,           0.80921,     0.80921,     0.68537,           0.67877          )
test_date    <-   c("2024-10-22", "2024-10-23", "w024-10-23",      "2024-10-21",      "2024-10-21","2024-10-23","2024-10-22",      "2024-10-22"     )
test_time    <-   c("6.20 mins",  "5.95 mins",  "25.89 mins",      "24.97 mins?",     "30.00 mins","34.51 mins","0.12 mins",      "0.12 mins"       )
test_results <- data.frame(test_dataset, test_size, test_obscure, test_training, test_final_holdout_test,
                           test_date, test_time)
test_results %>% knitr::kable(col.names=c("Dataset", "Rows", "Obscure Removed", "Training Set", "final_holdout_test Set", "Date", "Time"), 
                              align="llllllll", "simple", position="center")
# question mark in time indicates run all as opposed to knit



show_parameters()  %>%
    kbl(caption = "Parameters for This Analysis") %>%
    kable_styling(full_width = FALSE, latex_options = "hold_position", position = "left")

# Show how long this analysis took
print(paste('Elapsed time for this analysis:', format(as.numeric(difftime(Sys.time(), start_time, units="mins")), digits=5), "mins"))

