###############################################################
# MovieLens Project
# edX / HarvardX PH125.9x 'Data Science: Capstone'
# Author: jrollb (except section downloading and preparing 'edx' and 'validation')
# 5 Jan, 2021
###############################################################

# The first section contains the code provided together with the problem set.
# I have added two lines to store the datasets in a folder 'rds'
# Afterwards follows the code written to solve the problem

# Switch: download or read from rds
switchNewDownload <- TRUE # select TRUE, initially. Later set it to FALSE to avoid new download.

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>> Code provided - start >>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(switchNewDownload){
  
  ##########################################################
  # Create edx set, validation set (final hold-out test set)
  ##########################################################
  
  # Note: this process could take a couple of minutes
  
  if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
  if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  
  library(tidyverse)
  library(caret)
  library(data.table)
  
  # MovieLens 10M dataset:
  # https://grouplens.org/datasets/movielens/10m/
  # http://files.grouplens.org/datasets/movielens/ml-10m.zip
  
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  
  # if using R 3.6 or earlier:
  if(F){
    movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                               title = as.character(title),
                                               genres = as.character(genres))
  }
  
  # if using R 4.0 or later:
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Validation set will be 10% of MovieLens data
  set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  # Make sure userId and movieId in validation set are also in edx set
  validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)
  
  rm(dl, ratings, movies, test_index, temp, movielens, removed)
  
  # Added by me (jrollb)
  saveRDS(edx,"edx.rds")
  saveRDS(validation,"validation.rds")
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<
# <<< Code provided - end <<<
# <<<<<<<<<<<<<<<<<<<<<<<<<<<


#--------------------------------------------------------------
# Load (additional) libraries
#--------------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(knitr)
library(rpart)
library(rpart.plot)
library(gridExtra)

# Suppress messages from dplyr's 'summarize' function
options(dplyr.summarise.inform = FALSE)

#--------------------------------------------------------
# Define a function, which adds columns for the main genres
#--------------------------------------------------------

extend_genre_column <- function(x){
  x %>% mutate(genre.Action = as.numeric(str_detect(genres,"Action")),
               genre.Adventure = as.numeric(str_detect(genres,"Adventure")),
               genre.Comedy = as.numeric(str_detect(genres,"Comedy")),
               genre.Drama = as.numeric(str_detect(genres,"Drama")),
               genre.Thriller = as.numeric(str_detect(genres,"Thriller")))
}

#--------------------------------------------------------
# Load the data sets (only if first code part is skipped)
#--------------------------------------------------------

if(!switchNewDownload){
  # Load the training set 'edx'
  edx <- readRDS("edx.rds")
  
  # Load the test set 'validation'
  validation <- readRDS("validation.rds")
}

#--------------------------------------------------------
# Explorative analysis of the training set 'edx'
#--------------------------------------------------------

# Show sample output
set.seed(1)
edx %>% slice_sample(n = 7)

# Number of rows
nrow(edx)

# Number of columns
ncol(edx)

# Number of users
length(unique(edx$userId))

# Number of movies
length(unique(edx$movieId))

# Plot, illustrating the range of rating counts per movie
edx %>%
  group_by(movieId) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(index = 1:nrow(.)) %>%
  filter(index <= 2000) %>%
  ggplot(aes(x = index,y = n)) +
  geom_col() +
  labs(x="Movie index",y="Count") +
  geom_point(aes(x = 1,y = max(n)))


# Plot, illustrating the range of rating counts per user
edx %>%
  group_by(userId) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(index = 1:nrow(.)) %>%
  filter(index <= 800) %>%
  ggplot(aes(x = index,y = n)) +
  geom_col() +
  labs(x="User index",y="Count") +
  geom_point(aes(x = 1,y = max(n)))


# Histogram of ratings
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(rating,count)) +
  geom_col(width = 0.25)


# Example histogram for a highly rated movie
edx %>%
  filter(str_detect(title,"Forrest Gump")) %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(rating,count)) +
  geom_col(width = 0.25)


# Example histogram for a movie with average rating
edx %>%
  filter(str_detect(title,"Presidio")) %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(rating,count)) +
  geom_col(width = 0.25)


# Example for an unusual rating distribution
edx %>%
  filter(str_detect(title,"Plan 9 from")) %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(rating,count)) +
  geom_col(width = 0.25)


# Create a subset of 'edx' to investigate genre
tmpEdx <- edx %>%
  slice_sample(n = 50000) %>%
  select(userId, movieId, rating, genres) %>%
  extend_genre_column()


# Show boxplots for various genres
p1 <- tmpEdx %>%
  ggplot(aes(x = as.factor(genre.Action),y = rating)) +
  geom_boxplot() +
  labs(x = "Genre 'action'")

p2 <- tmpEdx %>%
  ggplot(aes(x = as.factor(genre.Comedy),y = rating)) +
  geom_boxplot() +
  labs(x = "Genre 'comedy'")

p3 <- tmpEdx %>%
  ggplot(aes(x = as.factor(genre.Drama),y = rating)) +
  geom_boxplot() +
  labs(x = "Genre 'drama'")

p4 <- tmpEdx %>%
  ggplot(aes(x = as.factor(genre.Thriller),y = rating)) +
  geom_boxplot() +
  labs(x = "Genre 'thriller'")

grid.arrange(p1,p2,p3,p4,ncol=2,nrow=2)


# Calculate average rating for various genres
tmpEdx %>%
  summarize(avg.rtg.action = sum(rating * genre.Action)/sum(genre.Action),
            avg.rtg.adventure = sum(rating * genre.Adventure)/sum(genre.Adventure),
            avg.rtg.comedy = sum(rating * genre.Comedy)/sum(genre.Comedy),
            avg.rtg.drama = sum(rating * genre.Drama)/sum(genre.Drama),
            avg.rtg.thriller = sum(rating * genre.Thriller)/sum(genre.Thriller),
            avg.rtg.overall = mean(rating))

# Note that this is a subset, such that the overall average is slightly different from the 'edx' set


# Apply a CART model for main genre relevance
tmpEdx2 <- tmpEdx %>%
  select(-userId,-movieId,-genres)

CART.model <- rpart(rating ~ .,data = tmpEdx2)
prp(CART.model,)

# Clean up
remove(tmpEdx)

#--------------------------------------------------------
# Define a function for calculating the RMSE
#--------------------------------------------------------

RMSE <- function(y_pred,y_actual){
  sqrt(mean((y_pred - y_actual)^2))
}

#--------------------------------------------------------------
# Split of 'edx' into training and test set
#--------------------------------------------------------------

# Parameter for the proportion of observations selected for the test set (typically < 0.5)
p_testset <- 0.35

# Split 'edx' into training and test set
set.seed(123, sample.kind="Rounding")
indices_test <- createDataPartition(y = edx$rating, times = 1, p = p_testset, list = FALSE)
edx_train <- edx[-indices_test,]
edx_test <- edx[indices_test,]

# Make sure userId and movieId in the validation set are also present in the edx set
edx_test <- edx_test %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")


#--------------------------------------------------------
# Baseline model Y = mu + epsilon_{i,u}
#--------------------------------------------------------

# Estimate parameter mu (aka mu_hat) as average rating
mu <- mean(edx_train$rating)

# Predict rating using the baseline model
y_hat <- rep(mu,nrow(edx_test))

# Calculate RMSE
rmse <- RMSE(y_hat,edx_test$rating)

# Store result in a tibble
rmse_results <- tibble(method = "Baseline model", RMSE = rmse)


#--------------------------------------------------------
# Model with movie effect: Y = mu + b_i + epsilon
#--------------------------------------------------------

# Estimate model parameter b_i (aka b_i_hat)
movie_averages <- edx_train %>%
  group_by(movieId) %>%
  summarize(title = first(title), b_i = mean(rating - mu))

# Predict rating using the model including the movie effect
y_hat <- edx_test %>%
  left_join(movie_averages, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

# Calculate RMSE
rmse <- RMSE(y_hat,edx_test$rating)

# Append result to tibble
rmse_results <- rmse_results %>%
  bind_rows(tibble(method = "Movie effect", RMSE = rmse))


#--------------------------------------------------------
# Movie effect: regularization
#--------------------------------------------------------

lambdas <- seq(0, 10, 1) # start with a broad range
lambdas <- seq(1.5, 3, 0.25) # refine

rmses <- sapply(lambdas, function(lam){

  movie_averages <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lam))

  y_hat <- edx_test %>%
    left_join(movie_averages, by='movieId') %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)

  return(RMSE(y_hat, edx_test$rating))
})

# Plot the RMSE values versus lambda values
qplot(lambdas, rmses)

# Pick the optimal lambda
lambda <- lambdas[which.min(rmses)]
rmse <- rmses[which.min(rmses)]

# Store the lambda for later purpose
lambda.movieEffect <- lambda

# Update the movie averages with the best regularized result for later use
movie_averages <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# Append result to tibble
rmse_results <- rmse_results %>%
  bind_rows(tibble(method = "Movie effect, regularized", RMSE = rmse))


#--------------------------------------------------------
# Model with user effect: Y = mu + b_i + b_u + epsilon
#--------------------------------------------------------

# Estimate parameter b_u (aka b_u_hat)
user_averages <- edx_train %>%
  left_join(movie_averages, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predict rating using the model including the user effect
y_hat <- edx_test %>%
  left_join(movie_averages, by='movieId') %>%
  left_join(user_averages, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Calculate RMSE
rmse <- RMSE(y_hat,edx_test$rating)

# Append result to tibble
rmse_results <- rmse_results %>%
  bind_rows(tibble(method = "Movie and user effect", RMSE = rmse))

#--------------------------------------------------------
# Movie and user effect, regularization
#--------------------------------------------------------

# lambdas <- seq(3, 8, 0.25) # START WITH A LARGE RANGE
# lambdas <- seq(4.5, 5.5, 0.1) # NARROW DOWN
lambdas <- seq(4.6, 5.2, 0.1) # to save run time

rmses <- sapply(lambdas, function(lam){

  mu <- mean(edx_train$rating)

  b_i <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lam))

  b_u <- edx_train %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lam))

  y_hat <- edx_test %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)

  return(RMSE(y_hat, edx_test$rating))
})

# Plot the RMSE values versus lambda values
qplot(lambdas, rmses)

# Pick the optimal lambda
lambda <- lambdas[which.min(rmses)]
rmse <- rmses[which.min(rmses)]

# Store the lambda for later use
lambda.movieUserEffect <- lambda

# Store averages using the best lambda:
movie_averages <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
user_averages <- edx_train %>%
  left_join(movie_averages, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# Append result to tibble
rmse_results <- rmse_results %>%
  bind_rows(tibble(method = "Movie + user, regularized", RMSE = rmse))


#--------------------------------------------------------
# Singular value decomposition (SVD): preparations
#--------------------------------------------------------

# Minimal number of ratings for each movie and user
nFilter <- 350 # Aiming for 350! During development: 450 is reasonably fast.

# Apply the filter
train_small <- edx_train %>%
  group_by(movieId) %>%
  filter(n() >= nFilter) %>% ungroup() %>%
  group_by(userId) %>%
  filter(n() >= nFilter) %>% ungroup()

# Convert tidy table to matrix 'y'
y <- train_small %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

rownames(y) <- y[,1]
y <- y[,-1]


# Show matrix dimensions
print(dim(y))


# Subtract means
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))


# Set the residuals to zero for the missing ratings (but see limitations in report!)
y[is.na(y)] <- 0


#--------------------------------------------------------
# Apply SVD algorithm
#--------------------------------------------------------
print("Start SVD algorithm...")
tmp <- Sys.time()

# Apply SVD algorithm to y
s <- svd(y)

time.SVD <- Sys.time() - tmp
print("... Finished SVD.")

# Show runtime
print(time.SVD)


#--------------------------------------------------------
# Understand SVD output
#--------------------------------------------------------

# The output contains three elements
names(s)

# Test correctness of the decomposition
# (suppressed, if there are more than 1000 singular values for timing reasons):
if(length(s$d < 1000)){
  y_svd <- s$u %*% diag(s$d) %*% t(s$v)
  print(paste0("Check SVD: Error of composing y from U D V^T: ",max(abs(y - y_svd))))
}


# Plot singular values and variance contributions (only shown if there are less than 1000 values)
if(length(s$d < 1000)){
  p1 <- data.frame(n=1:length(s$d),d=s$d) %>%
    ggplot(aes(n,d)) +
    geom_point() +
    ggtitle("Singular values (d)")
  p2 <- data.frame(n=1:length(s$d),d=s$d) %>%
    mutate(propVar = cumsum(d^2)/sum(d^2)) %>%
    ggplot(aes(n,propVar)) +
    geom_line() +
    labs(y = "proportion") +
    ggtitle("Proportion of variance explained")
  grid.arrange(p1,p2,ncol=2)
}


#--------------------------------------------------------
# First SVD contribution
#--------------------------------------------------------

pq1 <- with(s,(u[,1, drop=FALSE]*d[1]) %*% t(v[,1, drop=FALSE]))
colnames(pq1) <- colnames(y)
rownames(pq1) <- rownames(y)
delta_y_pq1 <- pq1 %>%
  as_tibble() %>%
  mutate(userID = rownames(y)) %>%
  gather("movie","pq1",-userID) %>%
  mutate(movieId = as.integer(movie),userId = as.integer(userID))

# Predict rating using the model including the user effect
y_hat <- edx_test %>%
  left_join(movie_averages, by='movieId') %>%
  left_join(user_averages, by='userId') %>%
  left_join(delta_y_pq1, by=c('movieId','userId')) %>%
  mutate(pred = mu + b_i + b_u + if_else(is.na(pq1),0,pq1)) %>%
  pull(pred)

# Calculate RMSE
rmse <- RMSE(y_hat,edx_test$rating)

# Append result to tibble
rmse_results <- rmse_results %>%
  bind_rows(tibble(method = "SVD first factor", RMSE = rmse))


# Intermediate results
rmse_results


# Show movie titles representing the first factor - high side
tmpTitleMapping <- edx_train %>%
  select(movieId,title) %>%
  distinct()

tmpMoviesPQ1 <- delta_y_pq1 %>%
  select(-movie,-userID) %>%
  group_by(movieId) %>%
  summarize(meanPQ1 = mean(pq1)) %>%
  left_join(tmpTitleMapping, by = "movieId") %>%
  select(title,meanPQ1)

tmpMoviesPQ1%>%
  arrange(desc(meanPQ1)) %>%
  head()


# Show movie titles representing the first factor - low side
tmpMoviesPQ1%>%
  arrange(meanPQ1) %>%
  head() 


# SVD with up to the n-th factor
nSVD.range <- c(2:3,seq(18,36,3))
for(nSVD in nSVD.range){
  pqN <- s$u[,1:nSVD,drop=FALSE] %*%
    diag(s$d[1:nSVD,drop=FALSE],nrow = nSVD,ncol = nSVD) %*%
    t(s$v[,1:nSVD,drop=FALSE])
  colnames(pqN) <- colnames(y)
  rownames(pqN) <- rownames(y)
  delta_y_pqN <- pqN %>%
    as_tibble() %>%
    mutate(userID = rownames(y)) %>%
    gather("movie","pqN",-userID) %>%
    mutate(movieId = as.integer(movie),userId = as.integer(userID)) %>%
    mutate(movie = NULL, userID = NULL)

  # Predict rating using the model including the user effect
  y_hat <- edx_test %>%
    left_join(movie_averages, by='movieId') %>%
    left_join(user_averages, by='userId') %>%
    left_join(delta_y_pqN, by=c('movieId','userId')) %>%
    mutate(pred = mu + b_i + b_u +
             if_else(is.na(pqN),0,pqN)) %>%
    pull(pred)

  # Calculate RMSE
  rmse <- RMSE(y_hat,edx_test$rating)

  # Append result to tibble
  rmse_results <- rmse_results %>%
    bind_rows(tibble(method = paste0("SVD with ",nSVD," factor(s)"), RMSE = rmse))
}

# Intermediate results
rmse_results

# Clean up
remove(delta_y_pq1)
remove(delta_y_pqN)



#--------------------------------------------------------
# Final fit to the 'edx' training set
#--------------------------------------------------------

# Enclosed in if(F){} to make section visible
if(T){
  ## Restrict the data set by requiring a minimum
  nFilter <- 500 # Aiming for 525! 550 is well feasible!!!
  
  
  # Error catching: I remove edx_train and edx_test in case I forget it in the code below
  # If I forgot to change it anywhere, the code will crash
  if(F){
    remove(edx_train)
    remove(edx_test)
  }
  
  
  
  #--------------------------------------------------------
  # Baseline model Y = mu + epsilon_{i,u}
  #--------------------------------------------------------
  
  # Estimate parameter mu (aka mu_hat) as average rating
  mu <- mean(edx$rating)
  
  # Predict rating using the baseline model
  y_hat <- rep(mu,nrow(edx))
  
  # Calculate RMSE
  rmse <- RMSE(y_hat,edx$rating)
  
  # Store result in a tibble
  rmse_results <- tibble(method = "Baseline model (in sample)", RMSE = rmse)
  
  #--------------------------------------------------------
  # Movie effect: regularization
  #--------------------------------------------------------
  
  # Movie averages with the best regularized result
  movie_averages <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda.movieEffect))
  
  # Store it for later use
  movie_averages.movieOnly <- movie_averages
  
  # Predict rating
  y_hat <- edx %>%
    left_join(movie_averages, by='movieId') %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  
  # Calculate RMSE
  rmse <- RMSE(y_hat, edx$rating)
  
  # Append result to tibble
  rmse_results <- rmse_results %>%
    bind_rows(tibble(method = "Movie effect, regularized", RMSE = rmse))
  
  #--------------------------------------------------------
  # Movie and user effect with regularization
  #--------------------------------------------------------
  
  # Movie averages with the best regularized result
  movie_averages <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda.movieUserEffect))
  
  # Estimate parameter b_u (aka b_u_hat)
  user_averages <- edx %>%
    left_join(movie_averages, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + lambda.movieUserEffect))
  
  # Predict rating using the model including the user effect
  y_hat <- edx %>%
    left_join(movie_averages, by='movieId') %>%
    left_join(user_averages, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  # Calculate RMSE
  rmse <- RMSE(y_hat,edx$rating)
  
  # Append result to tibble
  rmse_results <- rmse_results %>%
    bind_rows(tibble(method = "Movie and user effect (regul., in sample)", RMSE = rmse))
  
  #--------------------------------------------------------
  # Singular value decomposition
  #--------------------------------------------------------
  
  # Reduce the data set
  train_small <- edx %>%
    group_by(movieId) %>%
    filter(n() >= nFilter) %>% ungroup() %>%
    group_by(userId) %>%
    filter(n() >= nFilter) %>% ungroup()
  
  ## Prepare the matrix 'y'
  
  # Reduce the data set
  y <- train_small %>%
    select(userId, movieId, rating) %>%
    spread(movieId, rating) %>%
    as.matrix()
  
  # remove user ID column and use as row names
  rownames(y) <- y[,1]
  y <- y[,-1]
  
  # Subtract means
  y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
  y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
  
  # Set the residuals to zero for the missing ratings (but see limitations in report!)
  y[is.na(y)] <- 0
  
  # Print size of matrix
  print(dim(y))
  
  #--------------------------------------------------------
  # Apply SVD
  #--------------------------------------------------------
  print("Start SVD algorithm...")
  tmp <- Sys.time()
  
  # Apply SVD algorithm to y
  s <- svd(y)
  
  time.SVD <- Sys.time() - tmp
  print("... Finished SVD.")
  
  print("Run time SVD:")
  print(time.SVD)
  
  # Predict with SVD up to the n-th factor:
  nSVD.range <- c(20,25,30)
  for(nSVD in nSVD.range){
    pqN <- s$u[,1:nSVD,drop=FALSE] %*%
      diag(s$d[1:nSVD,drop=FALSE],nrow = nSVD,ncol = nSVD) %*%
      t(s$v[,1:nSVD,drop=FALSE])
    colnames(pqN) <- colnames(y)
    rownames(pqN) <- rownames(y)
    delta_y_pqN <- pqN %>%
      as_tibble() %>%
      mutate(userID = rownames(y)) %>%
      gather("movie","pqN",-userID) %>%
      mutate(movieId = as.integer(movie),userId = as.integer(userID)) %>%
      mutate(movie = NULL, userID = NULL)
    
    # Predict rating using the model including the user effect
    y_hat <- edx %>%
      left_join(movie_averages, by='movieId') %>%
      left_join(user_averages, by='userId') %>%
      left_join(delta_y_pqN, by=c('movieId','userId')) %>%
      mutate(pred = mu + b_i + b_u +
               if_else(is.na(pqN),0,pqN)) %>%
      pull(pred)
    
    # Calculate RMSE
    rmse <- RMSE(y_hat,edx$rating)
    
    # Append result to tibble
    rmse_results <- rmse_results %>%
      bind_rows(tibble(method = paste0("SVD with ",nSVD," factor(s)"), RMSE = rmse))
  }
  
  # Clean up
  remove(delta_y_pqN)
}


#--------------------------------------------------------------
# In-sample results
#--------------------------------------------------------------

# In-sample results
rmse_results


#--------------------------------------------------------------
# Apply model to validation set
#--------------------------------------------------------------

if(T){
  #--------------------------------------------------------
  # Baseline model Y = mu + epsilon_{i,u}
  #--------------------------------------------------------
  
  # Predict rating using the baseline model
  y_hat <- rep(mu,nrow(validation))
  
  # Calculate RMSE
  rmse <- RMSE(y_hat,validation$rating)
  
  # Store result in a tibble
  rmse_validation <- tibble(method = "Baseline model (out of sample)", RMSE = rmse)
  
  #--------------------------------------------------------
  # Movie effect with regularization
  #--------------------------------------------------------
  
  # Predict rating
  y_hat <- validation %>%
    left_join(movie_averages.movieOnly, by='movieId') %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  
  # Calculate RMSE
  rmse <- RMSE(y_hat, validation$rating)
  
  # Append result to tibble
  rmse_validation <- rmse_validation %>%
    bind_rows(tibble(method = "Movie effect, regularized (out of sample)", RMSE = rmse))
  
  #--------------------------------------------------------
  # User effect with regularization
  #--------------------------------------------------------
  
  # Predict rating using the model including the user effect
  y_hat <- validation %>%
    left_join(movie_averages, by='movieId') %>%
    left_join(user_averages, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  # Calculate RMSE
  rmse <- RMSE(y_hat,validation$rating)
  
  # Append result to tibble
  rmse_validation <- rmse_validation %>%
    bind_rows(tibble(method = "Movie and user effect (regul., out of sample)", RMSE = rmse))
  
  #--------------------------------------------------------
  # Apply SVD
  #--------------------------------------------------------
  
  # Predict with SVD up to the n-th factor:
  nSVD.range <- c(20,25,30)
  for(nSVD in nSVD.range){
    pqN <- s$u[,1:nSVD,drop=FALSE] %*%
      diag(s$d[1:nSVD,drop=FALSE],nrow = nSVD,ncol = nSVD) %*%
      t(s$v[,1:nSVD,drop=FALSE])
    colnames(pqN) <- colnames(y)
    rownames(pqN) <- rownames(y)
    delta_y_pqN <- pqN %>%
      as_tibble() %>%
      mutate(userID = rownames(y)) %>%
      gather("movie","pqN",-userID) %>%
      mutate(movieId = as.integer(movie),userId = as.integer(userID)) %>%
      mutate(movie = NULL, userID = NULL)
    
    # Predict rating using the model including the user effect
    y_hat <- validation %>%
      left_join(movie_averages, by='movieId') %>%
      left_join(user_averages, by='userId') %>%
      left_join(delta_y_pqN, by=c('movieId','userId')) %>%
      mutate(pred = mu + b_i + b_u +
               if_else(is.na(pqN),0,pqN)) %>%
      pull(pred)
    
    # Cut off the predicted ratings below 0.5 and above 5.0
    y_hat <- pmax(0.5,pmin(5.0,y_hat))
    
    # Calculate RMSE
    rmse <- RMSE(y_hat,validation$rating)
    
    # Append result to tibble
    rmse_validation <- rmse_validation %>%
      bind_rows(tibble(method = paste0("SVD with ",nSVD," factor(s), out of sample"), RMSE = rmse))
  }
  
  # Clean up
  remove(delta_y_pqN)
}

#--------------------------------------------------------------
# Final result (RMSE)
#--------------------------------------------------------------

# The final RMSE for the 25 factor SVD correction is
print(paste0("Final RMSE (validation set): ",rmse_validation$RMSE[5]))

