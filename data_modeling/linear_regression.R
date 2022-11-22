library(caret)
library(pROC)
library(forecast)

# Load the dataset
songs.df <- read.csv('dataset/songs_updated_v4.csv', stringsAsFactors = TRUE)
str(songs.df)

# Pre-process the dataset

# Mode is 0 for Minor, 1 for Major
songs.df$mode <- factor(songs.df$mode)
levels(songs.df$mode)
levels(songs.df$mode) <- c('Minor', 'Major')

# Key is E.g. 0 = C, 1 = C♯/D♭, 2 = D, and so on. If no key was detected, the value is -1.
# C, C#, D, D#, E, F, F#, G, G#, A, A#, B
songs.df$key <- factor(songs.df$key)
levels(songs.df$key)
levels(songs.df$key) <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')

# Let's make years a factor
songs.df$year <- factor(songs.df$year)

# Popularity is an integer, let's make it numeric
# songs.df$popularity <- factor(songs.df$popularity/10 * 10)
songs.df$popularity <- as.numeric(songs.df$popularity)

# Just like popularity, let's make duration_ms as.numeric
songs.df$duration_ms <- as.numeric(songs.df$duration_ms)

str(songs.df)

# Drop the genre, songs, and artists columns
songs.df <- songs.df[, -which(names(songs.df) == 'genre')]
songs.df <- songs.df[, -which(names(songs.df) == 'artist')]
songs.df <- songs.df[, -which(names(songs.df) == 'song')]


str(songs.df)

# Now let's partition the dataset
# Have to be careful with seed as some seeds will result in
# one of the factors ending up with only one unique value
# which breaks the logistic regression model
set.seed(121)
train.index <- sample(seq_len(nrow(songs.df)), 0.6*nrow(songs.df))
train.df <- songs.df[train.index, ]
valid.df <- songs.df[-train.index, ]

str(train.df)

# Now let's store output to file
sink(file = "data_modeling/linear_reg_output.txt")

songs.lm <- lm(popularity ~ ., data = train.df)

cat('Showing linear regression model summary:\n')
options(scipen = 999)
summary(songs.lm)

songs.lm.pred <- predict(songs.lm, valid.df)

cat('Time to test accuracy on the validation set\n')
accuracy(songs.lm.pred, valid.df$popularity)

sink()
