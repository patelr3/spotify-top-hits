library(caret)
library(pROC)

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

# Drop the genre column
songs.df <- songs.df[, -which(names(songs.df) == 'genre')]

str(songs.df)

# Now let's store output to file
sink(file = "data_modeling/log_reg_output.txt")

# Define what will be class 1
# Let's assume every value under 50 is not very popular and
# every value over 50 is popular
summary(songs.df$popularity)
songs.df$popularity <- ifelse(songs.df$popularity < 60, 0, 1)

# str(songs.df)

# Now let's partition the dataset
# Have to be careful with seed as some seeds will result in
# one of the factors ending up with only one unique value
# which breaks the logistic regression model
set.seed(121)
artist.col = which(names(songs.df) == 'artist')
song.col = which(names(songs.df) == 'song')
select.index <- c(-1, -2)
selected.df <- songs.df[, select.index]
train.index <- sample(1:nrow(selected.df), 0.6*nrow(selected.df))
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

# str(train.df)

# sapply(lapply(train.df, unique), length)

# Run the model
logit.reg <- glm(popularity ~ ., data = train.df, family = "binomial")
summary(logit.reg)

# Generate outcome
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# For now, set cutoff to 0.5
cat('Below is the starting cutoff value')
0.5
pred <- ifelse(logit.reg.pred > 0.5, 1, 0)

# Lets see confusion matrix with these values
cat('Confusion Matrix with cutoff value of 0.5')
confusionMatrix(factor(pred), factor(valid.df$popularity), positive = "1")

# Now, let's find the best cutoff
r <- roc(valid.df$popularity, logit.reg.pred)
# plot.roc(r)

cat('\nBelow is the best cutoff value')
coords(r, x = "best")

cat('\nBelow is different cutoff values')
coords(r, x = c(0.1, 0.3, 0.5, 0.7, 0.9))

best.cutoff <- coords(r, x = "best")$threshold
pred <- ifelse(logit.reg.pred > best.cutoff[1], 1, 0)
cat('\nNow the confusion matrix for the best cutoff')
confusionMatrix(factor(pred), factor(valid.df$popularity), positive = "1")

sink()
