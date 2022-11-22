class.df<-read.csv('dataset/songs_updated_v3.csv', stringsAsFactors = TRUE)
# View(class.df)

library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)

class.df$popularity <- as.numeric(class.df$popularity)
summary(class.df$popularity)
class.df$popularity <- ifelse(class.df$popularity < 60, 0, 1)


set.seed(11)
select_index <- c(-1, -2, -5, -9, -18)
selected.df <- class.df[, select_index]
train.index <- sample(seq_len(nrow(selected.df)), nrow(selected.df) * 0.6)
train.df <- selected.df[train.index,]
valid.df <- selected.df[-train.index, ]

# Default classification tree,
names(class.df)
names(train.df)
str(train.df)

# Now let's store output to file
sink(file = "data_modeling/classification_tree_output.txt")

default.ct <- rpart(popularity ~ ., data = train.df, method = 'class' )

default.ct.point.pred <- predict(default.ct, valid.df, type = 'class')
confusionMatrix(default.ct.point.pred, factor(valid.df$popularity), positive = '1')

# rpart.plot( default.ct, extra = 1)

default.ct$variable.importance

sink()
