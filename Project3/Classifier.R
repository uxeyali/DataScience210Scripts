library(tidyr)
library(dplyr)
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)
#  ====== TREE ========

Aeon <- read.csv("Aeon_MO_Classification.csv", 
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = TRUE)

summary(Aeon)


Aeon <- select(Aeon, -PositiveMO)
Aeon <- select(Aeon, -YardiHOHID)
Aeon <- select(Aeon, -Coats_intx_resource)

head(Aeon)

# ======= 1 TREE =========
# Aeon_tree <-
#   Aeon %>%
#   # Tild is used by R to replace = because = is assignment
#   rpart(formula = NegativeMO ~ .,
#         control = rpart.control(minsplit=4, cp=0.018))
# 
# # run the following to install rpart.plot
# prp(Aeon_tree)
# 
# # summary(Aeon_tree)

# ======= 2 TREE =========

test_rows <- sample(1:nrow(Aeon),0.30*nrow(Aeon),replace=F)
head(sort(test_rows),20)

my.inspect <- function(df) df %>% select(NegativeMO, in.test, original.row) %>% head
Aeon <-
  Aeon%>%
  mutate(original.row = 1:nrow(Aeon), in.test = row.names(Aeon) %in% test_rows)

Aeon$NegativeMO <- factor(Aeon$NegativeMO)

# Training
Aeon_train <-
  Aeon %>%
  filter(!in.test)
my.inspect(Aeon_train)

# Testing
Aeon_test <-
  Aeon %>%
  filter(in.test)
my.inspect(Aeon_test)

Aeon_train <- select(Aeon_train, -original.row)
Aeon_test <- select(Aeon_test, -original.row)

# Tree
Aeon_tree <-
  Aeon_train %>%
  # . means everything else
  rpart(formula = NegativeMO ~ .,control=rpart.control(cp=0.02,minsplit = 2))
prp(Aeon_tree, type=4, extra=3)

# type="class" tells the predictor this is a prediction problem involving classification and not regression

Aeon_predict <-
  predict(Aeon_tree,newdata=Aeon_test,type="class")
Aeon_predict


misclassify <- function(actual_labels, predict_labels) {
  conf.matrix <- table(actual_labels, predict_labels)
  rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
  colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
  numcorrect <- sum(actual_labels == predict_labels)
  numincorrect <- length(actual_labels) - numcorrect
  MCrate <- numincorrect/length(actual_labels)
  return(list(conf.matrix=conf.matrix, misclassification.rate=MCrate))
}
misclassify(Aeon_test$NegativeMO, Aeon_predict)