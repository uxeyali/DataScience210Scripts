library(tidyr)
library(dplyr)
library(arules)
library(arulesViz)
library(rpart)
library(rpart.plot)

AeonData2 <- read.csv("aeon_transactions.csv", 
                     header = TRUE,
                     sep = ",")

# head(AeonData2[,1:6])

AeonRules <-
  # Remove 105 when checking Negatives and remove 104 when checking Positive rules.
  AeonData2[-105] %>%
  mutate_if(is.integer, as.factor) %>%
  as("transactions") %>%
  apriori(parameter = list(supp = 0.2, conf = 0.10, maxlen=2))

AeonRules2 <-
  # Remove 105 when checking Negatives and remove 104 when checking Positive rules.
  AeonData2[-104] %>%
  mutate_if(is.integer, as.factor) %>%
  as("transactions") %>%
  apriori(parameter = list(supp = 0.2, conf = 0.10, maxlen=3))

# Find rules that have negative moveouts
rules_negative <-
  subset(AeonRules, subset = rhs %in% "Negative.MO=1")%>%
  sort(by="lift")
inspect(rules_negative[1:10])

# rules_N_DF <- as(rules_negative, "data.frame")
# write.csv(rules_N_DF, file = "NegativeRules.csv")
# 
# # plot the negative moveout rules
# plot(rules_negative, method="grouped")

# Find rules that have positive moveouts
rules_positive <-
  subset(AeonRules2, subset = rhs %in% "Positive.MO=1")%>%
  sort(by="lift")
inspect(rules_positive[1:10])
#
# 
# rules_P_DF <- as(rules_positive, "data.frame")
# write.csv(rules_P_DF, file = "PositiveRules.csv")
# 
# # plot the negative moveout rules
# plot(rules_positive, method="grouped")


