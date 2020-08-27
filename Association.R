########## ANALYSIS OF CENSUS DATASET CALLED Adult
#########  BY TAWOSE OLAMIDE TIMOTHY
#########  UB_NUMBER: *******
########   M.Sc Big Data Science and Technology
########   Faculty of Engineering and Informatics
#######    University of Bradford. 
#######    Session: 2017

####################################################################
####################################################################
######################### ASSOCIATION RULE MINING FOR THE PROCESSED AND BINNED DATASET
## READ BINNED DATA (PROCESSED IN KNIME) STORED AS CSV INTO R.
ASSOC <- read.csv('M:\\Rstudio codes\\ASSOC_BINNED_DATA.csv',header=T,na.strings=c(""))

install.packages("arules")  # association rules package installation
library(arules)
library(arulesViz)

# CHECKING DATA STRUCTURE IS CORRECT
str(ASSOC)

## Check to confirm missing values
sapply(ASSOC, function(x) sum(is.na(x)))  # total NA in each column

# capital_gain and Capital_loss was converted to numeric from int 
# because when using the "cut" function, the data must be numeric
ASSOC$Capital_gain = as.numeric(ASSOC$Capital_gain)
ASSOC$Capital_loss = as.numeric(ASSOC$Capital_loss)

ASSOC$Capital_gain <- ordered(cut(ASSOC$Capital_gain,c(-Inf, 0, median(ASSOC$Capital_gain[ASSOC$Capital_gain > 0]), Inf)), labels = c("None", "Low", "High"))
ASSOC$Capital_loss <- ordered(cut(ASSOC$Capital_loss,c(-Inf, 0, median(ASSOC$Capital_loss[ASSOC$Capital_loss >0]), Inf)), labels = c("None", "Low", "high"))

# converting data to sparse or binary index matrix for apriori algorithm
# creating transactions
Assoc_rule <- as(ASSOC,"transactions")
summary(Assoc_rule)  # checking transactions
str(Assoc_rule)
# look at the first ten transactions
inspect(Assoc_rule[1:10])

# examine the frequency of items
itemFrequency(Assoc_rule[, 1:5])

# plot the frequency of items
#plot(x , y,xlab="x axis", ylab="y axis",  pch=19, col.axis = 'blue', col.lab = 'red', cex.axis = 1.5, cex.lab = 2)
itemFrequencyPlot(Assoc_rule, support = 0.1,col=topo.colors(6))
itemFrequencyPlot(Assoc_rule, topN = 20,col=topo.colors(6))



# set better support and confidence levels to learn more rules
ACrules <- apriori(Assoc_rule, parameter = list(support = 0.01, confidence = 0.8, minlen = 4))
ACrules

## Evaluating model performance
# summary of data association rules
summary(ACrules)

plot(ACrules)

# the number of rules found is large
# we subset the rules based on income in the right hand side with lift measure 1
rulesLowIncome <- subset(ACrules, subset = rhs %in% "Class=<=50K" &  + lift > 1.2)
rulesHighIncome <- subset(ACrules, subset = rhs %in% "Class=>50K" &  + lift > 1.2)

plot(rulesLowIncome)
plot(rulesHighIncome)

# sorting  rules by confidence of the first five rules
inspect(sort(rulesLowIncome, by = "confidence")[1:3])
inspect(sort(rulesHighIncome, by = "confidence")[1:3])

##### Visualizing Association Rules
# Network-based visualization of rules
IncomeLowrules <- head(sort(rulesLowIncome, by="lift"),10)
IncomeHighrules <- head(sort(rulesHighIncome, by="lift"),10)
plot(IncomeLowrules, method="graph")
plot(IncomeHighrules, method="graph")

#Graph-based visualization with itemsets as vertices.
plot(IncomeLowrules, method="graph", control=list(type="itemsets"))
plot(IncomeHighrules, method="graph", control=list(type="itemsets"))
plot(IncomeHighrules, method="graph", control=list(type="items"))


# writing the rules to a CSV file
write(rulesLowIncome, file = "M:\\Rstudio codes\\LowIncome.csv",
      sep = ",", quote = TRUE, row.names = FALSE)
write(rulesHighIncome, file = "M:\\Rstudio codes\\HighIncome.csv",
      sep = ",", quote = TRUE, row.names = FALSE)



