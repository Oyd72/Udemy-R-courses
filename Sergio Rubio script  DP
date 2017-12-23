
########################
# 1 - Data preparation #
########################

#Set current directory:
getwd()
setwd("./RProgrammingCourse/data")
getwd()

#Read data file:
#Basic import fin <- read.csv("Future-500.csv")
fin <- read.csv("Future-500.csv", na.strings=c(""))

head(fin)
tail(fin, 10)

str(fin)
summary(fin)

#Convert to factor:
fin$ID <- as.factor(fin$ID)
fin$Inception <- as.factor(fin$Inception)

str(fin)

#Factor Variable Trap (FVT):
#Converting characters to numerics:
a <- c("12", "13", "14", "12", "12")
typeof(a)

b <- as.numeric(a)
b
typeof(b)

#Converting factors to numerics:
z <- factor(a <- c("12", "13", "14", "12", "12"))
typeof(z)

#Wrong way:
y <- as.numeric(z)
y
typeof(y)

#Right way
x <- as.numeric(as.character(z))
x
typeof(x)

#FVT example:

head(fin)
str(fin)

#Wrong way:
#fin$Profit <- factor(fin$Profit)
#str(fin)

#fin$Profit <- as.numeric(fin$Pro)
#str(fin)

#Character substitution:
?sub #Replace first instance of pattern
?gsub #Replace all instances of pattern

fin$Expenses <- gsub(" Dollars", "", fin$Expenses)
fin$Expenses <- gsub(",", "", fin$Expenses)
fin$Expenses

str(fin)

fin$Revenue <- gsub("\\$", "", fin$Revenue)
fin$Revenue <- gsub(",", "", fin$Revenue)
fin$Revenue

fin$Growth <- gsub("%", "", fin$Growth)
fin$Growth <- gsub(",", "", fin$Growth)
fin$Growth

#Convert to numeric
fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)

str(fin)
summary(fin)

#Dealing with missing data:
    #1. Predict with 100% accuracy
    #2. Leave record as is
    #3. Remove record entirely
    #4. Replace with mean or median
    #5. Fill in by exploring correlations and similarities

#Missing values (NA):
?NA #Not Available

TRUE #1
FALSE #0
NA

TRUE == FALSE
TRUE == 5
FALSE == FALSE
FALSE == 0
NA == TRUE
NA == FALSE
NA == NA

#Locating missing data:
head(fin, 24)
complete.cases(fin)
fin[complete.cases(fin),]
fin[!complete.cases(fin),]

# fin <- read.csv("Future-500.csv", na.strings=c(""))

#Filtering for non-missing data:
fin[fin$Revenue == 9746272 , ] #Contains NAs
which(fin$Revenue == 9746272)
?which
fin[which(fin$Revenue == 9746272),]
fin[which(fin$Employees == 45),]


#Filtering for missing data:
head(fin, 24)

fin == NA
fin[fin$Expenses == NA,]

is.na(fin$Expenses)
fin[is.na(fin$Expenses),]

#Removing records with missing data:
fin_backup <- fin

fin[!complete.cases(fin),]
fin[is.na(fin$Industry),]
fin[!is.na(fin$Industry),]

fin <- fin[!is.na(fin$Industry),]
fin[is.na(fin$Industry),]
fin[!complete.cases(fin),]

#Resetting the dataframe index:
rownames(fin) <- 1:nrow(fin)
rownames(fin)

rownames(fin) <- NULL
rownames(fin)

#Replacing missing data - Factual analysis:
fin[!complete.cases(fin),]
fin[is.na(fin$State),]

fin[(is.na(fin$State) & fin$City == 'New York'),'State'] <- 'NY'
fin[(is.na(fin$State) & fin$City == 'San Francisco'),'State'] <- 'CA'

#Replacing missing data - Median imputation method:
fin[!complete.cases(fin),]

mean(fin[fin$Industry=='Retail' & !is.na(fin$Employees),'Employees'])
mean(fin[fin$Industry=='Retail','Employees'], na.rm=TRUE)

median(fin[fin$Industry=='Retail' & !is.na(fin$Employees),'Employees'])
med_empl_retail <- median(fin[fin$Industry=='Retail','Employees'], na.rm=TRUE)
med_empl_retail

fin[is.na(fin$Employees) & fin$Industry=='Retail',]
fin[is.na(fin$Employees) & fin$Industry=='Retail', 'Employees'] <- med_empl_retail

med_empl_finserv <- median(fin[fin$Industry=='Financial Services','Employees'], na.rm=TRUE)
med_empl_finserv

fin[is.na(fin$Employees) & fin$Industry=='Financial Services',]
fin[is.na(fin$Employees) & fin$Industry=='Financial Services','Employees'] <- med_empl_finserv

fin[!complete.cases(fin),]

#Growth:
med_growth_constr <- median(fin[fin$Industry=='Construction','Growth'], na.rm=TRUE)
med_growth_constr

fin[is.na(fin$Growth) & fin$Industry=='Construction','Growth'] <- med_growth_constr

fin[!complete.cases(fin),]

#Revenue:
med_rev_constr <- median(fin[fin$Industry=='Construction','Revenue'], na.rm=TRUE)
med_rev_constr

fin[is.na(fin$Revenue) & fin$Industry=='Construction','Revenue'] <- med_rev_constr

#Expenses:
med_exp_constr <- median(fin[fin$Industry=='Construction', 'Expenses'], na.rm=TRUE)
med_exp_constr

fin[is.na(fin$Expenses) & fin$Industry=='Construction' & is.na(fin$Expenses),'Expenses'] <- med_exp_constr

fin[!complete.cases(fin),]

#Replacing missing data - Deriving values method:
#Profit = Revenue - Expenses
#Expenses = Revenue - Profit

fin[is.na(fin$Profit),'Profit'] <- (fin[is.na(fin$Profit),'Revenue'] - fin[is.na(fin$Profit),'Expenses'])
fin[!complete.cases(fin),]

fin[is.na(fin$Expenses), 'Expenses'] <- (fin[is.na(fin$Expenses), 'Revenue'] - fin[is.na(fin$Expenses), 'Profit'])
fin[!complete.cases(fin),]

#Visualization:
library(ggplot2)

#A scatterplot classified by industry showing revenue, expenses, profit
p <- ggplot(data=fin)
p + geom_point(aes(x=Revenue, y=Expenses, colour=Industry, size=Profit))

#A scatterplot that includes industry trends for the expenses~revenue relationship
d <- ggplot(data=fin, aes(x=Revenue, y=Expenses, colour=Industry))
d + geom_point() + geom_smooth(fill=NA, size=1.2)

#Boxplot showing growth by industry:
f <- ggplot(data=fin, aes(x=Industry, y=Growth, colour=Industry))
f + geom_boxplot(size = 1)
f + geom_jitter() + geom_boxplot(size = 1, alpha=0.5, outlier.color=NA)
