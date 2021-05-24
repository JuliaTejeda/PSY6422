
# 1. Prepare the data 

# 1.1. Install the packages  

install.packages("tidyverse")
install.packages("psych") # allows the summary of the statistics
install.packages("corrplot") # this packages is used to do the correlation plot
install.packages("lm.beta") #package used to do the regression. 
install.packages("plotly") #package used to convert a graph into a interactive animated visualization 

# 1.2.Enable and Load the packages 

library("tidyverse")
library("psych")
library("corrplot")
library("lm.beta")
library("plotly")


# 2. Prepare the data needed to reproduce the analysis.

# 2.1.Load the data

library(readr)

mydata <- read_csv("~/Desktop/PSY6422- data analysis and managment /data /data.csv")

View(mydata)

# 2.2.Clean data

df <- mydata %>% select( p_highest, confidence2, p_selected, age, 
                         gender, education, likelihood,
                         Finished, ResponseId, choice_tertiary) %>% 
  filter(age < 70, Finished == 1)


# 2.3. Clean data to delete the finished column 
#This was done so I could use the Correlation function:

db <- df %>% mutate(ResponseId, p_selected, p_highest, likelihood,
                    confidence2, age, gender, education, 
                    choice_tertiary)

head(db, 6)


#3.Run Data analysis

#3.1. Rename the variables to group them in the data frame

ID <- rep(db$ResponseId)
Confidence<- rep(db$confidence2)
Age <-rep(db$age)
Gender<-rep(db$gender)
Selection <- rep(db$p_selected)
Probability <- rep(db$p_highest)
Groups <- rep(db$choice_tertiary) #choice condition (pre_ choice=, post choice=, post_no_choice=)
Education <- rep(db$education) # 1: no high school degree; 2: high school degree; 3: associate's degree; 4: bachelor's degree; 5: graduate degree (e.g., master's, Ph.D.)

#3.2. Group all the variables as data.frame: 

dat <- data.frame(Probability= Probability, Groups=Groups, 
                  Selection= Selection, Age= Age, Gender= Gender,
                  Education=Education, Confidence=Confidence)

head(dat, 6)

#Calculate summary statistics

describe(dat) 

#3.2. Correlation Pearson between all the variables (OPTIONAL):

round(cor(dat), digits = 2)

co <-round(cor(dat), digits = 2)

(co)

#3.2.1. Plot of the Pearson Correlation between all variables(OPTIONAL):

corrplot(co, method = "circle") 


#3.2.2. Calculate the effect sizes of

#Standardize the variables to do 

mad <- lm(scale(Selection) ~ scale(Probability), data=dat)
coef_lmbeta <- lm.beta(mad)
Selection

sv <- lapply(dat, scale) # this is to standardize all the variables
sv


#4. Visualization

# Specify and create different sub-group for the variables education, Groups and equal

dat$Groups<- as.factor(dat$Groups)#to divide the education into groups two groups
Groups <- dat$Groups 

dat$Education<- as.factor(dat$Education) #to divide the education into groups where
Education <- dat$Education

dat$equal<- ifelse(dat$Selection==dat$Probability,1,0) #to create a new variable that includes both selection and probability as equal.
Equal <- dat$equal

# 4.1.  Calculate the OSL regression

m1 <- lm(Confidence~Equal + Age + Groups + Education + Gender, data= dat)
summary(m1)  

#plot OSL regression

plot(lm(Confidence~Equal + Age + Groups + Education + Gender, data= dat))

#4.2.Calculate other statistical variables

#predict scores
p1<- predict(m1)
p1

#Confidence intervals for the variables 
confint(m1)


#4.3.Change the variables of education and groups from numeric to character

#Replace numeric levels for character where 1: no high school degree; 2: high school degree; 3: associate's degree; 4: bachelor's degree; 5: graduate degree (e.g., master's, Ph.D.)
grad <- c ("No High School", "High School degree", "Associate's degree", 
           "Undergraduate degree", "Postgraduate degree (PhD, Masters)")

Ed <- Education
(Ed <- factor(Ed, labels=grad))

education = Ed

#Replace numeric levels for the Groups to character where 

cond <- c ("pre-choice", "Post-choice", "Post-no-choice")
gc <- Groups

(gc <- factor(gc, labels = cond))

# To change the binary variable 0 & 1, as factor:

dat$BoxSelected<- ifelse(dat$equal==1, "Highest probability", 
                         "Lower probability")
SelectedBox <- dat$BoxSelected

SelectedBox

# 4.4. load and install package to add stat p-value to the graph

install.packages("ggpubr")
library("ggpubr")

#Boxplot for the variables selected box, confidence, education and groups

p <- ggplot(data=dat, aes(x= SelectedBox, y= Confidence, colour = education)) +
  stat_compare_means(label = "p.format") + 
  geom_boxplot(aes(frame= gc)) + 
  labs( title = "Does preexisting beliefs cause an illusion of control?") + 
  xlab("Probability estimate for the box") +
  ylab("Level of Confidence") 

#Plot interactive animated visualization

ggplotly(p)


#Boxplot graph visualisation for the variable of education 

p <- ggplot(data=dat, aes(x= Education, y= Confidence, colour = education)) +
  geom_boxplot(aes(frame= gc)) + 
  labs( title = "Does preexisting beliefs cause an illusion of control?") + 
  xlab("Level of Education") +
  ylab("Level of Confidence") 

p

#Plot interactive animated visualization

ggplotly(p)


#4.4.1. Plot visualisation using the mean between confidence and gender (OPTIONAL) 

#the mean of confidence

cm <- round(mean(Confidence), 2)

#the mean of Gender

mg <- round(mean(Gender), 2)

#plot the mean between confidence and gender (OPTIONAL)

install.packages("devtools")
require("devtools")

# data arrangement 

data("dat")

head(dat[, c("Gender", "Confidence", "Age")])

#Visualisation of the mean between confidence and gender with p-value(OPTIONAL)

my_comparisons <- list( c("0", "1"), c("1", "2"), c("0", "2") )

ggbarplot(dat, x = "Gender", y = "Confidence", color= "pink",  
          add = c("mean_se", "jitter"), 
          xlab = "Gender (where 0 = male, 1 = female & 2 = Others)" ) +
  labs(title = "Mean between Confidence and Gender") +
  stat_compare_means(comparisons = my_comparisons) # Add pairwise comparisons p-value

