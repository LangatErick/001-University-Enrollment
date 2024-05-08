# 001-University-Enrollment
The university started offering online courses to reach a wider range of students. The university wants you to help them understand enrollment trends. They would like you to identify what contributes to higher enrollment. In particular, whether the course type (online or classroom) is a factor.
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/e85d1019-1ab9-427a-abc2-20c83646dc4c)

## Project_University Enrollment

**Instructions**

● Use Python or R to perform the tasks required.

● Write your solutions in the workspace provided on your certification page.

● Include all of the visualizations you create to complete the tasks.

Visualizations must be visible in the published version of the workspace. Links to external visualizations will not be accepted.

● You do not need to include code unless the question says you must.

● You must pass all criteria to pass this exam.

**The full criteria can be found here.**

Background: You are working as a data scientist at a local University. The university started offering online courses to reach a wider range of students. The university wants you to help them understand enrollment trends. They would like you to identify what contributes to higher enrollment. **In particular, whether the course type (online or classroom) is a factor.** 1

```{r warning=FALSE, message=FALSE}
#Import libraries
library(tidyverse)
library(dplyr)
library(corrplot)
library(VIM)
library(cowplot)
library(parsnip)
library(ggplot2)
library(janitor)
library(rsample)
library(e1071)#svm model
library(DataExplorer)
library(report)
library(lmtest)#test the model#harvtest(model) funtion we want p-value>0.05
# shapiro.test(residuals(model)) we want p-value>0.05
theme_set(theme_test())
library(tinytex)
library(bookdown)
library(rmarkdown)
```

```{r  warning=FALSE, message=FALSE}
#Import dataset
enrol <-read_csv("university_enrollment_2306.csv")
head(enrol)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/6a440f79-517c-4f7a-a3a8-5b11edb24be5)

```{r warning=FALSE, message=FALSE}
glimpse(enrol)
str(enrol)
```
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/3a1f489f-479c-465e-940b-5a3f91d6c84e)
```

```{r warning=F, message=FALSE}
#check the number of missing values per column
as.data.frame(
 colSums(is.na(enrol))
)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/90bc1b24-514a-409f-93d6-4d082c60cf0c)

### Data Manipulation & Cleaning
```{r}
#####Column 1
#no duplicates
sum(duplicated(enrol$course_id))

unique(enrol$course_type)#"classroom" "online"  #no missing detected
enrol$course_type <- as.factor(enrol$course_type)
#convert to a factor variable

#Enrollment_Count-Nothing to be changed
#nothing to be changed
unique(enrol$year)
#2018 2020 2016 2013 2019 2014 2021 2022 2011 2015 2012 2017
#Nothing to be amended

###Pre_score should be numeric###
enrol$pre_score <- as.numeric(enrol$pre_score)
sum(is.na(enrol$pre_score))
enrol$pre_score <- ifelse(is.na(enrol$pre_score), 0, enrol$pre_score)

###Post_Score should be numeric
class(enrol$post_score)
sum(is.na(enrol$post_score))
enrol$post_score <- ifelse(is.na(enrol$post_score), 0, enrol$post_score)
##########
unique(enrol$pre_requirement)#Replace missing with NONE
enrol$pre_requirement<-ifelse(is.na(enrol$pre_requirement),
                              "None",enrol$pre_requirement)
# enrol$pre_requirement <- as.factor(enrol$pre_requirement)
# class(enrol$pre_requirement)
# levels(enrol$pre_requirement)

################
unique(enrol$department)#Replace Math-Mathemathics#no missing values
enrol$department <- ifelse(enrol$department=="Math",
                           "Mathematics",
                           enrol$department)
# enrol$department <- as.factor(enrol$department)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/870f6ebb-4f4f-4161-9490-29c3bb6b7233)

![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/a39af5fa-e5e8-4c09-ac53-aec6a731645a)


```{r}
#To this far we have corrected our variables as per the instruction
glimpse(enrol)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/f1a849ca-4855-49b4-b0bf-85deebe13670)

# Tasks: Our Objectives

**Write your answers in your workspace.**

## 1. For every column in the data:

a.  State whether the values match the description given in the table above.
```{r}
str(enrol)
#All the variables has been corrected as per the description
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/552c3a19-ee40-4958-b030-5711fb198863)

b\. State the number of missing values in the column.

```{r}
#Number of missing values per column
colSums(is.na(enrol))
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/d8474db7-5a50-44cd-b0c8-c2c3aff5619f)

c\. Describe what you did to make values match the description if they did not match.

-   course_id- the column was OK

-   course_type- Nothing to be corrected

-   year-Nothing to be corrected

-   enrollment_count-Nothing to be changed

-   pre_score- I changed the variable to numeric, then I replace the Missing values with "0" Zero

-   post_score-I replace the Missing values with "0"

-   pre_requirement- I Replace missing with "`NONE`"

-   department- There were no missing values, I Replace '*`Math`*'- with "*`Mathemathics`*"

## 2. Describe the distribution of the enrollment counts. Your answer must include a visualization that shows the distribution.

```{r}
d1 =enrol %>% 
  group_by(year) %>% 
  summarise(Total_yearly=sum(enrollment_count))
```

```
range(d1$Total_yearly)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/1528d4c0-8fdc-4a4d-a2fd-8be9841ea42f)

```
hist(d1$Total_yearly, col='orange')
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/0e3a87c8-87fe-4623-a389-54b3d423efe3)

```
p1 <- d1 %>% 
  ggplot(aes(x=factor(year), y=Total_yearly))+
  geom_col(position = 'dodge', fill=rainbow(12)) +
  scale_y_continuous(labels = scales::comma)+
  geom_text(aes(label=Total_yearly),
            position = position_stack(0.4),
            vjust=-.2,
            hjust=-.04
            )+
  ggtitle('Distribution of Enrollment Per Year')+
  coord_flip()
p1
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/11a788cb-09ce-413d-a5f3-18aa82553f2f)

The **Range** of the Total enrollment per year is `32696 - 38897,` and the histogram of the Total enrollment shows that the Enrollment Yearly is normally distributed.
```{r}
d <- enrol %>% 
     group_by(course_type) %>% 
    summarise(Total_enrol= sum(enrollment_count)) %>% 
    arrange(desc(Total_enrol))
# DT::datatable(d)

p0 <- d %>% 
  ggplot(aes(x=course_type, y=Total_enrol))+
  geom_col(fill=rainbow(2)) +
  scale_y_continuous(labels = scales::comma)+
  geom_text(aes(label=Total_enrol),
            vjust=4,
            hjust=-.3
            ) +
  ggtitle('Distribution of Enrollment Per Course Type')



d2 <- enrol %>% 
  group_by(pre_requirement) %>% 
  summarise(Total_P=sum(enrollment_count))
  
p2 <- d2 %>% 
  ggplot(aes(x=factor(pre_requirement), y=Total_P))+
  geom_col(fill=rainbow(3)) +
  geom_text(aes(label=Total_P),
            position = position_stack(0.5)
                        )+
  ggtitle('Distribution of Enrollment Per Requirement')

d3 <- enrol %>% 
  group_by(department) %>% 
  summarise(Total_D=sum(enrollment_count))
p3 <-  d3 %>%  
   ggplot(aes(x=department, y= Total_D, fill=department))+
   geom_col()+
   geom_text(
     aes(label=Total_D),
     position = position_stack(0.6)
   )+
   theme(legend.position = 'none')+
   ggtitle('Distribution of Enrollment  Count Per Department')

plot_grid(p0, p2, p3, labels = "auto")
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/89332835-4466-45db-ad39-de8721c3b09c)
**In the above charts.**

-Most students prefer `Online` Classes

-When the course requirement is `NONE`, the enrollment will also goes up.

-Most students prefer `Technology` courses

```{r}
enrol %>%
select(starts_with(c('enrol', 'year', 'pre_s', 'post'))) %>%
cor() %>%
corrplot(type = "upper", addCoef.col = "black", diag=FALSE)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/9b2e3bc5-a2d7-4fe0-9e7c-07ca9c89eaad)

We have the correlation coefficients in each box. Positive correlations are in blue. Negative correlations are in red.

r \>= 0.5 large or strong association

r = 0.3 medium association

r = 0.1 small or weak association

**Summary of correlations:**

In respect to the above conditions, we can conclude that , the year of enrollment, the pre-score and post-score value doesn't affect the enrollment outcome.

## 3. Create a visualization that shows how many courses were of each type. Use the visualization to:

```{r}
plot_grid(p0, p2, p3, labels = "auto")
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/9e4e3873-b740-4c2a-b7da-b2ef9448985e)

a\. State which type of course has the most observations

`-Online Courses attract more number of learners`

`-Most learners prefer Technology Courses`

`-When their are NO, or NONE requirement is needed, most learners enroll.`

b\. Explain whether the observations are balanced across the types.

`-The observations are not balanced at all, for instance Online enrollment is 347442 while Classroom enrollment is 80954.`

## 4. Describe the relationship between `course type` and the `enrollment count`. Your answer must include a visualization to demonstrate the relationship.

```{r}
d %>% 
  ggplot(aes(x=course_type, y=Total_enrol))+
  geom_col(fill=rainbow(2)) +
  scale_y_continuous(labels = scales::comma)+
  geom_text(aes(label=Total_enrol),
            vjust=4,
            hjust=-.3
            ) +
  ggtitle('Distribution of Enrollment Per Course Type')
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/fa76f3c6-7956-46c4-b770-0fd7a0c4da40)

```
enrol %>% 
  ggplot(aes(course_type,enrollment_count)) +
  geom_boxplot()
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/f348cbf4-f7ff-4c51-ab87-a60ad5b39843)

`-Majority of learners prefer Online compared to Classroom,`

## 5. The university wants to predict how many students will enroll in a course. State the type of machine learning problem that this is (regression/ classification/ clustering).

`-This is a Regression Problem`

# `Model Development and fitting`

## 6. Fit a baseline model to predict how many students will enroll using the data provided. You must include your code.

```{r}
names(enrol)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/85ee06ae-4327-4bb1-8d54-fbae2c81993b)

```{r}
#
set.seed(13579)
#split train/test set
split <- initial_split(enrol, prop = 0.75)
train <- training(split)
test <- testing(split)
```

```{r}
#Base line Model
#Linear Regression Model
enrol$department1 <- NULL
enrol$course_id <- NULL
model <- lm(formula = enrollment_count~ scale(year)+scale(pre_score) +
              scale(post_score)+
              factor(course_type) + factor(pre_requirement)+
              factor(department),  data=train)
summary(model)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/408d7cfc-895e-4c02-bc88-641148bb8168)

```
report(model)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/1cf8ee96-e9d3-4de7-b05b-89758bae963d)

```
pred <- predict(model, test) %>%
  round(0) 
data.frame(Actual=test$enrollment_count,Prediction= pred)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/9fa59609-77e2-4ec2-a381-e9451b4eb9b1)

```
caret::RMSE(pred,test$enrollment_count)
caret::R2(pred,test$enrollment_count)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/43371629-30d9-4da9-a3ea-78f6eb2cb2b0)

`The generic interpretation for each of our coefficients is for every one unit increase in the x variable, the y variable (enrollment_count) increases by beta units.`

What can we derive from our correlation analysis and how can we use this to inform **`enrollment_count`**?

We will just focus on the relationship between **`enrollment_count`**and each independent (x) variable. **What's not working in `enrollment`?**

1\. `Year— significant.`

`2. Department— significant`

`3. factor(pre_requirement)Intermediate — significant.`

**What is working with marketing?**

`1.Course Tpye`

`2.Pre Requirement  NONE`

**What is not as impactful?**

`1. Pre score`

`2.Post score`

## 7. Fit a comparison model to predict how many students will enroll using the data provided. You must include your code.

```{r}
#comparison Model
#Support Vector Regression Model

model2 <- svm(formula = enrollment_count~year+pre_score +post_score+
              factor(course_type) + factor(pre_requirement)+
              factor(department),  data=train)
# summary(model2)
pred2 <- predict(model2, test) %>% round(0)

data.frame(test$enrollment_count, pred2)

caret::RMSE(pred2,test$enrollment_count)
caret::R2(pred2,test$enrollment_count)
```
![image](https://github.com/LangatErick/001-University-Enrollment/assets/124883947/218a3457-8448-433f-8de5-b31eacf10d81)

## 8. Explain why you chose the two models used in parts 6 and 7.

`I choose the Two models so that I check the one with more accurate resulst`

## 9. Compare the performance of the two models used in parts 6 and 7, using any method suitable. You must include your code.

R-Square adjusted for Linear Regression Model is `0.9999398`, compared to SVM Model with R2 of `0.9962162` , with the R2 , we can conclude that the **Linear Model** Performed better than **SVM**.

## 10. Explain which model performs better and why.

**Linear Model** Performed better than **SVM. The RMSE of Linear Model is** `0.2975784` **smaller than for SVM Model at** `2.334824`
