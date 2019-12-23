#Kaggle.com dataset.Download the dataset from the following link:
#https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data/download/eM0xChhsTAba4VhLeD2K%2Fversions%2FfgruQrMFEPB6Vq19bCe2%2Ffiles%2FGlobalLandTemperaturesByMajorCity.csv?datasetVersionNumber=1
#https://github.com/djinasarre/climate_change/blob/master/climate_changedef.R
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
url <- "https://github.com/djinasarre/climate_change/raw/master/GlobalLandTemperaturesByMajorCity.csv"
tmp_filename <- tempfile()
download.file(url, tmp_filename)
cl_change <- read_csv(tmp_filename)
file.remove(tmp_filename)

# This is the structure of cl_change data 
str(cl_change)

#We can see the number of unique countries, cities and dates are 
#in our dataset:

distinct <- cl_change %>% 
  summarize(n_City = n_distinct(City),
            n_Country = n_distinct(Country),
            n_dt=n_distinct(dt))
distinct 
# 100 cities, 49 countries and 3239 dates.



#we will insert a new column for the continent each country belongs
#We create the data frame  

cl_change <- as.data.frame(cl_change)  
cl_change$continent <- countrycode(sourcevar = cl_change[, "Country"],
                                   origin = "country.name",
                                   destination = "continent")
# we use mutate to create columns of years and months to work in a easier way.

cl_change <-mutate (cl_change, year= year(dt), month = month(dt))



cl_changeavg<- cl_change%>% group_by(City,year,continent,Latitude,Country)%>%summarise(average= mean(AverageTemperature), averageUncertainty=mean(AverageTemperatureUncertainty))
cl_changeavg<- na.omit(cl_changeavg)
#As we see in these two plots below, European continent temperatures  were the first to be taken and 
#with a very high degree of uncertainty, it will not be until approximately 1890 when we see 
#temperatures taking in all the cities under study with average Uncertainty lowest than 2.
cl_changeavg%>% ggplot(aes(year, averageUncertainty, color = Country,alpha = 1/10)) +  geom_point()+
cl_changeavg%>% ggplot(aes(year, average, color = continent,alpha = 1/10 )) +  geom_point()

# We include a line for each city so we can gain a better intuition of how these points relate to each other
cl_changeavg%>% ggplot(aes(year, average, color = City,alpha = 1/10 )) +  geom_point()+geom_line()+geom_smooth(method="lm")
#If We  plot the  mean Average temperature for each year since 1753 we obseve a step up around 1800
#as we incorporate cities.
cl_change<- na.omit(cl_change)
avg_evo <- cl_change %>% 
  group_by(year) %>% filter( year>=1743 & year<=2013 )%>%
  summarize(average= mean(AverageTemperature, na.rm = TRUE))%>%
  qplot(year, average, data = .)+ xlim(1743, 2013)+ylim(0,22.5)+
  geom_point() +
  geom_line()+
  geom_smooth(method="lm")
avg_evo

#We reduce the period observed to the period from 1890 when they incorporate measurements of all
# the cities under study.
avg_ev <- cl_change %>% 
  group_by(year) %>% filter( year>=1890 & year<=2013 )%>%
  summarize(average= mean(AverageTemperature, na.rm = TRUE))%>%
  qplot(year, average, data = .)+ xlim(1890, 2013)+ylim(17,21)+
  geom_point() +
  geom_line()+
  geom_smooth(method="lm")
avg_ev
# We see that the evolution of mean land average temperatures has increased.
avg_evo1 <- cl_change %>% filter(year >= 1890)%>%
  group_by(year) %>%
  summarize(average= mean(AverageTemperature, na.rm = TRUE))

preindustrial<- cl_change%>% filter(year >= 1890& year <= 1900)%>% group_by(year)%>%summarise(average1890_1900=mean(AverageTemperature, na.rm = TRUE))
preindustrial <- mean(preindustrial$average1890_1900)

# We want to plot the global mean land temperature difference from preindustrial years (1890-1900) in ºC
avg_evo2 <- avg_evo1 %>%
  group_by(year) %>%filter(year>=1890&year<=2012)%>%
  mutate (gmtd = average-preindustrial)%>%
  qplot(year, gmtd, data = .)+
  xlim(1890, 2012)+ylim(-1,1.5)+
  geom_line()+
  geom_point() +
  geom_smooth(method="lm")+
  ylab("Global mean land temperature peindustrial difference ")
avg_evo2
#We see from the graphic that difference evolution has increased since preindustrial period.

cl_change <- filter(cl_change, year >= 1900 & year<=2012)

#We also observe that the uncertainty taking temperatures has been reduced considerably over time
uncertainty_evo <- cl_change %>%
  group_by(year) %>%
  summarize(averageuncertainty = mean(AverageTemperatureUncertainty ))%>%
  qplot(year, averageuncertainty, data = .)+ xlim(1900, 2012)+ylim(0.25,0.85)+
  geom_point() +
  geom_line()+
  geom_smooth(method="lm")
uncertainty_evo


#Let's take a look at data that may be interesting:
#1. We look for highest and lowest historical average land temperatures for each City since 1900
low_temperatures <- cl_change %>% filter(AverageTemperature <= -22.285)%>%  
  select(dt, City, Country, AverageTemperature)
arrange(low_temperatures, AverageTemperature) 

high_temperatures <- cl_change %>% filter(AverageTemperature >= 37.371)%>%  select(dt, City, Country, AverageTemperature)
arrange(high_temperatures, -AverageTemperature) 

# Lets look to Average Temperature histograms and Average land temperature uncertity
#differences between the mean of period 1961-1990 and year 2012
qplot(AverageTemperature,na.rm = TRUE, geom = "histogram", binwidth = 5, data = cl_change, color = I("black"))
qplot(AverageTemperatureUncertainty,na.rm = TRUE, geom = "histogram", binwidth = 0.1, data = cl_change, color = I("black"))+xlim(0,2.2)

alpha <- cl_change%>%filter(year>=1961&year<=1990)%>% summarize(average = mean(AverageTemperature), standard_deviation = sd(AverageTemperature), median(AverageTemperature))
alpha
omega <- cl_change%>%filter(year==2012)%>% summarize(average = mean(AverageTemperature), standard_deviation = sd(AverageTemperature), median(AverageTemperature))
omega
dif<-omega-alpha
dif[,1]
# average year temperatures difference  is 0.63 ºC 
high_temperatures_year <- cl_change %>% 
  group_by(year) %>%
  summarize(average= mean(AverageTemperature ))
high_temperatures_year <- arrange(high_temperatures_year, -average)  %>% print(n=10)                                        
#We see that the in the last 14 years until 2012 the 10 highest land average temperatures per 
#year have been reached.

#Let's see how the Mediterranean countries and no mediterranean (others) have evolved over this period of time.

cl_changem <- cl_change %>%  mutate (group = case_when(
  Country %in% c("Spain", "France", "Monaco", "Italy", "Slovenia", "Croatia", "Bosnia and Herzegovina", "Montenegro", "Albania", "Greece", "Turkey", "Syria", "Lebanon", "Israel", "Egypt", "Libya", "Tunisia", "Algeria", "Morocco") ~ "mediterranean",
  Country != "mediterranean" ~ "others")) 
cl_changem %>%  group_by(year,group)%>%summarise(average=mean(AverageTemperature)) %>% 
  ggplot(aes(year, average, col = group)) +
  geom_line()
# we observe the same trend of temperature increase,more evident in the Mediterranean countries.

#Lets group by continent and look the evolution over years
cl_change2<- cl_change %>%             
  group_by(year,continent) %>%                 
  summarise(Average=mean(AverageTemperature), Uncertainty=mean(AverageTemperatureUncertainty))

c<-cl_change2 %>% filter( year>=1900)%>% group_by(year,continent ) %>%
  ggplot(aes(year,Average, col = continent, na.rm=TRUE)) +
  geom_line()+
  geom_point() +
  geom_smooth(method="lm")
c


#Now we will take the averages temperature for periods from 1900 to 1950 


cl_average0050 <- cl_change%>% filter(year >= 1900& year <= 1950)%>%group_by(City)%>%summarise(average0050=mean(AverageTemperature))

cl_averages <- cl_change%>%group_by(City)%>%summarise(averages=mean(AverageTemperature))

tab <- left_join(cl_average0050, cl_averages, by = "City") 

table <- left_join(tab, cl_change, by = "City")

table <- select(table, -dt, -AverageTemperature,-AverageTemperatureUncertainty, -month,-year)


cl_changeZ1<- table %>%             
  group_by(City,Country,Latitude,Longitude, continent) %>%                 
  summarise(Average0050=mean(average0050) )

#Now we will take the averages temperature for periods from 1961 to 1990 and the last five full years 2008 to 2012


cl_average6190 <- cl_change%>% filter(year >= 1961& year <= 1990)%>%group_by(City)%>%summarise(average6190=mean(AverageTemperature))

cl_average0812 <- cl_change%>% filter(year >= 2008& year <= 2012) %>%group_by(City)%>%summarise(average0812=mean(AverageTemperature))

tab <- left_join(cl_average6190, cl_average0812, by = "City") 

table <- left_join(tab, cl_change, by = "City")

table <- select(table, -dt, -AverageTemperature,-AverageTemperatureUncertainty, -month,-year)


cl_changeZ<- table %>%             
  group_by(City,Country,Latitude,Longitude, continent) %>%                 
  summarise(Average6190=mean(average6190), Average0812=mean(average0812) )

cl_changeZ$diffs<-cl_changeZ$Average0812-cl_changeZ$Average6190



#Lets look at the  most affected cities by climate change and
# which cities are affected less
maxdiff <- cl_changeZ %>% filter(diffs > 1.16)%>%  select(City, Country, Latitude, Longitude, continent, Average6190, Average0812,  diffs)
arrange(maxdiff, -diffs) 

mindiff <- cl_changeZ %>% filter(diffs < 0.386)%>%  select(City, Country, Latitude, Longitude, continent, Average6190, Average0812,  diffs)
arrange(mindiff, diffs) 


#let's see the difference between periods by continents and countries 
cl_changeZ%>% ggplot(aes(x = fct_reorder(continent,diffs) , y =diffs)) +
  geom_boxplot(col=(c("black","red","yellow","green","blue"))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")


cl_changeZ%>%ungroup(Country)%>%
  ggplot(aes(x = fct_reorder(Country,diffs) , y = diffs)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")

# Between periods we see that Europe is the continent and Russia
#and Ukraine the countries that have biggest differences.
Latitude <- as.character(cl_changeavg$Latitude)

cl_changeavg$Latitude <-case_when(
  str_sub(Latitude, -1, -1) == "N" ~ as.numeric(str_sub(Latitude, 1, nchar(Latitude) - 1)),
  str_sub(Latitude, -1, -1) == "S" ~ -as.numeric(str_sub(Latitude, 1, nchar(Latitude) - 1)),
  TRUE ~ NA_real_
)

cl_changeZ<- cl_changeZ%>%ungroup(cl_changeZ)%>%select("City","diffs")
cl_changeavg<- as.data.frame (cl_changeavg)
cl_changeavg <- left_join(cl_changeavg, cl_changeZ, by = "City")

cl_changeZ1<- cl_changeZ1%>%ungroup(cl_changeZ1)%>%select("City","Average0050")
cl_changeavg<- as.data.frame (cl_changeavg)
cl_changeavg <- left_join(cl_changeavg, cl_changeZ1, by = "City")
cl_changeavg<-cl_changeavg%>%mutate(diffs0050=average-Average0050)
# We tile the countries with colors representing difference between mean temperature 1900-1950 and
#each year temperature.
cl_changeavg %>% ggplot(aes(year, Country, fill = diffs0050)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdYlBu")) +
  geom_vline(xintercept=1890, col = "blue") +
  theme_minimal() +  
  theme(panel.grid = element_blank(), legend.position="bottom", text = element_text(size = 8)) +
  ylab("") + xlab("")



cl_changes<-cl_changeavg%>%select("continent", "Latitude", "year", "average", "averageUncertainty","diffs")



###We want to see how to differentiate Continents predicted with the year, difference between 
#land averages temperature for periods from 1961 to 1990 and the last five complete years 2008
#to 2012 (diffs), and cities year mean temperature (average), Latitude and cities year mean 
#average temperature Uncertainty (averageUncertainty).

y <- cl_changes$continent
set.seed(26, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.8, list = FALSE)
train_set <- cl_changes %>% slice(test_index)
test_set<- cl_changes %>% slice(-test_index)

# We change vectors train_set_images and test_set_images classes as matrix:
# Image Data is transformed to matrix because they are easier indexable.
train_set_images<- train_set[,2:6] 
test_set_images <- test_set[,2:6]

train_set_images <- as.matrix(train_set[, 2:dim(train_set)[2]])
test_set_images <- as.matrix(test_set[, 2:dim(test_set)[2]]) 
#The dataset includes two components, a train set and a test set:
#Each of these components includes now a matrix with 5 features in the columns:
names(train_set)
dim(train_set_images)
class(train_set_images)
#and change vectors train_set$continent and test_set$continent classes:
class(train_set$continent)
train_set$continent <- type.convert(train_set$continent)
class(train_set$continent)
class(test_set$continent)
test_set$continent <- type.convert(test_set$continent)
class(test_set$continent)
table(train_set$continent)

#We create simple machine learning algorithms using fewer features trying PCA and explore the 
#variance of the PCs.
col_means <- colMeans(test_set_images)
#Our predictors here have five dimensions, lets look to the correlation:
cor(train_set_images)
pca <- prcomp(train_set_images)
pc <- 1:ncol(test_set_images)
qplot(pc, pca$sdev)
#First PCs explain a large percent of the variability
summary(pca)$importance[,1:5] 
#We plot first two PCs and give us information about the class
data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2],
           continent=factor(train_set$continent)) %>%
  sample_n(4000) %>% 
  ggplot(aes(PC1, PC2, fill=continent))+
  geom_point(cex=3, pch=21)
#We try 2 dimensions since this explains about 99% of the data
library(caret)
k <- 2
x_train1 <- pca$x[,1:k]
y1 <- factor(train_set$continent)
fit <- knn3(x_train1, y1)
#transform the test set:
test_set_images <- as.matrix(test_set[, 2:dim(test_set)[2]])   
x_test1 <- sweep(test_set_images, 2, col_means) %*% pca$rotation
x_test1 <- x_test1[,1:k]
#And we predict:
 y_hat1 <- predict(fit, x_test1, type = "class")

results <- data_frame(method = "knn3 dimension reduction", 
                      Acc = confusionMatrix(y_hat1, factor(test_set$continent))$overall["Accuracy"])
results
# And give us an accuracy of 0.7152086.

#We will sample 8000 random rows from
# the training set and 800 random rows from the test set:
set.seed(27, sample.kind="Rounding")
index <- sample(nrow(train_set_images ), 8000)
x <- train_set_images[index,]
y <- factor(train_set$continent[index])

index <- sample(nrow(test_set_images ), 800)
x_test <- test_set_images[index,]
y_test <- factor(test_set$continent[index])

#Now we are ready to fit some models. Before we start, we need to add column names to
# the feature matrices as these are required by caret:
col_index <- 1:ncol(x)
colnames(x) <- 1:ncol(train_set_images)
colnames(x_test) <- colnames(train_set_images)


#We start with kNN. The first step is to optimize for  k
#To compute a distance between each observation in the test set and each observation in the 
#training set we will therefore use k-fold cross validation to improve speed.

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[ ,col_index], y, 
                   method = "knn", 
                   tuneGrid = data.frame(k = seq(9,71,2)),
                   trControl = control)



ggplot(train_knn, highlight = TRUE)
train_knn$bestTune #the parameter k=9 maximized the accuracy
train_knn$finalModel #the best performing model 
#We see the standard deviation bars obtained from the cross validation 
#samples:
train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD, 
                    ymax = Accuracy + AccuracySD))



#Now that we optimize our algorithm, we can fit it to the entire dataset:
fit_knn <- knn3(x[, col_index], y,  k = train_knn$bestTune)
y_hat_knn <- predict(fit_knn, x_test[, col_index], type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]
cm
results <- bind_rows(results,
                     data_frame(method="knn",  
                                Acc = confusionMatrix(y_hat_knn, factor(y_test))$overall["Accuracy"]))
results

#We now achieve an accuracy of about 0.85125. From the specificity and sensitivity, we
# also see that Class Africa are the hardest to detect  and the most commonly incorrectly 
#predicted is Asia.
cm$byClass[,1:2]

# Now we try to increase accuracy with random forest algorithm.
#This will take 15 minutes approximately. Be patient!

set.seed(3, sample.kind = "Rounding")
dat <- data.frame(x = x, y = y)
fit <- train(y ~ ., method = "Rborist",   
             tuneGrid = data.frame(predFixed = 1, 
                                   minNode = seq(25, 100, 25)),
             data = dat)
ggplot(fit)
# The value that maximizes the Accuracy(Boostrap) is minNode 25

control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = 25 , predFixed = c(1,2,3,4,5))


train_rf <-  train(x[, col_index], 
                   y, 
                   method = "Rborist", 
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 1250)

ggplot(train_rf)
train_rf$bestTune
#we have optimized our tree, we are going to fit our final model:
fit_rf <- Rborist(x[, col_index], y, 
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

results <- bind_rows(results,
                     data_frame(method="Rborist",  
                                Acc = confusionMatrix(y_hat_rf, y_test)$overall["Accuracy"]))
results
# Our final model give us an Accuracy of 0.99875

#Finally I want to computes the importance of each feature, we applied to an object 
#using "randomForest".

rf <- randomForest(x, y,  ntree = 1000, minNode = train_rf$bestTune$minNode,
                   predFixed = train_rf$bestTune$predFixed)
imp <- importance(rf)
#We can see which features are most being used:
imp

###########################################################################################################
###2 We want now to see how to differentiate Class Africa 
#and Others we will use dimension reduction

y1<-as.data.frame(y1)
y2<-train_set%>%  mutate (continent = case_when(continent %in% c ("Americas","Asia","Europe","Oceania") ~ "others",continent %in% c("Africa")~ "Africa"))  
x_train1<-as.data.frame(x_train1)
x_train1<-x_train1%>%mutate(y=y2$continent)
y3<-test_set %>%  mutate (continent = case_when(continent %in% c("Africa")~ "Africa",continent!= "Africa" ~ "others"))  
x_test1<-as.data.frame(x_test1)
x_test1<-x_test1%>%mutate(y=y3$continent)
# We load the data and show a plot of the predictors with outcome 
#represented with color.
x_test1%>% ggplot(aes(PC1, PC2, color = y)) +  geom_point()
##Because we want this example to run on a small laptop,
# we will consider a subset of the dataset. We will sample 1000 random rows from
# the training set and 100 random rows from the test set
set.seed(6, sample.kind="Rounding")
index <- sample(nrow(x_train1[,1:2]), 1000)
x <- x_train1[,1:2][index,]
y <- factor(x_train1$y[index])
x_train<-x%>%mutate(y=y)
index <- sample(nrow(x_test1[,1:2]), 100)
x_test <- x_test1[,1:2][index,]
y_test <- factor(x_test1$y[index])
x_test<-x_test%>%mutate(y=y_test)


if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(splines)) install.packages("splines", repos = "http://cran.us.r-project.org")
if(!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org")
if(!require(mgcv)) install.packages("mgcv", repos = "http://cran.us.r-project.org")
if(!require(nlme)) install.packages("nlme", repos = "http://cran.us.r-project.org")
#We use the training set to build a model with several of the models 
#available from the caret package. We will test out all of the 
#following models.This process will take about 15 minutes.
#We need to install some packages. Keep in mind that probably get some 
#warnings. Be patient!
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam","rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp", "adaboost", "gbm","svmRadial", "svmRadialCost", "svmRadialSigma")
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = x_train)
}) 
#We see that we train all  the models
names(fits) <- models
names(fits)
# We use sapply  to create a matrix (rows:100 colums:23) of predictions for the test set
pred <- sapply(fits, function(object) 
  predict(object, newdata = x_test))

dim(pred)
# Compute accuracy for each model on the test set and look for the 
#mean accuracy across all models.
acc <- colMeans(pred == x_test$y)
acc
which.max(acc)
mean(acc)
results <- bind_rows(results,
                     data_frame(method="Ensamble(dimension reduction AO)",  
                                Acc = mean(acc)))
results
#Our maximum accuracy is achieved with knn  model. Being the 
#average of the models 0.8586957

#We build an ensemble prediction by majority vote and compute the 
#accuracy of the ensemble.

votes <- rowMeans(pred == "others")
y_hat <- ifelse(votes > 0.5, "others", "Africa")
mean(y_hat == x_test$y )
results <- bind_rows(results,
                     data_frame(method="Ensamble(prediction by majority vote AO)",  
                                Acc = mean(y_hat == x_test$y )))
results
# The accuracy of the ensemble is 0.87
#the number of individual methods that do better than the ensemble prediction by majority vote is 1.
ind <- acc > mean(y_hat == x_test$y)
sum(ind)
models[ind]

#we  use the  accuracy estimates obtained from cross validation with the training
#data to remove the methods that do not perform well and
# re-do the ensemble only with methods with an estimated accuracy of greater than the mean. 

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
acc_hat
mean(acc_hat)

ind <- acc_hat >= mean(acc_hat)


votes <- rowMeans(pred[,ind] == "others")
y_hat <- ifelse(votes >= 0.5, "others", "Africa")
mean(y_hat == x_test$y)
results <- bind_rows(results,
                     data_frame(method="Ensamble(ensemble methods accuracy greater than the mean AO)",  
                                Acc = mean(y_hat == x_test$y )))
results
# The mean accuracy of the new ensemble is 0.88


