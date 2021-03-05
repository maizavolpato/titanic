library(dplyr)
library(ggplot2)
library(titanic)

titanic <- titanic_train %>% 
  select(PassengerId, Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>% 
  filter(!is.na(Age)) %>% mutate(Survived = factor(Survived),
                                 Pclass = factor(Pclass),
                                 Sex = factor(Sex))


titanic %>% ggplot(aes(Age,y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack") +
  ylab("Total pessengers")+
  ggtitle("Total of pessengers per Age and Sex")

titanic %>% ggplot(aes(Age,y = ..count.., fill = Sex)) +
  geom_density() +
  facet_grid(Sex ~.) +
  ylab("Total pessengers")+
  ggtitle("Total of pessengers per Age and Sex separately") +
  theme(legend.position = "none")

titanic %>% ggplot(aes(Sex, fill = Survived)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion") +
  ggtitle("Proportion of survived per Sex")

titanic %>% ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack") +
  facet_grid(Survived ~.)+
  ylab("Total pessengers") +
  ggtitle("Total of pessengers survived per Age and Sex")

titanic %>%  
  ggplot(aes(Survived, Age, colour = Sex)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  ggtitle("Proportion of pessengers survived per Age and Sex")

titanic %>% ggplot(aes(Age, y = ..count.., fill = Pclass)) +
  geom_density(alpha = 0.5, position = "stack") +
  facet_grid(Sex ~.) +
  ylab("Total pessengers") +
  ggtitle("Total of pessengers per Class and Age")

titanic %>% ggplot(aes(Sex, fill = Pclass)) +
  geom_bar() +
  ylab("Total pessengers") +
  ggtitle("Total of pessengers per Sex and Class")

titanic %>% ggplot(aes(Age, y = ..count.., fill = Pclass)) +
  geom_density(alpha = 0.5, position = "stack") +
  facet_grid(Sex ~ Survived) +
  ylab("Total pessengers") +
  ggtitle("Total of pessengers survived per Class and Sex")

titanic %>% ggplot(aes(Sex, fill = Pclass)) +
  geom_bar() +
  facet_grid(Survived ~.) +
  ylab("Total pessengers") +
  ggtitle("Total of pessengers per Sex and Class versus Survived")

titanic %>% ggplot(aes(Survived, fill = Pclass)) +
  geom_bar() +
  ylab("Total pessengers") +
  ggtitle("Total of pessengers survived per Class")

titanic %>% ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion") +
  ggtitle("Proportion of pessengers survived per Class")

titanic %>% ggplot(aes(Pclass, Fare, colour = Pclass)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_y_log10() +
  ylab("Fare (log scale)") +
  ggtitle("Proportion of pessengers per Class and Fare")

titanic %>% ggplot(aes(Pclass, Fare, colour = Survived)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_y_log10() +
  ylab("Fare (log scale)") +
  ggtitle("Proportion of pessengers survived per Clas and Fare")
