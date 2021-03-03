library(dplyr)
library(ggplot2)
library(titanic)

titanic <- titanic_train %>% 
  select(PassengerId, Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>% 
  filter(!is.na(Age)) %>% mutate(Survived = factor(Survived),
                                 Pclass = factor(Pclass),
                                 Sex = factor(Sex))


titanic %>% ggplot(aes(Age,y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack")

titanic %>% ggplot(aes(Age,y = ..count.., fill = Sex)) +
  geom_density() +
  facet_grid(Sex ~.)

titanic %>% ggplot(aes(Sex, fill = Survived)) +
  geom_bar(position = position_fill()) 

titanic %>% ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack") +
  facet_grid(Survived ~.)

titanic %>%  
  ggplot(aes(Survived, Age, colour = Sex)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

titanic %>% ggplot(aes(Age, y = ..count.., fill = Pclass)) +
  geom_density(alpha = 0.5, position = "stack") +
  facet_grid(Sex ~.)

titanic %>% ggplot(aes(Sex, fill = Pclass)) +
  geom_bar()

titanic %>% ggplot(aes(Age, y = ..count.., fill = Pclass)) +
  geom_density(alpha = 0.5, position = "stack") +
  facet_grid(Sex ~ Survived)

titanic %>% ggplot(aes(Sex, fill = Pclass)) +
  geom_bar() +
  facet_grid(Survived ~.)

titanic %>% ggplot(aes(Survived, fill = Pclass)) +
  geom_bar()

titanic %>% ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())


