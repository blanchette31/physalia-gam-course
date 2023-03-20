#Prepare workspace 

rm(list=ls())

# Darlingtonia example
library("readr")
library("dplyr")
library("ggplot2")
library("here") # shortcut to specify file location relative to working directory

# load data
wasp <- read_csv(here("data", "darlingtonia.csv"), comment = "#",
                 col_types = "dl") # d = decimal , l = logical;  error code if doesn't match expectations. Great way to verify if data frame is what we want. 

head(wasp)
tail(n = 10 , wasp)

# tibbles are really fast compared to dataframes but also are better for visualizing data in the console especially when working with large dataframes as it will not fill the entire console with data as opposed to dataframes 

m <- glm(visited ~ leafHeight, data = wasp, family = binomial(link = "logit")) #do not put the name of the dataframe in the formula it will cause problems. Don't do wasp$visited and wasp$leafHeight use the "data =" argument

summary(m)

# generate data that we want to predict at to visualize the model
pdat <- with(wasp,
             tibble(leafHeight = seq(min(leafHeight),
                                     max(leafHeight),
                                     length = 100)))
pdat

# predict
pred <- predict(m, pdat, type = "link", se.fit = TRUE) # se.fit to create standard error to show the confidence interval in the plot 
str(pred)

p_link <- predict(m, pdat, type = "response")
head(p_link) # these are estimated probabilities on a scale of 0 to 1

# inverse link
ilink <- family(m)$linkinv # g-1()

# add prediction data and predicted values
pdat <- pdat %>%
  bind_cols(data.frame(pred)) %>%
  mutate(fitted = ilink(fit),
         upper = ilink(fit + (2 * se.fit)),
         lower = ilink(fit - (2 * se.fit)))
# plot
ggplot(wasp, aes(x = leafHeight,
                 y = as.numeric(visited))) +
  geom_point(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper,
                  x = leafHeight), data = pdat,
              inherit.aes = FALSE, alpha = 0.2) +
  geom_line(data = pdat, aes(y = fitted)) +
  labs(x = "Leaf Height [cm]",
       y = "Probability of visitation")

# likelihood ratio test
# compare m with a model without leafHeight
m0 <- update(m, . ~ . - leafHeight)

# (Generalized) Likelihood Ratio Test (GLRT)
anova(m0, m, test = "LRT")

# what about polynomials of leafHeight?
m2 <- update(m, . ~ . + I(leafHeight^2))
summary(m2)

anova(m, m2, test = "LRT")
