
#install packages
library('devtools')
install_github('philchalmers/mirt')

# load packages
library(tidyverse) # For data wrangling and basic visualizations
library(psych) # For descriptive stats and assumption checks
library(mirt) # IRT modeling

# load data ----
## Conway et al. (2019) Data
wmir <- read.csv("../data/WMI_Read_Han_wide.csv")[,-1]
wmirot <- read.csv("../data/WMI_Rot_Han_wide.csv")[,-1]

colnames(wmir) <- c("Subject", 
                    "V1.3", "V1.4","V1.5", "V1.6", "V1.7",
                    "V2.3", "V2.4","V2.5", "V2.6", "V2.7",
                    "V3.3", "V3.4","V3.5", "V3.6", "V3.7")

colnames(wmirot) <- c("Subject", 
                      "S1.2","S1.3", "S1.4","S1.5", 
                      "S2.2","S2.3", "S2.4","S2.5", 
                      "S3.2","S3.3", "S3.4","S3.5")


# Wmi is the full dataset (N = 261)
wmi <- merge(wmir, wmirot, by = "Subject")

head(wmi)
## Shows subjects (261 subjects), tested for reading span and rotation span
## Integers for number of correctly recalled element for each item
## Reading span: 15 items ("V") in 3 blocks (V1, V2, V3). Item size form 3-7 (given by Vx.3, .4, .5, etc).
## All correct for V2.5 is 5, for V1.7 the max is 7 etc
## Rotation span: 12 items ("S") in 3 blocks (S1, S2, S3). Item size form 2-5 (given by Sx.2, .4, .5, etc).

# Key theory: terms ----
# Scale: the individuals standing on a latent trait/construct (i.e. how good quality is the individual? ), e.g. multiple items on a test
# Dichotim models: 2 response categories (yes/no, correct/incorrect)
# polytomous model: >2 response categories, e.g. 1-5 in likerts
# unidim: 1 latent trait
# multidim: >1 latent traits
# theta: the latent structure/trait that is measured by the scale
# information: precision of measurement of the item/test. High information, high precision. Represented as a function
## reflecting the range of trait level over which this item/test is most useful for distinguishing among individuals
# Item Characteristic Curve (ICC): person’s probability for endorsing an item category (p) and the level on the construct
## measured by the scale (theta). Slope is used to determine if an item is able to differentiate well based on ability level (high value better)
# Item Difficulty Parameter (b): the trait level on the latent scale where a person has a 50% chance of responding positively to the item
## For polytomous models, multiple threshold parameters (ds) are estimated
# Item Discrimination Parameter (a): how accurately a given item can differentiate individuals based on ability level. 
## describes the strength of an item’s discrimination between people with trait levels below and above the threshold b
## the a parameter for an item reflects the magnitude of item reliability (how much the item is contributing to total score variance)



# Unidimensional Dichotomous IRT Models: 1PL (Rasch) ----
itemplot(shiny = TRUE) # allows interactive shiny plot

# make rotational data into a binary response (all correct = correct = "1". Not all correct = incorrect = 0)
dat1 <- key2binary(wmirot[,-1],
                   key = c(2,3,4,5,2,3,4,5,2,3,4,5))
head(dat1)

# 1PL (Rasch) model: discrimination parameters for all items are fixed to 1, while difficulty paramters are freely estimated in the model
# Model specification. Here we indicate that all columns in the dataset (1 to 12) measure the same latent factor ("rotation")
## A 1PL model assumes that different items do NOT vary in the ability to discriminate between persons with different latent trait levels.
### So each item has different difficulty (hard item, only high ability likely to get correct) 
### and giving different information (traits are good at discrimination at different trait levels, e .g. easy question better at low ability)

uniDich.model1 <- mirt.model("rotation = 1 - 12")

# Model estimation. Here we indicate that we are estimating a Rasch model, and standard errors for parameters are estimated.
uniDich.result1 <- mirt::mirt(dat1, uniDich.model1, itemtype = "Rasch", SE = TRUE)

# Results
M2(uniDich.result1)
## Good fit if: RMSEA < .06; SRMSR < .08; CFI > .95; TLI > .95
### All of these thresholds are met, good model fit

itemfit(uniDich.result1)
## fit of the items, how well does each item fit the model?
##Non-significant S-X2 values and RMSEA < .06 are usually considered evidence of adequate fit for an item
### All items fit the model

residuals(uniDich.result1, df.p = T) 
## test the local independence assumption, all good here
## Large and significant LD indices are indicators of potential issues of local dependence

head(personfit(uniDich.result1))
## person fit statistics (Zh) indicate how much a person’s responses on this test deviates from the the model prediction

# IRT parameters from the estimated model. For this example, we are obtaining simplified output without SEs/CIs (simplify = TRUE) for conventional IRT parameters (IRTpar = TRUE).
coef(uniDich.result1,simplify = TRUE, IRTpar = TRUE)$items
## a = 1 fixed in this model. b (difficulty level) is estimated
## larger b parameter indicates higher difficulty of an item. 
## For example, the second item, S1.3 (item size 3 in the 1st block), has a b = -0.94. 
## This indicates that, according to the model estimation, a person with ability level that is 0.94 SD 
### below the average has 50% of chance to answer this item (S1.3) correctly (i.e. easy question)

# In the function we cam specify the range of theta we'd like to visualize on the x axis of the plots. 
## In this example we set it to -4 to 4 (4 SDs below and above average).
plot(uniDich.result1, type = "trace", theta_lim = c(-4,4))
##  According to the item trace plot of this example, the 3 items with item size 2 (S1.2,S2.2,S3.2) are relatively easy items, 
## in which subjects with average ability (\(\theta\) = 0) are estimated to have about 80% to 90% of chance to answer correctly. 
## On the other hand, the 3 items with size 5 are relatively hard items, 
## in which subjects with average ability are estimated to have about only 10% to 20% of chance to answer correctly
## Probability of correct answer given an individuals ability (0 = mean ability)

# We can specify the exact set of items we want to plot in the ploting function of mirt. 
## Here we can also only visualize the 1st, 5th, and 9th item from the dataset by addin an argument "which.items = c(1,5,9)" in the function. 
## This will make the function to only plot the 3 items with set size 2 in the task. Please feel free to give a try.
plot(uniDich.result1, type = "infotrace")
## Item information plots visualize how much “information” about the latent trait ability an item can provide. 
## Conceptually, higher information implies less error of measure, 
## and the peak of an item information curve is at the point of its b parameter. 
## Thus, for easy items (such as the 3 items in the most left column below), 
## little information are provided on subjects with high ability levels (because they will almost always answer correctly).
## More information is better, and the range where the item is able to discriminate beween individuals 
### Sx.2 are good at low ability, but bad at higher ability. Whereas Sx.5 are good for high ability, but bad for low ability

# specific plots for item 1
itemplot(uniDich.result1, item = 1, type = "trace", CE = TRUE) 
plot(uniDich.result1, type = "infotrace", which.items = c(1))

# overall for the whole test (sum of all item info curves)
plot(uniDich.result1, type = "infoSE")
## As aforementioned, high information indicate less error (low SE) of measurement
## Seems to catch mean +-2SD relatively well


# Unidimensional Dichotomous IRT Models: 2PL ----
## In a 2PL model, not only the item difficulty parameters (bs) but also the item discrimination parameters (as) are estimated. 
## Thus, a 2PL model assumes that different items vary in the ability to discriminate between persons with different latent trait levels.
### This is in addition to each item having different difficulty (hard item, only high ability likely to get correct) 
### and giving different information (traits are good at discrimination at different trait levels, e .g. easy question better at low ability)
### Here some items can be better than other items at discrimination across a larger span of trait levels

uniDich.model2 <- mirt.model("rotation = 1 - 12")

uniDich.result2 <- mirt::mirt(dat1, uniDich.model2, itemtype = "2PL", SE = TRUE)

M2(uniDich.result2) # overall fit
itemfit(uniDich.result2) # item S1.3 may need to be looked at further (sign p)

coef(uniDich.result2,simplify = TRUE, IRTpar = TRUE)$items
# now a is also estimated
## For a dichotomous 2PL model, 
## the item discrimination parameters (a) reflect how well an item could discriminate between persons with low and high ability/trait levels. 
## Furthermore, the a parameter also reflects the magnitude to which an item is related to the latent trait measured by the scale. 
## Thus, a low discrimination parameter usually indicates potential issues for an item comparing to the general scale

plot(uniDich.result2, type = "trace", theta_lim = c(-4,4))
## differences in as are reflected by the changes in the steepness of the item trace curves. 
## Higher a's would be reflected as steeper item trace curves
## Here we see that both "intercept" (theta where it starts to increase) and slope/steepness are included, not only intercept as in 1PL

plot(uniDich.result2, type = "infotrace")
# a also reflected here, with peaks varying in high compared to the 1PL

plot(uniDich.result2, type = "infoSE")
# More information than previously with 1PL (given by higher I values), but also steeper peak 

# Adding constraints
## can make items with the same item size (e.g. 2 Qs per item, or 5Q per item) be estimated together
## Here we tell it to fit the same a paramter to each item within each of the 4 item group sizes
uniDich.model3 <- mirt.model("rotation = 1 - 12
                             CONSTRAIN = (1,5,9,a1), (2,6,10,a1),(3,7,11,a1),(4,8,12,a1)")

uniDich.result3 <- mirt::mirt(dat1, uniDich.model3, itemtype = "2PL", SE = TRUE)
coef(uniDich.result3,simplify = TRUE, IRTpar = TRUE)$items

# comparing models: without constraint vs with constraint
anova(uniDich.result3,uniDich.result2) # no sign p value


# Unidimensional Polytomous IRT Model ---
## use all info, know how many responses were correct for each item

dat2 <- as.matrix(wmirot[,-1])
head(dat2)

# Generalized Partial Credit Model
## As a polytomous model, GPCM estimates one item threshold parameter for each response category in an item instead 
## of one difficulty parameter for an item. Further more, 
## GPCM assumes an unique item discrimination parameter for each item instead of assuming a unitary reliability across items 
## (like the Rasch model)

unipoly.model1 <- mirt.model("rotation = 1 - 12")

unipoly.result1 <- mirt::mirt(dat2, uniDich.model2, itemtype = "gpcm", SE = TRUE)

M2(unipoly.result1) # good fit

itemfit(unipoly.result1) # good fit

coef(unipoly.result1,simplify = TRUE, IRTpars = TRUE)$items
## a as in the 2PL model (i.e. different slopes)
## several Bs, each corresponding to the response categories. Thresholds between parameters
### When choosing between the kth and the k-1th category, subjects with trait levels higher than that threshold 
## are more likely to approach the kth, while subjects with trait levels lower than that threshold are more likely to approach the k-1th
### S1.2 suggest that Q is very easy as all people higher than -4.5 SD seem to get 2 correct
### people with -2.4 SD get only 1 correct... not sure if it makes sense for b1 > b2...
### maybe conditional, if you are above -2.4 you get a least 1 correct, and are likely to get 2 correct if above -4.5?
### If so, S2.5 suggest that getting 4 correct makes you very likely to also get 5 correct, suggesting that the last Q is not very informative

plot(unipoly.result1, type = "trace", theta_lim = c(-4,4))
## Seems more reasonable here:
### P1 = 0 correct, P2 = 1 correct etc
### The bs correspond to where curves cross (i.e. where they are equally likely)
### at any given ability, can get the probability for each nubmer of correct answers
### S1.2: -4 SD: 0.55 for 0 correct, 0.2 for 1 correct and 0.25 for 2 correct
### S1.2: -2 SD: 0.1 for 0 correct, 0.15 for 1 correct and 0.75 for 2 correct

plot(unipoly.result1, type = "infotrace")
# information curves similar to 2PL with different hights

plot(unipoly.result1, type = "infoSE")
## even more info (given by higher I score), but seem better at estimating people below average than above average

# individual scoring
est.theta <- as.data.frame(fscores(unipoly.result1))
head(est.theta)
# estimate the individual latent trait scores

hist(est.theta$rotation)
est.theta %>% 
  ggplot(aes(x=rotation)) +
  geom_histogram(aes(y=..density..),
                 binwidth=.1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="aquamarine2")









