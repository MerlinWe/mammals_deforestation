## Follow-up from the OccModel JAGS script 

require(ggeffects)
require(effects)
require(DHARMa)
require(Hmisc)

setwd("/Users/serpent/OneDrive - Van Hall Larenstein/Thesis/Scripts/CommIntegrity")

dat <- read.csv("habitat_affinity.csv") # read habitat affinity data
camtraps  <- read.csv("camtraps_covariates.csv", header = TRUE)[,-10]  # read camtrap data
colnames(camtraps) <- c("Station", "location", "period", "forcov", "foredg", "lat", "long", "Setup_date", "Retrieval_date")

prop <- aggregate(dat$habitat_affinity, by = list(Station = dat$Station), FUN = function(x) {
prop.table(table(x))}) # calculate proportions of habitat affinities
prop <- unnest_wider(prop, x)
colnames(prop) <- c("Station", "FS_prop", "DA_prop")
prop$DA_prop[is.na(prop$DA_prop)] <- 0
prop$FS_prop[is.na(prop$FS_prop)] <- 0
prop <- na.omit(Merge(prop, camtraps[ ,c(1,4,5)], id = ~ Station))

#prop <- read.csv("/Users/serpent/OneDrive - Van Hall Larenstein/Thesis/Scripts/CommIntegrity/comm_int.csv")

# determine order of polynomial regression

fit_cov <- lm(FS_prop ~ poly(forcov, 3), data = prop)
fit_edg <- lm(FS_prop ~ poly(foredg, 3), data = prop)
AIC(fit_cov) # third 
AIC(fit_edg) # third 

fit_cov <- lm(FS_prop ~ poly(forcov, 2), data = prop)
fit_edg <- lm(FS_prop ~ poly(foredg, 2), data = prop)
AIC(fit_cov) # third 
AIC(fit_edg) # third 

# second order fits best 
summary(fit_cov)

# The model you fitted is:
# FS_prop = 0.65340 + 0.65482 * poly(forcov, 2)1 - 0.22264 * poly(forcov, 2)2
# where poly(forcov, 2) represents a second-order polynomial transformation of the forcov variable.
# The coefficients table shows the estimated values of the intercept and the coefficients of the polynomial terms. The intercept of 0.65340 represents the expected value of FS_prop when forcov is zero. The coefficient of poly(forcov, 2)1 is 0.65482, which represents the slope of the curve at the inflection point of the quadratic function. The coefficient of poly(forcov, 2)2 is -0.22264, which represents the curvature of the curve
# The p-value for the poly(forcov, 2)1 coefficient is 0.0164, which means that it is statistically significant at the 0.05 level. This suggests that there is evidence of a significant linear relationship between forcov and FS_prop. However, the p-value for the poly(forcov, 2)2 coefficient is 0.4030, which means that it is not statistically significant at the 0.05 level. This suggests that there is not enough evidence to conclude that there is a significant quadratic relationship between forcov and FS_prop.
# The residual standard error (RSE) of the model is 0.2641, which represents the average distance between the observed values of FS_prop and the predicted values of FS_prop. The multiple R-squared is 0.1146, which means that the model explains only 11.46% of the total variance in FS_prop. The adjusted R-squared is 0.08119, which adjusts the R-squared value for the number of predictor variables in the model. The F-statistic tests the overall significance of the model and has a p-value of 0.03974, which suggests that the model is statistically significant.
# Overall, the model suggests that there is a linear relationship between forcov and FS_prop, but there is not enough evidence to suggest a significant quadratic relationship. The model has a relatively low R-squared value, indicating that the model may not be a very good fit to the data.

ggplot(prop, aes(x = forcov)) +
  geom_point(aes(y=FS_prop), color = "forestgreen", alpha = .3, shape = 20) +
  geom_point(aes(y=DA_prop), color = "black", alpha = .3, shape = 20) +
  geom_smooth(aes(y=FS_prop), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "forestgreen") +
  geom_smooth(aes(y=DA_prop), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black") +
  xlab("Percentage of forest cover") +
  ylab("Community Integrity") +
  theme_linedraw() +
  theme(axis.text.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.text.x = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.x = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.title = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.caption = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.subtitle = element_text(face = 1, size = 6, family="Times New Roman"),
        legend.text = element_text(face = 1, size = 6, family="Times New Roman"),
        legend.title = element_text(face = 1, size = 6, family="Times New Roman"),
        legend.position = "none")

ggplot(prop, aes(x = foredg)) +
  geom_point(aes(y=FS_prop), color = "forestgreen", alpha = .3, shape = 20) +
  geom_point(aes(y=DA_prop), color = "black", alpha = .3, shape = 20) +
  geom_smooth(aes(y=FS_prop), method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "forestgreen") +
  geom_smooth(aes(y=DA_prop), method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "black") +
  xlab("Forest fragmentation") +
  ylab("Community Integrity") +
  theme_linedraw() +
  theme(axis.text.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.text.x = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.x = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.title = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.caption = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.subtitle = element_text(face = 1, size = 6, family="Times New Roman"),
        legend.text = element_text(face = 1, size = 6, family="Times New Roman"),
        legend.title = element_text(face = 1, size = 6, family="Times New Roman"),
        legend.position = "none")

## ------------------------------------------------------------------------------------------------

dat <- prop

#### Does a lm make sense? 

## --- 1. Outliers ---

plot(dat$DA_prop)
plot(dat$FS_prop)
plot(dat$forcov)
plot(dat$foredg)

## --- 2. Normality ---

hist(dat$DA_prop)
hist(dat$FS_prop)
hist(dat$forcov)
hist(dat$foredg)

## => See if transformation improves data structure 

hist(log(dat$forcov+.1))
hist(sqrt(dat$forcov+.1))

hist(log(dat$foredg+.1))
hist(sqrt(dat$foredg+.1))

dat$forcov <- sqrt(dat$forcov+.1)
dat$foredg <- log(dat$foredg+.1)

## --- 3. Collinearity ---

cor(dat$forcov, dat$foredg)

## --- 4. X~Y Relationships ---

# excellent
ggplot(dat, aes(x= forcov, y = FS_prop))+
  geom_point(size = .2)+
  geom_smooth(method = lm, formula = "y~x",
              color = "red", fill = "69b3a2", se = TRUE)+
  theme_bw()

# not so much 
ggplot(dat, aes(x= foredg, y = DA_prop))+
  geom_point(size = .2)+
  geom_smooth(method = lm, formula = "y~x",
              color = "red", fill = "69b3a2", se = TRUE)+
  theme_bw()

######################## Modelling ######################## 

fs_mod1 <- glm( FS_prop ~ forcov + foredg, family = "gaussian", data = dat)

summary(fs_mod1) # examine model
drop1(fs_mod1)   # outcrop can be dropped

fs_mod2 <- glm(FS_prop ~ forcov, family = "gaussian", data = dat)

summary(fs_mod2) 
FS <- Effect(c("forcov"), fs_mod2)

## Validate model 
res_p = simulateResiduals(fs_mod2)
plot(res_p, rank = T)
testZeroInflation(res_p)
testDispersion(res_p)
plotResiduals(res_p, form = dat$forcov)



da_mod1 <- glm(DA_prop ~ forcov + foredg, family = "gaussian", data = dat)

summary(da_mod1) # examine model
drop1(da_mod1)   # outcrop can be dropped

da_mod2 <- glm(DA_prop ~ forcov, family = "gaussian", data = dat)

summary(da_mod2) 
DA <- Effect(c("forcov"), da_mod2)

## Validate model 
res_p = simulateResiduals(da_mod2)
plot(res_p, rank = T)
testZeroInflation(res_p)
testDispersion(res_p)
plotResiduals(res_p, form = dat$forcov)


DA <- DA$data
FS <- FS$data

dat <- data.frame(cbind(DA, FS))
dat <- dat[, 1:3]
new <- reshape2::melt(dat, id.vars = c("forcov"))

plot <- ggplot(new)+
  geom_point(aes(x=forcov, y=value, group = variable, fill = variable, color = variable),
             alpha = .4, size = .5)+
  geom_smooth(aes(x=forcov, y=value, group = variable, fill = variable, color = variable),
              se = TRUE, method = "lm", alpha = .2) +
  scale_color_manual(values = c("black", "forestgreen"))+
  scale_fill_manual(values = c("black", "forestgreen"))+
  theme_linedraw() +
  theme(axis.text.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.text.x = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.x = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.title = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.caption = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.subtitle = element_text(face = 1, size = 6, family="Times New Roman"),
        legend.text = element_text(face = 1, size = 6, family="Times New Roman"),
        legend.title = element_text(face = 1, size = 6, family="Times New Roman"),
        legend.position = "none")+
  xlab("Forest cover %")+
  ylab("Proportion in community")


ggsave(filename = "habitat_affinity_plot.png", plot = plot, dpi = 1000,
       height = 100, width = 120, units = "mm", bg = "white")




