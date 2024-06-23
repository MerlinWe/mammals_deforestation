########## Multi species occupancy model #########

# see: https://cran.r-project.org/web/packages/camtrapR/vignettes/camtrapr5.html

library(tidyverse) # data manipulation
library(camtrapR)  # camtrapR 
library(purrr)     # data preparation
library(DT)        # data manipulation
library(Hmisc)     # data wrangling
library(knitr)     # reports 
library(ggpubr)   # plot grid 

setwd("/Users/serpent/OneDrive - Van Hall Larenstein/Thesis/Scripts/OccModel") # set WD to OccModel file 

## About the data: We have 11 stations and 10 environmental measurements for each stations. 

recordTable <- read.csv("mammals_final.csv", header = TRUE) # read capture data 
recordTable <- subset(recordTable, Classification != "Bos taurus") # exclude cattle 
recordTable <- subset(recordTable, !(Classification %in% c("Didelphis albiventris", "Coendou prehensilis", "Lontra longicaudis"))) # exclude species with >5 captures
species <- recordTable %>% group_by(Classification, Trivial) %>% summarise(n_captures = n()) # prepare species tab for descriptives

camtraps    <- read.csv("camtraps_covariates.csv", header = TRUE)[,-10]  # read camtrap data
colnames(camtraps) <- c("Station", "location", "period", "forcov", "foredg", "lat", "long", "Setup_date", "Retrieval_date")

# plot(camtraps$forcov)
# plot(camtraps$foredg)
# ggplot(camtraps, aes(x=forcov, y=foredg))+geom_smooth()
# cor(camtraps$forcov, camtraps$foredg)

## Build camera operation matrix to account for sampling effort 
camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    dateFormat   = "%Y-%m-%d",
                                    hasProblems  = FALSE)

## Get a list of species specific detection histories with pooled captures per 14 days to improve homogeneity 
DetHist_list <- lapply(unique(recordTable$Classification), FUN = function(x) {
  detectionHistory(
    recordTable          = recordTable,
    camOp                = camop_no_problem,
    stationCol           = "Station",
    speciesCol           = "Classification",
    recordDateTimeCol    = "DateTimeOriginal",
    species              = x, # this gets modified by lapply
    occasionLength       = 7,
    day1                 = "station",
    datesAsOccasionNames = FALSE,
    includeEffort        = TRUE,
    scaleEffort          = TRUE, 
    timeZone             = "America/La_Paz"
  )}
)

## Assign species names to the list of detection histories 
names(DetHist_list) <- unique(gsub("_", " ", recordTable$Trivial))

## Build a list that contains species specific detection histories as well as 
## the covariates to be used as model input...

ylist <- lapply(DetHist_list, FUN = function(x) x$detection_history) # prepare list
sitecovs <- camtraps[, c(2, 4:5)] # get covariates from camtraps file
sitecovs[,c(2:3)] <- scale(sitecovs[,c(2:3)]) # standardize forest cover and edge length 
sitecovs$location <- as.factor(sitecovs$location)

## List all species detection histories and covariates together as model input 
data_list <- list(ylist    = ylist,    # detection history 
                  siteCovs = sitecovs, # covariates 
                  obsCovs  = list(effort = DetHist_list[[1]]$effort))  # effort (identical for all species)

## ~~~~~~~~~~ Build model ~~~~~~~~~~

## Define model structure with cattle encounter, forest cover and forest edge as random effects and lat/long as fixed effects
mod.jags <- communityModel(data_list,
                           occuCovs = list(fixed = c("location"), ranef = c("forcov", "foredg")),
                           detCovsObservation = list(fixed = "effort"),
                           intercepts = list(det = "ranef", occu = "ranef"),
                           modelFile = "/Users/serpent/OneDrive - Van Hall Larenstein/Thesis/Scripts/OccModel/occ_model.txt")

summary(mod.jags) # check if model structure is as expected 
#load("/Users/serpent/OneDrive - Van Hall Larenstein/Thesis/Scripts/OccModel/occMod.RData")

## Fit a JAGS model using MCMC with 1000 iterations (of which the first 500 will be discarded), using 3 chains.
fit.jags <- fit(mod.jags, # input model (see above)
                n.iter = 5000, # 1000 iterations of the Markov Chain Monte Carlo that JAGS will perform
                n.burnin = 2500, # initial iterations of the MCMC algorithm that will be discarded as burn-in (first few iterations are often not representative of the long-run behavior of the chain)
                chains = 3) # Number of chains that JAGS will run to ensure that the results are not overly dependent on the initial conditions

fit_summary <- summary(fit.jags) # get model output 
diagnostics <- as.data.frame(round(fit_summary$statistics, 3))

#write.csv(diagnostics, file = "jags_summary.csv")
#load("/Users/serpent/OneDrive - Van Hall Larenstein/Thesis/Scripts/OccModel/occMod.RData")

## Visualize model output 

forcov_eff <- plot_effects(mod.jags, fit.jags, submodel = "state")$forcov # forest cover
forcov_eff <- forcov_eff + theme_bw() +
  theme(axis.text.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.text.x = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.x = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.title = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.caption = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.subtitle = element_text(face = 1, size = 6, family="Times New Roman"),
        strip.text.x = element_text(face = 1, size = 5, family="Times New Roman"))+
  labs(x="Forest cover (standardized)") +
  ggtitle(NULL, subtitle = NULL)
forcov_eff$layers[[1]]$aes_params$size <- .5 # adjust line size 

forcov_coef <- plot_coef(mod.jags, fit.jags, submodel = "state", ordered = TRUE, colorby = "significance")$forcov
forcov_coef[["data"]]$covariate <- gsub("forcov", "Forest cover", forcov_coef[["data"]]$covariate)
forcov_coef <- forcov_coef + theme_bw() +
  theme(axis.text.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.text.x = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.x = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.title = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.caption = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.subtitle = element_text(face = 1, size = 6, family="Times New Roman"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank())+
  labs(x="Effect size") +
  ggtitle(NULL)
forcov_coef$layers[[2]]$aes_params$size <- .2 # adjust point size

forcov_plot <- ggarrange(forcov_eff, forcov_coef, ncol = 2, nrow = 1, widths = c(1.7,1.2), 
                         labels = "auto", font.label = list(size = 8, family = "Times New Roman"))
ggsave(filename = "forcov_plot.png", plot = forcov_plot, dpi = 1000,
       height = 100, width = 190, units = "mm", bg = "white")


foredg_eff <- plot_effects(mod.jags, fit.jags, submodel = "state")$foredg # forest edge
foredg_eff <- foredg_eff + theme_bw() +
  theme(axis.text.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.text.x = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.x = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.title = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.caption = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.subtitle = element_text(face = 1, size = 6, family="Times New Roman"),
        strip.text.x = element_text(face = 1, size = 5, family="Times New Roman"))+
  labs(x="Forest fragmentation (standardized)") +
  ggtitle(NULL, subtitle = NULL)
foredg_eff$layers[[1]]$aes_params$size <- .5 # adjust line size 

foredg_coef <- plot_coef(mod.jags, fit.jags, submodel = "state", ordered = TRUE, colorby = "significance")$foredg
foredg_coef[["data"]]$covariate <- gsub("foredg", "Forest fragmentation", foredg_coef[["data"]]$covariate)
foredg_coef <- foredg_coef + theme_bw() +
  theme(axis.text.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.text.x = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.y = element_text(face = 1, size = 6, family="Times New Roman"),
        axis.title.x = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.title = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.caption = element_text(face = 1, size = 6, family="Times New Roman"),
        plot.subtitle = element_text(face = 1, size = 6, family="Times New Roman"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank())+
  labs(x="Effect size") +
  ggtitle(NULL)
foredg_coef$layers[[2]]$aes_params$size <- .2 # adjust point size

foredg_plot <- ggarrange(foredg_eff, foredg_coef, ncol = 2, nrow = 1, widths = c(1.7,1.2), 
                         labels = "auto", font.label = list(size = 8, family = "Times New Roman"))
ggsave(filename = "foredg_plot.png", plot = foredg_plot, dpi = 1000,
       height = 100, width = 190, units = "mm", bg = "white")







