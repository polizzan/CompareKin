####################
# INSTALL PACKAGES #
####################

## list packages to be installed from cran
from.cran <- c("arules", "DemoKin", "ggnewscale", "grid", "gridExtra", "ggpubr", "here", 
               "HMDHFDplus", "plyr", "RColorBrewer", "tidyverse", "tictoc")

## check if installed, else install
for(i in from.cran){
  
  if(system.file(package=i)==""){install.packages(i)}

}

#################################
# LOAD FREQUENTLY USED PACKAGES #
#################################

library(tidyverse)

#######################
# SPECIFY USER INPUTS #
#######################

## country codes in hfd and hmd
country1.code <- "DEUTE"
country2.code <- "DEUTW"

## country labels for plots
country1.label <- "East Germany"
country2.label <- "West Germany"

## single year for which vertical line will be drawn, '0' if no line should be drawn
discontinuity.line <- 1989

## open-ended age category, probability of survival will be set to 0 (110 or below)
open.ended.mort <- 110 

## ages for which age-specific fertility rates should be assumed to be non-zero (12-55)
fert.ages <- 15:49 

## sex ratio at birth, 1 female per x births
SRB <- 1/2.04

## male-female gap in mean age at fertility (country 1)
country1.fert.gap <- 4 

## male-female gap in mean age at fertility (country 2)
country2.fert.gap <- 3 

## 4 ages for which number of kin will be plotted across time
kin.by.age <- c(5, 20, 45, 60) 

## number of positive and negative categories for kin-ratio plot (up to 9)
kin.ratio.cat <- 5 

## color palettes
palette.tfr.e0 <- RColorBrewer::brewer.pal(n=12, "Paired")[c(8, 10, 7, 9)] ## order is female country 1, female country 2, male country 1, male country 2 
palette.mlg <- RColorBrewer::brewer.pal(n=12, "Paired")[c(8, 10, 4)] ## order is country 1, country 2, difference between country 1 and country 2

palette.kin.by.age <- RColorBrewer::brewer.pal(n=12, "Paired")[c(1:6, 9:10)] ## order is age 1 country 1, age 1 country 2, age 2 country 1, age 2 country 2, age 3 country 1, age 3 country 2, age 4 country 1, age 4 country 2
palette.kin.last.year <- RColorBrewer::brewer.pal(n=12, "Paired")[c(8, 10)] ## order is country 1, country 2
palette.kin.ratio <- c(rev(RColorBrewer::brewer.pal(n=kin.ratio.cat, "Blues")), "white", RColorBrewer::brewer.pal(n=kin.ratio.cat, "Reds")) ## order is negative, zero, positive

########################
# CREATE OUTPUT FOLDER #
########################

## find root directory
here::i_am("scripts/00_main.R")

## determine name of output folder
outpath <- paste("out", country1.label, country2.label, format(Sys.time(), "%y-%m-%d-%H%M%S"), sep="-")

## check if output folder already exists, else generate
ifelse(dir.exists(here::here(outpath)), 
       "Output folder already exists!", 
       dir.create(here::here(outpath)))

## output folder includes time stamp to allow for multiple outputs for the same country comparison to exist alongside each other (e.g., for robustness checks)
## time stamp is: year-month-day-hourminutesecond; this order ensures that output can be ordered conveniently by date

###############
# START TIMER #
###############

tictoc::tic()

#######################
# CREATE SOME OBJECTS #
#######################

## pull asfrs from hfd
## country 1, female
c1.fx.f <-
  HMDHFDplus::readHFD(unz(description=here::here("data", "hfd_statistics_d20240309.zip"),
                          filename="asfrRR.txt")) %>%
  filter(Code==country1.code) %>% 
  select(Year, Age, ASFR) 

## country 2, female
c2.fx.f <-
  HMDHFDplus::readHFD(unz(description=here::here("data", "hfd_statistics_d20240309.zip"), 
                          filename="asfrRR.txt")) %>%
  filter(Code==country2.code) %>% 
  select(Year, Age, ASFR) 

## pull qx from hmd (will be converted into px below)
## country 1, male
c1.px.m <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("lt_male/mltper_1x1/", country1.code, ".mltper_1x1.txt"))) %>%
  select(Year, Age, qx) 

## country 2, male
c2.px.m <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("lt_male/mltper_1x1/", country2.code, ".mltper_1x1.txt"))) %>%
  select(Year, Age, qx) 

## country 1, female
c1.px.f <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("lt_female/fltper_1x1/", country1.code, ".fltper_1x1.txt"))) %>%
  select(Year, Age, qx)

## country 2, female
c2.px.f <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("lt_female/fltper_1x1/", country2.code, ".fltper_1x1.txt"))) %>%
  select(Year, Age, qx)

## determine overlapping time period for which hfd/hmd information exists for both countries
data.years <- Reduce(intersect, 
                     list(unique(c1.fx.f$Year),
                          unique(c2.fx.f$Year),
                          unique(c1.px.m$Year),
                          unique(c2.px.m$Year),
                          unique(c1.px.f$Year),
                          unique(c2.px.m$Year)))

## determine time period for plotting
plot.years <- plyr::round_any(min(data.years), f=floor, accuracy=10):plyr::round_any(max(data.years), f=ceiling, accuracy=10) 

## ages for which survival information will be considered, px in last age = 0
mort.ages <- 0:open.ended.mort 

## ages for which asfrs are assumed to be zero
zero.fert <- c(0:(min(fert.ages)-1), (max(fert.ages)+1):open.ended.mort) 

## create data set with zero fertility
zero.fert.df <-
  data.frame(Year=rep(data.years, each=length(zero.fert)),
             Age=rep(zero.fert, length(data.years)),
             ASFR=rep(0, length(data.years) * length(zero.fert)))

## create mortality matrices for DemoKin functions
for(df in c("c1.px.f", "c2.px.f", "c1.px.m", "c2.px.m")){
  
  aux <-
    get(df) %>% 
    filter(Year %in% data.years,
           Age %in% mort.ages) %>% 
    mutate(px=case_when(Age==open.ended.mort ~ 0, ## set px in open-ended age category to 0
                        TRUE ~ 1-qx)) %>% 
    select(-qx) %>% 
    pivot_wider(names_from="Year", 
                values_from="px") %>% ## reshape to wide format
    select(-Age) %>% ## remove age column
    as.matrix() ## turn into matrix for DemoKin package
  
  rownames(aux) <- mort.ages ## assign row names
  assign(df, aux) ## rename object 'aux'
  remove(aux)
  
}

## create fertility matrices for DemoKin functions
## female fertility
for(df in c("c1.fx.f", "c2.fx.f")){ 
  
  aux <-
    get(df) %>% 
    filter(Year %in% data.years,
           Age %in% fert.ages) %>%
    add_row(zero.fert.df) %>% ## append zero-fertility data set
    arrange(Year, Age) %>% 
    pivot_wider(names_from="Year",
                values_from="ASFR") %>% ## reshape to wide format
    select(-Age) %>% ## remove age column
    as.matrix() ## turn into matrix for DemoKin package
  
  assign(df, aux) ## rename object 'aux'
  remove(aux)
  
}

## male fertility
## generate matrix of zeros corresponding in size to female fertility matrices
c1.fx.m <- ## country 1
  c2.fx.m <- ## country 2
  matrix(0, dim(c1.fx.f)[1], dim(c1.fx.f)[2]) 

## assign row names to male and female matrices
rownames(c1.fx.f) <- rownames(c1.fx.m) <- rownames(c2.fx.f) <- rownames(c2.fx.m) <- mort.ages 

## male fertility equal to female fertility shifted by age gap
c1.fx.m[paste(fert.ages+country1.fert.gap),] <- c1.fx.f[paste(fert.ages),]
c2.fx.m[paste(fert.ages+country2.fert.gap),] <- c2.fx.f[paste(fert.ages),] 

## assign column names to male fertility matrices
colnames(c1.fx.m) <- colnames(c2.fx.m) <- data.years 

## pull exposures from hmd
## country 1, female
c1.nx.f <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("exposures/Exposures_1x1/", country1.code, ".Exposures_1x1.txt"))) %>% 
  select(Year, Age, Exposure=Female)
  
## country 1, male
c1.nx.m <-  
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("exposures/Exposures_1x1/", country1.code, ".Exposures_1x1.txt"))) %>% 
  select(Year, Age, Exposure=Male)

## country 2, female
c2.nx.f <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("exposures/Exposures_1x1/", country2.code, ".Exposures_1x1.txt"))) %>% 
  select(Year, Age, Exposure=Female)

## country 2, male  
c2.nx.m <-  
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("exposures/Exposures_1x1/", country2.code, ".Exposures_1x1.txt"))) %>% 
  select(Year, Age, Exposure=Male)
  
## create exposure matrices for DemoKin functions
for(df in c("c1.nx.f", "c1.nx.m", "c2.nx.f", "c2.nx.m")) { 
  
  aux <-
    get(df) %>% 
    filter(Year %in% data.years,
           Age %in% mort.ages) %>%
    pivot_wider(names_from="Year",
                values_from="Exposure") %>% ## reshape to wide format
    select(-Age) %>% ## remove age column
    as.matrix() ## turn into matrix for DemoKin package
  
  rownames(aux) <- mort.ages ## assign row names
  assign(df, aux) ## rename object 'aux'
  remove(aux)
  
}

## create vector of kin considered
kin.list <- c("Grandfather", "Grandmother", 
              "Uncle", "Father", "Mother", "Aunt",
              "Male Cousin", "Older Brother", "Younger Brother", "Younger Sister", "Older Sister", "Female Cousin",
              "Nephew", "Son", "Daughter", "Niece",
              "Grandson", "Granddaughter")

## create vector of 'placeholder' kin for plotting
fake.kin <-
  c("g1m1", "g1m2", "g1f2", "g1f1", ## grandparent generation
    "g2m1", "g2f1", ## parent generation
    "g4m1", "g4f1", ## child generation
    "g5m1", "g5m2", "g5f2", "g5f1") ## grandchild generation

## create vector to determine order of regular and 'placeholder' kin in plots
kin.order <- 
  c("g1m1", "g1m2", "Grandfather", "Grandmother", "g1f2", "g1f1",
    "g2m1", "Uncle", "Father", "Mother","Aunt", "g2f1",
    "Male Cousin", "Older Brother", "Younger Brother", "Younger Sister", "Older Sister", "Female Cousin",
    "g4m1", "Nephew", "Son", "Daughter","Niece", "g4f1",
    "g5m1", "g5m2", "Grandson", "Granddaughter", "g5f2", "g5f1")

## create data set for 'placeholder' kin
fake.kin.df <-
  data.frame(
    year=rep(rep(data.years, each=length(mort.ages)), length(fake.kin)),
    age_focal=rep(mort.ages, length(fake.kin) * length(data.years)),
    kin=rep(fake.kin, each=(length(mort.ages) * length(data.years)))
  ) 

###################################################
# I: RUN KINSHIP MODELS FOR COUNTRY 1 & COUNTRY 2 #
###################################################

# run time-variant, two-sex kinship models for both female and male focal
c1.twosex.m <-
  DemoKin::kin2sex(pf=c1.px.f,
                   pm=c1.px.m,
                   ff=c1.fx.f,
                   fm=c1.fx.m,
                   nf=c1.nx.f,
                   nm=c1.nx.m,
                   time_invariant=FALSE,
                   sex_focal="m",
                   output_cohort=data.years) ## keep only individuals born after first time period

c2.twosex.m <-
  DemoKin::kin2sex(pf=c2.px.f,
                   pm=c2.px.m,
                   ff=c2.fx.f,
                   fm=c2.fx.m,
                   nf=c2.nx.f,
                   nm=c2.nx.m,
                   time_invariant=FALSE,
                   sex_focal="m",
                   output_cohort=data.years)

c1.twosex.f <-
    DemoKin::kin2sex(pf=c1.px.f,
                     pm=c1.px.m,
                     ff=c1.fx.f,
                     fm=c1.fx.m,
                     nf=c1.nx.f,
                     nm=c1.nx.m,
                     time_invariant=FALSE,
                     sex_focal="f",
                     output_cohort=data.years) 
  
c2.twosex.f <-
  DemoKin::kin2sex(pf=c2.px.f,
                   pm=c2.px.m,
                   ff=c2.fx.f,
                   fm=c2.fx.m,
                   nf=c2.nx.f,
                   nm=c2.nx.m,
                   time_invariant=FALSE,
                   sex_focal="f",
                   output_cohort=data.years)  

## save output from matrix kinship models
save(c1.twosex.f, c2.twosex.f, 
     file=here::here(outpath, "kin-f.RData"))

save(c1.twosex.m, c2.twosex.m, 
     file=here::here(outpath, "kin-m.RData"))

## replace some kin counts in male model with kin counts from female model 
c1.twosex.m$kin_full[c1.twosex.m$kin_full$kin %in% c("coa", "nos", "oa", "os"),] <-
  c1.twosex.f$kin_full[c1.twosex.f$kin_full$kin %in% c("coa", "nos", "oa", "os"),]

c1.twosex.m$kin_summary[c1.twosex.m$kin_summary$kin %in% c("coa", "nos", "oa", "os"),] <-
  c1.twosex.f$kin_summary[c1.twosex.f$kin_summary$kin %in% c("coa", "nos", "oa", "os"),]

c2.twosex.m$kin_full[c2.twosex.m$kin_full$kin %in% c("coa", "nos", "oa", "os"),] <-
  c2.twosex.f$kin_full[c2.twosex.f$kin_full$kin %in% c("coa", "nos", "oa", "os"),]

c2.twosex.m$kin_summary[c2.twosex.m$kin_summary$kin %in% c("coa", "nos", "oa", "os"),] <-
  c2.twosex.f$kin_summary[c2.twosex.f$kin_summary$kin %in% c("coa", "nos", "oa", "os"),]

#####################################
# II: PLOT OUTPUT OF KINSHIP MODELS #
#####################################

source(here::here("scripts", "01_out-kin.R"), local=TRUE)

##########################
# III: DESCRIPTIVE PLOTS #
##########################
#########################################################
# IIIa: PLOT TRENDS IN TFR AND LIFE EXPECTANCY AT BIRTH #
#########################################################

source(here::here("scripts", "02_des-a.R"), local=TRUE)

############################################
# IIIb: PLOT TRENDS IN GENERATIONAL LENGTH #
############################################

source(here::here("scripts", "02_des-b.R"), local=TRUE)

##############
# STOP TIMER #
##############

timer <- tictoc::toc()

#################################
# WRITE MODEL DETAILS INTO FILE #
#################################

# open file
out <- file(paste0(here::here(outpath), "/input.txt"), "w")

cat("\n", file=out) ## empty line
cat("Input Parameters Used for Comparison", "\n", file=out)
cat("\n", file=out) ## empty line
cat("Country 1, Code:", paste(country1.code), "\n", file=out)
cat("Country 1, Label:", paste(country1.label), "\n", file=out)
cat("\n", file=out) ## empty line
cat("Country 2, Code:", paste(country2.code), "\n", file=out)
cat("Country 2, Label:", paste(country2.label), "\n", file=out)
cat("\n", file=out) ## empty line
cat("Period:", paste(min(data.years), max(data.years), sep="-"), "\n", file=out)
cat("\n", file=out) ## empty line
cat("Reproductive Ages:", paste(min(fert.ages), max(fert.ages), sep="-"), "\n", file=out)
cat("Country 1, Male-Female Fertility Gap:", paste(country1.fert.gap), "Years", "\n", file=out)
cat("Country 2, Male-Female Fertility Gap:", paste(country2.fert.gap), "Years", "\n", file=out)
cat("\n", file=out) ## empty line
cat("Final Age Category:", paste(open.ended.mort), "\n", file=out)
cat("\n", file=out) ## empty line
cat("Discontinuity Drawn in Year:", paste(discontinuity.line), "\n", file=out)
cat("Kin Counts Shown for Ages:", paste(kin.by.age[1], kin.by.age[2], kin.by.age[3], kin.by.age[4], sep=", "), "\n", file=out)
cat("Number of Positive/Negative Categories for Kin Ratios:", paste(kin.ratio.cat), "\n", file=out)
cat("\n", file=out) ## empty line
cat("Total Time (in Minutes):", paste(round((timer$toc-timer$tic)/60, 2)), "\n", file=out)
cat("\n", file=out) ## empty line

## close file
close(out)
