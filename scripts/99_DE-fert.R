#####################################################################
# MALE / FEMALE AGE-SPECIFIC FERTILITY RATES IN EAST / WEST GERMANY #
#####################################################################

## install packages
for(i in c("here", "tidyverse")){
  
  if(system.file(package=i)==""){install.packages(i)}
  
}

## load packages
library(tidyverse)

## create output folder
if(!dir.exists(here::here("out"))){dir.create(here::here("out"))}

## load data from human fertility collection
## east german females
east.f <-
  read_csv(unz(description=here::here("data", "hfc_statistics_d20240311.zip"),
               filename="DEUTE_ASFRstand_TOT.txt")) %>% 
  filter(Collection=="HFD", AgeDef=="ACY") %>% 
  select(Country, Year=Year1, Age, ASFR) %>% 
  mutate(across(c(Year, Age, ASFR), ~ as.numeric(.)),
         Sex="Female",
         Country="East Germany") %>% 
  mutate(ASFR=ifelse(is.na(ASFR), 0, ASFR))

## check that there are no duplicate entries
nrow(east.f)==nrow(distinct(east.f, Year, Age))

## west german females
west.f <-
  read_csv(unz(description=here::here("data", "hfc_statistics_d20240311.zip"), 
               filename="DEUTW_ASFRstand_TOT.txt")) %>% 
  filter(Collection=="HFD", AgeDef=="ACY") %>% 
  select(Country, Year=Year1, Age, ASFR) %>% 
  mutate(across(c(Year, Age, ASFR), ~ as.numeric(.)),
         Sex="Female",
         Country="West Germany") %>% 
  mutate(ASFR=ifelse(is.na(ASFR), 0, ASFR))

nrow(west.f)==nrow(distinct(west.f, Year, Age))

## east german males
east.m <-
  read_csv(unz(description=here::here("data", "hfc_statistics_d20240311.zip"), 
               filename="m_DEUTE_ASFRstand_TOT.txt")) %>% 
  filter(Collection=="RE", AgeDef=="ACY") %>% 
  select(Country, Year=Year1, Age, ASFR) %>% 
  mutate(across(c(Year, Age, ASFR), ~ as.numeric(.)),
         Sex="Male",
         Country="East Germany") %>% 
  mutate(ASFR=ifelse(is.na(ASFR), 0, ASFR))

nrow(east.m)==nrow(distinct(east.m, Year, Age))

## west german males
west.m <-
  read_csv(unz(description=here::here("data", "hfc_statistics_d20240311.zip"), 
               filename="m_DEUTW_ASFRstand_TOT.txt")) %>% 
  filter(Collection=="RE", AgeDef=="ACY") %>% 
  select(Country, Year=Year1, Age, ASFR) %>% 
  mutate(across(c(Year, Age, ASFR), ~ as.numeric(.)),
         Sex="Male",
         Country="West Germany") %>% 
  mutate(ASFR=ifelse(is.na(ASFR), 0, ASFR))

nrow(west.m)==nrow(distinct(west.m, Year, Age))

## calculate mean difference in mean age at birth, east germany
east.diff <-
  east.f %>% 
  add_row(east.m) %>% 
  filter(Year %in% unique(east.m$Year)) %>% 
  ## calculate mean age at birth for each year and sex
  summarize(mab=sum((Age+0.5)*ASFR)/sum(ASFR), .by=c(Year, Sex)) %>% 
  pivot_wider(names_from=Sex,
              values_from=mab) %>% 
  ## calculate mean difference in mean age at birth
  summarize(diff=mean(Male-Female)) %>% 
  pull(diff) %>% 
  round()

## calculate mean difference in mean age at birth, west germany
west.diff <-
  west.f %>% 
  add_row(west.m) %>% 
  filter(Year %in% unique(west.m$Year)) %>% 
  summarize(mab=sum((Age+0.5)*ASFR)/sum(ASFR), .by=c(Year, Sex)) %>% 
  pivot_wider(names_from=Sex,
              values_from=mab) %>% 
  summarize(diff=mean(Male-Female)) %>% 
  pull(diff) %>% 
  round()

## plot age-specific fertility rates
fert.plot <-
  east.m %>%
  add_row(west.m) %>%
  add_row(east.f) %>% 
  add_row(west.f) %>% 
  ## shift female fertility schedule by mean difference in mean age at birth
  add_row(east.f %>% mutate(Age=Age + east.diff, Sex="Shifted Female")) %>%
  add_row(west.f %>% mutate(Age=Age + west.diff, Sex="Shifted Female")) %>%  
  ## plot only selected years
  filter(Year %in% seq(1991, 2011, 5)) %>% 
  ggplot(aes(x=Age, y=ASFR, color=Sex)) +
  ## draw lines
  geom_line(linewidth=0.25) +
  ## choose axis titles
  labs(x="Age", y="ASFR") +
  ## assign colors to lines
  scale_color_manual("", values=c("#C6DBEF", "#4292C6", "#08306B")) +
  ## choose axis values
  scale_x_continuous(breaks=seq(10, 60, 10),
                     labels=seq(10, 60, 10)) +
  theme_bw() +
  theme(aspect.ratio=1,
        panel.grid=element_blank(),
        panel.border=element_rect(linewidth=0.25),
        axis.ticks=element_line(linewidth=0.25),
        axis.ticks.length=unit(1, "pt"),
        strip.background=element_rect(fill="white",
                                      linewidth=0.25),        
        strip.text=element_text(size=7),
        axis.title=element_text(size=7, face="bold"),
        axis.text=element_text(size=6),
        legend.position="bottom",
        legend.key.width=unit(0.5, "cm"),
        legend.text=element_text(size=6),
        text=element_text(family="serif")) +
  coord_cartesian(xlim=c(10, 60)) +
  ## create panels
  facet_grid(Country ~ Year)

## save plot
for(i in c("pdf", "svg")){

ggsave(here::here("out", paste0("fert.", i)), 
       height=80, width=180, units="mm", 
       plot=fert.plot, device=i)
  
}