###################################################
# PLOT TRENDS IN TFR AND LIFE EXPECTANCY AT BIRTH #
###################################################

## male life expectancy, country 1
c1.e0.m <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("lt_male/mltper_1x1/", country1.code, ".mltper_1x1.txt"))) %>%
  filter(Age==0, Year %in% data.years) %>% 
  select(Year, Value=ex) %>% 
  mutate(Sex="Male",
         Region=country1.label,
         Outcome="Life Expectancy at Birth")

## male life expectancy, country 2 
c2.e0.m <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("lt_male/mltper_1x1/", country2.code, ".mltper_1x1.txt"))) %>%
  filter(Age==0, Year %in% data.years) %>% 
  select(Year, Value=ex) %>% 
  mutate(Sex="Male",
         Region=country2.label,
         Outcome="Life Expectancy at Birth")

## female life expectancy, country 1
c1.e0.f <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("lt_female/fltper_1x1/", country1.code, ".fltper_1x1.txt"))) %>%
  filter(Age==0, Year %in% data.years) %>% 
  select(Year, Value=ex) %>% 
  mutate(Sex="Female",
         Region=country1.label,
         Outcome="Life Expectancy at Birth")

## female life expectancy, country 2
c2.e0.f <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                          filename=paste0("lt_female/fltper_1x1/", country2.code, ".fltper_1x1.txt"))) %>%
  filter(Age==0, Year %in% data.years) %>% 
  select(Year, Value=ex) %>% 
  mutate(Sex="Female",
         Region=country2.label,
         Outcome="Life Expectancy at Birth")

## female tfr, country 1
c1.tfr <-
  HMDHFDplus::readHFD(unz(description=here::here("data", "hfd_statistics_d20240309.zip"), 
                          filename="tfrRR.txt")) %>%
  filter(Code==country1.code, Year %in% data.years) %>% 
  select(Year, Value=TFR) %>% 
  mutate(Sex="Female",
         Region=country1.label,
         Outcome="Total Fertility Rate")

## female tfr, country 2
c2.tfr <-
  HMDHFDplus::readHFD(unz(description=here::here("data", "hfd_statistics_d20240309.zip"), 
                          filename="tfrRR.txt")) %>%
  filter(Code==country2.code, Year %in% data.years) %>% 
  select(Year, Value=TFR) %>% 
  mutate(Sex="Female",
         Region=country2.label,
         Outcome="Total Fertility Rate")

## create plot
plot <-
  ## append e0 and tfr data sets
  c1.tfr %>%
  add_row(c2.tfr) %>% 
  add_row(c1.e0.f) %>%
  add_row(c2.e0.f) %>%
  add_row(c1.e0.m) %>%
  add_row(c2.e0.m) %>% 
  ggplot(aes(x=Year, 
             y=Value, 
             color=factor(interaction(Sex, Region, sep=", "),
                          levels=c(paste("Female", country1.label, sep=", "), 
                                   paste("Female", country2.label, sep=", "),
                                   paste("Male", country1.label, sep=", "),
                                   paste("Male", country2.label, sep=", "))))) +
  ## add vertical dashed line for selected year
  geom_vline(xintercept=discontinuity.line, linewidth=0.25, color="grey60", linetype="longdash") + 
  ## add horizontal line at 0
  geom_hline(yintercept=0, linewidth=0.25, linetype="solid") + 
  ## draw lines for tfr and e0
  geom_line(linewidth=0.5) +
  ## choose axis titles
  labs(x="Year", y="") + 
  ## choose axis values
  scale_x_continuous(limits=c(min(plot.years), max(plot.years)),
                     breaks=seq(min(plot.years), max(plot.years), 5),
                     labels=rev(rev(c(rbind(seq(min(plot.years), max(plot.years), 10), "")))[-1])) + ## label only every other tick
  ## assign colors to lines 
  scale_color_manual("", values=c(palette.tfr.e0)) + 
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
  coord_cartesian(xlim=c(min(plot.years), max(plot.years))) + 
  ## create panels, allow y axes to range freely
  facet_wrap(~ Outcome, scale="free_y")

## adjust y axes
## determine axis limits
e0.min <- plyr::round_any(min(c1.e0.m$Value, c2.e0.m$Value, c1.e0.f$Value, c2.e0.f$Value), f=floor, accuracy=5)
e0.max <- plyr::round_any(max(c1.e0.m$Value, c2.e0.m$Value, c1.e0.f$Value, c2.e0.f$Value), f=ceiling, accuracy=5)
tfr.max <- plyr::round_any(max(c1.tfr$Value, c2.tfr$Value), f=ceiling, accuracy=1)

g=ggplot_build(plot)

## adjust y axis in life expectancy panel (panel 1)
g$layout$panel_params[[1]]$y$limits <- c(e0.min-e0.max*0.01, e0.max+e0.max*0.01) ## leave 1% of maximum e0 value as 'buffer' 
g$layout$panel_params[[1]]$y$continuous_range <- c(e0.min-e0.max*0.01, e0.max+e0.max*0.01)
g$layout$panel_params[[1]]$y$breaks <- seq(e0.min, e0.max, 2.5)
g$layout$panel_params[[1]]$y$minor_breaks <- seq(e0.min, e0.max, 2.5)
g$layout$panel_params[[1]]$y$scale$labels <- rev(rev(c(rbind(seq(e0.min, e0.max, 5), "")))[-1]) ## label only every other tick

## adjust y axis in tfr panel (panel 2)
g$layout$panel_params[[2]]$y$limits <- c(-tfr.max*0.05, tfr.max+tfr.max*0.05) ## leave 5% of maximum tfr value as 'buffer'
g$layout$panel_params[[2]]$y$continuous_range <- c(-tfr.max*0.05, tfr.max+tfr.max*0.05)
g$layout$panel_params[[2]]$y$breaks <- seq(0, tfr.max, 0.5)
g$layout$panel_params[[2]]$y$minor_breaks <- seq(0, tfr.max, 0.5)
g$layout$panel_params[[2]]$y$scale$labels <- rev(rev(c(rbind(seq(0, tfr.max, 1), "")))[-1]) ## label only every other tick

## transform back into ggplot
e0.tfr.plot <- ggpubr::as_ggplot(ggplot_gtable(g))

## save plot
for(i in c("pdf", "svg")){

ggsave(paste0(here::here(outpath), "/e0-tfr.", i), 
       height=80, width=180, units="mm", 
       plot=e0.tfr.plot, device=i)

}
