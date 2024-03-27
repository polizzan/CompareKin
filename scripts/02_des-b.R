######################################
# PLOT TRENDS IN GENERATIONAL LENGTH #
######################################

for(k in c(country1.code, country2.code)){
  
  ## load female age-specific mortality rates
  mort <-
    HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_v20240226.zip"), 
                            filename=paste0("lt_female/fltper_1x1/", k, ".fltper_1x1.txt"))) 

  ## load female age-specific fertility rates  
  fert <-
    HMDHFDplus::readHFD(unz(description=here::here("data", "hfd_statistics_d20240309.zip"), 
                            filename="asfrRR.txt")) %>%
    filter(Code==k) %>% 
    select(Year, Age, ASFR) 

  aux <-
    ## merge mortality and fertility information
    mort %>% 
    left_join(fert, by=c("Year", "Age")) %>% 
    filter(Year %in% data.years) %>% 
    ## rate of female births to females
    mutate(ASFR=ifelse(is.na(ASFR), 0, ASFR * SRB)) %>% 
    ## calculate mean generational length at each age
    mutate(denom=rev(cumsum(rev(Lx/100000 * ASFR))),
           num=rev(cumsum(rev((Age + ax) * (Lx/100000 * ASFR)))),
           t=num/denom, .by=Year) %>% 
    filter(Age==0) %>% 
    select(Year, t) %>% 
    mutate(Country=k)
  
  ## combine information from both countries
  if(k==country1.code){
    
    generation <-
      aux
    
  }
  
  if(k==country2.code){
    
    generation <-
      generation %>% 
      add_row(
        aux
      )
    
  }
}

## create plot
plot <-
  generation %>% 
  mutate(Indicator="Mean Length of Generation") %>% 
  
  ## add difference in mean length of generation
  add_row(
    
    generation %>% 
      ## pivot to wide
      pivot_wider(names_from=Country, 
                  values_from=t) %>% 
      ## subtract
      mutate(Difference=get(country2.code) - get(country1.code)) %>% 
      select(Year, Difference) %>% 
      ## pivot to long
      pivot_longer(cols=Difference,
                   names_to="Country",
                   values_to="t")  %>% 
      mutate(Indicator="Difference")
    
  ) %>% 

  mutate(Indicator = factor(Indicator, levels=c("Mean Length of Generation", "Difference"))) %>% 
  ggplot() +
  ## add vertical dashed line for selected year
  geom_vline(xintercept=discontinuity.line, linewidth=0.25, color="grey60", linetype="longdash") +
  ## add horizontal line at 0  
  geom_hline(yintercept=0, linewidth=0.25, linetype="solid") +
  ## draw lines for mean generational length
  geom_line(aes(x=Year, y=t, color=factor(Country,
                                          levels=c(country1.code, country2.code, "Difference"),
                                          labels=c(country1.label, country2.label, "Difference"))), 
            linewidth=0.5) +
  ## choose axis titles
  labs(x="Year", y="Years") +
  ## choose axis values
  scale_x_continuous(breaks=seq(min(plot.years), max(plot.years), 5),
                     labels=rev(rev(c(rbind(seq(min(plot.years), max(plot.years), 10), "")))[-1])) + ## label only every other tick
  ## assign colors to lines
  scale_color_manual("", values=palette.mlg) +
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
  facet_wrap(~ Indicator, scales = "free_y")

## adjust y axes
## determine axis limits
T.min <- plyr::round_any(min(generation$t), f=floor, accuracy=2)
T.max <- plyr::round_any(max(generation$t), f=ceiling, accuracy=2)

Diff <-
  generation %>% 
  select(Country, t, Year) %>% 
  pivot_wider(names_from=Country, 
              values_from=t) %>% 
  mutate(Difference=get(country2.code) - get(country1.code)) %>% 
  pull(Difference) 

diff.max <- plyr::round_any(max(Diff), f=ceiling, accuracy=1)

g=ggplot_build(plot)

## adjust y axis in generational length panel (panel 1)
g$layout$panel_params[[1]]$y$limits <- c(T.min-T.max*0.01, T.max+T.max*0.01) ## leave 1% of maximum T value as 'buffer' 
g$layout$panel_params[[1]]$y$continuous_range <- c(T.min-T.max*0.01, T.max+T.max*0.01)
g$layout$panel_params[[1]]$y$breaks <- seq(T.min, T.max, 1)
g$layout$panel_params[[1]]$y$minor_breaks <- seq(T.min, T.max, 1)
g$layout$panel_params[[1]]$y$scale$labels <- rev(rev(c(rbind(seq(T.min, T.max, 2), "")))[-1]) ## label only every other tick

## adjust y axis in difference panel (panel 2)
g$layout$panel_params[[2]]$y$limits <- c(-diff.max*0.05, diff.max+diff.max*0.05) ## leave 5% of maximum diff value as 'buffer'
g$layout$panel_params[[2]]$y$continuous_range <- c(-diff.max*0.05, diff.max+diff.max*0.05)
g$layout$panel_params[[2]]$y$breaks <- seq(0, diff.max, 0.5)
g$layout$panel_params[[2]]$y$minor_breaks <- seq(0, diff.max, 0.5)
g$layout$panel_params[[2]]$y$scale$labels <- rev(rev(c(rbind(seq(0, diff.max, 1), "")))[-1]) ## label only every other tick

## transform back into ggplot
generational.length.plot <- ggpubr::as_ggplot(ggplot_gtable(g))

## save plot
for(i in c("pdf", "svg")){

ggsave(paste0(here::here(outpath), "/generational-length.", i), 
       height=80, width=180, units="mm", 
       plot=generational.length.plot, device=i)
  
}
