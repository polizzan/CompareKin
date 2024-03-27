#################################
# PLOT OUTPUT OF KINSHIP MODELS #
#################################

## loop over both focal sexes
for(z in c("f", "m")){ 
  
  if(z=="f"){
    
    c1.twosex <- c1.twosex.f 
    c2.twosex <- c2.twosex.f  
    
  }else{
    
    c1.twosex <- c1.twosex.m 
    c2.twosex <- c2.twosex.m    
    
  }  
  
  ## summarize kin types  
  c1c2.kin <-
    c1.twosex$kin_summary %>% 
    ## generate year variable
    mutate(year=cohort+age_focal) %>% 
    select(age_focal, sex_kin, year, kin, country1=count_living) %>%
    ## merge model results for country 1 and country 2
    merge(c2.twosex$kin_summary %>% 
            mutate(year=cohort+age_focal) %>% 
            select(age_focal, sex_kin, year, kin, country2=count_living),
          by.x=c("age_focal", "sex_kin", "year", "kin"),
          by.y=c("age_focal", "sex_kin", "year", "kin")) %>% 
    ## exclude great-grandparents and great-grandchildren
    filter(!kin %in% c("ggd", "ggm")) %>% 
    ## summarize kin types
    mutate(kin=case_when(kin %in% c("coa", "cya") & sex_kin=="f" ~ "Female Cousin", 
                         kin %in% c("coa", "cya") & sex_kin=="m" ~ "Male Cousin", 
                         kin=="d" & sex_kin=="f" ~ "Daughter",
                         kin=="d" & sex_kin=="m" ~ "Son",
                         kin=="gd" & sex_kin=="f" ~ "Granddaughter",
                         kin=="gd" & sex_kin=="m" ~ "Grandson",
                         kin=="gm" & sex_kin=="f" ~ "Grandmother",
                         kin=="gm" & sex_kin=="m" ~ "Grandfather",
                         kin=="m" & sex_kin=="f" ~ "Mother",
                         kin=="m" & sex_kin=="m" ~ "Father",
                         kin %in% c("nos", "nys") & sex_kin=="f" ~ "Niece", 
                         kin %in% c("nos", "nys") & sex_kin=="m" ~ "Nephew",
                         kin %in% c("oa", "ya") & sex_kin=="f" ~ "Aunt", 
                         kin %in% c("oa", "ya") & sex_kin=="m" ~ "Uncle",
                         kin %in% c("os") & sex_kin=="f" ~ "Older Sister",        
                         kin %in% c("os") & sex_kin=="m" ~ "Older Brother",
                         kin %in% c("ys") & sex_kin=="f" ~ "Younger Sister",
                         kin %in% c("ys") & sex_kin=="m" ~ "Younger Brother")) %>% 
    summarize(country1=sum(country1),
              country2=sum(country2), .by=c(age_focal, year, kin)) %>% ## kin count
    ## calculate kin ratio
    mutate(ratio=country1/country2) %>%  
    ## generate identifier, to be used later
    mutate(identifier=case_when(ratio==1 ~ "0",
                                ratio>0 & ratio<1 ~ "1",
                                ratio>1 & ratio<Inf ~ "2",
                                TRUE ~ NA)) %>%  
    ## set values to NA for all individuals born before first time period
    mutate(across(c(country1, country2, ratio, identifier), ~ case_when((year-age_focal)<min(data.years) ~ NA,
                                                                        TRUE ~ .))) 
  
  ## append data set with 'placeholder' kin
  c1c2.kin.with.fake <-
    c1c2.kin %>%
    add_row(fake.kin.df %>% mutate(country1=NA, country2=NA, ratio=NA, identifier=NA)) %>% 
    mutate(kin=factor(kin, levels=kin.order))
  
  ##############################################
  # a: PLOT TOTAL FAMILY SIZE IN LEXIS SURFACE #
  ##############################################
  
  c1c2.family.size <-
    c1c2.kin %>% 
    ## summarize numbers of all kin
    ## truncate numbers: family size is measured in integers
    summarize(country1=trunc(sum(country1)), 
              country2=trunc(sum(country2)),
              difference=trunc(sum(country1)-sum(country2)), 
              .by=c(age_focal, year)) %>% 
    ## pivot to long 
    pivot_longer(cols=c("country1", "country2", "difference"),
                 names_to="region",
                 values_to="number") %>%  
    ## generate factor variable to order plot panels: "difference" is in center
    mutate(region=factor(region, 
                         levels=c("country1", "difference", "country2"), 
                         labels=c(country1.label, paste0(country1.label, "\U2013", country2.label), country2.label))) 
  
  ## generate new data set with categorical variable for family size
  c1c2.family.size.num <-
    c1c2.family.size %>% 
    filter(region %in% c(country1.label, country2.label)) %>% 
    ## all numbers that exceed the 8th category are clustered in the open-ended category
    mutate(cat=case_when(number > min(number)+7 ~ paste0(">", (min(number)+7)), 
                         TRUE ~ as.character(number)),
           cat=factor(cat, 
                      levels=c(paste(min(number):(min(number)+7)), paste0(">", (min(number)+7)))))
  
  ## generate new data set with categorical variable for difference in family size
  c1c2.family.size.diff <-  
    c1c2.family.size %>% 
    filter(region %in% paste0(country1.label, "\U2013", country2.label)) %>% 
    mutate(cat=case_when(number < -4 ~ paste0("<", -4), ## there are 5 negative and 5 positive categories
                         number > 4 ~ paste0(">", 4),
                         TRUE ~ as.character(number)),
           cat=factor(cat, levels=c(paste0("<", -4), paste(-4:4), paste0(">", 4))))
  
  ## generate color palettes
  col.num <- RColorBrewer::brewer.pal(n=9, "Greens")
  col.diff <-  RColorBrewer::brewer.pal(n=11, "RdBu")
  
  ## create plot
  family.size.plot <-
    ggplot() +
    geom_tile(data=c1c2.family.size.num,
              aes(x=year, y=age_focal, fill=as.factor(cat))) +
    scale_fill_manual("Number",
                      values=col.num,
                      drop=FALSE, ## display unused categories
                      guide=guide_legend(order=1, ## place legend first 
                                         nrow=1, 
                                         title.position="top", 
                                         label.position="bottom", 
                                         title.hjust=0)) +
    ## add second 'fill' aesthetic 
    ggnewscale::new_scale("fill") + 
    geom_tile(data=c1c2.family.size.diff,
              aes(x=year, y=age_focal, fill=as.factor(cat))) +
    scale_fill_manual("Difference", 
                      values=col.diff,
                      drop=FALSE, 
                      guide=guide_legend(order=2, ## place legend second
                                         nrow=1, 
                                         title.position="top", 
                                         label.position="bottom", 
                                         title.hjust=0.5)) +
    ## add third 'fill' aesthetic 
    ggnewscale::new_scale("fill") + 
    geom_tile(data=c1c2.family.size.num,
              aes(x=year, y=age_focal, fill=as.factor(cat))) +
    scale_fill_manual("Number", 
                      values=col.num,
                      drop=FALSE,
                      guide=guide_legend(order=3, ## place legend third 
                                         nrow=1, 
                                         title.position="top", 
                                         label.position="bottom", 
                                         title.hjust=1)) +
    ## add vertical line for selected year 
    geom_vline(xintercept=discontinuity.line, linewidth=0.25, color="black") +  
    ## choose axis labels
    labs(x="Year", y="Age") + 
    ## choose axis values
    ## abbreviate labels
    scale_x_continuous(limits=c(min(plot.years), max(plot.years)),
                       breaks=seq(min(plot.years), max(plot.years), 10),
                       labels=paste0("'", substring(paste(seq(min(plot.years), max(plot.years), 10)), first=3, last=4))) + 
    scale_y_continuous(limits=c(min(mort.ages), max(mort.ages)),
                       breaks=seq(min(mort.ages), max(mort.ages), 10),
                       labels=seq(min(mort.ages), max(mort.ages), 10)) +
    theme_bw() +
    theme(aspect.ratio=1,
          panel.grid=element_blank(),
          panel.border=element_rect(linewidth=0.25),
          panel.background=element_rect(fill="grey80"), ## turn background grey: NAs plotted in grey
          axis.ticks=element_line(linewidth=0.25),
          axis.ticks.length=unit(1, "pt"),
          strip.background=element_rect(fill="white",
                                        linewidth=0.25),        
          strip.text=element_text(size=7),
          axis.title=element_text(size=7, face="bold"),
          axis.text=element_text(size=6),
          legend.position="bottom",
          legend.direction="horizontal",
          legend.key.height=unit(0.3, "cm"),
          legend.key.width=unit(0.3, "cm"),
          legend.title=element_text(size=7),
          legend.text=element_text(size=6),
          text=element_text(family="serif")) +
    coord_cartesian(xlim=c(min(plot.years), max(plot.years)),
                    ylim=c(0, min(length(data.years)-1, max(mort.ages)))) +
    facet_wrap(~ region)
  
  ## save plot
  for(i in c("pdf", "svg")){
  
  ggsave(paste0(here::here(outpath), paste0("/family-size_", z, ".", i)), 
         height=80, width=180, units="mm", 
         plot=family.size.plot, device=i)
  
  }  
    
  #######################################################
  # b: PLOT OF NUMBER OF KIN BY AGE GROUPS ACROSS YEARS #
  #######################################################
  #####
  # & #
  #####
  #####################################################
  # c: PLOT OF NUMBER OF KIN ACROSS AGES IN LAST YEAR #
  #####################################################
  
  c1c2.kin.by.age.group <-
    c1c2.kin.with.fake %>% 
    select(-c(ratio, identifier)) %>%
    ## select age groups
    filter(age_focal %in% kin.by.age) %>% 
    pivot_longer(cols=c("country1", "country2"),
                 names_to="region",
                 values_to="number") %>%   
    mutate(region=factor(region, levels=c("country1", "country2"), labels=c(country1.label, country2.label))) %>% 
    ## determine y axis range for each kin  
    mutate(label=ifelse(kin %in% fake.kin, NA, plyr::round_any(max(number, na.rm=TRUE), accuracy=0.2, f=ceiling)), .by=kin) 
  
  c1c2.kin.last.year <-
    c1c2.kin.with.fake %>% 
    select(-c(ratio, identifier)) %>%
    ## select last year
    filter(year %in% max(year)) %>%  
    pivot_longer(cols=c("country1", "country2"),
                 names_to="region",
                 values_to="number") %>%   
    mutate(region=factor(region, levels=c("country1", "country2"), labels=c(country1.label, country2.label))) %>% 
    mutate(label=ifelse(kin %in% fake.kin, NA, plyr::round_any(max(number, na.rm=TRUE), accuracy=0.2, f=ceiling)), .by=kin)
  
  ## harmonize y axis ranges across similar types of kin in each data set
  for(df in c("c1c2.kin.by.age.group", "c1c2.kin.last.year")){
    
    aux <-
      get(df)
    
    aux$label[aux$kin %in% c("Grandfather", "Grandmother")] <- max(aux$label[aux$kin %in% c("Grandfather", "Grandmother")], na.rm=TRUE)
    aux$label[aux$kin %in% c("Uncle", "Aunt")] <- max(aux$label[aux$kin %in% c("Uncle", "Aunt")], na.rm=TRUE)
    aux$label[aux$kin %in% c("Father", "Mother")] <- max(aux$label[aux$kin %in% c("Father", "Mother")], na.rm=TRUE)
    aux$label[aux$kin %in% c("Male Cousin", "Female Cousin")] <- max(aux$label[aux$kin %in% c("Male Cousin", "Female Cousin")], na.rm=TRUE)
    aux$label[aux$kin %in% c("Older Brother", "Older Sister")] <- max(aux$label[aux$kin %in% c("Older Brother", "Older Sister")], na.rm=TRUE)
    aux$label[aux$kin %in% c("Younger Brother", "Younger Sister")] <- max(aux$label[aux$kin %in% c("Younger Brother", "Younger Sister")], na.rm=TRUE)
    aux$label[aux$kin %in% c("Nephew", "Niece")] <- max(aux$label[aux$kin %in% c("Nephew", "Niece")], na.rm=TRUE)
    aux$label[aux$kin %in% c("Son", "Daughter")] <- max(aux$label[aux$kin %in% c("Son", "Daughter")], na.rm=TRUE)
    aux$label[aux$kin %in% c("Grandson", "Granddaughter")] <- max(aux$label[aux$kin %in% c("Grandson", "Granddaughter")], na.rm=TRUE)
    
    assign(df, aux)
    rm(aux)
    
  }
  
  ## plot number of kin
  kin.by.age.group.plot <-
    c1c2.kin.by.age.group %>% 
    mutate(age_focal=paste("Age", age_focal)) %>% 
    ggplot(aes(x=year, y=number, color=factor(interaction(region, as.factor(age_focal), sep = ", "),
                                              levels=c(paste(country1.label, paste("Age", kin.by.age[1]), sep=", "),
                                                       paste(country2.label, paste("Age", kin.by.age[1]), sep=", "),
                                                       paste(country1.label, paste("Age", kin.by.age[2]), sep=", "),
                                                       paste(country2.label, paste("Age", kin.by.age[2]), sep=", "),
                                                       paste(country1.label, paste("Age", kin.by.age[3]), sep=", "),
                                                       paste(country2.label, paste("Age", kin.by.age[3]), sep=", "),
                                                       paste(country1.label, paste("Age", kin.by.age[4]), sep=", "),
                                                       paste(country2.label, paste("Age", kin.by.age[4]), sep=", "))))) + 
    ## add horizontal line at 0
    geom_hline(yintercept=0, linewidth=0.25, color="black") + 
    ## add vertical line for selected year 
    geom_vline(xintercept=discontinuity.line, linewidth=0.25, color="black") +  
    ## draw lines for number of kin
    geom_line(linewidth=0.5) + 
    ## choose axis titles
    labs(x="Year", y="") +
    ## choose axis values
    ## abbreviate labels
    scale_x_continuous(limits=c(min(plot.years), max(plot.years)),
                       breaks=seq(min(plot.years), max(plot.years), 10),
                       labels=paste0("'", substring(paste(seq(min(plot.years), max(plot.years), 10)), first=3, last=4))) + 
    ## assign colors to lines 
    scale_color_manual("", values=palette.kin.by.age) +
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
          axis.text.x=element_text(size=5),
          axis.text.y=element_text(size=6),
          legend.position="bottom",
          legend.key.width=unit(0.5, "cm"),
          legend.text=element_text(size=7),
          text=element_text(family="serif")) +
    ## create panels, allow y axes to range freely
    facet_wrap(~ kin, ncol=6, scales="free") +
    coord_cartesian(xlim=c(min(plot.years), max(plot.years)),
                    ylim=c(0,10))
  
  kin.last.year.plot <-
    c1c2.kin.last.year %>% 
    ggplot(aes(x=age_focal, y=number, color=region)) + 
    ## add horizontal line at 0
    geom_hline(yintercept=0, linewidth=0.25, color="black") +
    ## draw lines for number of kin
    geom_line(linewidth=0.5) +
    ## choose axis titles
    labs(x="Age", y="") +
    ## choose axis values
    scale_x_continuous(limits=c(min(mort.ages), max(mort.ages)),
                       breaks=seq(min(mort.ages), max(mort.ages), 10),
                       labels=seq(min(mort.ages), max(mort.ages), 10)) +
    ## assign colors to lines 
    scale_color_manual("", values=palette.kin.last.year) +
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
          axis.text.x=element_text(size=5),
          axis.text.y=element_text(size=6),
          legend.position="bottom",
          legend.key.width=unit(0.5, "cm"),
          legend.text=element_text(size=7),
          text=element_text(family="serif")) +
    ## create panels, allow y axes to range freely
    facet_wrap(~ kin, ncol=6, scales="free") +
    coord_cartesian(xlim=c(0, min(length(data.years)-1, max(mort.ages))),
                    ylim=c(0,10))
  
  ## adjust y axis range for each panel in both plots
  for(plot in c("kin.by.age.group.plot", "kin.last.year.plot")){
    
    g=ggplot_build(get(plot))
    
    ## remove 'placeholder' kin 
    g$layout$layout <- g$layout$layout[g$layout$layout$kin %in% kin.list, ]  
    ## assign each panel of non-'placeholder' kin a new panel number
    g$layout$layout$PANEL <- 1:length(kin.list) 
    g$layout$layout$SCALE_X <- 1:length(kin.list) 
    g$layout$layout$SCALE_Y <- 1:length(kin.list)
    
    ## first layer = horizontal 0 line: same in all panels, so just keep first 'n' panels
    g$data[[1]] <- g$data[[1]][g$data[[1]]$PANEL %in% 1:length(kin.list), ]
    
    if(plot=="kin.by.age.group.plot"){
      
      ## second layer in "kin.by.age.group.plot" = vertical line: same in all panels, so just keep first 'n' panels
      
      g$data[[2]] <- g$data[[2]][g$data[[2]]$PANEL %in% 1:length(kin.list), ]
      
    }
    
    ## last layer = numbers of kin: different in each panel, keep only panels of non-'placeholder' kin
    if(plot=="kin.by.age.group.plot"){
      
      g$data[[3]] <- g$data[[3]][g$data[[3]]$PANEL %in% c(3, 4, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 20, 21, 22, 23, 27, 28), ]
      
      
    }else{
      
      g$data[[2]] <- g$data[[2]][g$data[[2]]$PANEL %in% c(3, 4, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 20, 21, 22, 23, 27, 28), ]
      
    }
    
    ## 'i' and 'j' are used to change the axis parameters in the appropriate panels and assign new panel numbers
    j <- 1 
    
    for(i in c(3, 4, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 20, 21, 22, 23, 27, 28)){ ## = panels of non-'placeholder' kin
      
      ## assign new panel number  
      if(plot=="kin.by.age.group.plot"){
        
        g$data[[3]]$PANEL[g$data[[3]]$PANEL==i] <- j 
        
      }else{
        
        g$data[[2]]$PANEL[g$data[[2]]$PANEL==i] <- j 
        
      }
      
      ## get y axis range
      if(plot=="kin.by.age.group.plot"){lmax <- c1c2.kin.by.age.group %>% filter(kin==kin.list[j]) %>% pull(label) %>% unique()}
      if(plot=="kin.last.year.plot"){lmax <- c1c2.kin.last.year %>% filter(kin==kin.list[j]) %>% pull(label) %>% unique()}
      
      ## adjust y axis in appropriate panel number 
      g$layout$panel_params[[j]]$y$limits <- c(-lmax*0.05, lmax+lmax*0.05) ## leave 5% of maximum value as 'buffer'
      g$layout$panel_params[[j]]$y$continuous_range <- c(-lmax*0.05, lmax+lmax*0.05)
      g$layout$panel_params[[j]]$y$breaks <- c(0, lmax/4, lmax/2, lmax/4*3, lmax)
      g$layout$panel_params[[j]]$y$minor_breaks <- c(0, lmax/4, lmax/2, lmax/4*3, lmax)
      g$layout$panel_params[[j]]$y$scale$labels <- sprintf("%04.2f", abs(c(0, lmax/4, lmax/2, lmax/4*3, lmax)))
      
      ## remove x axis for certain panels
      if(i %in% c(3, 4, 8, 9, 10, 11, 14, 15, 16, 17, 21, 22)){
        g$layout$panel_params[[j]]$x$breaks <- NULL 
        g$layout$panel_params[[j]]$x$minor_breaks <- NULL
        g$layout$panel_params[[j]]$x$scale$labels <- NULL
      }
      
      j <- j+1
      
    }
    
    g <- ggplot_gtable(g)
    
    assign(plot, g)
    rm(g)
    
  }
  
  ## transform back into ggplot
  kin.by.age.group.plot <- 
    ggpubr::as_ggplot(kin.by.age.group.plot) + 
    ## add letter 'f' (focal) at appropriate position
    annotate("text", x=0.5225, y=0.5, label="F", size=10, hjust=0.5) + 
    theme(text=element_text(family="serif"))
  
  kin.last.year.plot <- 
    ggpubr::as_ggplot(kin.last.year.plot) + 
    ## add letter 'f' (focal) at appropriate position
    annotate("text", x=0.5225, y=0.47, label="F", size=10, hjust=0.5) +
    theme(text=element_text(family="serif"))
  
  ## save plots
  for(i in c("pdf", "svg")){
  
  ggsave(paste0(here::here(outpath), "/kin-by-age_", z, ".", i), 
         height=160, width=180, units="mm", plot=kin.by.age.group.plot, 
         device=i)
  
  ggsave(paste0(here::here(outpath), "/kin-last-year_", z, ".", i), 
         height=160, width=180, units="mm", plot=kin.last.year.plot, 
         device=i)
  
  }
  
  #########################
  # d: PLOT OF KIN RATIOS #
  #########################
  
  ## determine labels for categories
  kin.ratio.cat.lab <-
    c1c2.kin.with.fake %>% 
    filter(identifier %in% c("0", "1", "2")) %>% 
    ## take inverse for ratios < 1
    ## 'include.lowest=FALSE' + 'right=TRUE' = ratio of 1 will not be included in any positive or negative category
    mutate(ratio=case_when(ratio>0 & ratio <1 ~ 1/ratio, 
                           TRUE ~ ratio),
           cat.ratio=arules::discretize(ratio, breaks=kin.ratio.cat, include.lowest=FALSE, right=TRUE)) %>% 
    pull(cat.ratio) %>% 
    levels() 
  
  ## extract upper and lower limits of categories
  lab1.aux <- kin.ratio.cat.lab %>% str_extract("(?<=\\().+(?=,)") %>% as.numeric() 
  lab2.aux <- kin.ratio.cat.lab %>% str_extract("(?<=,).+(?=\\])") %>% as.numeric()
  
  ## clean up labels: take inverse for negative categories and make sure that labels always have two decimals
  lab1.neg <- sprintf("%04.2f", 1/rev(lab2.aux))
  lab2.neg <- sprintf("%04.2f", 1/rev(lab1.aux))  
  lab1.pos <- sprintf("%04.2f", lab1.aux)
  lab2.pos <- sprintf("%04.2f", lab2.aux)  
  
  ## join positive and negative labels together
  labels <- c(paste0("<", lab2.neg[1]), 
              paste0(lab1.neg[-1], "-", lab2.neg[-1]), 
              "1.00", 
              paste0(lab1.pos[-length(lab1.pos)], "-", lab2.pos[-length(lab2.pos)]),
              paste0(">", lab1.pos[length(lab1.pos)])) 
  
  ## categorize data
  c1c2.kin.ratio <-
    c1c2.kin.with.fake %>% 
    filter(identifier %in% c("0", "1", "2")) %>% 
    ## take inverse
    mutate(ratio=case_when(ratio>0 & ratio <1 ~ 1/ratio,
                           TRUE ~ ratio),
           ## assign generic labels
           cat.ratio=as.numeric(as.character(arules::discretize(ratio, breaks=kin.ratio.cat, include.lowest=FALSE, right=TRUE, labels=c(1:kin.ratio.cat)))),  
           ## label 0 if ratio is equal to 1
           cat.ratio=case_when(identifier=="0" ~ 0, 
                               ## take negative of label if ratio is smaller than 1
                               identifier=="1" ~ -1 * cat.ratio, 
                               TRUE ~ cat.ratio)) %>% 
    ## add observations with missing values for cat.ratio
    add_row(
      
      c1c2.kin.with.fake %>% 
        filter(is.na(identifier)) %>% 
        mutate(cat.ratio=NA) 
      
    ) %>% 
    arrange(age_focal, year, kin) %>% 
    select(-identifier) %>% 
    ## assign actual labels
    mutate(cat.ratio=factor(cat.ratio, levels=-kin.ratio.cat:kin.ratio.cat, labels=labels)) 
  
  ## plot ratio of kin
  plot <-
    c1c2.kin.ratio %>%   
    ggplot(aes(x=year, y=age_focal, fill=cat.ratio)) + 
    geom_tile() +
    ## add vertical line for selected year
    geom_vline(xintercept=discontinuity.line, linewidth=0.25, color="black") + 
    ## choose axis titles
    labs(x="Year", y="Age") +
    ## choose axis values
    ## abbreviate labels
    scale_x_continuous(limits=c(min(plot.years), max(plot.years)),
                       breaks=seq(min(plot.years), max(plot.years), 10),
                       labels=paste0("'", substring(paste(seq(min(plot.years), max(plot.years), 10)), first=3, last=4))) + 
    scale_y_continuous(limits=c(min(mort.ages), max(mort.ages)),
                       breaks=seq(min(mort.ages), max(mort.ages), 10),
                       labels=seq(min(mort.ages), max(mort.ages), 10)) +
    ## assign colors to tiles, NAs will not be plotted
    scale_fill_manual("", values=palette.kin.ratio, na.translate=FALSE, drop=FALSE) + 
    ## format legend
    guides(fill=guide_legend(ncol=1, 
                             reverse=TRUE, 
                             label.position="right", 
                             label.hjust=0.5, 
                             byrow=TRUE, 
                             override.aes=list(color="black"))) + 

    theme_bw() +
    theme(aspect.ratio=1,
          panel.grid=element_blank(),
          panel.border=element_rect(linewidth=0.25),
          panel.background=element_rect(fill="grey80"), ## turn background grey: NAs plotted in grey
          axis.ticks=element_line(linewidth=0.25),
          axis.ticks.length=unit(1, "pt"),
          strip.background=element_rect(fill="white",
                                        linewidth=0.25),        
          strip.text=element_text(size=7),
          axis.title=element_text(size=7, face="bold"),
          axis.text.x=element_text(size=5),
          axis.text.y=element_text(size=6),
          legend.position=c(0.925, 0.175),
          legend.key.height=unit(0.3, "cm"),
          legend.key.width=unit(0.3, "cm"),
          legend.text=element_text(size=7),
          text=element_text(family="serif")) +
    ## create panels, allow y axes to range freely
    coord_cartesian(xlim=c(min(plot.years), max(plot.years)),
                    ylim=c(0, min(length(data.years)-1, max(mort.ages)))) +
    facet_wrap(~ kin, ncol=6, scales="free") 
  
  ## remove panels for 'placeholder' kin, and remove selected x and y axes
  g=ggplotGrob(plot)
  
  ## vector of features to remove
  panels <- c( 
    
    ## left axis
    "axis-l-1-1",
    "axis-l-2-1",
    "axis-l-4-1", 
    "axis-l-5-1",
    
    "axis-l-1-2",
    "axis-l-3-2",
    "axis-l-5-2", 
    
    "axis-l-2-3",
    "axis-l-3-3",
    "axis-l-4-3", 
    
    "axis-l-1-4",
    "axis-l-2-4",
    "axis-l-3-4", 
    "axis-l-4-4",   
    "axis-l-5-4",
    
    "axis-l-1-5",
    "axis-l-2-5",
    "axis-l-3-5", 
    "axis-l-4-5",   
    "axis-l-5-5",
    
    "axis-l-1-6",
    "axis-l-2-6",
    "axis-l-3-6", 
    "axis-l-4-6",   
    "axis-l-5-6",
    
    ## bottom axis
    "axis-b-1-1",
    "axis-b-2-1",
    "axis-b-3-1",
    "axis-b-4-1",
    "axis-b-5-1",
    "axis-b-6-1",
    
    "axis-b-1-2",
    "axis-b-2-2",
    "axis-b-3-2",
    "axis-b-4-2",
    "axis-b-5-2",
    "axis-b-6-2",
    
    "axis-b-2-3",
    "axis-b-3-3",
    "axis-b-4-3",
    "axis-b-5-3",
    
    "axis-b-1-4",
    "axis-b-3-4",
    "axis-b-4-4",
    "axis-b-6-4",
    
    "axis-b-1-5",
    "axis-b-2-5",
    "axis-b-5-5",
    "axis-b-6-5",
    
    ## strip
    "strip-t-1-1",
    "strip-t-2-1",
    "strip-t-5-1",
    "strip-t-6-1",
    "strip-t-1-2",
    "strip-t-6-2",
    "strip-t-1-4",
    "strip-t-6-4",
    "strip-t-1-5",
    "strip-t-2-5",
    "strip-t-5-5",
    "strip-t-6-5",
    
    ## panel
    "panel-1-1",
    "panel-2-1",
    "panel-4-1",
    "panel-5-1",
    "panel-6-1",
    "panel-4-2",
    "panel-3-4",
    "panel-1-5",
    "panel-2-5",
    "panel-3-5",
    "panel-5-5",
    "panel-6-5"
    
  )
  
  ## remove selected features
  for(j in panels){ 
    
    g$grobs[[max(which(g$layout$name==j))]]=grid::nullGrob()
    
  }
  
  ## transform back into ggplot
  kin.ratio.plot <- 
    ggpubr::as_ggplot(gridExtra::grid.arrange(g)) +
    ## add letter 'f' (focal) at appropriate position
    annotate("text", x=0.52, y=0.41, label="F", size=10, hjust=0.5) +
    theme(text=element_text(family="serif"))
  
  ## save plot
  for(i in c("pdf", "svg")){
  
  ggsave(paste0(here::here(outpath), "/kin-ratio_", z, ".", i), 
         height=160, width=180, units="mm", plot=kin.ratio.plot, 
         device=i)
    
  }

}
