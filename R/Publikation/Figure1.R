#plot parameter

size_plot <- 35
lwd_line <- 1.5


# data

load("data/data_age.RData")

data_age <- data_age %>%
  mutate(age_group= factor(age_group, levels =  c("<1","1-4", "5-14","15-19",
                                                  "20-29", "30-39", "40-49",
                                                  "50-79", ">=80")),
         mortality = round(mortality,2),
         mortality2 = round(mortality2,2),
         Year =as.integer(Year)) 
# Heatmap plot

data_jenk <- data_age %>%
  filter(!mortality==0)

jenks_breaks <- c(min(data_jenk['mortality2']),natural_breaks(k=10, data_jenk['mortality2']),max(data_jenk['mortality2']))

data_age <- data_age %>%
  mutate(mort_jenk = cut(mortality2,
                         breaks=jenks_breaks,
                         include.lowest = TRUE, right = FALSE),
         mort_jenk=as.character(mort_jenk),
         mort_jenk=replace_na(mort_jenk,"0"),
         mort_jenk= factor(mort_jenk, levels =  c("0","[0.06,2.34)", "[2.34,3.81)","[3.81,8.53)","[8.53,37.6)",
                                                  "[37.6,48.8)", "[48.8,81.3)", "[81.3,129)","[129,192)",
                                                  "[192,285)", "[285,371]")),
         mort_jenk=recode(mort_jenk, "[0.06,5.59)" = "(0.00,2.34)" ))


Figure_heatmap <- ggplot(data_age, aes(Year, age_group, fill= mort_jenk)) + 
  geom_tile() +
  geom_vline(xintercept = 1976, col = "red", lwd = 1.5) +  # Füge die vertikale rote Linie bei 1976 hinzu
  # scale_fill_viridis("Mortality", discrete=TRUE,guide = guide_legend(reverse = TRUE)) +
  scale_fill_viridis("Mortality", discrete=TRUE) +
  scale_x_continuous( "Year",
                      labels = c(seq(1880,2020,20)),
                      breaks = c(seq(1880,2020,20)))+
  
  xlab("Year")+
  ylab("Age groups")+
  ggtitle("A) Mortality per 100,000 inhabitants")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = size_plot),
    legend.text = element_text(size = size_plot),
    legend.title = element_text(size = size_plot),
    legend.position = "bottom",
    legend.key.width= unit(3, "line"),
    legend.key.size= unit(1, 'cm'),
    axis.text.x = element_text(size=size_plot),
    axis.text.y = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot)) 

cowplot::save_plot("output/Figure1.pdf", Figure_heatmap ,base_height=15,base_width=30)



#
#
#
 
lifetable <- read.csv("data_raw/Lifetables.txt", header=TRUE, sep="") %>%
  mutate(Age= recode(Age, "110+" = "110"),
         Age= as.integer(Age),
         age_group = case_when(Age==0 ~ "<1",
                               Age>=1 & Age <=4 ~ "1-4",
                               Age>=5 & Age <=14 ~ "5-14",
                               Age>=15 & Age <=19 ~ "15-19",
                               Age>=20 & Age <=29 ~ "20-29",
                               Age>=30 & Age <=39 ~ "30-39",
                               Age >= 40 & Age <= 49 ~ "40-49",
                               Age >= 50 & Age <= 79 ~ "50-79",
                               Age >=80 ~ ">=80"))%>%
  # select(Year, age_group, mx, ex) %>%
  group_by(Year, age_group) %>%
  mutate(mean_mx = mean(mx),
         mean_ex = mean(ex)) %>%
  distinct(Year, age_group, .keep_all = TRUE) %>%
  ungroup()%>%  
  filter(Age < 5)                                                                          #filter von 20 auf 5 Jahre verändert

data_life <- data_age %>%
  mutate(age_group= factor(age_group, levels =  c("<1","1-4", "5-14","15-19",
                                                  "20-29", "30-39", "40-49",
                                                  "50-79", ">=80")),
         age_deaths = recode(age_group,
                             "<1" = "1",
                             "1-4" = "2"),
         age_deaths = as.numeric(age_deaths),
         age_mean = recode(age_group,
                           "<1" = "0",
                           "1-4" = "3"),
         Year =as.integer(Year),
         cause_death=1) %>%
  filter(age_deaths <=2) %>%
  group_by(Year) %>%
  mutate(pop_20 = sum(pop_age)) %>%
  ungroup() %>%
  left_join(lifetable) %>%
  expand.dft(., freq="Total_death") %>%
  group_by(Year, age_mean) %>%
  mutate(death_n = sum(cause_death)) %>%
  ungroup() %>%
  mutate(ex_c = 5-age_mean,       #Das muss doch jetzt folglich mean_ex-age_mean sein und nicht irgend eine Zahl x-age_mean. Weil das macht ja die differenz aus, die man nachher mit der Anzahl Tode multipliziert um auf YLL zu kommen. 
         LYL = death_n * ex_c) %>%
  distinct(Year, age_mean, .keep_all = TRUE) %>%
  group_by(Year) %>%
  mutate(LYL_total = sum(LYL),
         death_total = sum( death_n )) %>%
  ungroup() %>%
  distinct(Year, .keep_all = TRUE)  %>%
  mutate(YLL_crude = LYL_total/pop_20*10000,
         YLL_crude = round(YLL_crude,2),
         YLL_crude2 = LYL_total/pop_20,
         YLL_crude2 = round(YLL_crude2,4),
         YLL_mean_age = LYL_total/death_total)

# write.xlsx(data_life,file="data/years_life_lost.xlsx",rowNames=FALSE, overwrite = TRUE)

data_mod <- data.frame(Year =seq(1970, 2022,1),YLL_crude2=0 )
data_life <- data_life %>%
  select(Year, YLL_crude2) %>%
  rbind(data_mod) %>%
  arrange(Year)


Figure_yll <- ggplot()+
  geom_step(data=data_life, aes(x=Year, y= YLL_crude2), lwd=lwd_line, col="grey20") +
  #geom_vline(xintercept=1973, lwd=1.2, col="black", alpha=0.3) +
  xlab("Year") +
  ylab("Years of life lost rate before the age of 5") +                                                  #von 20 auf 5 geändert
  ggtitle("B) Years of life lost rate before the age of 5")+
  scale_x_continuous( "Year",
                      labels = c(seq(1880,2020,20)),
                      breaks = c(seq(1880,2020,20)))+
  theme_bw() +
  theme(
    axis.text.y = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot),
    plot.title = element_text(size=size_plot))

Figure1 <- cowplot::plot_grid(Figure_heatmap,NULL,Figure_yll,
                              nrow=3, align="hv",
                              rel_heights = c(1,0.05,1))


cowplot::save_plot(paste0("output/Figure1m_test.pdf"), Figure1,base_height=25,base_width=25)



#Tabelle erstellen 
data_excel <- data_age %>%
  mutate(age_group= factor(age_group, levels =  c("<1","1-4", "5-14","15-19",
                                                  "20-29", "30-39", "40-49",
                                                  "50-79", ">=80")),
         age_deaths = recode(age_group,
                             "<1" = "1",
                             "1-4" = "2"),
         age_deaths = as.numeric(age_deaths),
         age_mean = recode(age_group,
                           "<1" = "0",
                           "1-4" = "3"),
         Year =as.integer(Year),
         cause_death=1) %>%
  filter(age_deaths <=2) %>%
  group_by(Year) %>%
  mutate(pop_20 = sum(pop_age)) %>%
  ungroup() %>%
  left_join(lifetable) %>%
  expand.dft(., freq="Total_death") %>%
  group_by(Year, age_mean) %>%
  mutate(death_n = sum(cause_death)) %>%
  ungroup() %>%
  mutate(ex_c = 5-age_mean,                                                                            #von 20 auf 5 geändert
         LYL = death_n * ex_c) %>%
  distinct(Year, age_mean, .keep_all = TRUE) %>%
  group_by(Year) %>%
  mutate(LYL_total = sum(LYL),
         death_total = sum( death_n )) %>%
  ungroup() %>%
  distinct(Year, .keep_all = TRUE)  %>%
  mutate(YLL_crude = LYL_total/pop_20*10000,
         YLL_crude = round(YLL_crude,2),
         YLL_crude2 = LYL_total/pop_20,
         YLL_crude2 = round(YLL_crude2,4),
         YLL_mean_age = LYL_total/death_total)

data_excel <- data_excel %>%
  select(Year, age_group, pop_20, YLL_mean_age, ex_c, death_total, LYL_total, YLL_crude, YLL_crude2) %>%
  #  rbind(data_mod) %>%
  arrange(Year)

write.xlsx(data_excel,file="data/years_life_lost.xlsx",rowNames=FALSE, overwrite = TRUE)

