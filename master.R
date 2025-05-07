library(tidyverse)
library(openxlsx)
library(data.table)
library(lubridate)
library(INLA)
library(scales)
library(rmarkdown)
library(wktmo)
library(sf)
library(sp)
library(spdep)
library(tmap)
library(tmaptools)
library(spgwr)
library(ggsci)
library(kableExtra)
library(epiR)
library(imputeTS)
library(TTR)
library(strucchange)
library(seasonal)
library(forecast)
library(paletteer)
library(BAMMtools)
library(cowplot)
library(viridis)
library(patchwork)
library(rgeoda)
library(conflicted)
library(vcdExtra)


conflict_prefer("rename", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("group_by", "dplyr")
conflict_prefer("summarise", "dplyr")



# 
col_pal <- pal_jco()(8)
# col_saes <-  paletteer::paletteer_d("colorBlindness::Brown2Blue12Steps", direction = 1)

col_saes <-  paletteer::paletteer_dynamic("cartography::turquoise.pal", 12, direction = -1)
col_greys <- paletteer_d("ggthemes::Seattle_Grays")

col_brown <- paletteer_d("ggthemes::excel_Feathered")[2:6]
col_jama <- paletteer_d("ggsci::default_jama")[2:6]

season.labs <- c("Jan","Feb", "Mar","Apr","May","Jun","Jul", "Aug","Sep","Okt", "Nov","Dec")

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
mypalette <- viridis(13, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
mypalette3 <- viridis(3, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
mypalette4 <- viridis(4, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
mypalette_c <- c("#440154FF", "#238a8dff", "#94D840FF", "#FDE725FF")

mypalette2 <- viridis(2, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")

