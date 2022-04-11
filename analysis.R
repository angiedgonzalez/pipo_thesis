setwd("G:/My Drive/2020_Cores_Gonzalez/Scripts and data")

Packages <- c("data.table", "MuMIn","caret","tidyverse", "readxl", "dplyr", "lubridate", 
              "lme4", "lmerTest", "ggpubr", "sjPlot", "sjmisc", "broom", "gtsummary")
#install.packages("glmmTMB")

### + guides(color=guide_legend(reverse = TRUE))
# could be code to reverse legend order for C13

lapply(Packages, library, character.only = TRUE)
library(visreg)
library(gratia)
library(DHARMa)
library(gridExtra)
library(effects)
library(ggeffects)
options(na.action=na.fail) ## needed for DREDGE 
all_data <- read.csv("allfield_data.csv")
all_data <- all_data %>% subset(select=-c(X))
tree_avgs <- read.csv("all_averages.csv")
ggplot(all_data)+geom_bar(aes(year))

all_data <- all_data %>% subset(year<=2020 & year>=2003)
all_data$site <- toupper(all_data$site)
tree_avgs$site <- toupper(tree_avgs$site)

### dropped to 2003. 


#### DUCT~GROWTH ####


#### tree models #####

tree_ductgrow_mod1 <- lmer(totalresinarea~scale(growth12)*scale(wuecentered)+scale(BA5)+scale(dbh)+(1|site), tree_avgs)
summary(tree_ductgrow_mod1)
plot_model(tree_ductgrow_mod1, type="diag") #looks good
tab_model(tree_ductgrow_mod1, show.se=TRUE, show.stat=TRUE)
tree_ductgrow_mod2 <- lmer(totalresinarea~scale(growth12)*scale(cwd)+scale(BA5)+scale(dbh)+(1|site), tree_avgs)
summary(tree_ductgrow_mod2)
tab_model(tree_ductgrow_mod2, show.df=TRUE,show.se=TRUE, show.stat=T)

tree_ductgrow_mod1 <- lmer(totalresinarea~growth12*wuecentered+BA5+dbh+(1|site), tree_avgs)          
plot_model(tree_ductgrow_mod1, type="pred", term=c("growth12"))

### plotting #### 
ef_growth <- as.data.frame(effect(term =c("scale(growth12)"), mod=tree_ductgrow_mod1))
summary(ef_growth)

ggplot() + geom_point(data=tree_avgs, aes(growth12, totalresinarea, group=site, shape=site),
) + geom_point(data=ef_growth, aes(x=growth12, y=fit), color="black"
) +geom_line(data=ef_growth, aes(x=growth12, y=fit), color="black", linetype="F1"
) +geom_ribbon(data= ef_growth, aes(x=growth12, ymin=lower, ymax=upper), alpha= 0.3, fill="gray"
) +labs(shape="Site", y=expression("Avg Annual Total Resin Area ("*mm^2*")"), x="Avg Annual Growth (mm)"
) + scale_shape_manual(values=1:8) 


ggpredict(tree_ductgrow_mod1, terms = c("growth12"), type = "fixed") %>%
  plot() + geom_point(data=tree_avgs, aes(growth12, totalresinarea, group=site, shape=site),
  ) +labs(title="", shape="Site", y=expression("Average Annual Total Resin Area ("*mm^2*")"), x="Average Annual Growth (mm)"
  ) + scale_shape_manual(values=1:8) + theme_gray() 

ef_growth <-ggpredict(tree_ductgrow_mod1, terms = c("growth12"), type = "fixed")
ef_growth

tiff("resingrowth_avg.tiff", units="in", width=5, height=5, res=300)
ggplot(ef_growth, aes(x = x, y = predicted)) +
  geom_line(linetype="F1")+
  geom_ribbon( aes(ymin = conf.low, ymax = conf.high), alpha = .15) + geom_point(data=tree_avgs, aes(growth12, totalresinarea, group=site, shape=site),
  ) +labs(title="", shape="Site", y=expression("Average Annual Total Resin Area ("*mm^2*")"), x="Average Annual Growth (mm)"
  ) + scale_shape_manual(values=1:8) + theme_gray() 
dev.off()


min(all_data$cwd)
max(all_data$cwd)

# does this relationship change over time?
#### year ####

#year_ductgrow_mod <- lmer(meanductsize~growth12+wue+cwd+dbh+BA20+(1|site/tree), all_data)
year_ductgrow_mod <- lmer(log(totalresinarea+1)~scale(growth12)*scale(wuecentered)+scale(FDSI)+(1|site/tree), all_data)
summary(year_ductgrow_mod)
tab_model(year_ductgrow_mod,show.stat = TRUE, show.df = TRUE, show.se = TRUE)

plot_model(year_ductgrow_mod, type="diag")

year_ductgrow_mod1 <- lmer(log(totalresinarea+1)~scale(growth12)*scale(cwd)+scale(FDSI)+(1|site/tree), all_data)
summary(year_ductgrow_mod1)
tab_model(year_ductgrow_mod1, show.df=TRUE,show.se=TRUE, show.stat=TRUE)

#plotting ######

year_ductgrow_mod1 <- lmer(totalresinarea~scale(growth12)*scale(cwd)+scale(FDSI)+(1|site/tree), all_data)
summary(year_ductgrow_mod1)

ef_ductgrow <- as.data.frame(effect(term =c("growth12"), mod=year_ductgrow_mod1))
summary(ef_ductgrow)

ggplot() + geom_point(data=all_data, aes(growth12, log(totalresinarea+1), group=site, shape=site),
) + geom_point(data=ef_ductgrow, aes(x=growth12, y=fit), color="black"
) +geom_line(data=ef_ductgrow, aes(x=growth12, y=fit), color="black", linetype="F1"
) +geom_ribbon(data= ef_ductgrow, aes(x=growth12, ymin=lower, ymax=upper), alpha= 0.3, fill="gray"
) +labs(shape="Site", y=expression("Log of Annual Total Resin Area +1 ("*mm^2*")"), x="Annual Growth (mm)"
) + scale_shape_manual(values=1:8) 

ef_growth1 <-ggpredict(year_ductgrow_mod1, terms = c("growth12"), type = "fixed")
ef_growth1

tiff("resingrowth_annual.tiff", units="in", width=5, height=5, res=300)
ggplot(ef_growth1, aes(x = x, y = predicted)
)+geom_line(linetype="F1")+geom_ribbon( aes(ymin = conf.low, ymax = conf.high), alpha = .15) +labs(title="", shape="Site", y=expression("Annual Total Resin Area ("*mm^2*")"), x="Annual Growth (mm)"
) + geom_point(data=all_data, aes(growth12, totalresinarea, group=site, shape=site),
)+ scale_shape_manual(values=1:8) + theme_gray() 
dev.off()

#### DUCTS~CONES ###########################

ggplot(tree_avgs) +geom_point(aes(x=coneabundance, y=totalresinarea, col=site)
)+geom_smooth(aes(y=totalresinarea, x=coneabundance),method="lm", se=F
) + labs(y=expression("Avg Total Resin Area ("*mm^2*")"), x="Log of Avg Cone Abundance")

#checking distributions
hist(tree_avgs$totalresinarea) #might have to log transform
hist(log(all_data$coneabundance)) # cones good log transformed.

#individual 

tree_ductscone <- lmer(log(totalresinarea)~scale(coneabundance)*scale(wuecentered)+scale(dbh)+scale(BA5)+(1|site), tree_avgs) 
plot_model(tree_ductscone, type="diag") # not bad
summary(tree_ductscone)
tab_model(tree_ductscone, show.df=TRUE,show.se=TRUE, show.stat=TRUE)

#what about cwd?
tree_ductscone1 <- lmer(log(totalresinarea)~scale(coneabundance)*scale(cwd)+scale(dbh)+scale(BA5)+(1|site), tree_avgs) 
summary(tree_ductscone1)
tab_model(tree_ductscone1, show.df = TRUE, show.stat=TRUE, show.se=TRUE)

#plotting#####

tree_ductscone <- lmer(log(totalresinarea)~scale(coneabundance)*scale(wuecentered)+scale(dbh)+scale(BA5)+(1|site), tree_avgs) 

tree_ductwue <- lmer(log(totalresinarea)~wuecentered+scale(dbh)+scale(BA5)+(1|site), tree_avgs) 
plot_model(tree_ductscone, type="diag")

ef_ductcones <- as.data.frame(effect(term =c("scale(wuecentered)"), mod=tree_ductscone))
summary(ef_ductgrow)

tiff("resinwue.tiff", units="in", width=5, height=5, res=300)
ggplot() + geom_point(data=tree_avgs, aes(x= wuecentered, y=log(totalresinarea), group=site, shape=site),
) +geom_line(data=ef_ductcones, aes(x=wuecentered, y=fit), color="black", linetype="F1"
) +geom_ribbon(data= ef_ductcones, aes(x=wuecentered, ymin=lower, ymax=upper),alpha= 0.3
) +labs(shape="Site", y=expression("Log of Average Annual Total Resin Area ("*mm^2*")"), x=expression(Delta*"13C - Centered by Site")
) + scale_shape_manual(values=1:8) + xlim(-1, 1)
dev.off()


ef_wue <-ggpredict(tree_ductscone, terms = c("wuecentered"), type = "fixed")
ef_wue
tiff("resinwue.tiff", units="in", width=5, height=5, res=300)
ggplot(ef_wue, aes(x = x, y = predicted)
) + geom_line(linetype="F1"
)+geom_ribbon( aes(ymin = conf.low, ymax = conf.high), alpha = .15
) + geom_point(data=tree_avgs, aes(wuecentered, totalresinarea, group=site, shape=site),
)+labs(shape="Site", y=expression("Average Annual Total Resin Area ("*mm^2*")"), x=(expression(Delta*"13C - Centered by Site")
))+ scale_shape_manual(values=1:8) + theme_gray() 
dev.off()




####year####

year_ductscone <- lmer(log(totalresinarea+1)~scale(conematur)*scale(cwd)+scale(FDSI)+(1|site/tree), all_data) 
year_ductscone1 <- lmer(log(totalresinarea+1)~scale(conematur)*scale(wuecentered)+scale(FDSI)+(1|site/tree), all_data) 
summary(year_ductscone) 
tab_model(year_ductscone, show.df = TRUE, show.stat = TRUE, show.se = TRUE)
tab_model(year_ductscone1, show.df = TRUE, show.stat = TRUE, show.se = TRUE)

plot_model(year_ductscone, type="diag")

##plotting ####


ef_ductcone <- as.data.frame(effect(term =c("scale(conematur)"), mod=year_ductscone))
summary(ef_ductcone)

tiff("resincones.tiff", units="in", width=5, height=5, res=300)
ggplot() + geom_point(data=all_data, aes(conematur, log(totalresinarea+1), shape=site)
) +geom_line(data=ef_ductcone, aes(x=conematur, y=fit), color="black", linetype="F1"
) +geom_ribbon(data= ef_ductcone, aes(x=conematur, ymin=lower, ymax=upper), alpha= 0.3
) +labs(shape="Site", y=expression("Log of Annual Total Resin Area +1 ("*mm^2*")"), x="Annual Cone Abundance"
) + scale_shape_manual(values=1:8) 
dev.off()

ef_cone <-ggpredict(year_ductscone, terms = c("conematur"), type = "fixed")

tiff("resincones.tiff", units="in", width=5, height=5, res=300)
ggplot(ef_cone, aes(x = x, y = predicted)) +
  geom_line(linetype="F1")+
  geom_ribbon( aes(ymin = conf.low, ymax = conf.high), alpha = .15) + geom_point(data=all_data, aes(x=conematur, y=totalresinarea, group=site, shape=site),
  ) +labs(title="", shape="Site", y=expression("Annual Total Resin Area ("*mm^2*")"), x="Annual Cone Abundance"
  ) + scale_shape_manual(values=1:8) + theme_gray() 
dev.off()



#### GROWTH~CONES ####

###tree####

tree_conesgrowth <- lmer(log(growth12)~scale(coneabundance)*scale(wuecentered)+scale(dbh)+scale(BA5)+(1|site), tree_avgs)
summary(tree_conesgrowth)
plot_model(tree_conesgrowth, type="diag")
tab_model(tree_conesgrowth, show.df = TRUE, show.stat = TRUE, show.se = TRUE)
tree_conesgrowth1 <- lmer(log(growth12)~scale(coneabundance)*scale(cwd)+scale(dbh)+scale(BA5)+(1|site), tree_avgs)
summary(tree_conesgrowth1)
tab_model(tree_conesgrowth1, show.df = TRUE, show.stat = TRUE, show.se = TRUE)

#### year ####

yr_conegrow<- lmer(log(growth12+1)~scale(conematur)*scale(wuecentered)+scale(FDSI)+(1|site/tree), data=all_data)
summary(yr_conegrow)
tab_model(yr_conegrow, show.df = TRUE, show.stat = TRUE, show.se = TRUE)

yr_conegrow1<- lmer(log(growth12+1)~scale(conematur)*scale(cwd)+scale(FDSI)+(1|site/tree), data=all_data)
summary(yr_conegrow1)
tab_model(yr_conegrow1, show.df = TRUE, show.stat = TRUE, show.se = TRUE)


##plotting####

tiff("annual_growthcones.tiff", units="in", width=5, height=5, res=300)
ggplot(all_data) +geom_point(aes(x=conematur, y=log(growth12+1)))+ geom_smooth(aes(x=conematur, y=log(growth12+1)), color="black", linetype="dashed", method="lm") +labs(y="Log of Annual Growth +1 (mm)", x="Annual Cone Abundance")
dev.off()

all_data$CWD <- all_data$cwd 
yr_conegrow3<- lmer(growth12~conematur*CWD+FDSI+(1|site/tree), data=all_data)
tiff("growthcones_cwd.tiff", units="in", width=5, height=5, res=300)
plot_model(yr_conegrow3, type="int", mdrt.values = "meansd", title=c(""
),legend.title =c("Climatic Water Deficit"),axis.title=c("Cone Abundance", "Annual Growth (mm)"
), group.terms = c(1, 2, 3), colors = "bw", show.data=TRUE, dot.size=0.5
) 
dev.off()


yr_conegrow1<- lmer(growth12~scale(conematur)*scale(cwd)+FDSI+(1|site/tree), data=all_data)


#annual w/ cwd interaction

yr_conegrow1<- lmer(log(growth12+1)~scale(conematur)*scale(cwd)+scale(FDSI)+(1|site/tree), data=all_data)
plot_model(yr_conegrow1, type = "diag")
summary(yr_conegrow1)
# plot the fit
ef_conegrow_cwd <- as.data.frame(effect(term =c("scale(conematur):scale(cwd)"), mod=yr_conegrow1))
g_fit <- ggplot(data = all_data, aes(x = conematur, y = log(growth12+1), col = cwd)) +
  geom_point()
g_fit
g_fit + geom_line(data = ef_conegrow_cwd, aes(x = conematur, y = fit, col = cwd, group = cwd)
)

ggplot() + geom_point(data=all_data, aes(conematur, log(growth12+1))
) +geom_line(data=ef_conegrow_cwd, aes(x=conematur, y=fit, shape=cwd, group=cwd),
) +labs(shape="Site", y=expression("Log of Annual Total Resin Area +1 ("*mm^2*")"), x="Annual Cone Abundance"
) + scale_linetype_manual(values=c("solid", "dashed", "solid", "dashed", "solid"))


##### C13 Relationships #####
c13cone <- lmer(log(coneabundance)~scale(wuecentered)+(1|site), tree_avgs)
anova(c13cone)

plot_model(c13cone, type="diag")

c13grow <- lm(wuecentered~growth12+crownclass+scale(BA5), tree_avgs)
anova(c13grow)

c13duct <- lm(wuecentered~totalresinarea+crownclass+scale(BA5), tree_avgs)
anova(c13duct)

c13cone2 <- lm(wuecentered~coneabundance+crownclass+scale(BA5), tree_avgs)
anova(c13cone2)

#model w/out main traits
C13mod <- lm(wuecentered~height+dbh+BA5+crownclass+cwd, tree_avgs)
anova(C13mod)


# no trait measured predicts C13. Weird.
#based on reading, canopy height, tree size, and site climate are all big predictors
#of wue. I tried all iterations of this and found no correlation.

plot_model(C13mod, type="diag")

summary(C13mod)


#### STUDY MAP #####

library(sf)
library(sp)
library(rgdal)
library(maps)
#install.packages("mapdata")
#library(mapdata)
library(rnaturalearth)
library(rnaturalearthdata)
library("tools") ## this allows you to change a single value to upper case

dev.off()
site_location <- read.csv("site_locations.csv")
sites <- st_as_sf(site_location, coords = c("long", "lat"),crs = 4326, agr = "constant")
states <- st_as_sf(map('state', plot = FALSE, fill = TRUE))
sf_use_s2(FALSE) ### cbind code below gets error unless you put as false first
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)


#state map
# sf object created is using different crs and will need to match states df if wanting to use it with map
states$nudge_y <- 0
states$nudge_y[states$ID=="Wyoming"] <- 1.5
states$nudge_y[states$ID=="Colorado"] <- 0.25
states$nudge_x <- -1
states$nudge_x[states$ID == "Nebraska"] <- +2.25
states$nudge_x[states$ID == "Colorado"] <- -1

custom.col <- c("#873919","#d7815a","#c76030","#cc9b38","#90883b","#97b442","#5ca857","#45c097")
custom.col <- rev(custom.col)
#40.5853° N, 105.0844° W FOCO
#40.0150° N, 105.2705° W Boulder
#39.7392° N, 104.9903° W Denver
#38.8339° N, 104.8214° W springs

COcities <- data.frame(state = rep("Colorado", 4), city = c("Fort Collins", 
                                                            "Boulder", "Denver", "Colorado Springs"), lat = c(40.5853, 40.0150, 39.7392, 38.8339
                                                            ), long = c(-105.0844, -105.2705, -104.9903,-104.8214))


#install.packages("ggrepel") moves labels easily in ggplot
library(ggrepel) 
library(ggspatial)

studyarea <- ggplot(data = states) +geom_sf()+coord_sf(
  xlim = c(-110, -101), ylim = c(36, 42), expand=FALSE
)+geom_text(data = states, aes(X, Y, label = ID), size = 4.5, fontface = "bold", 
            nudge_x = states$nudge_x, nudge_y = states$nudge_y
) +geom_text_repel(data = COcities, aes(x = long, y = lat, label = city), 
                   ,size=c(2, 2, 3, 2), nudge_x= c(0.25, 0.5, 0.25, 0.25)
) +geom_jitter(data = site_location, aes(x=long, y=lat, fill=cwd, shape=cwd), size = 3.5, shape = 24,
               position = position_jitter(seed=2, width = 0.12, height = 0.1)) +labs(x="", y="")+ scale_fill_gradientn(colors=custom.col, name="CWD"
               ) +annotation_scale(location = "bl", width_hint = 0.5) +annotation_north_arrow(location = "bl", which_north = "true", 
                                                                                              pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),height = unit(1, "cm"),
                                                                                              width = unit(1, "cm"),style = north_arrow_fancy_orienteering) 
studyarea                                  



#+ scale_fill_gradientn(colors=custom.col, name="CWD"
?scale_shape_manual
#country map
colorado <- data.frame(long=c(-105.55251), lat=c(38.99797))

theme_set(theme_bw())
country <-ggplotGrob(ggplot(data = states) +geom_sf(size=.00001, color="black") + geom_point(data = colorado, aes(x = long, y = lat), size = 1, 
                                                                                             shape = 0, stroke=1)+labs(x="", y="")+theme(axis.text.x = element_blank(),
                                                                                                                                         axis.text.y = element_blank(),axis.ticks = element_blank(),rect = element_blank(), panel.background = element_rect(fill = NULL), panel.grid.major=element_line(colour = "transparent")) )

## theme portion removes lat long and will put white background 
#grob will make it inlet. Transparent will remove grid. 

tiff("studyareamap.tiff", units="in", width=5, height=5, res=300)
studyarea + annotation_custom(grob = country, xmin = -110, xmax = -106, ymin = 40, ymax = 42)
dev.off()

