##############
#Paper Title: 'Open-air preservation of miniaturised lithics: Experimental research in the
#Cederberg Mountains, Southern Africa'
#Authors: "Natasha Phillips, Justin Pargeter, Alex Mackay and Marika Low"
#R script production date: "December 4, 2017"
#Output: html_document
##############

# Required libraries


list.of.packages <-
  c(
    'ggplot2',
    'lme4',
    'sjstats',
    'gridExtra',
    'grid',
    'gamlss',
    'epitools',
    'compute.es',
    'circular',
    'perm',
    'fitdistrplus',
    'tidyr',
    'data.table',
    'glue'
  )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)


lapply(list.of.packages, require, character.only = TRUE)

#Plot theme used for majority of the paper plots

theme_great_plot <- function (bs = 12) {
  theme_bw(base_size = bs) %+replace%
    theme(
      axis.text = element_text(size = rel(0.8)),
      plot.title = element_text(
        hjust = 0.5,
        vjust = 1,
        size = rel(1.1),
        colour = "dodgerblue4"
      ),
      axis.title.x = element_text(colour = "dodgerblue4"),
      axis.title.y = element_text(colour = "dodgerblue4", angle = 90),
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold")
    )
}


############Archaeology/Experimental data comparisons############

######Section 2.1 experiment and archaeology core/flake size plots and comparisons######


###add experiment lithic measurement data from setup
#Set working directory to folder where R code and data files are stored

lithic_data <-
  read.csv("lithic_data.csv", 
           na.strings = c("NA", '<NA>'))
str(lithic_data)

#add event variable with which to merge and distinguish experiment and archaeology data sets

lithic_data$event <- 
  rep("experiment",
       length(nrow(lithic_data)))

#Create core/flake column

lithic_data$type <-
  ifelse (lithic_data$Class %in% c("freehand core", 
                                   "bipolar core"),
          'core',
          'flake')

### add archaeological lithic data

arc_data<-read.csv("arc_data.csv")

str(arc_data)

#rename archaeological size variables to match experimental size data

setnames(
  arc_data,
  old = c('site', 'max_length', 'max_width', 'max_thickness', 'mass'),
  new = c('event', 'Max.length', 'Max.width', 'Max.thick', 'Weight')
)

#merge experimental lithic and archaeological lithic size data sets, delete unused vars

combined_arc_exp_data<-merge(arc_data, lithic_data, by = c("event","type",'Max.length','Max.width','Max.thick','Weight'), all=T)

combined_arc_exp_data<-combined_arc_exp_data[,-c(7,11:13,15)]

##Core size

#Subset cores data

lithic_data_cores<-subset(combined_arc_exp_data, type %in% "core")

lithic_data_cores$bipolar <- as.factor(ifelse(is.na(lithic_data_cores$Class),
                                              as.character(lithic_data_cores$bipolar),
                                              as.character(lithic_data_cores$Class)))

#recode variable level names

levels(lithic_data_cores$bipolar)[levels(lithic_data_cores$bipolar)=="bipolar core"] <- "bipolar"
levels(lithic_data_cores$bipolar)[levels(lithic_data_cores$bipolar)=="freehand core"] <- "freehand"

#plot core sizes by 'event', generate ANOVA test results and cohen's F effect size statistic
createComparisonPlot<-function(df, y_var, y_label, graph_title){
      comparison_plot<-ggplot(df, 
                               aes_string(x="bipolar", y=y_var, fill = "event")) + 
            geom_boxplot(outlier.size = 0, outlier.colour = 'white')+
            scale_fill_grey(start = 0, end = .9) +
            theme_great_plot()
      
      comparison_plot<-comparison_plot + 
            geom_point(aes_string(y=y_var, group='event'), 
                       position = position_dodge(width=0.75),na.rm=TRUE)+
            labs(title = graph_title, x = "Technology", y = y_label)+
            scale_x_discrete(labels=c("Bipolar", "Freehand"))
      
      return(comparison_plot)
}
createComparisonPlot_flakes<-function(df, y_var, y_label, graph_title){
      comparison_plot<-ggplot(df, 
                              aes_string(x="event", y=y_var, fill = "event")) + 
            geom_boxplot(outlier.size = 0, outlier.colour = 'white')+
            scale_fill_grey(start = 0, end = .9) +
            theme_great_plot()
      
      comparison_plot<-comparison_plot + 
            geom_point(aes_string(y=y_var, group='event'), 
                       position = position_dodge(width=0.75),na.rm=TRUE)+
            labs(title = graph_title, x = "Assemblage", y = y_label)+
            scale_x_discrete(labels=c("Klipfonteinrand","Sehonghong","Experiment"))+ 
            guides(fill=FALSE)
      
      return(comparison_plot)
}
createAnova<-function(df, y_var){
      frm<-as.formula(sprintf("%s~%s", y_var, "event"))
      compare_aov<-aov(frm, data=df)
      return(compare_aov)
}
fullTests <- function(df, y_var, y_label, graph_title){
      compare_aov = createAnova(df, y_var)
      print(summary(compare_aov))
      print(cohens_f(compare_aov))
      return(createComparisonPlot(df, y_var, y_label, graph_title))
}
fullTests_flakes <- function(df, y_var, y_label, graph_title){
      compare_aov = createAnova(df, y_var)
      print(summary(compare_aov))
      print(cohens_f(compare_aov))
      return(createComparisonPlot_flakes(df, y_var, y_label, graph_title))
}

#common plot one legend
grid_arrange_shared_legend <- function(...) {
      plots <- list(...)
      g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
      legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
      lheight <- sum(legend$height)
      grid.arrange(
            do.call(arrangeGrob, lapply(plots, function(x)
                  x + theme(legend.position="none"))),
            legend,
            ncol = 1,
            heights = unit.c(unit(1, "npc") - lheight, lheight))
}

#plot core size by 'event', generate ANOVA test results and cohen's F effect size statistic
#Table 1

core_plot_length <- fullTests(lithic_data_cores, y_var = 'Max.length', y_label = "Max Length (mm)", 
          graph_title = "Core Length (mm) and Technology")

core_plot_width <- fullTests(lithic_data_cores, y_var = 'Max.width', y_label = "Max Width (mm)", 
          graph_title = "Core Width (mm) and Technology")

core_plot_weight <- fullTests(lithic_data_cores, y_var = 'log(Weight)', y_label = "Log Core Weight (g)", 
          graph_title = "Log Core Weight (g) and Technology")

#Figure 7 combined plot
grid_arrange_shared_legend(core_plot_length,core_plot_width, core_plot_weight)

##Flake size

#Subset flakes
lithic_data_flakes<-subset(combined_arc_exp_data, type %in% "flake")

#plot flake length by 'event', generate ANOVA test results and cohen's F effect size statistic
#Table 2

flake_plot_length <- fullTests_flakes(lithic_data_flakes, y_var = 'Max.length', y_label = "Max Length (mm)", 
                              graph_title = "Flake Length (mm) and Technology")

flake_plot_width <- fullTests_flakes(lithic_data_flakes, y_var = 'Max.width', y_label = "Max Width (mm)", 
                             graph_title = "Flake Width (mm) and Technology")

flake_plot_weight <- fullTests_flakes(lithic_data_flakes, y_var = 'log(Weight)', y_label = "Log Flake Weight (g)", 
                              graph_title = "Log Flake Weight (g) and Technology")

flake_plot_thickness <- fullTests_flakes(lithic_data_flakes, y_var = 'Max.thick', 
                                  y_label = "Max Thickness (mm)", 
                                  graph_title = "Max Thickness (mm) and Technology")

#Figure 8
source("http://peterhaschke.com/Code/multiplot.R") #multiplot source code

multiplot(flake_plot_length,flake_plot_thickness, flake_plot_width, flake_plot_weight, cols = 2)

############Experimental data############

##Data preparation

# Load total station data (update to latest file version)
experiment_data <- read.csv("experiment_data.csv",na.strings = c("NA","", '<NA>'))

# calculate lithic shape values
experiment_data$elongation <- with(experiment_data, (`Max.width` / `Max.length`))
experiment_data$flatness <- with(experiment_data, (`Max.thick` / `Max.width`))
experiment_data$form <- with(experiment_data, ( flatness / elongation))

# rename and compute combined shape variables
experiment_data$volume <- with(experiment_data, (`Max.length` * `Max.width` * `Max.thick`))

#log transform variables
experiment_data$volume[experiment_data$volume == 0] <- 0.01 #add constant to 0 values to prevent inf output

experiment_logdata<-experiment_data

myvars<-c("Weight","Max.length","Max.width","Max.thick","elongation","flatness","form","volume")
experiment_logdata[myvars]<-lapply(experiment_logdata[myvars], log)

######Section 3.2.1 Lithic size and shape as predictors of lithic movement########

##Series 1-2 and 2-3 recording data: includes cores and flakes

##divide data into different recording series and recording series combinations for use in models

series_2<-subset(experiment_logdata, series %in% "2")

series_3<-subset(experiment_logdata, series %in% "3")

series_2_3<-subset(experiment_logdata, series %in% c("2","3"))
series_2_3$class<-factor(series_2_3$class)
series_2_3$slope<-factor(series_2_3$slope)
series_2_3$climate<-factor(series_2_3$climate)
series_2_3$series<-factor(series_2_3$series)

series_2_3_4<-subset(experiment_logdata, series %in% c("2","3","4"))
levels(series_2_3_4$class)[levels(series_2_3_4$class %in% c('F','f'))] <- NA

series_2_3_4$class<-factor(series_2_3_4$class)
series_2_3_4$slope<-factor(series_2_3_4$slope)
series_2_3_4$series<-factor(series_2_3_4$series)

series_2_3_4<-subset(series_2_3_4, class %in% c("cb", "cf")) #subsets into bipolar (cb) and freehand (cf) cores

#plot predictor variables to assess data distributions
#Figure 12

p1<-qplot(experiment_logdata$Weight,geom="histogram",binwidth = 0.5, xlab = "Weight",  fill=I("black"), col=I("white"))
p2<-qplot(experiment_logdata$Max.length,geom="histogram",binwidth = 0.5, xlab = "Length",  fill=I("black"), col=I("white"))
p3<-qplot(experiment_logdata$Max.width,geom="histogram",binwidth = 0.5, xlab = "Width",  fill=I("black"), col=I("white"))
p4<-qplot(experiment_logdata$Max.thick,geom="histogram",binwidth = 0.5, xlab = "Thickness",  fill=I("black"), col=I("white"))
p5<-qplot(experiment_logdata$elongation,geom="histogram",binwidth = 0.5, xlab = "Elongation",  fill=I("black"), col=I("white"))
p6<-qplot(experiment_logdata$flatness,geom="histogram",binwidth = 0.5, xlab = "Flatness",  fill=I("black"), col=I("white"))
p7<-qplot(experiment_logdata$volume,geom="histogram",binwidth = 0.5, xlab = "Volume",  fill=I("black"), col=I("white"))
p8<-qplot(experiment_data$near_dist_m,geom="histogram",binwidth = 0.5, xlab = "Horizontal movement",  fill=I("black"), col=I("white"))

multiplot(p1, p2, p3, p4, p5, p6, p7, p8, cols = 2)

#check response variable (movement)  against theoretical distributions for best fit
#Figure S1

#generate hypothetical distribution using movement dataset parameters

control <- as.numeric(na.omit(experiment_data$near_dist_m))

## estimate the parameters as fit to weibull, exponential, log normal, and gamma distributions

fit1 <- fitdist(control, "weibull")
fit2 <- fitdist(control, "exp")
fit3 <- fitdist(control, "lnorm")
fit4 <- fitdist(control, "gamma")

#model diagnostics plot

par(mfrow=c(2,2))
plot.legend <- c("Weibull","exp", "lognormal", "gamma")
denscomp(list(fit1, fit2, fit3, fit4), legendtext = plot.legend)
cdfcomp (list(fit1, fit2, fit3, fit4), legendtext = plot.legend)
qqcomp  (list(fit1, fit2, fit3, fit4), legendtext = plot.legend)
ppcomp  (list(fit1, fit2, fit3, fit4), legendtext = plot.legend)

##Series 2 tool movement modeled against tool size

#use generalize linear model framework
#model tool movement as Weibull distribution

#Table 4

series_2_size_gamlss1 = gamlss(
      near_dist_m ~ Weight+Max.length+Max.width+Max.thick,
      sigma.formula = ~ Weight+Max.length+Max.width+Max.thick,
      data = na.omit(series_2),
      family = WEI
)

#model summary 
#Table 4

summary(series_2_size_gamlss1)

#model diagnostic plot 
#Figure S2

newpar<-par(mfrow=c(2,2), mar=par("mar")+c(0,1,0,0),                        
            col.axis="blue4",col="blue4",col.main="blue4",col.lab="blue4",
            pch="+",
            cex=.45,cex.lab=1.2, cex.axis=1, cex.main=1.2 )

plot(series_2_size_gamlss1, par=newpar)

##Series 2 tool movement modeled against tool shape

#use generalize linear model framework
#model tool movement as Weibull distribution

#Table 4

series_2_shape_gamlss1 = gamlss(
      near_dist_m ~ volume+elongation+flatness,
      sigma.formula = ~ volume+elongation+flatness,
      data = na.omit(series_2),
      family = WEI
)

#model summary 
#Table 4

summary(series_2_shape_gamlss1)

#model diagnostic plot 
#Figure S3

plot(series_2_shape_gamlss1, par=newpar) #plot model diagnostics

##Model comparisons using AIC criteria 
#Table 6

AIC(series_2_shape_gamlss1, series_2_size_gamlss1)

##Series 3 tool movement modeled against tool size

#use generalize linear model framework
#model tool movement as Weibull distribution

#Table 5

series_3_size_gamlss1 = gamlss(
      near_dist_m ~ Weight+Max.length+Max.width+Max.thick,
      sigma.formula = ~ Weight+Max.length+Max.width+Max.thick,
      data = na.omit(series_3),
      family = WEI
)

#Model summary 
#Table 5

summary(series_3_size_gamlss1)

#Model diagnostic plot 
#Figure S4

plot(series_3_size_gamlss1, par=newpar)

##Series 3 tool movement modeled against tool size

#use generalize linear model framework
#model tool movement as Weibull distribution

#Table 5

series_3_shape_gamlss1 = gamlss(
      near_dist_m ~ volume+elongation+flatness,
      sigma.formula = ~ volume+elongation+flatness,
      data = na.omit(series_3),
      family = WEI
)

#Model summary 
#Table 5

summary(series_3_shape_gamlss1)

#Model diagnostic plot 
#Figure S5

plot(series_3_shape_gamlss1, par=newpar)

##Model comparisons using AIC criteria 
#Table 6

AIC(series_3_shape_gamlss1, series_3_size_gamlss1)

######Section 3.2.2. Lithic class, slope, and climate as predictors of lithic movement######

##Series 1-2 and 2-3 data cores and flakes

#Function to plot comparisons of tool movement by class, slope, and climate

createMovementPlot<-function(df, ylabel, graph_title, facet_wrap1, facet_wrap2){
      
      comparison_plot<-ggplot(data=subset(df, !is.na(slope)), 
                              aes(x=class, y=log(near_dist_m))) + 
            geom_boxplot(outlier.size = 0, outlier.colour = 'white')+
            facet_wrap(as.formula(paste("~", facet_wrap1, "+", facet_wrap2)))+
            theme_great_plot()

      comparison_plot<-comparison_plot + geom_jitter(shape=16, size=3, position=position_jitter(0.2))+
            labs(title = graph_title)+
            ylab(ylabel)+
            scale_x_discrete(labels=c("Bipolar core", "Freehand core", "Flake"))

      return(comparison_plot)
}

#Plot tool movement by series, class and slope
#Figure 10

createMovementPlot(series_2_3,
                   "Log tool movement",
                   "Series 2 and 3 tool movement by slope, series, and tool class",
                   'slope',
                   'series')

#ANOVA model: tool movement, series, class, slope and their interactions
#Table 7

model_10<-aov(log(series_2_3$near_dist_m)~
                    series_2_3$series+
                    series_2_3$class+
                    series_2_3$slope*series_2_3$climate+
                    series_2_3$class*series_2_3$series+
                    series_2_3$class*series_2_3$slope+ 
                    series_2_3$series*series_2_3$slope)

#model summary 
#Table 7

summary(model_10)

#ANOVA effect size 
#Table 7

cohens_f(model_10)

##Series 1-2, 2-3, and 3-4 data cores ONLY

#Plot tool movement by slope, series and tool class
#Figure 11

createMovementPlot(series_2_3_4,
                   "Log tool movement",
                   "Series 2, 3 and 4 tool movement by slope, series, and class",
                   'slope',
                   'series')

#ANOVA model: tool movement, climate, series, class and their interactions
#Table 8

model_11<-aov(log(series_2_3_4$near_dist_m)~series_2_3_4$climate+
                    series_2_3_4$series + 
                    series_2_3_4$class + 
                    series_2_3_4$series * series_2_3_4$slope + 
                    series_2_3_4$series * series_2_3_4$class)

#model summary 
#Table 8

summary(model_11)

#ANOVA effect size 
#Table 8

cohens_f(model_11) 

######Section 3.2.3. Lithic orientation######

#subset data for analysis and plotting

direction_data<-experiment_data
direction_data$series<-as.factor(direction_data$series)
direction_data$class<-factor(direction_data$class)

direction_data_2  <- direction_data[direction_data$series == "2",]
direction_data_2$series<-as.factor(direction_data_2$series)
direction_data_2$Identity <- seq.int(nrow(direction_data_2))

direction_data_3  <- direction_data[direction_data$series == "3",]
direction_data_3$series<-as.factor(direction_data_3$series)
direction_data_3$Identity <- seq.int(nrow(direction_data_3))

#Permutation tests for difference in means of tool orientation by series, slope, and class
#Permutation test function

create_perm_test<-function(df, y_var, x_var){
      frm<-as.formula(sprintf("%s~%s", y_var, x_var))
      perm_test<-permKS(frm, 
             data = direction_data, 
             method = 'exact.mc',
             control = permControl(nmc = 10^4))
      return(perm_test)
}

#Permutation test results 
#Table 9

create_perm_test(direction_data,"bearing_TN","series")
create_perm_test(direction_data,"bearing_TN","slope")
create_perm_test(direction_data,"bearing_TN","class")

##Series 2 orientation data by slope
#Subset series 2 and 3 north and south slope data

direction_data_2_north<-subset(direction_data_2, slope %in% 'north')
direction_data_2_south<-subset(direction_data_2, slope %in% 'south')

direction_data_3_north<-subset(direction_data_3, slope %in% 'north')
direction_data_3_south<-subset(direction_data_3, slope %in% 'south')

#Function to plot rose diagrams

create_rose_diagram<-function(df, xvar, graph_title){
      
      rose_diagram<-ggplot() +
            geom_histogram(data=df,aes_string(x = xvar), 
                           colour = "black", 
                           fill = "grey80") +
            theme(axis.text.x = element_text(size = 18)) +
            coord_polar(start = 0) +
            scale_x_continuous(limits = c(0, 360), breaks = c(0,45,90,135,180,225,270,315,360)) +
            theme_minimal(base_size = 14) +
            xlab("") +
            ggtitle(graph_title)+
            theme(plot.title = element_text(hjust = 0.5))
      
      return(rose_diagram)
}

#Figure-13: data added manually to arefact map

rose_north_2<-create_rose_diagram(direction_data_2_north,"bearing_TN","Series 2 tool orientation north slope")
rose_south_2<-create_rose_diagram(direction_data_2_south,"bearing_TN","Series 2 tool orientation south slope")
rose_north_3<-create_rose_diagram(direction_data_3_north,"bearing_TN","Series 3 tool orientation north slope")
rose_south_3<-create_rose_diagram(direction_data_3_south,"bearing_TN","Series 3 tool orientation south slope")

multiplot(rose_north_2,rose_south_2, rose_north_3, rose_south_3, cols = 2)

#test tool orientation data against random
#No table #, data discussed in text

rayleigh.test(direction_data_2$bearing_TN)
rayleigh.test(direction_data_3$bearing_TN)

######Section 3.2.4. Lithic attrition######

#Subset series 1, 2 and 3 data

series_1_2_3<-subset(experiment_logdata, series %in% c("1","2","3"))
series_1_2_3$class<-factor(series_1_2_3$class)

#Subset cores data (for series 1,2,3 and 4)

series_cores<-subset(experiment_logdata, class %in% c("cb","cf"))
series_cores$class<-factor(series_cores$class)
series_cores$class[series_cores$class==c("f","F")] <- NA

#Plot tool counts function

tool_count_by_series_plot<-function(df){
      ggplot(data=df, 
             aes(x=Series, y=Number, fill=Class)) +
            geom_bar(stat="identity", position=position_dodge(),color="black")+
            theme_great_plot()+
            labs(title = "tool counts by series", x = "Series", y = "tool count")+
            #theme(plot.title = element_text(hjust = 0.5),text = element_text(size=20))+
            scale_x_discrete(labels=c("One","Two", "Three", "Four"))+
            scale_fill_manual(values=c('black','grey','lightgray'),
                              name='',labels=c("Bipolar core", "Freehand core", "Flake"))+
            geom_text(aes(label=Number), vjust=-0.3, color="black", size=3.5,
                      position = position_dodge(0.9))
}

##Series 1,2 and 3 core and flake attrition

series_2_3_loss_tbl = table(series_1_2_3$class, series_1_2_3$series)

#Chi-square on core and flake counts across recording series 
#Table 10

chi_sq_2_3<-chisq.test(series_2_3_loss_tbl)

#effect size 

chies(chi_sq_2_3$statistic, 1193) #chi-square value and sample size

#Extract odds ratio and 95 % CI 
#Table 11

odd_1<-oddsratio.wald(series_2_3_loss_tbl)
odd_1$measure

#Figure 14

series_2_3_loss = data.frame(t(series_2_3_loss_tbl))
colnames(series_2_3_loss)<-c('Series','Class','Number')
tool_loss_series2_3<-tool_count_by_series_plot(series_2_3_loss)

##Series 1,2,3 and 4 core attrition

series_2_3_4_cores_loss_tbl = table(series_cores$class, series_cores$series)

series_2_3_4_cores_loss = data.frame(t(series_2_3_4_cores_loss_tbl))
colnames(series_2_3_4_cores_loss)<-c('Series','Class','Number')

#Chi-square on core counts across recording series 
#Table 10

chi_sq_2_3_4<-chisq.test(series_2_3_4_cores_loss_tbl)

#effect size 

chies(chi_sq_2_3_4$statistic, 1193) #chi-square value and sample size

#Extract odds ratio and 95 % CI 
#Table 11

odd_2<-oddsratio.wald(series_2_3_4_cores_loss_tbl)
odd_2$measure

#Figure 15

tool_loss_series2_3_4<-tool_count_by_series_plot(series_2_3_4_cores_loss)

######Section 3.2.5. Simulating lithic movement and loss over longer time scales######

##Code set based off of work by Marwick and colleagues (2017), 'Movement of lithics by trampling: 
#An experiment in the Madjedbebe sediments, northern Australia' 
#doi.org/10.1016/j.jas.2017.01.008

#Set the font size for the plots
base_size <- 8

#Load data

tool_event_coords_subset <- read.csv("tool_three_event_movement.csv")

#Combine distance recording measures for series 1 and 2

all_distances <- 
      with(tool_event_coords_subset, c(near_dist_m.1, 
                                           near_dist_m.2,
                                           near_dist_m.3))

###Simulation tool surface movement using "tool_three_event_movement.csv" dataset

move_the_tool <- function(tool_num,
                              tool_event_coords_subset,
                              n){
      mydf <- tool_event_coords_subset
      
      # select tool
      df_tool <- mydf[mydf$Identity == tool_num, ]
      
      # starting location 
      before_simulation <- dplyr::select(df_tool, 
                                         near_dist_m.1)
      
      # movement differences that we can resample from 
      series2 <- dplyr::select(df_tool, 
                               near_dist_m.2)
      
      series3 <- dplyr::select(df_tool, 
                               near_dist_m.3)
      
      # put differences into list to use easily
      # pool all tool distances, without respect to the 
      # distances associated with this specific tool
      # and draw randomly from all distances for all tools
      diff_list <- as.list(all_distances)
      
      tool_coords_simulated <- 
            vector("list", n)
      
      for(i in 1:n){
            
            # to index the previous event
            j <- i - 1
            
            # if this is the first move, start at 'before' 
            # otherwise start at the previous location
            
            if(i == 1){
                  starting_location <- before_simulation
            }else{
                  starting_location <- tool_coords_simulated[[j]]
            }
            
            # get coords for new location of the tool after
            # this event by randomly sampling for the diffs
            # for the tool that we observed during the experiment
            if(i == 1){
                  # if this is the first move, use the starting point coords
                  tool_coords_simulated[[1]] <-  starting_location
            }else{
                  
                  # get a random draw from the observed coords for this tool
                  random_draw <- diff_list[[sample(length(diff_list), 1)]]
                  # Model Version 2: randomly flip the sign of the movement value
                  # Un-hash the code line below to use it
                  # random_draw <- sample(c(random_draw, -random_draw), 1)
                  
                  #print("unaltered random draw")
                  #print(random_draw[,3])
                  
                  
                  # apply random draw from observed coords to this tool
                  tool_coords_simulated[[i]] <- 
                        starting_location + random_draw
                  
                  #print(i)
            }
            
      }
      
      # make into data frame
      tool_coords_simulated_df <- 
            do.call(rbind, tool_coords_simulated)
      
      tool_coords_simulated_df$tool_num <- tool_num
      tool_coords_simulated_df$event <- 1:nrow(tool_coords_simulated_df)
      
      names(tool_coords_simulated_df) <- 
            c("Distance", 
              "Identity",
              "Event")
      
      return(tool_coords_simulated_df)
      
}

move_the_tool(1, tool_event_coords_subset, 20)

# functions to move ALL the tools n many times and create plot

move_all_the_tools <- function(n, tool_event_coords_subset){
      
      # for each tool, move it n times
      
      how_many_tools <- nrow(tool_event_coords_subset)
      
      # store the output
      all_the_tools <- vector("list", how_many_tools)
      
      for(i in 1:380){
            all_the_tools[[i]] <- 
                  move_the_tool(tool_num = i, 
                                    tool_event_coords_subset, 
                                    n = n)
      }
      
      # make into data frame
      all_the_tools_df <- 
            do.call(rbind, all_the_tools)
      
      return(all_the_tools_df)
      
}
run_simulation_plot<-function(n, tool_event_coords_subset){
      # n = how many times to move each tool
      moved <- 
            move_all_the_tools(n = n, 
                                   tool_event_coords_subset)
      
      # Get the axis limits of the plot
      # range of hor movement
      rnge_n <- max(moved$Distance, na.rm = TRUE) - min(moved$Distance, na.rm = TRUE)
      rngs <- rnge_n
      max_extent <- round(rngs[which.max(rngs)], 3)
      
      # hor xmin, ymin, etc. 
      const <- 1.2
      xmin <- median(moved$Distance, na.rm = TRUE) - max_extent/const
      xmax <- median(moved$Distance, na.rm = TRUE) + max_extent/const
      
      # movement over time
      # distribution of hordist at event n
      moved_n <- moved[moved$Event == n,]
      moved_n$hdist <- 
            sqrt(
                  (tool_event_coords_subset$near_dist_m.1 - moved_n$Distance)^2
            )
      
      #How many tools beyond 5 m boundary?
      print(paste("Number beyong 5m boundary:",length(which(moved_n$hdist > 5))))
      
      # distribution of horizontal distance of all lithics at position of last event
      
      moved_plot <- 
            ggplot(moved_n,
                   aes(hdist)) +
            geom_histogram(bins = 20) +
            labs(title = glue('Horizontal distance from starting point after {prettyNum(n, big.mark = ",")} events'), x = "Distance (m)", y = "Count") +
            theme_great_plot(base_size) +
            geom_vline(xintercept=5, color = 'red')
}

sim_10 = run_simulation_plot(10, tool_event_coords_subset)
sim_100 = run_simulation_plot(100, tool_event_coords_subset)
sim_1000 = run_simulation_plot(1000, tool_event_coords_subset)
sim_10000 = run_simulation_plot(10000, tool_event_coords_subset)

grid.arrange(sim_10,sim_100, sim_1000, sim_10000, 
             ncol=2)

