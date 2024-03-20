# Situate yourself 
here::i_am(
  "code/02_make_figures.R"
)

# Load in the data
load("data/shape_zcta_restrict.RData")

# load in packages
library(sf)
library(tidyverse)
library(ggpubr)

# read in map
s19_g <- INLA::inla.read.graph(filename = "data/s19_adj")

# Create a model that depicts how transportation variables are affected by the 
# quantiled SES effect modifiers

# MODEL OUTCOME: Specified by User
# MODEL OFFSET: Crude Population

# create a function to include a stratified effect model
mainfigure_func <- function(Outcome, covar1, modifier1, modifier2){
  
  # covar1
  formula1 <- reformulate(termlabels = covar1, 
                          response = Outcome)
  model1 <- INLA::inla(update(formula1, .~.  +
                          f(Index, model = "besag", graph = s19_g, group=TimeIndex, 
                            control.group = list(model = "exchangeable")) +
                          f(TimeIndex)),
                 E = POPULATION, family = "poisson",
                 data = as.data.frame(shape_zcta_restrict),
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 control.predictor = list(compute = TRUE))
  smry1 <- summary(model1)
  
  # modifier1*covar1 - covar1
  formula2 <- reformulate(termlabels = c(paste0(modifier1,":", covar1), paste0("-",covar1)), 
                          response = Outcome)
  model2 <- INLA::inla(update(formula2, .~.  +
                          f(Index, model = "besag", graph = s19_g, group=TimeIndex, 
                            control.group = list(model = "exchangeable")) +
                          f(TimeIndex)),
                 E = POPULATION, family = "poisson",
                 data = as.data.frame(shape_zcta_restrict),
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 control.predictor = list(compute = TRUE))
  smry2 <- summary(model2)
  
  # modifier2*covar1 - covar1
  formula3 <- reformulate(termlabels = c(paste0(modifier2,":", covar1), paste0("-",covar1)), 
                          response = Outcome)
  model3 <- INLA::inla(update(formula3, .~.  +
                          f(Index, model = "besag", graph = s19_g, group=TimeIndex, 
                            control.group = list(model = "exchangeable")) +
                          f(TimeIndex)),
                 E = POPULATION, family = "poisson",
                 data = as.data.frame(shape_zcta_restrict),
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 control.predictor = list(compute = TRUE))
  smry3 <- summary(model3)
  
  # Create data frame with risk ratios of covar
  multivarInteraction_models <- data.frame(
    
    Covar_Coef_m1overall = exp(smry1$fixed[, "mean"][[2]]),
    Covar_Quant0.025_m1overall = exp(smry1$fixed[, "0.025quant"][[2]]),
    Covar_Quant0.975_m1overall = exp(smry1$fixed[, "0.975quant"][[2]]),
    
    Covar_Coef_m2int1 = exp(smry2$fixed[, "mean"][[2]]),
    Covar_Quant0.025_m2int1 = exp(smry2$fixed[, "0.025quant"][[2]]),
    Covar_Quant0.975_m2int1 = exp(smry2$fixed[, "0.975quant"][[2]]),
    
    Covar_Coef_m2int2 = exp(smry2$fixed[, "mean"][[3]]),
    Covar_Quant0.025_m2int2 = exp(smry2$fixed[, "0.025quant"][[3]]),
    Covar_Quant0.975_m2int2 = exp(smry2$fixed[, "0.975quant"][[3]]),
    
    Covar_Coef_m2int3 = exp(smry2$fixed[, "mean"][[4]]),
    Covar_Quant0.025_m2int3 = exp(smry2$fixed[, "0.025quant"][[4]]),
    Covar_Quant0.975_m2int3 = exp(smry2$fixed[, "0.975quant"][[4]]),
    
    Covar_Coef_m2int4 = exp(smry2$fixed[, "mean"][[5]]),
    Covar_Quant0.025_m2int4 = exp(smry2$fixed[, "0.025quant"][[5]]),
    Covar_Quant0.975_m2int4 = exp(smry2$fixed[, "0.975quant"][[5]]),
    
    Covar_Coef_m3int1 = exp(smry3$fixed[, "mean"][[2]]),
    Covar_Quant0.025_m3int1 = exp(smry3$fixed[, "0.025quant"][[2]]),
    Covar_Quant0.975_m3int1 = exp(smry3$fixed[, "0.975quant"][[2]]),
    
    Covar_Coef_m3int2 = exp(smry3$fixed[, "mean"][[3]]),
    Covar_Quant0.025_m3int2 = exp(smry3$fixed[, "0.025quant"][[3]]),
    Covar_Quant0.975_m3int2 = exp(smry3$fixed[, "0.975quant"][[3]]),
    
    Covar_Coef_m3int3 = exp(smry3$fixed[, "mean"][[4]]),
    Covar_Quant0.025_m3int3 = exp(smry3$fixed[, "0.025quant"][[4]]),
    Covar_Quant0.975_m3int3 = exp(smry3$fixed[, "0.975quant"][[4]]),
    
    Covar_Coef_m3int4 = exp(smry3$fixed[, "mean"][[5]]),
    Covar_Quant0.025_m3int4 = exp(smry3$fixed[, "0.025quant"][[5]]),
    Covar_Quant0.975_m3int4 = exp(smry3$fixed[, "0.975quant"][[5]]))
  
  # Pull out the overall estimates
  overall_Est <- multivarInteraction_models %>% select(c(1,2,3)) %>%
    mutate(Estimate= "Overall",
           Group= "Overall") %>%
    rename(Covar_Coef = Covar_Coef_m1overall,
           Covar_Quant0.025 = Covar_Quant0.025_m1overall,
           Covar_Quant0.975 = Covar_Quant0.975_m1overall)
  
  # Pull out the int1 estimates
  m2int1_Est <- multivarInteraction_models %>% select(c(4,5,6)) %>%
    mutate(Estimate= "Q1",
           Group= "% Poverty") %>%
    rename(Covar_Coef = Covar_Coef_m2int1,
           Covar_Quant0.025 = Covar_Quant0.025_m2int1,
           Covar_Quant0.975 = Covar_Quant0.975_m2int1)
  
  # Pull out the int2 estimates
  m2int2_Est <- multivarInteraction_models %>% select(c(7,8,9)) %>%
    mutate(Estimate= "Q2",
           Group= "% Poverty") %>%
    rename(Covar_Coef = Covar_Coef_m2int2,
           Covar_Quant0.025 = Covar_Quant0.025_m2int2,
           Covar_Quant0.975 = Covar_Quant0.975_m2int2)
  
  # Pull out the int3 estimates
  m2int3_Est <- multivarInteraction_models %>% select(c(10,11,12)) %>%
    mutate(Estimate= "Q3",
           Group= "% Poverty") %>%
    rename(Covar_Coef = Covar_Coef_m2int3,
           Covar_Quant0.025 = Covar_Quant0.025_m2int3,
           Covar_Quant0.975 = Covar_Quant0.975_m2int3)
  # Pull out the int4 estimates
  m2int4_Est <- multivarInteraction_models %>% select(c(13,14,15)) %>%
    mutate(Estimate= "Q4",
           Group= "% Poverty") %>%
    rename(Covar_Coef = Covar_Coef_m2int4,
           Covar_Quant0.025 = Covar_Quant0.025_m2int4,
           Covar_Quant0.975 = Covar_Quant0.975_m2int4)
  
  # Pull out the int1 estimates
  m3int1_Est <- multivarInteraction_models %>% select(c(16,17,18)) %>%
    mutate(Estimate= "Q1",
           Group= "% No Vehicle") %>%
    rename(Covar_Coef = Covar_Coef_m3int1,
           Covar_Quant0.025 = Covar_Quant0.025_m3int1,
           Covar_Quant0.975 = Covar_Quant0.975_m3int1)
  
  # Pull out the int2 estimates
  m3int2_Est <- multivarInteraction_models %>% select(c(19,20,21)) %>%
    mutate(Estimate= "Q2",
           Group= "% No Vehicle") %>%
    rename(Covar_Coef = Covar_Coef_m3int2,
           Covar_Quant0.025 = Covar_Quant0.025_m3int2,
           Covar_Quant0.975 = Covar_Quant0.975_m3int2)
  
  # Pull out the int3 estimates
  m3int3_Est <- multivarInteraction_models %>% select(c(22,23,24)) %>%
    mutate(Estimate= "Q3",
           Group= "% No Vehicle") %>%
    rename(Covar_Coef = Covar_Coef_m3int3,
           Covar_Quant0.025 = Covar_Quant0.025_m3int3,
           Covar_Quant0.975 = Covar_Quant0.975_m3int3)
  # Pull out the int4 estimates
  m3int4_Est <- multivarInteraction_models %>% select(c(25,26,27)) %>%
    mutate(Estimate= "Q4",
           Group= "% No Vehicle") %>%
    rename(Covar_Coef = Covar_Coef_m3int4,
           Covar_Quant0.025 = Covar_Quant0.025_m3int4,
           Covar_Quant0.975 = Covar_Quant0.975_m3int4)
  
  
  # Combine the Covariate Estimates into a long dataset
  multivar_models_long <- rbind(m2int1_Est, m2int2_Est, m2int3_Est, m2int4_Est,
                                m3int1_Est, m3int2_Est, m3int3_Est, m3int4_Est, overall_Est)
  
  # coerce as Model factor
  multivar_models_long$Estimate <- as.factor(multivar_models_long$Estimate)
  
  return(multivar_models_long)
  
}

# Generate Tables containing figures info
mainfig_disttrans <- mainfigure_func("MAJOR_MINOR_count_cln", "dist_to_transit_stop_scaled", "pct_poverty_scaled_cat",
                                     "pct_no_vehicle_scaled_cat")

mainfig_pcttransp <- mainfigure_func("MAJOR_MINOR_count_cln", "pct_work_pub_transp_scaled", "pct_poverty_scaled_cat",
                                     "pct_no_vehicle_scaled_cat")

mainfig_expense <- mainfigure_func("MAJOR_MINOR_count_cln", "pub_transp_per_capita_scaled", "pct_poverty_scaled_cat",
                                   "pct_no_vehicle_scaled_cat")

# create the first figure
disttrans_figfunc <- function(mainfig_disttrans, title, subtitle){
  
  # Create Y axis boundaries
  ylimmax <- max(mainfig_disttrans$Covar_Quant0.975)+.02
  ylimmin <- min(mainfig_disttrans$Covar_Quant0.025)-.02
  
  # Create color scheme for forest plots
  barColors <- c("#08519C","#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD")
  dotColors <- c("darkgray","darkgray", "darkgray", "darkgray", "darkgray","darkgray", "darkgray", "darkgray", "darkgray")
  
  # Create forest plot
  forestplot <- mainfig_disttrans %>%
    # Plot the forest plots
    ggplot(aes(x=interaction(Estimate, Group), y=Covar_Coef, ymin=Covar_Quant0.025,
               ymax=Covar_Quant0.975, col=Estimate,fill=Estimate)) +
    geom_linerange(size=4) +
    geom_hline(yintercept=1, lty=2) +
    geom_point(size=2, shape=21, colour="white", stroke = 0.5) +
    scale_fill_manual(values=dotColors)+
    scale_color_manual(values=barColors)+
    annotate(geom = "text", x = c(1:9), y = ylimmin-.04, label = mainfig_disttrans$Estimate, size = 2) +
    annotate(geom = "text", x = c(2.5,6.5), y = ylimmin-.25, label = c("% No Vehicle", "% Poverty"), size = 3) +
    coord_cartesian(ylim = c(ylimmin, ylimmax), expand = TRUE, clip = "off") +
    scale_y_continuous(name="Risk Ratio")+
    annotate("segment", x = .9, xend = 4.1, y = ylimmin-.15, yend = ylimmin-.15, size=.2)+
    annotate("segment", x = 4.9, xend = 8.1, y = ylimmin-.15, yend = ylimmin-.15, size=.2)+
    annotate("segment", x = .9, xend = .9, y = ylimmin-.15, yend = ylimmin-.1, size=.2)+
    annotate("segment", x = 4.1, xend = 4.1, y = ylimmin-.15, yend = ylimmin-.1, size=.2)+
    annotate("segment", x = 4.9, xend = 4.9, y = ylimmin-.15, yend = ylimmin-.1, size=.2)+
    annotate("segment", x = 8.1, xend = 8.1, y = ylimmin-.15, yend = ylimmin-.1, size=.2)+
    theme_minimal()+
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(1, 1, 4, 1), "lines"))
  
  # add a common title 
  print(annotate_figure(annotate_figure(forestplot, top=text_grob(paste(subtitle), size=14),), 
                        top = text_grob(paste(title), face = "bold", size=20)))
  
}
# Create an object for this figure
disttrans_finalfig <- disttrans_figfunc(mainfig_disttrans,"Distance to Transit Stop (m)",
                                        "on Major and Minor Amputation Outcomes")

# Save this figure as a png
ggsave(
  here::here("output/finalfig1.png"),
  plot = disttrans_finalfig,
  device = "png"
)

# create the second figure
pcttransp_figfunc <- function(mainfig_pcttransp, title, subtitle){
  
  # Create Y axis boundaries
  ylimmax <- max(mainfig_pcttransp$Covar_Quant0.975)+.3
  ylimmin <- min(mainfig_pcttransp$Covar_Quant0.025)-.3
  
  # Create color scheme for forest plots
  barColors <- c("#08519C","#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD")
  dotColors <- c("darkgray","darkgray", "darkgray", "darkgray", "darkgray","darkgray", "darkgray", "darkgray", "darkgray")
  
  # Create forest plot
  forestplot <- mainfig_pcttransp %>%
    # Plot the forest plots
    ggplot(aes(x=interaction(Estimate, Group, lex.order = FALSE), y=Covar_Coef, ymin=Covar_Quant0.025,
               ymax=Covar_Quant0.975, col=Estimate,fill=Estimate)) +
    geom_linerange(size=4) +
    geom_hline(yintercept=1, lty=2) +
    geom_point(size=2, shape=21, colour="white", stroke = 0.5) +
    scale_fill_manual(values=dotColors)+
    scale_color_manual(values=barColors)+
    annotate(geom = "text", x = c(1:9), y = ylimmin-.04, label = mainfig_pcttransp$Estimate, size = 2) +
    annotate(geom = "text", x = c(2.5,6.5), y = ylimmin-.7, label = c("% No Vehicle", "% Poverty"), size = 3) +
    coord_cartesian(ylim = c(ylimmin, ylimmax), expand = TRUE, clip = "off") +
    scale_y_continuous(name="Risk Ratio")+
    annotate("segment", x = .9, xend = 4.1, y = ylimmin-.4, yend = ylimmin-.4, size=.2)+
    annotate("segment", x = 4.9, xend = 8.1, y = ylimmin-.4, yend = ylimmin-.4, size=.2)+
    annotate("segment", x = .9, xend = .9, y = ylimmin-.4, yend = ylimmin-.3, size=.2)+
    annotate("segment", x = 4.1, xend = 4.1, y = ylimmin-.4, yend = ylimmin-.3, size=.2)+
    annotate("segment", x = 4.9, xend = 4.9, y = ylimmin-.4, yend = ylimmin-.3, size=.2)+
    annotate("segment", x = 8.1, xend = 8.1, y = ylimmin-.4, yend = ylimmin-.3, size=.2)+
    theme_minimal()+
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(1, 1, 4, 1), "lines"))
  
  # add a common title 
  print(annotate_figure(annotate_figure(forestplot, top=text_grob(paste(subtitle), size=14),), 
                        top = text_grob(paste(title), face = "bold", size=20)))
  
}

# Create an object for this figure
pcttransp_finalfig <- pcttransp_figfunc(mainfig_pcttransp,
                                        "% of Transportation to Work on Public Transportation",
                                        "on Major and Minor Amputation Outcomes")

# Save this figure as a png
ggsave(
  here::here("output/finalfig2.png"),
  plot = pcttransp_finalfig,
  device = "png"
)

# Create the third figure
expense_figfunc <- function(mainfig_expense, title, subtitle){
  
  # Create Y axis boundaries
  ylimmax <- max(mainfig_expense$Covar_Quant0.975)+.1
  ylimmin <- min(mainfig_expense$Covar_Quant0.025)-.02
  
  # Create color scheme for forest plots
  barColors <- c("#08519C","#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD")
  dotColors <- c("darkgray","darkgray", "darkgray", "darkgray", "darkgray","darkgray", "darkgray", "darkgray", "darkgray")
  
  # Create forest plot
  forestplot <- mainfig_expense %>%
    # Plot the forest plots
    ggplot(aes(x=interaction(Estimate, Group, lex.order = FALSE), y=Covar_Coef, ymin=Covar_Quant0.025,
               ymax=Covar_Quant0.975, col=Estimate,fill=Estimate)) +
    geom_linerange(size=4) +
    geom_hline(yintercept=1, lty=2) +
    geom_point(size=2, shape=21, colour="white", stroke = 0.5) +
    scale_fill_manual(values=dotColors)+
    scale_color_manual(values=barColors)+
    annotate(geom = "text", x = c(1:9), y = ylimmin-.03, label = mainfig_expense$Estimate, size = 2) +
    annotate(geom = "text", x = c(2.5,6.5), y = ylimmin-.08, label = c("% No Vehicle", "% Poverty"), size = 3) +
    coord_cartesian(ylim = c(ylimmin, ylimmax), expand = TRUE, clip = "off") +
    scale_y_continuous(name="Risk Ratio")+
    annotate("segment", x = .9, xend = 4.1, y = ylimmin-.06, yend = ylimmin-.06, size=.2)+
    annotate("segment", x = 4.9, xend = 8.1, y = ylimmin-.06, yend = ylimmin-.06, size=.2)+
    annotate("segment", x = .9, xend = .9, y = ylimmin-.06, yend = ylimmin-.05, size=.2)+
    annotate("segment", x = 4.1, xend = 4.1, y = ylimmin-.06, yend = ylimmin-.05, size=.2)+
    annotate("segment", x = 4.9, xend = 4.9, y = ylimmin-.06, yend = ylimmin-.05, size=.2)+
    annotate("segment", x = 8.1, xend = 8.1, y = ylimmin-.06, yend = ylimmin-.05, size=.2)+
    theme_minimal()+
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(1, 1, 4, 1), "lines"))
  
  # add a common title 
  print(annotate_figure(annotate_figure(forestplot, top=text_grob(paste(subtitle), size=14),), 
                        top = text_grob(paste(title), face = "bold", size=20)))
  
}

# Create an object for this figure
expense_finalfig <- expense_figfunc(mainfig_expense,
                                    "Per Capita Expense on Public Transportation",
                                    "on Major and Minor Amputation Outcomes")

# Save this figure as a png
ggsave(
  here::here("output/finalfig3.png"),
  plot = expense_finalfig,
  device = "png"
)
