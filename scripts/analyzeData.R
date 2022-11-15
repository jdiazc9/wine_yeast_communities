rm(list = ls())
library(reshape2)
library(scales)
save_plots <- T

# load latest version of ecoFunctions
source('../scripts/ecoFunctions.R')

# load data
data <- read.csv('../data/functional_effects_final.csv')
#data <- data[!(data$background == 'Ap,Pk,Sc5,Sp,Zb' & data$knock_in == 'Ku'), ] # is this an outlier?
data$knock_in <- factor(data$knock_in,
                        levels = c('Sc5', 'Sc8',
                                   'Sp', 'Lt', 'Zb',
                                   'Td', 'Ku', 'Hop',
                                   'Wa', 'Mp', 'Pk', 'Ap'))

# plot FEEs of Sc5 and Sc8
ggplot(data[data$knock_in %in% c('Sc5', 'Sc8'), ],
       aes(x = background_fun.mean, y = delta_fun.mean,
           xmin = background_fun.mean - background_fun.sd, xmax = background_fun.mean + background_fun.sd,
           ymin = delta_fun.mean - delta_fun.sd, ymax = delta_fun.mean + delta_fun.sd)) +
  geom_abline(slope = 0, intercept = 0,
              color = '#d1d3d4') +
  geom_abline(slope = -1, intercept = 1,
              color = '#d1d3d4',
              linetype = 'dashed') +
  geom_point(color = 'black',
             cex = 1.5,
             shape = 16) +
  geom_errorbar(alpha = 0.25) +
  geom_errorbarh(alpha = 0.25) +
  geom_smooth(method = 'lm',
              formula = y~x,
              color = 'firebrick1',
              se = F,
              fullrange = T) +
  facet_wrap(~ knock_in) +
  scale_x_continuous(name = 'F (background)',
                     limits = c(0, 1),
                     breaks = pretty_breaks(n = 3)) +
  scale_y_continuous(name = 'dF',
                     breaks = pretty_breaks(n = 3)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16, face = 'italic'),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.42, 0.9),
        aspect.ratio = 0.6) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size=0.5) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)

if (save_plots) {
  ggsave(filename = '../plots/sugars_consumption_scFEEs.pdf',
         device = 'pdf',
         dpi = 600,
         width = 200,
         height = 80,
         units = 'mm',
         limitsize = F)
}

# plot every other FEE
ggplot(data[!(data$knock_in %in% c('Sc5', 'Sc8')), ],
       aes(x = background_fun.mean, y = delta_fun.mean,
           xmin = background_fun.mean - background_fun.sd, xmax = background_fun.mean + background_fun.sd,
           ymin = delta_fun.mean - delta_fun.sd, ymax = delta_fun.mean + delta_fun.sd)) +
  geom_abline(slope = 0, intercept = 0,
              color = '#d1d3d4') +
  geom_abline(slope = -1, intercept = 1,
              color = '#d1d3d4',
              linetype = 'dashed') +
  geom_point(color = 'black',
             cex = 1.5,
             shape = 16) +
  geom_errorbar(alpha = 0.25) +
  geom_errorbarh(alpha = 0.25) +
  geom_smooth(method = 'lm',
              formula = y~x,
              color = 'firebrick1',
              se = F,
              fullrange = T) +
  facet_wrap(~ knock_in,
             nrow = 2) +
  scale_x_continuous(name = 'F (background)',
                     limits = c(0, 1),
                     breaks = c(0, 0.5, 1),
                     labels = c('0', '0.5', '1')) +
  scale_y_continuous(name = 'dF',
                     breaks = c(0, 0.5, 1),
                     labels = c('0', '0.5', '1')) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16, face = 'italic'),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.42, 0.9),
        aspect.ratio = 0.6) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size=0.5) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)

if (save_plots) {
  ggsave(filename = '../plots/sugars_consumption_FEEs.pdf',
         device = 'pdf',
         dpi = 600,
         width = 200,
         height = 80,
         units = 'mm',
         limitsize = F)
}

# plot every other fee, split backgrounds by presence/absence of Sc
plot_this <- data[!(data$knock_in %in% c('Sc5', 'Sc8')), ]
plot_this$Sc <- c('No', 'Yes')[1 + grepl('Sc', plot_this$background)]
plot_this$Sc <- factor(plot_this$Sc, levels = c('Yes', 'No'))
ggplot(plot_this,
       aes(x = background_fun.mean, y = delta_fun.mean, color = Sc,
           xmin = background_fun.mean - background_fun.sd, xmax = background_fun.mean + background_fun.sd,
           ymin = delta_fun.mean - delta_fun.sd, ymax = delta_fun.mean + delta_fun.sd)) +
  geom_abline(slope = 0, intercept = 0,
              color = '#d1d3d4') +
  geom_abline(slope = -1, intercept = 1,
              color = '#d1d3d4',
              linetype = 'dashed') +
  geom_point(cex = 1.5,
             shape = 16) +
  geom_errorbar(alpha = 0.25) +
  geom_errorbarh(alpha = 0.25) +
  geom_smooth(method = 'lm',
              formula = y~x,
              color = 'firebrick1',
              se = F,
              fullrange = T) +
  facet_wrap(~ knock_in,
             nrow = 2) +
  scale_x_continuous(name = 'F (background)',
                     limits = c(0, 1),
                     breaks = pretty_breaks(n = 3)) +
  scale_y_continuous(name = 'dF',
                     breaks = pretty_breaks(n = 2)) +
  scale_color_manual(name = expression(paste(italic(Sc), ' in background?')),
                     values = c('deepskyblue', 'gray')) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16, face = 'italic'),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        aspect.ratio = 0.6) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size=0.5) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)

if (save_plots) {
  ggsave(filename = '../plots/sugars_consumption_FEEs_split-by-Sc.pdf',
         device = 'pdf',
         dpi = 600,
         width = 230,
         height = 80,
         units = 'mm',
         limitsize = F)
}

# plot only backgrounds that do contain Sc
plot_this <- plot_this[plot_this$Sc == 'Yes', ]
ggplot(plot_this,
       aes(x = background_fun.mean, y = delta_fun.mean,
           xmin = background_fun.mean - background_fun.sd, xmax = background_fun.mean + background_fun.sd,
           ymin = delta_fun.mean - delta_fun.sd, ymax = delta_fun.mean + delta_fun.sd)) +
  geom_abline(slope = 0, intercept = 0,
              color = '#d1d3d4') +
  geom_abline(slope = -1, intercept = 1,
              color = '#d1d3d4',
              linetype = 'dashed') +
  geom_point(color = 'deepskyblue',
             cex = 1.5,
             shape = 16) +
  geom_errorbar(alpha = 0.25) +
  geom_errorbarh(alpha = 0.25) +
  geom_smooth(method = 'lm',
              formula = y~x,
              color = 'firebrick1',
              se = F,
              fullrange = T) +
  facet_wrap(~ knock_in,
             nrow = 2) +
  scale_x_continuous(name = 'F (background)',
                     breaks = pretty_breaks(n = 3)) +
  scale_y_continuous(name = 'dF',
                     breaks = pretty_breaks(n = 2)) +
  scale_color_manual(name = expression(paste(italic(Sc), ' in background?')),
                     values = c('deepskyblue', 'gray')) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16, face = 'italic'),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        aspect.ratio = 0.6) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size=0.5) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)

if (save_plots) {
  ggsave(filename = '../plots/sugars_consumption_FEEs_with-Sc.pdf',
         device = 'pdf',
         dpi = 600,
         width = 200,
         height = 80,
         units = 'mm',
         limitsize = F)
}

# get list of community functions
data_list <- data.frame(community = sapply(1:nrow(data),
                                           FUN = function(i) {
                                             x <- c(as.character(data$knock_in[i]), strsplit(data$background[i], split = ',')[[1]])
                                             x <- sort(x)
                                             x <- paste(x, collapse = ',')
                                             return(x)
                                           }),
                        fun = data$background_fun.mean + data$delta_fun.mean)
data_list <- aggregate(formula = fun ~ community,
                       data = data_list,
                       FUN = mean)

# plot histogram
data_list$sc <- 'No Sc'
data_list$sc[grepl('Sc5', data_list$community)] <- 'Sc5'
data_list$sc[grepl('Sc8', data_list$community)] <- 'Sc8'
data_list$sc <- c('Without Saccharomyces', 'With Saccharomyces')[1 + grepl('Sc', data_list$community)]

ggplot(data_list, aes(x = fun, group = sc, color = sc, fill = sc)) +
  stat_density(geom = 'line',
               position = 'identity',
               size = 1) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.025,
                 color = NA,
                 alpha = 0.25,
                 position = 'identity') +
  scale_x_continuous(name = 'Fraction of sugars consumed',
                     breaks = seq(0, 1, by=0.2)) +
  scale_y_continuous(name = '# of communities') +
  scale_color_manual(values = c('#f26522', '#8c3092')) +
  scale_fill_manual(values = c('#f26522', '#8c3092')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.42, 0.9),
        aspect.ratio = 0.6) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size=0.5) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)

if (save_plots) {
  ggsave(filename = '../plots/sugars_consumption_histogram.pdf',
         device = 'pdf',
         dpi = 600,
         width = 100,
         height = 80,
         units = 'mm',
         limitsize = F)
}

data_list$sc <- NULL

# wrapper function: leave x % of the communities out of the sample, use the rest to predict
pred_vs_obs <- function(data_list, fraction = 0.1) {
  
  # leave x % of data out of sample and predict
  which_oos <- sample(1:nrow(data_list), size = round(fraction*nrow(data_list)), replace = F)
  data_oos <- data_list[which_oos, ]
  data_list <- data_list[!(data_list$community %in% data_oos$community), ]
  data_list <- rbind(data_list, data.frame(community = '', fun = 0))
  
  predF <- predictF_base(data_oos$community, data_list)
  predF$fun[predF$fun < 0] <- 0
  pred_obs <- merge(predF, data_oos, by = 'community', suffixes = c('_pred', '_obs'))
  
  rsq <- summary(lm(fun_obs ~ fun_pred, pred_obs))$adj.r.squared
  
  # plot predicted vs observed
  lims <- c(pred_obs$fun_pred, pred_obs$fun_obs)
  lims <- c(min(lims), max(lims))
  
  myplot <- 
    ggplot(pred_obs, aes(x = fun_pred, y = fun_obs)) +
      geom_abline(slope = 1,
                  intercept = 0,
                  color = '#d1d3d4') +
      geom_point(cex = 2) +
      scale_x_continuous(name = 'Predicted fraction of\nsugars consumed',
                         limits = lims,
                         breaks = pretty_breaks(n = 4)) +
      scale_y_continuous(name = 'Measured fraction of\nsugars consumed',
                         limits = lims,
                         breaks = pretty_breaks(n = 4)) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 18),
            legend.position = 'none',
            aspect.ratio = 1) +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size=0.5) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)
  
  return(list(rsq = rsq, plot = myplot))
  
}

rsq <- rep(NA, 500)
for (i in 1:length(rsq)) {
  print(paste(i, ' / ', length(rsq), sep = ''))
  rsq[i] <- pred_vs_obs(data_list)$rsq
}

# plot predicted vs observed function
myplot <- pred_vs_obs(data_list)$plot
print(myplot)
if (save_plots) {
  ggsave(myplot,
         filename = '../plots/sugars_consumption_pred_obs.pdf',
         device = 'pdf',
         dpi = 600,
         width = 100,
         height = 80,
         units = 'mm',
         limitsize = F)
}

# plot histogram of R^2
ggplot(data.frame(run = seq(1, length(rsq)),
                  rsq = rsq),
       aes(x = rsq)) +
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(name = expression(italic(R)^2),
                     limits = c(0, 1),
                     breaks = c(0, 0.5, 1),
                     labels = c('0', '0.5', '1')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        aspect.ratio = 0.6) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size=0.5) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)

if (save_plots) {
  ggsave(filename = '../plots/sugars_consumption_pred_obs_rsquared.pdf',
         device = 'pdf',
         dpi = 600,
         width = 50,
         height = 40,
         units = 'mm',
         limitsize = F)
}

# same thing but leave-one-out instead of a subset
pred_vs_obs_LOO <- function(leave_out = 'Ap,Hop,Ku,Lt,Td') {
  
  # leave x % of data out of sample and predict
  data_oos <- data_list[data_list$community == leave_out, ]
  data_list <- data_list[!(data_list$community %in% data_oos$community), ]
  data_list <- rbind(data_list, data.frame(community = '', fun = 0))
  
  predF <- predictF_base(data_oos$community, data_list)
  predF$fun[predF$fun < 0] <- 0
  pred_obs <- merge(predF, data_oos, by = 'community', suffixes = c('_pred', '_obs'))
  
  return(pred_obs)
  
}

loo <- lapply(data_list$community[data_list$community != ''],
              FUN = pred_vs_obs_LOO)
loo <- do.call(rbind, loo)

loo$n_species <- nSpecies(loo$community)
loo$sq_err <- (loo$fun_pred - loo$fun_obs)^2

# plot of model accuracy vs community richness
# keep only communities that contain Sc
plot_this <- loo[grepl('Sc', loo$community) & loo$n_species > 1, ]
plot_this$n_species <- plot_this$n_species - 1
plot_this$n_species <- factor(plot_this$n_species,
                              levels = as.character(c(1, 3:6)))
ggplot(plot_this, aes(x = n_species, y = sq_err, group = n_species)) +
  geom_jitter(width = 0.15,
              alpha = 0.25,
              shape = 16) +
  geom_boxplot(outlier.shape = NA,
               fill = NA) +
  scale_x_discrete(name = '# of species\nco-inoculated with S. cerevisiae') +
  scale_y_continuous(name = expression((italic(F)[pred] - italic(F)[obs])^2),
                     limits = c(0, 0.6),
                     breaks = c(0, 0.2, 0.4, 0.6)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        aspect.ratio = 1.4)

if (save_plots) {
  ggsave(filename = '../plots/squared_error_vs_richness.pdf',
         device = 'pdf',
         dpi = 600,
         width = 100,
         height = 120,
         units = 'mm',
         limitsize = F)
}
  

# heatmap of slopes/intercepts
slopes <- data.frame(species = character(0),
                     slope = numeric(0),
                     intercept = numeric(0))
for (sp in unique(data$knock_in)) {
  
  fit_i <- lm(delta_fun.mean ~ background_fun.mean,
              data[data$knock_in == sp, ])$coefficients
  slopes <- rbind(slopes,
                  data.frame(species = sp,
                             slope = fit_i[2],
                             intercept = fit_i[1]))
  
}

write.csv(slopes, '../data/slopes.csv', row.names = F)

# standardize values (subtract mean, divide by standard deviation) and plot heatmap
std_slopes <- slopes
std_slopes$slope <- (slopes$slope - mean(slopes$slope))/sd(slopes$slope)
std_slopes$intercept <- (slopes$intercept - mean(slopes$intercept))/sd(slopes$intercept)

df_slopes <- melt(std_slopes, id.vars = 'species')
df_slopes$species <- factor(df_slopes$species,
                            levels = c('Sp', 'Lt', 'Zb', 'Td', 'Sc5', 'Sc8', 'Ku', 'Hop', 'Wa', 'Mp', 'Pk', 'Ap'))

ggplot(df_slopes, aes(x = variable, y = species, fill = value)) +
  geom_tile(color = 'black') +
  scale_x_discrete(name = '',
                   position = 'top') +
  scale_fill_gradient(low = '#f6e24a',
                      high = '#18224b') +
  scale_y_discrete(limits = rev) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 16,
                                   angle = 45,
                                   hjust = 0),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 16,
                                   face = 'italic'),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        aspect.ratio = 5.5)

if (save_plots) {
  ggsave(filename = '../plots/fees_heatmap.pdf',
         device = 'pdf',
         dpi = 600,
         width = 100,
         height = 150,
         units = 'mm',
         limitsize = F)
}

# predicted vs. observed slopes/intercepts
pred <- read.csv('../data/Predicted_FEE_Sugar.csv')
pred <- melt(pred, id.vars = 'species')
pred$variable <- setNames(c('slope', 'intercept'),
                          c('Predicted_Slope', 'Predicted_Intercept'))[pred$variable]
colnames(pred)[3] <- 'predicted_value'

slopes <- melt(slopes, id.vars = 'species')

po <- merge(slopes, pred, by = c('species', 'variable'))

# slopes
ggplot(po[po$variable == 'slope', ],
       aes(x = predicted_value, y = value)) +
  geom_smooth(method = 'lm',
              formula = y~x,
              se = F,
              color = 'black',
              fullrange = T) +
  geom_point(cex = 3,
             shape = 1) +
  scale_x_continuous(name = 'Predicted FEE slope',
                     limits = c(-0.9, 0.25)) +
  scale_y_continuous(name = 'Observed FEE slope',
                     limits = c(-0.9, 0.25)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 16,
                                  hjust = 0),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        aspect.ratio = 1)

if (save_plots) {
  ggsave(filename = '../plots/fees_pred-obs_slope.pdf',
         device = 'pdf',
         dpi = 600,
         width = 75,
         height = 75,
         units = 'mm',
         limitsize = F)
}

# intercepts
ggplot(po[po$variable == 'intercept', ],
       aes(x = predicted_value, y = value)) +
  geom_smooth(method = 'lm',
              formula = y~x,
              se = F,
              color = 'black',
              fullrange = T) +
  geom_point(cex = 3,
             shape = 1) +
  scale_x_continuous(name = 'Predicted FEE intercept',
                     limits = c(-0.5, 0.9)) +
  scale_y_continuous(name = 'Observed FEE intercept',
                     limits = c(-0.5, 0.9)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 16,
                                  hjust = 0),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        aspect.ratio = 1)

if (save_plots) {
  ggsave(filename = '../plots/fees_pred-obs_intercept.pdf',
         device = 'pdf',
         dpi = 600,
         width = 75,
         height = 75,
         units = 'mm',
         limitsize = F)
}

### MAKE PREDICTIONS FOR ALL OUT-OF-SAMPLE COMMUNITIES

# make all possible community names
species <- unique(data$knock_in)
comms <- lapply(1:length(species),
                FUN = function(n_species) {
                  
                  sp_index <- t(combn(length(species), n_species))
                  comm_names <- sapply(1:nrow(sp_index),
                                       FUN = function(i) paste(species[sp_index[i, ]], collapse = ','))
                  
                  return(comm_names)
                  
                })
comms <- unlist(comms)

comms_oos <- comms[!(comms %in% data_list$community)]
predictedF_oos <- predictF_base(comms_oos, # this can take a while (5-10min on my computer) to execute, there are a lot of out-of-sample communities
                                rbind(data_list, data.frame(community = '', fun = 0)))

write.csv(predictedF_oos, '../data/predicted_functions.csv', row.names = F)
