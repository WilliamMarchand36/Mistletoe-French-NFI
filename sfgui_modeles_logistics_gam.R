## Libraries

library(doBy)
library(mgcv)

library(arm)
library(car)
library(performance)

library(ggeffects)

library(ggplot2)

library(dplyr)
library(tidyr)

library(spatialRF)

library(sf)
library(rnaturalearth)

library(ggside)

library(patchwork)

library(rstatix)

######################################################################################################
######################################### Distribution maps ##########################################
######################################################################################################

data_placette <- read.csv("C://Users/WMarchand/Downloads/sfgui-20250407T055942Z-001/sfgui/data_plots_2008_2022_all_v2.csv", header = TRUE)

fr_contours <- ne_countries(country = 'france', scale = "medium", returnclass = 'sf')
fr_contours_proj <- st_transform(fr_contours, 27572)

data_placette_shp <- st_as_sf(data_placette, coords = c("xl", "yl"), crs = 27572)

data_placette_shp$presence_gui_factor <- ifelse(data_placette_shp$presence_gui == "absent", "absent", "present")
data_placette_shp$presence_gui_factor <- factor(data_placette_shp$presence_gui_factor,
                                                levels = c("absent", "present")
                                                )

data_placette_shp <- data_placette_shp[order(data_placette_shp$presence_gui_factor),]

data_placette$presence_gui_factor <- ifelse(data_placette$presence_gui == "absent", "absent", "present")
data_placette$presence_gui_factor <- factor(data_placette$presence_gui_factor,
                                                levels = c("absent", "present")
)

data_placette <- data_placette[order(data_placette$presence_gui_factor),]

## Album ##

data_placette_album <- subset(data_placette, u_txguialbum != 'X')
data_placette_album$presence_album_bin <- ifelse(data_placette_album$u_txguialbum == "0", 0, 1)
data_placette_album$presence_album_factor <- factor(data_placette_album$presence_album_bin,
                                                        levels = c(0, 1)
)
data_placette_album <- data_placette_album[order(data_placette_album$presence_album_factor),]
cohens_d(data_placette_album, tmoy~presence_album_factor)
cohens_d(data_placette_album, rmoy~presence_album_factor)

data_placette_album_shp <- st_as_sf(data_placette_album, coords = c("xl", "yl"), crs = 27572)
data_placette_album_shp <- data_placette_album_shp[order(data_placette_album_shp$presence_album_factor),]

map_album <- ggplot()+
  geom_sf(data = subset(data_placette_album_shp), aes(color=presence_album_factor))+
  scale_color_manual(name = "mistletoe presence", values = c("gray65", "#26ad00"))+
  geom_sf(data=fr_contours_proj, fill = "transparent")+
  coord_sf(xlim = c(75100, 1194200), ylim = c(1621600, 2671600))+
  theme_minimal()+
  theme(legend.position = "none")

climate_space_album <- ggplot(subset(data_placette_album))+
  geom_point(aes(x=tmoy, y=rmoy, color=presence_album_factor))+
  scale_color_manual(name = "mistletoe presence", values = c("gray65", "#26ad00"))+
  geom_xsidedensity(aes(x=tmoy, color=presence_album_factor)) +
  geom_ysidedensity(aes(y=rmoy, color = presence_album_factor)) +
  theme_minimal()+
  lims(x=c(min(data_placette$tmoy), max(data_placette$tmoy)), y=c(min(data_placette$rmoy), max(data_placette$rmoy)))+
  theme(legend.position = "none",
        ggside.panel.scale.x = .4,
        ggside.panel.scale.y = .4
        )+
  labs(x="temperature normals [°C]", y="precipitation normals [mm]")


## Abietis ##

data_placette_abietis <- subset(data_placette, u_txguiabiet != 'X')
data_placette_abietis$presence_abietis_bin <- ifelse(data_placette_abietis$u_txguiabiet == "0", 0, 1)
data_placette_abietis$presence_abietis_factor <- factor(data_placette_abietis$presence_abietis_bin,
                                                        levels = c(0, 1)
)
data_placette_abietis <- data_placette_abietis[order(data_placette_abietis$presence_abietis_factor),]

cohens_d(data_placette_abietis, tmoy~presence_abietis_factor)
cohens_d(data_placette_abietis, rmoy~presence_abietis_factor)

data_placette_abietis_shp <- st_as_sf(data_placette_abietis, coords = c("xl", "yl"), crs = 27572)
data_placette_abietis_shp <- data_placette_abietis_shp[order(data_placette_abietis_shp$presence_abietis_factor),]

map_abietis <- ggplot()+
  geom_sf(data = subset(data_placette_abietis_shp), aes(color=presence_abietis_factor))+
  scale_color_manual(name = "mistletoe presence", values = c("gray65", "#029dd1"))+
  geom_sf(data=fr_contours_proj, fill = "transparent")+
  coord_sf(xlim = c(75100, 1194200), ylim = c(1621600, 2671600))+
  theme_minimal()+
  theme(legend.position = "none")

climate_space_abietis <- ggplot(subset(data_placette_abietis))+
  geom_point(aes(x=tmoy, y=rmoy, color=presence_abietis_factor))+
  scale_color_manual(name = "mistletoe presence", values = c("gray65", "#029dd1"))+
  geom_xsidedensity(aes(x=tmoy, color=presence_abietis_factor)) +
  geom_ysidedensity(aes(y=rmoy, color = presence_abietis_factor)) +
  theme_minimal()+
  theme(legend.position = "none",
        ggside.panel.scale.x = .4,
        ggside.panel.scale.y = .4
        )+
  lims(x=c(min(data_placette$tmoy), max(data_placette$tmoy)), y=c(min(data_placette$rmoy), max(data_placette$rmoy)))+
  labs(x="temperature normals [°C]", y="precipitation normals [mm]")

## Austriacum ##

data_placette_austriacum <- subset(data_placette, u_txguiaustr != 'X')
data_placette_austriacum$presence_austriacum_bin <- ifelse(data_placette_austriacum$u_txguiaustr == "0", 0, 1)
data_placette_austriacum$presence_austriacum_factor <- factor(data_placette_austriacum$presence_austriacum_bin,
                                                              levels = c(0, 1)
)
cohens_d(data_placette_austriacum, tmoy~presence_austriacum_factor)
cohens_d(data_placette_austriacum, rmoy~presence_austriacum_factor)

data_placette_austriacum <- data_placette_austriacum[order(data_placette_austriacum$presence_austriacum_factor),]
data_placette_austriacum_shp <- st_as_sf(data_placette_austriacum, coords = c("xl", "yl"), crs = 27572)
data_placette_austriacum_shp <- data_placette_austriacum_shp[order(data_placette_austriacum_shp$presence_austriacum_factor),]

map_austriacum <- ggplot()+
  geom_sf(data = subset(data_placette_austriacum_shp), aes(color=presence_austriacum_factor))+
  scale_color_manual(name = "mistletoe presence", values = c("gray65", "#4d00bf"))+
  geom_sf(data=fr_contours_proj, fill = "transparent")+
  coord_sf(xlim = c(75100, 1194200), ylim = c(1621600, 2671600))+
  theme_minimal()+
  theme(legend.position = "none")

climate_space_austriacum <- ggplot(subset(data_placette_austriacum))+
  geom_point(aes(x=tmoy, y=rmoy, color=presence_austriacum_factor))+
  scale_color_manual(name = "mistletoe presence", values = c("gray65", "#4d00bf"))+
  geom_xsidedensity(aes(x=tmoy, color=presence_austriacum_factor)) +
  geom_ysidedensity(aes(y=rmoy, color = presence_austriacum_factor)) +
  theme_minimal()+
  theme(legend.position = "none",
        ggside.panel.scale.x = .4,
        ggside.panel.scale.y = .4
        )+
  lims(x=c(min(data_placette$tmoy), max(data_placette$tmoy)), y=c(min(data_placette$rmoy), max(data_placette$rmoy)))+
  labs(x="temperature normals [°C]", y="precipitation normals [mm]")

############################################################################################
########################## FIGURE 1 ########################################################
############################################################################################

map_album / climate_space_album | map_abietis / climate_space_abietis | map_austriacum / 
  climate_space_austriacum

ggsave("map_climatespaces.png", height = 12, width = 20, dpi = 300)
ggsave("map_climatespaces.svg", height = 12, width = 20)


######################################################################################################
######################################### Plot-level models ##########################################
######################################################################################################

## Abietis ##

data_placette_abietis$div_rg1_factor <- as.factor(as.factor(data_placette_abietis$div_rg1))

test_gam_placettes_abietis <- gam(
  family = binomial(),
  data = subset(data_placette_abietis,
  ),
  formula = presence_abietis_bin ~ 
    + s(dens_tiges, bs = "cr", k = 3)
    + div_rg1_factor
    + s(tnmoy, bs = "cr", k = 3)
    + s(txmoy, bs = "cr", k = 3)
    + s(rmoy, bs = "cr", k = 3)
    + s(reserutile, bs = "cr", k = 3)
    + s(troph_r, bs = "cr", k = 3)
    + s(hydr_r, bs = "cr", k = 3)
    + s(wc2.1_30s_bio_7, bs = "cr", k = 3)
    + s(wc2.1_30s_bio_3, bs = "cr", k = 3)
    + s(wc2.1_30s_bio_18, bs = "cr", k = 3)
    , weights = poids
)

summ_npp_abiet <- summary(test_gam_placettes_abietis)
p.table <- as.data.frame(summ_npp_abiet$p.table)
p.table$var <- rownames(p.table)
write.csv(p.table, "coefs_abiet_npp.csv", row.names = FALSE)

s.table <- as.data.frame(summ_npp_abiet$s.table)
s.table$var <- rownames(s.table)
write.csv(s.table, "coefs_smooth_abiet_npp.csv", row.names = FALSE)
  
binnedplot(fitted(test_gam_placettes_abietis), resid(test_gam_placettes_abietis, type = "response"))

concurvity(test_gam_placettes_abietis)
r2(test_gam_placettes_abietis)

spatialRF::vif(data_placette_abietis[, c("dens_tiges",
                                         "div_rg1_factor",
                                         "tnmoy",
                                         "txmoy",
                                         "rmoy",
                                         "reserutile",
                                         "troph_r",
                                         "hydr_r",
                                         "wc2.1_30s_bio_7",
                                         "wc2.1_30s_bio_3",
                                         "wc2.1_30s_bio_18"
)])

list_vars <- names(test_gam_placettes_abietis$model)[-c(1:2, ncol(test_gam_placettes_abietis$model))]

data_predict <- data.frame()

for(i in list_vars){

predict_data_i <- ggpredict(test_gam_placettes_abietis, c(paste0(i, ' [all]')), weights = "poids")
predict_data_i$variable <- rep(i)

data_predict <- rbind(data_predict, predict_data_i)

}
saveRDS(data_predict, "predict_env_abietis.RDS")
data_predict <- readRDS("predict_env_abietis.RDS")
data_predict$variable_factor <- factor(data_predict$variable,
                                       levels = c(
                                         "hydr_r",
                                         "troph_r",
                                         "reserutile",
                                         "dens_tiges",
                                         "rmoy",
                                         "tnmoy",
                                         "txmoy",
                                         "wc2.1_30s_bio_3",
                                         "wc2.1_30s_bio_7",
                                         "wc2.1_30s_bio_18"
                                       )
)

############################################################################################
########################## FIGURE 2 - middle panels ########################################
############################################################################################

ggplot(data_predict)+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), fill="#029dd1", alpha = .4)+
  geom_line(aes(x=x, y=predicted), color="#029dd1")+
  theme_bw()+
  facet_wrap(.~variable_factor, scales = "free", nrow = 2, labeller = as_labeller(c("hydr_r" = "hydric index\n[unitless]",
                                                                             "reserutile" = "soil water reserve\n[mm]",
                                                                             "rmoy" = "precipitation normals\n[mm]",
                                                                             "dens_tiges" = "stem density\n[trees / ha]",
                                                                             "tnmoy" = "min. temp. normals\n[°C]",
                                                                             "txmoy" = "max. temp. normals\n[°C]",
                                                                             "troph_r" = "trophic index\n[unitless]",
                                                                             "wc2.1_30s_bio_18" = "prec. of warmest\nquarter [mm]",
                                                                             "wc2.1_30s_bio_3" = "isothermality\n[%]",
                                                                             "wc2.1_30s_bio_7" = "temp. annual range\n[°C]"
  ))
  )+
  labs(x="", y="Predicted probability of mistletoe occurrence [unitless]")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        plot.margin = margin(.5, 2, .5, .5, unit = "cm")
  )
ggsave("pred_abietis_env.png", width = 16, height = 6, dpi = 300)
ggsave("pred_abietis_env.svg", width = 16, height = 6)

## Austriacum ##

data_placette_austriacum$div_rg1_factor <- as.factor(data_placette_austriacum$div_rg1)

test_gam_placettes_austriacum <- gam(
  family = binomial(),
  data = subset(data_placette_austriacum,
  ),
  formula = presence_austriacum_bin ~ 
    + s(dens_tiges, bs = "cr", k = 3)
    + div_rg1_factor
    + s(tnmoy, bs = "cr", k = 3)
    + s(rmoy, bs = "cr", k = 3)
    + s(reserutile, bs = "cr", k = 3)
    + s(troph_r, bs = "cr", k = 3)
    + s(hydr_r, bs = "cr", k = 3)
    + s(wc2.1_30s_bio_7, bs = "cr", k = 3)
    + s(wc2.1_30s_bio_3, bs = "cr", k = 3)
    + s(wc2.1_30s_bio_18, bs = "cr", k = 3)
    , weights = poids
    )

AIC(test_gam_placettes_austriacum)

binnedplot(fitted(test_gam_placettes_austriacum), resid(test_gam_placettes_austriacum, type = "response"))

summ_npp_austr <- summary(test_gam_placettes_austriacum)
p.table <- as.data.frame(summ_npp_austr$p.table)
p.table$var <- rownames(p.table)
write.csv(p.table, "coefs_austr_npp.csv", row.names = FALSE)

s.table <- as.data.frame(summ_npp_austr$s.table)
s.table$var <- rownames(s.table)
write.csv(s.table, "coefs_smooth_austr_npp.csv", row.names = FALSE)

concurvity(test_gam_placettes_austriacum)
r2(test_gam_placettes_austriacum)

spatialRF::vif(data_placette_austriacum[, c("dens_tiges",
                                                             "div_rg1_factor",
                                                             "tnmoy",
                                                             "rmoy",
                                                             "reserutile",
                                                             "troph_r",
                                                             "hydr_r",
                                                             "wc2.1_30s_bio_7",
                                                             "wc2.1_30s_bio_3",
                                                             "wc2.1_30s_bio_18"
)])

list_vars <- names(test_gam_placettes_austriacum$model)[-c(1:2, ncol(test_gam_placettes_austriacum$model))]

data_predict <- data.frame()

for(i in list_vars){
  
  predict_data_i <- ggpredict(test_gam_placettes_austriacum, c(paste0(i, ' [all]')), weights = "poids")
  predict_data_i$variable <- rep(i)
  
  data_predict <- rbind(data_predict, predict_data_i)
  
}

saveRDS(data_predict, "predict_env_austriacum.RDS")
data_predict <- readRDS("predict_env_austriacum.RDS")
data_predict$variable_factor <- factor(data_predict$variable,
                                       levels = c(
                                         "hydr_r",
                                         "troph_r",
                                         "reserutile",
                                         "dens_tiges",
                                         "rmoy",
                                         "tnmoy",
                                         "wc2.1_30s_bio_3",
                                         "wc2.1_30s_bio_7",
                                         "wc2.1_30s_bio_18"
                                       )
)

############################################################################################
########################## FIGURE 2 - bottom panels ########################################
############################################################################################

ggplot(data_predict)+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), fill="#4d00bf", alpha = .4)+
  geom_line(aes(x=x, y=predicted), color="#4d00bf")+
  theme_bw()+
  facet_wrap(.~variable_factor, scales = "free", nrow = 2, labeller = as_labeller(c("hydr_r" = "hydric index\n[unitless]",
                                                                             "reserutile" = "soil water reserve\n[mm]",
                                                                             "rmoy" = "precipitation normals\n[mm]",
                                                                             "dens_tiges" = "stem density\n[trees / ha]",
                                                                             "tnmoy" = "min. temp. normals\n[°C]",
                                                                             "troph_r" = "trophic index\n[unitless]",
                                                                             "wc2.1_30s_bio_18" = "prec. of warmest\nquarter [mm]",
                                                                             "wc2.1_30s_bio_3" = "isothermality\n[%]",
                                                                             "wc2.1_30s_bio_7" = "temp. annual range\n[°C]"
  ))
  )+
  labs(x="", y="Predicted probability of mistletoe occurrence [unitless]")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        plot.margin = margin(.5, 2, .5, .5, unit = "cm")
  )

ggsave("pred_austriacum_env.png", width = 16, height = 6, dpi = 300)
ggsave("pred_austriacum_env.svg", width = 16, height = 6)

## Album ##

data_placette_album$div_rg1_factor <- as.factor(data_placette_album$div_rg1)

test_gam_placettes_album <- gam(
  family = binomial(),
  data = subset(data_placette_album,
  ),
  formula = presence_album_bin ~ 
    + s(dens_tiges, bs = "cr", k = 3)
    + div_rg1_factor
    + s(tnmoy, bs = "cr", k = 3)
    + s(zp, bs = "cr", k = 3)
    + s(rmoy, bs = "cr", k = 3)
    + s(reserutile, bs = "cr", k = 3)
    + s(troph_r, bs = "cr", k = 3)
    + s(hydr_r, bs = "cr", k = 3)
    + s(wc2.1_30s_bio_7, bs = "cr", k = 3)
    + s(wc2.1_30s_bio_3, bs = "cr", k = 3)
    + s(wc2.1_30s_bio_18, bs = "cr", k = 3)
    , weights = poids
    )

summ_npp_album <- summary(test_gam_placettes_album)
p.table <- as.data.frame(summ_npp_album$p.table)
p.table$var <- rownames(p.table)
write.csv(p.table, "coefs_album_npp.csv", row.names = FALSE)

s.table <- as.data.frame(summ_npp_album$s.table)
s.table$var <- rownames(s.table)
write.csv(s.table, "coefs_smooth_album_npp.csv", row.names = FALSE)

spatialRF::vif(data_placette_album[, c("dens_tiges",
                                                  "div_rg1_factor",
                                                  "tnmoy",
                                                  "rmoy",
                                                  "zp",
                                                  "reserutile",
                                                  "troph_r",
                                                  "hydr_r",
                                                  "wc2.1_30s_bio_7",
                                                  "wc2.1_30s_bio_3",
                                                  "wc2.1_30s_bio_18"
                                                  )])

AIC(test_gam_placettes_album)

binnedplot(fitted(test_gam_placettes_album), resid(test_gam_placettes_album, type = "response"))

concurvity(test_gam_placettes_album)
r2(test_gam_placettes_album)

list_vars <- names(test_gam_placettes_album$model)[-c(1:2, ncol(test_gam_placettes_album$model))]

data_predict <- data.frame()

for(i in list_vars){
  
  predict_data_i <- ggpredict(test_gam_placettes_album, c(paste0(i, ' [all]')), weights = "poids")
  predict_data_i$variable <- rep(i)
  
  data_predict <- rbind(data_predict, predict_data_i)
  
}

saveRDS(data_predict, "predict_env_album.RDS")
data_predict <- readRDS("predict_env_album.RDS")
data_predict$variable_factor <- factor(data_predict$variable,
                                       levels = c(
                                         "hydr_r",
                                         "troph_r",
                                         "reserutile",
                                         "zp",
                                         "dens_tiges",
                                         "rmoy",
                                         "tnmoy",
                                         "wc2.1_30s_bio_3",
                                         "wc2.1_30s_bio_7",
                                         "wc2.1_30s_bio_18"
                                       )
                                       )

############################################################################################
########################## FIGURE 2 - top panels ###########################################
############################################################################################

ggplot(data_predict)+
  geom_ribbon(aes(x=x, ymin=ifelse(conf.low > 0, conf.low, 0), ymax=conf.high), fill="#26ad00", alpha = .3)+
  geom_line(aes(x=x, y=predicted), color="#26ad00")+
  theme_bw()+
  facet_wrap(.~variable_factor, scales = "free", nrow = 2, labeller = as_labeller(c("hydr_r" = "hydric index\n[unitless]",
                                                                             "reserutile" = "soil water reserve\n[mm]",
                                                                             "rmoy" = "precipitation normals\n[mm]",
                                                                             "dens_tiges" = "stem density\n[trees / ha]",
                                                                             "tnmoy" = "min. temp. normals\n[°C]",
                                                                             "troph_r" = "trophic index\n[unitless]",
                                                                             "zp" = "elevation\n[m]",
                                                                             "wc2.1_30s_bio_18" = "prec. of warmest\nquarter [mm]",
                                                                             "wc2.1_30s_bio_3" = "isothermality\n[%]",
                                                                             "wc2.1_30s_bio_7" = "temp. annual range\n[°C]"
                                                                             ))
             )+
  labs(x="", y="Predicted probability of mistletoe occurrence [unitless]")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        plot.margin = margin(.5, 2, .5, .5, unit = "cm")
        )

ggsave("pred_album_env.png", width = 16, height = 6, dpi = 300)
ggsave("pred_album_env.svg", width = 16, height = 6)

######################################################################################################
######################################### Tree-level models ##########################################
######################################################################################################

data_arbre <- read.csv("C://Users/WMarchand/Downloads/sfgui-20250407T055942Z-001/sfgui/data_trees_2008_2022_v2.csv", header = TRUE)

## Abietis ##

data_arbre_abietis <- subset(data_arbre, ssp_gui == "abietis")
data_arbre_abietis_bio$mortbg2_factor <- as.factor(data_arbre_abietis$mortbg2)
data_arbre_abietis_bio$npp_factor <- as.factor(data_arbre_abietis_bio$npp)

test_gam_arbres_abietis <- gam(
  family = binomial,
  data = subset(data_arbre_abietis_bio, mortbg2 != "X"),
  formula = is_gui_bin ~ 
    s(c13, bs = "cr",  k = 4)
  + mortbg2_factor
  + te(npp_factor, bs = "re", k = 3)
  
  , weights = poids_models
)

binnedplot(fitted(test_gam_arbres_abietis), resid(test_gam_arbres_abietis, type = "response"))

saveRDS(test_gam_arbres_abietis, "gam_mortbg2_abietis_reduced.RDS")
test_gam_arbres_abietis <- readRDS("gam_mortbg2_abietis_reduced.RDS")

summ_a_abiet <- summary(test_gam_arbres_abietis)
p.table <- as.data.frame(summ_a_abiet$p.table)
p.table$var <- rownames(p.table)
write.csv(p.table, "coefs_abiet_a.csv", row.names = FALSE)

s.table <- as.data.frame(summ_a_abiet$s.table)
s.table$var <- rownames(s.table)
write.csv(s.table, "coefs_smooth_abiet_a.csv", row.names = FALSE)

concurvity(test_gam_arbres_abietis)
r2(test_gam_arbres_abietis)

pred_mortbg2_abietis <- ggaverage(test_gam_arbres_abietis, c('mortbg2_factor [all]'))
saveRDS(pred_mortbg2_abietis, "pred_values_mortbg2_average_abietis_reduced.RDS")

## Austriacum ##

data_arbre_austriacum <- subset(data_arbre, ssp_gui == "austriacum")
data_arbre_austriacum_bio$mortbg2_factor <- as.factor(data_arbre_austriacum_bio$mortbg2)
data_arbre_austriacum_bio$npp_factor <- as.factor(data_arbre_austriacum_bio$npp)

test_gam_arbres_austriacum <- gam(
  family = binomial,
  data = subset(data_arbre_austriacum_bio, mortbg2 != "X"),
  formula = is_gui_bin ~ 
    s(c13, bs = "cr",  k = 4)
  + mortbg2_factor
  + te(npp_factor, bs = "re", k = 3)
  
  , weights = poids_models
)

binnedplot(fitted(test_gam_arbres_austriacum), resid(test_gam_arbres_austriacum, type = "response"))

summ_a_austr <- summary(test_gam_arbres_austriacum)
p.table <- as.data.frame(summ_a_austr$p.table)
p.table$var <- rownames(p.table)
write.csv(p.table, "coefs_austr_a.csv", row.names = FALSE)

s.table <- as.data.frame(summ_a_austr$s.table)
s.table$var <- rownames(s.table)
write.csv(s.table, "coefs_smooth_austr_a.csv", row.names = FALSE)

concurvity(test_gam_arbres_austriacum)
r2(test_gam_arbres_austriacum)

saveRDS(test_gam_arbres_austriacum, "gam_mortbg2_austriacum_reduced.RDS")

pred_mortbg2_austriacum <- ggaverage(test_gam_arbres_austriacum, c('mortbg2_factor [all]'))
saveRDS(pred_mortbg2_austriacum, "pred_values_mortbg2_average_austriacum_reduced.RDS")

## Album ##

data_arbre_album <- subset(data_arbre, ssp_gui == "album")
data_arbre_album_bio$mortbg2_factor <- as.factor(data_arbre_album_bio$mortbg2)
data_arbre_album_bio$npp_factor <- as.factor(data_arbre_album_bio$npp)

head(data_arbre_album_bio)

test_gam_arbres_album <- gam(
  family = binomial,
  data = subset(data_arbre_album_bio, mortbg2 != "X"),
  formula = is_gui_bin ~ 
    s(c13, bs = "cr",  k = 4)
  + mortbg2_factor
  + te(npp_factor, bs = "re", k = 3)
  , weights = poids_models
)

binnedplot(fitted(test_gam_arbres_album), resid(test_gam_arbres_album, type = "response"))

summ_a_album <- summary(test_gam_arbres_album)
p.table <- as.data.frame(summ_a_album$p.table)
p.table$var <- rownames(p.table)
write.csv(p.table, "coefs_album_a.csv", row.names = FALSE)

s.table <- as.data.frame(summ_a_album$s.table)
s.table$var <- rownames(s.table)
write.csv(s.table, "coefs_smooth_album_a.csv", row.names = FALSE)

concurvity(test_gam_arbres_album)
r2(test_gam_arbres_album)

saveRDS(test_gam_arbres_album, "gam_mortbg2_album_reduced.RDS")


pred_mortbg2_album <- ggaverage(test_gam_arbres_album, c('mortbg2_factor [all]'))
saveRDS(pred_mortbg2_album, "pred_values_mortbg2_average_album_reduced.RDS")

pred_mortbg2_album$ssp_gui <- "album"
pred_mortbg2_austriacum$ssp_gui <- "austriacum"
pred_mortbg2_abietis$ssp_gui <- "abietis"

pred_mortbg2_all <- rbind(pred_mortbg2_abietis,
                          pred_mortbg2_austriacum,
                          pred_mortbg2_album
                          )

pred_mortbg2_all$mortbg2[pred_mortbg2_all$x == 0] <- "0-5%"
pred_mortbg2_all$mortbg2[pred_mortbg2_all$x == 1] <- "5-25%"
pred_mortbg2_all$mortbg2[pred_mortbg2_all$x == 2] <- "25-50%"
pred_mortbg2_all$mortbg2[pred_mortbg2_all$x == 3] <- "50-95%"
pred_mortbg2_all$mortbg2[pred_mortbg2_all$x == 4] <- ">95%"

pred_mortbg2_all$mortbg2 <- factor(pred_mortbg2_all$mortbg2,
                                   levels = c("0-5%",
                                              "5-25%",
                                              "25-50%",
                                              "50-95%",
                                              ">95%"
                                              )
                                   )
############################################################################################
########################## FIGURE 3 ########################################################
############################################################################################

library(ggtext)
ggplot(pred_mortbg2_all)+
  geom_errorbar(aes(x=mortbg2, ymin=conf.low, ymax=conf.high, color=ssp_gui), width = .4)+
  geom_point(aes(x=mortbg2, y=predicted, color=ssp_gui), size = 2)+
  scale_color_manual(name = "<i>V.album</i> subspecies", values = c("album" = "#26ad00",
                                                             "abietis" = "#029dd1",
                                                             "austriacum" = "#4d00bf"
                                                             ))+
  labs(x="Crown dieback class", y="Predicted probability of tree infestation")+
  theme_bw()+
  theme(legend.title = element_markdown(size = 14),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = c(.15, .9)
        )

ggsave("pred_mortbg2_reduced.png", width = 10, height = 8, dpi = 300)


############################################################################################
########################## tree-level models - growth ######################################
############################################################################################

data_ir5_bio <- read.csv("C://Users/WMarchand/Downloads/sfgui-20250407T055942Z-001/sfgui/data_ir5_2008_2022_v2.csv", header = TRUE)
data_ir5_bio$sfgui_factor <- as.factor(data_ir5_bio$sfgui)
data_ir5_bio$npp_factor <- as.factor(data_ir5_bio$npp)

data_ir5_bio$div_rg1_factor <- as.factor(data_ir5_bio$div_rg1)
data_ir5_bio$ess_factor <- as.factor(data_ir5_bio$ess)

## Abietis ##

test_gam_ir5 <- gam(
  data = subset(data_ir5_bio, ssp_gui == "abietis"),
  formula = log(croissance_5ans) ~ 
    s(c13, bs = "cr",  k = 4)
  + div_rg1_factor
  + sfgui_factor
  + s(dens_tiges, bs = "cr", k = 3)
  + s(tnmoy, bs = "cr", k = 3)
  + s(txmoy, bs = "cr", k = 3)
  + s(rmoy, bs = "cr", k = 3)
  + s(reserutile, bs = "cr", k = 3)
  + s(troph_r, bs = "cr", k = 3)
  + s(hydr_r, bs = "cr", k = 3)
  + s(zp, bs = "cr", k = 3)
  + s(wc2.1_30s_bio_7, bs = "cr", k = 3)
  + s(wc2.1_30s_bio_3, bs = "cr", k = 3)
  + s(wc2.1_30s_bio_18, bs = "cr", k = 3)
  + s(wc2.1_30s_bio_6, bs = "cr", k = 3)
  + s(age_1, bs = "cr", k = 3)
  
  + te(npp_factor, bs = "re", k = 3)
  
  , weights = poids_models
)

spatialRF::vif(subset(data_ir5_bio, ssp_gui == "abietis" )[, c("c13",
                                                              "zp",
                                                               "txmoy",
                                                               "tnmoy",
                                                               "dens_tiges",
                                                               "rmoy",
                                                               "reserutile",
                                                               "troph_r",
                                                               "hydr_r",
                                                               "wc2.1_30s_bio_7",
                                                               "wc2.1_30s_bio_3",
                                                                "wc2.1_30s_bio_6",
                                                               "wc2.1_30s_bio_18"
)])

plot(resid(test_gam_ir5, type = "response")~fitted(test_gam_ir5))

concurvity(test_gam_ir5)
r2(test_gam_ir5)

summ_ir5_abiet <- summary(test_gam_ir5)
p.table <- as.data.frame(summ_ir5_abiet$p.table)
p.table$var <- rownames(p.table)
write.csv(p.table, "coefs_abiet_ir5_c13.csv", row.names = FALSE)

s.table <- as.data.frame(summ_ir5_abiet$s.table)
s.table$var <- rownames(s.table)
write.csv(s.table, "coefs_smooth_abiet_ir5_c13.csv", row.names = FALSE)

saveRDS(test_gam_ir5, "gam_modele_ir5_abietis_divrg1_c13.RDS")
test_gam_ir5 <- readRDS("gam_modele_ir5_abietis_divrg1_c13.RDS")

ave_predictd_ir5_abiet <- ggaverage(test_gam_ir5, c('sfgui_factor [all]'), weights = "poids_models")
saveRDS(ave_predictd_ir5_abiet, "pred_values_average_c13.RDS")

## Austriacum ##

test_gam_ir5 <- gam(
  data = subset(data_ir5_bio, ssp_gui == "austriacum"),
  formula = log(croissance_5ans) ~ 
    s(c13, bs = "cr",  k = 4)
  + div_rg1_factor
  + sfgui_factor
  + ess_factor
  + ess_factor:sfgui_factor
  + s(txmoy, bs = "cr", k = 3)
  + s(tnmoy, bs = "cr", k = 3)
  + s(rmoy, bs = "cr", k = 3)
  + s(dens_tiges, bs = "cr", k = 3)
  + s(reserutile, bs = "cr", k = 3)
  + s(troph_r, bs = "cr", k = 3)
  + s(hydr_r, bs = "cr", k = 3)
  + s(wc2.1_30s_bio_3, bs = "cr", k = 3)
  + s(wc2.1_30s_bio_18, bs = "cr", k = 3)
  + s(age_1, bs = "cr", k = 3)
  
  + te(npp_factor, bs = "re", k = 3)
  
  , weights = poids_models
)

saveRDS(test_gam_ir5, "gam_modele_ir5_austriacum_divrg1_c13.RDS")
test_gam_ir5_austr <- readRDS("gam_modele_ir5_austriacum_divrg1_c13.RDS")

summ_ir5_austr <- summary(test_gam_ir5_austr)
p.table <- as.data.frame(summ_ir5_austr$p.table)
p.table$var <- rownames(p.table)
write.csv(p.table, "coefs_austr_ir5_c13.csv", row.names = FALSE)

s.table <- as.data.frame(summ_ir5_austr$s.table)
s.table$var <- rownames(s.table)
write.csv(s.table, "coefs_smooth_austr_ir5_c13.csv", row.names = FALSE)

spatialRF::vif(subset(data_ir5_bio, ssp_gui == "austriacum")[, c("c13",
                                                              "txmoy",
                                                              "tnmoy",
                                                              "dens_tiges",
                                                              "rmoy",
                                                              "reserutile",
                                                              "troph_r",
                                                              "hydr_r",
                                                              "wc2.1_30s_bio_3",
                                                              "wc2.1_30s_bio_18"
)])

plot(resid(test_gam_ir5, type = "response")~fitted(test_gam_ir5))

concurvity(test_gam_ir5)
r2(test_gam_ir5_austr)

ave_pred_ir5_austriacum <- ggaverage(test_gam_ir5, c('ess_factor [all]', 'sfgui_factor [all]'), weights = "poids_models")
saveRDS(ave_pred_ir5_austriacum, "pred_values_ir5_average_austriacum_c13.RDS")

## Album ##

test_gam_ir5 <- gam(
  data = subset(data_ir5_bio, ssp_gui == "album"  & ess != 41),
  formula = log(croissance_5ans) ~ 
    s(c13, bs = "cr",  k = 4)
  + div_rg1_factor
  + sfgui_factor
  + ess_factor
  + ess_factor:sfgui_factor
  + s(tnmoy, bs = "cr", k = 3)
  + s(rmoy, bs = "cr", k = 3)
  + s(dens_tiges, bs = "cr", k = 3)
  + s(reserutile, bs = "cr", k = 3)
  + s(troph_r, bs = "cr", k = 3)
  + s(hydr_r, bs = "cr", k = 3)
  + s(zp, bs = "cr", k = 3)
  + s(wc2.1_30s_bio_7, bs = "cr", k = 3)
  + s(wc2.1_30s_bio_3, bs = "cr", k = 3)
  + s(wc2.1_30s_bio_18, bs = "cr", k = 3)
  
  + s(age_1, bs = "cr", k = 3)
  
  + te(npp_factor, bs = "re", k = 3)
  
  , weights = poids_models
)

saveRDS(test_gam_ir5, "gam_modele_ir5_album_divrg1_noess41_c13.RDS")
test_gam_ir5_album <- readRDS("gam_modele_ir5_album_divrg1_noess41_c13.RDS")

summ_ir5_album <- summary(test_gam_ir5_album)
p.table <- as.data.frame(summ_ir5_album$p.table)
p.table$var <- rownames(p.table)
write.csv(p.table, "coefs_album_ir5_noess41_c13.csv", row.names = FALSE)

s.table <- as.data.frame(summ_ir5_album$s.table)
s.table$var <- rownames(s.table)
write.csv(s.table, "coefs_smooth_album_ir5_noess41_c13.csv", row.names = FALSE)

plot(resid(test_gam_ir5, type = "response")~fitted(test_gam_ir5))

concurvity(test_gam_ir5)
r2(test_gam_ir5_album)

spatialRF::vif(subset(data_ir5_bio, ssp_gui == "album"   & c13 > .708)[, c("c13",
                                                            "zp",
                                                            "tnmoy",
                                                            "dens_tiges",
                                                            "rmoy",
                                                            "reserutile",
                                                            "troph_r",
                                                            "hydr_r",
                                                            "wc2.1_30s_bio_7",
                                                            "wc2.1_30s_bio_3",
                                                            "wc2.1_30s_bio_18"
)])

ave_pred_ir5_album <- ggaverage(test_gam_ir5, c('ess_factor [all]', 'sfgui_factor [all]'), weights = "poids_models")
saveRDS(ave_pred_ir5_album, "pred_values_ir5_average_album_noess41_c13.RDS")

ave_pred_ir5_austriacum <- readRDS("pred_values_ir5_average_austriacum_c13.RDS")
ave_pred_ir5_austriacum$ssp_gui <- "austriacum"
ave_pred_ir5_abietis <- readRDS("pred_values_average_c13.RDS")
ave_pred_ir5_abietis$ssp_gui <- "abietis"
ave_pred_ir5_abietis$group <- ave_pred_ir5_abietis$x
ave_pred_ir5_abietis$x <- rep(as.factor(61))
ave_pred_ir5_album <- readRDS("pred_values_ir5_average_album_noess41_c13.RDS")
ave_pred_ir5_album$ssp_gui <- "album"

ave_pred_ir5_all <- rbind(ave_pred_ir5_austriacum,
                          ave_pred_ir5_abietis,
                          ave_pred_ir5_album
                          )
unique(ave_pred_ir5_all$x)
ave_pred_ir5_all$x <- as.character(ave_pred_ir5_all$x)

mod_ess <- read.csv("C://Users/willi/Downloads/Modalites_ESS.csv", sep=",", encoding = "Latin1")
mod_ess$mode <- as.character(mod_ess$mode)

ave_pred_ir5_all_lib <- inner_join(ave_pred_ir5_all, mod_ess, by = c("x" = "mode"))
ave_pred_ir5_all_lib$labs[ave_pred_ir5_all_lib$group == "0"] <- "no cluster"
ave_pred_ir5_all_lib$labs[ave_pred_ir5_all_lib$group == "1"] <- "1-2 clusters"
ave_pred_ir5_all_lib$labs[ave_pred_ir5_all_lib$group == "2"] <- "3-5 clusters"
ave_pred_ir5_all_lib$labs[ave_pred_ir5_all_lib$group == "3"] <- "more than 5 clusters"
ave_pred_ir5_all_lib$labs <- factor(ave_pred_ir5_all_lib$labs,
                                       levels = c("no cluster",
                                                  "1-2 clusters",
                                                  "3-5 clusters",
                                                  "more than 5 clusters"
                                                  )
                                       )
ave_pred_ir5_all_lib$libelle[ave_pred_ir5_all_lib$x == "21"] <- "Small maples"
ave_pred_ir5_all_lib$libelle[ave_pred_ir5_all_lib$x == "15"] <- "Big maples"

############################################################################################
########################## Figure 4 ########################################################
############################################################################################

ggplot(ave_pred_ir5_all_lib)+
  geom_errorbar(aes(x=libelle, ymin=conf.low, ymax=conf.high, color = labs), position = position_dodge(width = .6), width = .4)+
  geom_point(aes(x=libelle, y=predicted, color = labs), position = position_dodge(width = .6), size = 2)+
  scale_color_manual(name = "Number of\nMistletoe clusters", values = c("no cluster" = "#5cb3fa",
                                                             "1-2 clusters" = "#ffee7d",
                                                             "3-5 clusters" = "#ffac69",
                                                             "more than 5 clusters" = "#ff8269"
                                                             )
                     )+
  labs(x = "", y = "5-yr growth [mm]")+
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = "right",
        legend.direction = "vertical"
        )+
  coord_flip()

ggsave("ir5_mistletoe_species_noess41_c13_v1.png", width = 10, height = 8, dpi = 300)

####################################################################################
############## SUPPLEMENTARY FIGURES ###############################################
####################################################################################

## test abietis Vosges

test_gam_arbres_abietis <- gam(
  family = binomial,
  data = subset(data_arbre_abietis_bio, mortbg2 != "X" & greco == "D"),
  formula = is_gui_bin ~ 
    s(c13, bs = "cr",  k = 4)
  + mortbg2_factor
  + te(npp_factor, bs = "re", k = 3)
  
  , weights = poids_models
)

pred_mortbg2_abietis <- ggpredict(test_gam_arbres_abietis, c('mortbg2_factor [all]'))

pred_mortbg2_abietis$mortbg2[pred_mortbg2_abietis$x == 0] <- "0-5%"
pred_mortbg2_abietis$mortbg2[pred_mortbg2_abietis$x == 1] <- "5-25%"
pred_mortbg2_abietis$mortbg2[pred_mortbg2_abietis$x == 2] <- "25-50%"
pred_mortbg2_abietis$mortbg2[pred_mortbg2_abietis$x == 3] <- "50-95%"
pred_mortbg2_abietis$mortbg2[pred_mortbg2_abietis$x == 4] <- ">95%"

pred_mortbg2_abietis$mortbg2 <- factor(pred_mortbg2_abietis$mortbg2,
                                   levels = c("0-5%",
                                              "5-25%",
                                              "25-50%",
                                              "50-95%",
                                              ">95%"
                                   )
)

library(ggtext)
ggplot(pred_mortbg2_abietis)+
  geom_errorbar(aes(x=mortbg2, ymin=conf.low, ymax=conf.high), color="#029dd1", width = .4)+
  geom_point(aes(x=mortbg2, y=predicted), size = 2, color="#029dd1")+
  labs(y="Predicted probability of tree infestation", x="Crown dieback class", 
       title = "Relationship between crown dieback and fir mistletoe infestation", 
       subtitle = "in the Vosges ecological region")+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
  )

ggsave("pred_mortbg2_abietis_Vosges.png", width = 10, height = 8, dpi = 300)

## test austriacum Alpes

test_gam_arbres_austriacum <- gam(
  family = binomial,
  data = subset(data_arbre_austriacum_bio, mortbg2 != "X" & ser_86 %in% c("H30", "H41", "H42")),
  formula = is_gui_bin ~ 
    s(c13, bs = "cr",  k = 4)
  + mortbg2_factor
  + te(npp_factor, bs = "re", k = 3)
  
  , weights = poids_models
)

pred_mortbg2_austriacum <- ggaverage(test_gam_arbres_austriacum, c('mortbg2_factor'))

pred_mortbg2_austriacum$mortbg2[pred_mortbg2_austriacum$x == 0] <- "0-5%"
pred_mortbg2_austriacum$mortbg2[pred_mortbg2_austriacum$x == 1] <- "5-25%"
pred_mortbg2_austriacum$mortbg2[pred_mortbg2_austriacum$x == 2] <- "25-50%"
pred_mortbg2_austriacum$mortbg2[pred_mortbg2_austriacum$x == 3] <- "50-95%"
pred_mortbg2_austriacum$mortbg2[pred_mortbg2_austriacum$x == 4] <- ">95%"

pred_mortbg2_austriacum$mortbg2 <- factor(pred_mortbg2_austriacum$mortbg2,
                                       levels = c("0-5%",
                                                  "5-25%",
                                                  "25-50%",
                                                  "50-95%",
                                                  ">95%"
                                       )
)

library(ggtext)
ggplot(pred_mortbg2_austriacum)+
  geom_errorbar(aes(x=mortbg2, ymin=conf.low, ymax=conf.high), color="#4d00bf", width = .4)+
  geom_point(aes(x=mortbg2, y=predicted), size = 2, color="#4d00bf")+
  labs(y="Predicted probability of tree infestation", x="Crown dieback class", 
       title = "Relationship between crown dieback and pine mistletoe infestation", 
       subtitle = "in the southern Alps ecological region")+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
  )

ggsave("pred_mortbg2_austriacum_AlpesSud.png", width = 10, height = 8, dpi = 300)

## relationship c13 - mistletoe infestation ##

newxy1_abiet <- data.frame(c13 = rnorm(n= 1000, mean = mean(data_arbre_abietis_bio$c13), sd = sd(data_arbre_abietis_bio$c13)),
                           mortbg2_factor = factor(c(0), levels = levels(data_arbre_abietis_bio$mortbg2_factor))
)
z1_abiet <- predict(test_gam_arbres_abietis, newdata = newxy1_abiet, newdata.guaranteed = TRUE, exclude = 'te(npp_factor)', se.fit = TRUE)
newxy1_abiet$fit <- plogis(z1_abiet$fit)
newxy1_abiet$se.fit <- plogis(z1_abiet$se.fit)
newxy1_abiet$low <- plogis(z1_abiet$fit - 1.96 * z1_abiet$se.fit)
newxy1_abiet$upp <- plogis(z1_abiet$fit + 1.96 * z1_abiet$se.fit)
newxy1_abiet$subspecies <- "abietis"


newxy1_austr <- data.frame(c13 = rnorm(n= 1000, mean = mean(data_arbre_austriacum_bio$c13), sd = sd(data_arbre_austriacum_bio$c13)),
                           mortbg2_factor = factor(c(0), levels = levels(data_arbre_austriacum_bio$mortbg2_factor))
)
z1_austr <- predict(test_gam_arbres_austriacum, newdata = newxy1_austr, newdata.guaranteed = TRUE, exclude = 'te(npp_factor)', se.fit = TRUE)
newxy1_austr$fit <- plogis(z1_austr$fit)
newxy1_austr$se.fit <- plogis(z1_austr$se.fit)
newxy1_austr$low <- plogis(z1_austr$fit - 1.96 * z1_austr$se.fit)
newxy1_austr$upp <- plogis(z1_austr$fit + 1.96 * z1_austr$se.fit)
newxy1_austr$subspecies <- "austriacum"

newxy1_album <- data.frame(c13 = rnorm(n= 1000, mean = mean(data_arbre_album_bio$c13), sd = sd(data_arbre_album_bio$c13)),
                           mortbg2_factor = factor(c(0), levels = levels(data_arbre_album_bio$mortbg2_factor))
)
z1_album <- predict(test_gam_arbres_album, newdata = newxy1_album, newdata.guaranteed = TRUE, exclude = 'te(npp_factor)', se.fit = TRUE)
newxy1_album$fit <- plogis(z1_album$fit)
newxy1_album$se.fit <- plogis(z1_album$se.fit)
newxy1_album$low <- plogis(z1_album$fit - 1.96 * z1_album$se.fit)
newxy1_album$upp <- plogis(z1_album$fit + 1.96 * z1_album$se.fit)
newxy1_album$subspecies <- "album"


predc13 <- rbind(newxy1_album,
                 newxy1_abiet,
                 newxy1_austr
)

library(ggtext)
ggplot(predc13)+
  geom_ribbon(aes(x=c13, ymin=low, ymax=upp, fill=subspecies), alpha = .3)+
  geom_line(aes(x=c13, y=fit, color=subspecies))+
  scale_color_manual(name = "<i>V.album</i> subspecies", values = c("album" = "#26ad00",
                                                                    "abietis" = "#029dd1",
                                                                    "austriacum" = "#4d00bf"
  ))+
  scale_fill_manual(name = "<i>V.album</i> subspecies", values = c("album" = "#26ad00",
                                                                   "abietis" = "#029dd1",
                                                                   "austriacum" = "#4d00bf"
  ))+
  labs(x="Tree girth [m]", y="Predicted probability of tree infestation")+
  theme_bw()+
  theme(legend.title = element_markdown(size = 14),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = c(.85, .15)
  )

ggsave("pred_c13_reduced.png", width = 10, height = 8, dpi = 300)