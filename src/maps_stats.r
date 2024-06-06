# Species1 = Lepus timidus; Species2 = Lepus europaeus

# packages
#install.packages("ggcorrplot")
#install.packages("corrplot")
#install.packages("pheatmap")
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggfortify)
library(vegan)
library(plotly)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(pheatmap)
library(dplyr)
library(fmsb)
library(emmeans)
#install.packages("patchwork")
library(patchwork) # To display 2 charts together

######################################################################################################
# GGplot: DISTRIBUTION OF THE TWO SPECIES AS A FUNCTION OF ELEVATION (3D_maps_renderpoints.r)
ggplot_elevation <- ggplot(matrix_full_elev, aes(x=elevation_points, fill= species)) +
  geom_density(adjust = 1,alpha=0.5)+
      labs(
        title="Distribution of the two species as a function of elevation",
        x="elevation",
        y="Count of observations") + 
  scale_fill_manual(values=c("darkgreen","darkred","darkblue"))+
  geom_histogram(aes(y=..density..), alpha=0.3, position="identity")+
  theme_classic()
print(ggplot_elevation)
Sys.sleep(3)
# We can see that Lepus europaeus is mainly distributed under 1000m and Lepus timidus above 1300m
# However, we can see a rather large zone a overlap between both species --> contact zone/potential hybridization zone

#######################################################################################################
# BOXPLOT: DISTRIBUTION OF THE TWO SPECIES AS A FUNCTION OF ELEVATION (3D_maps_renderpoints.r)
# RGRAPH GALLERY
boxplot_elevation <- ggplot(matrix_full_elev, aes(x=species, y=elevation_points, fill= species)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_ipsum() +
    theme(
      plot.title = element_text(size=11)
    ) +
    ggtitle("Distribution of the two species as a function of elevation") +
    xlab ("")
print(boxplot_elevation)
Sys.sleep(3)
# Lepus timidus at higher elevation than Lepus europaeus but still a lot of points at mid elevation

  # stat
  lepus_timidus_elev <- subset(matrix_full_elev, species == "Lepus timidus")$elevation_points
  lepus_europaeus_elev <- subset(matrix_full_elev, species == "Lepus europaeus")$elevation_points
  # Wilcoxon
  test_result <- wilcox.test(lepus_timidus_elev, lepus_europaeus_elev)
  print(test_result)
  # H0 = distribution of the two species as a function of altitude is identical
  # H1 = different
  # p-value = < 2.2e-16 => reject H0

########################################################################################################
# GGplot: Counted observations of both species as a function of Landcover (ecosystems.r)
ggplot_ecosystem <- ggplot(matrix_full_elev_eco, aes(x=as.factor(Landcover), fill=species)) +
    geom_bar(position="dodge") +
    labs(
        title="Distribution of the two species as a function of Landcover",
        x="Landcover",
        y="Count of observations") + 
    theme(plot.title = element_text(size=25, face="bold"),        # Title text size
          axis.title.x = element_text(size=20, face="bold"),      # X-axis title text size
          axis.title.y = element_text(size=20, face="bold"),      # Y-axis title text size
          axis.text.x = element_text(face="bold", size=25, angle=45),  # X-axis text size
          axis.text.y = element_text(size=20, face="bold")        # Y-axis text size
    )
print(ggplot_ecosystem)
Sys.sleep(3)
# Overlap of the distribution niche of the two species in majority in forest and grassland.
# We can see that Lepus europaeus is not occuring yet at higher elevation (shrubland, snow and ice, sparsely or non vegetated)

  # stat
  # contigency table
  contingency_table <- table(matrix_full_elev_eco$species, matrix_full_elev_eco$Landcover)
  # chi squared test
  chi_square_test <- chisq.test(contingency_table)
  print(chi_square_test)
  # H0 = distribution of species independant from Landcover
  # p -value = < 2.2e-16 => reject H0

######### Data Aggregation and Formatting for Plots

# Use the original dataset for data aggregation
data_stat <- matrix_full_elev_eco_clim_ndvi
# Aggregate data by W_Ecosystm
aggregated_data <- aggregate(
  cbind(elevation_points, vec_mean_precip, vec_mean_temp, NDVI) ~ W_Ecosystm, 
  data = data_stat, 
  FUN = mean
)
#print(aggregated_data)

# Extract species and W_Ecosystm columns
data_stat_discrete <- data_stat[c("species", "W_Ecosystm")]
# Merge aggregated data with discrete data by W_Ecosystm
aggregated_data_final_species <- merge(aggregated_data, data_stat_discrete, by = "W_Ecosystm")

# Ensure unique rows in the final aggregated data
aggregated_data_final_species <- aggregated_data_final_species %>% distinct()
# Further analysis
# Create a boxplot for temperature by landcover type
  P_fact <- ggplot(data = data_stat, mapping = aes(x = Landcover, y = temp, fill = Landcover))

  P_fact <- P_fact + geom_boxplot(varwidth = TRUE, outlier.shape = NA) +  # Change boxplot width 
  geom_jitter(alpha = 0.2, size = 2, width = 0.1) +  # Add points and spread them
  stat_summary(fun = mean, shape = 13, size = 1, colour = "darkgreen") +  # Add mean 
  theme_classic()

  print(P_fact)
  Sys.sleep(3)

  # interactive plot
  library(plotly)
  ggplotly(P_fact)
  Sys.sleep(3)

  # Fit a linear model with landcover as a factor
  linear_model <- lm(temp ~ Landcover, data = data_stat)

  anova(linear_model)

  # Conduct post-hoc tests with Tukey adjustment
  em <- emmeans(linear_model, list(pairwise ~ Landcover), adjust = "tukey")
  print(em)
  # significative different of temperature between a lot of landcover types except for: 
  # - cropland/settlement
  # - cropland/shrubland
  # - forest/grassland
  # - forest/shrubland
  # - settlement/shrubland
  # - snow and ice/sparsely or non vegetated

#########################################################################################################
# Distribution of the two species as a function of mean temperature (climatic_data.r)
ggplot_temp <- ggplot(matrix_full_elev_eco_clim, aes(x=vec_mean_temp, fill= species)) +
  geom_density(adjust = 1,alpha=0.5)+
      labs(
        title="Distribution of the two species as a function of temperature",
        x="temperature",
        y="Count of observations") + 
  scale_fill_manual(values=c("darkgreen","darkred","darkblue"))+
  geom_histogram(aes(y=..density..), alpha=0.3, position="identity")+
  theme_classic()
print(ggplot_temp)
Sys.sleep(3)
# Lepus timidus mainly distributed at lower temperatures than Lepus europaeus
# Overlap of occurence points between the 2 species from 2 to 10 °C

#########################################################################################################
# RIDGELINE PLOT - RGRAPH GALLERY: Distribution of the two species as a function of mean temperature

#install.packages("ggridges")
library(ggridges)
library(ggplot2)
  
ggplot(matrix_full_elev_eco_clim_ndvi, aes(x = vec_mean_temp, y = species, fill = species)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

    # stat
  lepus_timidus_temp <- subset(matrix_full_elev_eco_clim, species == "Lepus timidus")$vec_mean_temp
  lepus_europaeus_temp <- subset(matrix_full_elev_eco_clim, species == "Lepus europaeus")$vec_mean_temp
  # Wilcoxon
  test_result <- wilcox.test(lepus_timidus_temp, lepus_europaeus_temp)
  print(test_result)
  # H0 = distribution of the two species as a function of temperature is identical
  # H1 = different
  # p-value = < 2.2e-16 => reject H0

##########################################################################################################
# Distribution of the two species as a function of mean precipitations (climatic_data.r)
ggplot_precip <- ggplot(matrix_full_elev_eco_clim, aes(x=vec_mean_precip, fill= species)) +
  geom_density(adjust = 1,alpha=0.5)+
      labs(
        title="Distribution of the two species as a function of precipitations",
        x="precipitations",
        y="Count of observations") + 
  scale_fill_manual(values=c("darkgreen","darkred","darkblue"))+
  geom_histogram(aes(y=..density..), alpha=0.3, position="identity")+
  theme_classic()
print(ggplot_precip)
Sys.sleep(3)
# Two "peaks" for both species
# Lepus timidus seems to occur at higher levels of precipitations (maybe rain + snow)
# Strong graphical overlaping between the two species

  # stat
  lepus_timidus_precip <- subset(matrix_full_elev_eco_clim, species == "Lepus timidus")$vec_mean_precip
  lepus_europaeus_precip <- subset(matrix_full_elev_eco_clim, species == "Lepus europaeus")$vec_mean_precip
  # Wilcoxon
  test_result <- wilcox.test(lepus_timidus_precip, lepus_europaeus_precip)
  print(test_result)
  # H0 = distribution of the two species as a function of precipitations is identical
  # H1 = different
  # p-value = < 2.2e-16 => reject H0

##########################################################################################################
# SCATTER PLOT: Distribution of the two species as a function of precipitations and temperature (climatic_data.r)
ggplot_climatic <- ggplot(matrix_full_elev_eco_clim, aes(x = vec_mean_precip,y=vec_mean_temp, color = species)) +
  geom_point() +
  theme_minimal()
print(ggplot_climatic)
Sys.sleep(3)
# Lepus europaeus occurs at higher mean temperature and lower mean precipitations
# Lepus timidus occurs at lower mean temperature and higher mean precipitations (rain and snow)

#########################################################################################################
# PCA (multidimentional_plot.r)
df <- matrix_full_elev_eco_clim_ndvi
df <- na.omit(df)
df_continous <- df[,colnames(df) %in% c("elevation_points","vc_mean_temp","vc_mean_precip","NDVI")]
df_discrete <- df[,!(colnames(df) %in% c("elevation_points","temp","vc_mean_precip","NDVI"))]
df_continous <- apply(df_continous,2,as.numeric)
pca_res <- prcomp(df_continous, scale. = TRUE)
autoplot(pca_res)

pca <- autoplot(pca_res, data = df_discrete, colour = 'species',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, frame.type = 'norm') + theme_classic()
print(pca)
Sys.sleep(3)
# The PCA plot shows a distinct separation between the two species, suggesting that the environmental variables chosen 
#(elevation, temperature, rainfall and NDVI) are effective in differentiating these two groups. 
# The first principal component is particularly influential in this separation.

##########################################################################################################
# INTERACTIVE PLOT
# preparation for an interactive plot
row.names(df_continous) <- c(1:nrow(df_continous))
dist_matt <- vegdist(df_continous, method = "euclidian")  #
D3_data_dist <- cmdscale(dist_matt, k = 3)
D3_data_dist <- data.frame(D3_data_dist)
cols <- df_discrete$species

PCOA <- ggplot(D3_data_dist, aes(x = X1, y = X2, color = cols)) +
  geom_point() + ggtitle("PCA") +
  theme_classic()
#PCOA

intercative_pcao <- ggplotly(PCOA)
print(intercative_pcao)
Sys.sleep(3)

###########################################################################################################
# line chart: evolution elevation distribution of the two species overtime
evolution_prep <- matrix_full_elev_eco_clim_ndvi[grepl("^\\d{4}-\\d{2}-\\d{2}$", matrix_full_elev$date), ] %>%
  mutate(year = format(as.Date(date), "%Y")) %>%
  # Group by species and year, and select the first occurrence point for each group
  group_by(species, year) %>%
  slice(1) %>%
  ungroup()
evolution_prep <- subset(evolution_prep, select = -date)
evolution_prep$year <- as.numeric(evolution_prep$year)
str(evolution_prep)
evolution_prep <- na.omit(evolution_prep)
#view(evolution_prep)

p1 <- ggplot(evolution_prep, aes(x = year, y = elevation_points, colour = species)) +
  geom_line(size = 1) +  # Utilisez geom_line pour tracer les lignes
  scale_color_manual(values = c("blue", "red")) +  # Définissez les couleurs manuelles pour chaque espèce
  ggtitle("Evolution of distribution of the two species as a function of elevation overtime") +
  theme_minimal()
print(p1)
Sys.sleep(3)

# drastic upward shift of Lepus europaeus in higher elevation since ~2019
# not so marked for Lepus timidus, maybe because of its maximal capacity to shift upward (resources)
# Warming