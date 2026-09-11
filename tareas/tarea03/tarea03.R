
# Data visualization ------------------------------------------------------
library(tidyverse)
library(palmerpenguins)
library(ggthemes)

penguins
glimpse(penguins)
#Creating a plot: ggplot defines a plot object where layers can be added
#1st layer: creates an empty canvas
ggplot (data=penguins)
#2nd layer: fixes the axes
ggplot(
  data=penguins,
  mapping = aes (x = flipper_length_mm, y = body_mass_g)
       )
#3rd layer: scatterplot
ggplot(
  data=penguins,
  mapping = aes (x = flipper_length_mm, y = body_mass_g)
  ) + 
  geom_point()
#4rd layer: Separating by species ,color=species

ggplot(
  data=penguins,
  mapping = aes (x = flipper_length_mm, y = body_mass_g, color = species)
) + 
  geom_point()
#5.1th layer: Adding the linear model
ggplot(
  data=penguins,
  mapping = aes (x = flipper_length_mm, y = body_mass_g, color =species)
  ) +
  geom_point() +
  geom_smooth(method = "lm")
#5.2th layer: Single linear model
ggplot(
  data=penguins,
  mapping = aes (x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")
#6th layer: Assigning shape to the distinction between species
ggplot(
  data=penguins,
  mapping = aes (x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes (color = species, shape = species)) +
  geom_smooth(method = "lm")
#6.2th layer: Colorblind scale
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

# Exercises ---------------------------------------------------------------
#1 Rows and columns
nrow(penguins)
ncol(penguins)
#2 Bill_depth_mm
?bill_depth_mm
#a number denoting bill depth (millimeters)
#3 scatterplot bill_depth_mm vs. bill_length_mm
ggplot(
  data=penguins,
  mapping = aes (x=bill_depth_mm, y=bill_length_mm, color = species)
)+
  geom_point()
#4 scatterplot species vs. bill_depth_mm
ggplot(
  data=penguins,
  mapping = aes (x=species, y=bill_depth_mm, color = species)
)+
  geom_point()
#4.1 better option
ggplot(
  data=penguins,
  mapping = aes (x=species, y=bill_depth_mm, color = species)
)+
  geom_boxplot()
#5 Spot the error
ggplot(data = penguins,
#      mapping = aes (x= flipper_length_mm, y=body_mass_g)) + 
  geom_point()
#6 what does na.rm do?
#it removes "not available" values.
#7Add "data come from the palmerpenguins package"
ggplot(
  data=penguins,
  mapping = aes (x=species, y=bill_depth_mm, color = species)
)+
  geom_boxplot()+
  labs(caption= "data come from the palmerpenguins package")
#8 Recreate the visualization
ggplot(
data = penguins,
mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = bill_length_mm)) +
  geom_smooth(method ="loess") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)") 
#9
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)

#10
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()
#
ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

# ggplot2 calls -----------------------------------------------------------
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()
#A concisely way of writing:
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()
#Using the pipe
penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()
#For categorical variables
ggplot(penguins, aes(x = species)) +
  geom_bar()
#Reordering by frequency
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()
#For numerical variables
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
#Changing the binwidth 
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)
#Density plots
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

# Exercises ---------------------------------------------------------------
#1
penguins |> 
  ggplot(aes(y=species))+
  geom_bar()
#2
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")

#The second is more visible
#3 What does the argument bins in the geom_histogram do?
penguins |> 
  ggplot(aes(x=body_mass_g))+
  geom_histogram(bins = 8)
#It determines the number of ranges it creates
#4 Histogram of carat
diamonds |> 
  ggplot(aes(x=carat))+
  geom_histogram(bins = 8)

diamonds |> 
  ggplot(aes(x=carat))+
  geom_histogram(bins = 20)

diamonds |> 
  ggplot(aes(x=carat))+
  geom_histogram(bins = 40)

diamonds |> 
  ggplot(aes(x=carat))+
  geom_histogram(bins = 100)

# Visualizing relationships -----------------------------------------------
#boxplot
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()
#Density
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)
#filling the density curves
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.25)
#Two categorical variables
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()
#Position fill
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
#Adding labs
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill") +
  labs(y = "proportion")
#facet wrap
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)
#Saving plots
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.png")