library(tidyverse)
library(scales)
library(ggthemes)
library(viridis)

focus.col <- "#9C179E"
fill.col <- "gray80"
text.col <- "gray48"
title.col <- "gray26"
fontfam <- "sans"
text.fontfam <- "Palatino"

theme_foraging_map <- function() {
  # theme colors
  plot.bg <- "white"
  panel.bg <- plot.bg
  text.col <- text.col
  plot.title.col <- text.col
  axis.title.col <- text.col
  axis.text.col <- text.col
  axis.tick.col <- text.col
  grid.col <- "gray80"

  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0,0,0,0, unit="cm"),
        text = element_text(family=fontfam),
        plot.background = element_rect(fill = plot.bg, colour = plot.bg),
        panel.background = element_rect(fill = panel.bg),
        plot.title = element_text(colour = plot.title.col,
                                  family=text.fontfam,
                                  face = "bold", size = 18, vjust = 1),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(size=18)
        )
}


theme_custom_2 <- function() {
  # theme colors
  plot.bg <- "white"
  panel.bg <- plot.bg
  text.col <- text.col
  plot.title.col <- text.col
  axis.title.col <- text.col
  axis.text.col <- text.col
  axis.tick.col <- text.col
  grid.col <- "gray80"


  theme_wsj() +
  theme(
    plot.margin = margin(0.75, 0.75, 0.5, 0.75, "cm"),    
    text = element_text(family=fontfam),
    plot.background = element_rect(fill = plot.bg, colour = plot.bg),
    panel.background = element_rect(fill = panel.bg),
    axis.text = element_text(colour = axis.text.col, size=18),
    plot.title = element_text(colour = plot.title.col, face = "bold", size = 18, vjust = 1),
    axis.title = element_text(colour = axis.title.col, face = "bold", size = 18),
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = grid.col),
    #panel.grid.minor.y = element_blank(),
    #panel.spacing = margin(1, 2, 1, 2, unit='cm'),
    strip.text = element_text(size=18),
    #strip.background = element_rect(fill = main_text_col),
    axis.ticks = element_line(colour = axis.tick.col)
  )
}

theme_custom <- function() {
  # theme colors
  plot.bg <- "white"
  panel.bg <- plot.bg
  text.col <- text.col
  plot.title.col <- text.col
  axis.title.col <- text.col
  axis.text.col <- text.col
  axis.tick.col <- text.col
  #grid.col <- grid_col


  theme_light() +
  theme(
    plot.margin = margin(0.75, 0.75, 0.5, 0.75, "cm"),    
    text = element_text(family=fontfam),
    plot.background = element_rect(fill = plot.bg, colour = plot.bg),
    panel.background = element_rect(fill = panel.bg),
    axis.text = element_text(colour = axis.text.col, size=18),
    plot.title = element_text(colour = plot.title.col, face = "bold", size = 18, vjust = 1),
    axis.title = element_text(colour = axis.title.col, face = "bold", size = 18),
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank(),
    #panel.grid.major.y = element_line(colour = grid.col),
    #panel.grid.minor.y = element_blank(),
    #panel.spacing = margin(1, 2, 1, 2, unit='cm'),
    strip.text = element_text(size=18),
    #strip.background = element_rect(fill = main_text_col),
    axis.ticks = element_line(colour = axis.tick.col)
  )
}


position.map <- function(dataset) {
  pmap <- ggplot(dataset,
                 aes(x=x,
                     y=y)) +
                 stat_bin2d(bins=200, aes(alpha=..count..)) +
                 guides(color=F, alpha=F, fill=F) +
                 labs(x="", y="") +
                 xlim(-15, 15) +
                 ylim(-15, 15) +
                 scale_fill_viridis(name="Target Count", option="plasma") +
                 scale_alpha(range=c(0,1), limits=c(0,50)) +
                 theme_foraging_map()
}
