library(grid)
library(extrafont)
library(Cairo)

loadfonts()

source("data.R")
source("graphs.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid_foraging_map <- function(foraging_map, title="") {
  ### Grid layout stuff
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(9, 8)))

  # Graphs
  print(foraging_map, vp = vplayout(2:9, 1:8))

  # Title
  grid.text(title,
            vp=vplayout(1, 1:8),
            y=unit(0.6, "npc"),
            gp=gpar(fontfamily=text.fontfam,
                    fontface="bold",
                    col=title.col, cex=2))
}

grid_clustered_text <- function() {
  ## MAIN TEXT
  block.vp <- vplayout(1, 3:6)
  xval <- 0.5
  yval <- 0.2
  line.gap <- 0.2
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }
  block.cex <- 1.3
  block.hjust = 0.5
  grid.text(expression("The distribution of effort for simulated ants foraging in an environment"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(yval, "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("with clustered seeds. The foraging attention noticeably adheres to the"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 1), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("seed clusters, as seen in the " *
                       phantom("dark patches") *
                       "."),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression(phantom("seed clusters, as seen in the ") *
                       "dark patches" *
                       phantom(".")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=focus.col, cex=block.cex))
}

grid_power_text <- function() {
  ## MAIN TEXT
  block.vp <- vplayout(1, 3:6)
  xval <- 0.5
  yval <- 0.2
  line.gap <- 0.2
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }
  block.cex <- 1.3
  block.hjust = 0.5
  grid.text(expression("The distribution of effort for simulated ants foraging in an environment"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(yval, "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("with power-law-distributed seeds, meaning most of the seeds are in a few"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 1), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("large clusters, but there are many small clusters as well."),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
}

grid_random_text <- function() {
  ## MAIN TEXT
  block.vp <- vplayout(1, 3:6)
  xval <- 0.5
  yval <- 0.2
  line.gap <- 0.2
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }
  block.cex <- 1.3
  block.hjust = 0.5
  grid.text(expression("The distribution of effort for simulated ants foraging in an environment with"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(yval, "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("randomly distributed seeds. Here, individual seeds are placed uniformly and"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 1), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("independently at random throughout the environment with no clustering."),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
}

make_foraging_map <- function(fname, foraging_map, text_fn, device=pdf, ...) {
  device(fname, family=fontfam, width=12, height=13)
  grid_foraging_map(foraging_map, ...)
  text_fn()
  dev.off()
}

make_clustered_pdf <- function() {
  title <- "Clustered Seed Distribution"
  fname <- "foraging_map_clustered.pdf"
  make_foraging_map(fname, position.map(clustered.data()),
                    grid_clustered_text, title=title)
  embed_fonts(fname)
}

make_clustered_svg <- function() {
  title <- "Clustered Seed Distribution"
  fname <- "foraging_map_clustered.svg"
  make_foraging_map(fname, position.map(clustered.data()),
                    grid_clustered_text, device=svg,
                    title=title)
}


make_power_pdf <- function() {
  title <- "Power Law Seed Distribution"
  fname <- "foraging_map_power.pdf"
  make_foraging_map(fname, position.map(power.data()),
                    grid_power_text, title=title)
  embed_fonts(fname)
}

make_power_svg <- function() {
  title <- "Power Law Seed Distribution"
  fname <- "foraging_map_power.svg"
  make_foraging_map(fname, position.map(power.data()),
                    grid_power_text, device=svg,
                    title=title)
}


make_random_pdf <- function() {
  title <- "Random Seed Distribution"
  fname <- "foraging_map_random.pdf"
  make_foraging_map(fname, position.map(random.data()),
                    grid_random_text, title=title)
  embed_fonts(fname)
}

make_random_svg <- function() {
  title <- "Random Seed Distribution"
  fname <- "foraging_map_random.svg"
  make_foraging_map(fname, position.map(random.data()),
                    grid_random_text, device=svg,
                    title=title)
}
