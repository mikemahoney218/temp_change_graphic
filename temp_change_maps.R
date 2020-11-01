library(raster)
library(ggplot2)
library(dplyr)
library(maps)
library(sf)
library(here)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
raster_files <- grep("bil$", list.files(here::here("data")), value = TRUE)
raster_files <- paste0("data/", raster_files)

for (i in seq(1981, 2019, 1)) {
  cur_raster <- raster(grep(i, raster_files, value = TRUE))
  cur_val <- readAll(cur_raster)
  cur_val <- cur_val@data@values
  cur_table <- as.data.frame(cur_raster, xy = TRUE)[c("x", "y")]
  cur_table$year <- i
  cur_table$temp <- cur_val

  cur_table %>%
    filter(!is.na(temp)) %>%
    ggplot() +
    geom_raster(aes(x, y, fill = temp)) +
    geom_sf(data = states, aes(geometry = geom), fill = NA, size = 0.03) +
    coord_sf() +
    scale_fill_distiller(
      palette = "RdBu",
      limits = c(-6.758, 27.412)
    ) +
    theme_void() %+replace%
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "#222222"),
      panel.background = ggplot2::element_rect(
        fill = "#222222",
        color = NA
      ),
      panel.border = element_blank(),
      panel.spacing = unit(0, "cm"),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      plot.caption = element_text(color = "#f4f4f4", hjust = 0.98)
    ) +
    labs(caption = i) +
    ggsave(here::here("maps", paste0(i, ".png")),
           width = 15,
           height = 10,
           dpi = 72,
           units = "in")
}

image_files <- list.files(here::here("maps"))
image_files <- paste0("maps/", image_files)

oldwd <- getwd()
setwd(here::here())
system(paste("convert -delay 100",
             paste0(image_files[1:38], collapse = " "),
             "-delay 500",
             image_files[39],
             "-loop 0 maps/map_anim.gif"))

setwd(oldwd)
