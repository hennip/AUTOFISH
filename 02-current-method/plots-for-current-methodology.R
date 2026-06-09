library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(rlang)

rectangles <- st_read("02-current-method/shapefile/WGBIFS_ices_subdivision_rectangles_multipolygon_EMODnet_R.shp")

plot_ices_rectangles <- function(rectangles_sf,
                                 data_df,
                                 value_col = "mean_nasc",
                                 rect_col_sf = "Rectngl",
                                 rect_col_data = "rec",
                                 species_col = "species",
                                 species_filter = NULL,
                                 title = "ICES rectangle map",
                                 subtitle = NULL,
                                 fill_label = NULL,
                                 digits = 0,
                                 label_size = 2.2,
                                 label_color = "white",
                                 label_fontface = "bold",
                                 border_color = "grey40",
                                 border_size = 0.15,
                                 na_fill = "grey95",
                                 palette_option = "magma",
                                 check_overlap = FALSE,
                                 bbox = NULL) {
  
  # --- checks ---
  stopifnot(inherits(rectangles_sf, "sf"))
  
  if (!rect_col_sf %in% names(rectangles_sf)) {
    stop(paste0("Column '", rect_col_sf, "' not found in rectangles_sf"))
  }
  if (!rect_col_data %in% names(data_df)) {
    stop(paste0("Column '", rect_col_data, "' not found in data_df"))
  }
  if (!value_col %in% names(data_df)) {
    stop(paste0("Column '", value_col, "' not found in data_df"))
  }
  
  # --- optional species filtering ---
  if (!is.null(species_filter)) {
    if (!species_col %in% names(data_df)) {
      stop(paste0(
        "species_filter was supplied, but column '", species_col,
        "' was not found in data_df"
      ))
    }
    
    data_df <- data_df %>%
      filter(.data[[species_col]] == species_filter)
  }
  
  # --- join data ---
  joined <- rectangles_sf %>%
    left_join(data_df, by = setNames(rect_col_data, rect_col_sf))
  
  # --- label points inside polygons ---
  label_points <- st_point_on_surface(joined) %>%
    mutate(
      label_text = ifelse(
        is.na(.data[[value_col]]),
        "",
        format(round(.data[[value_col]], digits), nsmall = digits, trim = TRUE)
      )
    )
  
  # --- default legend label ---
  if (is.null(fill_label)) {
    fill_label <- value_col
  }
  
  # --- if species filter used and subtitle missing, create one ---
  if (!is.null(species_filter) && is.null(subtitle)) {
    subtitle <- paste("Species:", species_filter)
  }
  
  # --- plot ---
  p <- ggplot() +
    geom_sf(
      data = joined,
      aes(fill = .data[[value_col]]),
      color = border_color,
      linewidth = border_size
    ) +
    geom_sf_text(
      data = label_points %>% filter(!is.na(.data[[value_col]])),
      aes(label = label_text),
      size = label_size,
      color = label_color,
      fontface = label_fontface,
      check_overlap = check_overlap
    ) +
    scale_fill_viridis_c(
      option = palette_option,
      na.value = na_fill
    ) +
    theme_minimal(base_size = 12) +
    labs(
      title = title,
      subtitle = subtitle,
      fill = fill_label
    )
  
  # --- optional zoom ---
  # bbox = c(xmin, xmax, ymin, ymax)
  if (!is.null(bbox)) {
    p <- p + coord_sf(
      xlim = c(bbox[1], bbox[2]),
      ylim = c(bbox[3], bbox[4]),
      expand = FALSE
    )
  }
  
  return(p)
}

#use for nasc
p_nasc <- plot_ices_rectangles(
  rectangles_sf = rectangles,
  data_df = df_nasc_per_rec,
  value_col = "mean_nasc",
  rect_col_sf = "Rectngl",
  rect_col_data = "rec",
  title = "Mean NASC per ICES Rectangle",
  subtitle = "Baltic Sea overview",
  fill_label = "Mean NASC",
  digits = 0,
  label_size = 2.1,
  label_color = "white",
  label_fontface = "bold",
  check_overlap = FALSE
)

print(p_nasc)

#hauls per rect
p_hauls <- plot_ices_rectangles(
  rectangles_sf = rectangles,
  data_df = df_n_hauls_per_rec,
  value_col = "n_hauls_per_rec",
  rect_col_sf = "Rectngl",
  rect_col_data = "rec",
  title = "Hauls per ICES Rectangle",
  subtitle = "Baltic Sea overview",
  fill_label = "hauls",
  digits = 0,
  label_size = 2.1,
  label_color = "white",
  label_fontface = "bold",
  check_overlap = FALSE
)

print(p_hauls)

#for herring:
p_her <- plot_ices_rectangles(
  rectangles_sf = rectangles,
  data_df = df_p_species_per_rec,
  value_col = "p_species_per_rec",
  rect_col_sf = "Rectngl",
  rect_col_data = "rec",
  species_col = "species",
  species_filter = "126417",
  title = "Mean percentage of herring per ICES Rectangle",
  fill_label = "%",
  digits = 0,
  label_size = 2.0
)

print(p_her)

#for sprat:
p_spr <- plot_ices_rectangles(
  rectangles_sf = rectangles,
  data_df = df_p_species_per_rec,
  value_col = "p_species_per_rec",
  rect_col_sf = "Rectngl",
  rect_col_data = "rec",
  species_col = "species",
  species_filter = "126425",
  title = "Mean percentage of sprat per ICES Rectangle",
  fill_label = "%",
  digits = 0,
  label_size = 2.0
)

print(p_spr)

#for stickleback:
p_gta <- plot_ices_rectangles(
  rectangles_sf = rectangles,
  data_df = df_p_species_per_rec,
  value_col = "p_species_per_rec",
  rect_col_sf = "Rectngl",
  rect_col_data = "rec",
  species_col = "species",
  species_filter = "126505",
  title = "Mean percentage of stickleback per ICES Rectangle",
  fill_label = "%",
  digits = 0,
  label_size = 2.0
)

print(p_gta)

#for cod:
p_cod <- plot_ices_rectangles(
  rectangles_sf = rectangles,
  data_df = df_p_species_per_rec,
  value_col = "p_species_per_rec",
  rect_col_sf = "Rectngl",
  rect_col_data = "rec",
  species_col = "species",
  species_filter = "126436",
  title = "Mean percentage of cod per ICES Rectangle",
  fill_label = "%",
  digits = 0,
  label_size = 2.0
)

print(p_cod)

maps <- paste0(path_output, "BIAS_results_ices_maps", choose_year, ".pdf")

#save all plots to pdf
pdf(maps, width = 10, height = 8)
print(p_nasc)
print(p_hauls)
print(p_her)
print(p_spr)
print(p_cod)
print(p_gta)
dev.off()
getwd()
