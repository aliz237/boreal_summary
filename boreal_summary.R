library(optparse)
library(dplyr)
library(fs)
library(stringr)
library(rockchalk)
library(terra)

set_output_file_names <- function(predict_var, tile_num, year){
  key <- if (predict_var == 'AGB') 'agb' else 'ht'
  out_fn_stem = paste(
    paste0('output/boreal_', key, '_', year), format(Sys.time(),"%Y%m%d%s"), str_pad(tile_num, 7, pad = "0"),
    sep="_"
  )

  fn_suffix <- c('.tif', '_summary.csv', '_train_data.csv', '_stats.Rds', '_model.Rds')
  names <- c('map', 'summary', 'train', 'stats', 'model')

  output_file_names <- paste0(out_fn_stem, fn_suffix)
  names(output_file_names) <- names

  return(output_file_names)
}

write_ATL08_table <- function(target, df, out_file_path){
    out_columns <- if(target=='AGB') c('lon', 'lat', 'AGB', 'SE') else c('lon', 'lat', 'h_canopy')
    write.csv(df[, out_columns], file=out_file_path, row.names=FALSE)
}

write_output_summaries <-function(tile_summaries, boreal_summaries, target, output_fn){
  if (target == 'AGB')
    column_names <- c('tile_total', 'boreal_total')
  else
    column_names <- c('tile_mean', 'boreal_mean')

  df <- data.frame(tile_summaries, boreal_summaries)
  names(df) <- column_names

  write.csv(df, output_fn, row.names=FALSE)
}

tile_and_boreal_summary <- function(map, predict_var, boreal_poly, summary_and_convert_functions){
  convert_fun <- summary_and_convert_functions[['convert_fun']]
  summary_fun <- summary_and_convert_functions[['summary_fun']]

  map_conv <- if (is.null(convert_fun)) map else app(map, convert_fun)

  tile_summary <- global(map_conv, summary_fun, na.rm=TRUE)[[summary_fun]]

  boreal <- extract(map_conv, boreal_poly, fun=summary_fun, na.rm=TRUE)
  boreal_summary <- if(summary_fun=='sum') sum(boreal$lyr.1, na.rm=TRUE) else boreal$lyr1[1]

  return(list(tile_summary=tile_summary, boreal_summary=boreal_summary))
}


get_summary_and_convert_functions <- function(predict_var){
  if (predict_var == 'AGB'){
    summary_fun <- 'sum'
    convert_fun <- function(x){(x*0.09)/1000000000}
  }
  else{
    summary_fun <- 'mean'
    convert_fun <- NULL
  }
  return(list(summary_fun=summary_fun, convert_fun=convert_fun))
}

prepare_raster <- function(path, subset_bands=NULL, extra_bands=NULL, dest_raster=NULL){
  raster <- rast(path)
  raster_bands <- names(raster)

  if (!is.null(subset_bands))
    raster_bands <- intersect(raster_bands, subset_bands)

  if (!is.null(extra_bands))
    raster_bands <- c(raster_bands, extra_bands)

  raster <- subset(raster, raster_bands)

  if (!is.null(dest_raster))
    raster <- resample_if_needed(raster, dest_raster)

  return(raster)
}

resample_reproject_and_mask <- function(agb_path, lc_path){
  lc <- prepare_raster(lc_path, dest_raster=agb_path)
  agb <- rast(agb_path)$lyr.1
  stack <- c(agb, lc)
  return(stack)
}

mask_input_stack <- function(stack){
  MASK_LYR_NAMES = c('slopemask', 'ValidMask')
  MASK_LANDCOVER_NAMES = c(50, 60, 70, 80)

  print("Masking stack...")
  # Bricking the stack will make the masking faster (i think)
  # brick = rast(stack)
  for(LYR_NAME in MASK_LYR_NAMES){
    m <- terra::subset(stack, grep(LYR_NAME, names(stack), value = T))
    stack <- mask(stack, m == 0, maskvalue=TRUE)
  }

  for(LC_NAME in MASK_LANDCOVER_NAMES){
    n <- terra::subset(stack, grep('esa_worldcover_v100_2020', names(stack), value=LC_NAME))
    stack <- mask(stack, n == LC_NAME, maskvalue=TRUE)
  }

  return(stack)
}

boreal_summary <- function(agb_path, lc_path, boreal_vector_path, tile_num, year, predict_var){
  stack <- resample_reproject_and_mask(agb_path, lc_path)
  boreal_poly <- project(vect(boreal_vector_path), crs(stack))
  summary_and_convert_functions=get_summary_and_convert_functions(predict_var),

  results <- NULL 

  output_fns <- set_output_file_names(predict_var, tile_num, year)

  write_ATL08_table(predict_var, results[['train_df']], output_fns[['train']])
  write_single_model_summary(results[['model']], results[['train_df']],  predict_var, output_fns)

  print('AGB successfully predicted!')
  write_output_summaries(results[['tile_summary']], results[['boreal_summary']], predict_var,  output_fns[['summary']])

}

option_list <- list(
  make_option(
    c("-a", "--agb_path"), type = "character",
    help = "Path to the agb or ht file"
  ),
  make_option(
    c("-l", "--lc_path"), type = "character",
    help = "Path to the land cover mask file"
  ),
  make_option(
    c("-v", "--boreal_vector_path"), type = "character",
    help = "Path to the boreal vector file",
    ),
  make_option(
    c("-y", "--year"), type = "character",
    help = "Year of the input HLS imagery"
  ),
  make_option(
    c("-t", "--tile_num"), type = "character",
    help = "tile number"
  ),
  make_option(
    c("--predict_var"), type = "character", default = "AGB",
    help = "Variable to predict, it can be either AGB or Ht [default: %default]"
  ),
  make_option(
    c("--help"), action = "store_true",
    help = "Show help message"
  )
)

opt_parser <- OptionParser(option_list = option_list, add_help_option = FALSE)
opt <- parse_args(opt_parser)

cat("Parsed arguments:\n")
print(opt)

if (!is.null(opt$help)) {
  print_help(opt_parser)
}
do.call(boreal_summary, opt)
