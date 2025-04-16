library(MOSAIC)

#PATHS <- MOSAIC::get_paths(root="path-to-root-dir")

default_args <- MOSAIC::get_default_LASER_config(PATHS)

# Define output file paths for all seven formats
file_paths <- list(
     file.path(getwd(), 'inst/extdata/default_parameters.json'),
     file.path(getwd(), 'inst/extdata/default_parameters.json.gz')
)



# Loop over the file paths and call make_LASER_config() for each
for (fp in file_paths) {

     args <- default_args
     args$output_file_path <- fp
     do.call(MOSAIC::make_LASER_config, args)
     rm(args)

}

config <- jsonlite::fromJSON(file_paths[[1]])

identical(default_args, config)
all.equal(default_args, config, tolerance = 1e-8)
