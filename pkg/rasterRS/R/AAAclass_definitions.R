
setClass("RSInfo",
		representation(
				# ENVI header info
				band_names="character",
				bad_band_multipliers="numeric",
				class_lookup="character",# Check this
				class_names="character",
				classes="numeric",
				data_gain_values="numeric",
				data_offset_values="numeric",
				default_bands="numeric",
				default_stretch="character",
				dem_file="RasterStack",
				dem_band="numeric",
				description="character",
				scale_factor="numeric", # Coerces to reflectance between 0 and 1.
				sensor_ID="character"
				sensor_type="character",
				platform="character",

				# Time series info
				band_start_time="POSIXct",
				band_end_time="POSIXct",
				band_mid_time="POSIXct",
				
				# Band info
				zaxis="numeric",
				fwhm="numeric",
				zaxis_units="character",
				spectral_response_curves="list"
		)
)

setClass("RasterRS",
	contains=c("RasterStack",
			"RSInfo")
)