

RasterRS <- function(x,
		# Basic info
		description=as.character(NULL),
		NAflag=NA,
		# Classification info
		class_lookup=as.character(NULL),# Check this
		class_names=as.character(NULL),
		classes=as.numeric(NULL),
		# Band info
		band_names=names(x),
		bad_band_multipliers=rep(1,nlayers(x)),
		data_gain_values=rep(1,nlayers(x)),
		data_offset_values=rep(0,nlayers(x)),
		# Plotting info
		default_bands=c(3,2,1),
		default_stretch=as.character(NULL),
		# Associated DEM info
		dem_file=stack(raster(as.matrix(NA))),
		dem_band=1,
		# Spectral info
		zaxis=seq(nlayers(x)),
		fwhm=as.numeric(NULL),
		zaxis_units="BandNumber",
		spectral_response_curves=as.list(NULL),
		scale_factor=rep(1,nlayers(x)), # Coerces to reflectance between 0 and 1.
		# Sensor info
		sensor_ID="Unknown",
		sensor_type="Unknown",
		platform="Unknown",
		# Time series info
		band_start_time=as.POSIXct(NA),
		band_end_time=as.POSIXct(NA),
		band_mid_time=as.POSIXct(NA)
	)
		
{
	
	parameter_names <- ls()
	# Make sure the object is a Raster* and coerce it to a stack.
	if(is.Raster(x))
	{
		filenames <- raster_to_filenames(x,unique=TRUE)
		if(class(x)!="RasterStack") x <- stack(x)
	} else
	{
		stop("x must be a Raster* object.")
	}
	
	# Create a new object
	RasterRS_obj <- as(x,"RasterRS")
	
	# Check for file metadata.  Right now, only ENVI files are supported...
	if(length(filenames)==1)
	{
		xGDAL <- GDAL.open(filenames)
		xDriver <- getDriverName(getDriver(xGDAL))
		GDAL.close(xGDAL)
		
		if(xDriver=="ENVI")
		{
			envi_header_data <- read_ENVI_header(filenames,returnRSSlots=TRUE)
			list2env(envi_header_data,envir=environment())
		}
	}
	
	parameter_slot_intersect <- intersect(parameter_names,slotNames(RasterRS_obj)) 
	
	# Insert slots...
	for(i in seq(parameter_slot_intersect))
	{
		slot(RasterRS_obj,parameter_slot_intersect[i]) <- get(parameter_slot_intersect[i])
	}
	
	return(RasterRS_obj)
}