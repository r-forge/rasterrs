read_ENVI_header <- function(x,returnRSSlots=FALSE)
{
	if(is.Raster(x))
	{
		x <- filename(x)
	}
	
	# Check to make sure the header is chosen.
	if(class(x)=="character")
	{
		if(length(grep(".hdr",x))==1)
		{
			header=x
			if(!file.exists(header))
			{
				stop("No file with that name present...")
			}
		} else
		{
			# Try the two standard formats:
			if(file.exists(paste(x,".hdr",sep="")))
			{
				header=paste(x,".hdr",sep="")
			} else
			{
				periodPos <- gregexpr("\\.",x)[[1]]
				periodPosLast <- periodPos[length(periodPos)]
				header_temp <- paste(substring(x,1,periodPosLast),"hdr",sep="")
				if(file.exists(header_temp))
				{
					header <- header_temp
				} else
				{
					stop("No file with that name present...")
				}
			}
		}
	}
	
	
	# Read the header:
	header_raw <- readLines(header)
	
	# Search for "=" in each string to determine
	#	headers:
	
	equal_pos = regexpr("=",header_raw)
	start_lines=seq(header_raw)[equal_pos!=-1]
	end_lines=c(start_lines[-1]-1,start_lines[length(start_lines)])
	
	header_to_list_raw <- sapply(X=seq(start_lines),
			FUN=function(X,header_raw,start_lines,end_lines)
			{
				start_line=start_lines[X]
				end_line=end_lines[X]
				header_raw_sub=paste(header_raw[start_line:end_line],collapse="")
				
				header_split_pos <- regexpr("=",header_raw_sub)
				variable_name_raw <- substr(header_raw_sub,1,(header_split_pos-1))
				# Cribbed from http://stackoverflow.com/questions/2261079/whitespace-in-r
				variable_name <- gsub("^\\s+|\\s+$", "", variable_name_raw)
				
				header_data_raw <- substring(header_raw_sub,(header_split_pos+1))
				header_data <- gsub("^\\s+|\\s+$", "", header_data_raw)
				
				# Search for brackets
				brace_pos <- regexpr("\\{",header_data)
				
				if(brace_pos!=-1)
				{
					bracket_pos <- regexpr("\\[",header_data)
					parenth_pos <- regexpr("\\(",header_data)
					comma_pos <- regexpr("\\,",header_data)
					
					header_data <- gsub("^\\{+|\\}+$", "", header_data)
					header_data <- gsub("^\\s+|\\s+$", "", header_data)
					# For now, if there are parentheses, brackets, or
					#	no commas, don't do anything.
					if(bracket_pos == -1 && parenth_pos == -1 && comma_pos != -1)
					{
						header_data_split <- strsplit(header_data,",")[[1]]
						header_data <- gsub("^\\s+|\\s+$", "", header_data_split)
					}
				} 
				
				header_data <- type.convert(header_data,as.is=TRUE)
				return(list(variable_name,header_data))
			},
			header_raw,start_lines,end_lines,simplify=FALSE)
	
	header_data_list <- sapply(header_to_list_raw,function(X) X[[2]],simplify=FALSE)
	variable_names_all <- sapply(header_to_list_raw,function(X) X[[1]],simplify=TRUE)
	names(header_data_list) <- variable_names_all
	
	if(returnRSSlots)
	{
		ENVI_header_to_RSSlot_lookup <- as.data.frame(matrix(c(
						"band names","band_names",
						"bb1","bad_band_multipliers",
						"class lookup","class_lookup",
						"class names","class_names",
						"classes","classes",
						"data gain values","data_gain_values",
						"data ignore value","NAflag",
						"data offset values","data_offset_values",
						"default bands","default_bands",
						"default stretch","default_stretch",
						"dem band","dem_band",
						"dem file","dem_file",
						"description","description",
						"fwhm","fwhm",
						"reflectance scale factor","scale_factor",
						"sensor type","sensor_ID",
						"wavelength","zaxis",
						"wavelength units","zaxis_units"
				),ncol=2,byrow=TRUE
		),stringsAsFactors =FALSE)
		names(ENVI_header_to_RSSlot_lookup) <- c("ENVI_variable_names","RSSlots")
		
		header_data_list_sub <- header_data_list[names(header_data_list) %in% ENVI_header_to_RSSlot_lookup$ENVI_variable_names]
		
		header_data_list_sub_names <- sapply(names(header_data_list_sub),
			function(X) {
				ENVI_header_to_RSSlot_lookup$RSSlots[
						ENVI_header_to_RSSlot_lookup$ENVI_variable_names==X
				]
			}
		)
		names(header_data_list_sub) <- header_data_list_sub_names
		return(header_data_list_sub)
		
	} else
	{
		return(header_data_list)
	}
}

