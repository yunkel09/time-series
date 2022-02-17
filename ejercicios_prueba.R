
	
	import::from(dbplyr, in_schema)
	library(fabletools)
	library(feasts)
	pacman::p_load(conectigo, lubridate, tsibble, fable, tidyverse)
	
	con <- conectar_msql()
	
	mat <- c("OSP0001142", "OSP0001440")	
	
	mis <- tbl(con, in_schema("md", "descarga_vista")) |> 
		select(fecha_uso, 
				 codigo_ebs, 
				 cantidad, 
				 estado = estado_descarga_item, 
				 ticketid = ticket,
				 material) |> 
		filter(codigo_ebs %in% mat) |> 
		collect()

	
	mat <- c("OSP0001142", "OSP0001440")	
	
	mis |> 
		group_by(fecha = floor_date(fecha_uso, unit = "day"), codigo_ebs) |> 
		summarise(cantidad = sum(cantidad), .groups = "drop") |> 
		arrange(fecha)
	