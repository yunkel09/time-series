
	
	import::from(dbplyr, in_schema)
	library(fabletools)
	library(feasts)
	pacman::p_load(conectigo, lubridate, tsibble, fable, tidyverse)
	
	options(warn = -1,
			  dplyr.summarise.inform = FALSE,
			  tibble.print_min = 25,
			  readr.show_col_types = FALSE)
	
	
	show_in_excel <- function(.data) {
		tmp <- paste0(tempfile(), ".csv")
		write.csv(.data, tmp)
		fs::file_show(path = tmp)
	}
	
	
	con <- conectar_msql()
	
	mufas_vkoms <- c("OSP0001142", "OSP0001440", "OSP0000193", "ACC005283")	
	
	materiales_crudo <- tbl(con, in_schema("md", "descarga_vista")) |> 
		select(fecha_uso, 
				 codigo_ebs, 
				 cantidad,
				 tipo_ticket,
				 estado = estado_descarga_item, 
				 ticketid = ticket,
				 material) |> 
		filter(codigo_ebs %in% mufas_vkoms) |> 
		collect()

	mk <- c(OSP0000193 = "mufa48",
			  OSP0001440 = "mufa48",
			  ACC005283  = "mufa48",
			  OSP0001142 = "vkom")
	
	serie_tidy <- materiales_crudo |> 
		mutate(across(codigo_ebs, recode, !!!mk)) |>
		filter(estado == "aplicado", str_detect(tipo_ticket, "correctivo")) |> 
		drop_na()
	
	
	serie_agrupada <- serie_tidy |> 
		select(fecha = fecha_uso, material = codigo_ebs, cantidad) |> 
		group_by(fecha = floor_date(fecha, unit = "month"), material) |> 
		summarise(cantidad = sum(cantidad), .groups = "drop") |> 
		arrange(fecha)
	
	
	serie_ts <- serie_agrupada |>
		as_tsibble(key = material,
					  index = fecha,
					  regular = F, 
					  .drop = TRUE)
	
	
	# TODO: validar que todos las fechas estén correctas.
	# TODO: Los materiales que tengan valor 1 debo colocarle NA para realizar
	# imputación.
	# TODO: Enviarle a Wicho el archivo para mejorar el forecast.
	# TODO: Agregar la fibra de 48 y las bandejas.
	
	
	vec_anios <- seq(as.Date("2017-01-01"), as.Date("2022-02-01"), by = "1 month")
	
	validar <- tibble(fecha = vec_anios)
	
	# serie_wide <- 
		
		serie_ts |> 
		pivot_wider(names_from = material, values_from = cantidad)
		mutate(across(where(is.numeric), replace_na, 0))
		
	
	
	# ?autoplot.tbl_ts
	
	serie_ts |> 
		autoplot(vars(mufa48, vkom))
	
	
	mufas <- serie_ts |> 
		filter(material == "mufa48") |> 
		pivot_wider(names_from = material, values_from = cantidad) |> 
		mutate(anio = yearmonth(fecha)) |> 
		filter_index("2019 Jan" ~ .)
		
	
	
	
	
	
	
	
	
	
	
	
	
	