
	
	import::from(dbplyr, in_schema)
	import::from(cowplot, .except = "stamp")
	library(fabletools)
	library(feasts)
	pacman::p_load(scales, conectigo, lubridate, timetk, tsibble, fable, tidyverse)
	
	options(warn = -1,
			  dplyr.summarise.inform = FALSE,
			  tibble.print_min = 20,
			  readr.show_col_types = FALSE)
	
	cargar_fuentes()
	yunkel <- theme_cowplot(font_family = "yano") +
		theme(plot.margin = unit(c(3, 1, 1, 1), "mm"), 
				axis.title = element_text(size = 12))
	
	drako <- theme_bw(base_family = "yano", base_size = 14) +
		theme(plot.margin = unit(c(6, 1, 1, 1), "mm"),
				axis.title = element_text(size = 12),
				plot.subtitle = element_text(size = 8,
													  family = "sans"))
	
	
	con <- conectar_msql()
	
	materiales_criticos <- c("OSP0001142",   # VKOM
									 "OSP0001440",   # MUFA PARA FIBRA 48 COLOR AZUL
									 "OSP0000111",   # FIBER OPTIC CABLE ADSS FOR 150 METER
									 "OSP0001313",   # FIBER OPTIC CABLE ADSS FOR 150 METER GAPS 48 THREADS
									 "OSP0000193",   # MUFA PARA FIBRA 48 HILOS VERTICAL     
									 "OSP0000185",   # BANDEJAS PARA MUFAS PLP DE 24 O 48 PARA 12 FIBRAS
									 "ACC005283")	  # MUFA ONE
	
	
	
	tipos_mantenimiento <- c(correctivo = "correctivo",
									 preventivo = "preventivo")

	
		
	materiales_crudo <- tbl(con, in_schema("md", "descarga_vista")) |> 
		select(fecha = fecha_uso, 
				 codigo = codigo_ebs, 
				 cantidad,
				 tipo = tipo_ticket,
				 estado = estado_descarga_item, 
				 ticketid = ticket,
				 material = nombre_informal) |> 
		filter(codigo %in% materiales_criticos) |> 
		collect()


	
	mk <- c("Mufa 48 Hilos"           = "mufa48",
			  "Fibra 48 Hilos vano 150" = "fibra48",
			  "Bandejas Mufa de 48"     = "bandejas",
			  "Preformado Fibra 48"     = "vkom")
	
	
	
	serie_tidy <- materiales_crudo |> 
		mutate(
			
			across(material, recode, !!!mk), 
			
			mtto = case_when(
				 str_detect(tipo, "correctivo") ~ "correctivo",
				 TRUE ~ "preventivo"),
			
			across(fecha, as.Date)
			
		) |> 
		select(fecha, ticketid, codigo, material, estado, mtto, cantidad) |> 
		filter(estado != "rechazado")
		
	
	
	convertir_tsibble <- function(.data, tipo_mtto) {
		
		.data |> 
			filter(str_detect(mtto, tipo_mtto), estado == "aplicado") |> 
			group_by(date = floor_date(fecha, unit = "month"), material) |> 
			summarise(cantidad = sum(cantidad), .groups = "drop") |> 
			drop_na() |> 
			as_tsibble(key = material, index = date, regular = FALSE)
	}
	
	
	
	lista_materiales <- tipos_mantenimiento |> 
		map(~ serie_tidy |>
			 	convertir_tsibble(tipo_mtto = .x) |> 
			 	filter_index("2017 Jan" ~ "2021 Dec"))
	
	

	correctivo_wide <- lista_materiales |> pluck("correctivo") |> 
		pivot_wider(names_from = material, values_from = cantidad)
	
	
	correctivo_wide |> 
		autoplot(vars(bandejas, fibra48, mufa48, vkom)) +
		geom_smooth() + drako
	

	# sin usar grafico de timetk
	lista_materiales |> pluck("correctivo") |>
		ggplot(aes(x = date, y = cantidad)) +
		geom_line() +
		# geom_smooth(method = "gam", formula = y ~ s(x, k = 6)) +
		geom_smooth(method = "loess", formula = 'y ~ x') +
		# facet_wrap(material ~ ., scales = "free_y") +
		facet_grid(material ~ ., scales = "free_y") +
		scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
		drako
	
	# TODO: revisar gam vs loess
	# Análisis: Hay una tendencia generalmente al alza para todos, con excepción
	# de la fibra de 48, para lo cual la tendencia parece haberse revertido a
	# inicios de 2021. Esto tiene sentido si consideramos el efecto COVID-19
	
	# serie de tiempo
	lista_materiales |> pluck("correctivo") |> 
		plot_time_series(.date_var = date,
							  .value = cantidad,
							  .facet_ncol    = 2,
							  .smooth_degree = 2,
							  .facet_vars   = material,
							  .color_var    = material,
							  .facet_scales = "free",
							  .interactive  = FALSE) + 
		scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) + drako
		
		
	
	# anomalías
	lista_materiales |> pluck("correctivo") |> 
		plot_anomaly_diagnostics(date, cantidad, 
										 .message = FALSE, 
										 .facet_vars = material,
										 .facet_ncol = 2,
										 .alpha = 0.2,  # valores altos más agresivo
										 .ribbon_alpha = 0.25,
										 .interactive = TRUE)
		scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))
	
	
	
	lista_materiales |> pluck("correctivo") |> 
		plot_time_series_boxplot(.date_var   = date,
										 .value      = cantidad,
										 .facet_ncol = 2,
										 .period     = "1 year",
										 .facet_vars = material)
		
	
	
	lista_materiales |> pluck("correctivo") |> 
		plot_time_series_boxplot(.date_var   = date,
										 .value      = cantidad,
										 .facet_ncol = 2,
										 .period     = "3 months",
										 .facet_vars = material)
	
	# diagnóstico estacional
	lista_materiales |> pluck("correctivo") |>
		plot_seasonal_diagnostics(date, cantidad, 
										  .feature_set = "month.lbl", 
										  # .geom = "violin",
										  .facet_vars = material, .interactive = F)
	
	# TODO: crear función para iterar sobre el resto de materiales
	lista_materiales |> pluck("correctivo") |> 
		filter(material == "fibra48") |> 
		gg_season(cantidad) + geom_point() + drako
	

	lista_materiales |> pluck("correctivo") |>
		plot_stl_diagnostics(.date_var = date,
									.value = cantidad,
									.frequency = "auto",
									.trend = "auto",
									.facet_vars = material,
									.feature_set = c("observed", "season", "trend", "remainder"),
									.interactive = FALSE)
	
	
	# sumarizar por cuartos
	por_cuarto <- serie_tidy |> 
		# filter(mtto == "correctivo") |> 
		mutate(cuarto = yearquarter(fecha)) |> 
		group_by(cuarto, material, mtto) |> 
		summarise(cantidad = sum(cantidad), .groups = "drop") |> 
		drop_na() |> 
		as_tsibble(key = c(material,mtto), index = cuarto) |> 
		filter_index("2017 Q1" ~ "2021 Q4")
		
	
	por_mes <- serie_tidy |> 
		mutate(mes = yearmonth(fecha)) |> 
		group_by(mes, material, mtto) |> 
		summarise(cantidad = sum(cantidad), .groups = "drop") |> 
		drop_na() |> 
		as_tsibble(key = c(material,mtto), index = mes) |> 
		filter_index("2017 Jan" ~ "2021 Dec")

	# ok	
	por_cuarto |> gg_season(cantidad)
	
	# hay que usar fill_gaps() por que hay meses en los que no hay consumo.
	por_mes |>
		fill_gaps(cantidad = 0) |>
		gg_season(cantidad,
					 labels = "both",
					 size = 1,
					 pal = rcartocolor::carto_pal(5, "Bold")) +
		drako
	
	por_mes |>
		fill_gaps(cantidad = 0) |>
		gg_season(cantidad,
					 labels = "both",
					 size = 1,
					 pal = rcartocolor::carto_pal(5, "Safe")) +
		drako
		
		
	# TODO: no tiene la opción de facet grid
	por_cuarto |> 
		# filter(material == "fibra48") |> 
		gg_subseries(cantidad) +
		drako
		# facet_grid(material ~ ., scales = "free_y") + drako
		
	por_cuarto |> 
		filter(material == "fibra48") |> 
		gg_lag(cantidad)
		
		
	por_cuarto |> 
		filter(material == "fibra48") |> 
		ACF(cantidad) |> autoplot()
		
	
	# ok	
	por_cuarto %>%
		features(cantidad, feat_stl) |> 
		ggplot(aes(x = trend_strength, y = seasonal_strength_year, color = mtto)) +
		geom_point() +
		facet_wrap(vars(material)) + drako

	
	
	
	
	
	
	
	
	
	
	