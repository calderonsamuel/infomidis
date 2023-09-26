#' Descargar datos de InfoMIDIS
#' 
#' @param ambito Ámbito de la consulta. Uno de c("nacional", "vraem")
#' @param periodo_anual,mes Año y mes de los datos de consulta. Año debe especificarse en formato "YYYY" y mes en formato "MM". Se tienen datos de Octubre 2012.
#' @param departamento,provincia,distrito Código de lugar de búsqueda. Cada uno debe especificarse en formato "00". Solo necesario si se requiere mayor nivel de detalle.
#' @param sleep_time Time to wait between requests. Only useful when iterating over multiple calls to this function.
#' @return Un `<tibble>` con la información consultada
#' @export
infomidis <- function(ambito = "nacional",
					  periodo_anual = ultima_data_anio(),
					  mes = ultima_data_mes(),
					  departamento = NULL,
					  provincia = NULL,
					  distrito = NULL,
					  sleep_time = 0L) {
	
	ambito <- match.arg(ambito, c("nacional", "vraem"))
	
	im_request <- httr2::request("http://sdv.midis.gob.pe/Infomidis/api/programasSociales") %>% 
		httr2::req_url_query(ambito = ambito) %>% 
		httr2::req_url_query(periodo = glue::glue("{periodo_anual}{mes}")) %>% 
		httr2::req_url_query(depto = departamento) %>%
		httr2::req_url_query(prov = provincia) %>% 
		httr2::req_url_query(dist = distrito) 
	
	Sys.sleep(time = sleep_time)
	
	im_request %>% 
		httr2::req_perform() %>% 
		httr2::resp_body_json() %>% 
		purrr::map(tibble::as_tibble) %>% 
		purrr::list_rbind()
}

ultima_data_anio <- function() (Sys.Date() - lubridate::period(1, "month")) %>% format("%Y") 

ultima_data_mes <- function() (Sys.Date() - lubridate::period(1, "month")) %>% format("%m") 
