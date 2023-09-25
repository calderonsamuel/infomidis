descargar_info <- function(ambito = "N", periodo_anual = "2023", mes = "08", departamento = "01", provincia = "01", distrito = "01") {
	im_request <- httr2::request("http://sdv.midis.gob.pe/Infomidis/api/programasSociales") |> 
		httr2::req_url_query(ambito = ambito) |> 
		httr2::req_url_query(periodo = glue::glue("{periodo_anual}{mes}")) |> 
		httr2::req_url_query(depto = departamento) |> 
		httr2::req_url_query(prov = provincia) |> 
		httr2::req_url_query(dist = distrito) 
	
	im_request |> 
		httr2::req_perform() |> 
		httr2::resp_body_json() |> 
		purrr::map(tibble::as_tibble) |> 
		purrr::list_rbind()
}
