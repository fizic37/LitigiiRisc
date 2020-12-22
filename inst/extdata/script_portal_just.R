library(rebus)
load("data/litigii_sep.rda")
lista_totala_dosare <- litigii_sep$`Nr dosar instanta`

# Create regex for numar dosar correct identification
regex_dosar <- number_range(lo = 1,hi = 500000,allow_leading_zeroes = F) %R% "/" %R% number_range(lo = 1,hi = 500000,allow_leading_zeroes = F)  %R% 
  "/" %R% number_range(lo = 2005, hi = 2030,allow_leading_zeroes = F) %R% zero_or_more("*")

# extrag spatiul si tot ce vine dupa el cu comanda de mai jos
regex_clean <- one_or_more(SPACE) %R% ANY_CHAR

# I extract correct numar dosar, I name the list in order to identify the ID number of excluded and accepted numar dosar
lista_dosare_excluse <- purrr::map(lista_totala_dosare, ~stringr::str_extract(string = .x,pattern = regex_dosar)) %>% 
  purrr::set_names(nm = litigii_sep$Nr_crt) %>%
  purrr::keep(function(x) is.na(x)) %>% names() %>% as.numeric() %>% dplyr::slice(.data = litigii_sep,.)

lista_dosare_acceptate <- purrr::map(lista_totala_dosare, ~stringr::str_extract(string = .x,pattern = regex_dosar)) %>% 
  purrr::keep(~!is.na(.x)) %>% unlist()


# Functia de mai jos genereaza un xml curat,prelucrabil care contine toate info unui dosar
create_body <- function(numar_dosar) {
  headerfields = c(
    Accept = "text/xml",
    Accept = "multipart/*",
    'Content-Type' = "text/xml; charset=utf-8",
    'SOAPAction' = "portalquery.just.ro/CautareDosare")
  
  body_dosar <- paste0('<?xml version="1.0" encoding="utf-8"?>
    <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
      <soap:Body>
        <CautareDosare xmlns="portalquery.just.ro">
          <numarDosar>', numar_dosar,'</numarDosar>
        </CautareDosare>
      </soap:Body>
    </soap:Envelope>')
  
  reader = RCurl::basicTextGatherer()
  reader$reset()
  RCurl::curlPerform(
    url = "http://portalquery.just.ro/Query.asmx",
    httpheader = headerfields,
    postfields = body_dosar,
    writefunction = reader$update)
  reader$value() %>% stringr::str_replace(pattern = "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"><soap:Body>",
                                 replacement = "") %>% stringr::str_replace(pattern = "</soap:Body></soap:Envelope>", replacement ="") %>%
    stringr::str_remove_all(pattern = 'xsi:nil=\"true\"') %>%
    stringr::str_replace(pattern = '<CautareDosareResponse xmlns=\"portalquery.just.ro\">',replacement = "") %>%
    stringr::str_replace(pattern = '</CautareDosareResponse>',replacement = "") %>% xml2::read_xml()
}

# Create the function that will interogate lista numar dosar with safely. Note, it does not work to combine the below 2 commands
safe_extract <- purrr::safely(create_body)
# Main command to interogate portal just
lista_xml_total_dosare_juridic <- purrr::map(lista_dosare_acceptate, ~safe_extract(.x))
# Do not save. It will not work
#save(lista_xml_total_dosare_juridic,file = "lista_dosare_25_nov",version = 3,compress = "gzip")


lista_xml_total_dosare_juridic_results <- purrr::map(lista_xml_total_dosare_juridic, "result") 

numar_ultimul_dosar_portal_just_results <- purrr::map(purrr::compact(lista_xml_total_dosare_juridic_results), 
          ~xml2::xml_find_first(.x, xpath = "//numar",) %>% xml2::xml_text()) %>% unlist()
obiect_ultimul_dosar_portal_just_results <- purrr::map(purrr::compact(lista_xml_total_dosare_juridic_results), ~xml2::xml_find_first(.x, xpath = "//obiect",) %>%
                                                  xml2::xml_text()) %>% unlist()
ultima_data_pronuntare_results <- purrr::map(purrr::compact(lista_xml_total_dosare_juridic_results), ~xml2::xml_find_first(.x, xpath = "//dataPronuntare") %>%
                                        xml2::xml_text()) %>% unlist()
ultima_data_dosar_results <- purrr::map(purrr::compact(lista_xml_total_dosare_juridic_results), ~xml2::xml_find_first(.x, xpath = "//data") 
                                 %>% xml2::xml_text()) %>% unlist()
ultima_solutie_results <- purrr::map(purrr::compact(lista_xml_total_dosare_juridic_results), ~xml2::xml_find_first(.x, xpath = "//solutie") 
                              %>% xml2::xml_text()) %>% unlist()
ultima_solutie_scurt_results <- purrr::map(purrr::compact(lista_xml_total_dosare_juridic_results), ~xml2::xml_find_first(.x, xpath = "//solutieSumar",) 
                                    %>% xml2::xml_text()) %>% unlist()


lista_parti_results <- purrr::map(purrr::compact(lista_xml_total_dosare_juridic_results), ~xml2::xml_find_all(xpath = "//DosarParte/nume",x=.x)
                           %>% xml2::xml_text())
calitate_parti_results <- purrr::map(purrr::compact(lista_xml_total_dosare_juridic_results), ~xml2::xml_find_all(xpath = "//DosarParte/calitateParte",x=.x)
                              %>% xml2::xml_text())
index_nume_fond_results <- purrr::map(lista_parti_results, ~stringr::str_detect(string = .x, pattern = "FONDUL NA"))

lista_calitate_fngcimm_results <- purrr::map2(.x = calitate_parti_results, .y = index_nume_fond_results,
                                       ~.x[.y][1][1]) %>% unlist()

dosare_identificate_results <- data.frame(numar_ultimul_dosar_portal_just_results, 
                                          obiect_ultimul_dosar_portal_just_results,
                                          ultima_data_pronuntare_results,
                                          ultima_data_dosar_results, ultima_solutie_results, 
                                          ultima_solutie_scurt_results,lista_calitate_fngcimm_results,stringsAsFactors = F)
saveRDS(object = dosare_identificate_results, file = "dosare_juridic_6_dec_2020")

denumiri_fngcimm <- purrr::map2(.x = lista_parti_results, .y = index_nume_fond_results, ~.x[.y][1][1]) %>% purrr::keep(~!is.na(.x)) %>%
  unlist() %>% unique()

interogare_parte <- function(numeparte, startdate="2020-09-30T00:00:00", stopdate="2020-12-06T00:00:00") {
  headerfields = c(
    Accept = "text/xml",
    Accept = "multipart/*",
    'Content-Type' = "text/xml; charset=utf-8",
    'SOAPAction' = "portalquery.just.ro/CautareDosare")
  
  body_interogare <- paste0('<?xml version="1.0" encoding="utf-8"?>
                       <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
                       <soap:Body>
                       <CautareDosare xmlns="portalquery.just.ro">
                       <numeParte>',numeparte,'</numeParte>
                       <dataStart>',startdate,'</dataStart>
                       <dataStop>',stopdate,'</dataStop>
                       </CautareDosare>
                       </soap:Body>
                       </soap:Envelope>')
  
  reader = RCurl::basicTextGatherer()
  reader$reset()
  RCurl::curlPerform(
    url = "http://portalquery.just.ro/Query.asmx",
    httpheader = headerfields,
    postfields = body_interogare,
    writefunction = reader$update)
  reader$value() %>% stringr::str_replace(pattern = "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"><soap:Body>",
                                 replacement = "") %>% stringr::str_replace(pattern = "</soap:Body></soap:Envelope>", replacement ="") %>%
    stringr::str_remove_all(pattern = 'xsi:nil=\"true\"') %>%
    stringr::str_replace(pattern = '<CautareDosareResponse xmlns=\"portalquery.just.ro\">',replacement = "") %>%
    stringr::str_replace(pattern = '</CautareDosareResponse>',replacement = "") %>% xml2::read_xml()
}

#ATENTIE: linia principala de interogare
lista_xml_interogare <- purrr::map(denumiri_fngcimm, ~interogare_parte(.x))

numar_dosare_interogare <- purrr::map(lista_xml_interogare, ~xml2::xml_find_all(.x, xpath = "//numar") %>% xml2::xml_text()) %>% unlist() %>% unique()

date_dosare_interogare <- purrr::map(lista_xml_interogare, ~xml2::xml_find_all(.x, xpath = "//data") %>% xml2::xml_text()) %>% unlist() %>% 
  unique() %>% as.Date(format = "%Y-%m-%dT00:00:00")

#Doar de exmeplu
#purrr::map(lista_xml_interogare, ~xml2::xml_find_all(.x, xpath = "//Dosar/parti/DosarParte/nume") %>% xml2::xml_text()) %>% unlist() %>% unique()

# Extrag inca o data dupa numerele de dosar identificate. Este horror sa incerc extragerea de date din lista lista_xml_interogare

lista_xml_rezultate_interogare <- purrr::map(numar_dosare_interogare, ~safe_extract(.x))
lista_xml_rezultate_interogare_results <- purrr::map(lista_xml_rezultate_interogare, "result") 

data_pronuntare_interogare <- purrr::map(lista_xml_rezultate_interogare_results, ~xml2::xml_find_first(.x, xpath = "//dataPronuntare") %>%
                                    xml2::xml_text()) %>% unlist() %>% as.Date(format = "%Y-%m-%dT00:00:00")

solutie_simpla_interogare <- purrr::map(lista_xml_rezultate_interogare_results, ~xml2::xml_find_first(.x, xpath = "//solutie") %>%
                                   xml2::xml_text()) %>% unlist()

solutie_sumar_interogare <- purrr::map(lista_xml_rezultate_interogare_results, ~xml2::xml_find_first(.x, xpath = "//solutieSumar") %>% 
                                  xml2::xml_text())  %>% unlist()

categorie_caz_interogare <- purrr::map(lista_xml_rezultate_interogare_results, ~xml2::xml_find_all(.x, xpath = "//obiect") %>% xml2::xml_text())

lungime_lista_categ_caz_interogare <- purrr::map(categorie_caz_interogare, ~length(.x)) %>% unlist()

ultima_categorie_caz_interogare <- purrr::map2(.x = categorie_caz_interogare,.y = lungime_lista_categ_caz_interogare, ~.x[.y]) %>% 
                  purrr::map(.f = ~.x[1]) %>% unlist()

data_dosar_interogare <- purrr::map(lista_xml_rezultate_interogare_results, ~xml2::xml_find_all(.x, xpath = "//data") %>% xml2::xml_text())

index_data_fond_interogare <- purrr::map(data_dosar_interogare, ~length(.x)) %>% unlist()

prima_data_dosar_interogare <- purrr::map2(.x = data_dosar_interogare, .y = index_data_fond_interogare, ~.x[.y]) %>% unlist() %>% as.Date()

categorie_caz_nume_interogare <- purrr::map(lista_xml_rezultate_interogare_results, ~xml2::xml_find_first(.x, xpath = "//categorieCazNume") %>% xml2::xml_text()) %>% unlist()

lista_parti_interogare <- purrr::map(lista_xml_rezultate_interogare_results, ~xml2::xml_find_all(xpath = "//DosarParte/nume",x=.x) %>% xml2::xml_text())

calitate_parti_interogare <- purrr::map(lista_xml_rezultate_interogare_results, ~xml2::xml_find_all(xpath = "//DosarParte/calitateParte",x=.x) %>% xml2::xml_text())

index_nume_fond_interogare <- purrr::map(lista_parti_interogare, ~stringr::str_detect(string = .x, pattern = "FONDUL NA"))

lista_calitate_fngcimm_interogare <- purrr::map2(.x = calitate_parti_interogare, .y = index_nume_fond_interogare,
                                          ~.x[.y][1][1]) %>% unlist()

parti_dosar_prelucrat <- purrr::map(.x = lista_parti_interogare, ~stringr::str_c(.x,collapse = " ; ")) %>% unlist()

lista_dosare_noi <- data.frame(numar_dosare_interogare, data_pronuntare_interogare, solutie_simpla_interogare,
                               solutie_sumar_interogare, ultima_categorie_caz_interogare, prima_data_dosar_interogare,
                               categorie_caz_nume_interogare, lista_calitate_fngcimm_interogare,
                               parte_dosar = parti_dosar_prelucrat,stringsAsFactors = FALSE) 

View(lista_dosare_noi)
saveRDS(object = lista_dosare_noi, file = "lista_dosare_noi")