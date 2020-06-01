readKML <- function(file,keep_name_description=FALSE,layer,...) {
  # Set keep_name_description = TRUE to keep "Name" and "Description" columns
  #   in the resulting SpatialPolygonsDataFrame. Only works when there is
  #   ExtendedData in the kml file.
  
  sp_obj<-readOGR(file,layer,...)
  xml1<-read_xml(file)
  if (!missing(layer)) {
    different_layers <- xml_find_all(xml1, ".//d1:Folder") 
    layer_names <- different_layers %>% 
      xml_find_first(".//d1:name") %>% 
      xml_contents() %>% 
      xml_text()
    
    selected_layer <- layer_names==layer
    if (!any(selected_layer)) stop("Layer does not exist.")
    xml2 <- different_layers[selected_layer]
  } else {
    xml2 <- xml1
  }
  
  # extract name and type of variables
  
  variable_names1 <- 
    xml_find_first(xml2, ".//d1:ExtendedData") %>% 
    xml_children() 
  
  while(variable_names1 %>% 
        xml_attr("name") %>% 
        is.na() %>% 
        any()&variable_names1 %>%
        xml_children() %>% 
        length>0) variable_names1 <- variable_names1 %>%
    xml_children()
  
  variable_names <- variable_names1 %>%
    xml_attr("name") %>% 
    unique()
  
  # return sp_obj if no ExtendedData is present
  if (is.null(variable_names)) return(sp_obj)
  
  data1 <- xml_find_all(xml2, ".//d1:ExtendedData") %>% 
    xml_children()
  
  while(data1 %>%
        xml_children() %>% 
        length>0) data1 <- data1 %>%
    xml_children()
  
  data <- data1 %>% 
    xml_text() %>% 
    matrix(.,ncol=length(variable_names),byrow = TRUE) %>% 
    as.data.frame()
  
  colnames(data) <- variable_names
  
  if (keep_name_description) {
    sp_obj@data <- data
  } else {
    try(sp_obj@data <- cbind(sp_obj@data,data),silent=TRUE)
  }
  sp_obj
}