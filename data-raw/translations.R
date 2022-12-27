## Script for creating the translations

tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,

  # var_daily sel
  "var_daily_label", 'Tria la variable', 'Choose variable', 'Elige la variable',
  "Precipitation", "Precipitació (mm/dia)", "Precipitation (mm/day)", "Precipitación (mm/día)",
  "PET", "Evapo-transpiració potencial (mm/dia)", "Potential evapo-transpiration (mm/day)", "Evap-transpiración potencial (mm/día)",
  # "NetPrec", "Precipitació neta (mm)", "Net precipitation (mm)", "Precipitación neta (mm)",
  "Eplant", "Transpiració (mm/dia)", "Plant transpiration (mm/day)", "Transpiración plantas (mm/día)",
  "Esoil", "Evaporació del sòl (mm/dia)", "Soil evaporation (mm/day)", "Evaporación del suelo (mm/día)",
  "Runoff", "Escorrentia (mm/dia)", "Run-off (mm/day)", "Escorrentía (mm/día)",
  "DeepDrainage", "Drenatje a profunditat (mm/dia)", "Deep drainage (mm/day)", "Drenaje a profundidad (mm/día)",
  "REW", "Aigua extraïble relativa (%)", "Relative extractable water (%)", "Agua extraible relativa (%)",
  "Theta", "Contingut d'humitat (m3/m3)", "Soil moisture content (m3/m3)", "Contenido de humedad del suelo (m3/m3)",
  "Psi", "Potencial hídric del sòl (-MPa)", "Soil water potential (-MPa)", "Potencial hídrico del suelo (-MPa)",
  "DDS", "Intensitat de l'estrès (%)", "Stress intensity (%)", "Intensidad del estrés (%)",
  # "NDD", "Duració de l'estrès (dies)", "Stress duration (days)", "Duración del estrés (días)",
  "LMFC", "Contingut d'humitat de el combustible viu (%)", "Live Fuel Moisture Content (%)", "Contenido de humedad del combustible vivo (%)",
  "LAI", "Índex d'àrea foliar (m2/m2)", "Leaf area index (m2/m2)", "Índice de área foliar (m2/m2)",
  "Infiltration", "Infiltració (mm/dia)", "Infiltration (mm/day)", "Infiltración (mm/día)",
  "Interception", "Intercepció (mm/dia)", "Interception (mm/day)", "Intercepción (mm/día)",
  'Soil moisture', "Humitat del sòl", 'Soil moisture', "Humedad del suelo",
  'Climate', "Clima", 'Climate', "Clima",
  'Evaporative surface', "Superficie evaporativa", 'Evaporative surface', "Superficie evaporativa",
  'Water balance', "Balanç hídric", 'Water balance', "Balance hídrico",
  'Drought stress', "Estrès hídric", 'Drought stress', "Estrés hídrico",

  # date_daily sel
  "date_daily_label", 'Data', 'Date', 'Fecha',

  # display_daily sel
  "display_daily_label", "Mostra divisions", "Show divisions", "Mostrar divisiones",
  'none', "Cap", "None", "Ninguno",
  "Watersheds", "Conques", "Watersheds", "Cuencas hidrológicas",
  "Counties", "Comarques", "Counties", "Comarcas",
  "Municipalities", "Municipis", "Municipalities", "Municipios",
  "IFN plots", "Parcel·les IFN", "NFI plots", "Parcelas IFN",
  "file", "Arxiu espacial", "Spatial file", "Archivo espacial",

  # resolution_daily sel
  "resolution_daily_label", "Resolució del ràster", "Raster res", "Resolución del ráster",
  'Smoothed', "Suavitzat (1km)", "Smoothed (1km)", "Suavizado (1km)",
  '1km', "1km", "1km", "1km",
  '200m', "200m", "200m", "200m",

  # map translations
  'Relief', 'Relleu', 'Relief', 'Relieve',
  'Imagery', 'Satèl·lit', 'Imagery', 'Satélite',
  "OSM","OSM","OSM","OSM",

  # download raster button
  "download_raster_label", "Descàrrega el ràster", "Download raster", "Descarga el ráster",
  "download_raster_format", "Format", "Format", "Formato",
  "gtiff", "GeoTiff", "GeoTiff", "GeoTiff",
  "gpkg", "GeoPackage", "GeoPackage", "GeoPackage",

  # download trend button
  "download_series_label", "Descàrrega la sèrie", "Download series", "Descarga la serie",

  # tabs translations
  "main_tab_translation", "Explora", "Explore", "Explora",
  "data_translation", "Dades", "Data", "Datos",
  "map_translation", "Mapa", "Map", "Mapa",
  'series_tab_translation', "Sèries temporals", "Time series", "Series temporales",
  "save_translation", "Guardar", "Save", "Guardar",
  "tech_specs_translation", "Especificacions tècniques", "Technical specifications", "Especificaciones técnicas",

  # modal dialog waiting
  'modal_waiting_p', "Extraient tots els valors per a l'àrea seleccionada. Això pot trigar algun temps depenent del tipus i mida de l'àrea seleccionada (~ 5-60s)", "Extracting all values for the selected area. This can take a while depending on the type and size of the selected area (5 ~ 60 secs)", "Extrayendo todos los valores para el área seleccionada. Esto puede tardar algún tiempo dependiendo del tipo y tamaño del área seleccionada (~ 5-60s)",
  'dismiss_btn', "Tancar", "Dismiss", "Cerrar",

  # daily trends title
  'daily_trends_ifn_title', "{clicked_marker$id} en [{round(clicked_marker$lng, 3)} lng, {round(clicked_marker$lat, 3)} lat]", "{clicked_marker$id} at [{round(clicked_marker$lng, 3)} lng, {round(clicked_marker$lat, 3)} lat]", "{clicked_marker$id} en [{round(clicked_marker$lng, 3)} lng, {round(clicked_marker$lat, 3)} lat]",
  'daily_trends_other_title', "pixel en [{round(clicked_pixel$lng, 3)} lng, {round(clicked_pixel$lat, 3)} lat]", "pixel at [{round(clicked_pixel$lng, 3)} lng, {round(clicked_pixel$lat, 3)} lat]", "pixel en [{round(clicked_pixel$lng, 3)} long, {round(clicked_pixel$lat, 3)} lat]",

  # main tab titles
  'actual_tab_title', "Actual", "Current", "Actual",

  # use file selection
  "user_file_sel_label", "Selecciona l'arxiu a carregar", "Select the file to upload", "Selecciona el archivo a cargar",
  "user_file_sel_buttonLabel", "Inspecciona...", "Browse...", "Inspecciona...",
  "user_file_sel_placeholder", "Cap fitxer seleccionat", "No file selected", "Ningún archivo seleccionado",
  "file_text", 'El fitxer pot ser un shapefile (comprimit en un fitxer zip) o un fitxer GeoPackage (.gpkg). Han de tenir un camp anomenat "poly_id" amb els identificadors dels geometries continguts.', 'File can be a shapefile (compressed in a zip file) or GeoPackage file (.gpkg). They must have a field called "poly_id" with the identifiers of the contained geometries.', 'El archivo puede ser un shapefile (comprimido en un archivo zip) o un archivo GeoPackage (.gpkg). Deben tener un campo llamado "poly_id" con los identificadores de las geometrías contenidas.',

  # sweet alerts
  'sweet_alert_fileext_title', "Format de fitxer no acceptat", "File format not accepted", "Formato de archivo no aceptado",
  'sweet_alert_fileext_text', "L'arxiu carregat ha de ser un zip o gpkg", "Uploaded file must be a zip or a gpkg file", "El archivo cargado debe ser un zip o un gpkg",

  # progress
  "progress_raster", "Obtenció del ràster", "Retrieving the raster", "Obteniendo el ráster",
  "progress_detail_raster", "Això pot trigar una mica", "This may take some time", "Esto puede llevar algo de tiempo",
  "progress_ts", "Càlcul de les sèries temporals", "Calculating the time series", "Calculando las series temporales",
  "progress_detail_ts", "Això pot trigar una mica, en funció del nombre i / o la mida dels objectes espacials", "This may take some time, depending on the number and/or size of the spatial objects", "Esto puede llevar algún tiempo, dependiendo de número y/o tamaño de los objetos espaciales",
  # poly_id_var_check
  "poly_id_missing_title", "No s'ha trobat cap variable anomenada 'poly_id' al fitxer", "Not 'poly_id' variable found in file", "No se ha encontrado ninguna variable llamada 'poly_id' en el archivo",
  "poly_id_missing_message", "S'ha fet servir la primera variable del fitxer com a poly_id", "First variable found in file used as poly_id", "Se ha usado la primera variable del archivo como poly_id",

  # ....... TIPUS LLEGENDA .......
  # ..............................

  "type_legend_label", "Configurar paleta","Config palette", "Configurar paleta",
  "estandard_label", "Normal", "Normal", "Normal",
  "1st_label", "Discriminar valors alts","Discriminate higher values", "Discriminar valores altos",
  "2nd_label","Discriminar valors baixos","Discriminate lower values", "Discriminar valores bajos",
  'reverse_legend', "Invertir la paleta?","Reverse the palette?", "¿Invertir la paleta?",

  # .... Time Serie Divisions ....
  # ..............................

  'expl_divisions',"Mitjana de la variable (línea verda) obtinguda a la divisió seleccionada i representació de la (+/-) desviació estàndard (àrea sombrejada de color verd clar)","Mean of the variable (green line) obtained in the selected division and representation of the (+/-) standard deviation (light green shaded area).","Media de la variable (línea verde) obtenida en la división seleccionada y representación de la (+/-) desviación estándar (área sombreada de color verde claro)",
  'expl_no_divisions',"Mitjana de la variable (línea verda) obtinguda del píxel seleccionat","Mean of the variable (green line) obtained from the selected pixel","Media de la variable (línea verde) obtenida del píxel seleccionado",

  # ........ SAVE Options ........
  # ..............................

  "raster_download",'Ràster de les 14 variables per dia seleccionat (CRS = WGS84) </br>Resolució Ràster (nrow = 282, ncol = 288, nlayers = 14)','Raster of the 14 variables per selected day (CRS = WGS84) </br>Raster resolution (nrow = 282, ncol = 288, nlayers = 14).','Ráster de las 14 variables por día seleccionado (CRS = WGS84) </br>Resolución Ráster (nrow = 282, ncol = 288, nlayers = 14)',
  "csv_download","CSV [No Divisió] = Serie anual de la variable pel píxel seleccionat </br> CSV [Divisió Seleccionada] = Serie anual d'estadístics de la variable per la divisió seleccionada","CSV [No Division] = Annual series of the variable by the selected pixel </br> CSV [Selected Division] = Annual series of the variable statistics by the selected division","CSV [No División] = Serie anual de la variable por el píxel seleccionado </br> CSV [División Seleccionada] = Serie anual de estadísticos de la variable por la división seleccionada",

  # .......... HELP TAB ..........
  # ..............................

  'help_translation','Ajuda','Help','Ayuda',
  'help_description','Descripció : ','Description : ','Descripción : ',
  'units_description','Unitats : ','Units : ','Unidades : ',

  # "sidebar_h4_title", "Controls", 'Controls', "Controles",
  # "lidar_val_sel_label", "Seleccioneu la variable que voleu visualitzar", "Select the variable to visualize", "Selecciona la variable a visualizar",
  # "poly_type_sel_label", "Seleccioneu els polígons a agregar", "Select the polygons to aggregate", "Selecciona los polígonos a agregar",
  # "user_file_sel_label", "Pengeu un fitxer", "Upload a file", "Cargar un archivo",
  # "user_file_sel_button_label", "Navegueu", "Browse", "Explora",
  # "user_file_sel_placeholder", "No s’ha seleccionat cap fitxer", "No file selected", "No se seleccionado ningún archivo",
  # "sidebar_h4_results", "Resultats", "Results", "Resultados",
  # "sidebar_h4_download", "Descàrrega", "Download", "Descarga",
  # "main_panel_raster_siz_1", "Ràster per a la visualització té una mida de cel·la de 100x100 metres.", "Raster for visualization has a cell size of 100x100 meters.", "El ráster para la visualización tiene un tamaño de celda de 100x100 metros.",
  # "main_panel_raster_siz_2", "Ràster per als càlculs té una mida de cel·la de 20x20 metres.", "Raster for calculations has a cell size of 20x20 meters.", "El ráster para los cálculos tiene un tamaño de celda de 20x20 metros.",
  # "data_format_label", "Format de dades", "Data format", "Formato de los datos",
  # "data_length_label", "¿Totes les variables?", "All the variables?", "¿Todas las variables?",
  # "modal_dismiss_label", "Cancel·lar", "Dismiss", "Cancelar",
  # # lidar_val_sel choices
  # 'AB', "Àrea Basal (AB)", "Basal Area (AB)", "Área Basal (AB)",
  # 'BAT',  "Biomassa Aèria Total (BAT)", "Total Aerial Biomass (BAT)", "Biomasa Aérea Total (BAT)",
  # 'BF', "Biomassa de fulles (BH)", "Leaf Biomass (BH)", "Biomasa de Hojas (BH)",
  # 'CAT', "Carboni Aéri Total (CAT)", "Total Aerial Carbon (CAT)", "Carbono Aéreo Total (CAT)",
  # 'DBH', "Diàmetre Normal (DBH)", "Diameter at Breast Height (DBH)", " Diámetro Normal (DBH)",
  # 'HM', "Altura Mitjana (HM)", "Mean Height (HM)", "Altura Media (HM)",
  # 'REC', "Recobriment (REC)", "Coating (REC)", "Recubrimiento (REC)",
  # 'VAE', "Volum amb Escorça (VOB)", "Over Bark Volume(VOB)", "Volúmen con Corteza (VOB)",
  # # poly type sel choices
  # 'Catalonia', "Catalunya", "Catalonia", "Cataluña",
  # 'Provinces', "Provincies", "Provinces", "Provincias",
  # 'Counties', "Comarques", "Counties", "Comarcas",
  # 'Municipalities', "Municipis", "Municipalities", "Municipios",
  # 'Veguerias', "Vegueries", "Veguerias", "Veguerias",
  # 'Drawed polygon', "Polígon dibuxat", "Drew polygon", "Polígono dibujado",
  # 'File upload', "Fitxer", "File upload", "Archivo",
  # # data format options
  # 'GIS', 'SIG', 'GIS', 'SIG',
  # 'TABLE', 'Table', 'Table', 'Tabla',
  # 'shp', 'Shapefile', 'Shapefile', 'Shapefile',
  # 'wkt', 'Well Known Text', 'Well Known Text', 'Well Known Text',
  # 'gpkg', 'GeoPackage', 'GeoPackage', 'GeoPackage',
  # 'csv', 'CSV', 'CSV', 'CSV',
  # 'xlsx', 'Excel', 'Excel', 'Excel',
  # # data length choices
  # 'all_columns', 'Totes les variables', 'All the variables', 'Todas las variables',
  # 'visible', 'Només la variable selecionada', 'Only the selected variable', 'Sólo la variable seleccionada',
  # 'poly', 'Polígons', 'Polygons', 'Polígonos',
  # 'lidar', 'LiDAR', 'LiDAR', 'LiDAR',
  # # table names
  # 'poly_id', 'Polígon', 'Polygon', 'Polígono',
  # 'mean_ab', "Àrea Basal Mitjana (AB)", "Mean Basal Area (AB)", "Área Basal Media (AB)",
  # 'mean_bat',  "Biomassa Aèria Total Mitjana (BAT)", "Mean Total Aerial Biomass (BAT)", "Biomasa Aérea Total Media (BAT)",
  # 'mean_bf', "Biomassa de fulles Mitjana (BH)", "Mean Leaf Biomass (BH)", "Biomasa de Hojas Media (BH)",
  # 'mean_cat', "Carboni Aéri Total Mitjan (CAT)", "Mean Total Aerial Carbon (CAT)", "Carbono Aéreo Total Medio (CAT)",
  # 'mean_dbh', "Diàmetre Normal Mitjan (DBH)", "Mean Diameter at Breast Height (DBH)", " Diámetro Normal Medio (DBH)",
  # 'mean_hm', "Altura Mitjana Mitjana (HM)", "Mean Height (HM)", "Altura Media (HM)",
  # 'mean_rec', "Recobriment Mitjan (REC)", "Mean Coating (REC)", "Recubrimiento Medio (REC)",
  # 'mean_vae', "Volum amb Escorça Mitjan (VOB)", "Mean Over Bark Volume(VOB)", "Volúmen con Corteza Medio (VOB)",

  ## TODO continue translations thesaurus
) %>%
  {.} -> app_translations

tibble::tribble(
  # ~var_id, ~var_description_cat, ~var_description_eng, ~var_description_spa, ~var_description_help_cat, ~var_description_help_eng, ~var_description_help_spa, ~var_units_cat, ~var_units_eng, ~var_units_spa,
  #
  # "REW","Aigua disponible sòl","Available soil water","Agua disponible suelo","Percentatge d’aigua disponible al sòl, en relació al total d’aigua que pot ser extreta per les plantes.","Percentage of available water in the soil, in relation to the total water that can be extracted by plants.","Porcentaje de agua disponible en el suelo, en relación al total de agua que puede ser extraída por las plantas.","(%)","(%)","(%)",
  # "DDS","Estrés de la vegetació","Vegetation Stress","Estrés de la vegetación","Índex d’estrés de la vegetació, que mesura el grau de tancament dels estomes de les plantes al bosc.","Vegetation stress index, which measures the degree of stomata closure of plants in the forest.","Índice de estrés de la vegetación, que mide el grado de cierre de los estomas de las plantas en el bosque.","(%)","(%)","(%)",
  # "PET","Evapo-transpiració potencial","Potential evapo-transpiration","Evap-transpiración potencial","Demanda evaporativa potencial de l’atmosfera, mesurada mitjançant l’index d’evapo-transpiració potencial de Penman.","Potential evaporative demand of the atmosphere, measured by Penman's potential evapotranspiration index.","Demanda evaporativa potencial de la atmósfera, medida mediante el índice de evapo-transpiración potencial de Penman.","(mm/dia)","(mm/day)","(mm/día)",
  # "Precipitation","Precipitació","Precipitation","Precipitación","Precipitació diària, en forma de pluja o neu.","Daily precipitation, in the form of rain or snow.","Precipitación diaria, en forma de lluvia o nieve.","(mm/dia)","(mm/day)","(mm/día)",
  # "LMFC","Contingut d’Humitat de Combustible Viu","Live Fuel Moisture Content","Contendio de Humedad de Combustible Vivo","Contingut d’aigua del combustible fi viu (fulles i branquillons), en relació al seu pes sec.","Water content of live fine fuel (leaves and twigs), in relation to their dry weight.","Contenido de agua del combustible fino vivo (hojas y ramitas), en relación a su peso seco.","(%)","(%)","(%)",
  #
  ~var_id, ~var_description_help_cat, ~var_description_help_eng, ~var_description_help_spa, ~var_units_cat, ~var_units_eng, ~var_units_spa,

  "REW","Percentatge d’aigua disponible al sòl, en relació al total d’aigua que pot ser extreta per les plantes.","Percentage of available water in the soil, in relation to the total water that can be extracted by plants.","Porcentaje de agua disponible en el suelo, en relación al total de agua que puede ser extraída por las plantas.","(%)","(%)","(%)",
  "DDS","Índex d’estrés de la vegetació, que mesura el grau de tancament dels estomes de les plantes al bosc.","Vegetation stress index, which measures the degree of stomata closure of plants in the forest.","Índice de estrés de la vegetación, que mide el grado de cierre de los estomas de las plantas en el bosque.","(%)","(%)","(%)",
  "PET","Demanda evaporativa potencial de l’atmosfera, mesurada mitjançant l’index d’evapo-transpiració potencial de Penman.","Potential evaporative demand of the atmosphere, measured by Penman's potential evapotranspiration index.","Demanda evaporativa potencial de la atmósfera, medida mediante el índice de evapo-transpiración potencial de Penman.","(mm/dia)","(mm/day)","(mm/día)",
  "Precipitation","Precipitació diària, en forma de pluja o neu.","Daily precipitation, in the form of rain or snow.","Precipitación diaria, en forma de lluvia o nieve.","(mm/dia)","(mm/day)","(mm/día)",
  "LMFC","Contingut d’aigua del combustible fi viu (fulles i branquillons), en relació al seu pes sec.","Water content of live fine fuel (leaves and twigs), in relation to their dry weight.","Contenido de agua del combustible fino vivo (hojas y ramitas), en relación a su peso seco.","(%)","(%)","(%)",
  "Theta","","","","(m3/m3)","(m3/m3)","(m3/m3)",
  "Psi","","","","(-MPa)","(-MPa)","(-MPa)",
  "LAI","","","","(m2/m2)","(m2/m2)","(m2/m2)",
  "Interception","","","","(mm/dia)","(mm/day)",	"(mm/día)",
  "Infiltration","","","","(mm/dia)","(mm/day)","(mm/día)",
  "Runoff","","","","(mm/dia)","(mm/day)","(mm/día)",
  "DeepDrainage","","","","(mm/dia)","(mm/day)","(mm/día)",
  "Esoil","","","","(mm/dia)","(mm/day)","(mm/día)",
  "Eplant","","","","(mm/dia)","(mm/day)","(mm/día)",



) %>%
  {.} -> catdrought_var_thes

