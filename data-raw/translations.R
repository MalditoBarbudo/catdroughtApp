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

  # 3d map
  'using_3d', "Per modificar l'angle de visió mantenir apretat Ctrl mentre s'arrossega amb el ratolí", 'To modify the viewing angle, hold down Ctrl while dragging with the mouse', 'Para modificar el ángulo de visión mantener apretado Ctrl mientras se arrastra con el ratón',

  # map translations
  'Relief', 'Relleu', 'Relief', 'Relieve',
  'Imagery', 'Satèl·lit', 'Imagery', 'Satélite',

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
  'sweet_alert_IFN_plots_title', "Hi ha una nova app per explorar parcel·les disponible:", "A new sites explorer app is available:", "Hay una nueva app para explorar parcelas disponible:",
  'sweet_alert_IFN_plots_text', "Si esteu interessats en el balanç d'aigua a nivell de parcel·la, visiteu", "If you are intereseted in water balance at the plot level, please visit", "Si estás interesado en el balance hídrico a nivel de parcela, visita",
  "under_construction_title", "CatDrought App està en manteniment.", "CatDrought App is under maintenance.", "CatDrought App está bajo mantenimiento.",
  "under_construction_text", "Hem trobat un problema a la base de dades i estem treballant per resoldre'l. Les dades a partir del 10 de maig de 2023 no estan disponibles en aquests moments.", "We've encountered a problem within the database and we are working is solving it. Data starting from 10th May 2023 is not available at the moment.", "Hemos encontrado un problema dentro de la base de datos y estamos trabajando para solucionarlo. Los datos a partir del 10 de mayo de 2023 no están disponibles en este momento.",

  # progress
  "progress_raster", "Obtenció del ràster", "Retrieving the raster", "Obteniendo el ráster",
  "progress_detail_raster", "Això pot trigar una mica", "This may take some time", "Esto puede llevar algo de tiempo",
  "progress_ts", "Càlcul de les sèries temporals", "Calculating the time series", "Calculando las series temporales",
  "progress_detail_ts", "Això pot trigar una mica, en funció del nombre i / o la mida dels objectes espacials", "This may take some time, depending on the number and/or size of the spatial objects", "Esto puede llevar algún tiempo, dependiendo de número y/o tamaño de los objetos espaciales",
  # poly_id_var_check
  "poly_id_missing_title", "No s'ha trobat cap variable anomenada 'poly_id' al fitxer", "Not 'poly_id' variable found in file", "No se ha encontrado ninguna variable llamada 'poly_id' en el archivo",
  "poly_id_missing_message", "S'ha fet servir la primera variable del fitxer com a poly_id", "First variable found in file used as poly_id", "Se ha usado la primera variable del archivo como poly_id"


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
)  -> app_translations

# usethis::use_data(app_translations, internal = TRUE, overwrite = TRUE)
