# script to create the list for building palettes
# 'DDS', 'DeepDrainage', 'Eplant', 'Esoil', 'Infiltration',
# 'LAI', 'PET', 'Psi', 'REW', 'Runoff', 'Theta'
palettes_dictionary <- list(
  Theta = list(min = 0, max = 0.5, pal = viridis::mako(100), rev = FALSE),
  Psi = list(min = -4, max = 0, pal = viridis::mako(100), rev = TRUE),
  REW = list(min = 0, max = 1, pal = viridis::mako(100), rev = FALSE),

  PET = list(min = 0, max = 15, pal = viridis::cividis(100), rev = TRUE),
  Precipitation = list(min = 0, max = 100, pal = viridis::cividis(100), rev = TRUE),

  LAI = list(min = 0, max = 20, pal = viridis::viridis(100), rev = FALSE),

  Interception = list(min = 0, max = 100, pal = viridis::cividis(100), rev = TRUE),
  Infiltration = list(min = 0, max = 100, pal = viridis::cividis(100), rev = TRUE),
  Runoff = list(min = 0, max = 15, pal = viridis::cividis(100), rev = TRUE),
  DeepDrainage = list(min = 0, max = 15, pal = viridis::cividis(100), rev = TRUE),
  Esoil = list(min = 0, max = 5, pal = viridis::cividis(100), rev = TRUE),
  Eplant = list(min = 0, max = 5, pal = viridis::cividis(100), rev = TRUE),

  DDS = list(min = 0, max = 1, pal = viridis::rocket(100), rev = FALSE),
  LMFC = list(min = 0, max = 365, pal = viridis::rocket(100), rev = TRUE)
  # NetPrec = list(min = 0, max = 100, pal = viridis::cividis(100), rev = TRUE),
  # NDD = list(min = 0, max = 1, pal = viridis::inferno(100), rev = TRUE)
)
