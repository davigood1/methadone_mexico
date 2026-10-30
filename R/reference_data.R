# CIJ supplied annual patient counts. Annual volume is retained for source
# reconciliation; the figures never turn these counts into monthly patients.
cij_annual <- tibble::tribble(
  ~site, ~state, ~year, ~patients, ~annual_mg, ~months_observed,
  "Ciudad Juárez", "Chihuahua", 2018, 350, 5926030, 12,
  "Ciudad Juárez", "Chihuahua", 2019, 341, 5984540, 12,
  "Ciudad Juárez", "Chihuahua", 2020, 281, 4971290, 12,
  "Ciudad Juárez", "Chihuahua", 2021, 276, 4351840, 12,
  "Ciudad Juárez", "Chihuahua", 2022, 319, 5054650, 11,
  "Ciudad Juárez", "Chihuahua", 2023, 309, 2612100, 5,
  "Ciudad Juárez", "Chihuahua", 2024, 254, 3231010, 12,
  "Ciudad Juárez", "Chihuahua", 2025, 286, 2229980, 8,
  "Tijuana", "Baja California", 2018, 337, 3719600, 12,
  "Tijuana", "Baja California", 2019, 290, 3414140, 12,
  "Tijuana", "Baja California", 2020, 182, 2714990, 12,
  "Tijuana", "Baja California", 2021, 136, 2448330, 12,
  "Tijuana", "Baja California", 2022, 150, 2106430, 11,
  "Tijuana", "Baja California", 2023, 130, 1214080, 5,
  "Tijuana", "Baja California", 2024, 98, 1394020, 12,
  "Tijuana", "Baja California", 2025, 77, 663260, 8,
  "Zapopan", "Jalisco", 2018, 52, 760150, 12
)

# Counts, rather than names or licence IDs, from the COFEPRIS site extract.
site_counts <- tibble::tribble(
  ~locality, ~sector, ~n, ~longitude, ~latitude,
  "Ciudad Juárez", "Private", 2, -106.4850, 31.6904,
  "Ciudad Juárez", "Public (CIJ)", 1, -106.4850, 31.6904,
  "Culiacán", "Private", 1, -107.3940, 24.8091,
  "Hermosillo", "Private", 1, -110.9559, 29.0729,
  "Mexicali", "Private", 4, -115.4677, 32.6245,
  "Mexico City", "Private", 1, -99.1617, 19.3717,
  "Nuevo Laredo", "Private", 1, -99.5075, 27.4865,
  "Oaxaca de Juárez", "Private", 1, -96.7266, 17.0732,
  "Tijuana", "Private", 3, -117.0382, 32.5149,
  "Tijuana", "Public (CIJ)", 1, -117.0382, 32.5149,
  "Zapopan", "Public (CIJ)", 1, -103.3496, 20.6597
)
