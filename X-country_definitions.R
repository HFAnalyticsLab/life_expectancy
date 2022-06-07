# Define high income countries used in White 2002

high_income_countries <- 
  c(
    "AUS", "AUT", "BEL", "CAN", "DNK", "FIN",
    "FRATNP", "DEUTW", "GRC", "IRL", "ITA", "JPN",
    "NLD", "NZL_NP", "NOR", "PRT", "ESP", "SWE",
    "CHE", "GBR_NP", "USA"
  )

# Define all distinct countries in HMD

all_distinct_countries <- c(
  "AUS", "AUT", "BEL", "BGR", "BLR", "CAN", "CHL", "HRV", "CHE",
  "CZE", "DEUTE", "DEUTW", "DNK", "ESP", "EST", "FIN", "FRATNP", "GRC", "HUN",
  "IRL", "ISL", "ISR", "ITA", "JPN", "KOR", "LTU", "LUX", "LVA", "NLD", "NOR",
  "NZL_NP",
  "POL", "PRT", "RUS", "SVK", "SVN", "SWE", "TWN", "UKR", "GBR_NP", "USA"
)

country_labels <- tribble(
  ~code, 	~label,
  "AUS", 	"Australia",
  "AUT",  "Austria",
  "BLR", "Belarus",
  "BEL", "Belgium",
  "BGR", "Bulgaria",
  "CAN", "Canada",
  "CHL", "Chile", 
  "HRV", "Croatia",
  "CZE", "Czechia",
  "DNK", "Denmark",
  "EST", "Estonia",
  "FIN", "Finland",
  "FRATNP", "France",
  "DEUTNP", "Germany",
  "GRC", "Greece",
  "HUN", "Hungary",
  "ISL", "Iceland",
  "IRL", "Ireland",
  "ISR", "Israel",
  "ITA", "Italy",
  "JPN", "Japan",
  "KOR", "South Korea",
  "LVA", "Latvia",
  "LTU", "Lithuania",
  "LUX", "Luxembourg",
  "NLD", "Netherlands",
  "NZL_NP", "New Zealand",
  "NOR", "Norway",
  "POL", "Poland",
  "PRT", "Portugal",
  "RUS", "Russia",
  "SVK", "Slovakia",
  "SVN", "Slovenia",
  "ESP", "Spain",
  "SWE", "Sweden",
  "CHE", "Switzerland",
  "TWN", "Taiwan",
  "GBR_NP", "United Kingdom",
  "USA", "USA",
  "UKR", "Ukraine"
)
