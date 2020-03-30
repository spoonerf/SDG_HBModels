####function to link countries names (and synonyms) to ISO code

library(rworldmap)
data("countrySynonyms")

df <- countrySynonyms

df$ISO3 <- toupper(df$ISO3)

df_syms <- pivot_longer(df, starts_with("name")) %>% 
  select(ISO3, value) %>% 
  filter(ISO3 != "" & value != "" & !is.na(value)) %>% 
  group_by(ISO3) %>% 
  mutate(lower_case = tolower(value)) %>% 
  pivot_longer(., c("value", "lower_case")) %>% 
  select(ISO3, value) %>% 
  ungroup()

extras <- data.frame(ISO3 = as.character(c("CPV", "CPV", "CIV", "CIV", "CZE", 
                                           "CZE", "SWZ", "SWZ", "MKD", "MKD","PSE", "PSE",
                                           "ALA", "ALA", "ATF", "ATF", "ATG", "ATG", "BIH",
                                           "BIH", "BLM", "BLM", "COD", "COD", "COK", "COK",
                                           "CUW", "CUW", "CYM", "CYM", "CZE", "CZE", "DOM", 
                                           "DOM", "FLK", "FLK", "FRO", "FRO", "FSM", "FSM",
                                           "GNQ", "HMD", "HMD", "IOT", "IOT", "MHL", "MHL",
                                           "MNP", "MNP", "PCN", "PCN", "PRK", "PRK", "PYF", 
                                           "PYF", "ESH", "ESH", "SSD", "SSD", "SGS", "SGS",
                                           "SLB", "SLB", "SOM", "SOM", "MAF", "MAF", "SPM", 
                                           "SPM", "STP", "STP", "SXM", "SXM", "TCA", "TCA",
                                           "VAT", "VAT", "VCT", "VCT", "VGB", "VGB", "VIR",
                                           "VIR", "WLF", "WLF", "BES", "BES", "REU", "REU",
                                           "CXR", "CXR", "CCK", "CCK", "SGS", "SGS", "ALA",
                                           "ALA", "HMD", "HMD", "BLM", "BLM", "MAF", "MAF",
                                           "ANT", "ANT")),
                     value = as.character(c("Cabo Verde", "cabo verde", "Côte d'Ivoire", 
                                            "côte d'ivoire", "Czechia", "czechia", "Eswatini", 
                                            "eswatini", "North Macedonia", "north macedonia", 
                                            "State of Palestine", "state of palestine", "Aland", 
                                            "aland", "Fr. S. Antarctic Lands", "fr. s. antarctic lands",
                                            "Antigua and Barb.", "antigua and barb.", "Bosnia and Herz.", 
                                            "bosnia and herz.", "St-Barthélemy","st-barthélemy", "Dem. Rep. Congo",
                                            "dem. rep. congo", "Cook Is.", "cook is", "Curaçao", "curaçao",
                                            "Cayman Is.", "cayman is.", "Czech Rep.", "czech rep.", "Dominican Rep.",
                                            "dominican rep.", "Falkland Is.", "falkland is.", "Faeroe Is.", "faeroe is.",
                                            "Micronesia", "micronesia", "Eq. Guinea", "eq. guinea", "Heard I. and McDonald Is.",
                                            "heard i. and mcdonald is.", "Br. Indian Ocean Ter.", "br. indian ocean ter.", 
                                            "Marshall Is.", "marshall is.", "N. Mariana Is.", "n. mariana is.", 
                                            "Pitcairn Is.", "pitcairn is.", "Dem. Rep. Korea", "dem. rep. korea",
                                            "Fr. Polynesia", "fr. polynesia", "W. Sahara", "w. sahara", "S. Sudan",
                                            "s. sudan", "S. Geo. and S. Sandw. Is.", "s. geo. and s. sandw. is.", 
                                            "Solomon Is.", "solomon is.", "Somaliland", "somaliland", "St-Martin", "st-martin",
                                            "St. Pierre and Miquelon", "st pierre and miquelon", "São Tomé and Principe",
                                            "são tomé and principe", "Sint Maarten", "sint maarten", "Turks and Caicos Is.",
                                            "Vatican", "vatican", "St. Vin. and Gren.", "st. vin. and gren.", "British Virgin Is.",
                                            "british virgin is.", "U.S. Virgin Is.", "u.s. virgin is.", "Wallis and Futuna Is.", 
                                            "wallis and futuna is.", "Bonaire, Sint Eustatius and Saba", 
                                            "bonaire, sint eustatius and saba", "RÃ©union", "rÃ©union", "Christmas Island", 
                                            "christmas island", "Cocos (Keeling) Islands", "cocos (keeling) islands",
                                            "South Georgia and the South Sandwich Islands", "south georgia and the south sandwich islands", 
                                            "Ã…land Islands", "ã…land islands", "Heard Island and McDonald Islands", 
                                            "heard island and mcdonald islands", "Saint BarthÃ©lemy", "saint barthÃ©lemy",
                                            "Saint Martin (French Part)", "saint martin (french part)", "Netherlands Antilles", 
                                            "netherlands antilles")))

df_syms <- rbind(df_syms, extras)


get_iso <- function(country){
  iso_out <- df_syms$ISO3[match(country, df_syms$value)]  
  return(iso_out)
}
