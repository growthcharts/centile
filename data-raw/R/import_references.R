# import selected references
path <- file.path("data-raw/data")
libs <- "who"

refcodes <- character(0)
for (lib in libs) {
  files <- list.files(file.path(path, lib))
  for (file in files) {
    ref <- centile::import_rif(file.path(path, lib, file))
    s <- attr(ref, "study")
    refcode <- centile::make_refcode(s["name"], s["year"], s["yname"], s["sex"], s["sub"])
    refcodes <- c(refcodes, refcode)
    assign(refcode, ref)
  }
}

usethis::use_data(
  refcodes,
  who_2006_bmi_female_,
  who_2006_bmi_male_,
  who_2007_hdc_female_,
  who_2007_hdc_male_,
  who_2006_hgt_female_,
  who_2006_hgt_male_,
  who_2006_wfh_female_,
  who_2006_wfh_male_,
  who_2006_wgt_female_,
  who_2006_wgt_male_,
  internal = TRUE, overwrite = TRUE
)
