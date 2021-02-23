# import selected references
path <- file.path("data-raw/data")
libs <- "who"

for (lib in libs) {
  files <- list.files(file.path(path, libs))
  refcodes <- rep("", length(files))
  names(refcodes) <- files
  for (file in files) {
    ref <- centile::read_ref(file.path(path, lib, file))
    s <- attr(ref, "study")
    refcode <- centile::make_refcode(s["name"], s["year"], s["yname"], s["sex"], s["sub"])
    assign(refcode, ref)
    refcodes[file] <- refcode
  }
}

usethis::use_data(
  who_2011_bmi_female_,
  who_2011_bmi_male_,
  who_2011_hdc_female_,
  who_2011_hdc_male_,
  who_2011_hgt_female_,
  who_2011_hgt_male_,
  who_2011_wfh_female_,
  who_2011_wfh_male_,
  who_2011_wgt_female_,
  who_2011_wgt_male_,
  internal = TRUE, overwrite = TRUE
)
