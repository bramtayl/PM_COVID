library(purrr)

# Download as much as we can from Wu's folder on github.
download_from_github = function(file_name,
    github_data_folder = "https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/"
) {
    download.file(
        paste0(github_data_folder, file_name),
        file.path("data", file_name)
    )
}

download_from_github("census_county_interpolated.csv")
download_from_github("county_base_mortality.txt")
download_from_github("county_old_mortality.txt")
download_from_github("county_014_mortality.txt")
download_from_github("county_1544_mortality.txt")
download_from_github("county_4564_mortality.txt")
download_from_github("county_pm25.csv")
download_from_github("state_policy0410.csv")
download_from_github("temp_seasonal_county.csv")

# This is different from the brfss_county_interpolated on github, which has
# BMI's, not obesity rates.
download.file(
    "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020.csv",
    file.path("data", "analytic_data2020.csv")
)

download.file(
    "https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D",
    file.path("data", "hospitals.csv")
)

# Download data from a specific commit to replicate as closely as possible.
download_from_JHU = function(date,
    commit = "968f9e627140a318cc5876b79fb959298da9a348"
) {
    github_folder = paste0(
        "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/",
        commit,
        "/csse_covid_19_data/csse_covid_19_daily_reports/"
    )
    file_name = paste0(format(date, "%m-%d-%Y"), ".csv")
    download.file(
        paste0(github_folder, file_name),
        file.path("data", commit, file_name))
}

download_from_JHU(as.Date("2020-03-30"))
walk(
    seq(as.Date("2020-03-22"), as.Date("2020-09-07"), by = "days"),
    download_from_JHU
)