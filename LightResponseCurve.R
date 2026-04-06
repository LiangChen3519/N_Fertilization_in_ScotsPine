library(tidyverse)
library(broom)
library(readxl)
library(lubridate)
library(plotly)
library(ggh4x) 
library(ggnuplot)
library(photosynthesis)
library(patchwork)
library(segmented)
library(purrr)

# load data -- ros2
## ## load data and define column type one by one
ros2 <- read_xlsx("data/Rosinedal_flux_meteo_data.xlsx", sheet = "ROS2", skip = 0,
                  col_types = c("date","numeric","numeric","numeric","numeric","numeric",
                                "numeric","numeric","numeric","numeric","numeric","numeric","numeric"
                                ,"numeric","numeric"))

ros2 = ros2[-1,] 

glimpse(ros2)

# run the response curve
sites <- c()
R2 <- c()
GPP_2000 <- c()
DT <- c()

# years <- unique(year(ros2$Date_time))
# ros2 <-  filter(ros2,month(Date_time) %in% 4:10)
# for (y in years) {
# subd <- filter(ros2,year(Date_time) == y)
dates <- unique(date(ros2$dt))

for (i in 1:length(dates)) {
    # d = dates[i] # go through all dates
    if ((dates[i] + days(6)) <= dates[length(dates)]) {
        sdd <- filter(ros2, dates[i] <= date(dt) & date(dt) <= (dates[i] + days(6)))
        #sdd <- filter(sdd)
        skip <- FALSE
        tryCatch(
            {
                fit <- fit_photosynthesis(
                    .data = sdd,
                    .photo_fun = "aq_response",
                    .vars = list(.A = GPP, .Q = PPFD)
                )
            },
            error = function(e) {
                message("model failed with these data!")
                skip <- TRUE
            }
        )
        if (skip == TRUE) {
            next
            # skip <- FALSE
        } else {
            coefs <- coef(fit)
            tryCatch(
                {
                    r2 <- (cor(fit$m$fitted(), sdd$GPP))^2
                },
                error = function(e) {
                    message("missing value")
                    r2 <- 0
                }
            )
            site <- "ROS2"
            GPP2000 <- (coefs[2] * 2000 + coefs[1] - 
                            sqrt((coefs[2] * 2000 + coefs[1])^2 - 
                                     4 * coefs[2] * coefs[3] * 2000 * coefs[1])) / 
                (2 * coefs[3]) + coefs[4]
            dt <- (dates[i] + days(3))
            sites <- append(sites, site)
            R2 <- append(R2, r2)
            GPP_2000 <- append(GPP_2000, GPP2000)
            DT <- append(DT, dt)
        }
    } else {
        next
    }
}

ROS2_paras <- data.frame(sites, DT, GPP_2000, R2)


# ros3 sites
## load data and define column type one by one
ros3 <- read_xlsx("data/Rosinedal_flux_meteo_data.xlsx", sheet = "ROS3", skip = 0,
                  col_types = c("guess","numeric","numeric","numeric","numeric","numeric",
                                "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                "numeric","numeric"))
ros3$year <- year(ros3$Datetime)
ros3$month <- month(ros3$Datetime)
ros3$day <- day(ros3$Datetime)
glimpse(ros3)


sites <- c()
R2 <- c()
GPP_2000 <- c()
DT <- c()

dates <- unique(date(ros3$Datetime))
for (i in 1:length(dates)) {
    # d = dates[i] # go through all dates
    if ((dates[i] + days(6)) <= dates[length(dates)]) {
        sdd <- filter(ros3, dates[i] <= date(Datetime) & date(Datetime) <= (dates[i] + days(6)))
        sdd <- filter(sdd) # daytime data for LRC
        skip <- FALSE
        tryCatch(
            {
                fit <- fit_photosynthesis(
                    .data = sdd,
                    .photo_fun = "aq_response",
                    .vars = list(.A = GPP, .Q = PPFD)
                )
            },
            error = function(e) {
                message("model failed with these data!")
                skip <- TRUE
            }
        )
        if (skip == TRUE) {
            next
            # skip <- FALSE
        } else {
            coefs <- coef(fit)
            tryCatch(
                {
                    r2 <- (cor(fit$m$fitted(), sdd$GPP))^2
                },
                error = function(e) {
                    message("missing value")
                    r2 <- 0
                }
            )
            site <- "ROS3"
            GPP2000 <- (coefs[2] * 2000 + coefs[1] - 
                            sqrt((coefs[2] * 2000 + coefs[1])^2 - 
                                     4 * coefs[2] * coefs[3] * 2000 * coefs[1])) / 
                (2 * coefs[3]) + coefs[4]
            dt <- (dates[i] + days(3))
            sites <- append(sites, site)
            R2 <- append(R2, r2)
            GPP_2000 <- append(GPP_2000, GPP2000)
            DT <- append(DT, dt)
        }
    } else {
        next
    }
}
# }
ROS3_paras <- data.frame(sites, DT, GPP_2000, R2)
