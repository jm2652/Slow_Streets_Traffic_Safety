library(sf)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(tmap)

# Data preparation -----------------------------------------------

# Load & process Slow Streets network
setwd("Oakland Slow Streets Program/")
slow <- read_sf(
    "geo_export_8ea9ca7d-ebe9-4e88-904c-fd55b59907d6.shp")
setwd("../")
slow <- slow |> st_transform(crs = 4269)
plot(slow["shape_leng"])


# Load & process Crashes
# TODO: consider whether helpful to combine Date & Time into datetime

crashes <- read_csv("Crashes2023.csv",
                    col_types = 
                        cols(COLLISION_DATE = col_date("%Y-%m-%e"),
                             COLLISION_TIME = col_time("%H%M")
                        ))
# # TODO: Review further; seems not to be a problem
# problems(crashes) |> View()

# select relevant variables
crashes <- crashes |> 
    select(COLLISION_DATE, COLLISION_TIME, DISTANCE, DIRECTION,
       INTERSECTION, STATE_HWY_IND, COLLISION_SEVERITY, NUMBER_KILLED,
       NUMBER_INJURED, PARTY_COUNT, PRIMARY_COLL_FACTOR, PCF_VIOL_CATEGORY,
       HIT_AND_RUN, TYPE_OF_COLLISION, PED_ACTION, ROAD_SURFACE, ROAD_COND_1,
       LIGHTING, PEDESTRIAN_ACCIDENT, BICYCLE_ACCIDENT, MOTORCYCLE_ACCIDENT,
       TRUCK_ACCIDENT, ALCOHOL_INVOLVED, STWD_VEHTYPE_AT_FAULT, CHP_VEHTYPE_AT_FAULT,
       COUNT_SEVERE_INJ, COUNT_VISIBLE_INJ, COUNT_COMPLAINT_PAIN, 
       COUNT_PED_KILLED, COUNT_PED_INJURED, COUNT_BICYCLIST_KILLED, 
       COUNT_BICYCLIST_INJURED, COUNT_MC_KILLED, COUNT_MC_INJURED,
       PRIMARY_RAMP, SECONDARY_RAMP, POINT_X, POINT_Y)

# Convert crashes to sf object
crashessf <- crashes |> 
    filter(!(is.na(POINT_X) & is.na(POINT_Y))) |> 
    st_as_sf(coords = c("POINT_X", "POINT_Y"),
             crs = 4269) |> 
    st_transform()


# Slow Streets Analysis------------------------------------------------------

# Research question: did Oakland's Slow Streets program have a 
# meaningful effect on pedestrian and cyclist deaths in the project
# areas? Define project area as within 40m of the Slow Streets.
# Define outer area as being 200m outside of the Slow Streets.


# Join contiguous sections

# identify connecting sections
connected <- (st_intersects(slow))
# connected |> View()

ids <- slow |> st_drop_geometry() |> 
    select("id") |> distinct() |> pull()

segment1 <- st_union(filter(slow, id == ids[1]), 
                     filter(slow, id == ids[46])) |> 
    st_union(filter(slow, id == ids[56])) |> 
    st_union(filter(slow, id == ids[48])) |> 
    suppressWarnings()
# segment1["id"] |> plot()


segment2 <- st_union(filter(slow, id == ids[2]), 
                     filter(slow, id == ids[9])) |> 
    st_union(filter(slow, id == ids[3])) |> 
    st_union(filter(slow, id == ids[43])) |> 
    st_union(filter(slow, id == ids[44])) |> 
    suppressWarnings()
# segment2["id"] |> plot()

segment3 <- st_union(filter(slow, id == ids[4]), 
                     filter(slow, id == ids[5])) |> 
    st_union(filter(slow, id == ids[35])) |>
    st_union(filter(slow, id == ids[38])) |>
    st_union(filter(slow, id == ids[36])) |>
    st_union(filter(slow, id == ids[39])) |>
    st_union(filter(slow, id == ids[40])) |>
    st_union(filter(slow, id == ids[37])) |>
    suppressWarnings()
# segment3["id"] |> plot()

segment4 <- st_union(filter(slow, id == ids[6]), 
                     filter(slow, id == ids[14])) |> 
    st_union(filter(slow, id == ids[31])) |> 
    st_union(filter(slow, id == ids[30])) |> 
    suppressWarnings()
# segment4["id"] |> plot()

segment5 <- st_union(filter(slow, id == ids[7]), 
                     filter(slow, id == ids[11])) |> 
    st_union(filter(slow, id == ids[33])) |> 
    suppressWarnings()
# segment5["id"] |> plot()

segment6 <- filter(slow, id == ids[8])
# segment6["id"] |> plot()

segment7 <- st_union(filter(slow, id == ids[10]), 
                     filter(slow, id == ids[34])) |> 
    suppressWarnings()
# segment7["id"] |> plot()

segment8 <- filter(slow, id == ids[12]) |> 
    suppressWarnings()
# segment8["id"] |> plot()

segment9 <- filter(slow, id == ids[13]) |> 
    suppressWarnings()
# segment9["id"] |> plot()

segment10 <- st_union(filter(slow, id == ids[15]), 
                      filter(slow, id == ids[28])) |> 
    st_union(filter(slow, id == ids[29])) |> 
    st_union(filter(slow, id == ids[32])) |> 
    suppressWarnings()
# segment10["id"] |> plot()

segment11 <- st_union(filter(slow, id == ids[16]), 
                      filter(slow, id == ids[53])) |> 
    st_union(filter(slow, id == ids[51])) |> 
    st_union(filter(slow, id == ids[50])) |> 
    st_union(filter(slow, id == ids[49])) |> 
    suppressWarnings()
# segment11["id"] |> plot()

segment12 <- filter(slow, id == ids[17])
# segment12["id"] |> plot()

segment13 <- st_union(filter(slow, id == ids[18]), 
                      filter(slow, id == ids[19])) |> 
    st_union(filter(slow, id == ids[17])) |>
    st_union(filter(slow, id == ids[20])) |>
    st_union(filter(slow, id == ids[20])) |>
    st_union(filter(slow, id == ids[21])) |>
    st_union(filter(slow, id == ids[22])) |>
    st_union(filter(slow, id == ids[23])) |>
    st_union(filter(slow, id == ids[24])) |>
    st_union(filter(slow, id == ids[25])) |>
    st_union(filter(slow, id == ids[26])) |>
    st_union(filter(slow, id == ids[27])) |>
    st_union(filter(slow, id == ids[47])) |>
    suppressWarnings()
# segment13["id"] |> plot()

segment14 <- filter(slow, id == ids[27])
# segment14["id"] |> plot()

segment15 <- st_union(filter(slow, id == ids[41]),
                      filter(slow, id == ids[42])) |>
    suppressWarnings()
# segment15["id"] |> plot()

segment16 <- st_union(filter(slow, id == ids[45]),
                      filter(slow, id == ids[58])) |>
    st_union(filter(slow, id == ids[59])) |> 
    suppressWarnings()
# segment16["id"] |> plot()

segment17 <- filter(slow, id == ids[47])
# segment17["id"] |> plot()

segment18 <- st_union(filter(slow, id == ids[52]), 
                      filter(slow, id == ids[54])) |> 
    suppressWarnings()
# segment18["id"] |> plot()

segment19 <- filter(slow, id == ids[55])
# segment19["id"] |> plot()

segment20 <- filter(slow, id == ids[57])
# segment20["id"] |> plot()

segment21 <- st_union(filter(slow, id == ids[60]), 
                      filter(slow, id == ids[64])) |> 
    suppressWarnings()
# segment21["id"] |> plot()

segment22 <- st_union(filter(slow, id == ids[61]), 
                      filter(slow, id == ids[62])) |> 
    st_union(filter(slow, id == ids[63])) |> 
    suppressWarnings()
# segment22["id"] |> plot()

segmentlist <- list(segment1, segment2, segment3, segment4,
                    segment5, segment6, segment7, segment8,
                    segment9, segment10, segment11, segment12,
                    segment13, segment14, segment15, segment16,
                    segment17, segment18, segment19, segment20,
                    segment21, segment22)

inbuflist <- map(1:length(segmentlist), 
                 \(x) (st_buffer(segmentlist[[x]], 40)))
outbuflist <- map(1:length(segmentlist), 
                 \(x) (st_buffer(segmentlist[[x]], 200)))


# Function that returns crash points clipped from either the 40m
# inner clip or 200m outer donut clip of a single Slow Street
clip_n_crash <- function(index, InOrOut) {
    # select Slow Street
    id_in <- inbuflist[[index]]

    if (InOrOut == "in") {
        crash_in <- st_intersection(crashessf, id_in) |> suppressWarnings()
        in_list <- list("buffer" = id_in,
                        "crashes" = crash_in)
        return(in_list)

    } else if (InOrOut == "out") {
        # select Slow Street
        id_out <- outbuflist[[index]]

        # clip out inside from outside, leaving ring buffer
        id_clip <- st_sym_difference(id_in, id_out) |>
            suppressWarnings()

        # return warning if any invalid geometries
        is_invalid <- id_clip |> st_is_valid()
        if (length(which(!is_invalid)) > 0) print("Warning: Invalid geometry")

        crash_out <- st_intersection(crashessf, id_clip) |> suppressWarnings()
        out_list <- list("buffer" = id_clip,
                        "crashes" = crash_out)
        return(out_list)
    }
}
# test
clip_n_crash(1, "out") |> View()

# Map using each id with purrr. 
# TODO: Consider using furrr.

# create indices
segindices <- c(1:length(segmentlist))
# create df for inner crashes and outer crashes
inner_crasheslist <- map(segindices, clip_n_crash, "in")
outer_crasheslist <- map(segindices, clip_n_crash, "out")

# bind into dataframes
outer_crashes <- 1:length(segindices) |> 
    map_df(\(x) (bind_rows(pluck(outer_crasheslist, x, 2))))
inner_crashes <- 1:length(segindices) |> 
    map_df(\(x) (bind_rows(pluck(inner_crasheslist, x, 2))))
outer_buffer <- 1:length(segindices) |> 
    map_df(\(x) (bind_rows(pluck(outer_crasheslist, x, 1))))
inner_buffer <- 1:length(segindices) |> 
    map_df(\(x) (bind_rows(pluck(inner_crasheslist, x, 1))))

# Save
inner_crashes |> write_rds("inner_crashes.rds")
outer_crashes |> write_rds("outer_crashes.rds")
outer_buffer |> write_rds("outer_buffer.rds")
inner_buffer |> write_rds("inner_buffer.rds")


# Reset graphics state if needed
par(mar = rep(1, 4))

# Narrow down to pedestrian/bike crashes; append pre/post variable
pedbike_inner <- inner_crashes |> 
    mutate(SLOWORNO = 
               case_when((COLLISION_DATE < ymd(20200601) |
                              COLLISION_DATE > ymd(20220215)) ~ "NO",
                         (COLLISION_DATE > ymd(20200601) &
                              COLLISION_DATE < ymd(20220215)) ~ "SLOW"),
        BIKEPED = case_when(
            !(is.na(PEDESTRIAN_ACCIDENT) |
                  is.na(BICYCLE_ACCIDENT)) ~ 1,
            .default = 0
        ))
pedbike_inner <- pedbike_inner |> filter(BIKEPED == 1)
pedbike_outer <- outer_crashes |> 
    mutate(SLOWORNO = 
               case_when((COLLISION_DATE < ymd(20200601) |
                              COLLISION_DATE > ymd(20220215)) ~ "NO",
                         (COLLISION_DATE > ymd(20200601) &
                              COLLISION_DATE < ymd(20220215)) ~ "SLOW"),
           BIKEPED = case_when(
               !(is.na(PEDESTRIAN_ACCIDENT) |
                     is.na(BICYCLE_ACCIDENT)) ~ 1,
               .default = 0
           ))
pedbike_outer <- pedbike_outer |> filter(BIKEPED == 1)
# plot(pedbike_outer["SLOWORNO"])
# plot(pedbike_outer["BIKEPED"])
# plot(pedbike_inner["SLOWORNO"])
# plot(pedbike_inner["BIKEPED"])


# Student's T.test
# Interpretation: The Slow Streets program appeared to have a small effect
# on pedestrian and bicycle deaths within the Slow Streets corridors 
# (mean crashes during Slow Streets .0040 vs control .0000; p-value .537)
# compared to the area directly outside of the corridors
# (mean crashes during Slow Streets .0020 vs control .0025; p-value .873), 
# but the sample size is not large enough for this difference to be significant.
# This small sample size is due to the low number of crashes within the areas,
# and the relatively short time period that Slow Streets was enacted. 
# The COVID lockdown is a potentially confounding variable because Slow Streets
# was enacted during a time that crash rates were already lower because of 
# the lockdown; however, it is controlled for by the outer buffer control
# variable, which also spans the COVID lockdown time period.

# Outer ring
t.test(BIKEPED ~ SLOWORNO, pedbike_outer, var.equal = T)
# Inner ring
t.test(BIKEPED ~ SLOWORNO, pedbike_inner, var.equal = T)


# load file with Oakland census boundaries for border
demoacs <- read_rds("../Oakland_Crime/Data/demoacs.rds")

# create border
oak_border <- tm_shape(demoacs) + 
    tm_fill()

slowperiod <- oak_border +
    tm_shape(outer_buffer) +
    tm_polygons(col = "yellow") +
    tm_shape(filter(pedbike_outer, 
                    SLOWORNO == "SLOW")) +
    tm_symbols(col = "blue",
               size = .08) 
    # SLOW period has no crashes, so commented out
    # tm_shape(filter(pedbike_inner, 
    #                 SLOWORNO == "SLOW")) +
    # tm_symbols(col = "red",
    #            size = .08)
noperiod <- oak_border +
    tm_shape(outer_buffer) +
    tm_polygons(col = "yellow") +
    tm_shape(filter(pedbike_outer, 
                    SLOWORNO == "NO")) +
    tm_symbols(col = "blue",
               size = .08) +
    tm_shape(filter(pedbike_inner, 
                    SLOWORNO == "NO")) +
    tm_symbols(col = "red",
               size = .08)
tmap_save(noperiod, filename = "noperiod.jpg") 
tmap_save(slowperiod, filename = "slowperiod.jpg") 

# TODO: make maps look way better