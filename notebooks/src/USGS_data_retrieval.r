# library(devtools)
# install_github("USGS-R/dataRetrieval")

# detach("package:naniar", unload=TRUE)

options(repr.matrix.max.rows=600, repr.matrix.max.cols=200)

R.Version()$version.string

packageVersion("dataRetrieval")

ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

packages <- c('padr', 'dplyr', 'dataRetrieval', 'ggplot2', 'data.table', 'rgdal', 'leaflet', 'htmlwidgets')
ipak(packages)

# out_dir = "/global/project/projectdirs/m1800/pin/Reach_scale_model/data/well_data/GW_chemistry/"
out_dir = "~/Dropbox/PNNL/Projects/Columbia_Basin/data/"

fig_dir = "~/Dropbox/PNNL/Projects/Delaware_river_basin/figures/"
gis_file = "~/Dropbox/PNNL/Projects/Delaware_river_basin/GIS/"

CRB_shp = "~/Dropbox/PNNL/Projects/Columbia_Basin/Reach_domain_QGIS/CRB/CR_shape/CR.shp"
CRB_HU4_shp = "~/Dropbox/PNNL/Projects/Columbia_Basin/Reach_domain_QGIS/CRB/WBDHU/CRB_WBDHU4.shp"

# DRB_bound <- readOGR(paste0(gis_file, "DRB_bound/DRB_bound.shp") )
# delaware_river <- readOGR(paste0(gis_file, "delawareriver/delawareriver.shp") )
# delaware_river_trib <- readOGR(paste0(gis_file, "drb_riv_arc/drb_riv_arc.shp") )
# neversink <- readOGR(paste0(gis_file, "Neversink/Neversink.shp") )
# frenchCreek <- readOGR(paste0(gis_file, "french_creek_watershed/french_creek_watershed.shp") )
# delawareGap <- readOGR(paste0(gis_file, "delaware_gap/delawareGap.shp") )

# # transform cooridnated system
# DRB_bound <- spTransform(DRB_bound, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# delaware_river <- spTransform(delaware_river, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# delaware_river_trib <- spTransform(delaware_river_trib, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# neversink <- spTransform(neversink, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# frenchCreek <- spTransform(frenchCreek, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# delawareGap <- spTransform(delawareGap, CRS("+proj=longlat +datum=WGS84 +no_defs"))

CRB_bound <- readOGR(CRB_shp)
CRB_bound <- spTransform(CRB_bound, CRS("+proj=longlat +datum=WGS84 +no_defs"))

CRB_HU4 <- readOGR(CRB_HU4_shp)
CRB_HU4 <- spTransform(CRB_HU4, CRS("+proj=longlat +datum=WGS84 +no_defs"))

fillGapNA <- function(df, t_day){
    ## inputs: df-- the data.frame; timeStamp-- datetime column variable; t_day-- target time gap
    ## outputs: return df in data.table format with filled NA in the specified gap
    
    # set data.table
    setDT(df)
#     col = get(timeStamp)
    # find gap larger than t_gap min
    t_gap = t_day*24*60 # in minutes
    df[, gap := c(diff(timeStamp) > t_gap, F)]

    # insert NA in between the gap
    df = df[, if(gap) rbind(.SD, .(timeStamp = timeStamp + t_gap*60/2), fill = T)
     else .SD
    , by = 1:nrow(df)
    ][, -'gap']
    
    return(df)
}

# colors <- c("red", "white", "blue", "white", "blue", "red")
#         labels <- c("filled_square", "empty_square", "big_square", "empty_circle", "filled_circle", "big_circle")
#         sizes <- c(10, 20, 30, 10, 20, 30)
#         shapes <- c("square", "square", "square", "circle", "circle", "circle")
#         borders <- c("red", "blue", "black", "blue", "blue", "black")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.8){

            make_shapes <- function(colors, sizes, borders, shapes) {
                shapes <- gsub("circle", "50%", shapes)
                shapes <- gsub("square", "0%", shapes)
                paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
            }
            make_labels <- function(sizes, labels) {
                paste0("<div style='display: inline-block;height: ", 
                       sizes, "px;margin-top: 4px;line-height: ", 
                       sizes, "px;'>", labels, "</div>")
            }

            legend_colors <- make_shapes(colors, sizes, borders, shapes)
            legend_labels <- make_labels(sizes, labels)

            return(addLegend(map, colors = legend_colors, 
                             labels = legend_labels, opacity = opacity, title = "Site Type"))
        }

#find all the parameter codes
parameterCdFile <- parameterCdFile
names(parameterCdFile)
write.csv(parameterCdFile,'parameterCdFile.csv')

#find para codes with "nitrate"
NO3Cds <- parameterCdFile[grep("nitrate",
                                parameterCdFile$parameter_nm,
                                ignore.case=TRUE),]

?readNWISdata

# pCode <- c("00618","00620") #nitrate, water, mg/L
# pCode = "00010"
# pCode = c("00010", "00060", "00065")

# startDate <- "2019-01-01"
# endDate <- "2019-03-01"
# siteNumber = c("01434025", "01434021", "0143400680")
siteNumber = "01472150"
# parameterCd = "00600"
pCode = c("00060", "00065")
startDate <- as.Date("1980-01-01")
# endDate = as.Date('2019-01-01')
endDate = Sys.Date()

statCd <- "00003" #00001	Maximum; 00002	Minimum ; 00003	Mean; 00008	Median

# siteNumber <- "12510500" # USGS 12510500 YAKIMA RIVER AT KIONA, WA
# siteNumber <- "12472800" # COLUMBIA RIVER BELOW PRIEST RAPIDS DAM, WA
# siteNumber <- "12514400" # Columbia river at Pasco bridge

siteInfo <- readNWISsite(siteNumber)

names(siteInfo)

siteInfo$alt_va

avail_data = whatNWISdata(siteNumber = siteNumber)

avail_data

parameter_cd = avail_data$parm_cd

readNWISpCode(parameter_cd)

para_data = avail_data %>%
    filter(parm_cd %in% c("00060", "00061"))

# data = readNWISdata(siteNumber = siteNumber, service = "uv", parameterCd = "00061")
data = readNWISuv(siteNumbers = siteNumber, parameterCd = "00061")

rating_base <- readNWISrating(siteNumber="12472800")

rating_base

rating_exsa <- readNWISrating(siteNumber="12472800", type = 'exsa')

rating_exsa

rating_base_log = rating_base

rating_base_log$DEP = log(rating_base_log$DEP)
rating_base_log$INDEP = log(rating_base_log$INDEP)

lm.rating = lm(DEP~INDEP, data=rating_base_log)

lm.sum = summary(lm.rating)
lm.sum

a = exp(lm.rating$coefficients[1])
b = lm.rating$coefficients[2]

fitted_y = a*(rating_base$INDEP)^b

ray_y = -7.252 + 0.138*(rating_base$INDEP)^0.438

plot(rating_base$INDEP, rating_base$DEP)
lines(rating_base$INDEP, fitted_y, col = 'red')
lines(rating_base$INDEP, ray_y, col = 'blue')
text(10, 5.5e5, paste0('y = ax^b  (', 'a = ', sprintf("%.2f",a), ', ', 'b = ', sprintf("%.2f",b), ')'))
text(5, 5e5, sprintf("R^2 = %.2f",lm.sum$adj.r.squared))
legend(25, 10e4, c('raw', 'rating curve', 'rating curve by Ray'), col = c('black', 'red', 'blue'), 
       lty = c(NA, 1, 1), pch= c(1, NA, NA))

rating_base_log = rating_exsa

rating_base_log$DEP = log(rating_base_log$DEP)
rating_base_log$INDEP = log(rating_base_log$INDEP)

lm.rating = lm(INDEP~DEP, data=rating_base_log)

lm.sum = summary(lm.rating)
lm.sum

a = exp(lm.rating$coefficients[1])
b = lm.rating$coefficients[2]

fitted_y = a*(rating_exsa$DEP)^b

ray_y = -7.252 + 0.138*(rating_exsa$DEP)^0.438

plot(rating_exsa$DEP, rating_exsa$INDEP)
lines(rating_exsa$DEP, fitted_y, col = 'red', lw = 2)
lines(rating_exsa$DEP, ray_y, col = 'blue', lw=2)
text( 1.5e5, 35, paste0('y = ax^b  (', 'a = ', sprintf("%.4f",a), ', ', 'b = ', sprintf("%.2f",b), ')'))
text(1e5, 30, sprintf("R^2 = %.2f",lm.sum$adj.r.squared))
legend(3.5e5, 10, c('raw', 'rating curve', 'rating curve by Ray'), col = c('black', 'red', 'blue'), 
       lty = c(NA, 1, 1), pch= c(1, NA, NA))

# siteNumbers = c("01434025", "01434021", "0143400680", "01435000") # neversink
siteNumbers = c("01472157", "01472150") # frenchCk
# siteNumber = "01435000"

# pCode = c("00060", "00065")
pCode = "00060"
startDate <- as.Date("1980-01-01")
# endDate = as.Date('2019-01-01')
endDate = Sys.Date()

statCd <- "00003" #00001	Maximum; 00002	Minimum ; 00003	Mean; 00008	Median

storm_df = data.frame("name"=c("Sandy", "Irene", "Ivan"), 
                      "begin_date"= c(as.Date("2012-10-22"), as.Date("2011-08-26"), as.Date("2004-09-17")),
                     "end_date"= c(as.Date("2012-11-2"), as.Date("2011-08-28"), as.Date("2004-09-19")))

y_offset = c(1000, 1000, 1000)
x_offset = c(300, -300, 0)

for (siteNumber in siteNumbers){    
    print(siteNumber)
    df <- readNWISdv(siteNumber = siteNumber, parameterCd = pCode, startDate,  endDate)
    df = renameNWISColumns(df)
    df = df[df$Flow >= 0, ]

    #get variable and site info
    variableInfo <- attr(df, "variableInfo")
    siteInfo <- attr(df, "siteInfo")

    # # convert gage height
    # datum = siteInfo$alt_va
    # GH_new <- as.data.frame(lapply(df$GH, FUN=function(x) datum+x)) 

    titlename = paste(siteInfo$station_nm, siteInfo$site_no)
    p = ggplot(df, aes(Date, Flow)) + geom_line() + 
            xlim(startDate, endDate) + scale_y_continuous(trans='log10') + 
            ggtitle(titlename) + ylab(variableInfo$variableDescription[1]) +
            annotate("rect", xmin = storm_df$begin_date,
                  xmax = storm_df$end_date, ymin = min(df$Flow), ymax = max(df$Flow),  
                  fill = "red", alpha=0.5, color = "red", size = 0.5) + 
            annotate(geom = "text", x = storm_df$begin_date + x_offset, y = max(df$Flow) + y_offset, 
                     label = storm_df$name, 
                     color = "red")

#     print(p)

    ggsave(paste0(fig_dir,titlename,"_", variableInfo$variableDescription[1],".png"), 
           width = 8, height = 5, units = "in")
}

# siteNumber = c("01434025", "01434021", "0143400680", "01435000")
# siteNumber = "01435000"
siteNumber = "12510500"

pCode = c("00060", "00065")
# pCode = "000618"
startDate <- as.Date("2000-01-01")
# endDate = as.Date('2019-01-01')
endDate = Sys.Date()

statCd <- "00003" #00001	Maximum; 00002	Minimum ; 00003	Mean; 00008	Median

df <- readNWISdata(siteNumber = siteNumber, parameterCd = pCode, startDate = startDate,
                   endDate = endDate, service = "dv")

df = renameNWISColumns(df)

df = df[df$Flow >= 0, ]

str(df)


#get variable and site info
variableInfo <- attr(df, "variableInfo")
siteInfo <- attr(df, "siteInfo")

# convert gage height
datum = siteInfo$alt_va
GH_new <- as.data.frame(lapply(df$GH, FUN=function(x) datum+x)) 

storm_df = data.frame("name"=c("Sandy", "Irene", "Ivan"), 
                      "begin_date"= c(as.Date("2012-10-22"), as.Date("2011-08-26"), as.Date("2004-09-17")),
                     "end_date"= c(as.Date("2012-11-2"), as.Date("2011-08-28"), as.Date("2004-09-19")))

storm_df

# y_offset = c(1000, 1000, 1000)
# x_offset = c(300, -300, 0)
titlename = paste(siteInfo$station_nm, siteInfo$site_no)
p = ggplot(df, aes(dateTime, Flow)) + geom_line() +
#         xlim(startDate, endDate)  
        ggtitle(titlename) + ylab(variableInfo$variableDescription[1]) +
            scale_y_continuous(trans='log10')
#         annotate("rect", xmin = storm_df$begin_date,
#               xmax = storm_df$end_date, ymin = min(df$Flow), ymax = max(df$Flow),  
#               fill = "red", alpha=0.5, color = "red", size = 0.5) + 
#         annotate(geom = "text", x = storm_df$begin_date + x_offset, y = max(df$Flow) + y_offset, label = storm_df$name, 
#                  color = "red")

print(p)

ggsave(paste0(fig_dir,titlename,"_", variableInfo$variableDescription[1],".png"), 
       width = 8, height = 5, units = "in")

# siteNumbers = c("01434025", "01434021", "0143400680", "01435000") # neversink
# siteNumbers = "01472157"
siteNumber = "12510500"
# pCode = c("00618", "00681") # NO3, DOC
# pCode = c("00010", "00300", "00618", "00681")

pCode = "00618" # nitrate
# pCode = "00681" #DOC
startDate <- as.Date("2000-01-01")
# endDate = as.Date('2019-01-01')
endDate = Sys.Date()

# statCd <- "00003" #00001	Maximum; 00002	Minimum ; 00003	Mean; 00008	Median

storm_df = data.frame("name"=c("Sandy", "Irene", "Ivan"), 
                      "begin_date"= c(as.Date("2012-10-22"), as.Date("2011-08-26"), as.Date("2004-09-17")),
                     "end_date"= c(as.Date("2012-11-2"), as.Date("2011-08-28"), as.Date("2004-09-19")))

y_offset = c(0.5, 0.5, 0.5)
x_offset = c(300, -300, 0)

for (siteNumber in siteNumbers){
    print(siteNumber)
    for (icode in pCode){
        print(icode)
        df <- readNWISqw(siteNumber = siteNumber, parameterCd = icode, startDate,  endDate)
        df = renameNWISColumns(df)

        #get variable and site info
        variableInfo <- attr(df, "variableInfo")
        siteInfo <- attr(df, "siteInfo")

        # **plot data**
        titlename = paste(siteInfo$station_nm, siteInfo$site_no)
        p = ggplot(df, aes(sample_dt, result_va)) +  geom_line(color = "gray") + 
                geom_point(color = "dodgerblue", fill = NA, alpha = 1) +
                xlim(startDate, endDate) + xlab("Date") +
                ggtitle(titlename) + ylab(variableInfo$parameter_nm) +
                annotate("rect", xmin = storm_df$begin_date,
                      xmax = storm_df$end_date, ymin = min(df$result_va), ymax = max(df$result_va),  
                      fill = "red", alpha=0.5, color = "red", size = 0.5) + 
                annotate(geom = "text", x = storm_df$begin_date + x_offset, y = max(df$result_va) + y_offset, 
                         label = storm_df$name, 
                         color = "red")
        
        ggsave(paste0(fig_dir,titlename,"_", variableInfo$parameter_nm,".png"), 
           width = 8, height = 5, units = "in")
        
    }
    
}


data <- readNWISqw(siteNumber = siteNumber, parameterCd = pCode, startDate,  endDate)

data = renameNWISColumns(data)

dim(data)

#get variable and site info
variableInfo <- attr(data, "variableInfo")
siteInfo <- attr(data, "siteInfo")

variableInfo$parameter_nm

write.csv(data, "~/Dropbox/Conferences_and_Meetings/PI meetings/PI meeting 2020/data/USGS_12510500_NO3.csv")

par(mar = c(5, 5, 5, 5))  #sets the margin (mar) size of the plot window

plot(data$sample_dt, data$result_va, type="b", ylab = variableInfo$parameter_nm, 
     xlab = "")
# par(new = TRUE)
# plot(data$Date, GH_new, col = "red", 
#      type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
#      axes = FALSE)
# axis(4, col = "red", col.axis = "red")
# mtext(variableInfo$variableDescription[2], side = 4, line = 3, col = "red")
titlename = paste(siteInfo$station_nm, "-",siteInfo$site_no)
title(titlename)
# legend("topleft", variableInfo$unit, col = c("black", 
#                                                     "red"), lty = c(NA, 1), pch = c(1, NA))
# dev.copy(jpeg, width=8,height=6,units='in',res=300,quality=100, 
#          file = paste0(fig_dir,titlename,"_", variableInfo$parameter_nm,".jpg"))
# dev.off()

# siteNumber = c("01434025", "01434021", "0143400680", "01435000")
# siteNumber = "97992339"
# siteNumber = "452840122302202"
# siteNumber = "465033122570202"
siteNumber = "415944074280801" #neversink

# pCode = c("00060", "00065")
# pCode = "00060"
# pCode = "00681" #DOC
# pCode = "00618" # nitrate
startDate <- as.Date("1980-01-01")
# endDate = as.Date('2019-01-01')
endDate = Sys.Date()

# statCd <- "00003" #00001	Maximum; 00002	Minimum ; 00003	Mean; 00008	Median

data <- readNWISgwl(siteNumbers = siteNumber,  startDate = startDate,  endDate = endDate)

# data = renameNWISColumns(data)

head(data)

siteNumbers = c("01434025", "01435000", "0143400680", "01440000", 
                "01472157", "01438500", "01463500", "01428500", "01427510", "01434000")

# siteNumber = c("01434025", "01434021", "0143400680", "01435000")
# siteNumber = "01435000"
# siteNumber = "01434021" 
# siteNumber = "0143400680" 
# siteNumber = "01434025" 

# siteNumber = "01472157" 

# siteNumber = "01463500" # Delaware River at Trenton NJ

# siteNumber = "01440000" # GAP

# siteNumber = "01438500" # 

# siteNumber = "01457500"

# siteNumber = "01427510" #01427510 DELAWARE RIVER AT CALLICOON NY
# siteNumber = "01427207" #01427207 DELAWARE RIVER AT LORDVILLE NY
# siteNumber = "01434000" #01434000 DELAWARE RIVER AT PORT JERVIS NY
# siteNumber = "01428500" #01428500 DELAWARE R ABOVE LACKAWAXEN R NEAR BARRYVILLE NY

# pCode = c("00060", "00065")
# pCode = "00060"
startDate <- as.Date("1980-01-01")
# endDate = as.Date('2019-01-01')
endDate = Sys.Date()

statCd <- "00003" #00001	Maximum; 00002	Minimum ; 00003	Mean; 00008	Median

begin = as.POSIXct("1980-01-01",tz="UTC")
end = as.POSIXct("2020-01-01",tz="UTC")

# siteNumbers = "01434025"

for (siteNumber in siteNumbers){
    
    print(siteNumber)
    ##**discharge**

    df1 <- readNWISdata(siteNumber = siteNumber, parameterCd = "00060", startDate = startDate,
                       endDate = endDate, service = "dv")

    df1 = renameNWISColumns(df1)
    names(df1)[names(df1) == 'dateTime'] <- 'timeStamp'

    df1 = df1[df1$Flow >= 0, ]

    #get variable and site info
    variableInfo1 <- attr(df1, "variableInfo")
    siteInfo1 <- attr(df1, "siteInfo")

    # fill gap > 30 day with NA
    df1 = fillGapNA(df1, 30)

    # str(df1)

    ##**DOC**

    df2 <- readNWISqw(siteNumber = siteNumber, parameterCd = "00681", startDate  = startDate,  endDate = endDate)

    df2 = renameNWISColumns(df2)
    names(df2)[names(df2) == 'startDateTime'] <- 'timeStamp'

    #get variable and site info
    variableInfo2 <- attr(df2, "variableInfo")
    siteInfo2 <- attr(df2, "siteInfo")

    # df2

    # fill gap > 30 day with NA
    df2 = fillGapNA(df2, 30)

    ##**NO3**

    df3 <- readNWISqw(siteNumber = siteNumber, parameterCd = "00618", startDate  = startDate,  endDate = endDate)

    df3 = renameNWISColumns(df3)
    names(df3)[names(df3) == 'startDateTime'] <- 'timeStamp'

    #get variable and site info
    variableInfo3 <- attr(df3, "variableInfo")
    siteInfo3 <- attr(df3, "siteInfo")

    # fill gap > 30 day with NA
    df3 = fillGapNA(df3, 30)

    # str(df3)

    ### time series plot

    layout(matrix(c(1, 2, 3), ncol = 1), widths = 1, heights = c(3,2.5,3), respect = FALSE)
    par(mar = c(0, 4.1, 4.1, 2.1))

    titlename = c(siteInfo1$station_nm, siteInfo1$site_no)

    with(df1, plot(Flow~timeStamp, type = 'l', xaxt = 'n', main = titlename, 
                   xlim = c(begin, end), ylab = 'Discharge (cfs)'))

    par(mar = c(0, 4.1, 0.5, 2.1))
    with(df2, plot(result_va~timeStamp, type = 'l', col = "blue", xlim = c(begin, end), 
                   ylab = 'DOC (mg/L)', xaxt = 'n', xlab = NA))

    par(mar = c(4.1, 4.1, 0.5, 2.1))
    with(df3, plot(result_va~timeStamp, type = 'l', col = "green", xlim = c(begin, end), 
                   ylab = 'Nitrate (mg/L)', xlab = NA))

    dev.copy(jpeg, width=8,height=6,units='in',res=300,quality=100, 
             file = paste0(fig_dir,titlename,"_", "Discharge_DOC_NO3",".jpg"))
    dev.off()

    ### one2one plot

    ##**aggregate to use daily value for one2one plot**

    Q = df1 %>%
        thicken(interval = 'day', by = 'timeStamp') %>%
        group_by(timeStamp_day) %>%
        summarize(daily_mean = mean(Flow)) %>%
        filter(!is.na(daily_mean))

    DOC = df2 %>%
        thicken(interval = 'day', by = 'timeStamp') %>%
        group_by(timeStamp_day) %>%
        summarize(daily_mean = mean(result_va)) %>%
        filter(!is.na(daily_mean))

    NO3 = df3 %>%
        thicken(interval = 'day', by = 'timeStamp') %>%
        group_by(timeStamp_day) %>%
        summarize(daily_mean = mean(result_va)) %>%
        filter(!is.na(daily_mean))

    Q_DOC = merge(Q, DOC, by = "timeStamp_day")

    Q_NO3 = merge(Q, NO3, by = "timeStamp_day")

    ggplot(Q_DOC, aes(x=daily_mean.x, y=daily_mean.y)) + geom_point(alpha = 0.3) + 
         geom_smooth(method = "lm") + xlab("Discharge (cfs)") + ylab("DOC (mg/L)") + ggtitle(titlename)

    ggsave(paste0(fig_dir,titlename,"_Q_DOC.png"), device = "png", width = 6, height = 5, units = "in")

    ggplot(Q_NO3, aes(x=daily_mean.x, y=daily_mean.y)) + geom_point(alpha = 0.3) + 
         geom_smooth(method = "lm") + xlab("Discharge (cfs)") + ylab("Nitrate (mg/L)") + ggtitle(titlename)

    ggsave(paste0(fig_dir,titlename,"_Q_NO3.png"), device = "png", width = 6, height = 5, units = "in")
    
}

# siteNumber = c("01434025", "01434021", "0143400680", "01435000")
# siteNumber = "01435000"
# siteNumber = "01434021" 
# siteNumber = "0143400680" 
siteNumber = "01434025" 

# siteNumber = "01472157" 

# siteNumber = "01463500" # Delaware River at Trenton NJ

# siteNumber = "01440000" # GAP

siteNumber = "01438500" # 

# siteNumber = "01457500"

# siteNumber = "01427510" #01427510 DELAWARE RIVER AT CALLICOON NY
# siteNumber = "01427207" #01427207 DELAWARE RIVER AT LORDVILLE NY
# siteNumber = "01434000" #01434000 DELAWARE RIVER AT PORT JERVIS NY
# siteNumber = "01428500" #01428500 DELAWARE R ABOVE LACKAWAXEN R NEAR BARRYVILLE NY

# pCode = c("00060", "00065")
pCode = "00060"
startDate <- as.Date("1980-01-01")
# endDate = as.Date('2019-01-01')
endDate = Sys.Date()

statCd <- "00003" #00001	Maximum; 00002	Minimum ; 00003	Mean; 00008	Median

begin = as.POSIXct("1980-01-01",tz="UTC")
end = as.POSIXct("2020-01-01",tz="UTC")

df1 <- readNWISdata(siteNumber = siteNumber, parameterCd = "00060", startDate = startDate,
                   endDate = endDate, service = "dv")

df1 = renameNWISColumns(df1)
names(df1)[names(df1) == 'dateTime'] <- 'timeStamp'

df1 = df1[df1$Flow >= 0, ]

#get variable and site info
variableInfo1 <- attr(df1, "variableInfo")
siteInfo1 <- attr(df1, "siteInfo")

# fill gap > 30 day with NA
df1 = fillGapNA(df1, 30)

# str(df1)

df2 <- readNWISqw(siteNumber = siteNumber, parameterCd = "00681", startDate  = startDate,  endDate = endDate)

df2 = renameNWISColumns(df2)
names(df2)[names(df2) == 'startDateTime'] <- 'timeStamp'

#get variable and site info
variableInfo2 <- attr(df2, "variableInfo")
siteInfo2 <- attr(df2, "siteInfo")

# fill gap > 30 day with NA
df2 = fillGapNA(df2, 30)

df2_copy = df2[df2$timeStamp < as.POSIXct('2010-01-01')]

df2_copy = df2 %>% thicken(interval = 'day', by = 'timeStamp') %>% 
    pad(interval = "day", by = "timeStamp_day")

df2_copy

df3 <- readNWISqw(siteNumber = siteNumber, parameterCd = "00618", startDate  = startDate,  endDate = endDate)

df3 = renameNWISColumns(df3)
names(df3)[names(df3) == 'startDateTime'] <- 'timeStamp'

#get variable and site info
variableInfo3 <- attr(df3, "variableInfo")
siteInfo3 <- attr(df3, "siteInfo")

# fill gap > 30 day with NA
df3 = fillGapNA(df3, 30)

# str(df3)

layout(matrix(c(1, 2, 3), ncol = 1), widths = 1, heights = c(3,2.5,3), respect = FALSE)
par(mar = c(0, 4.1, 4.1, 2.1))

titlename = c(siteInfo1$station_nm, siteInfo1$site_no)

with(df1, plot(Flow~timeStamp, type = 'l', xaxt = 'n', main = titlename, 
               xlim = c(begin, end), ylab = 'Discharge (cfs)'))

par(mar = c(0, 4.1, 0.5, 2.1))
with(df2_copy, plot(result_va~timeStamp, type = 'l', col = "blue", xlim = c(begin, end), 
               ylab = 'DOC (mg/L)', xaxt = 'n', xlab = NA))

par(mar = c(4.1, 4.1, 0.5, 2.1))
with(df3, plot(result_va~timeStamp, type = 'l', col = "green", xlim = c(begin, end), 
               ylab = 'Nitrate (mg/L)', xlab = NA))

dev.copy(jpeg, width=8,height=6,units='in',res=300,quality=100, 
         file = paste0(fig_dir,titlename,"_", "Discharge_DOC_NO3",".jpg"))
dev.off()

Q = df1 %>%
    thicken(interval = 'day', by = 'timeStamp') %>%
    group_by(timeStamp_day) %>%
    summarize(daily_mean = mean(Flow)) %>%
    filter(!is.na(daily_mean))

DOC = df2 %>%
    thicken(interval = 'day', by = 'timeStamp') %>%
    group_by(timeStamp_day) %>%
    summarize(daily_mean = mean(result_va)) %>%
    filter(!is.na(daily_mean))

NO3 = df3 %>%
    thicken(interval = 'day', by = 'timeStamp') %>%
    group_by(timeStamp_day) %>%
    summarize(daily_mean = mean(result_va)) %>%
    filter(!is.na(daily_mean))

Q_DOC = merge(Q, DOC, by = "timeStamp_day")

Q_NO3 = merge(Q, NO3, by = "timeStamp_day")

ggplot(Q_DOC, aes(x=daily_mean.x, y=daily_mean.y)) + geom_point(alpha = 0.3) + 
    geom_abline(slope =1, color = "red") + xlab("Discharge (cfs)") + ylab("DOC (mg/L)") + ggtitle(titlename)

ggsave(paste0(fig_dir,titlename,"_Q_DOC.png"), device = "png", width = 6, height = 5, units = "in")

ggplot(Q_NO3, aes(x=daily_mean.x, y=daily_mean.y)) + geom_point(alpha = 0.3) + 
    geom_abline(slope =1, color = "red") + xlab("Discharge (cfs)") + ylab("Nitrate (mg/L)") + ggtitle(titlename)

ggsave(paste0(fig_dir,titlename,"_Q_NO3.png"), device = "png", width = 6, height = 5, units = "in")

# pCode = c("00060", "00065")
pCode = c("00618")
# pCode <- c("00662","00665")

data <- readNWISdata(stateCd="WA", parameterCd=pCode,
                     service="site", seriesCatalogOutput=TRUE)

head(data)

write.csv(data, "./WAsites_NO3.csv")

sub_data = filter(data, dec_lat_va >= "46.2" & dec_lat_va <= "47.0") %>%
  filter(dec_long_va >= "-118.6" & dec_long_va <= "-120.0") %>%
  filter(parm_cd %in% pCode)
# write.csv(NO3_hanford, paste(fname, "NO3_hanford.csv"))
siteNo = unique(sub_data$site_no)

head(sub_data)

m <- leaflet(data=sub_data) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(~dec_long_va,~dec_lat_va,
                   color = "red", radius=3, stroke=FALSE,
                   fillOpacity = 0.5, opacity = 0.8,
                   popup=~station_nm)

m

saveWidget(m, file=paste(out_dir, "m.html", sep = ""))

pCode = c("00010","00060", "00065","00095","00300","00301","00400", "00618", "00620", "00681","63680","72019","99133") # river gage parameters.

# latitude and longitude box region (lowerleft_lat, lowerleft_lon, upperright_lat, upperright_long)
# box.region = c(-120.0,46.2,-118.6,47.0) #hanford
# box.region = c(-76.6,38.3,-74.0,42.6) #delaware river basin

# HUCcode = c("02040201","02040202", "02040203", "02040204","02040205", 
#             "02040206", "02040207", "02040101","02040102", "02040103", 
#             "02040104","02040105", "02040106") # DRB
HUCcode = c("14020001")
# HUC1 = c("02040201","02040202", "02040203", "02040204","02040205", 
#             "02040206", "02040207", "02040101","02040102", "02040103")
# HUC2 = c("02040104","02040105", "02040106")

## use long_lat box to query data from sites
# sites <- readNWISdata(bBox = box.region, parameterCd = pCode, service="site", asDateTime = TRUE,
#                      seriesCatalogOutput=TRUE)

# sites1 <- readNWISdata(huc = HUC1, parameterCd = pCode, service="site", asDateTime = TRUE,
#                      seriesCatalogOutput=TRUE)
# sites2 <- readNWISdata(huc = HUC2, parameterCd = pCode, service="site", asDateTime = TRUE,
#                      seriesCatalogOutput=TRUE)
sites <- readNWISdata(huc = HUCcode, parameterCd = pCode, service="site", asDateTime = TRUE,
                     seriesCatalogOutput=TRUE)

# sites = bind_rows(sites1, sites2)

sub_sites = sites %>%
        filter(parm_cd %in% pCode) %>%
        filter(medium_grp_cd %in% "wat") %>%
        filter(count_nu >= 1)   

unique_sites <- sub_sites[!duplicated(sub_sites[c('site_no', 'parm_cd')]),] 

write.csv(unique_sites, paste(out_dir, "EastTaylor_USGS_sites.csv", sep = ""))

sub_sites_updated = filter(sub_sites, parm_cd %in% pCode & end_date > as.Date('2010-01-01'))

write.csv(sub_sites_updated, paste(out_dir, "EastTaylor_USGS_sites_endDate2010.csv", sep = ""))

sub_sites_updated = filter(sub_sites, parm_cd %in% pCode & end_date > as.Date('2020-01-01'))

write.csv(sub_sites_updated, paste(out_dir, "EastTaylor_USGS_sites_endDate2020.csv", sep = ""))

unique_sites_parm <- sub_sites_updated[!duplicated(sub_sites_updated[c('site_no', 'parm_cd')]),] 

table(unique_sites_parm['parm_cd'])

unique_sites <- sub_sites_updated[!duplicated(sub_sites_updated[c('site_no')]),] 

m <- leaflet(data=unique_sites) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(~dec_long_va,~dec_lat_va,
                   color = "green", radius=3, stroke=FALSE,
                   fillOpacity = 0.5, opacity = 0.8,
                   popup=~site_no)

m

saveWidget(m, file=paste(out_dir, "USGS_sites_updated.html", sep = ""))



pCode = '99133'
statCode = '00001'
start.Date <- "2019-02-01"
end.Date <- "2019-03-01"

## use long_lat box to query data from sites
data <- readNWISdata(bBox = box.region, parameterCd = pCode, statCd = statCode, service="qw", 
                     startDate = start.Date, endDate = end.Date , asDateTime = TRUE)

data = renameNWISColumns(data)

head(data)

names(data)

dim(data)

## use long_lat box to query data from database
qwData_data <- readNWISdata(bBox=box.region, parameterCd = pCode, qw_attributes="expanded", service="qw")
# qwData_data_NO3 = filter(qwData_data, parm_cd %in% pCode)

df = data.frame(qwData_data$agency_cd, qwData_data$site_no, qwData_data$startDateTime, 
                             qwData_data$sample_start_time_datum_cd, qwData_data$p00010)
names(df) = c("agency","site_no", "DateTime", "tz", "temperature_C" )

write.csv(df, paste(out_dir, "USGS_temp_data.csv", sep = ""))

media = "Water"
name = "Nitrate" #"Organic carbon", "Nitrate", "Phosphorus", "Nitrogen", "Calcium"
sampleFraction = "Dissolved"
unit = "mg/L"

# parameterCd = "USGS-00600"
startDate <- as.Date("2000-01-01")
# endDate = as.Date('2019-01-01')
endDate = Sys.Date()
# startDate = Sys.Date() - 365*10 # past 10 years
box.region = c(-76.6,38.3,-74.0,42.6) #delaware river basin
# HUCode = c("02040201","02040202", "02040203", "02040204","02040205", 
#             "02040206", "02040207", "02040101","02040102", "02040103", 
#             "02040104","02040105", "02040106") # DRB
HUCode = c("1701", "1702", "1703", "1704", "1705", "1706", "1707", "1708", "1709") # CRB
countrycode = "US"
statecode = "WA"

qwData <- readWQPdata(countrycode = countrycode, characteristicName=name, sampleMedia = media, startDate = startDate, 
                      endDate = endDate)

write.csv(qwData, file = paste0(out_dir, sampleFraction, " ", name,"_data_US.csv"))

qwData <- readWQPdata(huc=HUCode, characteristicName=name, sampleMedia = media, startDate = startDate, 
                      endDate = endDate)

write.csv(qwData, file = paste0(out_dir, sampleFraction, " ", name,"_CRB_00_20.csv"))

qwData <- readWQPdata(statecode = statecode, characteristicName=name, sampleMedia = media, startDate = startDate, 
                      endDate = endDate)

write.csv(qwData, file = "./nitrate_WA.csv")

varInfo = attr(qwData, "variableInfo")
siteInfo <- attr(qwData, "siteInfo")

# dim(siteInfo)

table(qwData$ResultMeasure.MeasureUnitCode)
table(qwData$ResultSampleFractionText)

qwData = qwData %>% mutate(ResultMeasureValue = 
                           ifelse(grepl("ug/l", qwData$ResultMeasure.MeasureUnitCode, ignore.case = T), 
                                                       ResultMeasureValue/1000, ResultMeasureValue))

# qwSummary = read.csv(paste0(out_dir, sampleFraction, " ",name,"_site_summary_80_19.csv"), stringsAsFactors=FALSE)

qwSummary <- qwData %>%
  filter(!is.na(ResultMeasureValue) & !is.na(ActivityStartDateTime)) %>%
  filter(ResultSampleFractionText %in% sampleFraction) %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(count=n(),
            start=min(ActivityStartDateTime, na.rm = TRUE),
            end=max(ActivityStartDateTime, na.rm = TRUE),
            max = max(ResultMeasureValue, na.rm = TRUE),
           mean = mean(ResultMeasureValue, na.rm = TRUE),
            median = median(ResultMeasureValue, na.rm = TRUE),
           min = min(ResultMeasureValue, na.rm = TRUE)) %>%
  filter(count >= 1) %>%
  arrange(-count) %>%
  left_join(siteInfo, by = "MonitoringLocationIdentifier")

write.csv(qwSummary, file = paste0(out_dir, "./nitrate_site_summary_CRB_00_20.csv"))

table(qwSummary$MonitoringLocationTypeName)

qwSummary = mutate(qwSummary, type = ifelse(grepl("Well", qwSummary$MonitoringLocationTypeName, ignore.case = T),"gw", "sw"))

# colors <- c("red", "white", "blue", "white", "blue", "red")
#         labels <- c("filled_square", "empty_square", "big_square", "empty_circle", "filled_circle", "big_circle")
#         sizes <- c(10, 20, 30, 10, 20, 30)
#         shapes <- c("square", "square", "square", "circle", "circle", "circle")
#         borders <- c("red", "blue", "black", "blue", "blue", "black")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.8){

            make_shapes <- function(colors, sizes, borders, shapes) {
                shapes <- gsub("circle", "50%", shapes)
                shapes <- gsub("square", "0%", shapes)
                paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
            }
            make_labels <- function(sizes, labels) {
                paste0("<div style='display: inline-block;height: ", 
                       sizes, "px;margin-top: 4px;line-height: ", 
                       sizes, "px;'>", labels, "</div>")
            }

            legend_colors <- make_shapes(colors, sizes, borders, shapes)
            legend_labels <- make_labels(sizes, labels)

            return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, title = "Site Type"))
        }

colors <- c("white", "darkblue")
        labels <- c("well", "stream/river")
        sizes <- c(10, 10)
        shapes <- c("circle", "circle")
        borders <- c("darkblue","darkblue")

col_types <- c("darkblue","dodgerblue","green4","gold1","orange","brown","red")
leg_vals <- unique(as.numeric(quantile(qwSummary$max, 
                probs=c(0,0.01,0.1,0.25,0.5,0.75,0.9,.99,1), na.rm=TRUE)))
pal = colorBin(col_types, qwSummary$max, bins = leg_vals)
rad <- 3*seq(1,4,length.out = 16)
qwSummary$sizes <- rad[as.numeric(cut(qwSummary$count, breaks=16))]
          
m = leaflet(data=qwSummary) %>% 
  setView(lng = -120, lat = 46, zoom = 7) %>%
  addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(data=delawareGap, weight=2, col= 'green') %>%
#   addPolygons(data=neversink, weight=2, col= 'green') %>%
#   addPolygons(data=frenchCreek, weight=2, col= 'green') %>%
#   addPolygons(data=delaware_river, weight=2, col= 'blue') %>%
#   addPolylines(data=delaware_river_trib, weight=1, col= 'gray') %>%
#   addPolylines(data=DRB_bound, weight=2, col= 'black') %>%
  addPolylines(data=CRB_HU4, weight=1, col= 'gray') %>%
  addPolylines(data=CRB_bound, weight=2, col= 'black') %>%
  addCircleMarkers(~dec_lon_va,~dec_lat_va,
                   fillColor = ~pal(max),radius = ~sizes,fillOpacity = ~ifelse(type == "gw", 0, 0.8), 
                   stroke=T, opacity = 1,color = ~pal(max), weight = 1,
                   popup= paste("station:", qwSummary$station_nm, "<br>",
                                "id:", qwSummary$site_no, "<br>",
                                "type:", qwSummary$MonitoringLocationTypeName, "<br>",
                               "count:", qwSummary$count,  "<br>",
                               "maximum:", qwSummary$max,  "<br>",
                                "median:", qwSummary$median, "<br>",
                                "mean:", qwSummary$mean,  "<br>",
                                "minimum:", qwSummary$min
                               )) %>%
  addLegend(position = 'bottomleft',
            pal=pal,
            values=~max,
            opacity = 0.8,
            labFormat = labelFormat(digits = 1), 
            title = paste0('Max Value (2000~present) <br>', '(', sampleFraction, ' ', name, '-', unit, ')' )) %>%
  addLegendCustom(colors, labels, sizes, shapes, borders)

m

saveWidget(m, file=paste(out_dir, sampleFraction, " ", name,"_CRB_map.html", sep = ""))
mapshot(m, file = paste(out_dir, sampleFraction, " ", name,"_CRB_map.png", sep = ""))

media = "Water"
group = "Stable Isotopes"
name = c("Oxygen Delta 18", "Deuterium/Hydrogen ratio")
# name = "Oxygen Delta 18"
sampleFraction = "Dissolved"
unit = "per mil"

# parameterCd = "USGS-00600"
startDate <- as.Date("1980-01-01")
# endDate = as.Date('2019-01-01')
endDate = Sys.Date()
# startDate = Sys.Date() - 365*10 # past 10 years
box.region = c(-76.6,38.3,-74.0,42.6) #delaware river basin
HUCcode = c("02040201","02040202", "02040203", "02040204","02040205", 
            "02040206", "02040207", "02040101","02040102", "02040103", 
            "02040104","02040105", "02040106") # DRB
# HUCcode = c("020402*", "020401*")
countrycode = "US"

qwData <- readWQPdata(huc=HUCcode, chracteristicType = group, characteristicName=name, sampleMedia = media, startDate = startDate, 
                      endDate = endDate)

write.csv(qwData, file = paste0(out_dir, group, " ", name,"_data_80_19.csv"))

USisotope = read.csv(paste0(out_dir, "US_water_isotope/isotope-data.csv"), 
                     na.strings=c("", "NA"), stringsAsFactors = FALSE)

USisotope = USisotope %>%
  replace_with_na(replace = list(d2H = c(-9999, 9999), d18O = c(-9999, 9999)))

USisotope = USisotope[!(is.na(USisotope["Site_Name"])) & 
                      !(is.na(USisotope["d2H"])) & 
                      !(is.na(USisotope["d18O"])) &
                      !(is.na(USisotope["Collection_Date"])), ]

siteInfo = USisotope[!duplicated(USisotope$Site_Name), ]

qwSummary_2H <- USisotope %>%
  group_by(Site_Name) %>%
  summarise(count=n(),
            start=min(Collection_Date, na.rm = TRUE),
            end=max(Collection_Date, na.rm = TRUE),
            max = max(d2H, na.rm = TRUE),
           mean = mean(d2H, na.rm = TRUE),
           min = min(d2H, na.rm = TRUE)) %>%
  filter(count >= 1) %>%
  arrange(-count) %>%
  left_join(siteInfo, by = "Site_Name")

write.csv(qwSummary_2H, file = paste0(out_dir, "US_water_isotope/isotope-d2H-summary.csv"))

qwSummary_18O <- USisotope %>%
  group_by(Site_Name) %>%
  summarise(count=n(),
            start=min(Collection_Date, na.rm = TRUE),
            end=max(Collection_Date, na.rm = TRUE),
            max = max(d18O, na.rm = TRUE),
           mean = mean(d18O, na.rm = TRUE),
           min = min(d18O, na.rm = TRUE)) %>%
  filter(count >= 1) %>%
  arrange(-count) %>%
  left_join(siteInfo, by = "Site_Name")

write.csv(qwSummary_18O, file = paste0(out_dir, "US_water_isotope/isotope-d18O-summary.csv"))

isoName = "delta 2H"
if (isoName == "delta 2H") {
    qwSummary = qwSummary_2H
} else {
    qwSummary = qwSummary_18O
}
 

# colors <- c("red", "white", "blue", "white", "blue", "red")
#         labels <- c("filled_square", "empty_square", "big_square", "empty_circle", "filled_circle", "big_circle")
#         sizes <- c(10, 20, 30, 10, 20, 30)
#         shapes <- c("square", "square", "square", "circle", "circle", "circle")
#         borders <- c("red", "blue", "black", "blue", "blue", "black")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.8){

            make_shapes <- function(colors, sizes, borders, shapes) {
                shapes <- gsub("circle", "50%", shapes)
                shapes <- gsub("square", "0%", shapes)
                paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
            }
            make_labels <- function(sizes, labels) {
                paste0("<div style='display: inline-block;height: ", 
                       sizes, "px;margin-top: 4px;line-height: ", 
                       sizes, "px;'>", labels, "</div>")
            }

            legend_colors <- make_shapes(colors, sizes, borders, shapes)
            legend_labels <- make_labels(sizes, labels)

            return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, title = "Site Type"))
        }

colors <- c("white", "darkblue")
        labels <- c("ground/tap", "stream/river/precipitation")
        sizes <- c(10, 10)
        shapes <- c("circle", "circle")
        borders <- c("darkblue","darkblue")

col_types <- c("darkblue","dodgerblue","green4","gold1","orange","brown","red")
leg_vals <- unique(as.numeric(quantile(qwSummary$max, 
                probs=c(0,0.01,0.1,0.25,0.5,0.75,0.9,.99,1), na.rm=TRUE)))
pal = colorBin(col_types, qwSummary$max, bins = leg_vals)
rad <- 3*seq(1,4,length.out = 16)
qwSummary$sizes <- rad[as.numeric(cut(qwSummary$count, breaks=16))]

# iconlist = iconList(Msq = , Mcir = )
          
m = leaflet(data=qwSummary) %>% 
  setView(lng = -75.1, lat = 40.8, zoom = 7) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=delawareGap, weight=2, col= 'green') %>%
  addPolygons(data=neversink, weight=2, col= 'green') %>%
  addPolygons(data=frenchCreek, weight=2, col= 'green') %>%
  addPolygons(data=delaware_river, weight=2, col= 'blue') %>%
  addPolylines(data=delaware_river_trib, weight=1, col= 'gray') %>%
  addPolylines(data=DRB_bound, weight=2, col= 'black') %>%
  addCircleMarkers(~Longitude,~Latitude,
                   fillColor = ~pal(max),radius = ~sizes,fillOpacity = ~ifelse(Type %in% c("Ground", "Tap"), 0, 0.8), 
                   stroke=T, opacity = 1,color = ~pal(max), weight = 1,
                   popup= paste("site name:", qwSummary$Site_Name, "<br>",
                                "type:", qwSummary$Type, "<br>",
                               "count:", qwSummary$count,  "<br>",
                                "start date:", qwSummary$start,  "<br>",
                                "end date:", qwSummary$end,  "<br>",
                               "maximum:", qwSummary$max,  "<br>",
                                "mean:", qwSummary$mean,  "<br>",
                                "minimum:", qwSummary$min
                               )) %>%
  addLegend(position = 'bottomleft',
            pal=pal,
            values=~max,
            opacity = 0.8,
            labFormat = labelFormat(digits = 1), 
            title = paste0('Max Value for ', isoName, ' (per mil)',  "<br>" )) %>%
  addLegendCustom(colors, labels, sizes, shapes, borders)

m

saveWidget(m, file=paste(fig_dir, isoName, "_conc_map.html", sep = ""))
mapshot(m, file = paste(fig_dir, isoName, "_conc_map.png", sep = ""))

media = "Water"
name = "Temperature, water" #"Organic carbon", "Nitrate", "Phosphorus", "Nitrogen", "Calcium"
# sampleFraction = "Dissolved"
unit = "deg C"

# parameterCd = "USGS-00600"
startDate <- as.Date("1980-01-01")
# endDate = as.Date('2019-01-01')
endDate = Sys.Date()
# startDate = Sys.Date() - 365*10 # past 10 years
box.region = c(-76.6,38.3,-74.0,42.6) #delaware river basin
HUCcode = c("02040201","02040202", "02040203", "02040204","02040205", 
            "02040206", "02040207", "02040101","02040102", "02040103", 
            "02040104","02040105", "02040106") # DRB
countrycode = "US"

# qwData <- readWQPdata(countrycode = countrycode, characteristicName=name, sampleMedia = media, startDate = startDate, 
#                       endDate = endDate)

# write.csv(qwData, file = paste0(out_dir, sampleFraction, " ", name,"_data_US.csv"))

qwData <- readWQPdata(huc=HUCcode, characteristicName=name, startDate = startDate, 
                      endDate = endDate)

write.csv(qwData, file = paste0(out_dir, name,"_data_80_19.csv"))

varInfo = attr(qwData, "variableInfo")
siteInfo <- attr(qwData, "siteInfo")

# dim(siteInfo)

table(qwData$ResultMeasure.MeasureUnitCode)
table(qwData$ResultSampleFractionText)

qwData = qwData %>% mutate(ResultMeasureValue = 
                           ifelse(grepl("F", qwData$ResultMeasure.MeasureUnitCode, ignore.case = T), 
                                                       (ResultMeasureValue - 32)*5/9, ResultMeasureValue))

# qwSummary = read.csv(paste0(out_dir, sampleFraction, " ",name,"_site_summary_80_19.csv"), stringsAsFactors=FALSE)

qwSummary <- qwData %>%
  filter(!is.na(ResultMeasureValue) & !is.na(ActivityStartDateTime)) %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(count=n(),
            start=min(ActivityStartDateTime, na.rm = TRUE),
            end=max(ActivityStartDateTime, na.rm = TRUE),
            max = max(ResultMeasureValue, na.rm = TRUE),
           mean = mean(ResultMeasureValue, na.rm = TRUE),
            median = median(ResultMeasureValue, na.rm = TRUE),
           min = min(ResultMeasureValue, na.rm = TRUE)) %>%
  filter(count >= 1) %>%
  arrange(-count) %>%
  left_join(siteInfo, by = "MonitoringLocationIdentifier")

write.csv(qwSummary, file = paste0(out_dir, name,"_site_summary_80_19.csv"))

table(qwSummary$MonitoringLocationTypeName)

qwSummary = mutate(qwSummary, type = ifelse(grepl("Well", qwSummary$MonitoringLocationTypeName, ignore.case = T),"gw", "sw"))

colors <- c("white", "darkblue")
        labels <- c("well", "stream/river")
        sizes <- c(10, 10)
        shapes <- c("circle", "circle")
        borders <- c("darkblue","darkblue")

col_types <- c("darkblue","dodgerblue","green4","gold1","orange","brown","red")
leg_vals <- unique(as.numeric(quantile(qwSummary$max, 
                probs=c(0,0.01,0.1,0.25,0.5,0.75,0.9,.99,1), na.rm=TRUE)))
pal = colorBin(col_types, qwSummary$max, bins = leg_vals)
rad <- 3*seq(1,4,length.out = 16)
qwSummary$sizes <- rad[as.numeric(cut(qwSummary$count, breaks=16))]
          
m = leaflet(data=qwSummary) %>% 
  setView(lng = -75.1, lat = 40.8, zoom = 7) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=delawareGap, weight=2, col= 'green') %>%
  addPolygons(data=neversink, weight=2, col= 'green') %>%
  addPolygons(data=frenchCreek, weight=2, col= 'green') %>%
  addPolygons(data=delaware_river, weight=2, col= 'blue') %>%
  addPolylines(data=delaware_river_trib, weight=1, col= 'gray') %>%
  addPolylines(data=DRB_bound, weight=2, col= 'black') %>%
  addCircleMarkers(~dec_lon_va,~dec_lat_va,
                   fillColor = ~pal(max),radius = ~sizes,fillOpacity = ~ifelse(type == "gw", 0, 0.8), 
                   stroke=T, opacity = 1,color = ~pal(max), weight = 1,
                   popup= paste("station:", qwSummary$station_nm, "<br>",
                                "id:", qwSummary$site_no, "<br>",
                                "type:", qwSummary$MonitoringLocationTypeName, "<br>",
                                "start date:", qwSummary$start,  "<br>",
                                "end date:", qwSummary$end,  "<br>",
                               "count:", qwSummary$count,  "<br>",
                               "maximum:", qwSummary$max,  "<br>",
                                "mean:", qwSummary$mean,  "<br>",
                                "median:", qwSummary$median,  "<br>",
                                "minimum:", qwSummary$min
                               )) %>%
  addLegend(position = 'bottomleft',
            pal=pal,
            values=~max,
            opacity = 0.8,
            labFormat = labelFormat(digits = 1), 
            title = paste0('Max Value (1980~present) <br>', '(', name, '-', unit, ')' )) %>%
  addLegendCustom(colors, labels, sizes, shapes, borders)

m

saveWidget(m, file=paste(fig_dir,  name,"_map.html", sep = ""))
mapshot(m, file = paste(fig_dir,  name,"_map.png", sep = ""))

site_id = "31DELRBC-091017"
siteData = qwData[qwData$MonitoringLocationIdentifier == site_id, ]

names(siteData)

siteData

plot(siteData$ActivityStartDateTime, siteData$ResultMeasureValue) 

media = "Water"
group = "Stable Isotopes"
# name = "Organic carbon"
name = "Nitrate"
sampleFraction = "Dissolved"
unit = "mg/L"

# siteID = c("USGS-01434025", "USGS-01434021", "USGS-0143400680")
# siteID = c("USGS-12510500")
siteID = c("USGS-12505450")

# parameterCd = "00600"
# paraCd = c("00060", "00065")
# paraCd = "00681" #DOC
paraCd = "00618"
startDate <- as.Date("2000-01-01")
# endDate = as.Date('2019-01-01')
endDate = Sys.Date()

qwData = readWQPqw(siteNumbers = siteID, parameterCd = paraCd, startDate = startDate, endDate = endDate)

write.csv(qwData, file = "./USGS-12505450_NO3.csv")

colnames(qwData)

head(qwData)

start.time = as.Date('2005-01-01')
end.time = as.Date('2006-01-01')

qwData.copy = filter(qwData, as.numeric(substr(ActivityStartDateTime,1,4)) ==2005)

str(qwData)

plot(qwData$ActivityStartDateTime, qwData$ResultMeasureValue, xlim = c(start.time, end.time))

qwData <- readWQPdata(siteid = siteID, characteristicName=name, sampleMedia = media, startDate = startDate, 
                      endDate = endDate)

startDate = as.POSIXct("1980-01-01")
# endDate = as.Date('2019-01-01')
endDate = as.POSIXct("2020-01-01")

precip_data = read.csv(paste0(out_dir, "precipitation.csv"), 
                     na.strings=c("", "NA", "unknown"), stringsAsFactors = FALSE)

precip_data = precip_data %>%
  replace_with_na(replace = list(HPCP = c(999.99)))

precip_data$DATE = as.POSIXct(precip_data$DATE, format = "%Y%m%d %H:%M", tz = "UTC")

table(precip_data$STATION)

storm_df = data.frame("name"=c("Sandy", "Irene", "Ivan"), 
                      "begin_date"= c(as.POSIXct("2012-10-22"), as.POSIXct("2011-08-26"), as.POSIXct("2004-09-17")),
                     "end_date"= c(as.POSIXct("2012-11-2"), as.POSIXct("2011-08-28"), as.POSIXct("2004-09-19")))

y_offset = c(0.1, 0.1, 0.1)
x_offset = c(30000000, -30000000, 0)

siteNumbers = unique(precip_data$STATION)

siteNumber = siteNumbers[1]

df = precip_data %>%
        filter(STATION %in% siteNumber)

        titlename = paste(df$STATION, df$STATION_NAME)

ggplot(df, aes(DATE, HPCP)) +  geom_col(color = "blue")+ 
                xlim(startDate, endDate) + xlab("Date") +
                ggtitle(titlename) + ylab("Precipitation (in hundredths of inches)") +
                annotate("rect", xmin = storm_df$begin_date,
                      xmax = storm_df$end_date, ymin = min(df$HPCP, na.rm = TRUE), ymax = max(df$HPCP, na.rm = TRUE),  
                      fill = "red", alpha=0.5, color = "red", size = 0.5) + 
                annotate(geom = "text", x = storm_df$begin_date + x_offset, y = max(df$HPCP, na.rm = TRUE) + y_offset, 
                         label = storm_df$name, 
                         color = "red")

for (siteNumber in siteNumbers){
    print(siteNumber)
    
    df = precip_data %>%
        filter(STATION %in% siteNumber)

        titlename = paste(unique(df$STATION), unique(df$STATION_NAME))
        p = ggplot(df, aes(DATE, HPCP)) +  geom_col(color = "blue") + 
                xlim(startDate, endDate) + xlab("Date") +
                ggtitle(titlename) + ylab("Precipitation (in hundredths of inches)") +
                annotate("rect", xmin = storm_df$begin_date,
                      xmax = storm_df$end_date, ymin = min(df$HPCP, na.rm = TRUE), ymax = max(df$HPCP, na.rm = TRUE),  
                      fill = "red", alpha=0.5, color = "red", size = 0.5) + 
                annotate(geom = "text", x = storm_df$begin_date + x_offset, y = max(df$HPCP, na.rm = TRUE) + y_offset, 
                         label = storm_df$name, 
                         color = "red")
        
        ggsave(paste0(fig_dir,titlename,".png"), 
           width = 8, height = 5, units = "in")

    
}

