source("utils.R")
source("generic_functions/ez_setup.R")
ez_setup(install=TRUE, extras=c("RSocrata","stringr","knitr","kableExtra"))

#service requests ----
#TODO: figure out non-local way of sourcing the 311 data.

sr <- data.table::fread("https://data.cityofchicago.org/api/views/dm4w-zcp5/rows.csv?accessType=DOWNLOAD")
colnames(sr) <- tolower(colnames(sr))
colnames(sr) <- gsub("community_area", "area_id", colnames(sr))
sub <- select(sr, sr_number, sr_type, status, ends_with("_date"))
sub[,4:5] <- lapply(sub[,4:5], mdy_hms)

#generating some summaries of this enormous dataset, dropping cols that are not
# usefully described by their `unique()` values
summaries <- lapply(select(sr, -sr_number, -street_address, -ends_with("date"),
                           -ends_with("number"), -c(x_coordinate:location)), unique)
#types of service requests and some sub-types
sr_types <- summaries[["sr_type"]] #these are the "categories" for 1 above
complaints <- sr_types[grep("Complaint",sr_types)]
requests <- sr_types[grep("Request", sr_types)]
violations <- sr_types[grep("Violation", sr_types)]
other <- sr_types[!sr_types%in%complaints&
                  !sr_types%in%requests&
                  !sr_types%in%violations]
od <- summaries[["owner_department"]]

# SR_NUMBER is a unique identifier for each entry:
#unique_id <- names(summaries)[lapply(summaries, length)==nrow(sr)]
# sanitation division is closed on the weekend?
#summaries[["sanitation_division_days"]]

#1. What category of service request is addressed the fastest? The slowest? ----
#subset to just completed requests
#subset to just completed requests
completed <- filter(sub, status=="Completed") %>% select(-status) %>% 
  mutate(dur_sec = as.numeric(difftime(.$closed_date,
                                       .$created_date))
  ) %>% 
  group_by(sr_type)
#summarize the df of completed requests
dur_sum <- summarise(completed, 
                     day_diff=difftime(closed_date,created_date,units="days")%>%
                       mean() %>% round(digits=8)) %>% filter(day_diff>0.5)
#find some specific values to print the results message
min_idx <- which(dur_sum$day_diff==min(dur_sum$day_diff))
min_time <- as.numeric(dur_sum$day_diff[min_idx])
min_type <- dur_sum$sr_type[min_idx]
min_dur <- nice_dur(min_time)

max_idx <- which(dur_sum$day_diff==max(dur_sum$day_diff))
max_time <- as.numeric(dur_sum$day_diff[max_idx])
max_type <- dur_sum$sr_type[max_idx]
max_dur <- nice_dur(max_time)

dur_sum$day_diff <- as.numeric(round(dur_sum$day_diff, 2))

# message("Fastest request type fulfilled: ", 
#         summaries$sr_type[[which(summaries$sr_short_code==min_code)]],
#         "\n>> ","Average life of service request: ", 
#         min_dur[[1]]," ",min_dur[[2]],"\n",
#         "Slowest request type fulfilled: ", 
#         summaries$sr_type[[which(summaries$sr_short_code==max_code)]],
#         "\n>> ","Average life of service request: ", 
#         max_dur[[1]]," ",max_dur[[2]])


#2. Do categories of service requests seem to be related to time of year? ----
ty <- mutate(sub, month_created=lubridate::month(sub$created_date,label=TRUE)) %>% 
  filter((month(created_date)>=month(Sys.Date()) & 
            day(created_date)>day(Sys.Date()) & 
            lubridate::year(created_date)==lubridate::year(Sys.Date())-1
  )|
    lubridate::year(created_date)==lubridate::year(Sys.Date())
  ) %>% 
  group_by(sr_type, month_created) %>% summarize(total_req=n()) %>% 
  spread(month_created, total_req)
ty[is.na(ty)] <- 0
ty_sum <- mutate(ungroup(ty), 
                 max = do.call(pmax, c(ty[,-1], na.rm=TRUE)),
                 min = do.call(pmin, c(ty[,-1], na.rm=TRUE))) %>%
  mutate(.,cv = apply(ty[,c(2:13)], 1,function(x){
    (sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE))*100}))
#high variance codes with a maximum request count of at least 10
ty_hvc <- filter(ty_sum, cv > 100 & max > 10)

to_plot <- select(ty_hvc, sr_type,Jan:Dec) %>% 
  gather(key="month", value="count", -sr_type)
to_plot$month <- factor(to_plot$month, month.abb)

ty_ggplot <- ggplot(to_plot, aes(x=month, y=count, group=1)) +
  geom_line() +
  facet_wrap(~str_wrap(sr_type,width=19), scales="free_y") +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 270, hjust = 1))
today_day <- day(Sys.Date())
today_month <- as.character(lubridate::month(Sys.Date(), label=TRUE,abbr=FALSE))
today_year <- lubridate::year(Sys.Date())

# Do categories of service requests seem to be related to time of day?
td <- mutate(sub, time_exact=make_datetime(hour=hour(sub$created_date),
                                           min=minute(sub$created_date),
                                           sec=second(sub$created_date)) %>% 
               format(format="%H:%M:%S"),
             time_hour=hour(sub$created_date)) %>% 
  group_by(sr_type, time_hour) %>% summarize(total_req=n()) %>% 
  spread(time_hour, total_req)
td[is.na(td)] <- 0
td_sum <- mutate(ungroup(td), 
                 max = do.call(pmax, c(td[,-1], na.rm=TRUE))) %>% 
  mutate(., cv = apply(td[,c(2:25)], 1,function(x){ 
    (sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE))*100} ))
td_hvc <- filter(td_sum, cv > 100 & max > 20)

to_plot <- select(td_hvc, sr_type, `0`:`23`) %>% 
  gather(key="hour", value="count", -sr_type)
to_plot$hour <- as.numeric(to_plot$hour)

td_ggplot <- ggplot(to_plot, aes(x=hour, y=count, group=1)) +
  geom_line() +
  scale_x_continuous(breaks=unique(to_plot$hour)[unique(to_plot$hour)%%2==0], 
                     labels=c("12am", paste0(c(2,4,6,8,10),"am"), "12pm", paste0(c(2,4,6,8,10),"pm"))) +
  facet_wrap(~str_wrap(sr_type, width=19), scales="free_y") +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x = element_text(size=7, angle = 270, hjust=-0.25, vjust = 0.5))
#3. Which departments are best/worst at closing requests quickly? ----
dep_sp <- select(sr, owner_department, sr_type, 
                 status, created_date, closed_date) %>% 
  filter(status=="Completed") %>% select(-status)
dep_sp[,3:4] <- lapply(dep_sp[,3:4], mdy_hms)
dep_sp <- mutate(dep_sp, dur_sec = as.numeric(difftime(dep_sp$closed_date,
                                                       dep_sp$created_date))
) %>% group_by(owner_department, sr_type) %>% 
  summarise(day_diff = difftime(closed_date,created_date, units="days") %>% 
              mean() %>% round(digits=8), count=n()) %>% filter(day_diff>0.5)
avg_speed <- summarize(dep_sp, avg_req_n_days=mean(day_diff) %>% round (2))
avg_speed$owner_department <- gsub(".*?- ","", avg_speed$owner_department)
avg_speed$owner_department <- ifelse(grepl("^Department of ",avg_speed$owner_department),
                                     avg_speed$owner_department,
                                     gsub("^","Department of ",avg_speed$owner_department))
min_dep <- avg_speed$owner_department[which(avg_speed$avg_req_n_days==
                                              min(avg_speed$avg_req_n_days))]
min_dep_time <- as.numeric(avg_speed$avg_req_n_days[which(
  avg_speed$avg_req_n_days==min(avg_speed$avg_req_n_days))])
max_dep <-  avg_speed$owner_department[which(avg_speed$avg_req_n_days==
                                               max(avg_speed$avg_req_n_days))]
max_dep_time <- as.numeric(avg_speed$avg_req_n_days[which(
  avg_speed$avg_req_n_days==max(avg_speed$avg_req_n_days))])

#4. What kinds of service requests might be related to police presence? ----

#posibly police presence
ppp <- c("Graffiti Removal Request", 
         "Operating Without Business License Complaint", 
         "City Vehicle Sticker Violation", "Tobacco - Sale to Minors Complaint")

#5. Which areas of budget changes are most/least popular? ----
budget <- read.socrata("https://data.cityofchicago.org/api/views/drbg-ny73/rows.csv?accessType=DOWNLOAD", 
                       "ykQDb7IDvb4jrHDEx6hk7f8uF")

gbudget <- gather(budget, cat, value, -c(date, zip_code, counter_column)) %>% 
  mutate(cat=gsub("(?<=assignment)_","-",cat, perl = TRUE)) %>% 
  mutate(cat=gsub("(?<=change)_","-",cat, perl = TRUE)) %>% 
  mutate(cat=gsub("(?<=interest)_","-",cat, perl = TRUE)) %>% 
  separate(cat, c("category", "type"), sep="-") %>% 
  select(-counter_column) %>% mutate(id=1:nrow(.))
gbudget$value <- gsub("Increase.*","increase",gbudget$value) %>% 
  gsub(".*Same","no change",.) %>% gsub("Reduce.*","reduce",.) %>% 
  gsub("No Opinion","no opinion",.) %>% tolower()
gbudget$value[gbudget$value==""] <- NA
gbudget$value[is.na(gbudget$value)] <- "no answer"
gb <- filter(gbudget, category!="X_1000_assignment")
gb_sum <- summarize(group_by(gb, category, type, value), count=n()) %>% 
  filter(value!="no answer")
gb_sum$category <- gsub("_interest","_int",gb_sum$category) %>% 
  gsub("_change","_cha", .) %>% tolower()
for (gbcat in gb_sum$category) {
  plot <- filter(gb_sum, category==gbcat)
  if (grepl("_int$",gbcat)) {
    plot$value <- factor(plot$value, levels=c("true","false"))
  }else if (grepl("revenue",gbcat)){
    plot$value <- factor(plot$value, levels=c("increase","no change", "no opinion"))
  }else if (grepl("spending",gbcat)){
    plot$value <- factor(plot$value, levels=c("increase","no change", "reduce"))
  }
  plot <- ggplot(plot, aes(str_wrap(str_to_title(gsub("_"," ",type)),
                                    width=16), count, fill=value)) +
    geom_col(position="dodge") +
    #scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) +
    labs(title=element_blank()) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.title=element_blank(),
          axis.text.x = element_text(angle=270, hjust=, vjust=0.5))
  assign(gbcat, plot)
}
print(revenue_int + labs(title="Respondent reported interest in information on a source of revenue"))
print(expense_int + labs(title="Respondent reported interest in information on a type of expenditure"))
revenue_cha + labs(title=str_wrap("Which revenues would you consider increasing or keeping the same in order to balance the City’s budget?", width=60))
spending_cha + labs(title=str_wrap("Which areas of spending would you consider reducing, increasing or remaining the same in order to balance the City’s budget?", width=70))

#Answer: Seems like people most want to reduce spending on City Dev and 
#        Cultural Affairs/Special Events, and increase spending in 
#        Infrastructure and "Community". In all other categories the majority
#        preferred keeping spending the same.
#6. Do any kinds of budget changes have a theoretical relationship? 
#   For plausible relationships, are survey respondents internally consistent?
#   (e.g. streets and sanitation spending and infrastructure spending should go 
#   together, spending for libraries and cultural affairs might work with or 
#   against a tax on sports/theater/entertainment) 
#
#Based on `spending_cha` above, it seems like respondents don't view spending
# on Streets and Sanitation as being as deserving of funding as "Infrastructure"
# despite a plausible relationship between the two. 

# trying to compare opinions on tax rates w/ opinions on spending:
# e.g. people tend to want to increase spending on infrastructure, so it makes 
# sense then that I see a slight preference for increasing ridesharing taxes
# people who drive, care about infrastructure and don't care about ridesharing
# also I think a strong desire to reduce city development kind of goes along 
# with an overwhelming desire not to increase property taxes

#making a visualization of the $1000 assignment survey
assignment <- filter(gbudget, category=="X_1000_assignment") %>% 
  select(-category, -date, -zip_code, -id) %>% group_by(type) %>% 
  filter(type!="total")
assignment[assignment=="no answer"] <- NA
assignment <- filter(assignment, !is.na(value))
assignment$value <- as.numeric(assignment$value)
ggplot(assignment, aes(reorder(str_wrap(str_to_title(gsub("_"," ",type)), 
                                        width=17), value, median), value)) +
  geom_boxplot() +
  coord_flip() +
  theme_classic() +
  labs(title="Amount of a hypothetical $1000 allocated to each area.") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = function(x){
    sub("^","$",ifelse(str_length(x)==4,sub("(^\\d)","\\1,",x),x))})


# Cleaning geographic and demographic input data ----
#initial read
com_area <- read.csv("data_files/CommAreas.csv", stringsAsFactors=FALSE)
nb <- read.csv("data_files/Census_Data.csv", stringsAsFactors=FALSE)
zips <- read.csv("data_files/Zip_Codes.csv", stringsAsFactors=FALSE)

#standardizing key-column names for join below
colnames(com_area) <- gsub("AREA_NUM_1","area_id", colnames(com_area))
colnames(nb) <- gsub("Community\\.Area\\.Number","area_id", colnames(nb))
colnames(zips) <- gsub("OBJECTID","area_id", colnames(zips))

#making final demo data object and dropping constituent objects
demo <- left_join(filter(nb, !is.na(area_id)), 
                  select(zips, area_id, ZIP), by="area_id") %>% 
        left_join(select(com_area, area_id, the_geom), by="area_id");rm(com_area,nb,zips)
colnames(demo) <- tolower(colnames(demo))
demo <- select(demo, area_id, community.area.name, zip, everything())
# What neighborhoods have the highest and lowest number of service requests?
# note: given the data set, this is the highest and lowest COMPLETED requests
# would be interesting to look into TOTAL or even CANCELLED/INCOMPLETE
sr_zip <- left_join(sr, select(demo, area_id, zip), by="area_id")

nbh_sum <- group_by(sr_zip, area_id) %>% summarise(count=n()) %>% 
  left_join(select(demo, area_id, community.area.name),.,by="area_id")

highest <- filter(nbh_sum, count==max(nbh_sum$count))$community.area.name
lowest <- filter(nbh_sum, count==min(nbh_sum$count))$community.area.name

# Is there anything notable about the SES (or related) demographics of those
#   neighborhoods?
demo_high <- filter(demo, community.area.name==highest) %>% select(-the_geom)
demo_low <- filter(demo, community.area.name==lowest) %>% select(-the_geom)

demo_high #- Demographics indicate that this area is has a more middle-class population
          #  with levels of poverty and unemployment being much lower than the other area.
          #  Total conjecture: High completion rate of service requests could be related to
          #  type of request (maybe the requests here are easier?) and impression of safety
          #  in the neighborhood.
demo_low
#so lets look at counts per neighborhood per service type for these two nbhs
nbh_type_sum <- group_by(sr_zip, area_id, sr_type) %>% summarise(count=n()) %>% 
  left_join(select(demo, area_id, community.area.name),.,by="area_id") %>% 
  filter(community.area.name%in%c(highest,lowest, "O'Hare"))

nbh_plot_table <- select(nbh_type_sum, -area_id) %>% spread(community.area.name, count)
nbh_plot_table[is.na(nbh_plot_table)] <- 0
colnames(nbh_plot_table) <- gsub("(\\s+|[[:punct:]])","_",colnames(nbh_plot_table)) %>% tolower()
#nbh_plot_filter <- filter(nbh_plot_table, count<1200)

nbh_diff <- mutate(nbh_plot_table, diff=abs(near_west_side-riverdale))
#looks like NWS has drastically more requests than RD in every regard, but the
# VAST majority of SRT for NWS is the 311 information only request. This SRT
# seems like it would be one of the easiest to complete and since it accounts for
# such a huge number of NWS's SRs it makes sense that in a count of completed requests
# this would put NWS at such a high ranking. Potential reason for high 311 info
# requests: Union Station, Greyhound Bus station, Illinois Medical District. 
# The neighborhood with the next most requests is O'Hare who's biggest SRT by 
# far is aircraft noise complaint at ~240,000 with the next most common being 
# "cab feedback" at ~640! It does seem though that both of these SRTs are 
# related to the airport.


ggplot(filter(nbh_plot_filter, sr_type!="311 INFORMATION ONLY CALL"), 
                   aes(sr_type, count, fill=community.area.name)) +
            geom_col(position="dodge") +
#            scale_y_log10() +
            theme(legend.title=element_blank(),
            axis.text.x = element_text(angle=270, hjust=, vjust=0.5))
            
# Do some neighborhoods typically get service requests addressed more quickly
#   than others?
# Is there anything notable about the demographics?
# What kinds of service requests are most common in high-crime areas?
# Are there any associations between some kinds of crimes and some kinds of
#   service requests?
#   (e.g. vandalism and property damage might be related to graffiti removal
#   and street light repair service requests)
# Are differences in budget survey responses by zip code?
#   What if you divide zip codes into low, middle, and high SES?
# Is there a theoretical relationship between the categories of 311 Service
#   Requests and the spending/revenue categories included in the budget survey?
#   e.g. fixing potholes relate to streets & sanitation spending
#   (“owner_department” seems like a relevant variable) - For any plausible
#   relationships you come up with, does it seem like there is regional
#   consistency? e.g. In areas where many potholes are reported, residents
#   support higher spending on streets & sanitation.
