
### ---- PROCESS raw data ---- ####

##### check tomst data files and get a list of tomst file and path #####


##load meta
meta_treat <- read.csv(paste(path_dat_clean, 'meta_treat.csv', sep = '/'))
str(meta_treat)
#testing commit


###select country

#get list of countries
tomst_data_country <- dir(path_tomst_raw)
#select from which country the data should be processed
tomst_data_country
c = 3 #<<<<<<<<<<<<<<<<<<<<<<<
(country <- tomst_data_country[c])

if(c!= 3){
(couNtry <- str_split(country, '_')[[1]][1])
} else{
  (couNtry <- paste(str_split(country, '_')[[1]][1],
                    str_split(country, '_')[[1]][2],
                    sep = '_'))
}

##
path_to_files <- paste(path_tomst_raw, country, sep = '/')

##load tomst meta file
meta_tomst <- fread(paste(path_tomst_raw, 'tomst_meta.csv', sep = '/'))

#filter selected country
meta_tomst <- meta_tomst %>%
  filter(country == couNtry)

##run function to get list of tomst files
tomst_ids <- get_file_list_tomst(path_to_files)

###check for duplicated names in the data files
if(nrow(tomst_ids) != length(unique(tomst_ids$tomst_id))){
  stop('Duplicated tomst id numbers')
} else {
  print('Duplication check passed. Continue.')
}

#check for duplicated names in the meta file
if(nrow(meta_tomst) != length(unique(meta_tomst$tomst_id))){
  stop('Duplicated tomst id numbers')
} else {
  print('Duplication check passed. Continue.')
}

#check length of unique file names in the meta vs. the data files
#calanda has one tomst logger less
if(length(unique(tomst_ids$tomst_id)) != length(unique(meta_tomst$tomst_id))){
  stop('Number of unique tomst files do not match number of unique files in the meta')
} else {
  print('Check passed. Continue.')
}

##filter out data from lower elevation and from bare plots
if(couNtry == 'norway'){
  meta_tomst <- meta_tomst %>% filter(site_ele == 'high')
}

##ctreate bock_plot id
if(unique(meta_tomst$country) == 'norway'){
  meta_tomst <- meta_tomst %>% 
    mutate(block_plot = paste('p', block, treat, sep = ''))
} else{
  meta_tomst <- meta_tomst %>% 
    mutate(block_plot = (paste(paste('p', block, sep = ''), treat, sep = '.')))
}

##filter selected country for treatments
meta_treat_country <- meta_treat %>% filter(country == couNtry)

##join tomst data with treatments
meta_tomst_sel <- meta_tomst %>% 
  select(block_plot, tomst_id) %>% #select important columns
  mutate(tomst_id = as.character(tomst_id)) %>%
  inner_join(., meta_treat_country, 'block_plot')

##add file names and path
meta_tomst_sel <- meta_tomst_sel %>% left_join(., tomst_ids, 'tomst_id')

#get vector of tomst data files to work with
f_path <- meta_tomst_sel$f_path

####-------------------------------#

##### extract and process tomst data ####

##run function to extract and process tomst data
tomst_data <- read_tomst_data(f_path, 2)

#add metadata
meta_tomst <- fread(paste(path_tomst_raw, 'tomst_meta.csv', sep = '/')) %>%
  select(tomst_id, country, block, treat, date_out)

df <- inner_join(tomst_data, meta_tomst,  'tomst_id')

#create block_plot
if(couNtry == 'norway'){
  df <- df %>%
    mutate(block_plot = paste('p', block, treat, sep = ''))
} else{
  df <- df %>%
    mutate(block_plot = paste('p', block, '.',  treat, sep = '')) 
}

## how many time-zones are recorded in the data
unique(df$zone)


##show timing of tomst data
time_range <- data.frame()
(block_plot_unique <- unique(df$block_plot))

for(a in 1:length(block_plot_unique)){
  
  #a = 1
  
  df_new <- df %>% filter(block_plot == block_plot_unique[a])
  
  range(df_new$datetime)
  
  time_range[a,1] <- block_plot_unique[a]
  time_range[a,2] <- min(df_new$date_out)
  time_range[a,3] <- min(df_new$datetime)
  time_range[a,4] <- max(df_new$datetime)
  
}

colnames(time_range) <- c('block_plot', 'date_out','time_start', 'time_end')
time_range

max(time_range$time_start)
min(time_range$time_end)

####select specific time

if(couNtry == 'switzerland'){
  cut_off_dat <- dmy('01/05/23')
  
  df_filtered <- df %>% filter(datetime > cut_off_dat)
}

if(couNtry == 'norway'){
  
  #reduce the time frame to selected dates
  end_date <- dmy(c('25-10-23')) # all tomst data will fit to this end time
  
 #
  df_filtered <- tibble() #create empty data frame
  (block_plot_unique <- unique(df$block_plot))
  
  for(a in 1:length(block_plot_unique)){
    
    #a = 1
    
    df_new <- df %>% filter(block_plot == block_plot_unique[a])
    
    date_installed <- dmy(unique(df_new$date_out)) + days(1)
    
    df_new <- df_new %>% filter(datetime > date_installed)
    
    df_new <- df_new %>% filter(datetime < end_date)
    
    df_filtered <- rbind.data.frame(df_filtered, df_new)
    
  }
}


if(couNtry == 'south_africa'){
  start_dat <- dmy('01/09/23')
  end_dat <- dmy('01/06/24')
  
  df_filtered <- df %>% filter(datetime > start_dat) |>
    filter(datetime < end_dat)
  
}

range(df_filtered$datetime)


###
write.csv(df_filtered, paste(path_dat_clean,
                             paste(paste('tomst', couNtry, sep='_'),
                                   '_clean_all.csv', sep = ''),
                             sep = '/'), row.names = FALSE)

###



##### REMOVE days with measurements #####

####load processed fluxes
###fluxes
fluxee <- fread(paste(path_dat_clean, 'fluxfiles_light_response_processed.csv', sep = '/'),)

couNtry
fluxee <- fluxee %>% filter(country == couNtry)

##
head(df_filtered)

#add date only column
df_filtered <- df_filtered %>%
  mutate(daTe = as.Date(df_filtered$datetime))

#
blOcks <- unique(df_filtered$block_plot)

for(a in 1:length(blOcks)){

  #a = 1
  
  ##split data into data containing the target block_plot and data without it
  
  df_filtered_noBlock <- df_filtered %>% filter(block_plot != blOcks[a])
  
  df_filtered_Block <- df_filtered %>% filter(block_plot == blOcks[a])
  
  ##get measurement dates
  fluxee_Block <- fluxee %>% filter(block_plot == blOcks[a])
  unique(fluxee_Block$block_plot)
  measurement.dates <- unique(fluxee_Block$daTe)
  
  ##for each measurement day, add the next day too
  #this is a secure measure to eliminate the situation when the warming chamber was not put on the place, 
  #or when the warming effect on the soil was not established in the next day after the chamber was not on the place
  measurement.dates_new <- vector()
  for(i in seq(length(measurement.dates), from = 1)){
     #i = 1
     measurement.dates_new <- c(as.character(measurement.dates_new),
                                c(as.character(measurement.dates[i]), 
                                  as.character(ymd(measurement.dates[i])+days(x = 1)))
     )
  }
  
  ##remove measurement dates from tomst data
  for(dt in 1:length(measurement.dates)){
    
    #dt = 1
    
    df_filtered_Block <- df_filtered_Block %>% filter(daTe != ymd(measurement.dates[dt]))
    #df_filtered_Block %>% filter(daTe == ymd(measurement.dates[dt]))
    
  }
  
  ##merge data back
  df_filtered <- rbind.data.frame(df_filtered_noBlock, df_filtered_Block )
  
}

#
write.csv(df_filtered, paste(path_dat_clean,
                        paste(paste('tomst', couNtry, sep='_'),
                              '_clean.csv', sep = ''),
                        sep = '/'), row.names = FALSE)


### GET measurement dates ####

head(fluxee)

df_fluxee <- fluxee %>%
  mutate(measurement = paste(daTe, block_plot, country, site, sep = '_'))

measurement_dates <- unique(df_fluxee$measurement)

df_measurement_dates <- data.frame(date = NA, block_plot = NA, country = NA, site = NA)
df_measurement_dates <- df_measurement_dates[-1, ]

for(rw in 1 : length(measurement_dates)){
  #rw = 1
  
  measurement_dates_split <- str_split(measurement_dates[rw], '_')
  
  df_measurement_dates[rw, 1] <- measurement_dates_split[[1]][1]
  df_measurement_dates[rw, 2] <- measurement_dates_split[[1]][2]
  df_measurement_dates[rw, 3] <- measurement_dates_split[[1]][3]
  df_measurement_dates[rw, 4] <- measurement_dates_split[[1]][4]
  
}

df_measurement_dates

write.csv(df_measurement_dates, paste(path_dat_clean,
                             paste(paste('tomst', couNtry, sep='_'),
                                   '_measurement_dates.csv', sep = ''),
                             sep = '/'), row.names = FALSE)

#


### DAILY MEANS ####

##load data

#meta
meta_treat <- read.csv(paste(path_dat_clean, 'meta_treat.csv', sep = '/'))
#meta_treat <- meta_treat |> mutate(block_plot = paste(block_plot, country, sep = '_'))

#fluxes
couNtry <- c('switzerland',  'norway', 'south_africa')


### combine tomst raw data

df_tomst <- tibble()

for(i in 1:3){
  
  #i = 1
  
  df_tomst_country <- fread(paste(path_dat_clean,
                             paste(paste('tomst', couNtry[i], sep='_'),
                                   '_clean.csv', sep = ''),
                             sep = '/'))
  
  df_tomst <- rbind.data.frame(df_tomst, df_tomst_country)
}



### summary of dates

df_summary <- df_tomst %>% mutate(block_plot = as.factor(block_plot)) %>%
  left_join(., meta_treat) %>%
  group_by(country, block, block_plot, treat_veg, treat_temp) %>%
  summarize(start = min(datetime),
            end = max(datetime))

#write.csv(x = df_summary, file = paste(path_dat_clean, 'tomst_dates.csv', sep = '/'))


### filter tomst data based on dates
#unify the start and end times across all plots for each country
#some plots will not have first month measurements
#as some tomst loggers were installed towards the end of the month


df_filtered <- tibble()

for(i in 1:nrow(df_summary)){
  
  #i = 1
  
  df_summ_plot <- df_summary[i, ] 
  
  df_tomst_single_plot <- left_join(df_summ_plot, df_tomst)
  
  if(df_summ_plot$country == 'norway'){
    
    if(df_summ_plot$start < ymd('23-06-21')){
      df_filt <- df_tomst_single_plot
    }else{
      df_filt <- df_tomst_single_plot %>%
        filter(daTe > ymd('23-06-30'))
    }
 
  }
  
  if(df_summ_plot$country == 'switzerland'){
    
    df_filt <- df_tomst_single_plot %>%
      filter(daTe < ymd('23-10-10'))
  }
  
  if(df_summ_plot$country == 'south_africa'){
    
    df_filt <- df_tomst_single_plot
  }
  
  df_filtered <- rbind.data.frame(df_filtered, df_filt)
  
}

#check start and end dates of the measurements
df_summary_filt <- df_filtered %>%
  group_by(country, block, block_plot, treat_veg, treat_temp) %>%
  summarize(start = min(datetime),
            end = max(datetime)) %>%
  ungroup()
df_summary_filt

### calculate stats per day and plot

str(df_filtered)

df_filtered <- data.frame(df_filtered)

df_filtered <- df_filtered |>
  mutate(block_plot_country = paste(block_plot, country, sep = '_'))

(block_plot_unique <- unique(df_filtered$block_plot_country))

df_all <- data.frame()
for(a in 1 : length(block_plot_unique)){
  
  #a = 1
  
  df_selected <- df_filtered %>% 
    #filter(country == 'south_africa') |>
    filter(block_plot_country == block_plot_unique[a])
  
  #str(df_selected)
  
  ## get year, month and day
  df_selected <- df_selected %>%
    mutate(year = year(datetime),
           month = as.factor(month(datetime)),
           day = day(datetime)) %>%
    mutate(daTe = lubridate::ymd(paste(year, month, day, sep ='_')))
  #str(df_selected)

  #create df with months which are in the dataset
  df_month <- data.frame(month_number = as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)),
             month_name = c('Jan', 'Feb', 'Mar', 'Apr',
                            'May','Jun', 'Jul', 'Aug', 'Sep', 'Oct',
                            'Nov', 'Dec'))
  
  month_lev <- data.frame(month_number = as.character(levels(df_selected$month)))
  
  month_names <- left_join(month_lev, df_month) %>% select(month_name)
  
  #change month numbers to month names
  levels(df_selected$month) <- month_names$month_name
  
  ## check weird numbers
  
  min(df_selected$T1)
  
  
  ##calculate stats for each day
  
  month_unique <- unique(df_selected$month)
  
  df_stats_daily_all <- data.frame()
  
  for(mm in 1:length(month_unique)) {
    
    #mm = 1
    
    #select monthly data
    df_selected_month <- df_selected %>% filter(month == month_unique[mm])
    
    #
    daTe_unique <- unique(df_selected_month$daTe)
    
    ####calculate daily stats data
    
    df_stats_daily <- data.frame()
  
    for(i in 1:length(daTe_unique)){
      
      #i = 1
      
      #select daily data
      df_date_sel <- df_selected_month %>% filter(daTe == daTe_unique[i])
      
      #select tomst data only
      tomst_data <- df_date_sel %>% select(T1 : moist) %>%
        rename('soil' = 'T1',
               'soil_surf' = 'T2',
               'air' = 'T3')
      
      #function to calculate daily mean, max and min
      fun_tomst_data_summary <- function(x){
        max = max(x, na.rm = TRUE)
        min = min(x, na.rm = TRUE)
        mean = mean(x, na.rm = TRUE)
        tibble(max, min, mean)
      }
      
      #calculate stats
      tomst_data_summary_day_all <- tibble(date = ymd(unique(df_date_sel$daTe)))
      for(tt in seq(ncol(tomst_data))){
        
        #tt = 1
        
        data_sel <- tomst_data[, tt]
        
        tomst_data_summary_day <- fun_tomst_data_summary(data_sel)
        colnames(tomst_data_summary_day) <- paste(colnames(tomst_data)[tt],colnames(tomst_data_summary_day), sep = '_')
        
        tomst_data_summary_day_all <- tibble(tomst_data_summary_day_all, tomst_data_summary_day)
      }
      
      #select tomst data only
      meta_daily <- df_date_sel %>%
        select(country:treat_temp, date_out, year:day)%>% .[1, ]
      tomst_data_summary_day_all <- tibble(meta_daily, tomst_data_summary_day_all)
      
      df_stats_daily <- rbind.data.frame(df_stats_daily, tomst_data_summary_day_all)
      
    }

    df_stats_daily_all <- rbind.data.frame(df_stats_daily_all, df_stats_daily) 
    
  }
  
  df_all <- rbind.data.frame(df_all, df_stats_daily_all)
  
}

## check for NAs
#seems like no NAs
df_all %>% select(-date_out) %>%
  filter(if_any(everything(), is.na))


#save
write.csv(x = df_all,
          paste(path_dat_analysis, 'tomst_daily.csv', sep = '/'),
          row.names = FALSE)



### WARMING EFFECT - multiple plots ####


## calculate warming effect

## load data

df_all <- fread(paste(path_dat_analysis, 'tomst_daily.csv', sep = '/'))

#
#df_all$month <- factor(df_all$month,levels = c('May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'))
unique(df_all$month)

## select tomst variables
vars_temp_soil <- colnames(df_all)[grep("^soil", colnames(df_all))]
vars_temp_air <- colnames(df_all)[grep("^air", colnames(df_all))]
vars_moist <- colnames(df_all)[grep("^moist", colnames(df_all))]
#
(vars <- c(vars_temp_soil, vars_temp_air, vars_moist))

## calculate warming effect

df_warm_eff_daily <- df_all %>%
  select(date, month, block, treat_veg, treat_temp, country, vars[1]) %>%
  rename(var = all_of(vars[1])) %>%
  pivot_wider(names_from = treat_temp, values_from = var) %>%
  select(date:country)

for(i in seq(length(vars))){
  
  #i = 1
  
  df_warm_eff <- df_all %>%
    select(date, month, block, treat_veg, treat_temp, country, vars[i]) %>%
    rename(var = all_of(vars[i])) %>%
    pivot_wider(names_from = treat_temp, values_from = c(var)) %>%
    mutate(warm_effect = warming - ambient) %>%
    select(warm_effect) %>%
    rename(!!vars[i] := warm_effect)
  
  df_warm_eff_daily <- tibble(df_warm_eff_daily, df_warm_eff)
  
}

#remove rows with NAs

df_warm_eff_daily %>%
  filter(if_any(everything(), is.na))

df_warm_eff_daily <- df_warm_eff_daily %>%
  drop_na()

##summarize monthly effect

str(df_warm_eff_daily)

vars

df_warm_eff_daily %>% ungroup() %>%select(date:country)

df_warm_eff_monthly <- df_warm_eff_daily %>% ungroup() %>%select(date:country, !!sym(vars[1])) %>%
  group_by(month, block, treat_veg, country) %>%
  summarize(meaN = mean(!!sym(vars[1]), na.rm = T)) %>%
  select(-meaN) %>%
  ungroup()

for(vv in seq(length(vars))){

  #vv = 1
  
  df_warm_eff_daily_summ <- df_warm_eff_daily %>% ungroup() %>%select(date:country, !!sym(vars[vv])) %>%
    group_by(month, block, treat_veg, country) %>%
    summarize(meaN = mean(!!sym(vars[vv]), na.rm = T)) %>%
    rename(!!(vars[vv]) := meaN) %>% ungroup() #%>% select(!!sym(vars[vv]))
  
  df_warm_eff_monthly <- df_warm_eff_monthly %>% left_join(., df_warm_eff_daily_summ)
  
}


#### Temperature #####


### plotting

str(df_warm_eff_monthly)

theme_own <- theme(axis.title = element_text(size = 14),
                   axis.text = element_text(size = 12),
                   strip.text = element_text(size = 14),
                   plot.title = element_text(size = 18),
                   axis.title.x = element_blank(),
                   legend.position = 'none')

temp_vars <- c('soil_mean','soil_max', 
               'soil_surf_mean', 'soil_surf_max',
               'air_mean', 'air_max')

couNtry <- c("switzerland", "norway", "south_africa"  )

#select
#f = 1 is for monthly means
#f = 2 is for monthly max
f = 1

#
pp <- list()
for(c in 1:3){

  #c = 3
  
  df_warm_eff_country <- df_warm_eff_monthly %>% filter(country == couNtry[c])
  
  
  if(couNtry[c] != 'south_africa'){
    df_warm_eff_country$month <- factor(df_warm_eff_country$month,
                         levels = c('May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'))
  }else{
    df_warm_eff_country$month <- factor(df_warm_eff_country$month,
                           levels = c('Sep', 'Oct','Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May'))
  }

  if(couNtry[c] == 'south_africa'){
    df_warm_eff_country <- df_warm_eff_country |>
      filter(month != 'Sep') |>
      filter(month != 'May')
    
    
  }
  
  ploTT <- list()
  
  for(a in 1:length(temp_vars)){
    
    #a = 1
    
    temp_var_symbol <- sym(temp_vars[a])
    
    df_summary <- df_warm_eff_country %>% 
      group_by(month, treat_veg) %>% 
      summarize(mean_y = mean(!!temp_var_symbol, na.rm = T),
                sd_y = sd(!!temp_var_symbol, na.rm = T),
                se_y = sd(!!temp_var_symbol, na.rm = T) / sqrt(n()))
    df_warm_eff_country <- ungroup(df_warm_eff_country)
    
  
    ploTT[[a]]<- 
      ggplot()+
      geom_point(data = df_warm_eff_country,aes(x = month,y = !!temp_var_symbol,color = treat_veg),alpha = 0.35,size = 3)+
      geom_point(data = df_summary,aes(x = month,y = mean_y),size = 2)+
      geom_errorbar(data = df_summary, aes(x = month, ymin = mean_y - 2*se_y,ymax = mean_y + 2*se_y), width = 0.2)+
      geom_hline(yintercept = 0, linewidth = 0.5, linetype = 'dashed')+
      facet_grid(.~treat_veg,labeller = labeller(treat_veg = c("control" = "Control Veg", "focals" = "Focals Veg")))+
      labs(x = 'Month', 
           y = expression("OTC effect (warmed - control) (" * degree * "C)"), 
           color = 'Vegetation')+
      theme_minimal()
    
  }

  indexes <- list()
  indexes[[1]] <- c(first = 1, second = 3, third = 5)
  indexes[[2]] <- c(first = 2, second = 4, third = 6)
  
  index_map <- indexes[[f]]
  
  ## select ylims
  
  if(c == 1){
    if(f == 1){
      ylims <- c(0, 3.1)
    }else{
      ylims <- c(0, 11)
    }
  }
  
  if(c == 2){
    if(f == 1){
      ylims <- c(-1.6, 2)
    }else{
      ylims <- c(-4, 6)
    }
  }
  
  if(c == 3){
    if(f == 1){
      ylims <- c(-2, 5)
    }else{
      ylims <- c(-2.5, 11)
    }
  }

  #
  (p <- plot_grid(ploTT[[index_map['first']]]+
                    ylim(ylims)+
                    theme(legend.position = 'none',
                          axis.title.x = element_blank(),
                          plot.title = element_text(size = 16))+
                    theme_own+
                    labs(title = 'Soil (6 cm depth)'),
                  ploTT[[index_map['second']]]+
                    ylim(ylims)+
                    theme(legend.position = 'none',
                          axis.title.y = element_blank(),
                          plot.title = element_text(size = 16))+
                    theme_own+
                    labs(title = 'Air (2 cm above the soil)'),
                  ploTT[[index_map['third']]]+
                    ylim(ylims)+
                    theme(legend.position = 'none',
                          axis.title.y = element_blank(),
                          axis.title.x = element_blank(),
                          plot.title = element_text(size = 16))+
                    theme_own+
                    labs(title = 'Air (15 cm above the soil)'),
                  nrow = 1,
                  align = "hv")
  )
  
  #change the first letter of country name
  capitalize_first_letter <- function(string) {
    paste0(toupper(substr(string, 1, 1)), substr(string, 2, nchar(string)))
  }
  
  country_sel <- capitalize_first_letter(couNtry[c])

  plot_letter <- c('(a)', '(b)', '(c)')
  
  (plot_title <- paste(plot_letter[c],country_sel,
                       sep = ' ')
  )
  
  
  title <- ggdraw() +
    draw_label(plot_title,
               fontface='bold',
               size = 20,
               x = 0,hjust = 0)
  
  pp[[c]] <- plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) 
  
}

plot_tiles <- c('Monthly average daily mean temperature',
                  'Monthly mean daily max temperature')

title <- ggdraw() +
  draw_label(plot_tiles[f],
             fontface='bold',
             size = 20,
             x = 0,hjust = 0)

pp_all <- plot_grid(title,
                     pp[[1]],
                     pp[[2]],
                    pp[[3]],
                     ncol=1,
                     align = "hv",
                     rel_heights=c(0.1, 1, 1, 1)) 

pp_all                   

#

plot_grand_name <- c('temp_monthly_mean', 'temp_monthly_max')
plot_png_name <- paste(plot_grand_name[f], 'png', sep = '.')
plot_path_name <- paste(path_figures , plot_png_name, sep = '/')

ggsave(plot_path_name,
  plot = pp_all, width = 15, height = 12, dpi = 300
  )



##### STATS ####


###### get number of blocks ####

df_warm_eff_all <- df_warm_eff_monthly

str(df_warm_eff_all)

block_numbers <- df_warm_eff_all %>% group_by(country, month, treat_veg) %>%
  summarize(n = length(block))



###### are they different from zero? ####

df_warm_eff_all <- df_warm_eff_monthly
str(df_warm_eff_all)

(couNtry <- unique(df_warm_eff_all$country))
(veggie <- unique(df_warm_eff_all$treat_veg))
(response <- colnames(df_warm_eff_all)[-c(1:4)])

#
df_t_test <- tibble()
for(c in 1:2){
  #c = 1
  ## country
  df_warm_eff_country <- df_warm_eff_all %>% filter(country == couNtry[c])
  
  df_t_test_veg <- tibble()
  for(v in seq(length(veggie))){
    #v = 1
    ##vegetation
    df_all_veg <- df_warm_eff_country %>% filter(treat_veg == veggie[v])
    
    ##month
    (time_month <- unique(df_all_veg$month))
    
    results_t_test <- tibble(country = couNtry[c],
                        treat_veg = veggie[v],
                        month = time_month)
    
    for(a in seq(length(time_month))){
      
      #a=1
      df_month <- df_all_veg %>% filter(month == time_month[a])
      
      ## response variables
      
      for(b in seq(length(response))){
        
        #b = 1
        temp_sel <- df_month %>% select(response[b])
        
        ##t-test
        t_test <- t.test(temp_sel[, 1])
        results_t_test[a, b+3] <- t_test$p.value
        
      }
    }
    
    df_t_test_veg <- rbind.data.frame(df_t_test_veg, results_t_test)
    
  }
  
  df_t_test <- rbind.data.frame(df_t_test, df_t_test_veg)
  
}
colnames(df_t_test)[-c(1:3)] <- response


## adjust p values
df_t_test_values <- df_t_test[, -c(1:3)]
df_t_test_p_adjust <- data.frame(apply(df_t_test_values, 2, function(x){p.adjust(x, 'BH')}))

## change numbers to stars
# Function to replace values with stars
replace_with_stars <- function(x) {
  ifelse(x < 0.001, "***",
         ifelse(x < 0.01, "**",
                ifelse(x < 0.05, "*", 
                       ifelse(x > 0.2, round(x, 2), round(x, 3)))))
}
#select data
temp_vars <- c('soil_mean','soil_surf_mean','air_mean',
               'soil_max','soil_surf_max','air_max')

df_t_test_sel <- df_t_test %>% select(all_of(temp_vars))
df_t_test_p_adjust_sel <- df_t_test_p_adjust %>% select(all_of(temp_vars))
colnames(df_t_test_p_adjust_sel) <- paste(colnames(df_t_test_p_adjust_sel), 'adj', sep = '_')

#run function to change to stars
df_t_test_sel_stars <- as.data.frame(lapply(df_t_test_sel, replace_with_stars))
df_t_test_p_adjust_sel_stars <- as.data.frame(lapply(df_t_test_p_adjust_sel, replace_with_stars))
colnames(df_t_test_sel_stars) <- paste(colnames(df_t_test_sel_stars), '*', sep='')

## combine datasets

#select columns for the raw t-test results
df_t_test_all <- df_t_test %>%
  select(all_of(c('country', 'treat_veg', 'month', temp_vars)))

#combine raw t-test results with adjusted data
df_t_test <- cbind.data.frame(df_t_test_all,
                              df_t_test_sel_stars,
                              df_t_test_p_adjust_sel_stars)

#add number of blocks per each group
df_t_test <- left_join(df_t_test, block_numbers)

#
colnames(df_t_test)
df_t_test <- df_t_test %>% select(country:month, n,  'soil_mean*':'air_max*', soil_mean:air_max,soil_mean_adj:air_max_adj)

#capitalize factor levels
firstup <- function(x) {
  x <- as.character(x)  # Ensure the input is a character vector
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

df_t_test <- df_t_test %>% mutate(country = firstup(country),
                                  treat_veg = firstup(treat_veg))
#write.csv
write.csv(x = df_t_test,
          file = paste(path_dat_analysis, 'tomst_t_test_temp.csv', sep = '/'),
          row.names = FALSE)


###### country * vegetation * time effect ####


str(df_warm_eff_all)
df_warm_eff_all <- droplevels(df_warm_eff_all)

#remove observation from May to balance the model
df_lmer <- df_warm_eff_all %>% filter(month != 'May')


###### ______soil mean #####

mod <- lmer(data = df_lmer, soil_mean ~ country * treat_veg * month+ (1|block))

#mod <- lm(data = df_lmer, soil_monthly ~ block + country * treat_veg * month)

anova(mod)
plot(mod)
lattice::qqmath(mod)
hist(resid(mod), breaks = 30)

#
mod_old <- mod
mod_new <- update(mod_old, .~. -country:treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_01 <- mod_new
#
mod_old <- mod_01
mod_new <- update(mod_old, .~. -treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
mod_02 <- mod_new
AIC(mod_old, mod_new)
#
mod_old <- mod_02
anova(mod_old)
mod_new <- update(mod_old, .~. -country:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
plot(mod_new)
lattice::qqmath(mod_new)
hist(resid(mod_new), breaks = 15)
mod_03 <- mod_new
#
mod_old <- mod_03
anova(mod_old)
mod_new <- update(mod_old, .~. -country:treat_veg)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
#
mod_old <- mod_03
anova(mod_old)
mod_new <- update(mod_old, .~. -month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
#

##model mod_03 is the final model
mod_final <- mod_03
anova(mod_final)
summary(mod_final)
r.squaredGLMM(mod_final)

plot(mod_final)
lattice::qqmath(mod_final)
hist(resid(mod_final), breaks = 15)


### emmeans

ref_grid(mod_final)

## country | treat_veg

emmip(mod_final, country ~ treat_veg,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ country|treat_veg, 
                     type='response',
                     bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ treat_veg*country)
EMM
pairs(EMM)




## month

emmip(mod_final, ~ month,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ country|treat_veg, 
               type='response',
               bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ treat_veg*country)
EMM
pairs(EMM)
#

###### _____soil surface mean #####

mod <- lmer(data = df_lmer, soil_surf_monthly ~ country * treat_veg * month+ (1|block))
anova(mod)
plot(mod)
lattice::qqmath(mod)
hist(resid(mod), breaks = 15)

#
mod_old <- mod
anova(mod_old)
mod_new <- update(mod_old, .~. -country:treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_01 <- mod_new
#
mod_old <- mod_01
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_02 <- mod_new
#
mod_old <- mod_02
anova(mod_old)
mod_new <- update(mod_old, .~. -country:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_3 <- mod_new

#
mod_old <- mod_03
anova(mod_old)
mod_new <- update(mod_old, .~. -country:treat_veg)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)

#
mod_old <- mod_03
anova(mod_old)
mod_new <- update(mod_old, .~. -month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
#

#final model
mod_final <- mod_03
anova(mod_final)

plot(mod_final)
lattice::qqmath(mod_final)
hist(resid(mod_final), breaks = 15)

### emmeans

ref_grid(mod_final)

emmip(mod_final,  ~ month,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ month, 
               type='response',
               bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ month*country)
EMM
pairs(EMM)


###### _____air mean #####


mod <- lmer(data = df_lmer, air_monthly ~ country * treat_veg * month+ (1|block))
anova(mod)
plot(mod)
lattice::qqmath(mod)
hist(resid(mod), breaks = 10)

#
mod_old <- mod
mod_new <- update(mod_old, .~. -country:treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_01 <- mod_new
#
mod_old <- mod_01
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_02 <- mod_new
#
mod_old <- mod_02
anova(mod_old)
mod_new <- update(mod_old, .~. -country:treat_veg)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_03 <- mod_new
#
mod_old <- mod_03
anova(mod_old)
mod_new <- update(mod_old, .~. -country:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_04 <- mod_new
#
mod_old <- mod_04
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_05 <- mod_new
#
mod_old <- mod_05
anova(mod_old)
mod_new <- update(mod_old, .~. -month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
#
mod_old <- mod_05
anova(mod_old)
mod_new <- update(mod_old, .~. -country)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
#

#final model
mod_final <- mod_05
anova(mod_final)

plot(mod_final)
lattice::qqmath(mod_final)
hist(resid(mod_final), breaks = 15)

### emmeans

ref_grid(mod_final)

emmip(mod_final,  ~ country,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ country, 
               type='response',
               bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ month*country)
EMM
pairs(EMM)


## month

emmip(mod_final,  ~ month,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ month, 
               type='response',
               bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ month*country)
EMM
pairs(EMM)


###### effect within country ####


###### _____switzerland#####

#remove observation from May to balance the model
df_lmer <- df_warm_eff_all %>% filter(month != 'May')

#select country
df_lmer <- df_lmer %>% filter(country == 'switzerland')

str(df_lmer)

df_pivot <- df_lmer %>%
  select(month, block, treat_veg, country,
  soil_mean, soil_surf_mean, air_mean) %>%
  pivot_longer(
    cols = soil_mean:air_mean,
    names_to = 'temp',
    values_to = 'warming_eff')


#
mod <- lmer(data = df_pivot, log(warming_eff) ~ temp * treat_veg * month+ (1|block))
anova(mod)
plot(mod)
lattice::qqmath(mod)
hist(resid(mod), breaks = 10)

#
mod_old <- mod
anova(mod_old)
mod_new <- update(mod_old, .~. -temp:treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_01 <- mod_new
#
mod_old <- mod_01
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_02 <- mod_new
#
mod_old <- mod_02
anova(mod_old)
mod_new <- update(mod_old, .~. -temp:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_03 <- mod_new
#
mod_old <- mod_03
anova(mod_old)
mod_new <- update(mod_old, .~. -temp:treat_veg )
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
#
mod_old <- mod_03
anova(mod_old)
mod_new <- update(mod_old, .~. -month )
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)

##final model
mod_final <- mod_03
aov.mod <- anova(mod_final)
aov.mod
write.table(t(aov.mod), 'clipboard', sep = '\t')
r.squaredGLMM(mod_final)

plot(mod_final)
lattice::qqmath(mod_final)
hist(resid(mod_final), breaks = 10)


### emmeans

ref_grid(mod_final)

emmip(mod_final, ~ treat_veg|temp,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ treat_veg|temp, 
               type='response',
               sigma = sigma(mod_final),
               bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ treat_veg*variable)
EMM
pairs(EMM)

### plotting
str(df_lmer)
str(df_pivot)

df_pivot <- df_pivot %>%
  mutate(temp = factor(temp,
                          levels = c('soil_mean', 'soil_surf_mean' ,'air_mean')))
emm_plot <- emmip(mod_final, ~ treat_veg|temp,
                  type='response', bias.adj = TRUE, 
                  CIs = TRUE,
                  plotit = FALSE)

df_pivot %>% ggplot()+
  geom_point(data = df_pivot, aes(x = treat_veg, y = warming_eff, color = month),
             size = 5, alpha = 0.25)+
  geom_point(data = emm_plot, aes(x = xvar, y = yvar),
             size = 4)+
  geom_errorbar(data = emm_plot, aes(x = xvar,
                                     ymin = LCL, ymax = UCL), width = 0.2)+
  facet_wrap(.~temp,
             labeller = labeller(temp = c('air_mean' = 'Air (15 cm)',
                                             'soil_mean' = 'Soil (6 cm)',
                                             'soil_surf_mean' = 'Air (2 cm)')))+
  labs(title = 'Switzerland',
       x = 'Vegetation treatment',
       y= 'Warming effect (warmed - ambient) [°C]',
       colour = '')+
  scale_x_discrete(labels = c('control' = 'Control', 'focals' = 'Focals'))+
  theme_minimal()+
  theme(axis.title = element_text(size = 14),
                   axis.text = element_text(size = 12),
                   strip.text = element_text(size = 14),
                   plot.title = element_text(size = 18))+
  geom_text(data = annotations, aes(x = x, y = y, label = label), size = 5)

annotations <- data.frame(
  temp = c('air_mean', 'soil_mean', 'soil_surf_mean'),
  x = 1.5,  # x coordinate (for centered between 'control' and 'focals')
  y = c(2.2, 2.5, 3.1),  # y coordinates for each facet
  label = c('n.s.', '***', '***')# labels for each facet
)


## month

emmip(mod_final, ~ month,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ month, 
               type='response',
               bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ month)
EMM
pairs(EMM)

### plotting
str(df_lmer)
str(df_pivot)

df_pivot <- df_pivot %>%
  mutate(temp = factor(temp,
                       levels = c('soil_monthly', 'soil_surf_monthly' ,'air_monthly')))
emm_plot <- emmip(mod_final, ~ month,
                  type='response', bias.adj = TRUE, 
                  CIs = TRUE,
                  plotit = FALSE)

df_pivot %>% ggplot()+
  geom_point(data = df_pivot, aes(x = month, y = warming_eff, color = temp),
             size = 5, alpha = 0.25,
             position=position_dodge(width=0.5))+
  geom_point(data = emm_plot, aes(x = xvar, y = yvar),
             size = 4)+
  geom_errorbar(data = emm_plot, aes(x = xvar,
                                     ymin = LCL, ymax = UCL), width = 0.2)+
  labs(title = 'Switzerland',
       x = '',
       y= 'Warming effect (warmed - ambient) [°C]',
       colour = '')+
  scale_color_discrete(labels = c('air_monthly' = 'Air (15 cm)',
                                  'soil_monthly' = 'Soil (6 cm)',
                                  'soil_surf_monthly' = 'Air (2 cm)'))+
  theme_minimal()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 18))+
  annotate('text',
           x = c(1,2,3,4,5),
           y = c(3.2, 2.7, 2.35, 2.4, 2.7),
           label = c('A', 'B', 'BC', 'C', 'BC'))


###### _____norway#####

#remove observation from May to balance the model
df_lmer <- df_warm_eff_all %>% filter(month != 'May')

#select country
df_lmer <- df_lmer %>% filter(country == 'norway')

str(df_lmer)

df_pivot <- df_lmer %>%
  select(month, block, treat_veg, country,
         soil_mean, soil_surf_mean, air_mean) %>%
  pivot_longer(
    cols = soil_mean:air_mean,
    names_to = 'temp',
    values_to = 'warming_eff')

mod <- lmer(data = df_pivot, warming_eff ~ temp * treat_veg * month+ (1|block))
anova(mod)
plot(mod)
lattice::qqmath(mod)
hist(resid(mod), breaks = 10)

#
mod_old <- mod
anova(mod_old)
mod_new <- update(mod_old, .~. -temp:treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_01 <- mod_new
#
mod_old <- mod_01
anova(mod_old)
mod_new <- update(mod_old, .~. -temp:treat_veg )
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_02 <- mod_new
#
mod_old <- mod_02
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_03 <- mod_new
#
mod_old <- mod_03
anova(mod_old)
mod_new <- update(mod_old, .~. -temp:month )
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
#
mod_old <- mod_03
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg )
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)

##final model
mod_final <- mod_03
anova(mod_final)
plot(mod_final)
lattice::qqmath(mod_final)
hist(resid(mod_final), breaks = 10)
r.squaredGLMM(mod_final)

#
aov.mod <- t(anova(mod_final))
aov.mod
write.table(aov.mod, 'clipboard', sep = '\t' )


### emmeans

## month|temp
ref_grid(mod_final)

emmip(mod_final, ~ month|temp,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ month|temp, 
               type='response',
               bias.adj = TRUE)
EMM


### plotting
str(df_pivot)

df_pivot <- df_pivot %>%
  mutate(temp = factor(temp,
                       levels = c('soil_monthly', 'soil_surf_monthly' ,'air_monthly')))

emm_plot <- emmip(mod_final, ~ month|temp,
                  type='response', bias.adj = TRUE, 
                  CIs = TRUE,
                  plotit = FALSE)

df_pivot %>% ggplot()+
  geom_point(data = df_pivot, aes(x = month, y = warming_eff),
             size = 3, alpha = 0.15)+
  geom_point(data = emm_plot, aes(x = xvar, y = yvar),
             size = 4)+
  geom_errorbar(data = emm_plot, aes(x = xvar,
                                     ymin = LCL, ymax = UCL), width = 0.2)+
  labs(title = 'Norway',
       x = '',
       y= 'Warming effect (warmed - ambient) [°C]',
       colour = '')+
  facet_wrap(.~ temp,
             labeller = labeller(temp = c('air_monthly' = 'Air (15 cm)',
                                 'soil_monthly' = 'Soil (6 cm)',
                                 'soil_surf_monthly' = 'Air (2 cm)')))+
  ylim(c(-1.45, 2.1))+
  theme_minimal()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 18))+
  geom_text(data = annotate_soil, x = c(1, 2, 3, 4, 5), y = y_soil, aes(label = label), size = 4)+
  geom_text(data = annotate_air_2, x = c(1, 2, 3, 4, 5), y = y_air_2, aes(label = label), size = 4)+
  geom_text(data = annotate_air_15, x = c(1, 2, 3, 4, 5), y = y_air_15, aes(label = label), size = 4)
  
annotate_soil <- data.frame(
  temp = c( 'soil_monthly'),#, 'soil_surf_monthly','air_monthly'),
  label = c('A', 'B', 'B', 'AB', 'A')
)

add_to_yvals <- 0.2
y_soil <- y_values$soil_monthly + add_to_yvals
y_air_2 <- y_values$soil_surf_monthly + add_to_yvals
y_air_15 <- y_values$air_monthly + add_to_yvals

annotate_air_15 <- data.frame(
  temp = c( 'air_monthly'),#, 'soil_surf_monthly','air_monthly'),
  label = c('A', 'AB', 'B', 'B', 'AB')
)

annotate_air_2 <- data.frame(
  temp = c( 'soil_surf_monthly'),#, 'soil_surf_monthly','air_monthly'),
  label = c('A', 'C', 'C', 'C', 'B')
)

y_values <- df_pivot %>% group_by(temp, month) %>%
  summarize(maxx = max(warming_eff)) %>%
  pivot_wider(names_from = temp, values_from = maxx)


## treat_veg

emmip(mod_final, ~ treat_veg,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ treat_veg, 
               type='response',
               bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ treat_veg)
EMM
pairs(EMM)

#


### plotting
str(df_lmer)
str(df_pivot)

emm_plot <- emmip(mod_final, ~ treat_veg,
                  type='response', bias.adj = TRUE, 
                  CIs = TRUE,
                  plotit = FALSE)

df_pivot %>% ggplot()+
  geom_point(data = df_pivot, aes(x = treat_veg, y = warming_eff, color = month),
             size = 5, alpha = 0.25,
             position=position_dodge(width=0.5)
             )+
  geom_point(data = emm_plot, aes(x = xvar, y = yvar),
             size = 4)+
  geom_errorbar(data = emm_plot, aes(x = xvar,
                                     ymin = LCL, ymax = UCL), width = 0.2)+
  labs(title = 'Norway',
       x = 'Vegtation treatment',
       y= 'Warming effect (warmed - ambient) [°C]',
       colour = '')+
  scale_x_discrete(labels = c('control' = 'Control',
                                  'focals' = 'Focals'))+
  theme_minimal()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 18))+
  annotate('text',
           x = c(1,2),
           y = c(2, 2),
           label = c('A', 'B'))





###### __________soil mean #####

#remove observation from May to balance the model
df_lmer <- df_warm_eff_all %>% filter(month != 'May')
#select country
df_lmer <- df_lmer %>% filter(country == 'norway')


mod <- lmer(data = df_lmer, soil_monthly ~ treat_veg * month+ (1|block))
anova(mod)
plot(mod)
lattice::qqmath(mod)
hist(resid(mod), breaks = 10)

#
mod_old <- mod
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_01 <- mod_new
#
mod_old <- mod_01
anova(mod_old)
mod_new <- update(mod_old, .~. -month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
#
mod_old <- mod_01
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_02 <- mod_new

##final model
mod_final <- mod_02

### emmeans

ref_grid(mod_final)

emmip(mod_final, ~ month,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ month, 
               type='response',
               bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ treat_veg*country)
EMM
pairs(EMM)

#




#### Moisture ####

str(df_warm_eff_monthly)

### plotting

str(df_warm_eff_monthly)

theme_own <- theme(axis.title = element_text(size = 14),
                   axis.text = element_text(size = 12),
                   strip.text = element_text(size = 14),
                   plot.title = element_text(size = 18),
                   axis.title.x = element_blank(),
                   legend.position = 'none')

temp_vars <- c( 'moist_mean', 'moist_max', 'moist_min')

moistures <- c('Monthly average daily mean soil moisture index',
               'Monthly average daily max soil moisture index',
               'Monthly average daily min soil moisture index')

y_limss <- list(c(-1400, 500), c(-1500, 2000), c(-1000, 1000))

#
pp <- list()
for(c in 1:3){
  
  #c = 3
  
  df_warm_eff_country <- df_warm_eff_monthly %>% filter(country == couNtry[c])
  
  if(couNtry[c] != 'south_africa'){
    df_warm_eff_country$month <- factor(df_warm_eff_country$month,
                                        levels = c('May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'))
  }else{
    df_warm_eff_country$month <- factor(df_warm_eff_country$month,
                                        levels = c('Sep', 'Oct','Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May'))
  }
  
  if(couNtry[c] == 'south_africa'){
    df_warm_eff_country <- df_warm_eff_country |>
      filter(month != 'Sep') |>
      filter(month != 'May')
  }
  
    
  ploTT <- list()

  for(a in 1:length(temp_vars)){
    
    #a = 1
    
    temp_var_symbol <- sym(temp_vars[a])
    
    #calculate mean and se
    df_summary <- df_warm_eff_country %>% 
      group_by(month, treat_veg) %>% 
      summarize(mean_y = mean(!!temp_var_symbol, na.rm = T),
                sd_y = sd(!!temp_var_symbol, na.rm = T),
                se_y = sd(!!temp_var_symbol, na.rm = T) / sqrt(n()))
    df_warm_eff_country <- ungroup(df_warm_eff_country)
  
    ploTT[[a]] <- ggplot()+
      geom_point(data = df_warm_eff_country,aes(x = month,y = !!temp_var_symbol,color = treat_veg),alpha = 0.35,size = 3)+
      geom_point(data = df_summary,aes(x = month,y = mean_y),size = 2)+
      geom_errorbar(data = df_summary, aes(x = month, ymin = mean_y - 2*se_y,ymax = mean_y + 2*se_y), width = 0.2)+
      geom_hline(yintercept = 0, linewidth = 0.5, linetype = 'dashed')+
      facet_grid(.~treat_veg,labeller = labeller(treat_veg = c("control" = "Control Veg", "focals" = "Focals Veg")))+
      labs(title = moistures[a],
           x = 'Month', 
           y = expression("OTC effect (warmed - control) (" * degree * "C)"), 
           color = 'Vegetation')+
      ylim(y_limss[[c]])+
      theme_minimal()+
      theme(legend.position = 'none',
            axis.title.x = element_blank())
    
  }
  
  #
  (p <- plot_grid(ploTT[[1]],
                  ploTT[[2]]+
                    theme(axis.title.y = element_blank()),
                  ploTT[[3]]+
                    theme(axis.title.y = element_blank()),
                  nrow = 1,
                  align = "hv")
  )
  
  #change the first letter of country name
  capitalize_first_letter <- function(string) {
    paste0(toupper(substr(string, 1, 1)), substr(string, 2, nchar(string)))
  }
  
  country_sel <- capitalize_first_letter(as.character(couNtry[c]))
  
  (plot_title <- country_sel)
  
  if(plot_title == 'South_africa'){
    plot_title <- c('South Africa')
  }
  
  title <- ggdraw() +
    draw_label(plot_title,
               fontface='bold',
               size = 20,
               x = 0,hjust = 0)
  
  pp[[c]] <- plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) 
  
}

pp_all <- plot_grid(pp[[1]],
                    pp[[2]],
                    pp[[3]],
                    ncol = 1,
                    align = "hv")
pp_all                   

##ggsave
plot_png_name <- paste('moist_tomst_t_test', 'png', sep = '.')
plot_path_name <- paste(path_figures , plot_png_name, sep = '/')
ggsave(plot_path_name,
       plot = pp_all, width = 15, height = 12, dpi = 300)



##### stats ####

###### get number of blocks ####

str(df_warm_eff_monthly)

block_numbers <- df_warm_eff_monthly %>%
  group_by(country, month, treat_veg) %>%
  summarize(n = length(block))



###### are they different from zero? ####

str(df_warm_eff_monthly)



(couNtry <- unique(df_warm_eff_monthly$country))
(veggie <- unique(df_warm_eff_monthly$treat_veg))

(response <- colnames(df_all)[grep("^moist", colnames(df_all))])
                  
#
df_t_test <- tibble()
for(c in 1:2){
  #c = 1
  ## country
  df_warm_eff_country <- df_warm_eff_monthly %>% filter(country == couNtry[c])
  
  df_t_test_veg <- tibble()
  for(v in seq(length(veggie))){
    #v = 1
    ##vegetation
    df_all_veg <- df_warm_eff_country %>% filter(treat_veg == veggie[v])
    
    ##month
    (time_month <- unique(df_all_veg$month))
    
    results_t_test <- tibble(country = couNtry[c],
                             treat_veg = veggie[v],
                             month = time_month)
    
    for(a in seq(length(time_month))){
      
      #a=1
      df_month <- df_all_veg %>% filter(month == time_month[a])
      
      ## response variables
      
      for(b in seq(length(response))){
        
        #b = 1
        temp_sel <- df_month %>% select(response[b])
        
        ##t-test
        t_test <- t.test(temp_sel[, 1])
        results_t_test[a, b+3] <- t_test$p.value
        
      }
    }
    
    df_t_test_veg <- rbind.data.frame(df_t_test_veg, results_t_test)
    
  }
  
  df_t_test <- rbind.data.frame(df_t_test, df_t_test_veg)
  
}
colnames(df_t_test)[-c(1:3)] <- response


## adjust p values
df_t_test_values <- df_t_test[, -c(1:3)]
df_t_test_p_adjust <- data.frame(apply(df_t_test_values, 2, function(x){p.adjust(x, 'BH')}))

## change numbers to stars
# Function to replace values with stars
replace_with_stars <- function(x) {
  ifelse(x < 0.001, "***",
         ifelse(x < 0.01, "**",
                ifelse(x < 0.05, "*", 
                       ifelse(x > 0.2, round(x, 2), round(x, 3)))))
}
#select data
select_vars <- df_warm_eff_monthly %>% select(starts_with('moist'))%>%
  colnames(.)

df_t_test_sel <- df_t_test %>% select(all_of(select_vars))
df_t_test_p_adjust_sel <- df_t_test_p_adjust %>% select(all_of(select_vars))
colnames(df_t_test_p_adjust_sel) <- paste(colnames(df_t_test_p_adjust_sel), 'adj', sep = '_')

#run function to change to stars
df_t_test_sel_stars <- as.data.frame(lapply(df_t_test_sel, replace_with_stars))
df_t_test_p_adjust_sel_stars <- as.data.frame(lapply(df_t_test_p_adjust_sel, replace_with_stars))
colnames(df_t_test_sel_stars) <- paste(colnames(df_t_test_sel_stars), '*', sep='')

## combine datasets

#select columns for the raw t-test results
df_t_test_all <- df_t_test %>%
  select(all_of(c('country', 'treat_veg', 'month', select_vars)))

#combine raw t-test results with adjusted data
df_t_test <- cbind.data.frame(df_t_test_all,
                              df_t_test_sel_stars,
                              df_t_test_p_adjust_sel_stars)

#add number of blocks per each group

df_t_test <- left_join(df_t_test, block_numbers)

#capitalize factor levels
firstup <- function(x) {
  x <- as.character(x)  # Ensure the input is a character vector
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}
df_t_test <- df_t_test %>% mutate(country = firstup(country),
                                  treat_veg = firstup(treat_veg))

df_t_test <- df_t_test %>% select(country:month, n,
                                  'moist_mean*', 'moist_max*', 'moist_min*',
                                  moist_mean, moist_max, moist_min,
                                  moist_mean_adj, moist_max_adj, moist_min_adj)
colnames(df_t_test)

#write.csv
write.csv(x = df_t_test,
          file = paste(path_dat_analysis, 'tomst_t_test_moist.csv', sep = '/'),
          row.names = FALSE)



###### effect within country ####


###### _____switzerland#####

#remove observation from May to balance the model
df_lmer <- df_warm_eff_monthly %>% filter(month != 'May')

#select country
df_lmer <- df_lmer %>% filter(country == 'switzerland')

str(df_lmer)

df_sel <- df_lmer %>%
  select(month, block, treat_veg, country,
         moist_min) %>%
  rename(warming_eff = moist_min)


#
mod <- lmer(data = df_sel, warming_eff ~ treat_veg * month+ (1|block))
anova(mod)
plot(mod)
lattice::qqmath(mod)
hist(resid(mod), breaks = 10)

#
mod_old <- mod
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_01 <- mod_new
#
mod_old <- mod_01
anova(mod_old)
mod_new <- update(mod_old, .~. -month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
#
mod_old <- mod_01
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)

##final model
mod_final <- mod
aov.mod <- anova(mod_final)
aov.mod
write.table(t(aov.mod), 'clipboard', sep = '\t')
r.squaredGLMM(mod_final)

plot(mod_final)
lattice::qqmath(mod_final)
hist(resid(mod_final), breaks = 10)


### emmeans

ref_grid(mod_final)

emmip(mod_final, ~ treat_veg|month,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ treat_veg|temp, 
               type='response',
               bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ treat_veg*variable)
EMM
pairs(EMM)

### plotting
str(df_lmer)
str(df_sel)

df_sel <- df_sel %>%
  mutate(temp = factor(temp,
                       levels = c('soil_mean', 'soil_surf_mean' ,'air_mean')))
emm_plot <- emmip(mod_final, ~ treat_veg|temp,
                  type='response', bias.adj = TRUE, 
                  CIs = TRUE,
                  plotit = FALSE)

df_sel %>% ggplot()+
  geom_point(data = df_sel, aes(x = treat_veg, y = warming_eff, color = month),
             size = 5, alpha = 0.25)+
  geom_point(data = emm_plot, aes(x = xvar, y = yvar),
             size = 4)+
  geom_errorbar(data = emm_plot, aes(x = xvar,
                                     ymin = LCL, ymax = UCL), width = 0.2)+
  facet_wrap(.~temp,
             labeller = labeller(temp = c('air_mean' = 'Air (15 cm)',
                                          'soil_mean' = 'Soil (6 cm)',
                                          'soil_surf_mean' = 'Air (2 cm)')))+
  labs(title = 'Switzerland',
       x = 'Vegetation treatment',
       y= 'Warming effect (warmed - ambient) [°C]',
       colour = '')+
  scale_x_discrete(labels = c('control' = 'Control', 'focals' = 'Focals'))+
  theme_minimal()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 18))+
  geom_text(data = annotations, aes(x = x, y = y, label = label), size = 5)

annotations <- data.frame(
  temp = c('air_mean', 'soil_mean', 'soil_surf_mean'),
  x = 1.5,  # x coordinate (for centered between 'control' and 'focals')
  y = c(2.2, 2.5, 3.1),  # y coordinates for each facet
  label = c('n.s.', '***', '***')# labels for each facet
)


## month

emmip(mod_final, ~ treat_veg,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ treat_veg, 
               type='response',
               bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ month)
EMM
pairs(EMM)

### plotting
str(df_lmer)
str(df_sel)

df_sel <- df_sel %>%
  mutate(temp = factor(temp,
                       levels = c('soil_monthly', 'soil_surf_monthly' ,'air_monthly')))
emm_plot <- emmip(mod_final, ~ month,
                  type='response', bias.adj = TRUE, 
                  CIs = TRUE,
                  plotit = FALSE)

df_sel %>% ggplot()+
  geom_point(data = df_sel, aes(x = month, y = warming_eff, color = temp),
             size = 5, alpha = 0.25,
             position=position_dodge(width=0.5))+
  geom_point(data = emm_plot, aes(x = xvar, y = yvar),
             size = 4)+
  geom_errorbar(data = emm_plot, aes(x = xvar,
                                     ymin = LCL, ymax = UCL), width = 0.2)+
  labs(title = 'Switzerland',
       x = '',
       y= 'Warming effect (warmed - ambient) [°C]',
       colour = '')+
  scale_color_discrete(labels = c('air_monthly' = 'Air (15 cm)',
                                  'soil_monthly' = 'Soil (6 cm)',
                                  'soil_surf_monthly' = 'Air (2 cm)'))+
  theme_minimal()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 18))+
  annotate('text',
           x = c(1,2,3,4,5),
           y = c(3.2, 2.7, 2.35, 2.4, 2.7),
           label = c('A', 'B', 'BC', 'C', 'BC'))


###### _____norway#####

#remove observation from May to balance the model
df_lmer <- df_warm_eff_monthly %>% filter(month != 'May')

#select country
df_lmer <- df_lmer %>% filter(country == 'norway')

str(df_lmer)

df_sel <- df_lmer %>%
  select(month, block, treat_veg, country,
         soil_monthly, soil_surf_monthly, air_monthly) %>%
  pivot_longer(
    cols = soil_monthly:air_monthly,
    names_to = 'temp',
    values_to = 'warming_eff')


mod <- lmer(data = df_sel, warming_eff ~ temp * treat_veg * month+ (1|block))
anova(mod)
plot(mod)
lattice::qqmath(mod)
hist(resid(mod), breaks = 10)

#
mod_old <- mod
anova(mod_old)
mod_new <- update(mod_old, .~. -temp:treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_01 <- mod_new
#
mod_old <- mod_01
anova(mod_old)
mod_new <- update(mod_old, .~. -temp:treat_veg )
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_02 <- mod_new
#
mod_old <- mod_02
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg:month)
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
mod_03 <- mod_new
#
mod_old <- mod_03
anova(mod_old)
mod_new <- update(mod_old, .~. -temp:month )
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)
#
mod_old <- mod_03
anova(mod_old)
mod_new <- update(mod_old, .~. -treat_veg )
anova(mod_new)
anova(mod_old, mod_new)
AIC(mod_old, mod_new)

##final model
mod_final <- mod_03
anova(mod_final)
r.squaredGLMM(mod_final)

#
aov.mod <- t(anova(mod_final))
write.table(aov.mod, 'clipboard', sep = '\t' )


### emmeans

## month|temp
ref_grid(mod_final)

emmip(mod_final, ~ month|temp,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ month|temp, 
               type='response',
               bias.adj = TRUE)
EMM


### plotting
str(df_sel)

df_sel <- df_sel %>%
  mutate(temp = factor(temp,
                       levels = c('soil_monthly', 'soil_surf_monthly' ,'air_monthly')))

emm_plot <- emmip(mod_final, ~ month|temp,
                  type='response', bias.adj = TRUE, 
                  CIs = TRUE,
                  plotit = FALSE)

df_sel %>% ggplot()+
  geom_point(data = df_sel, aes(x = month, y = warming_eff),
             size = 3, alpha = 0.15)+
  geom_point(data = emm_plot, aes(x = xvar, y = yvar),
             size = 4)+
  geom_errorbar(data = emm_plot, aes(x = xvar,
                                     ymin = LCL, ymax = UCL), width = 0.2)+
  labs(title = 'Norway',
       x = '',
       y= 'Warming effect (warmed - ambient) [°C]',
       colour = '')+
  facet_wrap(.~ temp,
             labeller = labeller(temp = c('air_monthly' = 'Air (15 cm)',
                                          'soil_monthly' = 'Soil (6 cm)',
                                          'soil_surf_monthly' = 'Air (2 cm)')))+
  ylim(c(-1.45, 2.1))+
  theme_minimal()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 18))+
  geom_text(data = annotate_soil, x = c(1, 2, 3, 4, 5), y = y_soil, aes(label = label), size = 4)+
  geom_text(data = annotate_air_2, x = c(1, 2, 3, 4, 5), y = y_air_2, aes(label = label), size = 4)+
  geom_text(data = annotate_air_15, x = c(1, 2, 3, 4, 5), y = y_air_15, aes(label = label), size = 4)

annotate_soil <- data.frame(
  temp = c( 'soil_monthly'),#, 'soil_surf_monthly','air_monthly'),
  label = c('A', 'B', 'B', 'AB', 'A')
)

add_to_yvals <- 0.2
y_soil <- y_values$soil_monthly + add_to_yvals
y_air_2 <- y_values$soil_surf_monthly + add_to_yvals
y_air_15 <- y_values$air_monthly + add_to_yvals

annotate_air_15 <- data.frame(
  temp = c( 'air_monthly'),#, 'soil_surf_monthly','air_monthly'),
  label = c('A', 'AB', 'B', 'B', 'AB')
)

annotate_air_2 <- data.frame(
  temp = c( 'soil_surf_monthly'),#, 'soil_surf_monthly','air_monthly'),
  label = c('A', 'C', 'C', 'C', 'B')
)

y_values <- df_sel %>% group_by(temp, month) %>%
  summarize(maxx = max(warming_eff)) %>%
  pivot_wider(names_from = temp, values_from = maxx)


## treat_veg

emmip(mod_final, ~ treat_veg,
      type='response', bias.adj = TRUE, 
      #sigma = rand.err.sigma,
      CIs = TRUE) + theme_minimal()

#
EMM <- emmeans(mod_final, specs = pairwise ~ treat_veg, 
               type='response',
               bias.adj = TRUE)
EMM

#
EMM <- emmeans(mod_final, ~ treat_veg)
EMM
pairs(EMM)

#


### plotting
str(df_lmer)
str(df_sel)

emm_plot <- emmip(mod_final, ~ treat_veg,
                  type='response', bias.adj = TRUE, 
                  CIs = TRUE,
                  plotit = FALSE)

df_sel %>% ggplot()+
  geom_point(data = df_sel, aes(x = treat_veg, y = warming_eff, color = month),
             size = 5, alpha = 0.25,
             position=position_dodge(width=0.5)
  )+
  geom_point(data = emm_plot, aes(x = xvar, y = yvar),
             size = 4)+
  geom_errorbar(data = emm_plot, aes(x = xvar,
                                     ymin = LCL, ymax = UCL), width = 0.2)+
  labs(title = 'Norway',
       x = 'Vegtation treatment',
       y= 'Warming effect (warmed - ambient) [°C]',
       colour = '')+
  scale_x_discrete(labels = c('control' = 'Control',
                              'focals' = 'Focals'))+
  theme_minimal()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 18))+
  annotate('text',
           x = c(1,2),
           y = c(2, 2),
           label = c('A', 'B'))


#### summarize ####

path_dat_clean <- c('C:/Users/radim/My Drive (radim.sarlej@gmail.com)/2. RESEARCH PROJECTS/RangeX/data_clean')

###fluxes
fluxee_all <- fread(paste(path_dat_clean, 'fluxfiles_light_response_processed_selected_fluxes_remove_repeated.csv', sep = '/'),)

#mutate and select columns needed
fluxee <- fluxee_all %>% mutate(block_plot_time_point = paste(block_plot, time_point, sep = '_')) %>%
  select(country, block_plot, block_plot_time_point, date_time, model, flux_lm )



###extract mean temp/moist data from tomst
#for the time of measurement period
#and the the times just before and just after the measurement period

couNtries <- c('norway', 'switzerland', 'south_africa')
fluxee_tomst_data <- tibble()

for(a in seq(length(couNtries), from = 1)){
  
  #a = 3
  
  (country_sel <- couNtries[a])
  
  #### summarize flux timing and duration
  
  ###select flux data for the whole country
  
  fluxee_sel <- fluxee %>%
    filter(country == country_sel)# & time_point == 'second')
  head(fluxee_sel)
  
  ##select the time for the first and the last measurements for each plot
  plot_time <- unique(fluxee_sel$block_plot_time_point)
  fluxee_sel_date_time <- tibble()
  
  for(f in seq(length(plot_time), from = 1)){
    
    #f = 1
    
    #select data for each block_plot_time_point
    fluxee_sel_block_time <- fluxee_sel %>%
      filter(block_plot_time_point == plot_time[f])
    
    #remove the one measurement which was done on a previous day
    #it's only for one block_plot_time_point   
    if(unique(fluxee_sel_block_time$block_plot_time_point) == 'p7C_second'){
      
      (measurement_time_day_before <- ymd_hms(c('2023-08-20 15:10:00')))
      #which measurements are from the second day
      next_day_measurements <- dmy_hm(fluxee_sel_block_time$date_time) > measurement_time_day_before
      
      fluxee_sel_block_time <- fluxee_sel_block_time [next_day_measurements, ]
    }
    
    #calculate end of measurements and the duration
    if(length(str_split((str_split(fluxee_sel_block_time$date_time[1], ' ')[[1]][2]),':')[[1]]) != 2){
      stop('Adjust code for [#calculate end of measurements and the duration]')
    }
    
    (t_end <- dmy_hm(max(fluxee_sel_block_time$date_time)))
    (t_start <- dmy_hm(min(fluxee_sel_block_time$date_time)))
    (t_duration <- as.duration(interval(t_start,t_end)))
    
    
    fluxee_sel_block_time_min <- fluxee_sel_block_time %>% 
      mutate(date_time_end = t_end,
             measurement_length = t_duration) %>%
      slice(1)
    
    
    fluxee_sel_date_time <- rbind(fluxee_sel_date_time, fluxee_sel_block_time_min)
    
  }
  
  head(fluxee_sel_date_time)
  
  #### Tomst data
  
  #create tomst file name
  tomst.file.name <- paste(paste('tomst', couNtries[a], sep='_'),
                           '_clean_all.csv',
                           sep = '')
  
  if(tomst.file.name %in% list.files(path_dat_clean, full.names = FALSE)){
    ## check if there is a tomst file for the particular country
    
    ## for the countries which have the tomst data
    tomst <- fread(paste(path_dat_clean, tomst.file.name, sep = '/'))
    head(tomst)
    
    ## extract unique block_plot_time_point
    
    (block.plot.time <- unique(fluxee_sel_date_time$block_plot_time_point))
    fluxee_tomst_data_country <- tibble()
    
    for(i in seq(block.plot.time)){
      
      #i = 1
      
      
      ### flux data
      
      #get the flux data for single block_plot_time_point
      fluxee_sel_flux <- fluxee_sel_date_time %>%
        filter(block_plot_time_point == block.plot.time[i])
      
      #change date_time to time format
      fluxee_sel_flux <- fluxee_sel_flux %>% 
        mutate(date_time = dmy_hm(date_time))
      
      #get block_plot name
      BlockPlot <- fluxee_sel_flux$block_plot
      
      
      ### extract temp before measurements
      
      #get the start of the measurement
      flux_start <- fluxee_sel_flux$date_time - minutes(15)
      #create the window before the measurement
      flux_window_before_start <- flux_start - minutes(45)
      
      #extact tomst data for the time before the measurement and calculate their means
      tomst_before_measurement <- tomst %>% 
        filter(datetime > flux_window_before_start &
                 datetime < flux_start &
                 block_plot == BlockPlot) %>%
        select(T1:T3) %>%
        summarize(T1_mean_before = mean(T1),
                  T2_mean_before = mean(T2),
                  T3_mean_before = mean(T3))
      
      tomst_before_measurement
      
      
      ##extract temp after measurements
      #create the end time
      flux_ends <- fluxee_sel_flux$date_time_end + minutes(30)
      #create the end time of the window after the measurememts
      flux_window_after_end <- flux_ends + minutes(45)
      
      #extact tomst data for the time after the measurement and calculate their means
      tomst_after_measurement <- tomst %>% 
        filter(datetime > flux_ends &
                 datetime < flux_window_after_end &
                 block_plot == BlockPlot) %>%
        select(T1:T3) %>%
        summarize(T1_mean_after = mean(T1),
                  T2_mean_after = mean(T2),
                  T3_mean_after = mean(T3))
      
      tomst_after_measurement
      
      
      ##extract to temp/moist during the measurements
      
      #create a time window for each measurement depending on the length of the measurement
      flux_start <- fluxee_sel_flux$date_time
      flux_end <- fluxee_sel_flux$date_time_end
      
      #get the block_plot for tomst selection and summarize the temp/moist
      
      tomst_measurement_summ <- tomst %>% 
        filter(datetime < flux_end & datetime > flux_start & block_plot == BlockPlot) %>%
        select(datetime, T1, T2, T3, moist, block_plot) %>%
        #group_by(block_plot) %>%
        mutate(rows = nrow(.)) %>%
        summarize(T1_mean = mean(T1),
                  T2_mean = mean(T2),
                  T3_mean = mean(T3),
                  moist_mean = mean(moist),
                  num_meas.ments = mean(rows)
        ) %>%
        tibble(., 
               tomst_before_measurement, 
               tomst_after_measurement,
               fluxee_sel_flux)
      
      #
      fluxee_tomst_data_country <- rbind.data.frame(fluxee_tomst_data_country,
                                                    tomst_measurement_summ)
    }
    
  } else {
    
    ### for the country, which has not got tomst file
    #the tomst data are set to NA
    
    ##create an empty tibble with column names same as for the summarize tomst data
    # Define the column names vector
    column_names <- c("T1_mean", "T2_mean", "T3_mean",
                      "moist_mean", "num_meas.ments",
                      "T1_mean_before", "T2_mean_before", "T3_mean_before",
                      "T1_mean_after", "T2_mean_after", "T3_mean_after")
    
    empty_tibble <- tibble(!!!as.list(setNames(rep(NA_real_, length(column_names)),
                                               column_names)))
    
    
    fluxee_tomst_data_country <- tibble(empty_tibble, fluxee_sel_date_time)
    
    #set date time
    
    fluxee_tomst_data_country <- fluxee_tomst_data_country %>%
      mutate(date_time = dmy_hm(date_time) )
    
  }
  
  
  #fluxee_tomst_data_country <- fluxee_tomst_data_country %>%
  #select(-c('plot_id'))
  
  
  fluxee_tomst_data <- rbind.data.frame(fluxee_tomst_data,
                                        fluxee_tomst_data_country)
  
  
}

head(data.frame(fluxee_tomst_data))

#fluxee_tomst_data <- fluxee_tomst_data %>% select(- flux, -model)

#
write.csv(fluxee_tomst_data,
          paste(path_dat_clean,
                'tomst_data_extracted_for_measurement_period.csv',
                sep = '/'))










