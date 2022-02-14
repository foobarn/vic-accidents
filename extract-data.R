
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)



get_accident = function(){
  
  # data import function for the 'accident' files
  
  a = read_csv('data/raw/ACCIDENT/ACCIDENT.csv') %>%
    mutate(ACCIDENTTIME = ACCIDENTTIME %>% hms())
  
  b = read_csv('data/raw/2000 TO 2005 ACCIDENT/ACCIDENT.CSV') %>%
    mutate(ACCIDENTTIME = ACCIDENTTIME %>%  hms())
  
  rbind(a,b) %>%
    rename_with(tolower, ) %>%
    rename_with(~gsub(' ', '_', .)) %>%
    mutate(
      accidentdate = accidentdate %>% dmy(),
      accidenttime = accidenttime %>% as.double(),
      n_injured_t = no_persons_inj_2+no_persons_inj_3,
      casualties = no_persons_inj_2+no_persons_inj_3+no_persons_killed) %>%
    arrange(accidentdate)
  
}

get_event = function(){
  
  a = read_csv('data/raw/ACCIDENT/ACCIDENT_EVENT.csv')
  b = read_csv('data/raw/2000 TO 2005 ACCIDENT/ACCIDENT_EVENT.CSV')
  
  rbind(a,b) %>%
    rename_with(tolower, ) %>%
    rename_with(~gsub(' ', '_', .))
}

get_event_summary = function(){
  
  df_event = get_event()
  
  # get count of events, and type of first event for
  # each accident
  df_event_smry = df_event %>%
    group_by(accident_no) %>%
    summarise(no_of_events=n()) %>%
    left_join(
      df_event %>%
        filter(event_seq_no==1) %>%
        group_by(accident_no) %>%
        summarise(event_type=min(event_type)),
      by=c('accident_no'='accident_no')
    ) %>%
    left_join(
      df_event %>%
        select(event_type, event_type_desc) %>%
        distinct(),
      by=c('event_type'='event_type'))
  
  df_event_smry
  df_has_obj = df_event %>%
    filter(!object_type_desc %in% c('Not Applicable', 'Unknown')) 
  
  # get count of objects and details of first object
  # where applicable 
  df_obj_smry = df_has_obj %>%
    group_by(accident_no) %>%
    summarise(no_of_objs=n()) %>%
    left_join(
      df_has_obj %>%
        group_by(accident_no) %>%
        summarise(object_type=min(object_type)),
      by=c('accident_no'='accident_no')
    ) %>%
    left_join(
      df_has_obj %>%
        select(object_type, object_type_desc) %>%
        distinct(),
      by=c('object_type'='object_type'))
  
  
  df_event_smry %>%
    left_join(df_obj_smry, by=c('accident_no'='accident_no'))
  
}


get_node = function(){
  
  a = read_csv('data/raw/ACCIDENT/NODE.csv') %>%
    rename_with(tolower, ) %>%
    rename_with(~gsub(' ', '_', .)) %>%
    select(accident_no, node_id, node_type, node_type, lat, long)
  
  b = read_csv('data/raw/2000 TO 2005 ACCIDENT/NODE.csv') %>%
    rename_with(tolower, ) %>%
    rename_with(~gsub(' ', '_', .)) %>%
    select(accident_no, node_id, node_type, node_type, lat, long)
  
  # ensure we only have one entry for each node_id
  rbind(a,b) %>%
    group_by(node_id) %>%
    summarise(node_type=max(node_type),lat=median(lat),long=median(long))
  
}

process_data_from_rstudio = function(){
  
  # uses the above funcs to get our data into the processed folder
  
  # use r studio api to set working dir to script location
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  accident_df = get_accident()
  
  df_events = get_event_summary()
  
  accident_df = accident_df %>%
    left_join(df_events, by=c('accident_no'='accident_no'))
  
  # get stats for each node
  node_stats = accident_df %>% 
    group_by(node_id) %>% 
    summarise(
      n_accidents_at_node=n(),
      n_inj_at_node=sum(no_persons_inj_2+no_persons_inj_3),
      n_deaths_at_node=sum(no_persons_killed),
      fatal = n_deaths_at_node>0) %>%
    mutate(casualties_at_node = n_inj_at_node+n_deaths_at_node)
  
  node_df = get_node() %>%
    left_join(node_stats, by=c('node_id'='node_id'))
  
  
  
  accident_df %>% write_delim('data/processed/accidents.csv',delim=',')
  node_df  %>%  write_delim('data/processed/nodes.csv',delim=',')
  
  

  
  
  # get the first accident for each node, which we will use
  # in our model to predict future accidents
  first_accident_date=accident_df %>%
    filter(node_id>0) %>%
    group_by(node_id) %>%
    summarise(accidentdate=min(accidentdate)) %>%
    inner_join(accident_df, by=c('node_id'='node_id','accidentdate'='accidentdate'))
  
  first_accident = first_accident_date %>%
    group_by(node_id) %>%
    summarise(accidenttime=min(accidenttime)) %>%
    inner_join(first_accident_date, by=c('node_id'='node_id','accidenttime'='accidenttime'))
  
  first_accident = first_accident %>% 
    left_join(node_df , on=c('node_id'='node_id')) %>% 
    select(
      node_id, 
      accidentdate,
      accidenttime, 
      accident_type,
      day_of_week,
      dca_code,
      light_condition,
      no_of_vehicles,
      no_persons,
      no_persons_inj_2,
      no_persons_inj_3,
      no_persons_killed,
      police_attend,
      road_geometry,
      severity,
      n_injured_t,
      casualties,
      no_of_events,
      no_of_objs,
      object_type,
      lat,
      long,
      n_accidents_at_node,
      n_inj_at_node,
      n_deaths_at_node,
      casualties_at_node,
      fatal) %>%
    mutate(fatal = as.numeric(fatal)) %>%
    mutate(
      no_of_objs = replace_na(no_of_objs, 0),
      object_type = as.numeric( replace_na(object_type, 0)))
  
  first_accident %>% write_delim('data/processed/node-first-accident.csv',delim=',')
}

process_data_from_rstudio()
