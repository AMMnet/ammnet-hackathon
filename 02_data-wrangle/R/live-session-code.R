# Script from the live session on Monday October 21st 2024
 
 ########### READING IN DATA



# Create a data folder
dir.create("data")

# Download example data
url <- "https://raw.githubusercontent.com/AMMnet/AMMnet-Hackathon/main/02_data-wrangle/data/"

download.file(paste0(url, "mockdata_cases1.csv"), destfile = "data/mockdata_cases1.csv")
download.file(paste0(url, "mosq_mock1.csv"), destfile = "data/mosq_mock1.csv")

# Load example data
library("readr")
data_cases   <- readr::read_csv("data/mockdata_cases1.csv")
#mosq_data  <- read_csv("data/mosq_mock1.csv")

########### EXPLORING THE DATA
# prevalence used as a measure of spread of infection #+/#n

# Erroneous values for prevalence
library("tidyverse")

data_new <- data_cases%>%
               dplyr::filter(prev <= 0 | prev >= 1)%>%
                 dplyr::mutate(prev2=positive/total)

data_cases[data_cases$prev<=0|data_cases$prev>=1,]

# Update erroneous values for prevalence
data_prev <- data_cases%>%
               dplyr::mutate(prev_updated=positive/total)

data_prev%>%
  dplyr::filter(prev_updated <= 0 | prev_updated >= 1)

####

# Filter erroneous values for prevalence, wrong way
data_error <- data_prev%>%
               dplyr::filter (prev_updated >= 0 | prev_updated <= 1)

# Filter erroneous values for prevalence
library(validate)
schema <- validate::validator(prev >= 0,
                              prev <= 1,
                              positive >= 0)

out   <- validate::confront(data_cases, schema)
summary(out)


##### SUMMARY STATS

mean(data_cases$prev[data_cases$location=="mordor"])


# Summary statistics 

# Filter erroneous values for prevalence
data_use <- data_prev%>%
               dplyr::filter (prev_updated >= 0 )%>%
                  dplyr::filter (prev_updated <= 1)

data_use%>%
  dplyr::group_by(location)%>%
  dplyr::summarise(nobs=n(),
                   mean_prev=mean(prev_updated),
                   min_prev=min(prev_updated),
                   max_prev=max(prev_updated))

### multiple grouping variables

# Summary statistics by location
### piping operator "\>" part of magrittr / tidyverse
data_use%>%
  dplyr::group_by(location, year)%>%
  dplyr::summarise(nobs=n(),
                   mean_prev=mean(prev_updated),
                   min_prev=min(prev_updated),
                   max_prev=max(prev_updated))

### piping operator "\>" part of base R (version 4.)
data_use|>
  dplyr::group_by(location, year)|>
  dplyr::summarise(nobs=n(),
                   mean_prev=mean(prev_updated),
                   min_prev=min(prev_updated),
                   max_prev=max(prev_updated))


#### data for mordor

data_mordor1 <- data_use[data_use$location=="mordor",] 

data_mordor <- data_use%>% 
                 dplyr::filter(location=="mordor")

head(data_mordor,n=2);tail(data_mordor)

data_use%>% 
  tail(n=7)


####

# Summary statistics by location, map summary function
library(purrr)

data_use_age_summary <- purrr::map(.x=seq(length(data_use_list)),
                                   .f=function(x){
                                     data_use_list[[x]]%>%
                                       dplyr::group_by(location,year,ages)%>%
                                       dplyr::summarise(nobs=n(),
                                                        mean_prev=mean(prev_updated),
                                                        min_prev=min(prev_updated),
                                                        max_prev=max(prev_updated)) 
                                     
                                   })


data_mordor <- data_use_age_summary[[1]]

data_mordor


#######

# Summary statistics with age groups
age_order <- c("under_5","5_to_14","15_above")

data_use_ordered <- data_use

data_use_ordered$age_group <- factor(data_use$ages,
                                     levels =age_order)

data_mordor_reordered <- data_use_ordered%>%
                          dplyr::group_by(location, year,age_group)%>%
                           dplyr::summarise(nobs=n(),
                                            mean_prev=mean(prev_updated),
                                            min_prev=min(prev_updated),
                                            max_prev=max(prev_updated))%>%
                               dplyr::filter(location=="mordor")

data_mordor_reordered

##### GRAPHS

#Plotting evolution over time
# Sphagetti plot

evolution_plot <- ggplot2::ggplot(data=data_use_ordered,
                                  mapping=aes(x=month,
                                              y=prev_updated,
                                              group=location,
                                              colour=location))+
                          ggplot2::facet_wrap(~year)+ 
                         ggplot2::geom_line(lwd=0.75)+

                          ggplot2::theme_bw()+
                            ggplot2::xlab("Month of the Year")+
                            ggplot2::ylab("Prevalence")+
  ggplot2::scale_x_discrete(limits=factor(1:12),
                            labels=c("J","F","M",
                                     "A","M","J",
                                     "J","A","S",
                                     "O","N","D"))+
  ggplot2::scale_y_continuous(breaks=seq(from=0,
                                         to=0.7,
                                         by=0.1))

evolution_plot

# bar plot (stacked)

#Check case count

data_use_ordered_long <- tidyr::pivot_longer(data=data_use_ordered,
                                             cols=c("positive","total"),
                                             names_to="Outcome",
                                             values_to="counts")


mordor_stacked_bar_graph <- ggplot2::ggplot(data=data_use_ordered_long%>%
                                              dplyr::filter(location=="mordor"),
                                            mapping=aes(x=month,
                                                        y=counts,
                                                        fill=Outcome))+
  ggplot2::scale_x_discrete(limits=factor(1:12),
                            labels=c("J","F","M",
                                     "A","M","J",
                                     "J","A","S",
                                     "O","N","D"))+
  ggplot2::geom_bar(position="stack", stat="identity")+
  ggplot2::facet_wrap(~year)+ 
  ggplot2::theme_bw()+
  ggplot2::xlab("Month of the Year")+
  ggplot2::ylab("Count")

mordor_stacked_bar_graph

?tidyr::pivot_wider()