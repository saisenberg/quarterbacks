# Collects all college football quarterback statistics from 1985-2018
# Code by Sam Isenberg, last updated 09/2018


library(dplyr)
library(rvest)
library(stringr)


# Set earliest quarterback draft year
beginning_qb_year <- 1985


# Set number of years prior (before earliest draft year) to scrape college statistics
num_years_prior <- 7


# Collect all links to college quarterback sports-reference pages
links_all <- c()
for(year in (beginning_qb_year-num_years_prior):(2017)){
  
  print(paste0('Scraping links for ', year, '!'))
  url <- paste0('https://www.sports-reference.com/cfb/years/', year, '-passing.html')
  
  links_list <- read_html(url) %>% html_nodes('a') %>% html_attr('href')
  links_list <- links_list[grepl(pattern = 'players/.+.htm', x = links_list)]
  links_list <- unique(links_list[1:which(links_list == '/cfb/players/derrick-henry-2.html')-1])
  links_all <- c(links_all, links_list)
  
}

links_all <- sort(unique(links_all)); rm(links_list)


# Initialize empty matrix of college statistics
cfb_stats <- matrix(data = NA, nrow = 0, ncol = 18, dimnames = list(c(NULL), c('player', 'yrs_in_school', 'school', 'conf', 'g', 'comp', 'att', 'pct', 'yds', 'yds_per_att', 'td', 'int', 'adj_yds_per_att', 'rate', 'rush_att', 'rush_yds', 'rush_avg', 'rush_td')))


# Scrape and clean statistics from each sports-reference page
for(player in links_all[625:632]){
  
  # Scrape player name and position
  tryCatch({
    
    url_player <- paste0('https://www.sports-reference.com', player)
    player_name <- trimws(read_html(url_player) %>% html_nodes('h1') %>% html_text())
    
    player_pos <- trimws(read_html(url_player) %>% html_nodes('p') %>% html_text())
    player_pos <- player_pos[grepl(pattern = "Position: ", x = player_pos)]
    player_pos <- str_replace(string = player_pos, pattern = "Position: ", replacement = "")
    
    
    # Ignore non-quarterbacks
    if(player_pos != 'QB'){
      next
    }
    
    print(paste0('Scraping player: ', player_name))
    
    
    # Collect player passing statistics
    player_pass <- as.data.frame(read_html(url_player) %>% html_table(header = F))
    player_pass[player_pass == ""] <- 0
    names(player_pass) <- unlist(player_pass[2,])
    player_pass <- filter(player_pass, !(Pos %in% c(0, 'Pos')))
    
    
    # Clean and aggregate player passing statistics
    player_yrs <- nrow(player_pass)
    player_school <- player_pass[player_pass$School != "" & !is.na(player_pass$School),]$School[length(player_pass[player_pass$School != "" & !is.na(player_pass$School),]$School)]
    player_conf <- player_pass[player_pass$Conf != "" & !is.na(player_pass$Conf),]$Conf[length(player_pass[player_pass$Conf != "" & !is.na(player_pass$Conf),]$Conf)]
    player_g <- sum(as.numeric(player_pass$G))
    player_cmp <- sum(as.numeric(player_pass$Cmp))
    player_att <- sum(as.numeric(player_pass$Att))
    player_pct <- player_cmp / player_att
    player_yds <- sum(as.numeric(player_pass$Yds))
    player_yds_per_att <- player_yds / player_att
    player_td <- sum(as.numeric(player_pass$TD))
    player_int <- sum(as.numeric(player_pass$Int))
    player_adj_yds_per_att <- (player_yds + (20 * player_td) - (45 * player_int)) / player_att
    player_rate <- ((8.4 * player_yds) + (330 * player_td) - (200 * player_int) + (100 * player_cmp)) / player_att
    
    
    # Write player passing statistics to vector
    player_pass_stats <- c(player_name, player_yrs, player_school, player_conf, player_g, player_cmp, player_att, player_pct, player_yds, player_yds_per_att, player_td, player_int, player_adj_yds_per_att, player_rate)
    
    
    # Collect and clean player rushing statistics
    player_rush <- read_html(url_player) %>% html_nodes('#all_rushing') %>% html_nodes(xpath = 'comment()') %>% html_text() %>% read_html() %>% html_node('table') %>% html_table()
    player_rush[player_rush == ""] <- 0
    names(player_rush) <- unlist(player_rush[1,])
    player_rush <- player_rush[which(player_rush$Year == 'Career'),c(7:10)]
    
    
    # Write player rushing statistics to vector
    player_rush_stats <- c(player_rush$Att, player_rush$Yds, player_rush$Avg, player_rush$TD)
    
    
    # Append player statistics to cfb_stats matrix
    cfb_stats <- rbind(cfb_stats, c(player_pass_stats, player_rush_stats))
  }, error = function(e){})
}

cfb_stats <- as.data.frame(cfb_stats)
cols <- names(cfb_stats)


# Collect and aggregate college football statistics for players missing from the qb_stats dataset, when available
missing_qb_stats <- fread('./data/missing_qbs.csv')
missing_qb_stats <- missing_qb_stats %>% group_by(player, school, conf) %>% summarise(yrs_in_school = length(player), g = sum(g), comp = sum(completions), att = sum(att), yds = sum(yds), td = sum(td), int = sum(int), rush_att = sum(rush_att), rush_yds = sum(rush_yds), rush_td = sum(rush_td))

missing_qb_stats$pct <- missing_qb_stats$comp / missing_qb_stats$att
missing_qb_stats$yds_per_att <- missing_qb_stats$yds / missing_qb_stats$att
missing_qb_stats$adj_yds_per_att <- (missing_qb_stats$yds + (20 * missing_qb_stats$td) - (45 * missing_qb_stats$int)) / missing_qb_stats$att
missing_qb_stats$rate <- ((8.4 * missing_qb_stats$yds) + (330 * missing_qb_stats$td) - (200 * missing_qb_stats$int) + (100 * missing_qb_stats$comp)) / missing_qb_stats$att
missing_qb_stats$rush_avg <- missing_qb_stats$rush_yds / missing_qb_stats$rush_att

missing_qb_stats <- as.data.frame(missing_qb_stats[, cols])

cfb_stats <- rbind(cfb_stats, missing_qb_stats); rm(missing_qb_stats)


# Name cleaning
cfb_stats$player <- str_replace(cfb_stats$player, 'Robert Griffin III', 'Robert Griffin')
cfb_stats$player <- str_replace(cfb_stats$player, 'E.J. Manuel','EJ Manuel')


# Determine whether quarterback played in a major conference
cfb_stats$major_conf <- as.factor(ifelse(cfb_stats$conf %in% c('ACC', 'Big 12', 'Big Ten', 'SEC', 'Pac-8', 'Pac-10', 'Pac-12', 'Big East') | cfb_stats$school %in% c('Notre Dame', 'Miami (FL)'), 1, 0))


# Write to csv
write.csv(cfb_stats, './data/cfb_stats.csv', row.names = F)
