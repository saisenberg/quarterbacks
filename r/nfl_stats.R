# Scrapes NFL quarterback statistics and clusters quarterbacks into successes & non-successes
# Code by Sam Isenberg, last updated 09/2018


library(data.table)
library(dplyr)
library(ggplot2)
library(gtools)
library(htmltab)
library(jsonlite)
library(stringr)


# Set earliest quarterback draft year
beginning_qb_year <- 1985


# Set most recent quarterback draft year
ending_qb_year <- 2017


# Scrape NFL quarterback career data
url_start <- paste0('https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=combined&year_min=', beginning_qb_year, '&year_max=2017&season_start=1&season_end=-1&pos%5B%5D=qb&draft_year_min=1936&draft_year_max=2018&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos%5B%5D=qb&draft_pos%5B%5D=rb&draft_pos%5B%5D=wr&draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c5val=1.0&order_by=pass_att&offset=')

offset_seq <- seq(0, 500, 100)
qb_stats_all <- data.frame()

for(url_end in offset_seq){
  
  print(paste0('Scraping from QB #', url_end, '!'))
  url <- paste0(url_start, url_end)
  data <- htmltab(url, which = 1, rm_nodata_cols = F)
  qb_stats_all <- rbind(qb_stats_all, data)
  
}

rm(url_start, url_end, offset_seq, url, data)


# Clean NFL quarterback career data
colnames(qb_stats_all) <- c('num', 'player', 'start_year', 'end_year', 'draft', 'team', 'league', 'g', 'gs', 'cmp', 'att', 'cmp_pct', 'yds', 'td', 'int', 'td_pct', 'int_pct', 'passing_rate', 'sacked', 'sack_yds', 'yards_per_attempt', 'adj_yards_per_attempt', 'adj_net_yards_per_attempt', 'yds_gained_per_game', 'w', 'l', 't')

qb_stats_all <- subset(qb_stats_all, select = -c(num)) %>% filter(player != 'Player')
qb_stats_all$player <- gsub(pattern = "\\*", replacement = "", x = qb_stats_all$player)
qb_stats_all$player <- gsub(pattern = "\\+", replacement = "", x = qb_stats_all$player)
qb_stats_all[,c(2:3, 7:26)] <- as.numeric(as.character(unlist(qb_stats_all[,c(2:3, 7:26)])))
qb_stats_all$years_in_league <- qb_stats_all$end_year - qb_stats_all$start_year + 1


# Scrape advanced NFL quarterback stats
qb_advanced <- data.frame()

for(year in beginning_qb_year:2017){
  
  print(paste0('Scraping NFL stats for year ', year, '!'))
  url <- paste0('https://www.pro-football-reference.com/years/', year, '/passing.htm')
  data <- htmltab(url, which = 1)
  data$year <- year
  
  qb_advanced <- smartbind(qb_advanced, data)
  
}

rm(data, url, year)


# Clean advanced NFL quarterback stats
rownames(qb_advanced) <- seq(1:nrow(qb_advanced))
colnames(qb_advanced)[2] <- 'player'
qb_advanced <- subset(qb_advanced, select = -c(Rk))
qb_advanced$Pos <- toupper(qb_advanced$Pos)
qb_advanced <- filter(qb_advanced, !is.na(player)) %>% filter(grepl(pattern = "QB", x = Pos) | is.na(Pos))
qb_advanced$player <- gsub(pattern = "\\*|\\+", replacement = "", x = qb_advanced$player)
qb_advanced <- qb_advanced[,c(1:27)]
qb_advanced[,c(3, 5:6, 8:ncol(qb_advanced))] <- as.numeric(as.character(unlist(qb_advanced[,c(3, 5:6, 8:ncol(qb_advanced))])))


# Create new columns (for clustering)
qb_stats_all$att_per_year <- qb_stats_all$att / qb_stats_all$years_in_league
qb_stats_all$gs_per_year <- qb_stats_all$gs / qb_stats_all$years_in_league
qb_stats_all$win_pct <- qb_stats_all$w / (qb_stats_all$w + qb_stats_all$l + qb_stats_all$t)


# Only include quarterbacks who: 
# (1) Retired after the cutoff year (1998) 
# (2) Started playing before the ending qb year (2017)
# (3) Have at least one creditedwin, loss or tie
cutoff_year <- beginning_qb_year+13
qb_stats <- filter(qb_stats_all, start_year < ending_qb_year, end_year > cutoff_year, (w+l+t)>=1)


# Scale features
qb_stats$scaled_rate <- as.numeric(scale(qb_stats$passing_rate)) * 2
qb_stats$scaled_att_per_year <- as.numeric(scale(qb_stats$att_per_year)) * 1.5
qb_stats$scaled_adj_yards_per_attempt <- as.numeric(scale(qb_stats$adj_yards_per_attempt)) * 1
qb_stats$scaled_win_pct <- as.numeric(scale(qb_stats$win_pct)) * 2.5
qb_stats$scaled_gs_per_year <- as.numeric(scale(qb_stats$gs_per_year)) * 1


# K-means clustering (full dataset)
set.seed(123)
kmeans_1 <- kmeans(qb_stats[,c(31:35)], centers = 9, nstart = 500)
qb_stats$cluster <- factor(kmeans_1$cluster)
ggplot(qb_stats, aes(x=scaled_att_per_year, y=scaled_rate, color = cluster, size=2)) + geom_point(size=5, alpha=0.5) + xlab('Attempts per year (scaled)') + ylab('Quarterback rating (scaled)') + ggtitle('Quarterback clusters') + theme(plot.title = element_text(hjust=0.5))

qbGroup <- function(num){
  return(qb_stats[qb_stats$cluster == num, 'player'])
}


# Separately split Group #4 quarterbacks into successes and non-successes
group4 <- filter(qb_stats, player %in% qbGroup(4) | player == 'Jimmy Garoppolo')
group4 <- group4[,c(1:30)]
group4$scaled_passing_rate <- as.numeric(scale(group4$passing_rate))
group4$cluster <- ifelse(group4$scaled_passing_rate > 0, 1, 2)
group4 <- filter(group4, cluster == 1)

qb_stats$success <- ifelse(qb_stats$cluster %in% c(8) | qb_stats$player %in% group4$player, 1, 0)
rm(group4)


# Import and clean college football statistics csv
cfb_stats <- fread('./data/cfb_stats.csv')


## Collect and clean heights & weights for QBs drafted through 2016
height_weight <- fromJSON(paste0(getwd(), './data/height_weight.json'))
height_weight$name <- trimws(height_weight$name)
height_weight$position <- trimws(height_weight$position)
height_weight <- filter(height_weight, position == 'QB' | name %in% c('Joe Webb', 'Tom Tupa'))
height_weight$name <- str_replace(string = height_weight$name, pattern = 'Robert Griffin III', replacement = 'Robert Griffin')
height_weight <- height_weight[,c('name', 'height', 'weight')]


# Collect heights and weights for QBs drafted in 2017-18
year_ranges <- list(c(2017, 2018))
height_weight_recent <- htmltab(paste0('https://www.pro-football-reference.com/play-index/nfl-combine-results.cgi?request=1&year_min=', year_ranges[[1]][1], '&year_max=', year_ranges[[1]][2], '&pos%5B%5D=QB&show=all&order_by=year_id'), which = 1, rm_nodata_cols = T)
height_weight_recent <- arrange(height_weight_recent, Year, Player) %>% filter(Player != 'Player')
height_weight_recent$Player <- str_replace(string = height_weight_recent$Player, pattern = "Mitchell Trubisky", replacement = "Mitch Trubisky")
height_weight_recent <- height_weight_recent[,c('Player', 'Height', 'Wt')]


# Separate data for QBs drafted before 2017
prev_drafted_qb <- as.data.frame(sort(unique(qb_stats$player)))
names(prev_drafted_qb) <- 'player'
prev_drafted_qb <- inner_join(prev_drafted_qb, cfb_stats, by = 'player')
prev_drafted_qb <- inner_join(prev_drafted_qb, height_weight, by = c('player' = 'name'))
prev_drafted_qb$height <- as.numeric(substr(x = prev_drafted_qb$height, start = 0, stop = str_locate(string = prev_drafted_qb$height, pattern = '-')-1))*12 + as.numeric(substr(x = prev_drafted_qb$height, start = str_locate(string = prev_drafted_qb$height, pattern = '-')+1, stop = nchar(prev_drafted_qb$height)))
prev_drafted_qb$weight <- as.numeric(prev_drafted_qb$weight)
prev_drafted_qb <- prev_drafted_qb %>% mutate(BMI = (0.453592*weight)/((0.0254*height)^2))
prev_drafted_qb <- inner_join(prev_drafted_qb, qb_stats[,c('player', 'success')], by = 'player')


# Separate data for QBs drafted after 2016
recent_drafted_qb <- as.data.frame(unique(sort(c(filter(qb_stats_all, start_year >= 2017)$player, height_weight_recent$Player))))
names(recent_drafted_qb) <- 'player'
recent_drafted_qb <- left_join(recent_drafted_qb, cfb_stats, by = 'player')
recent_drafted_qb <- left_join(recent_drafted_qb, height_weight_recent[,c('Player', 'Height', 'Wt')], by = c('player' = 'Player')) %>% rename(height = Height, weight = Wt)


# Update missing player height & weight data for recently drafted players
addPlayer <- function(playerInput, heightInput, weightInput){
  recent_drafted_qb[recent_drafted_qb$player == playerInput,]$height <<- heightInput
  recent_drafted_qb[recent_drafted_qb$player == playerInput,]$weight <<- weightInput
}

addPlayer('Jake Rudock', '6-3', 212)
addPlayer('Joe Callahan', '6-1', 216)
addPlayer('Nate Sudfeld', '6-6', 227)
addPlayer('Taylor Heinicke', '6-1', 210)
addPlayer('Taysom Hill', '6-2', 221)


# Clean height & weight data
recent_drafted_qb$height <- as.numeric(substr(x = recent_drafted_qb$height, start = 0, stop = str_locate(string = recent_drafted_qb$height, pattern = '-')-1))*12 + as.numeric(substr(x = recent_drafted_qb$height, start = str_locate(string = recent_drafted_qb$height, pattern = '-')+1, stop = nchar(recent_drafted_qb$height)))
recent_drafted_qb$weight <- as.numeric(recent_drafted_qb$weight)
recent_drafted_qb <- recent_drafted_qb %>% mutate(BMI = (0.453592*weight)/((0.0254*height)^2))


# Write to csv
write.csv(prev_drafted_qb, './data/prev_drafted_qb.csv', row.names = F)
write.csv(recent_drafted_qb, './data/recent_drafted_qb.csv', row.names = F)
