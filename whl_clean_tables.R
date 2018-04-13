
#Remove All Objects Except dt.rosters.final
#gdata::keep(dt.rosters.final, sure = TRUE)
#Clean Stats Tables
dt.rosters.final$pos.clean <- ifelse(dt.rosters.final$position_id == 1, "D", "F")
rosterTable <- dt.rosters.final[, c(37,161,40,26,44:66,123:125,131:136,143:146,156:159,73,80,87,94,101,108,115,122,142)]
#Remove Goalies
#rosterTable <- data.table(filter(rosterTable, position != "G"))
#Get Stats Table
rosterTable.stats <- rosterTable[,c(1:5,7:10,28,6,31,33:36)]
names(rosterTable.stats) <- c("Name", "Position", "Team", "Birth.Date", "GP", "G", "A1", "A2","PTS", "Season", "Shots", "Team.Shots", "Team.G", "Team.A1", "Team.A2", "Team.PTS")
#Get Draft Eligibility
rosterTable.stats$BD.Mod <- as.Date(rosterTable.stats$Birth.Date, "%Y-%m-%d")
rosterTable.stats$birth.year <- as.numeric(substr(rosterTable.stats$Birth.Date, 1, 4))
rosterTable.stats$draft.end <- as.Date(paste(rosterTable.stats$birth.year, 9, 15, sep = "-"), "%Y-%m-%d")
rosterTable.stats$draft.year <- ifelse(rosterTable.stats$BD.Mod > rosterTable.stats$draft.end, rosterTable.stats$birth.year + 19, rosterTable.stats$birth.year + 18)
rosterTable.stats$draft.season <- as.numeric(substr(rosterTable.stats$Season, 1, 4)) + 1
rosterTable.stats$Draft.Eligible <- ifelse(as.numeric(rosterTable.stats$draft.season) == as.numeric(rosterTable.stats$draft.year), "YES", "NO")
fwrite(rosterTable, 'data/rosterTable.csv')
#Get Stats
rosterTable.stats$PRIMARY.PTS <- round(rosterTable.stats$G + rosterTable.stats$A1,digits=2)
rosterTable.stats$TEAM.PRIMARY.PTS <- round(rosterTable.stats$Team.G + rosterTable.stats$Team.A1,digits=2)
rosterTable.stats$SH.PERC <- round(rosterTable.stats$G/rosterTable.stats$Shots,digits=2)
rosterTable.stats$PERC.TEAM.SHOTS <- round(rosterTable.stats$Shots/rosterTable.stats$Team.Shots,digits=2)
rosterTable.stats$PERC.TEAM.PTS <- round(rosterTable.stats$PTS/rosterTable.stats$Team.PTS,digits=2)
rosterTable.stats$PERC.TEAM.PrPTS <- round(rosterTable.stats$PRIMARY.PTS/rosterTable.stats$TEAM.PRIMARY.PTS,digits=2)
rosterTable.stats <- rosterTable.stats[,c(10,22,1:9,23,27,28,11,12,25,26)]
fwrite(rosterTable.stats, 'data/rosterTable_stats.csv')
#Get PP Table
rosterTable.pp <- rosterTable[,c(1:5,11:14,28,37:40)]
names(rosterTable.pp) <- c("Name", "Position", "Team", "Birth.Date", "GP", "PPG", "PPA1", "PPA2","PPPTS", "Season","Team.PPG","Team.PPA1","Team.PPA2","Team.PPPTS")
#Get Draft Eligibility
rosterTable.pp$BD.Mod <- as.Date(rosterTable.pp$Birth.Date, "%Y-%m-%d")
rosterTable.pp$birth.year <- as.numeric(substr(rosterTable.pp$Birth.Date, 1, 4))
rosterTable.pp$draft.end <- as.Date(paste(rosterTable.pp$birth.year, 9, 15, sep = "-"), "%Y-%m-%d")
rosterTable.pp$draft.year <- ifelse(rosterTable.pp$BD.Mod > rosterTable.pp$draft.end, rosterTable.pp$birth.year + 19, rosterTable.pp$birth.year + 18)
rosterTable.pp$draft.season <- as.numeric(substr(rosterTable.pp$Season, 1, 4)) + 1
rosterTable.pp$Draft.Eligible <- ifelse(as.numeric(rosterTable.pp$draft.season) == as.numeric(rosterTable.pp$draft.year), "YES", "NO")
#Get PP Stats
rosterTable.pp$PRIMARY.PPPTS <- round(rosterTable.pp$PPG + rosterTable.pp$PPA1,digits=2)
rosterTable.pp$PERC.PPTS <- round(rosterTable.pp$PPPTS/rosterTable.stats$PTS,digits=2)
rosterTable.pp$PERC.TEAM.PPPTS <- round(rosterTable.pp$PPPTS/rosterTable.pp$Team.PPPTS,digits=2)
rosterTable.pp <- rosterTable.pp[,c(10,1:9,21,22,23,20)]
fwrite(rosterTable.pp, 'data/rosterTable_pp.csv')
#Get EV Table
rosterTable.ev <- rosterTable[,c(1:5,24:27,28,53,41:46)]
names(rosterTable.ev) <- c("Name", "Position", "Team", "Birth.Date", "GP", "EVG", "EVA1", "EVA2","EVPTS", "Season", "Team.EVGA", "Team.EVGF", "Team.EVA1", "Team.EVA2", "Team.EVPTS","OI.EVGF","OI.EVGA")
#Get Draft Eligibility
rosterTable.ev$BD.Mod <- as.Date(rosterTable.ev$Birth.Date, "%Y-%m-%d")
rosterTable.ev$birth.year <- as.numeric(substr(rosterTable.ev$Birth.Date, 1, 4))
rosterTable.ev$draft.end <- as.Date(paste(rosterTable.ev$birth.year, 9, 15, sep = "-"), "%Y-%m-%d")
rosterTable.ev$draft.year <- ifelse(rosterTable.ev$BD.Mod > rosterTable.ev$draft.end, rosterTable.ev$birth.year + 19, rosterTable.ev$birth.year + 18)
rosterTable.ev$draft.season <- as.numeric(substr(rosterTable.ev$Season, 1, 4)) + 1
rosterTable.ev$Draft.Eligible <- ifelse(as.numeric(rosterTable.ev$draft.season) == as.numeric(rosterTable.ev$draft.year), "YES", "NO")
#Get EV Stats
rosterTable.ev$EVGF.PERC <- round(rosterTable.ev$OI.EVGF/(rosterTable.ev$OI.EVGF + rosterTable.ev$OI.EVGA),digits=2)
rosterTable.ev$TEAM.EVGF.PERC <- round(rosterTable.ev$Team.EVGF/(rosterTable.ev$Team.EVGF + rosterTable.ev$Team.EVGA),digits=2)
rosterTable.ev$EVGF.PERC.REL <- round(rosterTable.ev$EVGF.PERC - rosterTable.ev$TEAM.EVGF.PERC,digits=2)
rosterTable.ev$PRIMARY.EVPTS <- round(rosterTable.ev$EVG + rosterTable.ev$EVA1,digits=2)
rosterTable.ev$TEAM.PRIMARY.EVPTS <- round(rosterTable.ev$Team.EVGF + rosterTable.ev$Team.EVA1,digits=2)
rosterTable.ev$PERC.TEAM.EVPrPTS <- round(rosterTable.ev$PRIMARY.EVPTS / rosterTable.ev$TEAM.PRIMARY.EVPTS,digits=2)
rosterTable.ev$IPP.EV <- round(rosterTable.ev$EVPTS/rosterTable.ev$OI.EVGF,digits=2)
rosterTable.ev$IPPP.EV <- round(rosterTable.ev$PRIMARY.EVPTS/rosterTable.ev$OI.EVGF,digits=2)
rosterTable.ev <- rosterTable.ev[,c(10,1:9,24:31,23)]
fwrite(rosterTable.ev, 'data/rosterTable_ev.csv')

#Get Filter Lists
team.names <- rosterTable$teamname
#team.names <- c(team.names, "All")
season.names <- rosterTable$season.name
#season.names <- c(season.names, "All")
draft.eligible <- rosterTable.stats$Draft.Eligible
#draft.eligible <- c(draft.eligible, "Both")
position.f <- rosterTable$pos.clean

