library(readr)

wins        <- vector()
HR          <- vector()
HIT         <- vector()
AVG         <- vector()
R_score     <- vector()
OPS         <- vector()
SLG         <- vector()
wRAA        <- vector()
wOBA        <- vector()
WRC_plus    <- vector()
Bat_Clutch  <- vector()
Pit_clutch  <- vector()

P_DOLLAR        <- vector()
B_DOLLAR        <- vector()
wins_2002_2019  <- vector()
P_WAR           <- vector()
B_WAR           <- vector()
teamname_2019   <- vector()

for (i in 1995:2019){
    str1 <- "WDATA/"
    str2 <- ".txt"
    file_name <- paste(str1, i, str2, sep = "")

    if(i<=1997){
        for(j in 1:28){
            tmp <- read.table(file_name)
            wins <- c(wins,tmp[j,2])
        }
    } else {
        for(j in 1:30){
            tmp <- read.table(file_name)
            wins <- c(wins,tmp[j,2])
            if(i>=2002){
                wins_2002_2019 <- c(wins_2002_2019,tmp[j,2])
            }
            if(i==2019){
                teamname_2019  <- c(teamname_2019,tmp[j,1]) 
            }
        }
    }

    str1 <- "BValueDATA/"
    file_name <- paste(str1, i, str2, sep = "")
    tmp         <- read.table(file_name)

    if(i<=1997){
        for(j in 1:28){
            B_WAR       <- c(B_WAR,tmp[j,11])
        }
    } else {
        for(j in 1:30){
            B_WAR       <- c(B_WAR,tmp[j,11])
        }
        if(i>=2002){
            for(j in 1:30){
                B_DOLLAR    <- c(B_DOLLAR,tmp[j,12])
            }
        } 
    }


    str1 <- "PValueDATA/"
    file_name <- paste(str1, i, str2, sep = "")
    tmp         <- read.table(file_name)

    if(i<=1997){
        for(j in 1:28){
            P_WAR       <- c(P_WAR,tmp[j,7])
        }
    } else {
        for(j in 1:30){
            P_WAR       <- c(P_WAR,tmp[j,7])
        }
        if(i>=2002){
            for(j in 1:30){
                P_DOLLAR    <- c(P_DOLLAR,tmp[j,8])
            }
        } 
    }
}

# print(length(B_WAR))
# for(i in 1:744){
#     print(B_WAR[i])
# }

# print(max(P_DOLLAR))

# print(max(B_DOLLAR))

# ######################### Piter WAR #######################################3

png(file = "Pitcher_WAR-WINS.png")
plot(P_WAR,wins,
     main="Pitcher_WAR-WINS 1995-2019",
     ylab = "WINS",
     xlab = "Pitcher_WAR",
     )
abline(lm(wins~P_WAR),col="red")
legend("bottomright",legend=c("WINS = 1.57702AVG+58.12735",expression('r'^2~"= 0.4815")),cex=0.9)
dev.off()

summary(lm(wins~P_WAR))
cor.test(P_WAR,wins)

png(file = "Pitcher_WAR-WINS-qqplot.png")
qqnorm(P_WAR,col="blue",ylab="Pitcher WAR",pch=0)
qqline(P_WAR,col="red")
dev.off()

png(file = "Pitcher_WAR-WINS-res.png")
P_WAR_data <- data.frame(x=P_WAR,y=wins)
res <- residuals(lm(wins~P_WAR),data=P_WAR_data)
plot(P_WAR,res,ylim=c(-50,50))
dev.off()

png(file = "Pitcher_WAR-WINS-res-qqplot.png")
qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")
dev.off()

require(car)
ncvTest(lm(wins~P_WAR))

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

# ######################### Hitter WAR #######################################3

png(file = "Hitter_WAR-WINS.png")
plot(B_WAR,wins,
     main="Hitter_WAR-WINS 1995-2019",
     ylab = "WINS",
     xlab = "Hitter_WAR",
     )
abline(lm(wins~B_WAR),col="red")
legend("bottomright",legend=c("WINS = 1.1333AVG+59.1858",expression('r'^2~"= 0.5746")),cex=0.9)
dev.off()

summary(lm(wins~B_WAR))
cor.test(B_WAR,wins)

png(file = "Hitter_WAR-WINS-qqplot.png")
qqnorm(B_WAR,col="blue",ylab="Hitter WAR",pch=0)
qqline(B_WAR,col="red")
dev.off()

png(file = "Hitter_WAR-WINS-res.png")
B_WAR_data <- data.frame(x=B_WAR,y=wins)
res <- residuals(lm(wins~B_WAR),data=B_WAR_data)
plot(B_WAR,res,ylim=c(-50,50))
dev.off()

png(file = "Hitter_WAR-WINS-res-qqplot.png")
qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")
dev.off()

require(car)
ncvTest(lm(wins~B_WAR))

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

# ######################### Piter+Hitter WAR #######################################3


total_war <- vector()
total_war <- B_WAR+P_WAR

png(file = "WAR-WINS.png")
plot(total_war,wins,
     main="WAR-WINS 1995-2019",
     ylab = "WINS",
     xlab = "Hitter_WAR",
     )
abline(lm(wins~total_war),col="red")
legend("bottomright",legend=c("WINS = 0.97980AVG+48.11124",expression('r'^2~"= 0.7959")),cex=0.9)
dev.off()

summary(lm(wins~total_war))
cor.test(total_war,wins)

png(file = "WAR-WINS-qqplot.png")
qqnorm(total_war,col="blue",ylab="WAR",pch=0)
qqline(total_war,col="red")
dev.off()

png(file = "WAR-WINS-res.png")
war_data <- data.frame(x=total_war,y=wins)
res <- residuals(lm(wins~total_war),data=war_data)
plot(total_war,res,ylim=c(-50,50))
dev.off()

png(file = "WAR-WINS-res-qqplot.png")
qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")
dev.off()

require(car)
ncvTest(lm(wins~total_war))

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

WAR_2019        <- vector()
Sorted_WAR_2019 <- vector()

for(i in 715:744){
    # print(total_war[i])
    WAR_2019 <- c(WAR_2019,total_war[i])
}

# for(i in 1:30){
#     print((teamname_2019[i]))
# }

png(file = "WAR.png")
barplot(WAR_2019,names.arg=teamname_2019,main="Team's WAR in 2019",horiz=TRUE,las=1,cex.names=0.8)
dev.off()

Sorted_WAR_2019 <- WAR_2019

Sorted_WAR_2019 <- sort(WAR_2019)
Sorted_team_name_2019 <- vector()



for(i in 1:30){
    for(j in 1:30){
        if(Sorted_WAR_2019[i] == WAR_2019[j]){
            len  <- length(Sorted_team_name_2019)
            flag <- 0;
            
            if(len>0){
                for(k in 1:len){
                    if(teamname_2019[j] == Sorted_team_name_2019[k]){
                        flag <- 1;
                    }
                }
            }
            
            if(flag!=1){
                Sorted_team_name_2019 <- c(Sorted_team_name_2019,teamname_2019[j])
                break
            }
        }
    }
}

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

# for(i in 1:30){
#     print(WAR_2019[i])
# }

# for(i in 1:30){
#     cat(Sorted_WAR_2019[i],Sorted_team_name_2019[i],"\n")
# }

# png(file = "Sorted WAR.png")
# barplot(Sorted_WAR_2019,names.arg=Sorted_team_name_2019,main="Team's WAR in 2019",horiz=TRUE,las=1,cex.names=0.8)
# dev.off()

# wins_2008 <- vector()
# # for (i in 385:414){
# #     print(wins[i])
# # }

# i = 385
# while(i<=414){
#     j   <- i;
#     tmp <- vector()
#     while(j<=744){
#         tmp <- c(tmp,wins[j])
#         j <- j + 30
#     }
#     if(i==385){
#         plot(c(1995:2019),wins_2008)
#     } else {
#         lines(c(1995:2019), tmp, col = "blue")
#     }
# }

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

png(file = "r.png")
r_list <- c(0.6702741,0.6587364,0.5535135,0.5226805,0.5089214,0.5480306,
0.5147098,0.6011057,0.67184,0.7580179,0.6939018,0.892134)

factor_list <- c("ERA","ER","H","WHIP","XFIP","R","OPS","WRAA","WRC+","Hitter_WAR","Pitcher_WAR","Total_WAR")
barplot(r_list,names.arg=factor_list,horiz=TRUE,las=1,cex.names=0.8,xlim=c(0.4,1))
dev.off()

print(factor_list)