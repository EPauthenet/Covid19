#Plot of the covid-19 deaths by country from the ourworldindata dataset - April 2020 E.Pauthenet
library(ggplot2)
library(reshape2)
library(directlabels)
library(gridExtra)
library(grid)

#Load data
url = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
destfile = "~/Documents/R/CoronaVirus/Ourworldindata/owid-covid-data.csv"
download.file(url,destfile)
df = read.csv2(destfile,header = T,sep = ",")
df$date = as.Date(df$date)
df$location = as.factor(df$location)
levels(df$location)[levels(df$location)=="United States"] <- "US"
levels(df$location)[levels(df$location)=="United Kingdom"] <- "UK"
#
#Define different zooms
n1 = 10000 ; n2 = 3000 ; n3 = 1000
deaths_max = by(df$total_deaths,df$location,max)
df1 = df[df$location %in% names(which(deaths_max>n1)),]
df2 = df[df$location %in% names(which(deaths_max<=n1 & deaths_max>n2)),]
df3 = df[df$location %in% names(which(deaths_max<=n2 & deaths_max>n3)),]
df4 = df[df$location %in% names(which(deaths_max<=n3)),] 

#Define different zooms
df_rect=data.frame(x1 = as.Date(c("2020-03-25","2020-03-31"))
  ,x2 = c(as.Date(c("2020-05-20","2020-05-20")))
  ,y1 = c(0,0)
  ,y2 = c(n1,n2))

pal = c("firebrick","orange","steelblue","grey")
p1 = ggplot() +
  labs(title = paste("Covid-19 deaths by countries (",df$date[length(df$date)],")",sep = ""), subtitle = "source : https://covid.ourworldindata.org / Graphic : E. Pauthenet") +
  xlab("") + ylab("deaths") + ylim(0,50000) + xlim(as.Date(c("2020-01-25",df_rect$x2[2]))) + 
  theme_minimal() + theme(legend.position = "none") +
  geom_line(data = df4,aes(x = date, y = total_deaths, group = location),colour = pal[4]) +
  geom_line(data = df3,aes(x = date, y = total_deaths, group = location),colour = pal[3]) +
  geom_line(data = df2,aes(x = date, y = total_deaths, group = location),colour = pal[2]) +
  geom_line(data = df1,aes(x = date, y = total_deaths, group = location),colour = pal[1]) +
  geom_dl(data = df1,aes(x = date, y = total_deaths,label = location),colour = pal[1], method = list(dl.trans(x = x + .2),"last.bumpup")) +
  geom_rect(aes(xmin=x1, xmax=x2,ymin=y1, ymax=y2),alpha = 0, color=c("orange","steelblue"),data = df_rect,lty = 3)
print(p1)
#
p2 = ggplot() +
  xlab("") + ylab("") + ylim(df_rect$y1[1],df_rect$y2[1]) + xlim(c(df_rect$x1[1],df_rect$x2[2])) + 
  theme_minimal() + theme(legend.position = "none") +
  geom_line(data = df4,aes(x = date, y = total_deaths, group = location),colour = pal[4]) +
  geom_line(data = df3,aes(x = date, y = total_deaths, group = location),colour = pal[3]) +
  geom_line(data = df2,aes(x = date, y = total_deaths, group = location),colour = pal[2]) +
  geom_dl(data = df2,aes(x = date, y = total_deaths,label = location),colour = pal[2], method = list(dl.trans(x = x + .2),"last.bumpup")) +
  geom_rect(aes(xmin=x1, xmax=x2,ymin=y1, ymax=y2),alpha = 0, color=pal[c(2,3)],data = df_rect,lty = 3)
print(p2)
#
p3 = ggplot() +
  xlab("") + ylab("") + ylim(df_rect$y1[2],df_rect$y2[2]) + xlim(c(df_rect$x1[2],df_rect$x2[2])) + 
  theme_minimal() + theme(legend.position = "none") +
  geom_line(data = df4,aes(x = date, y = total_deaths, group = location),colour = pal[4]) +
  geom_line(data = df3,aes(x = date, y = total_deaths, group = location),colour = pal[3]) +
  geom_dl(data = df3,aes(x = date, y = total_deaths,label = location),colour = pal[3], method = list(dl.trans(x = x + .2),"last.bumpup")) +
  geom_rect(aes(xmin=x1, xmax=x2,ymin=y1, ymax=y2),alpha = 0, color=pal[c(2,3)],data = df_rect,lty = 3)
print(p3)
p4 = grid.arrange(p1,p2,p3,ncol=3, nrow=1, widths=c(4, 2, 2), heights=c(4))
ggsave(plot=p4, filename=paste("Countries_zoom_",df1$date[dim(df1)[1]],".png",sep = ""), height=6, width=12)

