# Alabama Write In Votes from Secretary of State Recordsx

library(ggplot2)
library(maps)
library(plyr)
library(dplyr)

countyGPSCoords<-read.csv('./AlabamaCountyGPSCenters.txt',sep=',')
sosEnrExport<-read.csv('./sosEnrExport.csv',sep=',')

writeInVotes<-sosEnrExport %>%
  filter(Contest.Code==1000900 & Candidate.Name=='Write-In') %>%
  select(County.Name,Votes) %>%
  arrange(County.Name)

totalVoters<-sosEnrExport %>%
  filter(Contest.Code==1000900) %>%
  select(County.Name,Votes) %>%
  group_by(County.Name,add=FALSE) %>%
  summarize(TotalCountyVoters=sum(Votes)) %>%
  arrange(County.Name)

writeInVotesWithCoords<-left_join(writeInVotes,countyGPSCoords,by=c("County.Name"="County"))
writeInVotesWithCoordsAndTotalVoters<-left_join(writeInVotesWithCoords,totalVoters,by=c("County.Name"="County.Name"))

writeInVotesWithCoordsPct<-writeInVotesWithCoordsAndTotalVoters %>%
  mutate(PctWriteIns=(Votes/TotalCountyVoters*100))

ggplot(data=writeInVotesWithCoordsPct)+
  borders(database='county',region='alabama',colour='grey60',fill='grey90')+
  geom_point(aes(x=Long,y=Lat,size=PctWriteIns),color='blue',fill='blue')+
  scale_size_area(max_size=3)+
  ggtitle('Percentage of Write-In Votes by County in Alabama')+
  xlab('')+
  ylab('')+
  theme(
    panel.background=element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())  

ggplot(data=writeInVotesWithCoords)+
  borders(database='county',region='alabama',colour='grey60',fill='grey90')+
  geom_point(aes(x=Long,y=Lat,size=Votes),color='darkgreen',fill='darkgreen')+
  scale_size_area(max_size=8)+
  ggtitle('Count of Write-In Votes by County in Alabama')+
  xlab('')+
  ylab('')+
  theme(
    panel.background=element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())  



