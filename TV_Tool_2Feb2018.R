install.packages("RODBC")
library(RODBC)

install.packages("sqldf")
library(sqldf)

# Set working directory
setwd("F:\\TV_Planning_Forecast")

getwd()

Demo_Forecast <- read.csv('TV_Tool_Input.csv') # Connects to Datamart #
#Demo_Forecastdb <- sqlFetch(Demo_Forecast,"TV Tool Data", as.is = TRUE) # Gets SQL Table #

# Fetch data based on some conditions
Demo_MainSet<-sqldf("SELECT [Source],[Date]
                   ,DayPart
                   ,[Month],[Year],[WeekNum],[Time],[Hour]
                   ,[Minute],UPPER([Station]) AS [Station]
                   ,[Length]
                   ,(CASE WHEN [TrueSpend] IS 'NA' THEN 0 ELSE [TrueSpend] END) AS [TrueSpend]
                   ,(CASE WHEN [TrueCalls] IS 'NA' THEN 0 ELSE [TrueCalls] END) AS [TrueCalls]
                   ,(CASE WHEN [TrueLeads] IS 'NA' THEN 0 ELSE [TrueLeads] END) AS [TrueLeads]
                   ,[SpotType]
                   ,[Network]
                   ,(CASE WHEN [TrueSpots] IS 'NA' THEN 0 ELSE [TrueSpots] END) AS [TrueSpots]
                   ,[GRP]
                   FROM Demo_Forecast
                   WHERE [Length] IN ('30','60')")


# Fetch planning data of upcoming month
Demo_PlanningData<-read.csv('TV plan for TV Stations.csv',header = TRUE,sep=",")

Demo_PlanningData<-sqldf("select Month,UPPER(Network) AS Network,
                        (CASE WHEN [Unit.Length]=':30' THEN 30
                        WHEN [Unit.Length]=':60' THEN 60 END) as UnitLength,
                        Daypart,
                        UPPER([Spot.Type]) as SpotType
                        ,UPPER(Type) AS Type,CAST(Cost as int) as Cost,Spot,[Cost.per.Spot] as CostPerSpot
                        from Demo_PlanningData WHERE [Cost.per.Spot] !='NA'
                        order by Network")


Demo_Historical_List<-sqldf("SELECT DISTINCT [Station],[Length],DayPart,
                           UPPER((CASE WHEN [SpotType]='' THEN 'NATIONAL' ELSE [SpotType] END)) AS [Spot Type],
                           UPPER((CASE WHEN [Source]='Core' THEN 'SYNDICATION'
                           WHEN [Source]='Adalyser' THEN 'CABLE' ELSE [Source] END)) AS [Source] 
                           FROM Demo_MainSet
                           ORDER BY [Station]")


# This gives the list of channels which the client is planning to buy and invest on
Demo_PlanningData_NotHistorical<-sqldf("SELECT * FROM Demo_PlanningData A LEFT OUTER JOIN Demo_Historical_List B
                                      ON Network=B.[Station] AND UnitLength=[Length] AND A.Daypart=B.DayPart
                                      AND SpotType=[SpotType] AND Type=[Source]
                                      WHERE B.[Station] IS NULL")



#Adalyser Data National

Demo_MainSet_Adalyser<-sqldf("SELECT * FROM Demo_MainSet
                            WHERE [Network]='Cable' AND [Source]='Adalyser' AND [SpotType]='NATIONAL'")


# str(AntennaTV) TO GET DATATYPE #


#plot(AntennaTV$`True Calls`, AntennaTV$`True Spots`)


V1<-sqldf("SELECT DISTINCT [Station] FROM Demo_MainSet_Adalyser
          ORDER BY [Station]")

# Converting to data frames and removing factor levels
indx <- sapply(V1, is.factor)
V1[indx] <- lapply(V1[indx], function(x) as.character(x))

V2<-sqldf("SELECT DISTINCT [Station],DayPart FROM Demo_MainSet_Adalyser
          ORDER BY [Station],DayPart")

indx <- sapply(V2, is.factor)
V2[indx] <- lapply(V2[indx], function(x) as.character(x))


j<-1

ArrayRowNum<-nrow(V1)+nrow(V2)+nrow(Demo_PlanningData_NotHistorical)

Demo_OutputArray<-array("",dim=c(ArrayRowNum,32,1))

colnames(Demo_OutputArray)<-c("MonthName","Network","Daypart","SpotType","Length","Coefficient","Intercept","Pvalue","Rsquare"
                             ,"AdjustedRsquare","Cost","Spot","Contact","Lead"
                             ,"ContactPerSpot","CostPerSpot","CostPerContact"
                             ,"CostPerLead","MaxCostPerSpot_BasedOnCPC"
                             ,"MaxCostPerSpot_BasedOnCPL","Max # Spot(MarginalContactLimitof1)"
                             ,"Max # Spot(CostPerContact)"
                             ,"Max # Spot(CostPerLead)"
                             ,"CostPerCall","CostPerLead","Cost","NoOfSpots","CostPerSpot","Guide","Historical CVR"
                             ,"Planned Contacts","Planned Leads")


#For loop to run the code for each network

for(i in 1:nrow(V1))
{
  Demo_FetchStationData_Adalyser<-subset(Demo_MainSet_Adalyser,Demo_MainSet_Adalyser$Station==V1$Station[i]
                                        ,SELECT=Demo_MainSet_Adalyser(Demo_MainSet_Adalyser$`Week Num`
                                                                     ,Demo_MainSet_Adalyser$`True Calls`,Demo_MainSet_Adalyser$`True Spots`))
  
  
  Demo_AdalyserSummary<-sqldf("SELECT [WeekNum],SUM([TrueCalls]) AS TrueCalls
                             ,(CASE WHEN SUM([TrueSpots]) IS 0 THEN 1 ELSE SUM([TrueSpots]) END) AS TrueSpots
                             ,SUM([TrueLeads]) AS TrueLeads
                             ,SUM([TrueSpend]) AS TrueSpend
                             FROM Demo_FetchStationData_Adalyser
                             GROUP BY [WeekNum]")
  
  
  # Regression Equation
  Demo_Station_Regression<-lm(Demo_AdalyserSummary$TrueCalls~log(Demo_AdalyserSummary$TrueSpots),data=Demo_AdalyserSummary)
  
  res <- summary(Demo_Station_Regression)
  co <- res$coefficients
  
  Demo_Coefficients<-array(co,dim=c(1,8,1))
  

  
  Demo_AdalyserNationalUnitLength<-sqldf("SELECT SUM([TrueCalls]) AS TrueCalls,SUM([TrueSpots]) AS TrueSpots,
                                        SUM([TrueLeads]) AS TrueLeads
                                        ,(SUM([TrueLeads])/SUM([TrueCalls])) AS ContactToLead
                                        ,(SUM([TrueLeads])/SUM([TrueSpots])) AS LeadperSpot
                                        FROM Demo_FetchStationData_Adalyser")
  
  
  indx <- sapply(Demo_PlanningData, is.factor)
  Demo_PlanningData[indx] <- lapply(Demo_PlanningData[indx], function(x) as.character(x))
  
  
  Demo_Station_PlanningData<-data.frame(subset(Demo_PlanningData,Demo_PlanningData$Network==V1$Station[i]
                                              & SpotType=='NATIONAL'
                                              & Type=='CABLE'
                                              ,SELECT=Demo_PlanningData))
  
  if(nrow(Demo_Station_PlanningData)!=0)
    
  {
    
    
    Demo_Station_PlanningData<-sqldf("SELECT Month,UnitLength,SUM(Cost) AS Cost,SUM(Spot) AS Spot,
                                    (SUM(Cost)/SUM(Spot)) AS CostPerSpot
                                    FROM Demo_Station_PlanningData")
    
    l<-1
    
    Demo_MaxNoSpot_Calculation<-array("",dim=c(4000,8,1))
    
    colnames(Demo_MaxNoSpot_Calculation)<-c("Spots","Contacts","Marginal Calls",
                                           "Avg Calls","Leads","Avg Leads","Cost per Call",
                                           "Cost per Lead")
    
    CostPerSpotUnitLength<-sqldf("SELECT CostPerSpot FROM Demo_Station_PlanningData")
    
    # Code to find the maximum spots client should buy on each network
    for(l in 1:nrow(Demo_MaxNoSpot_Calculation))
    {
      Demo_MaxNoSpot_Calculation[l,1,1]<-l
      Demo_MaxNoSpot_Calculation[l,2,1]<-Demo_Coefficients[1,1,1]+(Demo_Coefficients[1,2,1]*log(as.numeric(Demo_MaxNoSpot_Calculation[l,1,1])))
      
      v<-as.numeric(Demo_MaxNoSpot_Calculation[l,2,1])-as.numeric(Demo_MaxNoSpot_Calculation[(l-1),2,1])
      
      if(length(v)!=0)
      {
        Demo_MaxNoSpot_Calculation[l,3,1]<-v
      }
      
      Demo_MaxNoSpot_Calculation[l,4,1]<-as.numeric(Demo_MaxNoSpot_Calculation[l,2,1])/as.numeric(Demo_MaxNoSpot_Calculation[l,1,1])
      
      Demo_MaxNoSpot_Calculation[l,5,1]<-as.numeric(Demo_AdalyserNationalUnitLength$ContactToLead)*as.numeric(Demo_MaxNoSpot_Calculation[l,2,1])
      
      Demo_MaxNoSpot_Calculation[l,6,1]<-as.numeric(Demo_MaxNoSpot_Calculation[l,5,1])/as.numeric(Demo_MaxNoSpot_Calculation[l,1,1])
      
      Demo_MaxNoSpot_Calculation[l,7,1]<-as.numeric(CostPerSpotUnitLength)/as.numeric(Demo_MaxNoSpot_Calculation[l,4,1])
      
      Demo_MaxNoSpot_Calculation[l,8,1]<-as.numeric(CostPerSpotUnitLength)/as.numeric(Demo_MaxNoSpot_Calculation[l,6,1])
      
      l<-l+1
      
    }
    
    Demo_MaxNoSpot_Calculation<-data.frame(Demo_MaxNoSpot_Calculation)
    
    indx <- sapply(Demo_MaxNoSpot_Calculation, is.factor)
    Demo_MaxNoSpot_Calculation[indx] <- lapply(Demo_MaxNoSpot_Calculation[indx], function(x) as.numeric(as.character(x)))
    
    
    indx <- sapply(Demo_Station_PlanningData, is.factor)
    Demo_Station_PlanningData[indx] <- lapply(Demo_Station_PlanningData[indx], function(x) as.character(x))
    
    Demo_OutputArray[j,1,1]<-Demo_Station_PlanningData$Month
    
    Demo_OutputArray[j,2,1]<-V1$Station[i]
    
    Demo_OutputArray[j,3,1]<-"All of Them"
    
    Demo_OutputArray[j,4,1]<-"National"
    
    Demo_OutputArray[j,5,1]<-'All'
    
    Demo_OutputArray[j,6,1]<-format(Demo_Coefficients[1,2,1], scientific=FALSE)
    
    Demo_OutputArray[j,7,1]<-format(Demo_Coefficients[1,1,1], scientific=FALSE)
    
    Demo_OutputArray[j,8,1]<-format(Demo_Coefficients[1,8,1], scientific=FALSE)
    
    Demo_OutputArray[j,9,1]<-format(res$r.squared, scientific=FALSE)
    
    Demo_OutputArray[j,10,1]<-format(res$adj.r.squared, scientific=FALSE)
    
    Demo_OutputArray[j,11,1]<-sum(Demo_AdalyserSummary$TrueSpend)
    
    Demo_OutputArray[j,12,1]<-sum(Demo_AdalyserSummary$TrueSpots)
    
    Demo_OutputArray[j,13,1]<-sum(Demo_AdalyserSummary$TrueCalls)
    
    Demo_OutputArray[j,14,1]<-sum(Demo_AdalyserSummary$TrueLeads)
    
    Demo_OutputArray[j,15,1]<-sum(Demo_AdalyserSummary$TrueCalls)/sum(Demo_AdalyserSummary$TrueSpots)
    
    Demo_OutputArray[j,16,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueSpots)
    
    Demo_OutputArray[j,17,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueCalls)
    
    Demo_OutputArray[j,18,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueLeads)
    
    Demo_OutputArray[j,19,1]<-as.numeric(Demo_OutputArray[j,15,1])*80
    
    Demo_OutputArray[j,20,1]<-(Demo_AdalyserNationalUnitLength$LeadperSpot)*5300
    
    Demo_OutputArray[j,21,1]<-as.numeric(sqldf("SELECT (CASE WHEN [Spots.1]=1 THEN 'NA' ELSE [Spots.1] END) AS [Spots.1]
                                              FROM Demo_MaxNoSpot_Calculation WHERE [Marginal.Calls.1]=
                                              (SELECT MIN([Marginal.Calls.1]) FROM Demo_MaxNoSpot_Calculation 
                                              WHERE [Marginal.Calls.1]>=1 
                                              ORDER BY [Marginal.Calls.1])"))
    
    
    Demo_OutputArray[j,22,1]<-as.numeric(sqldf("SELECT (CASE WHEN [Spots.1]=1 THEN 'NA' ELSE [Spots.1] END) AS [Spots.1]
                                              FROM Demo_MaxNoSpot_Calculation WHERE [Cost.per.Call.1]=
                                              (SELECT MAX([Cost.per.Call.1]) FROM Demo_MaxNoSpot_Calculation 
                                              WHERE [Cost.per.Call.1]<=80 
                                              ORDER BY [Cost.per.Call.1])"))
    
    
    Demo_OutputArray[j,23,1]<-as.numeric(sqldf("SELECT (CASE WHEN [Spots.1]=1 THEN 'NA' ELSE [Spots.1] END) AS [Spots.1]
                                              FROM Demo_MaxNoSpot_Calculation WHERE [Cost.per.Lead.1]=
                                              (SELECT MAX([Cost.per.Lead.1]) FROM Demo_MaxNoSpot_Calculation 
                                              WHERE [Cost.per.Lead.1] <=5300
                                              ORDER BY [Cost.per.Lead.1])"))
    
    
  
    
    Demo_OutputArray[j,30,1]<-as.numeric(Demo_AdalyserNationalUnitLength$TrueLeads/Demo_AdalyserNationalUnitLength$TrueCalls)
    
    
    Demo_OutputArray[j,31,1]<-as.numeric(sqldf("SELECT [Contacts.1] FROM Demo_MaxNoSpot_Calculation 
                                              WHERE [Spots.1]=(SELECT Spot FROM Demo_Station_PlanningData)"))
    
    Demo_OutputArray[j,32,1]<-as.numeric(Demo_OutputArray[j,30,1])*as.numeric(Demo_OutputArray[j,31,1])
    
    Demo_OutputArray[j,24,1]<-(as.numeric(Demo_Station_PlanningData$Cost))/(as.numeric(Demo_OutputArray[j,31,1]))
    
    Demo_OutputArray[j,25,1]<-(as.numeric(Demo_Station_PlanningData$Cost))/(as.numeric(Demo_OutputArray[j,32,1]))
    
    Demo_OutputArray[j,26,1]<-Demo_Station_PlanningData$Cost
    
    Demo_OutputArray[j,27,1]<-Demo_Station_PlanningData$Spot
    
    Demo_OutputArray[j,28,1]<-Demo_Station_PlanningData$CostPerSpot
    
    if((Demo_OutputArray[j,8,1]>0.05 && Demo_OutputArray[j,9,1]<0.65) || (Demo_OutputArray[j,8,1]=='NaN') || (Demo_OutputArray[j,8,1]==1))
    {
      Demo_OutputArray[j,29,1]<-"Do not use"
      
    }else if(Demo_OutputArray[j,8,1]>0.05 || Demo_OutputArray[j,9,1]<0.65)
    {
      Demo_OutputArray[j,29,1]<-"Directional"
      
    }else
    {
      Demo_OutputArray[j,29,1]<-"Good"
    }
    
    
  }  else
  {
    Demo_OutputArray[j,1,1]<-0
    
    Demo_OutputArray[j,2,1]<-V1$Station[i]
    
    Demo_OutputArray[j,3,1]<-"All of Them"
    
    Demo_OutputArray[j,4,1]<-"National"
    
    Demo_OutputArray[j,5,1]<-'All'
    
    Demo_OutputArray[j,6,1]<-format(Demo_Coefficients[1,2,1], scientific=FALSE)
    
    Demo_OutputArray[j,7,1]<-format(Demo_Coefficients[1,1,1], scientific=FALSE)
    
    Demo_OutputArray[j,8,1]<-format(Demo_Coefficients[1,8,1], scientific=FALSE)
    
    Demo_OutputArray[j,9,1]<-format(res$r.squared, scientific=FALSE)
    
    Demo_OutputArray[j,10,1]<-format(res$adj.r.squared, scientific=FALSE)
    
    Demo_OutputArray[j,11,1]<-sum(Demo_AdalyserSummary$TrueSpend)
    
    Demo_OutputArray[j,12,1]<-sum(Demo_AdalyserSummary$TrueSpots)
    
    Demo_OutputArray[j,13,1]<-sum(Demo_AdalyserSummary$TrueCalls)
    
    Demo_OutputArray[j,14,1]<-sum(Demo_AdalyserSummary$TrueLeads)
    
    Demo_OutputArray[j,15,1]<-sum(Demo_AdalyserSummary$TrueCalls)/sum(Demo_AdalyserSummary$TrueSpots)
    
    Demo_OutputArray[j,16,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueSpots)
    
    Demo_OutputArray[j,17,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueCalls)
    
    Demo_OutputArray[j,18,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueLeads)
    
    Demo_OutputArray[j,19,1]<-as.numeric(Demo_OutputArray[j,15,1])*80
    
    Demo_OutputArray[j,20,1]<-(Demo_AdalyserNationalUnitLength$LeadperSpot)*5300
    
    
    Demo_OutputArray[j,21,1]<-as.numeric(0)
    
    
    Demo_OutputArray[j,22,1]<-as.numeric(0)
    
    
    Demo_OutputArray[j,23,1]<-as.numeric(0)
    
    Demo_OutputArray[j,24,1]<-as.numeric(0)
    
    Demo_OutputArray[j,25,1]<-as.numeric(0)
    
    
    Demo_OutputArray[j,26,1]<-as.numeric(0)
    
    
    Demo_OutputArray[j,27,1]<-as.numeric(0)
    
    Demo_OutputArray[j,28,1]<-as.numeric(0)
    
    if((Demo_OutputArray[j,8,1]>0.05 && Demo_OutputArray[j,9,1]<0.65) || (Demo_OutputArray[j,8,1]=='NaN') || (Demo_OutputArray[j,8,1]==1))
    {
      Demo_OutputArray[j,29,1]<-"Do not use"
      
    }else if(Demo_OutputArray[j,8,1]>0.05 || Demo_OutputArray[j,9,1]<0.65)
    {
      Demo_OutputArray[j,29,1]<-"Directional"
      
    }else
    {
      Demo_OutputArray[j,29,1]<-"Good"
    }
    
    
    Demo_OutputArray[j,30,1]<-as.numeric(0)
    Demo_OutputArray[j,31,1]<-as.numeric(0)
    Demo_OutputArray[j,32,1]<-as.numeric(0)
  }
  
  print(V1$Station[i])
  
  j<-j+1
  i<-i+1
}



# For loop to run the code for each network and DayPart. Same process repeats.

for(i in 1:nrow(V1))
{
  Demo_FetchStationData_Adalyser<-subset(Demo_MainSet_Adalyser,Demo_MainSet_Adalyser$Station==V1$Station[i]
                                         ,SELECT=Demo_MainSet_Adalyser(Demo_MainSet_Adalyser$`Week Num`
                                                                       ,Demo_MainSet_Adalyser$`True Calls`,Demo_MainSet_Adalyser$`True Spots`))
  
  
  d1<-sqldf("SELECT DISTINCT DayPart FROM Demo_FetchStationData_Adalyser
            ORDER BY DayPart")
  
  indx <- sapply(d1, is.factor)
  d1[indx] <- lapply(d1[indx], function(x) as.character(x))
  
  
  for(k in 1:nrow(d1))
  {
    Demo_FetchStationDayPart_Adalyser<-subset(Demo_FetchStationData_Adalyser,Demo_FetchStationData_Adalyser$DayPart==d1$DayPart[k]
                                             ,SELECT=Demo_FetchStationData_Adalyser(Demo_FetchStationData_Adalyser$`Week Num`
                                                                                   ,Demo_FetchStationData_Adalyser$`True Calls`,Demo_FetchStationData_Adalyser$`True Spots`))
    
    Demo_AdalyserSummary<-sqldf("SELECT DayPart,[WeekNum],SUM([TrueCalls]) AS TrueCalls
                               ,(CASE WHEN SUM([TrueSpots]) IS 0 THEN 1 ELSE SUM([TrueSpots]) END) AS TrueSpots
                               ,SUM([TrueLeads]) AS TrueLeads
                               ,SUM([TrueSpend]) AS TrueSpend
                               FROM Demo_FetchStationDayPart_Adalyser
                               GROUP BY [WeekNum],DayPart")
    
    Demo_Station_Regression<-lm(Demo_AdalyserSummary$TrueCalls~log(Demo_AdalyserSummary$TrueSpots),data=Demo_AdalyserSummary)
    
    res <- summary(Demo_Station_Regression)
    co <- res$coefficients
    
    Demo_Coefficients<-array(co,dim=c(1,8,1))
    
    Demo_AdalyserDayPartNationalUnitLength<-sqldf("SELECT SUM([TrueCalls]) AS TrueCalls,SUM([TrueSpots]) AS TrueSpots,
                                                 SUM([TrueLeads]) AS TrueLeads
                                                 ,(SUM([TrueLeads])/SUM([TrueCalls])) AS ContactToLead
                                                 ,(SUM([TrueLeads])/SUM([TrueSpots])) AS LeadperSpot
                                                 FROM Demo_FetchStationDayPart_Adalyser")
    
    
    Demo_Station_PlanningData<-data.frame(subset(Demo_PlanningData,Demo_PlanningData$Network==V1$Station[i]
                                                & SpotType=='NATIONAL'
                                                & Type=='CABLE'
                                                & Daypart==d1$DayPart[k]
                                                ,SELECT=Demo_PlanningData))
    
    if(nrow(Demo_Station_PlanningData)!=0)
    {
      Demo_Station_PlanningData<-sqldf("SELECT Month,UnitLength,SUM(Cost) AS Cost,SUM(Spot) AS Spot,
                                      (SUM(Cost)/SUM(Spot)) AS CostPerSpot
                                      FROM Demo_Station_PlanningData")
      
      l<-1
      
      Demo_MaxNoSpot_Calculation<-array("",dim=c(4000,8,1))
      
      colnames(Demo_MaxNoSpot_Calculation)<-c("Spots","Contacts","Marginal Calls",
                                             "Avg Calls","Leads","Avg Leads","Cost per Call",
                                             "Cost per Lead")
      
      CostPerSpotUnitLength<-sqldf("SELECT CostPerSpot FROM Demo_Station_PlanningData")
      
      
      for(l in 1:nrow(Demo_MaxNoSpot_Calculation))
      {
        Demo_MaxNoSpot_Calculation[l,1,1]<-l
        Demo_MaxNoSpot_Calculation[l,2,1]<-Demo_Coefficients[1,1,1]+(Demo_Coefficients[1,2,1]*log(as.numeric(Demo_MaxNoSpot_Calculation[l,1,1])))
        
        v<-as.numeric(Demo_MaxNoSpot_Calculation[l,2,1])-as.numeric(Demo_MaxNoSpot_Calculation[(l-1),2,1])
        
        if(length(v)!=0)
        {
          Demo_MaxNoSpot_Calculation[l,3,1]<-v
        }
        
        Demo_MaxNoSpot_Calculation[l,4,1]<-as.numeric(Demo_MaxNoSpot_Calculation[l,2,1])/as.numeric(Demo_MaxNoSpot_Calculation[l,1,1])
        
        Demo_MaxNoSpot_Calculation[l,5,1]<-as.numeric(Demo_AdalyserDayPartNationalUnitLength$ContactToLead)*as.numeric(Demo_MaxNoSpot_Calculation[l,2,1])
        
        Demo_MaxNoSpot_Calculation[l,6,1]<-as.numeric(Demo_MaxNoSpot_Calculation[l,5,1])/as.numeric(Demo_MaxNoSpot_Calculation[l,1,1])
        
        Demo_MaxNoSpot_Calculation[l,7,1]<-as.numeric(CostPerSpotUnitLength)/as.numeric(Demo_MaxNoSpot_Calculation[l,4,1])
        
        Demo_MaxNoSpot_Calculation[l,8,1]<-as.numeric(CostPerSpotUnitLength)/as.numeric(Demo_MaxNoSpot_Calculation[l,6,1])
        
        l<-l+1
        
      }
      
      Demo_MaxNoSpot_Calculation<-data.frame(Demo_MaxNoSpot_Calculation)
      
      indx <- sapply(Demo_MaxNoSpot_Calculation, is.factor)
      Demo_MaxNoSpot_Calculation[indx] <- lapply(Demo_MaxNoSpot_Calculation[indx], function(x) as.numeric(as.character(x)))
      
      indx <- sapply(Demo_Station_PlanningData, is.factor)
      Demo_Station_PlanningData[indx] <- lapply(Demo_Station_PlanningData[indx], function(x) as.character(x))
      
      Demo_OutputArray[j,1,1]<-Demo_Station_PlanningData$Month
      
      Demo_OutputArray[j,2,1]<-V1$Station[i]
      
      Demo_OutputArray[j,3,1]<-d1$DayPart[k]
      
      Demo_OutputArray[j,4,1]<-"National"
      
      Demo_OutputArray[j,5,1]<-'All'
      
      Demo_OutputArray[j,6,1]<-format(Demo_Coefficients[1,2,1], scientific=FALSE)
      
      Demo_OutputArray[j,7,1]<-format(Demo_Coefficients[1,1,1], scientific=FALSE)
      
      Demo_OutputArray[j,8,1]<-format(Demo_Coefficients[1,8,1], scientific=FALSE)
      
      Demo_OutputArray[j,9,1]<-format(res$r.squared, scientific=FALSE)
      
      Demo_OutputArray[j,10,1]<-format(res$adj.r.squared, scientific=FALSE)
      
      Demo_OutputArray[j,11,1]<-sum(Demo_AdalyserSummary$TrueSpend)
      
      Demo_OutputArray[j,12,1]<-sum(Demo_AdalyserSummary$TrueSpots)
      
      Demo_OutputArray[j,13,1]<-sum(Demo_AdalyserSummary$TrueCalls)
      
      Demo_OutputArray[j,14,1]<-sum(Demo_AdalyserSummary$TrueLeads)
      
      Demo_OutputArray[j,15,1]<-sum(Demo_AdalyserSummary$TrueCalls)/sum(Demo_AdalyserSummary$TrueSpots)
      
      Demo_OutputArray[j,16,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueSpots)
      
      Demo_OutputArray[j,17,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueCalls)
      
      Demo_OutputArray[j,18,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueLeads)
      
      Demo_OutputArray[j,19,1]<-as.numeric(Demo_OutputArray[j,15,1])*80
      
      Demo_OutputArray[j,20,1]<-(Demo_AdalyserDayPartNationalUnitLength$LeadperSpot)*5300
      
      Demo_OutputArray[j,21,1]<-as.numeric(sqldf("SELECT (CASE WHEN [Spots.1]=1 THEN 'NA' ELSE [Spots.1] END) AS [Spots.1]
                                                FROM Demo_MaxNoSpot_Calculation WHERE [Marginal.Calls.1]=
                                                (SELECT MIN([Marginal.Calls.1]) FROM Demo_MaxNoSpot_Calculation 
                                                WHERE [Marginal.Calls.1]>=1 
                                                ORDER BY [Marginal.Calls.1])"))
      
      
      Demo_OutputArray[j,22,1]<-as.numeric(sqldf("SELECT (CASE WHEN [Spots.1]=1 THEN 'NA' ELSE [Spots.1] END) AS [Spots.1]
                                                FROM Demo_MaxNoSpot_Calculation WHERE [Cost.per.Call.1]=
                                                (SELECT MAX([Cost.per.Call.1]) FROM Demo_MaxNoSpot_Calculation 
                                                WHERE [Cost.per.Call.1]<=80 
                                                ORDER BY [Cost.per.Call.1])"))
      
      
      Demo_OutputArray[j,23,1]<-as.numeric(sqldf("SELECT (CASE WHEN [Spots.1]=1 THEN 'NA' ELSE [Spots.1] END) AS [Spots.1]
                                                FROM Demo_MaxNoSpot_Calculation WHERE [Cost.per.Lead.1]=
                                                (SELECT MAX([Cost.per.Lead.1]) FROM Demo_MaxNoSpot_Calculation 
                                                WHERE [Cost.per.Lead.1] <=5300
                                                ORDER BY [Cost.per.Lead.1])"))
      
      
      Demo_OutputArray[j,30,1]<-as.numeric(Demo_AdalyserDayPartNationalUnitLength$TrueLeads/Demo_AdalyserDayPartNationalUnitLength$TrueCalls)
      
      
      Demo_OutputArray[j,31,1]<-as.numeric(sqldf("SELECT [Contacts.1] FROM Demo_MaxNoSpot_Calculation 
                                              WHERE [Spots.1]=(SELECT Spot FROM Demo_Station_PlanningData)"))
      
      Demo_OutputArray[j,32,1]<-as.numeric(Demo_OutputArray[j,30,1])*as.numeric(Demo_OutputArray[j,31,1])
      
      Demo_OutputArray[j,24,1]<-(as.numeric(Demo_Station_PlanningData$Cost))/(as.numeric(Demo_OutputArray[j,31,1]))
      
      Demo_OutputArray[j,25,1]<-(as.numeric(Demo_Station_PlanningData$Cost))/(as.numeric(Demo_OutputArray[j,32,1]))
      
      Demo_OutputArray[j,26,1]<-Demo_Station_PlanningData$Cost
      
      Demo_OutputArray[j,27,1]<-Demo_Station_PlanningData$Spot
      
      Demo_OutputArray[j,28,1]<-Demo_Station_PlanningData$CostPerSpot
      
      if((Demo_OutputArray[j,8,1]>0.05 && Demo_OutputArray[j,9,1]<0.65) || (Demo_OutputArray[j,8,1]=='NaN') || (Demo_OutputArray[j,8,1]==1))
      {
        Demo_OutputArray[j,29,1]<-"Do not use"
        
      }else if(Demo_OutputArray[j,8,1]>0.05 || Demo_OutputArray[j,9,1]<0.65)
      {
        Demo_OutputArray[j,29,1]<-"Directional"
        
      }else
      {
        Demo_OutputArray[j,29,1]<-"Good"
      }
      
    } else
    {
      Demo_OutputArray[j,1,1]<-0
      
      Demo_OutputArray[j,2,1]<-V1$Station[i]
      
      Demo_OutputArray[j,3,1]<-d1$DayPart[k]
      
      Demo_OutputArray[j,4,1]<-"National"
      
      Demo_OutputArray[j,5,1]<-'All'
      
      Demo_OutputArray[j,6,1]<-format(Demo_Coefficients[1,2,1], scientific=FALSE)
      
      Demo_OutputArray[j,7,1]<-format(Demo_Coefficients[1,1,1], scientific=FALSE)
      
      Demo_OutputArray[j,8,1]<-format(Demo_Coefficients[1,8,1], scientific=FALSE)
      
      Demo_OutputArray[j,9,1]<-format(res$r.squared, scientific=FALSE)
      
      Demo_OutputArray[j,10,1]<-format(res$adj.r.squared, scientific=FALSE)
      
      Demo_OutputArray[j,11,1]<-sum(Demo_AdalyserSummary$TrueSpend)
      
      Demo_OutputArray[j,12,1]<-sum(Demo_AdalyserSummary$TrueSpots)
      
      Demo_OutputArray[j,13,1]<-sum(Demo_AdalyserSummary$TrueCalls)
      
      Demo_OutputArray[j,14,1]<-sum(Demo_AdalyserSummary$TrueLeads)
      
      Demo_OutputArray[j,15,1]<-sum(Demo_AdalyserSummary$TrueCalls)/sum(Demo_AdalyserSummary$TrueSpots)
      
      Demo_OutputArray[j,16,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueSpots)
      
      Demo_OutputArray[j,17,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueCalls)
      
      Demo_OutputArray[j,18,1]<-sum(Demo_AdalyserSummary$TrueSpend)/sum(Demo_AdalyserSummary$TrueLeads)
      
      Demo_OutputArray[j,19,1]<-as.numeric(Demo_OutputArray[j,15,1])*80
      
      Demo_OutputArray[j,20,1]<-(Demo_AdalyserDayPartNationalUnitLength$LeadperSpot)*5300
      
      Demo_OutputArray[j,21,1]<-as.numeric(0)
      
      
      Demo_OutputArray[j,22,1]<-as.numeric(0)
      
      
      Demo_OutputArray[j,23,1]<-as.numeric(0)
      
      Demo_OutputArray[j,24,1]<-as.numeric(0)
      
      Demo_OutputArray[j,25,1]<-as.numeric(0)
      
      
      Demo_OutputArray[j,26,1]<-as.numeric(0)
      
      
      Demo_OutputArray[j,27,1]<-as.numeric(0)
      
      Demo_OutputArray[j,28,1]<-as.numeric(0)
      
      if((Demo_OutputArray[j,8,1]>0.05 && Demo_OutputArray[j,9,1]<0.65) || (Demo_OutputArray[j,8,1]=='NaN') || (Demo_OutputArray[j,8,1]==1))
      {
        Demo_OutputArray[j,29,1]<-"Do not use"
        
      }else if(Demo_OutputArray[j,8,1]>0.05 || Demo_OutputArray[j,9,1]<0.65)
      {
        Demo_OutputArray[j,29,1]<-"Directional"
        
      }else
      {
        Demo_OutputArray[j,29,1]<-"Good"
      }
      
      Demo_OutputArray[j,30,1]<-as.numeric(0)
      
      Demo_OutputArray[j,31,1]<-as.numeric(0)
      
      Demo_OutputArray[j,32,1]<-as.numeric(0)
      
    }
    
    print(d1$DayPart[k])
    
    j<-j+1
    k<-k+1
  }
  
  print(V1$Station[i])
  
  i<-i+1
}



indx <- sapply(Demo_PlanningData_NotHistorical, is.factor)
Demo_PlanningData_NotHistorical[indx] <- lapply(Demo_PlanningData_NotHistorical[indx], function(x) as.character(x))


#Planning data not in the historical data

for(i in 1:nrow(Demo_PlanningData_NotHistorical))
{
  Demo_OutputArray[j,1,1]<-Demo_PlanningData_NotHistorical$Month[i]
  
  Demo_OutputArray[j,2,1]<-Demo_PlanningData_NotHistorical$Network[i]
  
  Demo_OutputArray[j,3,1]<-Demo_PlanningData_NotHistorical$Daypart[i]
  
  Demo_OutputArray[j,4,1]<-Demo_PlanningData_NotHistorical$SpotType[i]
  
  Demo_OutputArray[j,5,1]<-Demo_PlanningData_NotHistorical$UnitLength[i]
  
  Demo_OutputArray[j,6,1]<-0
  
  Demo_OutputArray[j,7,1]<-0
  
  Demo_OutputArray[j,8,1]<-0
  
  Demo_OutputArray[j,9,1]<-0
  
  Demo_OutputArray[j,10,1]<-0
  
  Demo_OutputArray[j,11,1]<-0
  
  Demo_OutputArray[j,12,1]<-0
  
  Demo_OutputArray[j,13,1]<-0
  
  Demo_OutputArray[j,14,1]<-0
  
  Demo_OutputArray[j,15,1]<-0
  
  Demo_OutputArray[j,16,1]<-0
  
  Demo_OutputArray[j,17,1]<-0
  
  Demo_OutputArray[j,18,1]<-0
  
  Demo_OutputArray[j,19,1]<-0
  
  Demo_OutputArray[j,20,1]<-0
  
  
  Demo_OutputArray[j,21,1]<-0
  
  
  Demo_OutputArray[j,22,1]<-0
  
  
  Demo_OutputArray[j,23,1]<-0
  
  
  Demo_OutputArray[j,24,1]<-0
  
  Demo_OutputArray[j,25,1]<-0
  
  Demo_OutputArray[j,26,1]<-Demo_PlanningData_NotHistorical$Cost[i]
  
  Demo_OutputArray[j,27,1]<-Demo_PlanningData_NotHistorical$Spot[i]
  
  Demo_OutputArray[j,28,1]<-Demo_PlanningData_NotHistorical$CostPerSpot[i]
  
  Demo_OutputArray[j,29,1]<-'NA'
  
  j<-j+1
  i<-i+1
}


write.csv(Demo_OutputArray,"TV_Tool_output.csv")

# Demo_OutputArray<-data.frame(Demo_OutputArray)
# 
# sqlSave(Demo,Demo_OutputArray, tablename = "New_TV_Tool_Output",safer=FALSE, fast=FALSE, rownames = FALSE)
# 
# sqlQuery(Demo,"Truncate table [dbo].[TV_Tool_Output_PreviousMonth]")
# 
# sqlQuery(Demo,"insert into [dbo].[TV_Tool_Output_PreviousMonth] select * from TV_Tool_Output")
# 
# sqlQuery(Demo,"Truncate table [dbo].[TV_Tool_Output]")
# 
# sqlQuery(Demo,"insert into [dbo].[TV_Tool_Output] select * from New_TV_Tool_Output")
# 
# sqlQuery(Demo,"Drop table New_TV_Tool_Output")