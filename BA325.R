#RMB CCR Daily Validation 


#Define Libraries
library (sqldf)
library (plyr)
library (tibble)
library (dplyr)
library(janitor)
library (readxl)
library(xlsx)
library(pivottabler)
library(magrittr)
library(flexdashboard)
library(rpivotTable)
library(Perc)
library(tidyverse)
library(RODBC)

#Define working directory where files are stored 
setwd("C:/Users/zgw/Documents/CPM team/BA 325/R")

#Report pulls data from report_date t most recent data
report_date= 20210927
report_date_3= 20210930
report_dateM = 20210901




#This sets up the connection to the database
connect<-odbcConnect("CRBIAPRD",
                     uid = "CR_SACCR_BAREPORTS",
                     pwd = "BA#25Report01"
)

#This query pulls all data at counterparty level
query1 = paste0("SELECT businessDate, counterparty,RISK_TAKER_NM, assetClass,markToMarket_ZAR, RWA, CVARWA,(RWA+CVARWA*DIV_FACTOR)*0.08 AS CTPY_OTC_CAPITAL,
case when ((RWA1+CVARWA1*DIV_FACTOR1) = 0 OR (RWA1+CVARWA1*DIV_FACTOR1) IS NULL ) then 0 else ((RWA+CVARWA*DIV_FACTOR)-(RWA1+CVARWA1*DIV_FACTOR1)) END AS CTPY_OTC_CAPITAL_DIFF,
EAD, EAD1,
case when (EAD1 = '0' or EAD1 is NULL) then 0 else round(((EAD-EAD1)/EAD1 * 100),2) end as EAD_Diff_CPY,
case when (EAD1 = '0' or EAD1 is NULL) then 0 else round((((EAD-EAD1)/EAD1 * 100)*EAD1)/EAD2,2) end as EAD_Diff_AssetClass,
COLLATERAL_AMT, PD, LGD  from (select to_char(DAY_DT) as businessDate,  CPY_NM as counterparty, RISK_TAKER_NM, asset_class_nm as assetClass, sum(market_value_amt) as markToMarket_ZAR, sum(rwa_amt) as RWA,sum(CVA_RWA_AMT) as CVARWA,
                sum(ead_amt) as EAD,sum(collateral_amt) AS COLLATERAL_AMT,  sum(pd_pct) as PD, sum(lgd_pct) as LGD
                FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1
                where RISK_TAKER_NM = 'FirstRand Bank SA' 
                and RISK_TYPE_CD = 'PreSettlement' 
                and GROUP_CD in ('Loc_SA_SACCR_TradingBook','SACCR_TradingBook')
                 and NET_LEVEL_ID = '0' 
                and CPY_NM <> 'LCH.CLEARNET LIMITED'
                and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
                and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_date,", 'YYYYMMDD')
                group by day_dt, cpy_nm, risk_taker_nm, asset_class_nm)
                
		left join (SELECT case when TRIM(TO_CHAR(DAY_DT, 'DAY')) = 'FRIDAY' then to_char(DAY_DT+3)
                else to_char(DAY_DT+1) 
                end as businessDate1, CPY_NM as counterparty1,sum(ead_amt) as EAD1,sum(rwa_amt) as RWA1,sum(CVA_RWA_AMT) as CVARWA1
                FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1  
                where RISK_TAKER_NM = 'FirstRand Bank SA' 
                and RISK_TYPE_CD = 'PreSettlement' 
                and GROUP_CD in ('Loc_SA_SACCR_TradingBook','SACCR_TradingBook')
                 and NET_LEVEL_ID = '0' 
                and CPY_NM <> 'LCH.CLEARNET LIMITED'
                and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
                and CPY_NM <> '>Unknown' 
                AND TRUNC (DAY_DT) >= TO_DATE (",report_date_3,", 'YYYYMMDD')
                group by day_dt, cpy_nm, risk_taker_nm, asset_class_nm)
                on businessDate= businessDate1 and counterparty= counterparty1
                
		left join (SELECT case when TRIM(TO_CHAR(DAY_DT, 'DAY')) = 'FRIDAY' then to_char(DAY_DT+3)
                else to_char(DAY_DT+1) end as businessDate2, asset_class_nm as asset_class_nm2 ,sum(ead_amt) as EAD2
                FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1  
                where RISK_TAKER_NM = 'FirstRand Bank SA' 
                and RISK_TYPE_CD = 'PreSettlement' 
                and GROUP_CD in ('Loc_SA_SACCR_TradingBook','SACCR_TradingBook')
                and NET_LEVEL_ID = '0' 
                and CPY_NM <> 'LCH.CLEARNET LIMITED'
                and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
                and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_date_3,", 'YYYYMMDD')
                group by day_dt, asset_class_nm)
                on businessDate= businessDate2 and assetClass = asset_class_nm2
                
		left join
                (SELECT   DAY_DT,(12.5*2.33*SQRT(Power(PORT_CVA_A_AMT,2)+PORT_CVA_B_AMT*1000000000)/SUM_CVA_RWA) AS DIV_FACTOR 
                FROM (
                select DAY_DT, SUM(RWA_AMT) AS SUM_RWA_AMT, SUM(CVA_RWA_AMT) AS SUM_CVA_RWA, SUM(CVA_A_AMT) AS PORT_CVA_A_AMT,SUM(CVA_B_AMT) AS 		PORT_CVA_B_AMT
                 FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1
                where RISK_TAKER_NM = 'FirstRand Bank SA' 
                and RISK_TYPE_CD = 'PreSettlement' 
                and GROUP_CD in ('Loc_SA_SACCR_TradingBook','SACCR_TradingBook')
                 and NET_LEVEL_ID = '0' 
                and CPY_NM <> 'LCH.CLEARNET LIMITED'
                and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
                and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_date," , 'YYYYMMDD')
                group by day_dt))
                ON businessDate = DAY_DT
                
                	left join
                (SELECT   businessDate3,(12.5*2.33*SQRT(Power(PORT_CVA_A_AMT,2)+PORT_CVA_B_AMT*1000000000)/SUM_CVA_RWA) AS DIV_FACTOR1
                FROM (
                select case when TRIM(TO_CHAR(DAY_DT, 'DAY')) = 'FRIDAY' then to_char(DAY_DT+3)
                else to_char(DAY_DT+1) end as businessDate3, SUM(RWA_AMT) AS SUM_RWA_AMT, SUM(CVA_RWA_AMT) AS SUM_CVA_RWA, SUM(CVA_A_AMT) AS PORT_CVA_A_AMT,SUM(CVA_B_AMT) AS 		PORT_CVA_B_AMT
                 FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1
                where RISK_TAKER_NM = 'FirstRand Bank SA' 
                and RISK_TYPE_CD = 'PreSettlement' 
                and GROUP_CD in ('Loc_SA_SACCR_TradingBook','SACCR_TradingBook')
                 and NET_LEVEL_ID = '0' 
                and CPY_NM <> 'LCH.CLEARNET LIMITED'
                and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
                and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_date_3," , 'YYYYMMDD')
                group by day_dt))
                ON businessDate = businessDate3")


#Executing the query
counterparty <- sqlQuery(connect
                         , query1,max = 10000)

query2 = paste0("SELECT businessDate, counterparty,RISK_TAKER_NM, assetClass,markToMarket_ZAR, RWA, CVARWA,(RWA+CVARWA*DIV_FACTOR)*0.08 AS CTPY_OTC_CAPITAL,
EAD, COLLATERAL_AMT, PD, LGD  from (select to_char(DAY_DT) as businessDate,  CPY_NM as counterparty, RISK_TAKER_NM, asset_class_nm as assetClass, sum(market_value_amt) as markToMarket_ZAR, sum(rwa_amt) as RWA,sum(CVA_RWA_AMT) as CVARWA,
                sum(ead_amt) as EAD,sum(collateral_amt) AS COLLATERAL_AMT,  sum(pd_pct) as PD, sum(lgd_pct) as LGD

                FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1
                where RISK_TAKER_NM = 'FirstRand Bank SA' 
                and RISK_TYPE_CD = 'PreSettlement' 
                and GROUP_CD in ('Loc_SA_SACCR_TradingBook','SACCR_TradingBook')
                 and NET_LEVEL_ID = '0' 
                and CPY_NM <> 'LCH.CLEARNET LIMITED'
                and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
                and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_dateM,", 'YYYYMMDD')
                group by day_dt, cpy_nm, risk_taker_nm, asset_class_nm)
                
                
		left join

                (SELECT   DAY_DT,(12.5*2.33*SQRT(Power(PORT_CVA_A_AMT,2)+PORT_CVA_B_AMT*1000000000)/SUM_CVA_RWA) AS DIV_FACTOR 
                FROM (
                select DAY_DT, SUM(RWA_AMT) AS SUM_RWA_AMT, SUM(CVA_RWA_AMT) AS SUM_CVA_RWA, SUM(CVA_A_AMT) AS PORT_CVA_A_AMT,SUM(CVA_B_AMT) AS 				PORT_CVA_B_AMT
                 FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1
                where RISK_TAKER_NM = 'FirstRand Bank SA' 
                and RISK_TYPE_CD = 'PreSettlement' 
                and GROUP_CD in ('Loc_SA_SACCR_TradingBook','SACCR_TradingBook')
                 and NET_LEVEL_ID = '0' 
                and CPY_NM <> 'LCH.CLEARNET LIMITED'
                and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
                and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_dateM," , 'YYYYMMDD')
                group by day_dt))
                ON businessDate = DAY_DT")

#Executing the query
graph <- sqlQuery(connect
                  , query2,max = 60000)

#This query is used to calc CDS_capital
query3 = paste0("SELECT businessDate, counterparty,RISK_TAKER_NM, assetClass,markToMarket_ZAR, RWA, CVARWA,(RWA+CVARWA*DIV_FACTOR)*0.08 AS CTPY_CDS_CAPITAL,
case when ((RWA1+CVARWA1*DIV_FACTOR1) = 0 OR (RWA1+CVARWA1*DIV_FACTOR1) IS NULL ) then 0 else ((RWA+CVARWA*DIV_FACTOR)-(RWA1+CVARWA1*DIV_FACTOR1)) END AS CTPY_CDS_CAPITAL_DIFF,
EAD, EAD1,
case when (EAD1 = '0' or EAD1 is NULL) then 0 else round(((EAD-EAD1)/EAD1 * 100),2) end as EAD_Diff_CPY,
case when (EAD1 = '0' or EAD1 is NULL) then 0 else round((((EAD-EAD1)/EAD1 * 100)*EAD1)/EAD2,2) end as EAD_Diff_AssetClass,
COLLATERAL_AMT, PD, LGD  from (select to_char(DAY_DT) as businessDate,  CPY_NM as counterparty, RISK_TAKER_NM, asset_class_nm as assetClass, sum(market_value_amt) as markToMarket_ZAR, sum(rwa_amt) as RWA,sum(CVA_RWA_AMT) as CVARWA,
                sum(ead_amt) as EAD,sum(collateral_amt) AS COLLATERAL_AMT,  sum(pd_pct) as PD, sum(lgd_pct) as LGD
                FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1
                where RISK_TAKER_NM = 'FirstRand Bank SA'
		and GROUP_CD IN ('SACCR_Deriv_B_UCDS_Group','SACCR_TradingBook_B_UCDS_Group','Loc_SA_Deriv_B_UCDS_Group',
'Loc_SA_SACCR_TradingBook_B_UCDS_Group')
		and NET_LEVEL_ID = 0
		and CPY_NM <> 'LCH.CLEARNET LIMITED'
		and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
		and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_date,", 'YYYYMMDD')
                group by day_dt, cpy_nm, risk_taker_nm, asset_class_nm)
                
		left join (SELECT case when TRIM(TO_CHAR(DAY_DT, 'DAY')) = 'FRIDAY' then to_char(DAY_DT+3)
                else to_char(DAY_DT+1) 
                end as businessDate1, CPY_NM as counterparty1,sum(ead_amt) as EAD1,sum(rwa_amt) as RWA1,sum(CVA_RWA_AMT) as CVARWA1
                FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1  
                where RISK_TAKER_NM = 'FirstRand Bank SA'
		and GROUP_CD IN ('SACCR_Deriv_B_UCDS_Group','SACCR_TradingBook_B_UCDS_Group','Loc_SA_Deriv_B_UCDS_Group',
'Loc_SA_SACCR_TradingBook_B_UCDS_Group'
)
		and NET_LEVEL_ID = 0
		and CPY_NM <> 'LCH.CLEARNET LIMITED'
		and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
		and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_date_3,", 'YYYYMMDD')
                group by day_dt, cpy_nm, risk_taker_nm, asset_class_nm)
                on businessDate= businessDate1 and counterparty= counterparty1
                
		left join (SELECT case when TRIM(TO_CHAR(DAY_DT, 'DAY')) = 'FRIDAY' then to_char(DAY_DT+3)
                else to_char(DAY_DT+1) end as businessDate2, asset_class_nm as asset_class_nm2 ,sum(ead_amt) as EAD2
                FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1  
               where RISK_TAKER_NM = 'FirstRand Bank SA'
		and GROUP_CD IN ('SACCR_Deriv_B_UCDS_Group','SACCR_TradingBook_B_UCDS_Group','Loc_SA_Deriv_B_UCDS_Group',
'Loc_SA_SACCR_TradingBook_B_UCDS_Group'
)
		and NET_LEVEL_ID = 0
		and CPY_NM <> 'LCH.CLEARNET LIMITED'
		and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
		and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_date_3,", 'YYYYMMDD')
                group by day_dt, asset_class_nm)
                on businessDate= businessDate2 and assetClass = asset_class_nm2
                
		left join
                (SELECT   DAY_DT,(12.5*2.33*SQRT(Power(PORT_CVA_A_AMT,2)+PORT_CVA_B_AMT*1000000000)/SUM_CVA_RWA) AS DIV_FACTOR 
                FROM (
                select DAY_DT, SUM(RWA_AMT) AS SUM_RWA_AMT, SUM(CVA_RWA_AMT) AS SUM_CVA_RWA, SUM(CVA_A_AMT) AS PORT_CVA_A_AMT,SUM(CVA_B_AMT) AS 		PORT_CVA_B_AMT
                 FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1
                where RISK_TAKER_NM = 'FirstRand Bank SA'
		and GROUP_CD IN ('SACCR_Deriv_B_UCDS_Group','SACCR_TradingBook_B_UCDS_Group','Loc_SA_Deriv_B_UCDS_Group',
'Loc_SA_SACCR_TradingBook_B_UCDS_Group'
)
		and NET_LEVEL_ID = 0
		and CPY_NM <> 'LCH.CLEARNET LIMITED'
		and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
		and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_date," , 'YYYYMMDD')
                group by day_dt))
                ON businessDate = DAY_DT
                
                	left join
                (SELECT   businessDate3,(12.5*2.33*SQRT(Power(PORT_CVA_A_AMT,2)+PORT_CVA_B_AMT*1000000000)/SUM_CVA_RWA) AS DIV_FACTOR1
                FROM (
                select case when TRIM(TO_CHAR(DAY_DT, 'DAY')) = 'FRIDAY' then to_char(DAY_DT+3)
                else to_char(DAY_DT+1) end as businessDate3, SUM(RWA_AMT) AS SUM_RWA_AMT, SUM(CVA_RWA_AMT) AS SUM_CVA_RWA, SUM(CVA_A_AMT) AS PORT_CVA_A_AMT,SUM(CVA_B_AMT) AS 		PORT_CVA_B_AMT
                 FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1
                where RISK_TAKER_NM = 'FirstRand Bank SA'
		and GROUP_CD IN ('SACCR_Deriv_B_UCDS_Group','SACCR_TradingBook_B_UCDS_Group','Loc_SA_Deriv_B_UCDS_Group',
'Loc_SA_SACCR_TradingBook_B_UCDS_Group'
)
		and NET_LEVEL_ID = 0
		and CPY_NM <> 'LCH.CLEARNET LIMITED'
		and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
		and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_date_3," , 'YYYYMMDD')
                group by day_dt))
                ON businessDate = businessDate3")

CDS <- sqlQuery(connect,query3 , max = 2000000)

query4 = paste0("SELECT businessDate, counterparty,RISK_TAKER_NM, assetClass,markToMarket_ZAR, RWA, CVARWA,(RWA+CVARWA*DIV_FACTOR)*0.08 AS CTPY_CDS_CAPITAL,
EAD, COLLATERAL_AMT, PD, LGD  from (select to_char(DAY_DT) as businessDate,  CPY_NM as counterparty, RISK_TAKER_NM, asset_class_nm as assetClass, sum(market_value_amt) as markToMarket_ZAR, sum(rwa_amt) as RWA,sum(CVA_RWA_AMT) as CVARWA,
                sum(ead_amt) as EAD,sum(collateral_amt) AS COLLATERAL_AMT,  sum(pd_pct) as PD, sum(lgd_pct) as LGD

                FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1
                where RISK_TAKER_NM = 'FirstRand Bank SA'
		and GROUP_CD IN ('SACCR_Deriv_B_UCDS_Group','SACCR_TradingBook_B_UCDS_Group','Loc_SA_Deriv_B_UCDS_Group',
'Loc_SA_SACCR_TradingBook_B_UCDS_Group')
		and NET_LEVEL_ID = 0
		and CPY_NM <> 'LCH.CLEARNET LIMITED'
		and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
		and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_dateM,", 'YYYYMMDD')
                group by day_dt, cpy_nm, risk_taker_nm, asset_class_nm)
                
                
		left join

                (SELECT   DAY_DT,(12.5*2.33*SQRT(Power(PORT_CVA_A_AMT,2)+PORT_CVA_B_AMT*1000000000)/SUM_CVA_RWA) AS DIV_FACTOR 
                FROM (
                select DAY_DT, SUM(RWA_AMT) AS SUM_RWA_AMT, SUM(CVA_RWA_AMT) AS SUM_CVA_RWA, SUM(CVA_A_AMT) AS PORT_CVA_A_AMT,SUM(CVA_B_AMT) AS 				PORT_CVA_B_AMT
                 FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_ANALYSIS_DAILY_M1
                where RISK_TAKER_NM = 'FirstRand Bank SA'
		and GROUP_CD IN ('SACCR_Deriv_B_UCDS_Group','SACCR_TradingBook_B_UCDS_Group','Loc_SA_Deriv_B_UCDS_Group',
'Loc_SA_SACCR_TradingBook_B_UCDS_Group')
		and NET_LEVEL_ID = 0
		and CPY_NM <> 'LCH.CLEARNET LIMITED'
		and CPY_NM <> 'JSE CLEAR PROPRIETARY LIMITED'
		and CPY_NM <> '>Unknown'
                AND TRUNC (DAY_DT) >= TO_DATE (",report_dateM," , 'YYYYMMDD')
                group by day_dt))
                ON businessDate = DAY_DT")

#Executing the query
graph2 <- sqlQuery(connect
                   , query4,max = 60000)

#This query pulls all data at transaction level - this is the only table that contains the sourcesystem field
query5 =paste0( "SELECT to_char(DAY_DT) as businessDate,risk_type_cd, cpy_nm as counterparty,product_cd,instrument_cd, risk_taker_nm , exp_source_subsys_cd as sourceSystem, market_value_amt ,exposure_amt as MarkToMarket_ZAR, debit_mtm_amt, credit_mtm_amt, collateral_amt, collateral_type_cd as Type,facility_id_cd as dealID
      FROM CR_SACCR_DAILY_CLEAN.VW_SACCR_BA_TRAN_DATA_DAILY_M1 
where CPY_NM <> '>Unknown' 
and TRUNC (DAY_DT) >= TO_DATE (",report_date,", 'YYYYMMDD')")

#Executing the query
trans <- sqlQuery(connect,query5 , max = 2000000)


#Create subsets for pivots
data1 <- trans[ ,c("BUSINESSDATE","SOURCESYSTEM","COUNTERPARTY")]
data2 <- counterparty[ ,c("BUSINESSDATE","ASSETCLASS","COUNTERPARTY","MARKTOMARKET_ZAR")]
data3 <- counterparty[ ,c("BUSINESSDATE","ASSETCLASS","CVARWA","RWA","COUNTERPARTY")]
data4 <- counterparty[ ,c("BUSINESSDATE","ASSETCLASS","EAD","EAD_DIFF_CPY","EAD_DIFF_ASSETCLASS","COUNTERPARTY")] 
data5 <- counterparty[ ,c("BUSINESSDATE","ASSETCLASS","PD")]
data6 <- counterparty[ ,c("BUSINESSDATE","ASSETCLASS","COUNTERPARTY","CTPY_OTC_CAPITAL","CTPY_OTC_CAPITAL_DIFF")]
data7 <- counterparty[ ,c("BUSINESSDATE","ASSETCLASS","LGD")]
data8 <- counterparty[,c("BUSINESSDATE","ASSETCLASS","COUNTERPARTY","COLLATERAL_AMT")]
data9 <- graph[ ,c("BUSINESSDATE","ASSETCLASS","COUNTERPARTY","CTPY_OTC_CAPITAL")]
data10 <- CDS[ ,c("BUSINESSDATE","ASSETCLASS","COUNTERPARTY","CTPY_CDS_CAPITAL","CTPY_CDS_CAPITAL_DIFF")]
data11 <- graph2[ ,c("BUSINESSDATE","ASSETCLASS","COUNTERPARTY","CTPY_CDS_CAPITAL")]

saveRDS(data1,file = "SS.rds")
saveRDS(data2,file = "MtM.rds")
saveRDS(data3,file = "RWA.rds")
saveRDS(data4,file = "EAD.rds")
saveRDS(data5,file = "PD.rds")
saveRDS(data6,file = "CAP.rds")
saveRDS(data7,file = "LGD.rds")
saveRDS(data8,file = "Collateral.rds")
saveRDS(data9,file = "Graph.rds")
saveRDS(data10,file = "CDS.rds")
saveRDS(data11,file = "Graph2.rds")

write_xlsx(counterparty,"C:/Users/zgw/Documents/CPM team/BA 325/August.xlsx")


