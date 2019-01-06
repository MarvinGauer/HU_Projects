library(ggplot2)
library(scales)

setwd("/Users/MarvinGauer/Documents/HU/Etherscan/New/")
Sys.setenv("LANGUAGE"="En")

GAS        = read.csv("export-AvgGasPrice.csv")
TXFEE      = read.csv("export-TransactionFee.csv")
COUNT      = read.csv("export-BlockCountRewards.csv")
BLOCKTIME  = read.csv("export-BlockTime.csv")
HASH       = read.csv("export-NetworkHash.csv")
DIFF       = read.csv("export-BlockDifficulty.csv")
ADD        = read.csv("export-AddressCount.csv")
TX         = read.csv("export-TxGrowth.csv")
CONTRACTS  = read.csv("Ethereum_Contracts_New.csv", sep = ";",colClasses=c("Date"="character","Hash"="character","Tx"="numeric","Balance"="character"),header=TRUE,stringsAsFactors=F)

CONTRACTS$Date         = as.character(CONTRACTS$Date)

TX$Date.UTC.           = as.Date(TX$Date.UTC.,format = "%m/%d/%Y")
TXFEE$Date.UTC.        = as.Date(TXFEE$Date.UTC.,format = "%m/%d/%Y")
GAS$Date.UTC.          = as.Date(GAS$Date.UTC.,format = "%m/%d/%Y")
COUNT$Date.UTC.        = as.Date(COUNT$Date.UTC.,format = "%m/%d/%Y")
DIFF$Date.UTC.         = as.Date(DIFF$Date.UTC.,format = "%m/%d/%Y")
ADD$Date.UTC.          = as.Date(ADD$Date.UTC.,format = "%m/%d/%Y")
HASH$Date.UTC.         = as.Date(HASH$Date.UTC.,format = "%m/%d/%Y")
BLOCKTIME$Date.UTC.    = as.Date(BLOCKTIME$Date.UTC.,format = "%m/%d/%Y")

GAS        = GAS[GAS$Date.UTC. <= "2018-12-31",]
TXFEE      = TXFEE[TXFEE$Date.UTC. <= "2018-12-31",]
COUNT      = COUNT[COUNT$Date.UTC. <= "2018-12-31",]
BLOCKTIME  = BLOCKTIME[BLOCKTIME$Date.UTC. <= "2018-12-31",]
HASH       = HASH[HASH$Date.UTC. <= "2018-12-31",]
DIFF       = DIFF[DIFF$Date.UTC. <= "2018-12-31",]
ADD        = ADD[ADD$Date.UTC. <= "2018-12-31",]
TX         = TX[TX$Date.UTC. <= "2018-12-31",]

grep('[0-9]{6}',CONTRACTS$Date,value=FALSE)

CONTRACTS[14136,1]  = "15.08.18"
CONTRACTS[25859,1]  = "31.05.18"
CONTRACTS[30780,1] = "23.04.18"
CONTRACTS[37142,1] = "04.03.18"
CONTRACTS[38037,1] = "26.02.18"
CONTRACTS[42557,1] = "20.01.18"
CONTRACTS[43350,1] = "11.01.18"
CONTRACTS[43407,1] = "10.01.18"
CONTRACTS[44519,1] = "24.12.17"
CONTRACTS[49365,1] = "03.10.17"
CONTRACTS[49716,1] = "23.09.17"
CONTRACTS[3718,1] = "16.11.18"
CONTRACTS$Date[30780] = "23.04.18"
CONTRACTS$Date[42557] = "20.01.18"
CONTRACTS$Date[40159] = "09.02.18"
CONTRACTS$Date[46622] = "22.11.17"

CONTRACTS$Date[45348] = "12.12.17"
CONTRACTS$Date[52515] = "04.06.17"
CONTRACTS$Date[18420] = "16.07.18"
CONTRACTS$Date[51884] = "15.07.17"
CONTRACTS$Date[47133] = "15.11.17"
CONTRACTS$Date[18546] = "16.07.18"
CONTRACTS$Date[24465] = "10.06.18"
CONTRACTS$Date[25040] = "06.06.18"

CONTRACTS$Date[48594] = "21.10.17"
CONTRACTS$Date[49081] = "10.10.17"
CONTRACTS$Date[39967] = "10.02.18"
CONTRACTS$Date[52866] = "18.04.17"
CONTRACTS$Date[5651] = "31.10.18"
CONTRACTS$Date[36256] = "11.03.18"
CONTRACTS$Date[34973] = "21.03.18"
CONTRACTS$Date[22668] = "20.06.18"

grep('[0-9]{6}',CONTRACTS$Date,value=FALSE)

Date      <- vector()
for(i in 1:length(CONTRACTS$Date)){
  if(grepl(".",CONTRACTS$Date[i],fixed = TRUE)){
    Date[i] = as.character(as.Date(CONTRACTS$Date[i],format = "%d.%m.%y"))
  }else if(grepl("/",CONTRACTS$Date[i],fixed = TRUE)){
    Date[i] = as.character(as.Date(CONTRACTS$Date[i],format = "%d/%m/%y"))
  }else{
    print(i)
    print(CONTRACTS$Date[i])
  }
}

Date = as.Date(Date,format = "%Y-%m-%d")

CONTRACTS$Date = Date
CONTRACTS$Date = as.Date(CONTRACTS$Date,format = "%y-%m-%d")

# Delete diplicates and keep the one with the highest amount of transactions
n_occur <- data.frame(table(CONTRACTS$Hash))
#CONTRACTS[CONTRACTS$Contract_Identifier %in% n_occur$Var1[n_occur$Freq > 1],]

CONTRACTS <- CONTRACTS[order(CONTRACTS$Hash, -abs(CONTRACTS$Tx) ), ]
CONTRACTS <- CONTRACTS[ !duplicated(CONTRACTS$Hash), ]

#Aggregate by Date
df        = data.frame("Dates" = vector(), "NoC" = vector(), "TXs" = vector(), "Bal" = vector())

i = 1

for(j in 1:length(unique(CONTRACTS$Date))){
  df     = rbind(df, data.frame("Dates" = unique(CONTRACTS$Date)[j],
                                "NoC"   = length(CONTRACTS$Tx[CONTRACTS$Date == unique(CONTRACTS$Date)[j]]),
                                "TXs"   = sum(CONTRACTS$Tx[CONTRACTS$Date == unique(CONTRACTS$Date)[j]]),
                                "Bal"   = sum(as.numeric(trimws(unlist(strsplit(grep(" Ether",CONTRACTS$Balance[CONTRACTS$Date == unique(CONTRACTS$Date)[j]],fixed = TRUE,value = TRUE),split = " Ether",fixed = TRUE))))) ))
  i = i + 1
}

df <- df[order(df$Dates) , ]

df = cbind(cbind(cbind(df,data.frame("CUMNoC" = cumsum(df$NoC))),data.frame("CUMTXs" = cumsum(df$TXs))),data.frame("CUMBal" = cumsum(df$Bal)))

TX[,"cum_value"]    <- cumsum(TX$Value)
TXFEE[,"cum_value"] <- cumsum(TXFEE$Value)
COUNT[,"cum_value"] <- cumsum(COUNT$Value)

options(scipen=10000)

# Daily Transactions
ggplot(TX, aes(x = Date.UTC., y = Value)) + geom_line() +
  theme(panel.background = element_blank(), 
       axis.line.x = element_line(color = "black"),
       axis.line.y = element_line(color = "black"),
       axis.text.y = element_text(size = 6),
       axis.text.x = element_text(angle = 45, hjust = 1),
       legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(TX$Date.UTC.),to=max(TX$Date.UTC.),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(from=0,to = max(TX$Value), by = 250000), limits = c(0,max(TX$Value)),labels = comma) + 
  xlab("Date") + 
  ylab("Number of Ether Transactions per Day")
ggsave("NumberOfDailyTransactions.pdf", width = 6, height = 6) 


# Aggregated Daily Transactions
ggplot(TX, aes(x = Date.UTC., y = cum_value/1000000)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(TX$Date.UTC.),to=max(TX$Date.UTC.),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(labels = comma) + 
  xlab("Date") + 
  ylab("Aggregated Number of Ether Transactions in Mio")
ggsave("AggregatedNumberOfDailyTransactions.pdf", width = 6, height = 6) 

# Average Daily Gas in Mio wei
ggplot(GAS, aes(x = Date.UTC., y = Value/1000000)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(GAS$Date.UTC.),to=max(GAS$Date.UTC.),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(from=0,to = 1000000, by = 250000), limits = c(0,1000000), labels = comma) + 
  xlab("Date") + 
  ylab("Average Gas in Mio wei")
ggsave("AverageGasInWei.pdf", width = 6, height = 6)

# Blocks per Day
ggplot(COUNT, aes(x = Date.UTC., y = Value)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(COUNT$Date.UTC.),to=max(COUNT$Date.UTC.),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(labels = comma) +
  xlab("Date") + 
  ylab("Blocks per Day")
ggsave("BlockPerDay.pdf", width = 6, height = 6)

# Summed Blocks per Day
ggplot(COUNT, aes(x = Date.UTC., y = cum_value)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(COUNT$Date.UTC.),to=max(COUNT$Date.UTC.),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(from=0,to = 8000000, by = 2000000), limits = c(0,8000000), labels = comma) + 
  xlab("Date") + 
  ylab("Cummulated Block Count")
ggsave("CummulatedBlocksPerDay.pdf", width = 6, height = 6)

# Average Daily Difficulty
ggplot(DIFF, aes(x = Date.UTC., y = Value)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(DIFF$Date.UTC.),to=max(DIFF$Date.UTC.),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(from=0,to = 4000, by = 1000), limits = c(0,4000), labels = comma) + 
  xlab("Date") + 
  ylab("Average Daily Difficulty in TH")
ggsave("AverageDailyDifficulty.pdf", width = 6, height = 6)

# Total Distinct Addresses
ggplot(ADD, aes(x = Date.UTC., y = Value/1000000)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(ADD$Date.UTC.),to=max(ADD$Date.UTC.),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(from=0,to = 50, by = 10), limits = c(0,50), labels = comma) + 
  xlab("Date") + 
  ylab("Total Distinct Addresses in Mio")
ggsave("TotalDistinctAddresses.pdf", width = 6, height = 6)

# Daily Hashrate
ggplot(HASH, aes(x = Date.UTC., y = Value)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(HASH$Date.UTC.),to=max(HASH$Date.UTC.),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(labels = comma) +
  xlab("Date") + 
  ylab("HashRate in GH/s")
ggsave("HashRate.pdf", width = 6, height = 6)

# Daily BlockTime
ggplot(BLOCKTIME, aes(x = Date.UTC., y = Value)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(BLOCKTIME$Date.UTC.),to=max(BLOCKTIME$Date.UTC.),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(labels = comma) +
  xlab("Date") + 
  ylab("Daily Average of Blocktimes in secs") + 
  geom_hline(yintercept = mean(BLOCKTIME$Value),color = 'red', linetype = 'dashed') +
  annotate("text", x=as.Date("2018-02-28"), y=mean(BLOCKTIME$Value)-3.5, label= paste("Overall Average:", round(mean(BLOCKTIME$Value),2),"secs"))
ggsave("AvgBlockTimeinSecs.pdf", width = 6, height = 6)

# Daily Contracts 
ggplot(df, aes(x = Dates, y = NoC)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(df$Dates),to=max(df$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(labels = comma) +
  xlab("Verification Date") + 
  ylab("Verified Contracts per Day")
ggsave("DailyContracts.pdf", width = 6, height = 6)

# Daily Contract Transactions 
ggplot(df, aes(x = Dates, y = TXs)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(df$Dates),to=max(df$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(labels = comma) +
  xlab("Verification Date") + 
  ylab("Transactions in Verified Contracts")
ggsave("ContractTransactions.pdf", width = 6, height = 6)

# Daily Balances Contract Transactions 
ggplot(df, aes(x = Dates, y = Bal)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(df$Dates),to=max(df$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(from=0,to = 4000, by = 1000), limits = c(0,4000), labels = comma) + 
  xlab("Verification Date") + 
  ylab("Balance of Verified Contracts in Ether")
ggsave("ContractBalances.pdf", width = 6, height = 6)

# CUM Contract Transactions 
ggplot(df, aes(x = Dates, y = CUMTXs)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(df$Dates),to=max(df$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(from=0,to = 100000000, by = 25000000), limits = c(0,100000000), labels = comma) + 
  xlab("Date") + 
  ylab("Cumulative Transactions in Verified Contracts")
ggsave("CUMContractTransactions.pdf", width = 6, height = 6)

# CUM daily verified Contracts 
ggplot(df, aes(x = Dates, y = CUMNoC)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(df$Dates),to=max(df$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(labels = comma) +
  xlab("Date") + 
  ylab("Cumulative Verified Contracts per Day")
ggsave("CUMVerifiedContracts.pdf", width = 6, height = 6)

# CUM Balance verified Contracts 
ggplot(df, aes(x = Dates, y = CUMBal)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(df$Dates),to=max(df$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(from=0,to = 80000, by = 20000), limits = c(0,80000),labels = comma) + 
  xlab("Date") + 
  ylab("Cumulative Balance of Verified Contracts in Ether")
ggsave("CUMBalVerifiedContracts.pdf", width = 6, height = 6)

# Combined Plot of daily Transactions overall and within verified contracts
colnames(TX)[1] = "Dates"
df2 = merge(TX, df, by="Dates", all.x=TRUE)

ggplot(df2,aes(x = Dates, y = cum_value/1000000,colour = 'Overall Transactions')) + geom_line() +
  geom_line(aes(x = Dates, y = CUMTXs/1000000, colour = 'Verified Contract Transactions')) +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm"),legend.position="bottom") + scale_x_date(breaks = as.Date(seq(from=min(df2$Dates),to=max(df2$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(from=0,to = 400, by = 100), limits = c(0,400), labels = comma) + 
  xlab("Date") + 
  ylab("Aggregated Number of Transactions in Mio") +scale_colour_manual(name="Type: ",values=c("black","red"), guide = guide_legend(fill = NULL,colour = NULL))#
ggsave("AggregatedNumberOfDailyTransactionsContractsAndOverall.pdf", width = 6, height = 6) 


# Cleaned Versions

# Average Daily Gas in Mio wei

GAS$Value[GAS$Value>=250000000000] = mean(GAS$Value[GAS$Value<250000000000])

ggplot(GAS, aes(x = Date.UTC., y = Value/1000000)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(GAS$Date.UTC.),to=max(GAS$Date.UTC.),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(labels = comma) +
  xlab("Date") + 
  ylab("Average Gas in Mio wei")
ggsave("CLEANEDAverageGasInWei.pdf", width = 6, height = 6)

df$TXs[df$TXs>=5000000] = mean(df$TXs[df$TXs<5000000])

# Daily Contract Transactions 
ggplot(df, aes(x = Dates, y = TXs)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(df$Dates),to=max(df$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(labels = comma) +
  xlab("Verification Date") + 
  ylab("Transactions in Verified Contracts")
ggsave("CLEANEDContractTransactions.pdf", width = 6, height = 6)

df$Bal[df$Bal>=2000] = mean(df$Bal[df$Bal<2000])

# Daily Balances Contract Transactions 
ggplot(df, aes(x = Dates, y = Bal)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(df$Dates),to=max(df$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(labels = comma) +
  xlab("Verification Date") + 
  ylab("Balance of Verified Contracts in Ether")
ggsave("CLEANEDContractBalances.pdf", width = 6, height = 6)

df = df[,c("Dates","NoC","TXs","Bal")]
df <- df[order(df$Dates) , ]

df = cbind(cbind(cbind(df,data.frame("CUMNoC" = cumsum(df$NoC))),data.frame("CUMTXs" = cumsum(df$TXs))),data.frame("CUMBal" = cumsum(df$Bal)))

# CUM Contract Transactions 
ggplot(df, aes(x = Dates, y = CUMTXs)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(df$Dates),to=max(df$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(from=0,to = 100000000, by = 20000000), limits = c(0,100000000), labels = comma) + 
  xlab("Date") + 
  ylab("Cumulative Transactions in Verified Contracts")
ggsave("CLEANEDCUMContractTransactions.pdf", width = 6, height = 6)

# CUM Balance verified Contracts 
ggplot(df, aes(x = Dates, y = CUMBal)) + geom_line() +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm")) + scale_x_date(breaks = as.Date(seq(from=min(df$Dates),to=max(df$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(from=0,to = 80000, by = 20000), limits = c(0,80000), labels = comma) +
  xlab("Date") + 
  ylab("Cumulative Balance of Verified Contracts in Ether")
ggsave("CLEANEDCUMBalVerifiedContracts.pdf", width = 6, height = 6)


# Combined Plot of daily Transactions overall and within verified contracts
colnames(TX)[1] = "Dates"
df2 = merge(TX, df, by="Dates", all.x=TRUE)

ggplot(df2,aes(x = Dates, y = cum_value/1000000,colour = 'Overall Transactions')) + geom_line() +
  geom_line(aes(x = Dates, y = CUMTXs/1000000, colour = 'Verified Contract Transactions')) +
  theme(panel.background = element_blank(), 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.4, "cm"),legend.position="bottom") + scale_x_date(breaks = as.Date(seq(from=min(df2$Dates),to=max(df2$Dates),by = "quarter")),labels=date_format("%b %y")) +
  scale_y_continuous(labels = comma) +
  xlab("Date") + 
  ylab("Aggregated Number of Transactions in Mio") +scale_colour_manual(name="Type: ",values=c("black","red"), guide = guide_legend(fill = NULL,colour = NULL))#
ggsave("CLEANEDAggregatedNumberOfDailyTransactionsContractsAndOverall.pdf", width = 6, height = 6) 

