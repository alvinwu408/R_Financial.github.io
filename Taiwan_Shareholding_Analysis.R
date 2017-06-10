####################################################################################################
# Project Name		: 搜集『集保戶股權分散表』資料
# Designer			: Alvin Wu
# Version			: V1.0
# Date				: 20170603
# Function 0			: 前置作業
# Function 1			: 利用預先下載好的『台灣股票代碼清單_20170603.csv』，下載所有目前網站上可供查詢之集保戶資料。
# Function 2			: 下載最新一週之『集保戶股權分散表』
# Function 3			: 將最新一週之『集保戶股權分散表』與舊有資料結合，並匯出成一個新檔案
# Function 4			: 將個股之資料匯入，繪成圖形分析
# Function 5		: 利用資料庫中的集保庫存資料，進一步計算出各股票的持股分級狀況，並寫入到一個 CSV 檔案中
# Function 6		: 將持股分級之 CSV 檔案讀入到一個 Array 中，並篩選出持股分級增加比率的『前十名』
####################################################################################################


####################################################################################################
# Function Name		: 前置作業
# Updated Date		: 20170603
# Description		: 設定函式庫與常用變數
# Main Code			: 

{
	# 清空所有紀錄
	rm( list = ls ( all = TRUE))

	# 宣告函式庫
	library(RCurl)		# 宣告後可以使用 runMean 函式
	library(plotly)		# 宣告後可以使用 plot_ly 函式
	library(quantmod)	# 宣告後可以使用 runMean 函式
	library(bitops)
	library(ggplot2)

	# 設置工作資料夾
	setwd("/Users/Alvin/Google Drive/07. Alvin Finance Data Center")

	# 其他常用參數
	today_date <- as.character(Sys.Date())
	today_date_no_hyphen <- gsub("-",replacement="", today_date) 
}

####################################################################################################


###################################################################################################
# Function Name		: Function 1
# Updated Date		: 20170603
# Description		: 批量下載『全部股票在全部日期』之『集保戶股權分散表』
# Main Code			: 

{
	# 使用者輸入參數：
	target_saving_path 	<- "xx. Data Collection/01. Taiwan Shareholding Info/20170604_All Stock Download/"
	myHttpheader <- c("User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ", "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8", "Accept-Language"="en-us", "Connection"="keep-alive", "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7")

	# 從網站上下載『日期資訊』，將日期資訊存在 date_info 中
	{
	url <- "http://www.tdcc.com.tw/smWeb/QryStock.jsp"

	data <- ""
	while(data == ""){
		data <- getURL(url, httpheader=myHttpheader)
		Sys.sleep(5)}

	date_info <- strsplit(unlist(data[1]),"<option >")[[1]]
	date_info <- strsplit(date_info,"</option>")
	date_info <- lapply(date_info,function(x){x[1]})
	date_info <- unlist(date_info)
	date_info <- date_info[2:length(date_info)]
	}

	# 輸入股票代碼清單至『stock_num』變數中
	{
	stock_num_vector = read.table("/Users/Alvin/Google Drive/07. Alvin Finance Data Center/xx. Data Collection/01. Taiwan Shareholding Info/台灣股票代碼清單_20170603.csv")
	stock_num <- stock_num_vector[,1]
	}

	# 使用者輸入參數：
	stock_num_wanted	<- ""
	stock_num_specific	<- which(stock_num == stock_num_wanted)
	stock_num_start		<- 1
	stock_num_end		<- length(stock_num)
	data_info_start		<- 1
	data_info_end		<- length(date_info)

	# 建置合適之 Array 來存放個股票在各週之『集保戶股權分散表』
	{
	matrix_shareholding <- array(dim = c(18, 4, length(date_info)))
	matrix_shareholding[1,,] <- c("證券代號：","NA","資料日期：","NA")
	matrix_shareholding[2,,] <- c("持股/單位數分級","人數","股數/單位數","佔集保庫存數比例(%)")
	matrix_shareholding[3:18,1,] <- c("1-999","1,000-5,000","5,001-10,000","10,001-15,000","15,001-20,000","20,001-30,000","30,001-40,000","40,001-50,000","50,001-100,000","100,001-200,000","200,001-400,000","400,001-600,000","600,001-800,000","800,001-1,000,000","1,000,001以上","合計")
	}

	# 利用 For-Loop 連續下載資料
	{
		for (n in c(stock_num_start:stock_num_end))
		{
			for (i in c(data_info_start:data_info_end))
			{
				url <- paste("http://www.tdcc.com.tw/smWeb/QryStock.jsp?SCA_DATE=",date_info[i],"&SqlMethod=StockNo&StockNo=",stock_num[n],"&StockName=&sub=%ACd%B8%DF",sep="")
				raw_data <- ""
				while(raw_data == "")
				{
					raw_data <- getURL(url, httpheader=myHttpheader)
					Sys.sleep(runif(1,min=0.1,max=2))
				}
				data <- strsplit(raw_data,"<td align=\"right\">")[[1]]
				data <- strsplit(data,"</td>\r\n")
				data <- lapply(data,function(x){x[1]})
				data <- unlist(data)
				data <- data[2:49]

				matrix_shareholding[1,2,i] <- stock_num[n]
				matrix_shareholding[1,4,i] <- date_info[i]
				matrix_shareholding[3:18,2:4,i] <- matrix(data,nrow=,ncol=3,byrow=TRUE)

				file_name <- paste(target_saving_path, stock_num[n], "_Shareholding_Info_", date_info[data_info_end], "~", date_info[data_info_start], ".csv", sep="")
				if(i==1){
					write.table(matrix_shareholding[,,i], append = FALSE, file_name, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE)	
				}else{write.table(matrix_shareholding[,,i], append = TRUE, file_name, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE)}
			}
			cat("\n目前執行進度：", stock_num[n], "；完成比例：",n/length(stock_num)*100,"%", "；目前系統時間：", as.character(Sys.time()))
		}
	}
}

####################################################################################################


####################################################################################################
# Function Name		: Function 2
# Updated Date		: 20170603
# Description		: 批量下載『全部股票在最新日期』之『集保戶股權分散表』
# Main Code			: 

{
	# 使用者輸入參數：
	target_saving_path 	<- "xx. Data Collection/01. Taiwan Shareholding Info/20170610_All Stock This Week/"
	myHttpheader <- c("User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ", "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8", "Accept-Language"="en-us", "Connection"="keep-alive", "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7")


	# 從網站上下載『日期資訊』，將日期資訊存在 date_info 中
	{
	url <- "http://www.tdcc.com.tw/smWeb/QryStock.jsp"

	data <- ""
	while(data == ""){
		data <- getURL(url, httpheader=myHttpheader)
		Sys.sleep(5)}

	date_info <- strsplit(unlist(data[1]),"<option >")[[1]]
	date_info <- strsplit(date_info,"</option>")
	date_info <- lapply(date_info,function(x){x[1]})
	date_info <- unlist(date_info)
	date_info <- date_info[2:length(date_info)]
	}

	# 輸入股票代碼清單至『stock_num』變數中
	{
	stock_num_vector = read.table("/Users/Alvin/Google Drive/07. Alvin Finance Data Center/xx. Data Collection/01. Taiwan Shareholding Info/台灣股票代碼清單_20170603.csv")
	stock_num <- stock_num_vector[,1]
	}

	# 使用者輸入參數：
	stock_num_wanted	<- "8422"
	stock_num_specific	<- which(stock_num == stock_num_wanted)
	stock_num_start		<- 1
	stock_num_end		<- length(stock_num)
	data_info_start		<- 1
	data_info_end		<- length(date_info)

	# 建置合適之 Matrix 來存放個股票在當週之『集保戶股權分散表』
	{
	matrix_shareholding <- matrix(nrow=18,ncol=4)
	matrix_shareholding[1,] <- c("證券代號：","NA","資料日期：","NA")
	matrix_shareholding[2,] <- c("持股/單位數分級","人數","股數/單位數","佔集保庫存數比例(%)")
	matrix_shareholding[3:18,1] <- c("1-999","1,000-5,000","5,001-10,000","10,001-15,000","15,001-20,000","20,001-30,000","30,001-40,000","40,001-50,000","50,001-100,000","100,001-200,000","200,001-400,000","400,001-600,000","600,001-800,000","800,001-1,000,000","1,000,001以上","合計")
	}

	# 利用 For-Loop 連續下載資料
	{
		for (n in c(stock_num_start:stock_num_end))
		{
			for (i in c(1:1))
			{
				url <- paste("http://www.tdcc.com.tw/smWeb/QryStock.jsp?SCA_DATE=",date_info[i],"&SqlMethod=StockNo&StockNo=",stock_num[n],"&StockName=&sub=%ACd%B8%DF",sep="")
				raw_data <- ""
				while(raw_data == "")
				{
					raw_data <- getURL(url, httpheader=myHttpheader)
					Sys.sleep(runif(1,min=0.1,max=2))
				}
				data <- strsplit(raw_data,"<td align=\"right\">")[[1]]
				data <- strsplit(data,"</td>\r\n")
				data <- lapply(data,function(x){x[1]})
				data <- unlist(data)
				data <- data[2:49]

				matrix_shareholding[1,2] <- stock_num[n]
				matrix_shareholding[1,4] <- date_info[i]
				matrix_shareholding[3:18,2:4] <- matrix(data,nrow=,ncol=3,byrow=TRUE)

				file_name <- paste(target_saving_path, stock_num[n], "_Shareholding_Info_", date_info[i],".csv", sep="")
				if(i==1){
					write.table(matrix_shareholding, append = FALSE, file_name, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE)	
				}else{write.table(matrix_shareholding, append = TRUE, file_name, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE)}
			}
			cat("\n目前執行進度：", stock_num[n], "；完成比例：",n/length(stock_num)*100,"%", "；目前系統時間：", as.character(Sys.time()))
		}
	}
}

####################################################################################################


####################################################################################################
# Function Name		: Function 3
# Updated Date		: 20170610
# Description		: 將當週之集保庫存資料，與資料庫中的舊資料整合，再次匯出
# Main Code			: 

{
	# 輸入股票代碼清單至『stock_num』變數中
	{
	stock_num_vector = read.table("/Users/Alvin/Google Drive/07. Alvin Finance Data Center/xx. Data Collection/01. Taiwan Shareholding Info/台灣股票代碼清單_20170603.csv")
	stock_num <- stock_num_vector[,1]
	}

	for (n in c(1:length(stock_num)))
	{
		target_stock_num <- stock_num[n]
		target_file_old <- paste("/Users/Alvin/Google Drive/07. Alvin Finance Data Center/xx. Data Collection/01. Taiwan Shareholding Info/01. All Stock Shareholding Summary/20160604~20170603/", target_stock_num,"_Shareholding_Info_20160604~20170603.csv",sep="")
		target_file_new <- paste("/Users/Alvin/Google Drive/07. Alvin Finance Data Center/xx. Data Collection/01. Taiwan Shareholding Info/20170610_All Stock This Week/", target_stock_num,"_Shareholding_Info_20170609.csv",sep="")
		target_file_combined <- paste("/Users/Alvin/Google Drive/07. Alvin Finance Data Center/xx. Data Collection/01. Taiwan Shareholding Info/01. All Stock Shareholding Summary/20160604~20170609/", target_stock_num,"_Shareholding_Info_20160604~20170609.csv",sep="")

		shareholding_raw_data_old = read.table(target_file_old)
		shareholding_raw_data_new = read.table(target_file_new)
		shareholding_raw_data_combined = rbind(shareholding_raw_data_new, shareholding_raw_data_old)

		# 將 Array 的數值儲存到 CSV 檔案中
		if(i==1){
			write.table(shareholding_raw_data_combined, append = FALSE, target_file_combined, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE)	
		}else{write.table(shareholding_raw_data_combined, append = TRUE, target_file_combined, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE)}
		cat("\n目前執行進度：", stock_num[n], "；完成比例：",n/length(stock_num)*100,"%", "；目前系統時間：", as.character(Sys.time()))
	}
}

####################################################################################################


####################################################################################################
# Function Name		: Function 4
# Updated Date		: 20170604
# Description		: 已將目標股票之『集保戶股權分散表』整理成固定格式之『CSV』檔案，透過這個程式，可以迅速分析其各分級持股比率變化，並且匯出 Plot_ly 圖形
# Main Code			: 

{
	# 使用者設定參數
	target_stock_num <- "2437"
	target_file <- paste("/Users/Alvin/Google Drive/07. Alvin Finance Data Center/xx. Data Collection/01. Taiwan Shareholding Info/20170604_All Stock Download/", target_stock_num,"_Shareholding_Info_20160604~20170603.csv",sep="")
	
	# 將輸入之資料整理到 Data Frame 中
	{	
	# 將目標股票之『集保戶股權分散表』匯入
	shareholding_raw_data = read.table(target_file)
	shareholding_raw_data  = as.matrix(shareholding_raw_data)
	row_of_data <- length(shareholding_raw_data[,1])
	set_of_data <- row_of_data / 18

	# 建置合適之 Array 來存放股票在各週之『集保戶股權分散表』
	shareholding_data <- array(dim = c(18, 4, set_of_data))
	shareholding_data[1,,] <- c("證券代號：","NA","資料日期：","NA")
	shareholding_data[2,,] <- c("持股/單位數分級","人數","股數/單位數","佔集保庫存數比例(%)")
	shareholding_data[3:18,1,] <- c("1-999","1,000-5,000","5,001-10,000","10,001-15,000","15,001-20,000","20,001-30,000","30,001-40,000","40,001-50,000","50,001-100,000","100,001-200,000","200,001-400,000","400,001-600,000","600,001-800,000","800,001-1,000,000","1,000,001以上","合計")

	# 將 CSV 中的二維矩陣，轉為三維 Array 存放。
	for (i in c(1:set_of_data))
	{
		shareholding_data[,,i] <- shareholding_raw_data[((set_of_data-i+1)*18-17):((set_of_data-i+1)*18),]
	}

	# 製作各種大股東持股比例之矩陣，方便輸出圖表
	shareholding_percentage_up <- matrix(nrow=set_of_data, ncol=15)
	shareholding_percentage_down <- matrix(nrow=set_of_data, ncol=15)
	
	for (i in c(1:set_of_data))
	{
		# 先將資料日期寫入矩陣中
		shareholding_percentage_up[i,1] <- shareholding_data[1,4,i]
		shareholding_percentage_down[i,1] <- shareholding_data[1,4,i]

		# 再將各分級之加總結果寫入		
		for(j in c(1:14))
		{
			shareholding_percentage_up[i,j+1] <- sum(as.numeric(shareholding_data[(3+j):17,4,i]))
			shareholding_percentage_down[i,j+1] <- sum(as.numeric(shareholding_data[(2+j):3,4,i]))
		}
	}

	# 將矩陣改成 Data Frame, 並寫入各行之名字
	shareholding_percentage_up <- as.data.frame(shareholding_percentage_up)
	shareholding_percentage_down <- as.data.frame(shareholding_percentage_down)

	colnames(shareholding_percentage_up)<-c("資料日期","大於1張","大於5張","大於10張","大於15張","大於20張","大於30張","大於40張","大於50張","大於100張","大於200張","大於400張","大於600張","大於800張","大於1000張")
	colnames(shareholding_percentage_down)<-c("資料日期","小於1張","小於5張","小於10張","小於15張","小於20張","小於30張","小於40張","小於50張","小於100張","小於200張","小於400張","小於600張","小於800張","小於1000張")
}

	# 互動式繪圖函數
	{
		p <- plot_ly()
		# p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於1張)), type = "scatter",mode = "lines",name = "1張", showlegend = TRUE)
		# p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於5張)), type = "scatter",mode = "lines",name = "5張", showlegend = TRUE)
		# p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於10張)), type = "scatter",mode = "lines",name = "10張", showlegend = TRUE)
		# p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於15張)), type = "scatter",mode = "lines",name = "15張", showlegend = TRUE)
		# p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於20張)), type = "scatter",mode = "lines",name = "20張", showlegend = TRUE)
		# p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於30張)), type = "scatter",mode = "lines",name = "30張", showlegend = TRUE)
		# p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於40張)), type = "scatter",mode = "lines",name = "40張", showlegend = TRUE)
		p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於50張)), type = "scatter",mode = "lines",name = "50張", showlegend = TRUE)
		p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於100張)), type = "scatter",mode = "lines",name = "100張", showlegend = TRUE)
		p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於200張)), type = "scatter",mode = "lines",name = "200張", showlegend = TRUE)
		p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於400張)), type = "scatter",mode = "lines",name = "400張", showlegend = TRUE)
		p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於600張)), type = "scatter",mode = "lines",name = "600張", showlegend = TRUE)
		p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於800張)), type = "scatter",mode = "lines",name = "800張", showlegend = TRUE)
		p <- add_trace(p, x=shareholding_percentage_up$資料日期, y=as.numeric(as.character(shareholding_percentage_up$大於1000張)), type = "scatter",mode = "lines",name = "1000張", showlegend = TRUE)
		layout(p, title=paste(target_stock_num, " 之大股東持股分佈",sep=""), xaxis=list(title="資料日期", showgrid=T), yaxis=list(title="\n持股比率（%）",range = c(min(as.numeric(as.character(shareholding_percentage_up$大於1000張))), max(as.numeric(as.character(shareholding_percentage_up$大於50張))) )))	
	}

	# shareholding_percentage_400up_avg <- runMean(as.numeric(as.character(shareholding_percentage_up$大於400張)), n=20)
	# shareholding_percentage_400up_avg <- as.character(na.omit(shareholding_percentage_400up_avg))


	
	# 將互動式圖表上傳到網路上
	Sys.setenv("plotly_username"="alvinwu408")
	Sys.setenv("plotly_api_key"="7yw7OR9Lzz5pYyTQJIwh")
	
	plotly_POST(p, filename = "2637_Analysis_Test")
	chart_link = api_create(x = last_plot(), filename = NULL, fileopt = "overwrite", sharing = "public")
chart_link
	p

}

####################################################################################################


####################################################################################################
# Function Name		: Function 5
# Updated Date		: 20170608
# Description		: 利用資料庫中的集保庫存資料，進一步計算出各股票的持股分級狀況，並寫入到一個 CSV 檔案中
# Main Code			: 

{
	# 使用者輸入參數：
	target_saving_path 	<- "xx. Data Collection/01. Taiwan Shareholding Info/02. All Stock Share Rating Summary/"


	# 輸入股票代碼清單至『stock_num』變數中
	{
	stock_num_vector = read.table("/Users/Alvin/Google Drive/07. Alvin Finance Data Center/xx. Data Collection/01. Taiwan Shareholding Info/台灣股票代碼清單_20170603.csv")
	stock_num <- stock_num_vector[,1]
	}


	# 將資料庫中所有股票的集保庫存 CSV 檔，整理出持股分級資料後，寫入到 CSV 檔案中
	for (n in c(1:length(stock_num)))
	{
		target_stock_num <- stock_num[n]
		target_file <- paste("/Users/Alvin/Google Drive/07. Alvin Finance Data Center/xx. Data Collection/01. Taiwan Shareholding Info/01. All Stock Shareholding Summary/20160604~20170609/", target_stock_num,"_Shareholding_Info_20160604~20170609.csv",sep="")
		
		# 批次將檔案讀入
		{	
			# 將目標股票之『集保戶股權分散表』匯入
			shareholding_raw_data = read.table(target_file)
			shareholding_raw_data  = as.matrix(shareholding_raw_data)
			row_of_data <- length(shareholding_raw_data[,1])
			set_of_data <- row_of_data / 18

			# 建置合適之 Array 來存放股票在各週之『集保戶股權分散表』
			shareholding_data <- array(dim = c(18, 4, set_of_data))
			shareholding_data[1,,] <- c("證券代號：","NA","資料日期：","NA")
			shareholding_data[2,,] <- c("持股/單位數分級","人數","股數/單位數","佔集保庫存數比例(%)")
			shareholding_data[3:18,1,] <- c("1-999","1,000-5,000","5,001-10,000","10,001-15,000","15,001-20,000","20,001-30,000","30,001-40,000","40,001-50,000","50,001-100,000","100,001-200,000","200,001-400,000","400,001-600,000","600,001-800,000","800,001-1,000,000","1,000,001以上","合計")

			# 將 CSV 中的二維矩陣，轉為三維 Array 存放。
			for (i in c(1:set_of_data))
			{
				shareholding_data[,,i] <- shareholding_raw_data[((set_of_data-i+1)*18-17):((set_of_data-i+1)*18),]
			}

			# 製作各種大股東持股比例之矩陣，方便輸出圖表
			shareholding_percentage_up <- matrix(nrow=set_of_data+2, ncol=15)
			shareholding_percentage_down <- matrix(nrow=set_of_data+2, ncol=15)	
			shareholding_percentage_up[1,1:6] <- c("證券代號：",target_stock_num,"證券名稱：","NA","資料計算日期：", today_date)
			shareholding_percentage_down[1,1:6] <- c("證券代號：",target_stock_num,"證券名稱：","NA","資料計算日期：", today_date)			
			shareholding_percentage_up[2,] <- c("資料日期","大於1張","大於5張","大於10張","大於15張","大於20張","大於30張","大於40張","大於50張","大於100張","大於200張","大於400張","大於600張","大於800張","大於1000張")
			shareholding_percentage_down[2,] <- c("資料日期","小於1張","小於5張","小於10張","小於15張","小於20張","小於30張","小於40張","小於50張","小於100張","小於200張","小於400張","小於600張","小於800張","小於1000張")

			# 矩陣格式建立完成後，開始批次輸入
			for (i in c(1:set_of_data))
			{
				# 先將資料日期寫入矩陣中
				shareholding_percentage_up[i+2,1] <- shareholding_data[1,4,i]
				shareholding_percentage_down[i+2,1] <- shareholding_data[1,4,i]

				# 再將各分級之加總結果寫入		
				for(j in c(1:14))
				{
					shareholding_percentage_up[i+2,j+1] <- sum(as.numeric(shareholding_data[(3+j):17,4,i]))
					shareholding_percentage_down[i+2,j+1] <- sum(as.numeric(shareholding_data[(2+j):3,4,i]))
				}
			}

			# 將矩陣資料輸出到 CSV 檔案中
			file_name <- paste(target_saving_path, "All_Stock_Share_Rating_Up_Info_", shareholding_data[1,4,1],"~",shareholding_data[1,4,set_of_data],".csv", sep="")
			if(n==1){
				write.table(shareholding_percentage_up, append = FALSE, file_name, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE)	
			}else{write.table(shareholding_percentage_up, append = TRUE, file_name, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE)}
		}
		cat("\n目前執行進度：", stock_num[n], "；完成比例：",n/length(stock_num)*100,"%", "；目前系統時間：", as.character(Sys.time()))
	}
}

####################################################################################################


####################################################################################################
# Function Name		: Function 6
# Updated Date		: 20170609
# Description		: 將持股分級之 CSV 檔案讀入到一個 Array 中，並篩選出持股分級增加比率的『前十名』
# Main Code			: 

{
	# 輸入股票代碼清單至『stock_num』變數中
	{
	stock_num_vector = read.table("/Users/Alvin/Google Drive/07. Alvin Finance Data Center/xx. Data Collection/01. Taiwan Shareholding Info/台灣股票代碼清單_20170603.csv")
	stock_num <- stock_num_vector[,1]
	}

	# 持股分級檔案位置	
	target_file <- "/Users/Alvin/Google Drive/07. Alvin Finance Data Center/xx. Data Collection/01. Taiwan Shareholding Info/02. All Stock Share Rating Summary/All_Stock_Share_Rating_Up_Info_20160604~20170609.csv"

	data_input <- read.table(target_file)
	data_input <- as.matrix(data_input)
	row_of_data <- length(data_input[,1]) / length(stock_num)
	set_of_data <- length(stock_num)

	# 建立一個放置全部資料之 Array
	all_stock_shareholding_percentage_up <- array(dim = c(row_of_data, 15, length(stock_num)))
	all_stock_shareholding_percentage_down <- array(dim = c(row_of_data, 15, length(stock_num)))

	# 將 CSV 中的二維矩陣，轉為三維 Array 存放。
	for (n in c(1:set_of_data))
	{
		all_stock_shareholding_percentage_up[,,(set_of_data-n+1)] <- data_input[((set_of_data-n+1)*row_of_data-(row_of_data-1)):((set_of_data-n+1)*row_of_data),]
	}

	# 利用該 Array 進一步計算出每週的各持股分級比率變化
	all_stock_shareholding_percentage_up_sub <- array(dim = c((row_of_data-1), 15, length(stock_num)))
	for (n in c(1:length(stock_num)))
	{
		for (i in c(1:(row_of_data-3)))
		{
			all_stock_shareholding_percentage_up_sub[(i+2),1,n] <- all_stock_shareholding_percentage_up[(i+3),1,n]
			ratio_sub <- as.numeric(all_stock_shareholding_percentage_up[(i+3),2:15,n]) - as.numeric(all_stock_shareholding_percentage_up[(i+2),2:15,n])
			all_stock_shareholding_percentage_up_sub[(i+2),2:15,n] <- as.character(round(ratio_sub,2))
		}
	}

	# 選出『大於400張』之變化前十名股票

	stock_400up_top_10_latest_week <- stock_num[order(all_stock_shareholding_percentage_up_sub[row_of_data-1,12,])[length(stock_num):(length(stock_num)-10)]]
	stock_400up_top_10_latest_week
}

####################################################################################################




