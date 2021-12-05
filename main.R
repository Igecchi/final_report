install.packages('dplyr') # 初回のみ
library(readr)
library(dplyr)

#ダウンロードしたデータを取り込む
finance_info_2016_2021 <- read_csv("Documents/GitHub/final_report/data/financial_data/finance_info_2016_2021.csv", 
                                   locale = locale(encoding = "SHIFT-JIS"))
View(finance_info_2016_2021)
stock_info_2016_2021 <- read_csv("Documents/GitHub/final_report/data/financial_data/stock_info_2016_2021_monthly.csv", 
                                 locale = locale(encoding = "SHIFT-JIS"))
View(stock_info_2016_2021)

## 記述統計量の確認 ##
colnames(finance_info_2016_2021)
summary(finance_info_2016_2021)
#del <- c(14, 15, 31,32, 37, 42, 44, 48, 54, 61,62, 68,69, 84, 90, 95,96, 103, 118, 125, 141, 143,
         #146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,
         #165,166,167,168,169,170)
#del <- c(1, 3,4,5,6,7, 14,15, 31,32, 37, 42, 44, 48, 54, 61,62, 68,69, 84, 90, 95,96, 103, 118, 125, 141, 143,
#         146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,
#         165,166,167,168,169,170)
#tmp <- finance_info_2016_2021[ , -c(finance)]
finance <- c(2,3,
             6,8,9,10,
             11,12,13,16,18,20,21,30,31,32,33,34,35,36,37,38,39,44,45, #資産 31・32・37・44はいらないかも
             46,65,75, #負債
             80,81,82, #純資産
             83,84,85,86,87,90,89, #PL(期首からの累計期間), 84(売上高・営業収益(うち金融収益)・90はなくてもOK
             127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144, #145, #89販管費(期首からの累計期間)・141・143はいらないかも
             88, 91, #88売上総利益, 91研究開発費いレない方がいいかも
             94, 97, 99, #営業利益, 営業外収益, 営業外費用
             105, #経常利益／税金等調整前当期純利益
             114, #税金等調整前当期利益
             115, 116, #継続・非継続事業からの純利益
             117, #当期純利益
             119,120,121,122,125,126 #その他項目(従業員関連)・125はいらないかも
             ) 
            #18,20,30,   44, 85営業費用, 89販管費

#不要部分のデータを削除して抽出
finance_2016_2021_tmp = select(.data = finance_info_2016_2021, all_of(finance))
# データエクスポート
# write.csv(x = finance_2016_2021_tmp, file = "Documents/GitHub/final_report/data/financial_data/finance_2016_2021_tmp.csv")

#2016-2021のデータ
finance <- read_csv("Documents/GitHub/final_report/data/new/finance_2016_2021.csv")
View(finance)
stock <- read_csv("Documents/GitHub/final_report/data/new/stock_2016_2021_monthly.csv")
View(stock)

#データを結合する
df <- dplyr::left_join(finance,stock, by = c("corp_name", "fiscal_date", "fiscal_year", "fiscal_month"))
#View(df)


#時価総額を、他のデータ同様に百万単位に揃える
market_cap_new <- data.frame(df$market_cap / 1000000)
names(market_cap_new) <- c("market_cap_new")
df <- cbind(df, market_cap_new)

#simple qのデータを追加する
simple_q <- data.frame((df$market_cap_new + df$total_debt)/df$total_assets)
names(simple_q) <- c("simple_q")
df <- cbind(df, simple_q)

#ROEの列を追加する
roe <- data.frame(df$net_income_consolidated/df$capital_attributable_to_owners_of_the_parent_company)
names(roe) <- c("roe")
df <- cbind(df, roe)

#ROAの列を追加する
roa <- data.frame(df$net_income_consolidated/df$total_assets)
names(roa) <- c("roa")
df <- cbind(df, roa)

#年ダミーを入れる
for (i in 2016:2021){
        dummy <- paste0("d_",i)
        tmp <- if_else(df$fiscal_year == i, 1, 0)
        tmp <- data.frame(tmp)
        names(tmp) <- c(dummy)
        df <- cbind(df, tmp)
        }
# View(df)
# colnames(df)

#特定の列がNAとなる行を抽出する
df[!complete.cases(df$simple_q),]

# データエクスポート
# write.csv(x = df, file = "Documents/GitHub/final_report/data/new/df_joined.csv")

a <- c(13,14,15,16,17,18,19,20,21,22,23)
aa <- select(.data = finance_2016_2021_tmp, all_of(a))

## 相関関係をみる ##
#1.散布図行列(できない)
par(family= "HiraKakuProN-W3")
pairs(aa, panel = panel.smooth)

#2.散布図・相関行列図(できない)
install.packages('psych')
library(psych)
psych::pairs.panels(tmp)

#3.相関行列図（円）
install.packages('corrplot')
library(corrplot)
corrplot::corrplot(cor(iris[,-5]))

# 回帰分析を回す(できない)
lm(formula = "<その他項目>のれん・負ののれん償却額" ~ ., data=tmp)



