# install.packages('dplyr') # 初回のみ
library(readr)
library(dplyr)
library(tidyverse)
library(car)

#ダウンロードしたデータを取り込む
finance_info_2017_2020 <- read_csv("data/2017_2020_finance_final2_modified.csv")
# View(finance_info_2017_2020)

### 前処理　###
## financeデータの前処理 ##
# 連結基準が日本でないデータを削除
finance_info_2017_2020<-finance_info_2017_2020[!(finance_info_2017_2020$consolidation_criteria!=1),]
# 2017~2020の4年分データがある企業のみに絞る
finance_info_2017_2020<-finance_info_2017_2020[!(finance_info_2017_2020$fiscal_year==2016),]
finance_info_2017_2020<-finance_info_2017_2020[!(finance_info_2017_2020$fiscal_year==2021),]

tmp <- data.frame(table(finance_info_2017_2020$corp_name))
less_than4 <- filter(tmp, tmp[2]<4) #4年未満の企業
less_than4 <- as.vector(less_than4$Var1)

for(i in less_than4){
  finance_info_2017_2020<-finance_info_2017_2020[!(finance_info_2017_2020$corp_name == i),]
}

table(finance_info_2017_2020$corp_name, finance_info_2017_2020$fiscal_year ) ##全行を見れるわけではないので注意
#0の入ってる企業がいくつかあったのでその企業を削除する
corp_name <- c("ＮＩＳＳＨＡ", "ＴＨＫ", "ドラフト")
for(i in corp_name){
  finance_info_2017_2020<-finance_info_2017_2020[!(finance_info_2017_2020$corp_name == i),]
}

## 4年分以上の企業（スプレッドシートで目視で加工を行う）
tmp2 <- data.frame(table(finance_info_2017_2020$corp_name))
filter(tmp2, tmp2[2]>4)

# write.csv(x = finance_info_2017_2020, file = "~/Documents/GitHub/final_report/data/2017_2020_finance_final2_modified2.csv")
# fwrite(2017_2020_finance_final2_modified2,"Documents/GitHub/final_report/data/new/2017_2020_finance_final2_modified2.csv")

############## 元データ修正後　##########################
#ダウンロードしたデータを取り込む
df <- read_csv("~/Documents/GitHub/final_report/data/target_final.csv")

corp_profile <- read_csv("~/Documents/GitHub/final_report/data/corp_profile.csv")

# summary(target)


stock_info_2017_2021 <- read_csv("~/Documents/GitHub/final_report/data/stock_info_2017_2021_monthly_all.csv")
stock_component <- read_csv("~/Documents/GitHub/final_report/data/stock_component.csv") ##株主比率に関するデータ
# stock_info_2017_2021 <- stock_info_2017_2021 %>% 
#   mutate("dividend" = dividend_per_stock * n_stock) ##配当額を株式数に換算する

# View(stock_info_2017_2021)
basic_info_2017_2021 <- read_csv("~/Documents/GitHub/final_report/data/basic_info_2017_2021_all.csv")
# transfer_2017_2021 <- read_csv("Documents/GitHub/final_report/data/transfer_2017_2021.csv", locale = locale(encoding = "SHIFT-JIS"))


#データを結合する
# df <- dplyr::left_join(finance,stock, by = c("corp_name", "fiscal_date", "fiscal_year", "fiscal_month"))
df <- dplyr::left_join(df, basic_info_2017_2021, by = c("corp_name"))
df <- dplyr::left_join(df, stock_info_2017_2021, by = c("corp_name", "fiscal_date", "fiscal_year", "fiscal_month"))
df <- dplyr::left_join(df, stock_component, by = c("corp_id","corp_name", "fiscal_year", "fiscal_month"))

df %>% mutate(., no = row_number()) %>% group_by(., corp_name, fiscal_year) %>% filter(., n() > 1)

#### データ加工　####
df_tmp <- df %>%
  mutate("market_cap_new" = market_cap / 1000000) %>% #時価総額を、他のデータ同様に百万単位に揃える
  mutate("p_gov" = p_gov / 100) %>%
  mutate("p_bank" = p_bank / 100) %>%
  mutate("p_stockc" = p_stockc / 100) %>%
  mutate("p_other" = p_other/ 100) %>%
  mutate("p_foreign" = p_foreign / 100) %>%
  mutate("p_individual" = p_individual / 100)
  
  # mutate("simple_q" = (market_cap_new + total_debt) / total_assets) %>% 
  # mutate("roe" = net_income_consolidated / capital_attributable_to_owners_of_the_parent_company) %>% 
  # mutate("roa" = net_income_consolidated / total_assets) %>% 
  # mutate("per" = market_cap_new / net_income_consolidated)

## 相関係数を調べる
df_tmp[is.na(df_tmp)] <- 0
cor_all <- round(cor(dplyr::select(.data = df_tmp, market_cap_new , founded_year , n_consolidated_subsidiaries , n_consolidated_subsidiaries_ipo , current_assets , #cash_and_cash_equivalents ,
                            non_current_assets , tangible_fixed_assets , intangible_fixed_assets , 
                            #patent_right , software , goodwill , leased_asset , trademark ,# sales_goodwill , design_right ,
                            other_intangible_fixed_assets , total_investment_and_other_assets , #development_cost ,
                            current_liabilities , non_current_liabilities ,
                            sales_operating_revenue , financial_income , sales_cost , sales_operating_cost , unrealized_profit_on_installment_sales , #financial_costs ,
                            sga_sales_commission , sga_storage_costs , sga_advertising_expenses , sga_sales_expansion_costs , sga_allowance_for_doubtful_accounts , sga_officer_compensation ,
                            sga_provision_for_retirement_benefits_for_officers , sga_provision_for_bonuses_for_directors , sga_personnel_welfare_expenses , sga_provision_for_retirement_benefits ,
                            sga_depreciation , sga_goodwill_amortization , sga_rent , sga_taxes_and_public_dues , sga_patent_fee_paid , sga_rd_cost , sga_warranty_repair_costs , sga_other ,
                            employees_end_term , avg_temp_employees , r_d_expenses , capital_investment , goodwill_amortization , #officer_bonus_provision ,
                            d_manufacture , #d_saas, 
                            p_gov, p_bank, p_stockc, p_foreign, p_other, p_individual, 
                            d_2018 , d_2019 , #d_2020 ,
                            d_1 , d_2 , d_3 , d_4 , d_5 , d_6 , d_7 , d_8 , d_9 ,d_10 , d_11# d_12 ,
                            )),2)


vif_res <- 1/(1-(cor_all^2))
vif_res
write.csv(x = vif_res, file = "~/Documents/GitHub/final_report/data/vif_cor_all.csv")

# #年ダミーを入れる(スプレッドシートで作業済み)
# for (i in 2017:2021){
#   dummy <- paste0("d_",i)
#   tmp <- if_else(df$fiscal_year == i, 1, 0)
#   tmp <- data.frame(tmp)
#   names(tmp) <- c(dummy)
#   df <- cbind(df, tmp)
# }
# 
# #月ダミーを入れる(スプレッドシートで作業済み)
# for (i in 1:12){
#   dummy <- paste0("d_",i)
#   tmp <- if_else(df$fiscal_month == i, 1, 0)
#   tmp <- data.frame(tmp)
#   names(tmp) <- c(dummy)
#   df <- cbind(df, tmp)
# }

df_all <- df_tmp
df_tmp <- df_all[, -1]
df_tmp <- df_tmp[, -2]

df_tmp <- as.data.frame(df_tmp)
df_tmp[is.na(df_tmp)] <- 0
df_short<-df_tmp[!(df_tmp$fiscal_year==2020),]

df_middle<-df_tmp[!(df_tmp$fiscal_year==2020),]
df_middle <- df_middle[!(df_middle$fiscal_year==2019),]

df_long <- df_tmp[!(df_tmp$fiscal_year==2020),]
df_long <- df_long[!(df_long$fiscal_year==2019),]
df_long <- df_long[!(df_long$fiscal_year==2018),]

## 単年度分析用に製造業と非製造業、saas企業に分ける ##
df_saas <- df_tmp[(df_tmp$d_saas==1),]
df_manu <- df_tmp[(df_tmp$d_manufacture==1),]
df_non_manu <- df_tmp[(df_tmp$d_manufacture==0),]

## df_shortについて製造業と非製造業、saas企業に分ける ##
df_short[is.na(df_short)] <- 0
df_saas_short <- df_short[(df_short$d_saas==1),]
df_manu_short <- df_short[(df_short$d_manufacture==1),]
df_non_manu_short <- df_short[(df_short$d_manufacture==0),]

## df_shortについて製造業と非製造業、saas企業に分ける ##
df_middle[is.na(df_middle)] <- 0
df_saas_middle <- df_middle[(df_middle$d_saas==1),]
df_manu_middle <- df_middle[(df_middle$d_manufacture==1),]
df_non_manu_middle <- df_middle[(df_middle$d_manufacture==0),]

## df_longについて製造業と非製造業、saas企業に分ける ##
df_long[is.na(df_long)] <- 0
df_saas_long <- df_long[(df_long$d_saas==1),]
df_manu_long <- df_long[(df_long$d_manufacture==1),]
df_non_manu_long <- df_long[(df_long$d_manufacture==0),]

### 記述統計量を保存する ###
library(tidyverse)
library(skimr)
# install.packages("summarytools", dependencies = T)
library(tibble)
library(summarytools)
library(car)

## 4年分のデータについて
saas_desc <- df_saas %>% 
  descr(transpose = TRUE) %>% 
  tb()

manu_desc <- df_manu_short %>% 
  descr(transpose = TRUE) %>% 
  tb()

non_manu_desc <- df_non_manu_short %>% 
  descr(transpose = TRUE) %>% 
  tb()

write.csv(x = saas_desc, file = "~/Documents/GitHub/final_report/data/saas_desc.csv")
write.csv(x = manu_desc, file = "~/Documents/GitHub/final_report/data/manu_desc.csv")
write.csv(x = non_manu_desc, file = "~/Documents/GitHub/final_report/data/non_manu_desc.csv")

## 記述統計量
out_desc1 <- df_tmp %>%
  descr(transpose = TRUE) %>%
  tb()
write.csv(x = out_desc1, file = "~/Documents/GitHub/final_report/data/all_desc.csv")

## 固定効果モデルで推定する ##
library(plm)

### 単独年 ###
single_model <- market_cap_new ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + #cash_and_cash_equivalents + #non_current_assets + 
  tangible_fixed_assets + intangible_fixed_assets + 
  #patent_right + software + goodwill + leased_asset + trademark +## sales_goodwill + design_right +
  other_intangible_fixed_assets + total_investment_and_other_assets + ##development_cost +
  current_liabilities + non_current_liabilities +
  sales_operating_revenue + financial_income + #sales_cost + 
  sales_operating_cost + unrealized_profit_on_installment_sales + #financial_costs +
  sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation +
  sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits +
  sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other +
  employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + goodwill_amortization + ##officer_bonus_provision +
  d_manufacture + d_saas + d_manufacture * d_saas +
  p_gov + p_bank + p_stockc + p_other + p_foreign + p_individual +
  d_2017 + d_2018 + d_2019 + 
  d_1 + d_2 + d_3 + d_4 + d_5 + d_6 + d_7 + d_8 + d_9 +d_10 + d_11

single_model2 <- market_cap_new ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + #cash_and_cash_equivalents + #non_current_assets + 
  tangible_fixed_assets + intangible_fixed_assets + 
  #patent_right + software + goodwill + leased_asset + trademark +## sales_goodwill + design_right +
  other_intangible_fixed_assets + total_investment_and_other_assets + ##development_cost +
  current_liabilities + non_current_liabilities +
  sales_operating_revenue + financial_income + #sales_cost + 
  sales_operating_cost + unrealized_profit_on_installment_sales + #financial_costs +
  sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation +
  sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits +
  sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other +
  employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + goodwill_amortization + ##officer_bonus_provision +
  d_manufacture + d_saas + d_manufacture * d_saas +
  p_gov + p_bank + p_stockc + p_other + p_foreign + p_individual +
  d_2017 + d_2018 + d_2019 + 
  d_1 + d_2 + d_3 + d_4 + d_5 + d_6 + d_7 + d_8 + d_9 +d_10 + d_11 +
  d_saas *(
    founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + #cash_and_cash_equivalents + #non_current_assets + 
      tangible_fixed_assets + intangible_fixed_assets + 
      #patent_right + software + goodwill + leased_asset + trademark +## sales_goodwill + design_right +
      other_intangible_fixed_assets + total_investment_and_other_assets + ##development_cost +
      current_liabilities + non_current_liabilities +
      sales_operating_revenue + financial_income + #sales_cost + 
      sales_operating_cost + unrealized_profit_on_installment_sales + #financial_costs +
      sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation +
      sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits +
      sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other +
      employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + goodwill_amortization##officer_bonus_provision +
  )
  

# データの準備
all <- pdata.frame(df_tmp, index = c("corp_name", "fiscal_year"))

## 個別効果を無視して重回帰分析する
all_ols <- lm(single_model, data=all)
all_ols2 <- lm(single_model2, data=all)
alias(all_ols) #lm(single_model2, data=all))

vif_res2 <- car::vif(all_ols)
# write.csv(x = vif_res2, file = "~/Documents/GitHub/final_report/data/vif_cor_all2.csv")

all_ols <- plm(single_model, data=all, model="pooling")
all_ols2 <- plm(single_model2, data=all, model="pooling")

summary(all_ols)
summary(fixef(all_ols))


# 固定効果モデル(LSDV(within)推定)
# filter(all, n() > 1)
all_fe <- plm(single_model, data=all, model="within")
summary(all_fe)
all_fe2 <- plm(single_model2, data=all, model="within")
summary(all_fe2)

# all_fe_log <- plm(single_model_log, data=all, model="within")

summary(all_fe)
summary(fixef(all_fe))

#F検定を行う(p値が小さければ、帰無仮説が棄却され「個別効果はある」ということになる)
pFtest(all_fe, all_ols)

# GLS推定
# all_gls <- plm(formula = single_model, data=all,model="random")#多重共線性のため結果が得られない
all_gls <- plm(formula = single_model, data=all,model="random")
summary(all_gls)

## ハウスマン検定を行う
phtest(all_fe, all_gls)

## saas企業 ##
saas <- pdata.frame(df_saas, index = c("corp_name", "fiscal_year"))
saas_fe <- plm(single_model, data=df_saas, model="within")
saas_fe2 <- plm(single_model2, data=df_saas, model="within")
# saas_fe_log <- plm(single_model_log, data=df_saas, model="within")
# summary(saas_fe)


## 非製造業 ##
non_manu <- pdata.frame(df_non_manu, index = c("corp_name", "fiscal_year"))
non_manu_fe <- plm(single_model, data=non_manu, model="within")
non_manu_fe2 <- plm(single_model2, data=non_manu, model="within")
# non_manu_fe_log <- plm(single_model_log, data=non_manu, model="within")
# summary(non_manu_fe)

## 製造業 ##
manu <- pdata.frame(df_manu, index = c("corp_name", "fiscal_year"))
manu_fe <- plm(single_model , data=manu, model="within")
manu_fe2 <- plm(single_model2 , data=manu, model="within")
# manu_fe_log <- plm(single_model_log , data=manu, model="within")
# summary(manu_fe)

library(stargazer)
## OLSの4年分の分析
stargazer(all_ols, type = "html", out = "Documents/GitHub/final_report/data/ols_ouput_4years_v2.doc")
stargazer(all_ols2, type = "html", out = "Documents/GitHub/final_report/data/ols_ouput_4years_v2.5.doc")

library(memisc)
#回帰分析の結果を比較できる形でCSVファイルに保存
ols_resultTable<- mtable(all_ols2)
write.mtable(ols_resultTable,file="~/Documents/GitHub/final_report/data/ols_ouput_4years_v2.5.csv",colsep=",")

## 固定効果の4年分の分析
stargazer(all_fe, saas_fe, manu_fe, non_manu_fe, type = "html", out = "Documents/GitHub/final_report/data/fe_ouput_4years_v2.doc")

write.csv(summary(all_fe2)$coef,file="~/Documents/GitHub/final_report/data/fe_ouput_4years_v2.5.csv")
stargazer(all_fe2, type = "html", out = "Documents/GitHub/final_report/data/fe_ouput_4years_v2.5.doc")
# stargazer(all_fe_log, saas_fe_log, manu_fe_log, non_manu_fe_log, type = "html", out = "Documents/GitHub/final_report/data/fe_log_ouput_4years.doc")


library(MASS)
all_ols_dt3_2_new_ <- lm(model_dt3, data=df_long)
step.all_ols_dt3_2_new_ <- stats::step(all_ols_dt3_2_new_) #かなり時間かかるので誤って実行しないようにコメントアウト
summaryStep.all_ols_dt3_2_new_ <- summary(step.all_ols_dt3_2_new_)

coef <- summaryStep.all_ols_dt3_2_new_$coefficients           #回帰係数
r.squared <- summaryStep.all_ols_dt3_2_new_$r.squared         #決定係数
adj.r.squared <- summaryStep.all_ols_dt3_2_new_$adj.r.squared #修正決定係数
resultTable <- cbind(coef, r.squared = r.squared, adj.r.squared = adj.r.squared)
resultTable2 <- cbind(coef)
write.csv(resultTable, "~/Documents/GitHub/final_report/data/stepwise_ols.csv")
stargazer(resultTable, type = "html", out = "Documents/GitHub/final_report/data/stepwise_ols.doc")
stargazer(resultTable2, type = "html", out = "Documents/GitHub/final_report/data/stepwise_ols2.doc")



step.all_ols2_2_new_ <- stats::step(all_ols2) #かなり時間かかるので誤って実行しないようにコメントアウト
summaryStep.all_ols2_2_new_ <- summary(step.all_ols2_2_new_)

coef <- summaryStep.all_ols2_2_new_$coefficients           #回帰係数
r.squared <- summaryStep.all_ols2_2_new_$r.squared         #決定係数
adj.r.squared <- summaryStep.all_ols2_2_new_$adj.r.squared #修正決定係数
resultTable3 <- cbind(coef, r.squared = r.squared, adj.r.squared = adj.r.squared)
resultTable4 <- cbind(coef)
stargazer(resultTable3, type = "html", out = "Documents/GitHub/final_report/data/ols_ouput_4years_stepwise.doc")
# ## 対数を取ったバージョン ※0は対数値が取れないため、0の次の最小値を足した上で対数を取る
# #参考=>https://datachemeng.com/post-4632/
# single_model_log <- log(market_cap_new) ~ log(founded_year) + log(n_consolidated_subsidiaries) + log(n_consolidated_subsidiaries_ipo) + log(current_assets) + #log(cash_and_cash_equivalents) + #log(non_current_assets) + 
#   log(tangible_fixed_assets) + log(intangible_fixed_assets) + 
#   #log(patent_right) + log(software) + log(goodwill) + log(leased_asset) + log(trademark) + #log(sales_goodwill) + log(design_right) +
#   log(other_intangible_fixed_assets) + log(total_investment_and_other_assets) + #log(development_cost) +
#   log(current_liabilities) + log(non_current_liabilities) +
#   log(sales_operating_revenue) + log(financial_income) + #log(sales_cost) + 
#   log(sales_operating_cost) + log(unrealized_profit_on_installment_sales) + #log(financial_costs) +
#   log(sga_sales_commission) + log(sga_storage_costs) + log(sga_advertising_expenses) + log(sga_sales_expansion_costs) + log(sga_allowance_for_doubtful_accounts) + log(sga_officer_compensation) +
#   log(sga_provision_for_retirement_benefits_for_officers) + log(sga_provision_for_bonuses_for_directors) + log(sga_personnel_welfare_expenses) + log(sga_provision_for_retirement_benefits) +
#   log(sga_depreciation) + log(sga_goodwill_amortization) + log(sga_rent) + log(sga_taxes_and_public_dues) + log(sga_patent_fee_paid) + log(sga_rd_cost) + log(sga_warranty_repair_costs) + log(sga_other) +
#   log(employees_end_term) + log(avg_temp_employees) + log(r_d_expenses) + log(capital_investment) + log(goodwill_amortization) + #log(officer_bonus_provision) +
#   d_manufacture + d_saas + d_manufacture * d_saas +
#   d_2017 + d_2018 + d_2019 + #d_2020 +
#   d_1 + d_2 + d_3 + d_4 + d_5 + d_6 + d_7 + d_8 + d_9 +d_10 + d_11 + # d_12 +
#   d_2017 * d_1 + d_2017 * d_2 + d_2017 * d_3 + d_2017 * d_4 + d_2017 * d_5 + d_2017 * d_6 + d_2017 * d_7 + d_2017 * d_8 + d_2017 * d_9 + d_2017 * d_10 + d_2017 * d_11 +# d_2017 * d_12 +
#   d_2018 * d_1 + d_2018 * d_2 + d_2018 * d_3 + d_2018 * d_4 + d_2018 * d_5 + d_2018 * d_6 + d_2018 * d_7 + d_2018 * d_8 + d_2018 * d_9 + d_2018 * d_10 + d_2018 * d_11 +# d_2018 * d_12 +
#   d_2019 * d_1 + d_2019 * d_2 + d_2019 * d_3 + d_2019 * d_4 + d_2019 * d_5 + d_2019 * d_6 + d_2019 * d_7 + d_2019 * d_8 + d_2019 * d_9 + d_2019 * d_10 + d_2019 * d_11 +# d_2019 * d_12 +
#   d_2020 * d_1 + d_2020 * d_2 + d_2020 * d_3 + d_2020 * d_4 + d_2020 * d_5 + d_2020 * d_6 + d_2020 * d_7 + d_2020 * d_8 + d_2020 * d_9 + d_2020 * d_10 + d_2020 * d_11 #+ d_2020 * d_12
