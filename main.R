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

#ダウンロードしたデータを取り込む
df <- read_csv("~/Documents/GitHub/final_report/data/target_final.csv")
# summary(target)


stock_info_2017_2021 <- read_csv("~/Documents/GitHub/final_report/data/stock_info_2017_2021_monthly_all.csv")
# stock_info_2017_2021 <- stock_info_2017_2021
#   mutate("dividend" = dividend_per_stock * n_stock) ##配当額を株式数に換算する

# View(stock_info_2017_2021)
basic_info_2017_2021 <- read_csv("~/Documents/GitHub/final_report/data/basic_info_2017_2021_all.csv")
# transfer_2017_2021 <- read_csv("Documents/GitHub/final_report/data/transfer_2017_2021.csv", locale = locale(encoding = "SHIFT-JIS"))


## stockデータの前処理 ##
#2021年11月・12月のデータを削除
# stock_info_2017_2021<-stock_info_2017_2021[!(stock_info_2017_2021$fiscal_year==2021 & stock_info_2017_2021$fiscal_month==11),]
# stock_info_2017_2021<-stock_info_2017_2021[!(stock_info_2017_2021$fiscal_year==2021 & stock_info_2017_2021$fiscal_month==12),]
# summary(stock_info_2017_2021)

# target

#データを結合する
# df <- dplyr::left_join(finance,stock, by = c("corp_name", "fiscal_date", "fiscal_year", "fiscal_month"))
df <- dplyr::left_join(df, basic_info_2017_2021, by = c("corp_name"))
df <- dplyr::left_join(df, stock_info_2017_2021, by = c("corp_name", "fiscal_date", "fiscal_year", "fiscal_month"))

#### データ加工　####
df <- df %>%
  mutate("market_cap_new" = market_cap / 1000000) #時価総額を、他のデータ同様に百万単位に揃える
  # mutate("simple_q" = (market_cap_new + total_debt) / total_assets) %>% 
  # mutate("roe" = net_income_consolidated / capital_attributable_to_owners_of_the_parent_company) %>% 
  # mutate("roa" = net_income_consolidated / total_assets) %>% 
  # mutate("per" = market_cap_new / net_income_consolidated)

## 相関係数を調べる
cor_all <- round(cor(select(.data = df_tmp, market_cap_new , founded_year , n_consolidated_subsidiaries , n_consolidated_subsidiaries_ipo , current_assets , cash_and_cash_equivalents ,
                            non_current_assets , tangible_fixed_assets , intangible_fixed_assets , patent_right , software , goodwill , leased_asset , trademark ,# sales_goodwill , design_right ,
                            other_intangible_fixed_assets , total_investment_and_other_assets , #development_cost ,
                            current_liabilities , non_current_liabilities ,
                            sales_operating_revenue , financial_income , sales_cost , sales_operating_cost , unrealized_profit_on_installment_sales , financial_costs ,
                            sga_sales_commission , sga_storage_costs , sga_advertising_expenses , sga_sales_expansion_costs , sga_allowance_for_doubtful_accounts , sga_officer_compensation ,
                            sga_provision_for_retirement_benefits_for_officers , sga_provision_for_bonuses_for_directors , sga_personnel_welfare_expenses , sga_provision_for_retirement_benefits ,
                            sga_depreciation , sga_goodwill_amortization , sga_rent , sga_taxes_and_public_dues , sga_patent_fee_paid , sga_rd_cost , sga_warranty_repair_costs , sga_other ,
                            employees_end_term , avg_temp_employees , r_d_expenses , capital_investment , goodwill_amortization , #officer_bonus_provision ,
                            d_manufacture , d_saas,
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

#変化分・変化率を取るために、まずはlead(lag)を取る
# for_lag <- c('current_assets','cash_and_cash_equivalents','non_current_assets','tangible_fixed_assets','intangible_fixed_assets','sales_goodwill','patent_right','software',
#               'goodwill','leased_asset','trademark','design_right','other_intangible_fixed_assets','total_investment_and_other_assets','development_cost','total_assets',
#               'current_liabilities','non_current_liabilities','total_debt','non_controlling_interests','total_liabilities_and_net_assets','capital_attributable_to_owners_of_the_parent_company',
#               'sales_operating_revenue','financial_income','sales_cost','sales_operating_cost','unrealized_profit_on_installment_sales',
#               'sga','financial_costs','employees_end_term','avg_temp_employees','r_d_expenses','capital_investment','officer_bonus_provision','goodwill_amortization',
#               'sga_sales_commission','sga_storage_costs','sga_advertising_expenses','sga_sales_expansion_costs','sga_allowance_for_doubtful_accounts','sga_officer_compensation',
#               'sga_provision_for_retirement_benefits_for_officers','sga_provision_for_bonuses_for_directors','sga_personnel_welfare_expenses','sga_provision_for_retirement_benefits',
#               'sga_depreciation','sga_goodwill_amortization','sga_rent','sga_taxes_and_public_dues','sga_patent_fee_paid','sga_rd_cost','sga_warranty_repair_costs','sga_other',
#               'gross_profit','rd_expenses','operating_income','non_operating_income','non_operating_cost','ordinary_profit','income_before_income_taxes',
#               'net_income_from_continuing_operations','net_income_from_non_continuing_operations','net_income_consolidated','market_cap',
#               'dividend','market_cap_new','simple_q','roe','roa','per')

## leadを取る
df_tmp <- df %>%
  group_by(corp_name) %>%
  mutate_all(funs(lead1 = dplyr::lead(., n=1), lead2 = dplyr::lead(., n=2), lead3 = dplyr::lead(., n=3)))
df_tmp

#変化分・変化率を取る
df_tmp <- df_tmp %>% 
  mutate('dt1_n_consolidated_subsidiaries' = n_consolidated_subsidiaries_lead1 - n_consolidated_subsidiaries) %>%
  mutate('dt1_n_consolidated_subsidiaries_ipo' = n_consolidated_subsidiaries_ipo_lead1 - n_consolidated_subsidiaries_ipo) %>%
  mutate('dt1_current_assets' = current_assets_lead1 - current_assets) %>%
  mutate('dt1_cash_and_cash_equivalents' = cash_and_cash_equivalents_lead1 - cash_and_cash_equivalents) %>%
  mutate('dt1_non_current_assets' = non_current_assets_lead1 - non_current_assets) %>%
  mutate('dt1_tangible_fixed_assets' = tangible_fixed_assets_lead1 - tangible_fixed_assets) %>%
  mutate('dt1_intangible_fixed_assets' = intangible_fixed_assets_lead1 - intangible_fixed_assets) %>%
  mutate('dt1_patent_right' = patent_right_lead1 - patent_right) %>%
  mutate('dt1_software' = software_lead1 - software) %>%
  mutate('dt1_goodwill' = goodwill_lead1 - goodwill) %>%
  mutate('dt1_leased_asset' = leased_asset_lead1 - leased_asset) %>%
  mutate('dt1_trademark' = trademark_lead1 - trademark) %>%
  mutate('dt1_other_intangible_fixed_assets' = other_intangible_fixed_assets_lead1 - other_intangible_fixed_assets) %>%
  mutate('dt1_total_investment_and_other_assets' = total_investment_and_other_assets_lead1 - total_investment_and_other_assets) %>%
  mutate('dt1_total_assets' = total_assets_lead1 - total_assets) %>%
  mutate('dt1_current_liabilities' = current_liabilities_lead1 - current_liabilities) %>%
  mutate('dt1_non_current_liabilities' = non_current_liabilities_lead1 - non_current_liabilities) %>%
  mutate('dt1_total_debt' = total_debt_lead1 - total_debt) %>%
  mutate('dt1_non_controlling_interests' = non_controlling_interests_lead1 - non_controlling_interests) %>%
  mutate('dt1_total_liabilities_and_net_assets' = total_liabilities_and_net_assets_lead1 - total_liabilities_and_net_assets) %>%
  mutate('dt1_capital_attributable_to_owners_of_the_parent_company' = capital_attributable_to_owners_of_the_parent_company_lead1 - capital_attributable_to_owners_of_the_parent_company) %>%
  mutate('dt1_sales_operating_revenue' = sales_operating_revenue_lead1 - sales_operating_revenue) %>%
  mutate('dt1_financial_income' = financial_income_lead1 - financial_income) %>%
  mutate('dt1_sales_cost' = sales_cost_lead1 - sales_cost) %>%
  mutate('dt1_sales_operating_cost' = sales_operating_cost_lead1 - sales_operating_cost) %>%
  mutate('dt1_unrealized_profit_on_installment_sales' = unrealized_profit_on_installment_sales_lead1 - unrealized_profit_on_installment_sales) %>%
  mutate('dt1_sga' = sga_lead1 - sga) %>%
  mutate('dt1_financial_costs' = financial_costs_lead1 - financial_costs) %>%
  mutate('dt1_employees_end_term' = employees_end_term_lead1 - employees_end_term) %>%
  mutate('dt1_avg_temp_employees' = avg_temp_employees_lead1 - avg_temp_employees) %>%
  mutate('dt1_r_d_expenses' = r_d_expenses_lead1 - r_d_expenses) %>%
  mutate('dt1_capital_investment' = capital_investment_lead1 - capital_investment) %>%
  mutate('dt1_goodwill_amortization' = goodwill_amortization_lead1 - goodwill_amortization) %>%
  mutate('dt1_sga_sales_commission' = sga_sales_commission_lead1 - sga_sales_commission) %>%
  mutate('dt1_sga_storage_costs' = sga_storage_costs_lead1 - sga_storage_costs) %>%
  mutate('dt1_sga_advertising_expenses' = sga_advertising_expenses_lead1 - sga_advertising_expenses) %>%
  mutate('dt1_sga_sales_expansion_costs' = sga_sales_expansion_costs_lead1 - sga_sales_expansion_costs) %>%
  mutate('dt1_sga_allowance_for_doubtful_accounts' = sga_allowance_for_doubtful_accounts_lead1 - sga_allowance_for_doubtful_accounts) %>%
  mutate('dt1_sga_officer_compensation' = sga_officer_compensation_lead1 - sga_officer_compensation) %>%
  mutate('dt1_sga_provision_for_retirement_benefits_for_officers' = sga_provision_for_retirement_benefits_for_officers_lead1 - sga_provision_for_retirement_benefits_for_officers) %>%
  mutate('dt1_sga_provision_for_bonuses_for_directors' = sga_provision_for_bonuses_for_directors_lead1 - sga_provision_for_bonuses_for_directors) %>%
  mutate('dt1_sga_personnel_welfare_expenses' = sga_personnel_welfare_expenses_lead1 - sga_personnel_welfare_expenses) %>%
  mutate('dt1_sga_provision_for_retirement_benefits' = sga_provision_for_retirement_benefits_lead1 - sga_provision_for_retirement_benefits) %>%
  mutate('dt1_sga_depreciation' = sga_depreciation_lead1 - sga_depreciation) %>%
  mutate('dt1_sga_goodwill_amortization' = sga_goodwill_amortization_lead1 - sga_goodwill_amortization) %>%
  mutate('dt1_sga_rent' = sga_rent_lead1 - sga_rent) %>%
  mutate('dt1_sga_taxes_and_public_dues' = sga_taxes_and_public_dues_lead1 - sga_taxes_and_public_dues) %>%
  mutate('dt1_sga_patent_fee_paid' = sga_patent_fee_paid_lead1 - sga_patent_fee_paid) %>%
  mutate('dt1_sga_rd_cost' = sga_rd_cost_lead1 - sga_rd_cost) %>%
  mutate('dt1_sga_warranty_repair_costs' = sga_warranty_repair_costs_lead1 - sga_warranty_repair_costs) %>%
  mutate('dt1_sga_other' = sga_other_lead1 - sga_other) %>%
  mutate('dt1_gross_profit' = gross_profit_lead1 - gross_profit) %>%
  mutate('dt1_rd_expenses' = rd_expenses_lead1 - rd_expenses) %>%
  mutate('dt1_operating_income' = operating_income_lead1 - operating_income) %>%
  mutate('dt1_non_operating_income' = non_operating_income_lead1 - non_operating_income) %>%
  mutate('dt1_non_operating_cost' = non_operating_cost_lead1 - non_operating_cost) %>%
  mutate('dt1_ordinary_profit' = ordinary_profit_lead1 - ordinary_profit) %>%
  mutate('dt1_income_before_income_taxes' = income_before_income_taxes_lead1 - income_before_income_taxes) %>%
  mutate('dt1_net_income_from_continuing_operations' = net_income_from_continuing_operations_lead1 - net_income_from_continuing_operations) %>%
  mutate('dt1_net_income_from_non_continuing_operations' = net_income_from_non_continuing_operations_lead1 - net_income_from_non_continuing_operations) %>%
  mutate('dt1_net_income_consolidated' = net_income_consolidated_lead1 - net_income_consolidated) %>%
  mutate('dt1_dividend' = dividend_lead1 - dividend) %>%
  mutate('dt1_market_cap_new' = market_cap_new_lead1 - market_cap_new) %>%
  
  mutate('dt2_n_consolidated_subsidiaries' = n_consolidated_subsidiaries_lead2 - n_consolidated_subsidiaries) %>%
  mutate('dt2_n_consolidated_subsidiaries_ipo' = n_consolidated_subsidiaries_ipo_lead2 - n_consolidated_subsidiaries_ipo) %>%
  mutate('dt2_current_assets' = current_assets_lead2 - current_assets) %>%
  mutate('dt2_cash_and_cash_equivalents' = cash_and_cash_equivalents_lead2 - cash_and_cash_equivalents) %>%
  mutate('dt2_non_current_assets' = non_current_assets_lead2 - non_current_assets) %>%
  mutate('dt2_tangible_fixed_assets' = tangible_fixed_assets_lead2 - tangible_fixed_assets) %>%
  mutate('dt2_intangible_fixed_assets' = intangible_fixed_assets_lead2 - intangible_fixed_assets) %>%
  mutate('dt2_patent_right' = patent_right_lead2 - patent_right) %>%
  mutate('dt2_software' = software_lead2 - software) %>%
  mutate('dt2_goodwill' = goodwill_lead2 - goodwill) %>%
  mutate('dt2_leased_asset' = leased_asset_lead2 - leased_asset) %>%
  mutate('dt2_trademark' = trademark_lead2 - trademark) %>%
  mutate('dt2_other_intangible_fixed_assets' = other_intangible_fixed_assets_lead2 - other_intangible_fixed_assets) %>%
  mutate('dt2_total_investment_and_other_assets' = total_investment_and_other_assets_lead2 - total_investment_and_other_assets) %>%
  mutate('dt2_total_assets' = total_assets_lead2 - total_assets) %>%
  mutate('dt2_current_liabilities' = current_liabilities_lead2 - current_liabilities) %>%
  mutate('dt2_non_current_liabilities' = non_current_liabilities_lead2 - non_current_liabilities) %>%
  mutate('dt2_total_debt' = total_debt_lead2 - total_debt) %>%
  mutate('dt2_non_controlling_interests' = non_controlling_interests_lead2 - non_controlling_interests) %>%
  mutate('dt2_total_liabilities_and_net_assets' = total_liabilities_and_net_assets_lead2 - total_liabilities_and_net_assets) %>%
  mutate('dt2_capital_attributable_to_owners_of_the_parent_company' = capital_attributable_to_owners_of_the_parent_company_lead2 - capital_attributable_to_owners_of_the_parent_company) %>%
  mutate('dt2_sales_operating_revenue' = sales_operating_revenue_lead2 - sales_operating_revenue) %>%
  mutate('dt2_financial_income' = financial_income_lead2 - financial_income) %>%
  mutate('dt2_sales_cost' = sales_cost_lead2 - sales_cost) %>%
  mutate('dt2_sales_operating_cost' = sales_operating_cost_lead2 - sales_operating_cost) %>%
  mutate('dt2_unrealized_profit_on_installment_sales' = unrealized_profit_on_installment_sales_lead2 - unrealized_profit_on_installment_sales) %>%
  mutate('dt2_sga' = sga_lead2 - sga) %>%
  mutate('dt2_financial_costs' = financial_costs_lead2 - financial_costs) %>%
  mutate('dt2_employees_end_term' = employees_end_term_lead2 - employees_end_term) %>%
  mutate('dt2_avg_temp_employees' = avg_temp_employees_lead2 - avg_temp_employees) %>%
  mutate('dt2_r_d_expenses' = r_d_expenses_lead2 - r_d_expenses) %>%
  mutate('dt2_capital_investment' = capital_investment_lead2 - capital_investment) %>%
  mutate('dt2_goodwill_amortization' = goodwill_amortization_lead2 - goodwill_amortization) %>%
  mutate('dt2_sga_sales_commission' = sga_sales_commission_lead2 - sga_sales_commission) %>%
  mutate('dt2_sga_storage_costs' = sga_storage_costs_lead2 - sga_storage_costs) %>%
  mutate('dt2_sga_advertising_expenses' = sga_advertising_expenses_lead2 - sga_advertising_expenses) %>%
  mutate('dt2_sga_sales_expansion_costs' = sga_sales_expansion_costs_lead2 - sga_sales_expansion_costs) %>%
  mutate('dt2_sga_allowance_for_doubtful_accounts' = sga_allowance_for_doubtful_accounts_lead2 - sga_allowance_for_doubtful_accounts) %>%
  mutate('dt2_sga_officer_compensation' = sga_officer_compensation_lead2 - sga_officer_compensation) %>%
  mutate('dt2_sga_provision_for_retirement_benefits_for_officers' = sga_provision_for_retirement_benefits_for_officers_lead2 - sga_provision_for_retirement_benefits_for_officers) %>%
  mutate('dt2_sga_provision_for_bonuses_for_directors' = sga_provision_for_bonuses_for_directors_lead2 - sga_provision_for_bonuses_for_directors) %>%
  mutate('dt2_sga_personnel_welfare_expenses' = sga_personnel_welfare_expenses_lead2 - sga_personnel_welfare_expenses) %>%
  mutate('dt2_sga_provision_for_retirement_benefits' = sga_provision_for_retirement_benefits_lead2 - sga_provision_for_retirement_benefits) %>%
  mutate('dt2_sga_depreciation' = sga_depreciation_lead2 - sga_depreciation) %>%
  mutate('dt2_sga_goodwill_amortization' = sga_goodwill_amortization_lead2 - sga_goodwill_amortization) %>%
  mutate('dt2_sga_rent' = sga_rent_lead2 - sga_rent) %>%
  mutate('dt2_sga_taxes_and_public_dues' = sga_taxes_and_public_dues_lead2 - sga_taxes_and_public_dues) %>%
  mutate('dt2_sga_patent_fee_paid' = sga_patent_fee_paid_lead2 - sga_patent_fee_paid) %>%
  mutate('dt2_sga_rd_cost' = sga_rd_cost_lead2 - sga_rd_cost) %>%
  mutate('dt2_sga_warranty_repair_costs' = sga_warranty_repair_costs_lead2 - sga_warranty_repair_costs) %>%
  mutate('dt2_sga_other' = sga_other_lead2 - sga_other) %>%
  mutate('dt2_gross_profit' = gross_profit_lead2 - gross_profit) %>%
  mutate('dt2_rd_expenses' = rd_expenses_lead2 - rd_expenses) %>%
  mutate('dt2_operating_income' = operating_income_lead2 - operating_income) %>%
  mutate('dt2_non_operating_income' = non_operating_income_lead2 - non_operating_income) %>%
  mutate('dt2_non_operating_cost' = non_operating_cost_lead2 - non_operating_cost) %>%
  mutate('dt2_ordinary_profit' = ordinary_profit_lead2 - ordinary_profit) %>%
  mutate('dt2_income_before_income_taxes' = income_before_income_taxes_lead2 - income_before_income_taxes) %>%
  mutate('dt2_net_income_from_continuing_operations' = net_income_from_continuing_operations_lead2 - net_income_from_continuing_operations) %>%
  mutate('dt2_net_income_from_non_continuing_operations' = net_income_from_non_continuing_operations_lead2 - net_income_from_non_continuing_operations) %>%
  mutate('dt2_net_income_consolidated' = net_income_consolidated_lead2 - net_income_consolidated) %>%
  mutate('dt2_dividend' = dividend_lead2 - dividend) %>%
  mutate('dt2_market_cap_new' = market_cap_new_lead2 - market_cap_new) %>%
  
  mutate('dt3_n_consolidated_subsidiaries' = n_consolidated_subsidiaries_lead3 - n_consolidated_subsidiaries) %>%
  mutate('dt3_n_consolidated_subsidiaries_ipo' = n_consolidated_subsidiaries_ipo_lead3 - n_consolidated_subsidiaries_ipo) %>%
  mutate('dt3_current_assets' = current_assets_lead3 - current_assets) %>%
  mutate('dt3_cash_and_cash_equivalents' = cash_and_cash_equivalents_lead3 - cash_and_cash_equivalents) %>%
  mutate('dt3_non_current_assets' = non_current_assets_lead3 - non_current_assets) %>%
  mutate('dt3_tangible_fixed_assets' = tangible_fixed_assets_lead3 - tangible_fixed_assets) %>%
  mutate('dt3_intangible_fixed_assets' = intangible_fixed_assets_lead3 - intangible_fixed_assets) %>%
  mutate('dt3_patent_right' = patent_right_lead3 - patent_right) %>%
  mutate('dt3_software' = software_lead3 - software) %>%
  mutate('dt3_goodwill' = goodwill_lead3 - goodwill) %>%
  mutate('dt3_leased_asset' = leased_asset_lead3 - leased_asset) %>%
  mutate('dt3_trademark' = trademark_lead3 - trademark) %>%
  mutate('dt3_other_intangible_fixed_assets' = other_intangible_fixed_assets_lead3 - other_intangible_fixed_assets) %>%
  mutate('dt3_total_investment_and_other_assets' = total_investment_and_other_assets_lead3 - total_investment_and_other_assets) %>%
  mutate('dt3_total_assets' = total_assets_lead3 - total_assets) %>%
  mutate('dt3_current_liabilities' = current_liabilities_lead3 - current_liabilities) %>%
  mutate('dt3_non_current_liabilities' = non_current_liabilities_lead3 - non_current_liabilities) %>%
  mutate('dt3_total_debt' = total_debt_lead3 - total_debt) %>%
  mutate('dt3_non_controlling_interests' = non_controlling_interests_lead3 - non_controlling_interests) %>%
  mutate('dt3_total_liabilities_and_net_assets' = total_liabilities_and_net_assets_lead3 - total_liabilities_and_net_assets) %>%
  mutate('dt3_capital_attributable_to_owners_of_the_parent_company' = capital_attributable_to_owners_of_the_parent_company_lead3 - capital_attributable_to_owners_of_the_parent_company) %>%
  mutate('dt3_sales_operating_revenue' = sales_operating_revenue_lead3 - sales_operating_revenue) %>%
  mutate('dt3_financial_income' = financial_income_lead3 - financial_income) %>%
  mutate('dt3_sales_cost' = sales_cost_lead3 - sales_cost) %>%
  mutate('dt3_sales_operating_cost' = sales_operating_cost_lead3 - sales_operating_cost) %>%
  mutate('dt3_unrealized_profit_on_installment_sales' = unrealized_profit_on_installment_sales_lead3 - unrealized_profit_on_installment_sales) %>%
  mutate('dt3_sga' = sga_lead3 - sga) %>%
  mutate('dt3_financial_costs' = financial_costs_lead3 - financial_costs) %>%
  mutate('dt3_employees_end_term' = employees_end_term_lead3 - employees_end_term) %>%
  mutate('dt3_avg_temp_employees' = avg_temp_employees_lead3 - avg_temp_employees) %>%
  mutate('dt3_r_d_expenses' = r_d_expenses_lead3 - r_d_expenses) %>%
  mutate('dt3_capital_investment' = capital_investment_lead3 - capital_investment) %>%
  mutate('dt3_goodwill_amortization' = goodwill_amortization_lead3 - goodwill_amortization) %>%
  mutate('dt3_sga_sales_commission' = sga_sales_commission_lead3 - sga_sales_commission) %>%
  mutate('dt3_sga_storage_costs' = sga_storage_costs_lead3 - sga_storage_costs) %>%
  mutate('dt3_sga_advertising_expenses' = sga_advertising_expenses_lead3 - sga_advertising_expenses) %>%
  mutate('dt3_sga_sales_expansion_costs' = sga_sales_expansion_costs_lead3 - sga_sales_expansion_costs) %>%
  mutate('dt3_sga_allowance_for_doubtful_accounts' = sga_allowance_for_doubtful_accounts_lead3 - sga_allowance_for_doubtful_accounts) %>%
  mutate('dt3_sga_officer_compensation' = sga_officer_compensation_lead3 - sga_officer_compensation) %>%
  mutate('dt3_sga_provision_for_retirement_benefits_for_officers' = sga_provision_for_retirement_benefits_for_officers_lead3 - sga_provision_for_retirement_benefits_for_officers) %>%
  mutate('dt3_sga_provision_for_bonuses_for_directors' = sga_provision_for_bonuses_for_directors_lead3 - sga_provision_for_bonuses_for_directors) %>%
  mutate('dt3_sga_personnel_welfare_expenses' = sga_personnel_welfare_expenses_lead3 - sga_personnel_welfare_expenses) %>%
  mutate('dt3_sga_provision_for_retirement_benefits' = sga_provision_for_retirement_benefits_lead3 - sga_provision_for_retirement_benefits) %>%
  mutate('dt3_sga_depreciation' = sga_depreciation_lead3 - sga_depreciation) %>%
  mutate('dt3_sga_goodwill_amortization' = sga_goodwill_amortization_lead3 - sga_goodwill_amortization) %>%
  mutate('dt3_sga_rent' = sga_rent_lead3 - sga_rent) %>%
  mutate('dt3_sga_taxes_and_public_dues' = sga_taxes_and_public_dues_lead3 - sga_taxes_and_public_dues) %>%
  mutate('dt3_sga_patent_fee_paid' = sga_patent_fee_paid_lead3 - sga_patent_fee_paid) %>%
  mutate('dt3_sga_rd_cost' = sga_rd_cost_lead3 - sga_rd_cost) %>%
  mutate('dt3_sga_warranty_repair_costs' = sga_warranty_repair_costs_lead3 - sga_warranty_repair_costs) %>%
  mutate('dt3_sga_other' = sga_other_lead3 - sga_other) %>%
  mutate('dt3_gross_profit' = gross_profit_lead3 - gross_profit) %>%
  mutate('dt3_rd_expenses' = rd_expenses_lead3 - rd_expenses) %>%
  mutate('dt3_operating_income' = operating_income_lead3 - operating_income) %>%
  mutate('dt3_non_operating_income' = non_operating_income_lead3 - non_operating_income) %>%
  mutate('dt3_non_operating_cost' = non_operating_cost_lead3 - non_operating_cost) %>%
  mutate('dt3_ordinary_profit' = ordinary_profit_lead3 - ordinary_profit) %>%
  mutate('dt3_income_before_income_taxes' = income_before_income_taxes_lead3 - income_before_income_taxes) %>%
  mutate('dt3_net_income_from_continuing_operations' = net_income_from_continuing_operations_lead3 - net_income_from_continuing_operations) %>%
  mutate('dt3_net_income_from_non_continuing_operations' = net_income_from_non_continuing_operations_lead3 - net_income_from_non_continuing_operations) %>%
  mutate('dt3_net_income_consolidated' = net_income_consolidated_lead3 - net_income_consolidated) %>%
  mutate('dt3_dividend' = dividend_lead3 - dividend) %>%
  mutate('dt3_market_cap_new' = market_cap_new_lead3 - market_cap_new)

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
df_non_manu <- df_tmp[!(df_tmp$d_manufacture==0),]

## df_shortについて製造業と非製造業、saas企業に分ける ##
df_short[is.na(df_short)] <- 0
df_saas_short <- df_short[(df_short$d_saas==1),]
df_manu_short <- df_short[(df_short$d_manufacture==1),]
df_non_manu_short <- df_short[!(df_short$d_manufacture==0),]

## df_shortについて製造業と非製造業、saas企業に分ける ##
df_middle[is.na(df_middle)] <- 0
df_saas_middle <- df_middle[(df_middle$d_saas==1),]
df_manu_middle <- df_middle[(df_middle$d_manufacture==1),]
df_non_manu_middle <- df_middle[!(df_middle$d_manufacture==0),]

## df_longについて製造業と非製造業、saas企業に分ける ##
df_long[is.na(df_long)] <- 0
df_saas_long <- df_long[(df_long$d_saas==1),]
df_manu_long <- df_long[(df_long$d_manufacture==1),]
df_non_manu_long <- df_long[!(df_long$d_manufacture==0),]

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

## 3年分のデータについて(基本的には、lagをとっているだけなので省略)
# short_saas_desc <- df_saas_short %>% 
#   descr(transpose = TRUE) %>% 
#   tb()
# 
# short_manu_desc <- df_manu_short %>% 
#   descr(transpose = TRUE) %>% 
#   tb()
# 
# short_non_manu_desc <- df_non_manu_short %>% 
#   descr(transpose = TRUE) %>% 
#   tb()
# 
# write.csv(x = short_saas_desc, file = "~/Documents/GitHub/final_report/data/short_saas_desc.csv")
# write.csv(x = short_manu_desc, file = "~/Documents/GitHub/final_report/data/short_manu_desc.csv")
# write.csv(x = short_non_manu_desc, file = "~/Documents/GitHub/final_report/data/short_non_manu_desc.csv")

out_desc1 <- df_tmp %>% 
  descr(transpose = TRUE) %>% 
  tb()
write.csv(x = out_desc1, file = "~/Documents/GitHub/final_report/data/all_desc.csv")

## 固定効果モデルで推定する ##
library(plm)

### 単独年 ###
single_model <- market_cap_new ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + cash_and_cash_equivalents + non_current_assets + 
  tangible_fixed_assets + intangible_fixed_assets + patent_right + software + goodwill + leased_asset + trademark +## sales_goodwill + design_right +
  other_intangible_fixed_assets + total_investment_and_other_assets + ##development_cost +
  current_liabilities + non_current_liabilities +
  sales_operating_revenue + financial_income + sales_cost + sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs +
  sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation +
  sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits +
  sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other +
  employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + goodwill_amortization + ##officer_bonus_provision +
  d_manufacture + d_saas + d_manufacture * d_saas +
  d_2017 + d_2018 + d_2019 + 
  d_1 + d_2 + d_3 + d_4 + d_5 + d_6 + d_7 + d_8 + d_9 +d_10 + d_11 + 
  d_2017 * d_1 + d_2017 * d_2 + d_2017 * d_3 + d_2017 * d_4 + d_2017 * d_5 + d_2017 * d_6 + d_2017 * d_7 + d_2017 * d_8 + d_2017 * d_9 + d_2017 * d_10 + d_2017 * d_11 +
  d_2018 * d_1 + d_2018 * d_2 + d_2018 * d_3 + d_2018 * d_4 + d_2018 * d_5 + d_2018 * d_6 + d_2018 * d_7 + d_2018 * d_8 + d_2018 * d_9 + d_2018 * d_10 + d_2018 * d_11 +
  d_2019 * d_1 + d_2019 * d_2 + d_2019 * d_3 + d_2019 * d_4 + d_2019 * d_5 + d_2019 * d_6 + d_2019 * d_7 + d_2019 * d_8 + d_2019 * d_9 + d_2019 * d_10 + d_2019 * d_11 +
  d_2020 * d_1 + d_2020 * d_2 + d_2020 * d_3 + d_2020 * d_4 + d_2020 * d_5 + d_2020 * d_6 + d_2020 * d_7 + d_2020 * d_8 + d_2020 * d_9 + d_2020 * d_10 + d_2020 * d_11 

### 単独年 ###
single_model2 <- market_cap_new ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + cash_and_cash_equivalents + #non_current_assets + 
  tangible_fixed_assets + intangible_fixed_assets + patent_right + software + goodwill + leased_asset + trademark +## sales_goodwill + design_right +
  other_intangible_fixed_assets + total_investment_and_other_assets + ##development_cost +
  current_liabilities + non_current_liabilities +
  sales_operating_revenue + financial_income + #sales_cost + 
  sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs +
  sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation +
  sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits +
  sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other +
  employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + goodwill_amortization + ##officer_bonus_provision +
  d_manufacture + d_saas + d_manufacture * d_saas +
  d_2017 + d_2018 + d_2019 + 
  d_1 + d_2 + d_3 + d_4 + d_5 + d_6 + d_7 + d_8 + d_9 +d_10 + d_11 #+ 
  #d_2017 * d_1 + d_2017 * d_2 + d_2017 * d_3 + d_2017 * d_4 + d_2017 * d_5 + d_2017 * d_6 + d_2017 * d_7 + d_2017 * d_8 + d_2017 * d_9 + d_2017 * d_10 + d_2017 * d_11 +
  #d_2018 * d_1 + d_2018 * d_2 + d_2018 * d_3 + d_2018 * d_4 + d_2018 * d_5 + d_2018 * d_6 + d_2018 * d_7 + d_2018 * d_8 + d_2018 * d_9 + d_2018 * d_10 + d_2018 * d_11 +
  #d_2019 * d_1 + d_2019 * d_2 + d_2019 * d_3 + d_2019 * d_4 + d_2019 * d_5 + d_2019 * d_6 + d_2019 * d_7 + d_2019 * d_8 + d_2019 * d_9 + d_2019 * d_10 + d_2019 * d_11 +
  #d_2020 * d_1 + d_2020 * d_2 + d_2020 * d_3 + d_2020 * d_4 + d_2020 * d_5 + d_2020 * d_6 + d_2020 * d_7 + d_2020 * d_8 + d_2020 * d_9 + d_2020 * d_10 + d_2020 * d_11 

## 対数を取ったバージョン ※0は対数値が取れないため、0の次の最小値を足した上で対数を取る
#参考=>https://datachemeng.com/post-4632/
single_model_log <- log(market_cap_new) ~ log(founded_year) + log(n_consolidated_subsidiaries) + log(n_consolidated_subsidiaries_ipo) + log(current_assets) + log(cash_and_cash_equivalents) +
  log(non_current_assets) + log(tangible_fixed_assets) + log(intangible_fixed_assets) + log(patent_right) + log(software) + log(goodwill) + log(leased_asset) + log(trademark) + #log(sales_goodwill) + log(design_right) +
  log(other_intangible_fixed_assets) + log(total_investment_and_other_assets) + #log(development_cost) +
  log(current_liabilities) + log(non_current_liabilities) +
  log(sales_operating_revenue) + log(financial_income) + log(sales_cost) + log(sales_operating_cost) + log(unrealized_profit_on_installment_sales) + log(financial_costs) +
  log(sga_sales_commission) + log(sga_storage_costs) + log(sga_advertising_expenses) + log(sga_sales_expansion_costs) + log(sga_allowance_for_doubtful_accounts) + log(sga_officer_compensation) +
  log(sga_provision_for_retirement_benefits_for_officers) + log(sga_provision_for_bonuses_for_directors) + log(sga_personnel_welfare_expenses) + log(sga_provision_for_retirement_benefits) +
  log(sga_depreciation) + log(sga_goodwill_amortization) + log(sga_rent) + log(sga_taxes_and_public_dues) + log(sga_patent_fee_paid) + log(sga_rd_cost) + log(sga_warranty_repair_costs) + log(sga_other) +
  log(employees_end_term) + log(avg_temp_employees) + log(r_d_expenses) + log(capital_investment) + log(goodwill_amortization) + #log(officer_bonus_provision) +
  d_manufacture + d_saas + d_manufacture * d_saas +
  d_2017 + d_2018 + d_2019 + #d_2020 +
  d_1 + d_2 + d_3 + d_4 + d_5 + d_6 + d_7 + d_8 + d_9 +d_10 + d_11 + # d_12 +
  d_2017 * d_1 + d_2017 * d_2 + d_2017 * d_3 + d_2017 * d_4 + d_2017 * d_5 + d_2017 * d_6 + d_2017 * d_7 + d_2017 * d_8 + d_2017 * d_9 + d_2017 * d_10 + d_2017 * d_11 +# d_2017 * d_12 +
  d_2018 * d_1 + d_2018 * d_2 + d_2018 * d_3 + d_2018 * d_4 + d_2018 * d_5 + d_2018 * d_6 + d_2018 * d_7 + d_2018 * d_8 + d_2018 * d_9 + d_2018 * d_10 + d_2018 * d_11 +# d_2018 * d_12 +
  d_2019 * d_1 + d_2019 * d_2 + d_2019 * d_3 + d_2019 * d_4 + d_2019 * d_5 + d_2019 * d_6 + d_2019 * d_7 + d_2019 * d_8 + d_2019 * d_9 + d_2019 * d_10 + d_2019 * d_11 +# d_2019 * d_12 +
  d_2020 * d_1 + d_2020 * d_2 + d_2020 * d_3 + d_2020 * d_4 + d_2020 * d_5 + d_2020 * d_6 + d_2020 * d_7 + d_2020 * d_8 + d_2020 * d_9 + d_2020 * d_10 + d_2020 * d_11 #+ d_2020 * d_12

## 複数年の変化を比較する（1年後） ##
# fe_model_01 <- dt_market_cap_new_01 ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + 
#                 dt_current_assets_01 + dt_cash_and_cash_equivalents_01 +
#                 dt_non_current_assets_01 + dt_tangible_fixed_assets_01 + dt_intangible_fixed_assets_01 + dt_sales_goodwill_01 + 
#                 dt_patent_right_01 + dt_software_01 + dt_goodwill_01 + dt_leased_asset_01 + dt_trademark_01 + 
#                 dt_design_right_01 + dt_other_intangible_fixed_assets_01 + dt_total_investment_and_other_assets_01 + dt_development_cost_01 + 
#                 total_assets +
#                 dt_current_liabilities_01 + dt_non_current_liabilities_01 +
#                 total_debt +
#                 sales_operating_revenue + financial_income + sales_cost + sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs + 
#                 dt_sales_operating_revenue_01 + dt_financial_income_01 + dt_sales_cost_01 + dt_sales_operating_cost_01 + dt_unrealized_profit_on_installment_sales_01 + dt_financial_costs_01 + 
#                 sga +
#                 dt_sga_sales_commission_01 + dt_sga_storage_costs_01 + dt_sga_advertising_expenses_01 + dt_sga_sales_expansion_costs_01 + dt_sga_allowance_for_doubtful_accounts_01 + dt_sga_officer_compensation_01 + 
#                 dt_sga_provision_for_retirement_benefits_for_officers_01 + dt_sga_provision_for_bonuses_for_directors_01 + dt_sga_personnel_welfare_expenses_01 + dt_sga_provision_for_retirement_benefits_01 + 
#                 dt_sga_depreciation_01 + dt_sga_goodwill_amortization_01 + dt_sga_rent_01 + dt_sga_taxes_and_public_dues_01 + dt_sga_patent_fee_paid_01 + dt_sga_rd_cost_01 + dt_sga_warranty_repair_costs_01 + dt_sga_other_01 + 
#                 employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + officer_bonus_provision + goodwill_amortization + 
#                 dt_employees_end_term_01 + dt_avg_temp_employees_01 + dt_r_d_expenses_01 + dt_capital_investment_01 + dt_officer_bonus_provision_01 + dt_goodwill_amortization_01 + 
#                 d_2018 + d_2019

fe_model_dt1_ <- market_cap_new ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + cash_and_cash_equivalents + non_current_assets + 
  tangible_fixed_assets + intangible_fixed_assets + patent_right + software + goodwill + leased_asset + trademark +# sales_goodwill + design_right +
  other_intangible_fixed_assets + total_investment_and_other_assets + #development_cost +
  current_liabilities + non_current_liabilities +
  sales_operating_revenue + financial_income + sales_cost + sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs +
  sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation +
  sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits +
  sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other +
  employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + goodwill_amortization + #officer_bonus_provision +
  d_manufacture + d_saas + d_manufacture * d_saas +
  d_2018 + d_2019 + #d_2020 +
  d_1 + d_2 + d_3 + d_4 + d_5 + d_6 + d_7 + d_8 + d_9 +d_10 + d_11 + # d_12 +
  d_2018 * d_1 + d_2018 * d_2 + d_2018 * d_3 + d_2018 * d_4 + d_2018 * d_5 + d_2018 * d_6 + d_2018 * d_7 + d_2018 * d_8 + d_2018 * d_9 + d_2018 * d_10 + d_2018 * d_11 +# d_2018 * d_12 +
  d_2019 * d_1 + d_2019 * d_2 + d_2019 * d_3 + d_2019 * d_4 + d_2019 * d_5 + d_2019 * d_6 + d_2019 * d_7 + d_2019 * d_8 + d_2019 * d_9 + d_2019 * d_10 + d_2019 * d_11 +# d_2019 * d_12 +
  d_2020 * d_1 + d_2020 * d_2 + d_2020 * d_3 + d_2020 * d_4 + d_2020 * d_5 + d_2020 * d_6 + d_2020 * d_7 + d_2020 * d_8 + d_2020 * d_9 + d_2020 * d_10 + d_2020 * d_11 +#+ d_2020 * d_12
  #dt1_n_consolidated_subsidiaries + dt1_n_consolidated_subsidiaries_ipo + #入れ忘れ
  dt1_current_assets + dt1_cash_and_cash_equivalents + dt1_non_current_assets + dt1_tangible_fixed_assets + dt1_intangible_fixed_assets + dt1_patent_right + dt1_software + dt1_goodwill + dt1_leased_asset +
  dt1_trademark + dt1_other_intangible_fixed_assets + dt1_total_investment_and_other_assets + dt1_total_assets + dt1_current_liabilities + dt1_non_current_liabilities + dt1_total_debt + dt1_non_controlling_interests +
  dt1_total_liabilities_and_net_assets + dt1_capital_attributable_to_owners_of_the_parent_company + dt1_sales_operating_revenue + dt1_financial_income + dt1_sales_cost + dt1_sales_operating_cost + dt1_unrealized_profit_on_installment_sales +
  dt1_sga + dt1_financial_costs + dt1_employees_end_term + dt1_avg_temp_employees + dt1_r_d_expenses + dt1_capital_investment + dt1_goodwill_amortization + dt1_sga_sales_commission + dt1_sga_storage_costs + dt1_sga_advertising_expenses +
  dt1_sga_sales_expansion_costs + dt1_sga_allowance_for_doubtful_accounts + dt1_sga_officer_compensation + dt1_sga_provision_for_retirement_benefits_for_officers + dt1_sga_provision_for_bonuses_for_directors + dt1_sga_personnel_welfare_expenses +
  dt1_sga_provision_for_retirement_benefits + dt1_sga_depreciation + dt1_sga_goodwill_amortization + dt1_sga_rent + dt1_sga_taxes_and_public_dues + dt1_sga_patent_fee_paid + dt1_sga_rd_cost + dt1_sga_warranty_repair_costs + dt1_sga_other +
  dt1_gross_profit + dt1_rd_expenses + dt1_operating_income + dt1_non_operating_income + dt1_non_operating_cost + dt1_ordinary_profit + dt1_income_before_income_taxes + dt1_net_income_from_continuing_operations +
  dt1_net_income_from_non_continuing_operations + dt1_net_income_consolidated + dt1_dividend

fe_model_dt1_2_ <- market_cap_new ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + cash_and_cash_equivalents + #non_current_assets + 
  tangible_fixed_assets + intangible_fixed_assets + patent_right + software + goodwill + leased_asset + trademark + ##sales_goodwill + design_right +
  other_intangible_fixed_assets + total_investment_and_other_assets + ##development_cost +
  current_liabilities + non_current_liabilities +
  sales_operating_revenue + financial_income + #sales_cost + 
  sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs +
  sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation +
  sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits +
  sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other +
  employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + goodwill_amortization + ##officer_bonus_provision +
  d_manufacture + d_saas + d_manufacture * d_saas +
  d_2018 + d_2019 + ##d_2020 +
  d_1 + d_2 + d_3 + d_4 + d_5 + d_6 + d_7 + d_8 + d_9 +d_10 + d_11 + ##d_12 +
  #d_2018 * d_1 + d_2018 * d_2 + d_2018 * d_3 + d_2018 * d_4 + d_2018 * d_5 + d_2018 * d_6 + d_2018 * d_7 + d_2018 * d_8 + d_2018 * d_9 + d_2018 * d_10 + d_2018 * d_11 + ##d_2018 * d_12 +
  #d_2019 * d_1 + d_2019 * d_2 + d_2019 * d_3 + d_2019 * d_4 + d_2019 * d_5 + d_2019 * d_6 + d_2019 * d_7 + d_2019 * d_8 + d_2019 * d_9 + d_2019 * d_10 + d_2019 * d_11 + ##d_2019 * d_12 +
  #d_2020 * d_1 + d_2020 * d_2 + d_2020 * d_3 + d_2020 * d_4 + d_2020 * d_5 + d_2020 * d_6 + d_2020 * d_7 + d_2020 * d_8 + d_2020 * d_9 + d_2020 * d_10 + d_2020 * d_11 + ##+ d_2020 * d_12
  dt1_n_consolidated_subsidiaries + dt1_n_consolidated_subsidiaries_ipo +
  dt1_current_assets + dt1_cash_and_cash_equivalents + #dt1_non_current_assets + 
  dt1_tangible_fixed_assets + dt1_intangible_fixed_assets + dt1_patent_right + dt1_software + dt1_goodwill + dt1_leased_asset +
  dt1_trademark + dt1_other_intangible_fixed_assets + dt1_total_investment_and_other_assets + dt1_total_assets + dt1_current_liabilities + dt1_non_current_liabilities + dt1_total_debt + dt1_non_controlling_interests +
  dt1_total_liabilities_and_net_assets + dt1_capital_attributable_to_owners_of_the_parent_company + dt1_sales_operating_revenue + dt1_financial_income + #dt1_sales_cost + 
  dt1_sales_operating_cost + dt1_unrealized_profit_on_installment_sales + #dt1_sga + 
  dt1_financial_costs + dt1_employees_end_term + dt1_avg_temp_employees + dt1_r_d_expenses + dt1_capital_investment + dt1_goodwill_amortization + dt1_sga_sales_commission + dt1_sga_storage_costs + dt1_sga_advertising_expenses +
  dt1_sga_sales_expansion_costs + dt1_sga_allowance_for_doubtful_accounts + dt1_sga_officer_compensation + dt1_sga_provision_for_retirement_benefits_for_officers + dt1_sga_provision_for_bonuses_for_directors + dt1_sga_personnel_welfare_expenses +
  dt1_sga_provision_for_retirement_benefits + dt1_sga_depreciation + dt1_sga_goodwill_amortization + dt1_sga_rent + dt1_sga_taxes_and_public_dues + dt1_sga_patent_fee_paid + dt1_sga_rd_cost + dt1_sga_warranty_repair_costs + dt1_sga_other +
  dt1_gross_profit + dt1_rd_expenses + dt1_operating_income + dt1_non_operating_income + dt1_non_operating_cost + dt1_ordinary_profit + dt1_income_before_income_taxes + dt1_net_income_from_continuing_operations +
  dt1_net_income_from_non_continuing_operations + dt1_net_income_consolidated + dt1_dividend

fe_model_dt2_2_ <- market_cap_new ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + cash_and_cash_equivalents + #non_current_assets + 
  tangible_fixed_assets + intangible_fixed_assets + patent_right + software + goodwill + leased_asset + trademark + ##sales_goodwill + design_right +
  other_intangible_fixed_assets + total_investment_and_other_assets + ##development_cost +
  current_liabilities + non_current_liabilities +
  sales_operating_revenue + financial_income + #sales_cost + 
  sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs +
  sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation +
  sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits +
  sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other +
  employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + goodwill_amortization + ##officer_bonus_provision +
  d_manufacture + d_saas + d_manufacture * d_saas +
  d_2018 + #d_2019 + ##d_2020 +
  d_1 + d_2 + d_3 + d_4 + d_5 + d_6 + d_7 + d_8 + d_9 +d_10 + d_11 + ##d_12 +
  #d_2018 * d_1 + d_2018 * d_2 + d_2018 * d_3 + d_2018 * d_4 + d_2018 * d_5 + d_2018 * d_6 + d_2018 * d_7 + d_2018 * d_8 + d_2018 * d_9 + d_2018 * d_10 + d_2018 * d_11 + ##d_2018 * d_12 +
  #d_2019 * d_1 + d_2019 * d_2 + d_2019 * d_3 + d_2019 * d_4 + d_2019 * d_5 + d_2019 * d_6 + d_2019 * d_7 + d_2019 * d_8 + d_2019 * d_9 + d_2019 * d_10 + d_2019 * d_11 + ##d_2019 * d_12 +
  #d_2020 * d_1 + d_2020 * d_2 + d_2020 * d_3 + d_2020 * d_4 + d_2020 * d_5 + d_2020 * d_6 + d_2020 * d_7 + d_2020 * d_8 + d_2020 * d_9 + d_2020 * d_10 + d_2020 * d_11 + ##+ d_2020 * d_12
  dt1_n_consolidated_subsidiaries + dt1_n_consolidated_subsidiaries_ipo +
  dt1_current_assets + dt1_cash_and_cash_equivalents + #dt1_non_current_assets + 
  dt1_tangible_fixed_assets + dt1_intangible_fixed_assets + dt1_patent_right + dt1_software + dt1_goodwill + dt1_leased_asset +
  dt1_trademark + dt1_other_intangible_fixed_assets + dt1_total_investment_and_other_assets + dt1_total_assets + dt1_current_liabilities + dt1_non_current_liabilities + dt1_total_debt + dt1_non_controlling_interests +
  dt1_total_liabilities_and_net_assets + dt1_capital_attributable_to_owners_of_the_parent_company + dt1_sales_operating_revenue + dt1_financial_income + #dt1_sales_cost + 
  dt1_sales_operating_cost + dt1_unrealized_profit_on_installment_sales + #dt1_sga + 
  dt1_financial_costs + dt1_employees_end_term + dt1_avg_temp_employees + dt1_r_d_expenses + dt1_capital_investment + dt1_goodwill_amortization + dt1_sga_sales_commission + dt1_sga_storage_costs + dt1_sga_advertising_expenses +
  dt1_sga_sales_expansion_costs + dt1_sga_allowance_for_doubtful_accounts + dt1_sga_officer_compensation + dt1_sga_provision_for_retirement_benefits_for_officers + dt1_sga_provision_for_bonuses_for_directors + dt1_sga_personnel_welfare_expenses +
  dt1_sga_provision_for_retirement_benefits + dt1_sga_depreciation + dt1_sga_goodwill_amortization + dt1_sga_rent + dt1_sga_taxes_and_public_dues + dt1_sga_patent_fee_paid + dt1_sga_rd_cost + dt1_sga_warranty_repair_costs + dt1_sga_other +
  dt1_gross_profit + dt1_rd_expenses + dt1_operating_income + dt1_non_operating_income + dt1_non_operating_cost + dt1_ordinary_profit + dt1_income_before_income_taxes + dt1_net_income_from_continuing_operations +
  dt1_net_income_from_non_continuing_operations + dt1_net_income_consolidated + dt1_dividend +
  dt2_n_consolidated_subsidiaries + dt2_n_consolidated_subsidiaries_ipo +
  dt2_current_assets + dt2_cash_and_cash_equivalents + #dt2_non_current_assets + 
  dt2_tangible_fixed_assets + dt2_intangible_fixed_assets + dt2_patent_right + dt2_software + dt2_goodwill + dt2_leased_asset +
  dt2_trademark + dt2_other_intangible_fixed_assets + dt2_total_investment_and_other_assets + dt2_total_assets + dt2_current_liabilities + dt2_non_current_liabilities + dt2_total_debt + dt2_non_controlling_interests +
  dt2_total_liabilities_and_net_assets + dt2_capital_attributable_to_owners_of_the_parent_company + dt2_sales_operating_revenue + dt2_financial_income + #dt2_sales_cost + 
  dt2_sales_operating_cost + dt2_unrealized_profit_on_installment_sales + #dt2_sga + 
  dt2_financial_costs + dt2_employees_end_term + dt2_avg_temp_employees + dt2_r_d_expenses + dt2_capital_investment + dt2_goodwill_amortization + dt2_sga_sales_commission + dt2_sga_storage_costs + dt2_sga_advertising_expenses +
  dt2_sga_sales_expansion_costs + dt2_sga_allowance_for_doubtful_accounts + dt2_sga_officer_compensation + dt2_sga_provision_for_retirement_benefits_for_officers + dt2_sga_provision_for_bonuses_for_directors + dt2_sga_personnel_welfare_expenses +
  dt2_sga_provision_for_retirement_benefits + dt2_sga_depreciation + dt2_sga_goodwill_amortization + dt2_sga_rent + dt2_sga_taxes_and_public_dues + dt2_sga_patent_fee_paid + dt2_sga_rd_cost + dt2_sga_warranty_repair_costs + dt2_sga_other +
  dt2_gross_profit + dt2_rd_expenses + dt2_operating_income + dt2_non_operating_income + dt2_non_operating_cost + dt2_ordinary_profit + dt2_income_before_income_taxes + dt2_net_income_from_continuing_operations +
  dt2_net_income_from_non_continuing_operations + dt2_net_income_consolidated + dt2_dividend

model_dt3_2_new_ <- market_cap_new ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + cash_and_cash_equivalents + #non_current_assets + 
  tangible_fixed_assets + intangible_fixed_assets + patent_right + software + goodwill + leased_asset + trademark + ##sales_goodwill + design_right +
  other_intangible_fixed_assets + total_investment_and_other_assets + ##development_cost +
  current_liabilities + non_current_liabilities +
  sales_operating_revenue + financial_income + #sales_cost + 
  sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs +
  sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation +
  sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits +
  sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other +
  employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + goodwill_amortization + ##officer_bonus_provision +
  d_manufacture + #d_saas + 
  #d_manufacture * d_saas +
  #d_2018 + d_2019 + ##d_2020 +
  d_1 + d_2 + d_3 + d_4 + d_5 + d_6 + d_7 + d_8 + d_9 +d_10 + d_11 + ##d_12 +
  #d_2018 * d_1 + d_2018 * d_2 + d_2018 * d_3 + d_2018 * d_4 + d_2018 * d_5 + d_2018 * d_6 + d_2018 * d_7 + d_2018 * d_8 + d_2018 * d_9 + d_2018 * d_10 + d_2018 * d_11 + ##d_2018 * d_12 +
  #d_2019 * d_1 + d_2019 * d_2 + d_2019 * d_3 + d_2019 * d_4 + d_2019 * d_5 + d_2019 * d_6 + d_2019 * d_7 + d_2019 * d_8 + d_2019 * d_9 + d_2019 * d_10 + d_2019 * d_11 + ##d_2019 * d_12 +
  #d_2020 * d_1 + d_2020 * d_2 + d_2020 * d_3 + d_2020 * d_4 + d_2020 * d_5 + d_2020 * d_6 + d_2020 * d_7 + d_2020 * d_8 + d_2020 * d_9 + d_2020 * d_10 + d_2020 * d_11 + ##+ d_2020 * d_12
  dt1_n_consolidated_subsidiaries + dt1_n_consolidated_subsidiaries_ipo +
  dt1_current_assets + dt1_cash_and_cash_equivalents + #dt1_non_current_assets + 
  dt1_tangible_fixed_assets + dt1_intangible_fixed_assets + dt1_patent_right + dt1_software + dt1_goodwill + dt1_leased_asset +
  dt1_trademark + dt1_other_intangible_fixed_assets + dt1_total_investment_and_other_assets + dt1_total_assets + dt1_current_liabilities + dt1_non_current_liabilities + dt1_total_debt + dt1_non_controlling_interests +
  dt1_total_liabilities_and_net_assets + dt1_capital_attributable_to_owners_of_the_parent_company + dt1_sales_operating_revenue + dt1_financial_income + #dt1_sales_cost + 
  dt1_sales_operating_cost + dt1_unrealized_profit_on_installment_sales + #dt1_sga + 
  dt1_financial_costs + dt1_employees_end_term + dt1_avg_temp_employees + dt1_r_d_expenses + dt1_capital_investment + dt1_goodwill_amortization + dt1_sga_sales_commission + dt1_sga_storage_costs + dt1_sga_advertising_expenses +
  dt1_sga_sales_expansion_costs + dt1_sga_allowance_for_doubtful_accounts + dt1_sga_officer_compensation + dt1_sga_provision_for_retirement_benefits_for_officers + dt1_sga_provision_for_bonuses_for_directors + dt1_sga_personnel_welfare_expenses +
  dt1_sga_provision_for_retirement_benefits + dt1_sga_depreciation + dt1_sga_goodwill_amortization + dt1_sga_rent + dt1_sga_taxes_and_public_dues + dt1_sga_patent_fee_paid + dt1_sga_rd_cost + dt1_sga_warranty_repair_costs + dt1_sga_other +
  dt1_gross_profit + dt1_rd_expenses + dt1_operating_income + dt1_non_operating_income + dt1_non_operating_cost + dt1_ordinary_profit + dt1_income_before_income_taxes + dt1_net_income_from_continuing_operations +
  dt1_net_income_from_non_continuing_operations + dt1_net_income_consolidated + dt1_dividend +
  dt2_n_consolidated_subsidiaries + dt2_n_consolidated_subsidiaries_ipo +
  dt2_current_assets + dt2_cash_and_cash_equivalents + #dt2_non_current_assets + 
  dt2_tangible_fixed_assets + dt2_intangible_fixed_assets + dt2_patent_right + dt2_software + dt2_goodwill + dt2_leased_asset +
  dt2_trademark + dt2_other_intangible_fixed_assets + dt2_total_investment_and_other_assets + dt2_total_assets + dt2_current_liabilities + dt2_non_current_liabilities + dt2_total_debt + dt2_non_controlling_interests +
  dt2_total_liabilities_and_net_assets + dt2_capital_attributable_to_owners_of_the_parent_company + dt2_sales_operating_revenue + dt2_financial_income + #dt2_sales_cost + 
  dt2_sales_operating_cost + dt2_unrealized_profit_on_installment_sales +#dt2_sga + 
  dt2_financial_costs + dt2_employees_end_term + dt2_avg_temp_employees + dt2_r_d_expenses + dt2_capital_investment + dt2_goodwill_amortization + dt2_sga_sales_commission + dt2_sga_storage_costs + dt2_sga_advertising_expenses +
  dt2_sga_sales_expansion_costs + dt2_sga_allowance_for_doubtful_accounts + dt2_sga_officer_compensation + dt2_sga_provision_for_retirement_benefits_for_officers + dt2_sga_provision_for_bonuses_for_directors + dt2_sga_personnel_welfare_expenses +
  dt2_sga_provision_for_retirement_benefits + dt2_sga_depreciation + dt2_sga_goodwill_amortization + dt2_sga_rent + dt2_sga_taxes_and_public_dues + dt2_sga_patent_fee_paid + dt2_sga_rd_cost + dt2_sga_warranty_repair_costs + dt2_sga_other +
  dt2_gross_profit + dt2_rd_expenses + dt2_operating_income + dt2_non_operating_income + dt2_non_operating_cost + dt2_ordinary_profit + dt2_income_before_income_taxes + dt2_net_income_from_continuing_operations +
  dt2_net_income_from_non_continuing_operations + dt2_net_income_consolidated + dt2_dividend +
  dt3_n_consolidated_subsidiaries + dt3_n_consolidated_subsidiaries_ipo +
  dt3_current_assets + dt3_cash_and_cash_equivalents + #dt3_non_current_assets + 
  dt3_tangible_fixed_assets + dt3_intangible_fixed_assets + dt3_patent_right + dt3_software + dt3_goodwill + dt3_leased_asset +
  dt3_trademark + dt3_other_intangible_fixed_assets + dt3_total_investment_and_other_assets + dt3_total_assets + dt3_current_liabilities + dt3_non_current_liabilities + dt3_total_debt + dt3_non_controlling_interests +
  dt3_total_liabilities_and_net_assets + dt3_capital_attributable_to_owners_of_the_parent_company + dt3_sales_operating_revenue + dt3_financial_income + #dt3_sales_cost + 
  dt3_sales_operating_cost + dt3_unrealized_profit_on_installment_sales + #dt3_sga + 
  dt3_financial_costs + dt3_employees_end_term + dt3_avg_temp_employees + dt3_r_d_expenses + dt3_capital_investment + dt3_goodwill_amortization + dt3_sga_sales_commission + dt3_sga_storage_costs + dt3_sga_advertising_expenses +
  dt3_sga_sales_expansion_costs + dt3_sga_allowance_for_doubtful_accounts + dt3_sga_officer_compensation + dt3_sga_provision_for_retirement_benefits_for_officers + dt3_sga_provision_for_bonuses_for_directors + dt3_sga_personnel_welfare_expenses +
  dt3_sga_provision_for_retirement_benefits + dt3_sga_depreciation + dt3_sga_goodwill_amortization + dt3_sga_rent + dt3_sga_taxes_and_public_dues + dt3_sga_patent_fee_paid + dt3_sga_rd_cost + dt3_sga_warranty_repair_costs + dt3_sga_other +
  dt3_gross_profit + dt3_rd_expenses + dt3_operating_income + dt3_non_operating_income + dt3_non_operating_cost + dt3_ordinary_profit + dt3_income_before_income_taxes + dt3_net_income_from_continuing_operations +
  dt3_net_income_from_non_continuing_operations + dt3_net_income_consolidated + dt3_dividend +
  d_saas *(
    dt1_n_consolidated_subsidiaries + dt1_n_consolidated_subsidiaries_ipo +
      dt1_current_assets + dt1_cash_and_cash_equivalents + #dt1_non_current_assets + 
      dt1_tangible_fixed_assets + dt1_intangible_fixed_assets + dt1_patent_right + dt1_software + dt1_goodwill + dt1_leased_asset +
      dt1_trademark + dt1_other_intangible_fixed_assets + dt1_total_investment_and_other_assets + dt1_total_assets + dt1_current_liabilities + dt1_non_current_liabilities + dt1_total_debt + dt1_non_controlling_interests +
      dt1_total_liabilities_and_net_assets + dt1_capital_attributable_to_owners_of_the_parent_company + dt1_sales_operating_revenue + dt1_financial_income + #dt1_sales_cost + 
      dt1_sales_operating_cost + dt1_unrealized_profit_on_installment_sales + #dt1_sga + 
      dt1_financial_costs + dt1_employees_end_term + dt1_avg_temp_employees + dt1_r_d_expenses + dt1_capital_investment + dt1_goodwill_amortization + dt1_sga_sales_commission + dt1_sga_storage_costs + dt1_sga_advertising_expenses +
      dt1_sga_sales_expansion_costs + dt1_sga_allowance_for_doubtful_accounts + dt1_sga_officer_compensation + dt1_sga_provision_for_retirement_benefits_for_officers + dt1_sga_provision_for_bonuses_for_directors + dt1_sga_personnel_welfare_expenses +
      dt1_sga_provision_for_retirement_benefits + dt1_sga_depreciation + dt1_sga_goodwill_amortization + dt1_sga_rent + dt1_sga_taxes_and_public_dues + dt1_sga_patent_fee_paid + dt1_sga_rd_cost + dt1_sga_warranty_repair_costs + dt1_sga_other +
      dt1_gross_profit + dt1_rd_expenses + dt1_operating_income + dt1_non_operating_income + dt1_non_operating_cost + dt1_ordinary_profit + dt1_income_before_income_taxes + dt1_net_income_from_continuing_operations +
      dt1_net_income_from_non_continuing_operations + dt1_net_income_consolidated + dt1_dividend +
      dt2_n_consolidated_subsidiaries + dt2_n_consolidated_subsidiaries_ipo +
      dt2_current_assets + dt2_cash_and_cash_equivalents + #dt2_non_current_assets + 
      dt2_tangible_fixed_assets + dt2_intangible_fixed_assets + dt2_patent_right + dt2_software + dt2_goodwill + dt2_leased_asset +
      dt2_trademark + dt2_other_intangible_fixed_assets + dt2_total_investment_and_other_assets + dt2_total_assets + dt2_current_liabilities + dt2_non_current_liabilities + dt2_total_debt + dt2_non_controlling_interests +
      dt2_total_liabilities_and_net_assets + dt2_capital_attributable_to_owners_of_the_parent_company + dt2_sales_operating_revenue + dt2_financial_income + #dt2_sales_cost + 
      dt2_sales_operating_cost + dt2_unrealized_profit_on_installment_sales +#dt2_sga + 
      dt2_financial_costs + dt2_employees_end_term + dt2_avg_temp_employees + dt2_r_d_expenses + dt2_capital_investment + dt2_goodwill_amortization + dt2_sga_sales_commission + dt2_sga_storage_costs + dt2_sga_advertising_expenses +
      dt2_sga_sales_expansion_costs + dt2_sga_allowance_for_doubtful_accounts + dt2_sga_officer_compensation + dt2_sga_provision_for_retirement_benefits_for_officers + dt2_sga_provision_for_bonuses_for_directors + dt2_sga_personnel_welfare_expenses +
      dt2_sga_provision_for_retirement_benefits + dt2_sga_depreciation + dt2_sga_goodwill_amortization + dt2_sga_rent + dt2_sga_taxes_and_public_dues + dt2_sga_patent_fee_paid + dt2_sga_rd_cost + dt2_sga_warranty_repair_costs + dt2_sga_other +
      dt2_gross_profit + dt2_rd_expenses + dt2_operating_income + dt2_non_operating_income + dt2_non_operating_cost + dt2_ordinary_profit + dt2_income_before_income_taxes + dt2_net_income_from_continuing_operations +
      dt2_net_income_from_non_continuing_operations + dt2_net_income_consolidated + dt2_dividend +
      dt3_n_consolidated_subsidiaries + dt3_n_consolidated_subsidiaries_ipo +
      dt3_current_assets + dt3_cash_and_cash_equivalents + #dt3_non_current_assets + 
      dt3_tangible_fixed_assets + dt3_intangible_fixed_assets + dt3_patent_right + dt3_software + dt3_goodwill + dt3_leased_asset +
      dt3_trademark + dt3_other_intangible_fixed_assets + dt3_total_investment_and_other_assets + dt3_total_assets + dt3_current_liabilities + dt3_non_current_liabilities + dt3_total_debt + dt3_non_controlling_interests +
      dt3_total_liabilities_and_net_assets + dt3_capital_attributable_to_owners_of_the_parent_company + dt3_sales_operating_revenue + dt3_financial_income + #dt3_sales_cost + 
      dt3_sales_operating_cost + dt3_unrealized_profit_on_installment_sales + #dt3_sga + 
      dt3_financial_costs + dt3_employees_end_term + dt3_avg_temp_employees + dt3_r_d_expenses + dt3_capital_investment + dt3_goodwill_amortization + dt3_sga_sales_commission + dt3_sga_storage_costs + dt3_sga_advertising_expenses +
      dt3_sga_sales_expansion_costs + dt3_sga_allowance_for_doubtful_accounts + dt3_sga_officer_compensation + dt3_sga_provision_for_retirement_benefits_for_officers + dt3_sga_provision_for_bonuses_for_directors + dt3_sga_personnel_welfare_expenses +
      dt3_sga_provision_for_retirement_benefits + dt3_sga_depreciation + dt3_sga_goodwill_amortization + dt3_sga_rent + dt3_sga_taxes_and_public_dues + dt3_sga_patent_fee_paid + dt3_sga_rd_cost + dt3_sga_warranty_repair_costs + dt3_sga_other +
      dt3_gross_profit + dt3_rd_expenses + dt3_operating_income + dt3_non_operating_income + dt3_non_operating_cost + dt3_ordinary_profit + dt3_income_before_income_taxes + dt3_net_income_from_continuing_operations +
      dt3_net_income_from_non_continuing_operations + dt3_net_income_consolidated + dt3_dividend
  )
  

# データの準備
all <- pdata.frame(df_tmp, index = c("corp_name", "fiscal_year"))

## 個別効果を無視して重回帰分析する
all_ols <- lm(single_model, data=all)
all_ols2 <- lm(single_model2, data=all)
alias(all_ols2) #lm(single_model2, data=all))

vif_res2 <- car::vif(all_ols2)
# write.csv(x = vif_res2, file = "~/Documents/GitHub/final_report/data/vif_cor_all2.csv")

# all_ols <- plm(single_model, data=all, model="pooling") #多重共線性のため結果が得られない
all_ols_dt1 <- lm(fe_model_dt1, data=df_short)
all_ols_dt1_2 <- lm(fe_model_dt1_2, data=df_short)
all_ols_dt2_2 <- lm(fe_model_dt2_2, data=df_middle)
all_ols_dt3_2 <- lm(model_dt3_2, data=df_long)

all_ols_dt1_ <- lm(fe_model_dt1_, data=df_short)
all_ols_dt1_2_ <- lm(fe_model_dt1_2_, data=df_short)
all_ols_dt2_2_ <- lm(fe_model_dt2_2_, data=df_middle)
all_ols_dt3_2_ <- lm(model_dt3_2_, data=df_long)
all_ols_dt3_2_new <- lm(model_dt3_2_new_, data=df_long)

library(MASS)
all_ols_dt3_2_new_ <- lm(model_dt3_2_new_, data=df_long)
# step.all_ols_dt3_2_new_ <- step(all_ols_dt3_2_new_) #かなり時間かかるので誤って実行しないようにコメントアウト
summaryStep.all_ols_dt3_2_new_ <- summary(step.all_ols_dt3_2_new_)

coef <- summaryStep.all_ols_dt3_2_new_$coefficients           #回帰係数
r.squared <- summaryStep.all_ols_dt3_2_new_$r.squared         #決定係数
adj.r.squared <- summaryStep.all_ols_dt3_2_new_$adj.r.squared #修正決定係数
resultTable <- cbind(coef, r.squared = r.squared, adj.r.squared = adj.r.squared)
# write.csv(resultTable, "~/Documents/GitHub/final_report/data/stepwise_olm.csv")
# stargazer(resultTable, type = "html", out = "Documents/GitHub/final_report/data/stepwise_olm.doc")
# alias(all_ols_dt1_2)
# vif(all_ols_dt1_2)
summary(all_ols)
summary(fixef(all_ols))
summary(all_ols_dt1)
summary(all_ols_dt3_2)
all_ols_dt3_2_new


# 固定効果モデル(LSDV(within)推定)
all_fe <- plm(single_model, data=all, model="within")
all_fe2 <- plm(single_model2, data=all, model="within")
# all_fe_log <- plm(single_model_log, data=all, model="within")
all_fe_dt1 <- plm(fe_model_dt1, data=df_short, model="within")
all_fe_dt1_2 <- plm(fe_model_dt1_2, data=df_short, model="within")
all_fe_dt2_2 <- plm(fe_model_dt2_2, data=df_middle, model="within")

all_fe_dt1_ <- plm(fe_model_dt1_, data=df_short, model="within")
all_fe_dt1_2_ <- plm(fe_model_dt1_2_, data=df_short, model="within")
all_fe_dt2_2_ <- plm(fe_model_dt2_2_, data=df_middle, model="within")
summary(all_fe)
summary(fixef(all_fe))
summary(all_fe_dt1)
# fixef_all_fe_dt1 <- summary(fixef(all_fe_dt1))
# write.csv(x = fixef_all_fe_dt1, file = "~/Documents/GitHub/final_report/data/fixef_all_fe_dt1.csv")

#F検定を行う(p値が小さければ、帰無仮説が棄却され「個別効果はある」ということになる)
pFtest(all_fe, all_ols)
pFtest(all_fe_dt1, all_ols_dt1)
pFtest(all_fe_dt1_2, all_ols_dt1_2)

# GLS推定
# all_gls <- plm(formula = single_model, data=all,model="random")#多重共線性のため結果が得られない
all_gls2 <- plm(formula = single_model2, data=all,model="random")
summary(all_gls)

## ハウスマン検定を行う
phtest(all_fe2, all_gls2)

## saas企業 ##
saas <- pdata.frame(df_saas, index = c("corp_name", "fiscal_year"))
saas_fe <- plm(single_model, data=df_saas, model="within")
saas_fe2 <- plm(single_model2, data=df_saas, model="within")
# saas_fe_log <- plm(single_model_log, data=df_saas, model="within")
saas_fe_dt1 <- plm(fe_model_dt1, data=df_saas_short, model="within")
saas_fe_dt1_2 <- plm(fe_model_dt1_2, data=df_saas_short, model="within")
# saas_fe_dt2_2 <- plm(fe_model_dt2_2, data=df_saas_middle, model="within") #サンプル数が少ないため推定できず
# saas_ols_dt2_2 <- lm(fe_model_dt2_2, data=df_saas_middle) #サンプル数が少ないため推定できず

saas_fe_dt1_ <- plm(fe_model_dt1_, data=df_saas_short, model="within")
saas_fe_dt1_2_ <- plm(fe_model_dt1_2_, data=df_saas_short, model="within")
saas_ols_dt3_2_ <- lm(model_dt3_2_, data=df_saas_long)
saas_ols_dt3_2_new <- lm(model_dt3_2_new, data=df_saas_long)
summary(saas_ols_dt2_2)
# summary(saas_fe)
summary(saas_fe_dt1)

## 非製造業 ##
non_manu <- pdata.frame(df_non_manu, index = c("corp_name", "fiscal_year"))
non_manu_fe <- plm(single_model, data=non_manu, model="within")
non_manu_fe2 <- plm(single_model2, data=non_manu, model="within")
# non_manu_fe_log <- plm(single_model_log, data=non_manu, model="within")
non_manu_fe_dt1 <- plm(fe_model_dt1, data=df_non_manu_short, model="within")
non_manu_fe_dt1_2 <- plm(fe_model_dt1_2, data=df_non_manu_short, model="within")
non_manu_fe_dt2_2 <- plm(fe_model_dt2_2, data=df_non_manu_middle, model="within")

non_manu_fe_dt1_ <- plm(fe_model_dt1_, data=df_non_manu_short, model="within")
non_manu_fe_dt1_2_ <- plm(fe_model_dt1_2_, data=df_non_manu_short, model="within")
non_manu_fe_dt2_2_ <- plm(fe_model_dt2_2_, data=df_non_manu_middle, model="within")
non_manu_ols_dt3_2_ <- lm(model_dt3_2_, data=df_non_manu_long)
non_manu_ols_dt3_2_new <- lm(model_dt3_2_new, data=df_non_manu_long)
non_manu_ols_dt3_2_new_ <- lm(model_dt3_2_new_, data=df_non_manu_long)
# summary(non_manu_fe)
summary(non_manu_fe_dt1)

## 製造業 ##
manu <- pdata.frame(df_manu, index = c("corp_name", "fiscal_year"))
manu_fe <- plm(single_model , data=manu, model="within")
manu_fe2 <- plm(single_model2 , data=manu, model="within")
# manu_fe_log <- plm(single_model_log , data=manu, model="within")
manu_fe_dt1 <- plm(fe_model_dt1, data=df_manu_short, model="within")
manu_fe_dt1_2 <- plm(fe_model_dt1_2, data=df_manu_short, model="within")
manu_fe_dt2_2 <- plm(fe_model_dt2_2, data=df_manu_middle, model="within")

manu_fe_dt1_ <- plm(fe_model_dt1_, data=df_manu_short, model="within")
manu_fe_dt1_2_ <- plm(fe_model_dt1_2_, data=df_manu_short, model="within")
manu_fe_dt2_2_ <- plm(fe_model_dt2_2_, data=df_manu_middle, model="within")
manu_ols_dt3_2_ <- lm(model_dt3_2_, data=df_manu_long)
manu_ols_dt3_2_new <- lm(model_dt3_2_new, data=df_manu_long)
manu_ols_dt3_2_new_ <- lm(model_dt3_2_new_, data=df_manu_long)
# summary(manu_fe)
summary(manu_fe_dt1)

library(stargazer)
stargazer(all_fe, saas_fe, manu_fe, non_manu_fe, type = "html", out = "Documents/GitHub/final_report/data/fe_ouput_4years_v2.doc")
stargazer(all_fe2, saas_fe2, manu_fe2, non_manu_fe2, type = "html", out = "Documents/GitHub/final_report/data/fe2_ouput_4years_v2.doc")
# stargazer(all_fe_log, saas_fe_log, manu_fe_log, non_manu_fe_log, type = "html", out = "Documents/GitHub/final_report/data/fe_log_ouput_4years.doc")
stargazer(all_fe_dt1, saas_fe_dt1, manu_fe_dt1, non_manu_fe_dt1, type = "html", out = "Documents/GitHub/final_report/data/fe_short_ouput_3years_v2.doc") #saas_fe_dt1で多重共線性の影響あるため出力できず
stargazer(all_ols_dt1, all_fe_dt1, saas_fe_dt1, manu_fe_dt1, non_manu_fe_dt1, type = "html", out = "Documents/GitHub/final_report/data/ols_fe_short_ouput_3years_v2.doc") #saas_fe_dt1で多重共線性の影響あるため出力できず
stargazer(all_fe_dt1_2, saas_fe_dt1_2, manu_fe_dt1_2, non_manu_fe_dt1_2, type = "html", out = "Documents/GitHub/final_report/data/fe_short_ouput_3years_v2.doc")
stargazer(all_ols_dt1_2, all_fe_dt1_2, saas_fe_dt1_2, manu_fe_dt1_2, non_manu_fe_dt1_2, type = "html", out = "Documents/GitHub/final_report/data/ols_fe_short_ouput_3years_v2.doc")
# stargazer(all_ols_dt2_2, all_fe_dt2_2, saas_fe_dt2_2, manu_fe_dt2_2, non_manu_fe_dt2_2, type = "html", out = "Documents/GitHub/final_report/data/ols_fe_middle_ouput_2years_v2.doc") #saasの部分、サンプル数が少ないため推定できず
stargazer(all_ols_dt2_2, all_fe_dt2_2, manu_fe_dt2_2, non_manu_fe_dt2_2, type = "html", out = "Documents/GitHub/final_report/data/ols_fe_middle_ouput_2years_v2_without_saas.doc")

# stargazer(all_fe_dt1_, saas_fe_dt1_, manu_fe_dt1_, non_manu_fe_dt1_, type = "html", out = "Documents/GitHub/final_report/data/fe_short_ouput_3years_v3.doc") #saas_fe_dt1で多重共線性の影響あるため出力できず
# stargazer(all_ols_dt1_, all_fe_dt1_, saas_fe_dt1_, manu_fe_dt1_, non_manu_fe_dt1_, type = "html", out = "Documents/GitHub/final_report/data/ols_fe_short_ouput_3years_v3.doc") #saas_fe_dt1で多重共線性の影響あるため出力できず
stargazer(all_fe_dt1_2_, saas_fe_dt1_2_, manu_fe_dt1_2_, non_manu_fe_dt1_2_, type = "html", out = "Documents/GitHub/final_report/data/fe_short_ouput_3years_v3.doc")
stargazer(all_ols_dt1_2_, all_fe_dt1_2_, saas_fe_dt1_2_, manu_fe_dt1_2_, non_manu_fe_dt1_2_, type = "html", out = "Documents/GitHub/final_report/data/ols_fe_short_ouput_3years_v3.doc")
# stargazer(all_ols_dt2_2_, all_fe_dt2_2_, saas_fe_dt2_2_, manu_fe_dt2_2_, non_manu_fe_dt2_2_, type = "html", out = "Documents/GitHub/final_report/data/ols_fe_middle_ouput_2years_v3.doc") #saasの部分、サンプル数が少ないため推定できず
stargazer(all_ols_dt2_2_, all_fe_dt2_2_, manu_fe_dt2_2_, non_manu_fe_dt2_2_, type = "html", out = "Documents/GitHub/final_report/data/ols_fe_middle_ouput_2years_v3_without_saas.doc")

stargazer(all_ols_dt3_2_, saas_ols_dt3_2_, manu_ols_dt3_2_, non_manu_ols_dt3_2_, type = "html", out = "Documents/GitHub/final_report/data/ols_long_year_v3.doc")
stargazer(all_ols_dt3_2_new, saas_ols_dt3_2_new, manu_ols_dt3_2_new, non_manu_ols_dt3_2_new, type = "html", out = "Documents/GitHub/final_report/data/ols_long_year_v4.doc")
stargazer(all_ols_dt3_2_new_, manu_ols_dt3_2_new_, non_manu_ols_dt3_2_new_, type = "html", out = "Documents/GitHub/final_report/data/ols_long_year_v4.1.doc")
