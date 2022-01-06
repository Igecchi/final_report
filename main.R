# install.packages('dplyr') # 初回のみ
library(readr)
library(dplyr)

#ダウンロードしたデータを取り込む
finance_info_2016_2021 <- read_csv("Documents/GitHub/final_report/data/financial_data/finance_info_2016_2021.csv",
                                   locale = locale(encoding = "SHIFT-JIS"))
# View(finance_info_2016_2021)
# stock_info_2016_2021 <- read_csv("Documents/GitHub/final_report/data/financial_data/stock_info_2016_2021_monthly.csv", 
#                                  locale = locale(encoding = "SHIFT-JIS"))
# View(stock_info_2016_2021)


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

###  2016-2021のデータ ###
library(readr)
library(dplyr)
# finance <- read_csv("Documents/GitHub/final_report/data/trash/finance_2016_2021.csv")
# stock <- read_csv("Documents/GitHub/final_report/data/trash/stock_2016_2021_monthly.csv")
# View(stock)

###  2017-2021のデータ ###
# finance_info_2017_2021_old <- read_csv("Documents/GitHub/final_report/data/finance_2017_2021_all.csv")
finance_info_2017_2021 <- read_csv("Documents/GitHub/final_report/data/finance_2017_2021_final.csv")
finance_info_2017_2021<-finance_info_2017_2021[!(finance_info_2017_2021$consolidation_criteria==3),]

## できなかったのでスプレッドシートで作業　＃＃＃＃
# finance_info_2017_2021 %>% 
#   mutate(lead_corp_name_01 = lead(corp_name, n = 1)) %>% 
#   mutate(lead_corp_name_03 = lead(corp_name, n = 3)) %>% 
#   mutate(lead_fiscal_year_01 = lead(fiscal_year, n = 1)) %>% 
#   mutate(lead_fiscal_year_03 = lead(fiscal_year, n = 3))
# colnames(finance_info_2017_2021)

stock_info_2017_2021 <- read_csv("Documents/GitHub/final_report/data/stock_2017_2021_monthly_all.csv")
# View(stock_info_2017_2021)
# basic_info_2017_2021 <- read_csv("Documents/GitHub/final_report/data/basic_info_2017_2021.csv", locale = locale(encoding = "SHIFT-JIS"))
basic_info_2017_2021 <- read_csv("Documents/GitHub/final_report/data/basic_info_2017_2021_all.csv")
transfer_2017_2021 <- read_csv("Documents/GitHub/final_report/data/transfer_2017_2021.csv", locale = locale(encoding = "SHIFT-JIS"))


#2021年11月・12月のデータを削除
stock_info_2017_2021<-stock_info_2017_2021[!(stock_info_2017_2021$fiscal_year==2021 & stock_info_2017_2021$fiscal_month==11),]
stock_info_2017_2021<-stock_info_2017_2021[!(stock_info_2017_2021$fiscal_year==2021 & stock_info_2017_2021$fiscal_month==12),]
# summary(stock_info_2017_2021)

#データを結合する
# df <- dplyr::left_join(finance,stock, by = c("corp_name", "fiscal_date", "fiscal_year", "fiscal_month"))
df <- dplyr::left_join(finance_info_2017_2021, basic_info_2017_2021, by = c("corp_name"))
df <- dplyr::left_join(df, stock_info_2017_2021, by = c("corp_name", "fiscal_year", "fiscal_month")) #, "fiscal_date"

# df<-df[!(df$consolidation_criteria==3),]
# write.csv(x = df, file = "Documents/GitHub/final_report/data/new/df_joined.csv")

# df <- read_csv("Documents/GitHub/final_report/data/new/df_joined.csv")
# df<-df[!(df$consolidation_criteria==3),]


#4年分データを取れない企業は取り除く
corp <- c( 'ＡＣＳＬ','ＡＤワークスグループ','ＢｕｙＳｅｌｌ　Ｔｅｃｈｎｏｌ','ＥＮＥＯＳホールディングス','ＧａｍｅＷｉｔｈ','ＧＭＯ　ＴＥＣＨ','ＧＭＯメディア','Ｍａｃｂｅｅ　Ｐｌａｎｅｔ','ＭＲＴ',
           'ＭＳ−Ｊａｐａｎ','ＮＥＣ','ＲＥＶＯＬＵＴＩＯＮ','ＳＩＧ','アイシン','アイティメディア','アサヒホールディングス','アサンテ','アズーム','アドソル日進','アルテリア・ネットワークス','インティメート・マージャー','インフォネット','ウィルズ',
           'エコモット','エス・サイエンス','カナミックネットワーク','カンロ','キリンホールディングス','クボタ','グレイステクノロジー','クレハ','コロワイド','コンヴァノ','サイジニア','サイバーセキュリティクラウド','サントリー食品インターナショナ',
           'シェアリングテクノロジー','じげん','シスメックス','ジャステック','シャルレ','スプリックス','スミダコーポレーション','チームスピリット','ディジタルメディアプロフェッシ','テモナ','ナブテスコ','ニコン','バンク・オブ・イノベーション','ピアズ','ビーグリー','ビザスク','ビジョナル','フィードフォース','フィット','ブラザー工業',
           'ブリッジインターナショナル','フレアス','プロパティエージェント','プロレド・パートナーズ','ポート','ホクシン','メタップス','ユニ・チャーム','ヨシックスホールディングス','リンクアンドモチベーション','ロコガイド','ワンダープラネット','横浜ゴム','協和キリン','光通信','三晃金属工業','三菱ケミカルホールディングス','山洋電気',
           '中広','日機装','日本酸素ホールディングス','日本食品化工','豊田自動織機','豊田通商','北日本紡績','味の素','木曽路','和心','Ａｉｍｉｎｇ','ＡＩメカテック','ＡＳＪ','Ｃａｓａ','ＣＹＢＥＲＤＹＮＥ','ＧＡ　ｔｅｃｈｎｏｌｏｇｉｅｓ','ＧＣＡ',
           'ＧＭＯペイメントゲートウェイ','ＧＭＯペパボ','Ｊ．フロント　リテイリング','ＪＳＲ','ＪＶＣケンウッド','Ｊトラスト','ＴＯＫＹＯ　ＢＡＳＥ','アーバンライク','アイドマ・ホールディングス','アイビーシー','アクシージア','アクセル','アジャイルメディア・ネットワー','イーブックイニシアティブジャパ','イヴレス',
           'イオンフィナンシャルサービス','ウェルビー','エクセディ','エニグモ','エヌリンクス','オージックグループ','オプティム','オリンパス','カカクコム','きずなホールディングス','きちりホールディングス','キッズウェル・バイオ','キャピタル・アセット・プランニ','キャリア','クシム',
           'くふうカンパニー','コカ・コーラ　ボトラーズジャパ','サイバーリンクス','サッポロホールディングス','ジャパンエンジンコーポレーショ','シルバーエッグ・テクノロジー','スター・マイカ・ホールディング','スペース','スマートバリュー','スマレジ','ソフトバンク','ダイサン','タナベ経営','チェンジ','チタン工業',
           'デザインワン・ジャパン','テスホールディングス','デリバリーコンサルティング','テルモ','トラスコ中山','ネオジャパン','ネットマーケティング','ハウスコム','はせがわ','ビーアンドピー','ビープラッツ','ヒト・コミュニケーションズ・ホ','ヒューマンクリエイションホール','ファイズホールディングス','ブックオフグループホールディン','ブライトパス・バイオ','フリー',
           'マキタ','ミネベアミツミ','メイホーホールディングス','メディネット','メドレー','メンバーズ','ユニカフェ','ライオン','ラキール','リクルートホールディングス','ルネサスエレクトロニクス','ログリー','霞ヶ関キャピタル','柿安本店','京セラ','共和コーポレーション','光陽社','三浦工業','三菱電機','識学','住友ベークライト','住友化学','松尾電機','全研本社','大日本住友製薬',
           '中村屋','東京個別指導学院','日医工','日本ハム','日本ペイントホールディングス','日本精機','日本製鉄','日本電信電話','夢展望','薬王堂ホールディングス','ＢＡＳＥ','ＢｌｕｅＭｅｍｅ','ＥＮＥＣＨＡＮＧＥ','Ｆａｓｔ　Ｆｉｔｎｅｓｓ　Ｊａ','ＦＦＲＩセキュリティ','ＦＩＧ','Ｇｅｎｋｙ　ＤｒｕｇＳｔｏｒｅ','ＧＭＯフィナンシャルゲート','Ｉ−ｎｅ','ｉ−ｐｌｕｇ','ＩＴｂｏｏｋホールディングス','ＪＦＥホールディングス',
           'Ｋａｉｚｅｎ　Ｐｌａｔｆｏｒｍ','Ｋｉｄｓ　Ｓｍｉｌｅ　Ｈｏｌｄ','Ｋｉｐｓ','ＱＬＳホールディングス','ｒａｋｕｍｏ','ＳＡＮＥＩ','Ｓｈａｒｉｎｇ　Ｉｎｎｏｖａｔ','ＳＲＥホールディングス','ＳＴＩフードホールディングス','Ｓｕｎ　Ａｓｔｅｒｉｓｋ','Ｔ．Ｓ．Ｉ','ＶＴホールディングス','ＷＡＳＨハウス','アートフォースジャパン','アールプランナー','アイ・パートナーズフィナンシャ','アイダ設計','アイドママーケティングコミュニ',
           'アイリッジ','アドベンチャー','アトラグループ','アマダ','イノベーション','インターネットイニシアティブ','ウィルグループ','エヌ・ティ・ティ・データ','オーケーエム','オリエンタル白石','カーブスホールディングス','カゴメ','カレント自動車','ギフティ','グッドパッチ','クリーマ','クリエイト・レストランツ・ホー','クレディセゾン','グローバルウェイ','コンフィデンス',
           'サイバートラスト','さくらさくプラス','ジーネクスト','ジーフット','シーボン','ジェイ・イー・ティ','システムソフト','ジョイフル本田','スペースバリューホールディング','セラク','セレス','セレンディップ・ホールディング','ダイヤモンドエレクトリックホー','ツガミ','ディップ','デジタルガレージ','トヨタ紡織','トリケミカル研究所','トレンダーズ','ナレッジスイート','バルコス','バルミューダ','バンドー化学','ビーイングホールディングス',
           'ヒロセ電機','ファースト住建','フィーチャ','フリュー','ブロードマインド','ベステラ','ペッパーフードサービス','ベルテクスコーポレーション','ポピンズホールディングス','ミンカブ・ジ・インフォノイド','モダリス','ヤマシタヘルスケアホールディン','ライトオン','リバーホールディングス','ローランド','ロコンド','一寸房','海帆','鎌倉新書','紀文食品','勤次郎',
           '串カツ田中ホールディングス','三菱重工業','住友金属鉱山','電算','土木管理総合試験所','東京産業','東京通信','日清食品ホールディングス','日本ＢＳ放送','日本触媒','日本電解','農業総合研究所','富士山マガジンサービス','豊田合成' )

for(i in corp){
  df<-df[!(df$corp_name == i),]
}

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

#PERの列を追加する
per <- data.frame(df$market_cap_new/df$net_income_consolidated)
names(per) <- c("per")
df <- cbind(df, per)

# write.csv(x = df, file = "Documents/GitHub/final_report/data/new/df_joined2_df.csv")
# 
# df <- read_csv("Documents/GitHub/final_report/data/new/df_joined2.csv")

library(data.table)

# fwrite(df,"Documents/GitHub/final_report/data/new/df_joined2.csv")
# dt <- data.table(df)

df <- read_csv("Documents/GitHub/final_report/data/new/target3.csv")

corp <- c('Ｃ　Ｃｈａｎｎｅｌ','Ｆｒｉｎｇｅ８１','ＩＮＣＬＵＳＩＶＥ','ＪＭＡＣＳ','ＪＴＯＷＥＲ','ＭｒＭａｘＨＤ','ＮｅｘＴｏｎｅ','アクアライン','イクヨ','ウイルテック','エディア','エルテス','カクヤスグループ','かどや製油','キャリアリンク','コーナン商事','コマースＯｎｅホールディングス','シーティーエス','ジェイック','ジャパンフーズ','シンクロ・フード','シンメンテホールディングス','スタジオアリス','ダブルエー','ナルミヤ・インターナショナル','パレモ・ホールディングス','ピースリー','メディアドゥ','ヤオコー','ランサーズ','ランディックス','リグア','リビングプラットフォーム','ワシントンホテル','駅探','王将フードサービス','関門海','技研ホールディングス','三陽商会','松屋アールアンドディ','清鋼材','川本産業','東亜石油','日本インシュレーション')
for(i in corp){
  df<-df[!(df$corp_name == i),]
}

df$dt_current_assets_03<- df$lead_current_assets_03 - df$current_assets
df$dt_cash_and_cash_equivalents_03<- df$lead_cash_and_cash_equivalents_03 - df$cash_and_cash_equivalents
df$dt_non_current_assets_03<- df$lead_non_current_assets_03 - df$non_current_assets
df$dt_tangible_fixed_assets_03<- df$lead_tangible_fixed_assets_03 - df$tangible_fixed_assets
df$dt_intangible_fixed_assets_03<- df$lead_intangible_fixed_assets_03 - df$intangible_fixed_assets
df$dt_sales_goodwill_03<- df$lead_sales_goodwill_03 - df$sales_goodwill
df$dt_patent_right_03<- df$lead_patent_right_03 - df$patent_right
df$dt_software_03<- df$lead_software_03 - df$software
df$dt_goodwill_03<- df$lead_goodwill_03 - df$goodwill
df$dt_leased_asset_03<- df$lead_leased_asset_03 - df$leased_asset
df$dt_trademark_03<- df$lead_trademark_03 - df$trademark
df$dt_design_right_03<- df$lead_design_right_03 - df$design_right
df$dt_other_intangible_fixed_assets_03<- df$lead_other_intangible_fixed_assets_03 - df$other_intangible_fixed_assets
df$dt_total_investment_and_other_assets_03<- df$lead_total_investment_and_other_assets_03 - df$total_investment_and_other_assets
df$dt_development_cost_03<- df$lead_development_cost_03 - df$development_cost
df$dt_total_assets_03<- df$lead_total_assets_03 - df$total_assets
df$dt_current_liabilities_03<- df$lead_current_liabilities_03 - df$current_liabilities
df$dt_non_current_liabilities_03<- df$lead_non_current_liabilities_03 - df$non_current_liabilities
df$dt_total_debt_03<- df$lead_total_debt_03 - df$total_debt
df$dt_non_controlling_interests_03<- df$lead_non_controlling_interests_03 - df$non_controlling_interests
df$dt_total_liabilities_and_net_assets_03<- df$lead_total_liabilities_and_net_assets_03 - df$total_liabilities_and_net_assets
df$dt_capital_attributable_to_owners_of_the_parent_company_03<- df$lead_capital_attributable_to_owners_of_the_parent_company_03 - df$capital_attributable_to_owners_of_the_parent_company
df$dt_sales_operating_revenue_03<- df$lead_sales_operating_revenue_03 - df$sales_operating_revenue
df$dt_financial_income_03<- df$lead_financial_income_03 - df$financial_income
df$dt_sales_cost_03<- df$lead_sales_cost_03 - df$sales_cost
df$dt_sales_operating_cost_03<- df$lead_sales_operating_cost_03 - df$sales_operating_cost
df$dt_unrealized_profit_on_installment_sales_03<- df$lead_unrealized_profit_on_installment_sales_03 - df$unrealized_profit_on_installment_sales
df$dt_sga_03<- df$lead_sga_03 - df$sga
df$dt_financial_costs_03<- df$lead_financial_costs_03 - df$financial_costs
df$dt_employees_end_term_03<- df$lead_employees_end_term_03 - df$employees_end_term
df$dt_avg_temp_employees_03<- df$lead_avg_temp_employees_03 - df$avg_temp_employees
df$dt_r_d_expenses_03<- df$lead_r_d_expenses_03 - df$r_d_expenses
df$dt_capital_investment_03<- df$lead_capital_investment_03 - df$capital_investment
df$dt_officer_bonus_provision_03<- df$lead_officer_bonus_provision_03 - df$officer_bonus_provision
df$dt_goodwill_amortization_03<- df$lead_goodwill_amortization_03 - df$goodwill_amortization
df$dt_sga_sales_commission_03<- df$lead_sga_sales_commission_03 - df$sga_sales_commission
df$dt_sga_storage_costs_03<- df$lead_sga_storage_costs_03 - df$sga_storage_costs
df$dt_sga_advertising_expenses_03<- df$lead_sga_advertising_expenses_03 - df$sga_advertising_expenses
df$dt_sga_sales_expansion_costs_03<- df$lead_sga_sales_expansion_costs_03 - df$sga_sales_expansion_costs
df$dt_sga_allowance_for_doubtful_accounts_03<- df$lead_sga_allowance_for_doubtful_accounts_03 - df$sga_allowance_for_doubtful_accounts
df$dt_sga_officer_compensation_03<- df$lead_sga_officer_compensation_03 - df$sga_officer_compensation
df$dt_sga_provision_for_retirement_benefits_for_officers_03<- df$lead_sga_provision_for_retirement_benefits_for_officers_03 - df$sga_provision_for_retirement_benefits_for_officers
df$dt_sga_provision_for_bonuses_for_directors_03<- df$lead_sga_provision_for_bonuses_for_directors_03 - df$sga_provision_for_bonuses_for_directors
df$dt_sga_personnel_welfare_expenses_03<- df$lead_sga_personnel_welfare_expenses_03 - df$sga_personnel_welfare_expenses
df$dt_sga_provision_for_retirement_benefits_03<- df$lead_sga_provision_for_retirement_benefits_03 - df$sga_provision_for_retirement_benefits
df$dt_sga_depreciation_03<- df$lead_sga_depreciation_03 - df$sga_depreciation
df$dt_sga_goodwill_amortization_03<- df$lead_sga_goodwill_amortization_03 - df$sga_goodwill_amortization
df$dt_sga_rent_03<- df$lead_sga_rent_03 - df$sga_rent
df$dt_sga_taxes_and_public_dues_03<- df$lead_sga_taxes_and_public_dues_03 - df$sga_taxes_and_public_dues
df$dt_sga_patent_fee_paid_03<- df$lead_sga_patent_fee_paid_03 - df$sga_patent_fee_paid
df$dt_sga_rd_cost_03<- df$lead_sga_rd_cost_03 - df$sga_rd_cost
df$dt_sga_warranty_repair_costs_03<- df$lead_sga_warranty_repair_costs_03 - df$sga_warranty_repair_costs
df$dt_sga_other_03<- df$lead_sga_other_03 - df$sga_other
df$dt_gross_profit_03<- df$lead_gross_profit_03 - df$gross_profit
df$dt_rd_expenses_03<- df$lead_rd_expenses_03 - df$rd_expenses
df$dt_operating_income_03<- df$lead_operating_income_03 - df$operating_income
df$dt_non_operating_income_03<- df$lead_non_operating_income_03 - df$non_operating_income
df$dt_non_operating_cost_03<- df$lead_non_operating_cost_03 - df$non_operating_cost
df$dt_ordinary_profit_03<- df$lead_ordinary_profit_03 - df$ordinary_profit
df$dt_income_before_income_taxes_03<- df$lead_income_before_income_taxes_03 - df$income_before_income_taxes
df$dt_net_income_from_continuing_operations_03<- df$lead_net_income_from_continuing_operations_03 - df$net_income_from_continuing_operations
df$dt_net_income_from_non_continuing_operations_03<- df$lead_net_income_from_non_continuing_operations_03 - df$net_income_from_non_continuing_operations
df$dt_net_income_consolidated_03<- df$lead_net_income_consolidated_03 - df$net_income_consolidated

df$gr_current_assets_03<- df$lead_current_assets_03 / df$current_assets
df$gr_cash_and_cash_equivalents_03<- df$lead_cash_and_cash_equivalents_03 / df$cash_and_cash_equivalents
df$gr_non_current_assets_03<- df$lead_non_current_assets_03 / df$non_current_assets
df$gr_tangible_fixed_assets_03<- df$lead_tangible_fixed_assets_03 / df$tangible_fixed_assets
df$gr_intangible_fixed_assets_03<- df$lead_intangible_fixed_assets_03 / df$intangible_fixed_assets
df$gr_sales_goodwill_03<- df$lead_sales_goodwill_03 / df$sales_goodwill
df$gr_patent_right_03<- df$lead_patent_right_03 / df$patent_right
df$gr_software_03<- df$lead_software_03 / df$software
df$gr_goodwill_03<- df$lead_goodwill_03 / df$goodwill
df$gr_leased_asset_03<- df$lead_leased_asset_03 / df$leased_asset
df$gr_trademark_03<- df$lead_trademark_03 / df$trademark
df$gr_design_right_03<- df$lead_design_right_03 / df$design_right
df$gr_other_intangible_fixed_assets_03<- df$lead_other_intangible_fixed_assets_03 / df$other_intangible_fixed_assets
df$gr_total_investment_and_other_assets_03<- df$lead_total_investment_and_other_assets_03 / df$total_investment_and_other_assets
df$gr_development_cost_03<- df$lead_development_cost_03 / df$development_cost
df$gr_total_assets_03<- df$lead_total_assets_03 / df$total_assets
df$gr_current_liabilities_03<- df$lead_current_liabilities_03 / df$current_liabilities
df$gr_non_current_liabilities_03<- df$lead_non_current_liabilities_03 / df$non_current_liabilities
df$gr_total_debt_03<- df$lead_total_debt_03 / df$total_debt
df$gr_non_controlling_interests_03<- df$lead_non_controlling_interests_03 / df$non_controlling_interests
df$gr_total_liabilities_and_net_assets_03<- df$lead_total_liabilities_and_net_assets_03 / df$total_liabilities_and_net_assets
df$gr_capital_attributable_to_owners_of_the_parent_company_03<- df$lead_capital_attributable_to_owners_of_the_parent_company_03 / df$capital_attributable_to_owners_of_the_parent_company
df$gr_sales_operating_revenue_03<- df$lead_sales_operating_revenue_03 / df$sales_operating_revenue
df$gr_financial_income_03<- df$lead_financial_income_03 / df$financial_income
df$gr_sales_cost_03<- df$lead_sales_cost_03 / df$sales_cost
df$gr_sales_operating_cost_03<- df$lead_sales_operating_cost_03 / df$sales_operating_cost
df$gr_unrealized_profit_on_installment_sales_03<- df$lead_unrealized_profit_on_installment_sales_03 / df$unrealized_profit_on_installment_sales
df$gr_sga_03<- df$lead_sga_03 / df$sga
df$gr_financial_costs_03<- df$lead_financial_costs_03 / df$financial_costs
df$gr_employees_end_term_03<- df$lead_employees_end_term_03 / df$employees_end_term
df$gr_avg_temp_employees_03<- df$lead_avg_temp_employees_03 / df$avg_temp_employees
df$gr_r_d_expenses_03<- df$lead_r_d_expenses_03 / df$r_d_expenses
df$gr_capital_investment_03<- df$lead_capital_investment_03 / df$capital_investment
df$gr_officer_bonus_provision_03<- df$lead_officer_bonus_provision_03 / df$officer_bonus_provision
df$gr_goodwill_amortization_03<- df$lead_goodwill_amortization_03 / df$goodwill_amortization
df$gr_sga_sales_commission_03<- df$lead_sga_sales_commission_03 / df$sga_sales_commission
df$gr_sga_storage_costs_03<- df$lead_sga_storage_costs_03 / df$sga_storage_costs
df$gr_sga_advertising_expenses_03<- df$lead_sga_advertising_expenses_03 / df$sga_advertising_expenses
df$gr_sga_sales_expansion_costs_03<- df$lead_sga_sales_expansion_costs_03 / df$sga_sales_expansion_costs
df$gr_sga_allowance_for_doubtful_accounts_03<- df$lead_sga_allowance_for_doubtful_accounts_03 / df$sga_allowance_for_doubtful_accounts
df$gr_sga_officer_compensation_03<- df$lead_sga_officer_compensation_03 / df$sga_officer_compensation
df$gr_sga_provision_for_retirement_benefits_for_officers_03<- df$lead_sga_provision_for_retirement_benefits_for_officers_03 / df$sga_provision_for_retirement_benefits_for_officers
df$gr_sga_provision_for_bonuses_for_directors_03<- df$lead_sga_provision_for_bonuses_for_directors_03 / df$sga_provision_for_bonuses_for_directors
df$gr_sga_personnel_welfare_expenses_03<- df$lead_sga_personnel_welfare_expenses_03 / df$sga_personnel_welfare_expenses
df$gr_sga_provision_for_retirement_benefits_03<- df$lead_sga_provision_for_retirement_benefits_03 / df$sga_provision_for_retirement_benefits
df$gr_sga_depreciation_03<- df$lead_sga_depreciation_03 / df$sga_depreciation
df$gr_sga_goodwill_amortization_03<- df$lead_sga_goodwill_amortization_03 / df$sga_goodwill_amortization
df$gr_sga_rent_03<- df$lead_sga_rent_03 / df$sga_rent
df$gr_sga_taxes_and_public_dues_03<- df$lead_sga_taxes_and_public_dues_03 / df$sga_taxes_and_public_dues
df$gr_sga_patent_fee_paid_03<- df$lead_sga_patent_fee_paid_03 / df$sga_patent_fee_paid
df$gr_sga_rd_cost_03<- df$lead_sga_rd_cost_03 / df$sga_rd_cost
df$gr_sga_warranty_repair_costs_03<- df$lead_sga_warranty_repair_costs_03 / df$sga_warranty_repair_costs
df$gr_sga_other_03<- df$lead_sga_other_03 / df$sga_other
df$gr_gross_profit_03<- df$lead_gross_profit_03 / df$gross_profit
df$gr_rd_expenses_03<- df$lead_rd_expenses_03 / df$rd_expenses
df$gr_operating_income_03<- df$lead_operating_income_03 / df$operating_income
df$gr_non_operating_income_03<- df$lead_non_operating_income_03 / df$non_operating_income
df$gr_non_operating_cost_03<- df$lead_non_operating_cost_03 / df$non_operating_cost
df$gr_ordinary_profit_03<- df$lead_ordinary_profit_03 / df$ordinary_profit
df$gr_income_before_income_taxes_03<- df$lead_income_before_income_taxes_03 / df$income_before_income_taxes
df$gr_net_income_from_continuing_operations_03<- df$lead_net_income_from_continuing_operations_03 / df$net_income_from_continuing_operations
df$gr_net_income_from_non_continuing_operations_03<- df$lead_net_income_from_non_continuing_operations_03 / df$net_income_from_non_continuing_operations
df$gr_net_income_consolidated_03<- df$lead_net_income_consolidated_03 / df$net_income_consolidated

df$dt_dividend_01<-df$lead_dividend_01 - df$dividend
df$dt_market_cap_new_01<-df$lead_market_cap_new_01 - df$market_cap_new
df$dt_simple_q_01<-df$lead_simple_q_01 - df$simple_q
df$dt_roe_01<-df$lead_roe_01 - df$roe
df$dt_roa_01<-df$lead_roa_01 - df$roa
df$dt_per_01<-df$lead_per_01 - df$per

df$gr_dividend_01<-df$lead_dividend_01 / df$dividend
df$gr_market_cap_new_01<-df$lead_market_cap_new_01 / df$market_cap_new
df$gr_simple_q_01<-df$lead_simple_q_01 / df$simple_q
df$gr_roe_01<-df$lead_roe_01 / df$roe
df$gr_roa_01<-df$lead_roa_01 / df$roa
df$gr_per_01<-df$lead_per_01 / df$per

df$dt_dividend_03<-df$lead_dividend_03 - df$dividend
df$dt_market_cap_new_03<-df$lead_market_cap_new_03 - df$market_cap_new
df$dt_simple_q_03<-df$lead_simple_q_03 - df$simple_q
df$dt_roe_03<-df$lead_roe_03 - df$roe
df$dt_roa_03<-df$lead_roa_03 - df$roa
df$dt_per_03<-df$lead_per_03 - df$per

df$gr_dividend_03<-df$lead_dividend_03 / df$dividend
df$gr_market_cap_new_03<-df$lead_market_cap_new_03 / df$market_cap_new
df$gr_simple_q_03<-df$lead_simple_q_03 / df$simple_q
df$gr_roe_03<-df$lead_roe_03 / df$roe
df$gr_roa_03<-df$lead_roa_03 / df$roa
df$gr_per_03<-df$lead_per_03 / df$per

df$dt_lead_market_cap_01<-df$lead_market_cap_01 - df$market_cap
df$gr_lead_market_cap_01<-df$lead_market_cap_01 / df$market_cap
df$dt_market_cap_03<-df$lead_market_cap_03 - df$market_cap
df$gr_market_cap_03<-df$lead_market_cap_03 / df$market_cap

colnames(df)

#年ダミーを入れる
for (i in 2017:2021){
  dummy <- paste0("d_",i)
  tmp <- if_else(df$fiscal_year == i, 1, 0)
  tmp <- data.frame(tmp)
  names(tmp) <- c(dummy)
  df <- cbind(df, tmp)
}

# vars <- c()
# df %>%
#   for (var in vars){
#   tmp <- paste("lead_",var,"_01")
#   mutate(tmp = lead(var, n = 1))
#   tmp <- paste("lead_",var,"_03")
# }
  

#設立年を抽出する（スプレッドシートで加工済み）
# founded_year <- as.numeric(substr(as.character(df$founded_date), 1, 4))
# names(founded_year) <- c("founded_year")
# df <- cbind(df, founded_year)

# 後でleft_joinして、変化分・変化率を導けるように準備する
# fiscal_year <- data.frame(df$fiscal_year - 1)
# df_minus1 <- select(df, -fiscal_year)
# names(fiscal_year) <- c("fiscal_year")
# df_minus1 <- cbind(df_minus1, fiscal_year)


# データエクスポート
# write.csv(x = df, file = "Documents/GitHub/final_report/data/new/df_joined.csv")


## データ読み込み ##
# df <- read_csv("Documents/GitHub/final_report/data/new/target.csv")
# df$continuity_period <- as.integer(df$continuity_period)
# df$continuity_period

# fwrite(df,"Documents/GitHub/final_report/data/new/all.csv")
# df <- read_csv("Documents/GitHub/final_report/data/new/all.csv")

## 記述統計量を作成 ##
# install.packages("summarytools", dependencies = T)
# install.packages("gtsummary")
# install.packages("tidyverse")
# install.packages("gt")
# install.packages("flextable")
# library(summarytools)
library(gtsummary)
library(tidyverse)

df[is.na(df)] <- 0
df<-df[!(df$fiscal_year==2021),]
# df %>% 
#   descr()

# df %>% 
#   tbl_summary()

df_short<-df[!(df$fiscal_year==2020),]

df_long<-df[!(df$fiscal_year==2020),]
df_long<-df_long[!(df_long$fiscal_year==2019),]
df_long<-df_long[!(df_long$fiscal_year==2018),]

## df_shortについて製造業と非製造業、saas企業に分ける ##
df_short[is.na(df_short)] <- 0
df_saas_short <- df_short[(df_short$d_b2b_saas==1),]
df_manu_short <- df_short[(df_short$d_manufacture==1),]
df_non_manu_short <- df_short[!(df_short$d_manufacture==1),]

## df_longについて製造業と非製造業、saas企業に分ける ##
df_long[is.na(df_long)] <- 0
df_saas_long <- df_long[(df_long$d_b2b_saas==1),]
df_manu_long <- df_long[(df_long$d_manufacture==1),]
df_non_manu_long <- df_long[!(df_long$d_manufacture==1),]

## 固定効果モデルで推定する ##
library(plm)

### 単独年 ###
fe_model_tmp <- market_cap_new ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + cash_and_cash_equivalents +
              non_current_assets + tangible_fixed_assets + intangible_fixed_assets + sales_goodwill + patent_right + software + goodwill + leased_asset + trademark +
              design_right + other_intangible_fixed_assets + total_investment_and_other_assets + development_cost +
              current_liabilities + non_current_liabilities +
              sales_operating_revenue + financial_income + sales_cost + sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs +
              sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation +
              sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits +
              sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other +
              employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + officer_bonus_provision + goodwill_amortization

# fe_model_tmp <- log(market_cap_new) ~ log(founded_year) + log(n_consolidated_subsidiaries) + log(n_consolidated_subsidiaries_ipo) + log(current_assets) + log(cash_and_cash_equivalents) +
#                 log(non_current_assets) + log(tangible_fixed_assets) + log(intangible_fixed_assets) + log(sales_goodwill) + log(patent_right) + log(software) + log(goodwill) + log(leased_asset) + log(trademark) + 
#                 log(design_right) + log(other_intangible_fixed_assets) + log(total_investment_and_other_assets) + log(development_cost) + 
#                 log(current_liabilities) + log(non_current_liabilities) +
#                 log(sales_operating_revenue) + log(financial_income) + log(sales_cost) + log(sales_operating_cost) + log(unrealized_profit_on_installment_sales) + log(financial_costs) + 
#                 log(sga_sales_commission) + log(sga_storage_costs) + log(sga_advertising_expenses) + log(sga_sales_expansion_costs) + log(sga_allowance_for_doubtful_accounts) + log(sga_officer_compensation) + 
#                 log(sga_provision_for_retirement_benefits_for_officers) + log(sga_provision_for_bonuses_for_directors) + log(sga_personnel_welfare_expenses) + log(sga_provision_for_retirement_benefits) + 
#                 log(sga_depreciation) + log(sga_goodwill_amortization) + log(sga_rent) + log(sga_taxes_and_public_dues) + log(sga_patent_fee_paid) + log(sga_rd_cost) + log(sga_warranty_repair_costs) + log(sga_other) + 
#                 log(employees_end_term) + log(avg_temp_employees) + log(r_d_expenses) + log(capital_investment) + log(officer_bonus_provision) + log(goodwill_amortization)

df_saas_short <- df_saas_short %>% distinct(corp_name,fiscal_year,.keep_all=TRUE)
saas_short <- pdata.frame(df_saas_short, index = c("corp_name", "fiscal_year"))
saas_fe <- plm(fe_model_tmp, data=saas_short, model="within", effect="twoways")
# summary(saas_fe)

df_non_manu_short <- df_non_manu_short %>% distinct(corp_name,fiscal_year,.keep_all=TRUE)
non_manu_short <- pdata.frame(df_non_manu_short, index = c("corp_name", "fiscal_year"))
non_manu_fe <- plm(fe_model_tmp, data=non_manu_short, model="within" )
# summary(non_manu_fe)

df_manu_short <- df_manu_short %>% distinct(corp_name,fiscal_year,.keep_all=TRUE)
manu_short <- pdata.frame(df_manu_short, index = c("corp_name", "fiscal_year"))
manu_fe <- plm(fe_model_tmp , data=manu_short, model="within", effect="twoways")
# summary(manu_fe)

# install.packages("modelsummary")
# library(modelsummary)
# 
# out <- list(
#   "製造業" = manu_fe,
#   "非製造業" = non_manu_fe,
#   "SaaS企業(非製造業)" = saas_fe
# )
# msummary(out)

library(stargazer)
stargazer(saas_fe, manu_fe, non_manu_fe, type = "html", out = "Documents/GitHub/final_report/data/out00.doc")

## 複数年の変化を比較する（1年後） ##
fe_model_01 <- dt_market_cap_new_01 ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + 
              dt_current_assets_01 + dt_cash_and_cash_equivalents_01 +
              dt_non_current_assets_01 + dt_tangible_fixed_assets_01 + dt_intangible_fixed_assets_01 + dt_sales_goodwill_01 + 
              dt_patent_right_01 + dt_software_01 + dt_goodwill_01 + dt_leased_asset_01 + dt_trademark_01 + 
              dt_design_right_01 + dt_other_intangible_fixed_assets_01 + dt_total_investment_and_other_assets_01 + dt_development_cost_01 + 
              total_assets +
              dt_current_liabilities_01 + dt_non_current_liabilities_01 +
              total_debt +
              sales_operating_revenue + financial_income + sales_cost + sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs + 
              dt_sales_operating_revenue_01 + dt_financial_income_01 + dt_sales_cost_01 + dt_sales_operating_cost_01 + dt_unrealized_profit_on_installment_sales_01 + dt_financial_costs_01 + 
              sga +
              dt_sga_sales_commission_01 + dt_sga_storage_costs_01 + dt_sga_advertising_expenses_01 + dt_sga_sales_expansion_costs_01 + dt_sga_allowance_for_doubtful_accounts_01 + dt_sga_officer_compensation_01 + 
              dt_sga_provision_for_retirement_benefits_for_officers_01 + dt_sga_provision_for_bonuses_for_directors_01 + dt_sga_personnel_welfare_expenses_01 + dt_sga_provision_for_retirement_benefits_01 + 
              dt_sga_depreciation_01 + dt_sga_goodwill_amortization_01 + dt_sga_rent_01 + dt_sga_taxes_and_public_dues_01 + dt_sga_patent_fee_paid_01 + dt_sga_rd_cost_01 + dt_sga_warranty_repair_costs_01 + dt_sga_other_01 + 
              employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + officer_bonus_provision + goodwill_amortization + 
              dt_employees_end_term_01 + dt_avg_temp_employees_01 + dt_r_d_expenses_01 + dt_capital_investment_01 + dt_officer_bonus_provision_01 + dt_goodwill_amortization_01 + 
              d_2017 + d_2018
df_saas_short <- df_saas_short %>% distinct(corp_name,fiscal_year,.keep_all=TRUE)
saas_short <- pdata.frame(df_saas_short, index = c("corp_name", "fiscal_year"))
saas_fe_01 <- plm(fe_model_01, data=saas_short, model="within" )


df_non_manu_short <- df_non_manu_short %>% distinct(corp_name,fiscal_year,.keep_all=TRUE)
non_manu_short <- pdata.frame(df_non_manu_short, index = c("corp_name", "fiscal_year"))
non_manu_fe_01 <- plm(fe_model_01, data=df_non_manu_short, model="within" )
# summary(non_manu_fe)

df_manu_short <- df_manu_short %>% distinct(corp_name,fiscal_year,.keep_all=TRUE)
manu_short <- pdata.frame(df_manu_short, index = c("corp_name", "fiscal_year"))
manu_fe_01 <- plm(fe_model_01, data=manu_short, model="within", effect = "twoways")
summary(manu_fe_01)
fixef(manu_fe_01)

# library(modelsummary)
# 
# out01 <- list(
#   "製造業" = manu_fe_01,
#   "非製造業" = non_manu_fe_01,
#   "SaaS企業(非製造業)" = saas_fe_01
# )
# msummary(out01)

# library(stargazer)
stargazer(manu_fe_01, non_manu_fe_01, saas_fe_01, type = "html", out = "Documents/GitHub/final_report/data/out01.doc")

## 複数年の変化を比較する（3年後） ##
fe_model_03 <- dt_market_cap_new_03 ~ founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + 
                dt_current_assets_03 + dt_cash_and_cash_equivalents_03 +
                dt_non_current_assets_03 + dt_tangible_fixed_assets_03 + dt_intangible_fixed_assets_03 + dt_sales_goodwill_03 + 
                dt_patent_right_03 + dt_software_03 + dt_goodwill_03 + dt_leased_asset_03 + dt_trademark_03 + 
                dt_design_right_03 + dt_other_intangible_fixed_assets_03 + dt_total_investment_and_other_assets_03 + dt_development_cost_03 + 
                total_assets +
                dt_current_liabilities_03 + dt_non_current_liabilities_03 +
                total_debt +
                sales_operating_revenue + financial_income + sales_cost + sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs + 
                dt_sales_operating_revenue_03 + dt_financial_income_03 + dt_sales_cost_03 + dt_sales_operating_cost_03 + dt_unrealized_profit_on_installment_sales_03 + dt_financial_costs_03 + 
                sga +
                dt_sga_sales_commission_03 + dt_sga_storage_costs_03 + dt_sga_advertising_expenses_03 + dt_sga_sales_expansion_costs_03 + dt_sga_allowance_for_doubtful_accounts_03 + dt_sga_officer_compensation_03 + 
                dt_sga_provision_for_retirement_benefits_for_officers_03 + dt_sga_provision_for_bonuses_for_directors_03 + dt_sga_personnel_welfare_expenses_03 + dt_sga_provision_for_retirement_benefits_03 + 
                dt_sga_depreciation_03 + dt_sga_goodwill_amortization_03 + dt_sga_rent_03 + dt_sga_taxes_and_public_dues_03 + dt_sga_patent_fee_paid_03 + dt_sga_rd_cost_03 + dt_sga_warranty_repair_costs_03 + dt_sga_other_03 + 
                employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + officer_bonus_provision + goodwill_amortization + 
                dt_employees_end_term_03 + dt_avg_temp_employees_03 + dt_r_d_expenses_03 + dt_capital_investment_03 + dt_officer_bonus_provision_03 + dt_goodwill_amortization_03
df_saas_long <- df_saas_long %>% distinct(corp_name,fiscal_year,.keep_all=TRUE)
saas_long <- pdata.frame(df_saas_long, index = c("corp_name", "fiscal_year"))
saas_fe_03 <- plm(fe_model_03, data=saas_long, model="within")

df_non_manu_long <- df_non_manu_long %>% distinct(corp_name,fiscal_year,.keep_all=TRUE)
non_manu_long <- pdata.frame(df_non_manu_long, index = c("corp_name", "fiscal_year"))
non_manu_fe_03 <- plm(fe_model_03, data=non_manu_long, model="within")
# summary(non_manu_fe)

df_manu_long <- df_manu_long %>% distinct(corp_name,fiscal_year,.keep_all=TRUE)
manu_long <- pdata.frame(df_manu_long, index = c("corp_name", "fiscal_year"))
manu_fe_03 <-plm(fe_model_03, data=manu_long, model="within")

# library(modelsummary)
# 
# out03 <- list(
#   "製造業" = manu_fe_03,
#   "非製造業" = non_manu_fe_03,
#   "SaaS企業(非製造業)" = saas_fe_03
# )
# msummary(out03)

library(stargazer)
stargazer(manu_fe_03, non_manu_fe_03, saas_fe_03, type = "html", out = "Documents/GitHub/final_report/data/out03.doc")


# ## 試しに回帰してみる
# df[is.na(df)] <- 0
# out <- lm(market_cap ~ continuity_period + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + cash_and_cash_equivalents +
#             `non-current_assets` + tangible_fixed_assets + intangible_fixed_assets + sales_goodwill + patent_right + software + goodwill + leased_asset + trademark + 
#             design_right + other_intangible_fixed_assets + total_investment_and_other_assets + development_cost + 
#             current_liabilities + `non-current_liabilities` +
#             sales_operating_revenue + financial_income + sales_cost + sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs + 
#             sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation + 
#             sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits + 
#             sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other + 
#             employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + officer_bonus_provision + goodwill_amortization + 
#             d_2017 + d_2018 + d_2019 + d_2020 + d_2021, df) #dividend, d2016, ※founded_yearは、continuity_periodで置き換えた
# summary(out)
# founded_year + n_consolidated_subsidiaries + n_consolidated_subsidiaries_ipo + current_assets + cash_and_cash_equivalents +
#         `non-current_assets` + tangible_fixed_assets + intangible_fixed_assets + sales_goodwill + patent_right + software + goodwill + leased_asset + trademark + 
#         design_right + other_intangible_fixed_assets + total_investment_and_other_assets + development_cost + total_assets + 
#         current_liabilities + `non-current_liabilities` + total_debt + 
#         `non-controlling_interests` + total_liabilities_and_net_assets + capital_attributable_to_owners_of_the_parent_company + 
#         sales_operating_revenue + financial_income + sales_cost + sales_operating_cost + unrealized_profit_on_installment_sales + financial_costs + sga + 
#         sga_sales_commission + sga_storage_costs + sga_advertising_expenses + sga_sales_expansion_costs + sga_allowance_for_doubtful_accounts + sga_officer_compensation + 
#         sga_provision_for_retirement_benefits_for_officers + sga_provision_for_bonuses_for_directors + sga_personnel_welfare_expenses + sga_provision_for_retirement_benefits + 
#         sga_depreciation + sga_goodwill_amortization + sga_rent + sga_taxes_and_public_dues + sga_patent_fee_paid + sga_rd_cost + sga_warranty_repair_costs + sga_other + 
#         gross_profit + rd_expenses + operating_income + `Non-operating_income` + `Non-operating_cost` + ordinary_profit + income_before_income_taxes + net_income_from_continuing_operations + 
#         net_income_from_non-continuing_operations + net_income_consolidated + 
#         employees_end_term + avg_temp_employees + r_d_expenses + capital_investment + officer_bonus_provision + goodwill_amortization + 
#         dividend + d_2016 + d_2017 + d_2018 + d_2019 + d_2020 + d_2021 + simple_q + roe + roa + per


#特定の列がNAとなる行を抽出する
# df[!complete.cases(df$simple_q),]
# 
# ## 相関関係をみる ##
# #1.散布図行列(できない)
# par(family= "HiraKakuProN-W3")
# pairs(aa, panel = panel.smooth)
# 
# #2.散布図・相関行列図(できない)
# install.packages('psych')
# library(psych)
# psych::pairs.panels(tmp)
# 
# #3.相関行列図（円）
# install.packages('corrplot')
# library(corrplot)
# corrplot::corrplot(cor(iris[,-5]))
# 
# # 回帰分析を回す(できない)
# lm(formula = "<その他項目>のれん・負ののれん償却額" ~ ., data=tmp)
