
# パッケージのインストール
install.packages("rtweet")

#パッケージのロード
library(rtweet)

auth_setup_default()


# パッケージのインストール（※最初の1回だけ)

# install.packages("rtweet")

#パッケージのロード（※必要な場合）
# library(rtweet)




# 自己紹介文に「投資」が入っているユーザを200人まで取得
useDSs <- search_users("投資", n = 200)
useDSs

# ワードを日本株，中国株，米国株でそれぞれツイートを取得
rt <- search_tweets(q = "日本株", n = 1000, include_rts = FALSE, retryonratelimit = TRUE)
?search_tweets

# 中身を確認
rt

# データの大きさを確認
library(tidyverse) # %>% を使うためにtidyverseパッケージを呼び出す
rt %>% dim() # dim()でデータ行列の大きさを確認する

# データの列名を確認する
rt %>% colnames() 

# tweetが更新されるとデータが変わってしまうので、データを保存する。
# それぞれnihonkabu, tyuugokukabu, beikokukabuという名前にする
save(rt, file = "nihonkabu.Rdata") 

# データの読み込み
# それぞれnihonkabu, tyuugokukabu, beikokukabuという名前にする

load("nihonkabu.Rdata") 

# Rdataファイルに格納されているオブジェクトのリストを確認
ls()

####################### 形態素解析の実行 #######################

# tweet本文をテキストに保存
# それぞれnihonkabu, tyuugokukabu, beikokukabuという名前にする

rt %>% select(full_text) %>% pull() %>% write("nihonkabu.txt") 

# このファイルを対象に形態素解析を実行
# それぞれnihonkabu, tyuugokukabu, beikokukabuテキストファイルについて実行

library(RMeCab)
txt_df <- docDF("nihonkabu.txt", type = 1) # typeオプション=1ならば形態素単位で分割

# データフレームの行数（語彙数）を確認
txt_df %>% NROW()

# 不要な記号・単語等を削除するために、まず品詞情報を確認する
txt_df %>% select(POS1) %>% distinct() %>% pull() # 品詞大分類（POS1）の内容を確認。「フィラー」とは、つなぎ表現（えーっと、うーん、など）のこと
txt_df %>% select(POS2) %>% distinct() %>% pull() # 品詞細分類（POS2）の内容を確認

# 分析に使う形態素を決めて、それらを抽出する
txt_df <- txt_df %>% filter(POS1 %in% c("名詞", "形容詞", "動詞"),
                            POS2 %in% c("一般", "自立", "非自立", "助詞類接続"))

# 最終的なデータフレームの行数（語彙数）を確認
txt_df %>% NROW()


####################### ワードクラウドの作成 #######################


# 日本語を含むplotsの文字化け防止作業
install.packages("ragg") # raggパッケージをインストール
# 上記実行後、RStudioのTools > Global Options > General > Graphics > Backendを[AGG]にする
# Apply して OKし、RStudioを再起動する。その後、以下を実行してください。
# ちなみに、再起動すると、今までロードしてきたlibrary類はリセットされます。再度ロードしてください。
# インストールしたraggパッケージと、[AGG]の設定は、今後Rstudioを再起動しても引き継がれます。
browseURL("https://uribo.hatenablog.com/entry/2021/03/29/202756") # 参考


#wordcloud2とhtmlwidgetsのインストール
install.packages("wordcloud2")
library(wordcloud2)

# ストップワードの削除
# ストップワードリストは、京都大学情報学研究科社会情報学専攻 田中克己研究室で開発・公開されているSlothLib
# (http://www.dl.kuis.kyoto-u.ac.jp/slothlib/)における成果の一部を利用

stop_words <- read_tsv("http://svn.sourceforge.jp/svnroot/slothlib/CSharp/Version1/SlothLib/NLP/Filter/StopWord/word/Japanese.txt",
                       col_names = "TERM")
ja_stop_words <- stop_words %>% 
                        add_row(TERM = c("ある", "ん", "する","てる", "いる", "の", "いう", "しまう", "なる", "日本株","株","https","co","t"))

# anti_join()関数で、ストップワードに一致しないTERMを抽出
# それぞれnihonkabu, tyuugokukabu, beikokukabuのそれぞれのテキストファイルで実行
txt2_df <- txt_df %>% select(TERM, FREQ = nihonkabu.txt) %>% 
  arrange(FREQ) %>% tail(100) %>% anti_join(ja_stop_words, by = "TERM")

# ストップワードを削除したデータフレーム
txt2_df

#ワードクラウドの作成
  txt2_df %>% wordcloud2()


####################### ネットワークグラフの描画 #######################


# 日本語を含むplotsの文字化け防止作業
install.packages("ragg") # raggパッケージをインストール
  
# それぞれnihonkabu, tyuugokukabu, beikokukabuのそれぞれのテキストファイルで実行
txt3_df <- docDF("nihonkabu.txt", type = 1,
                 pos = c("名詞", "形容詞", "動詞"), N = 2, nDF = TRUE)

# 先頭の5行を確認
txt3_df %>% head(5)

# グラフ作成用のパッケージをインストール＆ロード
install.packages("igraph")
library(igraph)
install.packages("ggraph")
library(ggraph)

#上位50ペアに限定して可視化する
txt3_df %>% arrange(nihonkabu.txt)　%>% tail(50) %>% 
  select(N1, N2, nihonkabu.txt) %>% graph_from_data_frame() %>% 
  ggraph(layout = 'graphopt') +
  geom_edge_diagonal(alpha = 1, label_colour = "blue") +
  geom_node_label(aes(label = name), size = 5,
  repel = TRUE) # repelをTRUEにすると、ラベルの重なりを避けられる


####################### tweetの内容判定 #######################

# 日本語極性辞書（名詞編）を使う。
browseURL("https://www.nlp.ecei.tohoku.ac.jp/research/open-resources/")

# 辞書の読み込み。negativeなら-1点、positiveなら1点、neutralなら0点をtweetに与える処理
negaposi <- read_tsv(
            "http://www.cl.ecei.tohoku.ac.jp/resources/sent_lex/pn.csv.m3.120408.trim", 
            col_names = c("TERM", "VALUE", "CRITERIA"))
negaposi <- negaposi %>% mutate(VALUE = case_when(
                                                  VALUE == "n" ~ -1,
                                                  VALUE == "p" ~ 1,
                                                  TRUE ~ 0))
negaposi %>% select(VALUE) %>% summary()


# tweetごとに形態素解析をし、tweetのID（id_str）と関連付けて保存する関数rmecabc_()を定義。tweetごとにデータフレームを返す
rmecabc_ <- function(si, txt){
  txt <- unlist(RMeCabC(txt, 1))
  tibble(id_str = si, TERM = txt)
}

# tweetごとのデータフレームを、map2_dfr()で結合する
library(purrr)
tweets <- map2_dfr(rt$id_str, rt$full_text, ~ rmecabc_(..1, ..2))
tweets %>% NROW()
tweets %>% head()

# 極性辞書と、tweetのデータフレームを結合する
negaposi <- negaposi %>% select(TERM, VALUE)
dat2 <- tweets %>% left_join(negaposi)

# tweetごとに極性値の合計を求める。最もネガティブな投稿の得点はMin.、最もポジティブな投稿の得点はMax.
dat2 <- dat2 %>% group_by(id_str) %>% 
  summarise(VALUE = sum(VALUE, na.rm = TRUE))
dat2 %>% select(VALUE) %>% summary()

# 最もポジティブな投稿を確認する

most <- dat2 %>% filter(VALUE == max(VALUE))
most %>% map_df(~ filter(rt, id_str %in% .x)) %>% select(text) %>% pull

# 最もネガティブな投稿を確認する
min <- dat2 %>% filter(VALUE == min(VALUE))
min %>% map_df(~ filter(rt, id_str %in% .x)) %>% select(text) %>% pull

