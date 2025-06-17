library(readxl)

df <- read_excel("response_origin.xlsx")
num_cols <- ncol(df)
cols_delete_front <- 1:10
cols_delete_back <- (num_cols - 6):num_cols
cols_delete <- c(cols_delete_front, cols_delete_back)
df <- df[, -cols_delete]
ncol(df)


colnames(df) <- paste0("Q", 1:47)

origin <- c('전혀 아니다.', '아니다.', '보통이다.', '그렇다.', '매우 그렇다.')
likert <- c('1', '2', '3', '4', '5')

cols_convert <- 1:44
df[cols_convert] <- lapply(df[cols_convert], function(x) plyr::mapvalues(x, from = origin, to = likert))



origin_text <- c('난관을 극복하고 목표를 이루게 하는 힘이 있으며 이를 조직에 전파하고 구성원을 한 방향으로 모은다.',
                 '전문성과 축적된 경험을 토대로 한 아이디어로 남들과 차별화한다.',
                 '도전정신으로 새로운 방법을 모색하고 행동으로 옮긴다.',
                 '탁월한 기술, 전문지식, 또는 외국어 능력을 가지고 새로운 기술을 배우는 노력을 보인다.',
                 '부지런하고 근면하다.', '회사의 방향과 규칙을 잘 따르거나 지키고 맡은 업무를 잘 처리한다.')
text_num <- c('1', '2', '3', '4', '5', '6')
text_convert <- 45
df[text_convert] <- lapply(df[text_convert], function(x) plyr::mapvalues(x, from = origin_text, to = text_num))

write.csv(df, file = 'Leadership_preprocessing.csv', row.names = FALSE, fileEncoding = "cp949")
