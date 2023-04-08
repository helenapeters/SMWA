install.packages("tuber")
install.packages('https')
library(tuber)

##Scrape Youtube Data
myclientid='24403699099-gvcon4ph9qbvotffogimd6fg932m1t6f.apps.googleusercontent.com'
clientsecret='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'
yt_oauth(myclientid,clientsecret,token="")

lastcommentseng1=get_all_comments('1OeC9CGtWcM')
IfDarknessHadASon=get_all_comments('x_t53a5Ons0')
LuxAeterna=get_all_comments('_u-7rWKnVVo')

##Save R Data Objects
save(lastcommentseng1, file = '72SeasonsYoutubeComments.RData')
save(IfDarknessHadASon, file = 'IfDarknessHadASonYoutubeComments.RData')
save(LuxAeterna, file = 'LuxAeternaYoutubeComments.Rdata')
