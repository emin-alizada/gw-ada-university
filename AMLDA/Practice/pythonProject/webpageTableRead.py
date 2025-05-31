# import requests as req
# import pandas as pd
#
# url = 'https://www.worldometers.info/coronavirus/'
# header = {
#     "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_4) AppleWebKit/600.7.12 (KHTML, like Gecko) Version/8.0.7 Safari/600.7.12",
#     "X-Requested-With": "XMLHttpRequest"
# }
#
# r = req.get(url, headers=header)
#
# dfs = pd.read_html(r.text, displayed_only=False)
#
# print(dfs)
# # dfs[1].loc[8:20, ['Country,Other', 'ActiveCases']]
