import pandas
import json
import requests
import csv

jsonPath = "https://api.covid19uk.live/historyfigures"
r = requests.get(jsonPath)

dataJSON = json.dumps((r.json()))
decodeJSON = json.loads(dataJSON)

historyDF = pandas.DataFrame(decodeJSON["data"])
historyDF.to_csv(r'/Users/chaolinhan/Documents/GitHub/Covid19predict/daraPreProcess/historyfigure.csv', na_rep= "NULL", index=False)