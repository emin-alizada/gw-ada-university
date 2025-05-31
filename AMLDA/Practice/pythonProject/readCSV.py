import csv
import pandas as pd

path = "./test.csv"
lines = []
with open(path) as file:
    csvFile = csv.reader(file, delimiter=',')
    line_count = 0
    for row in csvFile:
        lines.append(row)

csvDF = pd.DataFrame(lines)
print(csvDF)