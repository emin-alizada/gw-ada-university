import numpy as np
import pandas
from pandas import DataFrame

# arr = np.random.rand(24)
# print(arr)

# arr = pandas.DataFrame([1, 4, 3, 1])

# arr.info()
# print(arr.describe())
# print(arr.nunique())


# f = lambda x: '% .2f' % x
# print(arr.applymap(f))


data1 = {
    'cities': ['baku', 'sumgait', 'gence'],
    'year': [2022, 2020, 2022],
    'pop': [1.8, 2.5, 2.0]
}

dc1 = DataFrame(data1)
dc2 = DataFrame(data1)
dc3 = DataFrame(data1)

tmp = dc1.append(dc2, ignore_index=True)
dcnew = tmp.append(dc3, ignore_index=True)

print(dcnew)