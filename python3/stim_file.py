
import requests
import sys
import json


arg1 = sys.argv[1]
print(arg1)
with open(arg1) as rdr:
    js = json.load(rdr)

print(js['cols'], js['cols'][0], js['cols'][1])
r = requests.post('http://localhost:5000/stats/regress-with-col', json=js)
print(r.json())

 