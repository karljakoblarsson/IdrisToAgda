#!/usr/bin/python

import sys
import pandas
from collections import defaultdict
import csv


output = sys.argv[1]
files = sys.argv[2:]

stats = defaultdict(int)

for f in files:
    csv = pandas.read_csv(f)
    for index, row in csv.iterrows():
        key = (row[0])
        number = int(row[1])
        stats[key] += number

total = 0

for val in stats.values():
    total += val

with open(output, 'w') as f:
    for key in stats.keys():
        f.write("{0},{1},{2:.0%}\n".format(key, stats[key], stats[key]/total))
exit()
