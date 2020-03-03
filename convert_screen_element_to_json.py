import csv
import json
import pathlib
import pprint

REFERENCE_SHEET_BASENAME = 'ScreenElement'
OUTPUT_FILENAME = REFERENCE_SHEET_BASENAME + '.json'

items = []
with open(REFERENCE_SHEET_BASENAME + '.csv') as csv_file:
  reader = csv.DictReader(csv_file)
  for row in reader:
    elem = dict()
    elem['name'] = row['name']
    elem['category'] = row['category']
    elem['xCoord'] = int(row['xCoord'])
    elem['yCoord'] = int(row['yCoord'])
    elem['shortcut'] = int(row['shortcut'])
    items.append(elem)

with open(OUTPUT_FILENAME, 'w') as output_file:
  output_file.write(json.dumps(items, indent=2))
