import csv
import json
import pathlib
import pprint
import subprocess

REFERENCE_SHEET_BASENAME = 'VA-11_Cheat_Sheet'
REFERENCE_SHEET_FILENAME_ODS = REFERENCE_SHEET_BASENAME + '.ods'
OUTPUT_FILENAME = REFERENCE_SHEET_BASENAME + '.json'

# Convert reference sheet from ods to csv.
ods_path = pathlib.Path(REFERENCE_SHEET_FILENAME_ODS)
if not ods_path.exists():
  print('{filename} not found!'.format(filename=REFERENCE_SHEET_FILENAME_ODS))
  exit()
subprocess.run(['libreoffice', '--headless', '--convert-to', 'csv', REFERENCE_SHEET_FILENAME_ODS])

items = []
with open(REFERENCE_SHEET_BASENAME + '.csv') as csv_file:
  reader = csv.DictReader(csv_file)
  for row in reader:
    drink = dict()
    drink['name'] = row['name']
    drink['flavor'] = row['flavor']
    drink['kind'] = row['kind']
    drink['trait'] = row['trait']
    drink['price'] = int(row['price'])
    recipe = dict()
    recipe['adelhyde'] = int(row['adelhyde']) if row['adelhyde'] else 0
    recipe['bronsonExtract'] = int(row['bronson_extract']) if row['bronson_extract'] else 0
    recipe['powderedDelta'] = int(row['powdered_delta']) if row['powdered_delta'] else 0
    recipe['flanergide'] = int(row['flanergide']) if row['flanergide'] else 0
    recipe['karmotrine'] = -1 if row['karmotrine'] == 'opt' else int(row['karmotrine']) if row['karmotrine'] else 0
    recipe['addIce'] = row['add_ice'] == 'Y'
    recipe['age'] = row['age'] == 'Y'
    recipe['wait'] = row['wait'] == 'Y'
    drink['recipe'] = recipe
    items.append(drink)

with open(OUTPUT_FILENAME, 'w') as output_file:
  output_file.write(json.dumps(items, indent=2))
