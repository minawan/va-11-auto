import csv
import pprint

items = dict()
with open('VA-11_Cheat_Sheet.csv') as csv_file:
  reader = csv.DictReader(csv_file)
  for row in reader:
    drink = dict()
    drink['flavor'] = row['flavor']
    drink['kind'] = row['kind']
    drink['trait'] = row['trait']
    drink['price'] = int(row['price'])
    recipe = dict()
    recipe['adelhyde'] = int(row['adelhyde']) if row['adelhyde'] else 0
    recipe['bronson_extract'] = int(row['bronson_extract']) if row['bronson_extract'] else 0
    recipe['powdered_delta'] = int(row['powdered_delta']) if row['powdered_delta'] else 0
    recipe['flanergide'] = int(row['flanergide']) if row['flanergide'] else 0
    recipe['karmotrine'] = -1 if row['karmotrine'] == 'opt' else int(row['karmotrine']) if row['karmotrine'] else 0
    recipe['ice'] = row['ice'] == 'Y'
    recipe['age'] = row['age'] == 'Y'
    recipe['mix'] = row['mix'] == 'Y'
    recipe['blend'] = row['blend'] == 'Y'
    drink['recipe'] = recipe
    items[row['name']] = drink
pprint.pprint(items)
