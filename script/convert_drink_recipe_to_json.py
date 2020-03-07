import csv
import json
import pathlib
import sys

if len(sys.argv) < 3:
    print('Usage: {command} input_filename.csv output_filename.json'.format(command=sys.argv[0]))
    exit(1)

input_filename = pathlib.Path(sys.argv[1])
output_filename = pathlib.Path(sys.argv[2])

if not input_filename.exists():
    print('{filename} not found!'.format(filename=input_filename))
    exit(1)

items = dict()
with open(input_filename, 'r') as csv_file:
    reader = csv.DictReader(csv_file)
    for row in reader:
        drink = dict()
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
        items[row['name']] = drink

with open(output_filename, 'w') as output_file:
    output_file.write(json.dumps(items, indent=2))
