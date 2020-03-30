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
        drink['adelhyde'] = int(row['adelhyde'])
        drink['bronsonExtract'] = int(row['bronson_extract'])
        drink['powderedDelta'] = int(row['powdered_delta'])
        drink['flanergide'] = int(row['flanergide'])
        drink['karmotrine'] = int(row['karmotrine'])
        drink['addIce'] = row['add_ice'] == 'Y'
        drink['age'] = row['age'] == 'Y'
        drink['wait'] = row['wait'] == 'Y'
        items[row['name']] = drink

with open(output_filename, 'w') as output_file:
    output_file.write(json.dumps(items, indent=2))
