all: build

build: thrift DrinkRecipe.json ScreenElement.csv

thrift: command.thrift recipe.thrift action.thrift
	mkdir -p thrift
	thrift -r --gen py --gen go:package_prefix=github.com/minawan/va-11-auto/thrift/gen-go/ -o thrift/ command.thrift
	thrift -r --gen py --gen go:package_prefix=github.com/minawan/va-11-auto/thrift/gen-go/ -o thrift/ action.thrift

DrinkRecipe.json: convert_drink_recipe_to_json.py DrinkRecipe.ods
	python3 convert_drink_recipe_to_json.py

ScreenElement.csv: ScreenElement.ods
	libreoffice --headless --convert-to csv ScreenElement.ods

clean:
	rm -f DrinkRecipe.csv
	rm -f DrinkRecipe.json
	rm -f ScreenElement.csv
	rm -f output.sh
	rm -rf __pycache__/
	rm -rf thrift/
