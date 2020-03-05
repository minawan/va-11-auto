.PHONY: all thrift clean

all: build

build: DrinkRecipe.json ScreenElement.csv server

thrift:
	$(MAKE) -C thrift

DrinkRecipe.json: convert_drink_recipe_to_json.py DrinkRecipe.ods
	python3 convert_drink_recipe_to_json.py

ScreenElement.csv: ScreenElement.ods
	libreoffice --headless --convert-to csv ScreenElement.ods

server: thrift
	$(MAKE) -C server

clean:
	rm -f DrinkRecipe.csv
	rm -f DrinkRecipe.json
	rm -f ScreenElement.csv
	rm -f output.sh
	rm -rf __pycache__/
	$(MAKE) clean -C server
	$(MAKE) clean -C thrift
