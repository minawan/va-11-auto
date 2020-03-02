all: build

build: thrift centroid.py

thrift: command.thrift recipe.thrift action.thrift
	mkdir -p thrift
	thrift -r --gen py --gen go:package_prefix=github.com/minawan/va-11-auto/thrift/gen-go/ -o thrift/ command.thrift
	thrift -r --gen py --gen go:package_prefix=github.com/minawan/va-11-auto/thrift/gen-go/ -o thrift/ action.thrift

centroid.py: ScreenElementBoilerplate ScreenElement.csv
	./ScreenElementBoilerplate

DrinkRecipe.json: convert_drink_recipe_to_json.py DrinkRecipe.ods
	python3 convert_drink_recipe_to_json.py

GenerateTableReader: GenerateTableReader.hs
	ghc GenerateTableReader

ScreenElement.csv: ScreenElement.ods
	libreoffice --headless --convert-to csv ScreenElement.ods

ScreenElement.hs: GenerateTableReader ScreenElement.json
	./GenerateTableReader

ScreenElement.o: ScreenElement.hs

ScreenElementBoilerplate: ScreenElement.o ScreenElementBoilerplate.hs
	ghc ScreenElementBoilerplate

clean:
	rm -f DrinkRecipe.csv
	rm -f DrinkRecipe.json
	rm -f GenerateScreenElement
	rm -f GenerateScreenElement.hs
	rm -f GenerateTableReader
	rm -f ScreenElement.csv
	rm -f ScreenElement.hs
	rm -f ScreenElementBoilerplate
	rm -f centroid.py
	rm -f *.dyn_hi
	rm -f *.dyn_o
	rm -f *.hi
	rm -f *.o
	rm -f main.sh
	rm -rf __pycache__/
	rm -rf thrift/
