all: build

build: GenerateDrinkRecipe.hs
	ghc GenerateDrinkRecipe

run: build DrinkRecipe.ods
	python3 convert_drink_recipe_to_json.py
	./GenerateDrinkRecipe

clean:
	rm -f DrinkRecipe.csv DrinkRecipe.json *.hi *.o *.dyn_hi *.dyn_o GenerateDrinkRecipe GenerateTableReader drink.py GenerateScreenElement.hs
