all: build

build: GenerateDrinkRecipe.hs
	ghc GenerateDrinkRecipe

run: build DrinkRecipe.ods
	python3 convert_drink_recipe_to_json.py
	./GenerateDrinkRecipe

clean:
	rm -f DrinkRecipe.csv DrinkRecipe.json GenerateDrinkRecipe.hi GenerateDrinkRecipe.o GenerateDrinkRecipe GenerateDrinkRecipe.dyn_hi GenerateDrinkRecipe.dyn_o drink.py
