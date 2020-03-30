all: DrinkRecipe.json ScreenElement.json

DrinkRecipe.json: convert_drink_recipe_to_json.py
	python3 convert_drink_recipe_to_json.py ../$(input_directory)/DrinkRecipe.csv ../$(output_directory)/DrinkRecipe.json

ScreenElement.json: convert_screen_element_to_json.py
	python3 convert_screen_element_to_json.py ../$(input_directory)/ScreenElement.csv ../$(output_directory)/ScreenElement.json
