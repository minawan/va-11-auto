all: DrinkRecipe.json ScreenElement.json

DrinkRecipe.csv:
	libreoffice --headless --convert-to csv ../$(input_directory)/DrinkRecipe.ods
	mv DrinkRecipe.csv ../$(output_directory)

DrinkRecipe.json: convert_drink_recipe_to_json.py DrinkRecipe.csv
	python3 convert_drink_recipe_to_json.py ../$(output_directory)/DrinkRecipe.csv ../$(output_directory)/DrinkRecipe.json

ScreenElement.csv:
	libreoffice --headless --convert-to csv ../$(input_directory)/ScreenElement.ods
	mv ScreenElement.csv ../$(output_directory)

ScreenElement.json: convert_screen_element_to_json.py ScreenElement.csv
	python3 convert_screen_element_to_json.py ../$(output_directory)/ScreenElement.csv ../$(output_directory)/ScreenElement.json
