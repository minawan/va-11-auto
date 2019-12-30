all:
	ghc GenerateDrinkRecipe

run: all
	python3 va_11_hall_a.py
	./GenerateDrinkRecipe

clean:
	rm -f VA-11_Cheat_Sheet.csv VA-11_Cheat_Sheet.json GenerateDrinkRecipe.hi GenerateDrinkRecipe.o GenerateDrinkRecipe GenerateDrinkRecipe.dyn_hi GenerateDrinkRecipe.dyn_o drink.py
