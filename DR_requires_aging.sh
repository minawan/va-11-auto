#!/usr/bin/env bash

csvsql --query "

SELECT *
FROM DrinkRecipe
WHERE age
ORDER BY name;

" DrinkRecipe.csv | csvlook
