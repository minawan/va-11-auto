#!/usr/bin/env bash

csvsql --query "

SELECT name
FROM DrinkRecipe
WHERE adelhyde + bronson_extract + powdered_delta + flanergide + karmotrine > 10
ORDER BY name;

" DrinkRecipe.csv | csvlook
