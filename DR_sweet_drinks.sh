#!/usr/bin/env bash

csvsql --query "

SELECT name
FROM DrinkRecipe
WHERE flavor = 'Sweet'
ORDER BY name;

" DrinkRecipe.csv | csvlook
