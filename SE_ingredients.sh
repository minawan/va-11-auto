#!/usr/bin/env bash

csvsql --query "

SELECT *
FROM ScreenElement
WHERE category = 'Ingredient'
ORDER BY name;

" ScreenElement.csv | csvlook
