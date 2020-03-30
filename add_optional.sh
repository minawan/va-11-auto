#!/usr/bin/env bash

csvsql --query "

SELECT
	name,
	flavor,
	kind,
	trait,
	price,
	adelhyde,
	bronson_extract,
	powdered_delta,
	flanergide,
	ABS(karmotrine) AS karmotrine,
	add_ice,
	age,
	wait
FROM DrinkRecipe;

" DrinkRecipe.csv | csvlook
