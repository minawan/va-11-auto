#!/usr/bin/env bash

csvsql --query "

WITH RefinedInput AS (
	SELECT
		drink_name,
		reset,
		slot,
		double,
		CASE
			WHEN drink_name = 'CREVICE_SPIKE' AND double THEN 1
			ELSE add_opt
		END AS add_opt,
		serve,
		use_shortcut
	FROM Input
), ResetAction AS (
	SELECT 'RESET' AS action
	FROM RefinedInput
	WHERE reset
), DrinkRow AS (
	SELECT
		name,
		adelhyde,
		bronson_extract,
		powdered_delta,
		flanergide,
		CASE
			WHEN add_opt AND karmotrine = -1 THEN 1
			WHEN karmotrine = -1 THEN 0
			ELSE karmotrine
		END AS karmotrine,
		add_ice,
		age,
		wait
	FROM RefinedInput
	JOIN DrinkRecipe
	ON drink_name = name
), AdelhydeCount(n) AS (
	SELECT 1
	FROM DrinkRow
	WHERE DrinkRow.adelhyde != 0
	UNION ALL
	SELECT n + 1
	FROM AdelhydeCount, DrinkRow
	WHERE n < DrinkRow.adelhyde
), BronsonExtractCount(n) AS (
	SELECT 1
	FROM DrinkRow
	WHERE DrinkRow.bronson_extract != 0
	UNION ALL
	SELECT n + 1
	FROM BronsonExtractCount, DrinkRow
	WHERE n < DrinkRow.bronson_extract
), PowderedDeltaCount(n) AS (
	SELECT 1
	FROM DrinkRow
	WHERE DrinkRow.powdered_delta != 0
	UNION ALL
	SELECT n + 1
	FROM PowderedDeltaCount, DrinkRow
	WHERE n < DrinkRow.powdered_delta
), FlanergideCount(n) AS (
	SELECT 1
	FROM DrinkRow
	WHERE DrinkRow.flanergide != 0
	UNION ALL
	SELECT n + 1
	FROM FlanergideCount, DrinkRow
	WHERE n < DrinkRow.flanergide
), KarmotrineCount(n) AS (
	SELECT 1
	FROM DrinkRow
	WHERE DrinkRow.karmotrine != 0
	UNION ALL
	SELECT n + 1
	FROM KarmotrineCount, DrinkRow
	WHERE n < DrinkRow.karmotrine
), IngredientActions AS (
	SELECT action
	FROM AdelhydeCount
	CROSS JOIN (
		SELECT 'ADELHYDE' AS action
	)
	UNION ALL
	SELECT action
	FROM BronsonExtractCount
	CROSS JOIN (
		SELECT 'BRONSON_EXTRACT' AS action
	)
	UNION ALL
	SELECT action
	FROM PowderedDeltaCount
	CROSS JOIN (
		SELECT 'POWDERED_DELTA' AS action
	)
	UNION ALL
	SELECT action
	FROM FlanergideCount
	CROSS JOIN (
		SELECT 'FLANERGIDE' AS action
	)
	UNION ALL
	SELECT action
	FROM KarmotrineCount
	CROSS JOIN (
		SELECT 'KARMOTRINE' AS action
	)
), RecipeActions AS (
	SELECT action
	FROM ResetAction
	UNION ALL
	SELECT action
	FROM IngredientActions
)
SELECT action, xCoord, yCoord, shortcut
FROM RecipeActions
LEFT JOIN
ScreenElement
ON action = name 
;

" Input.csv DrinkRecipe.csv ScreenElement.csv | csvlook
