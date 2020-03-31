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
), SelectSlotAction AS (
	SELECT slot AS action
	FROM RefinedInput
), BaseDrinkRecipe AS (
	SELECT
		adelhyde,
		bronson_extract,
		powdered_delta,
		flanergide,
		karmotrine,
		add_ice,
		age,
		wait,
		CASE
			WHEN adelhyde + bronson_extract + powdered_delta + flanergide + karmotrine <= 10 THEN double
			ELSE 0
		END AS double,
		add_opt
	FROM RefinedInput
	JOIN DrinkRecipe
	ON drink_name = name
), BigDrinkRecipe AS (
	SELECT
		CASE
			WHEN double THEN 2 * adelhyde
			ELSE adelhyde
		END AS adelhyde,
		CASE
			WHEN double THEN 2 * bronson_extract
			ELSE bronson_extract
		END AS bronson_extract,
		CASE
			WHEN double THEN 2 * powdered_delta
			ELSE powdered_delta
		END AS powdered_delta,
		CASE
			WHEN double THEN 2 * flanergide
			ELSE flanergide
		END AS flanergide,
		CASE
			WHEN double AND karmotrine != -1 THEN 2 * karmotrine
			ELSE karmotrine
		END AS karmotrine,
		add_ice,
		age,
		wait,
		add_opt
	FROM BaseDrinkRecipe
), RefinedDrinkRecipe AS (
	SELECT
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
	FROM BigDrinkRecipe
), AdelhydeCount(n) AS (
	SELECT 1
	FROM RefinedDrinkRecipe
	WHERE RefinedDrinkRecipe.adelhyde != 0
	UNION ALL
	SELECT n + 1
	FROM AdelhydeCount, RefinedDrinkRecipe
	WHERE n < RefinedDrinkRecipe.adelhyde
), BronsonExtractCount(n) AS (
	SELECT 1
	FROM RefinedDrinkRecipe
	WHERE RefinedDrinkRecipe.bronson_extract != 0
	UNION ALL
	SELECT n + 1
	FROM BronsonExtractCount, RefinedDrinkRecipe
	WHERE n < RefinedDrinkRecipe.bronson_extract
), PowderedDeltaCount(n) AS (
	SELECT 1
	FROM RefinedDrinkRecipe
	WHERE RefinedDrinkRecipe.powdered_delta != 0
	UNION ALL
	SELECT n + 1
	FROM PowderedDeltaCount, RefinedDrinkRecipe
	WHERE n < RefinedDrinkRecipe.powdered_delta
), FlanergideCount(n) AS (
	SELECT 1
	FROM RefinedDrinkRecipe
	WHERE RefinedDrinkRecipe.flanergide != 0
	UNION ALL
	SELECT n + 1
	FROM FlanergideCount, RefinedDrinkRecipe
	WHERE n < RefinedDrinkRecipe.flanergide
), KarmotrineCount(n) AS (
	SELECT 1
	FROM RefinedDrinkRecipe
	WHERE RefinedDrinkRecipe.karmotrine != 0
	UNION ALL
	SELECT n + 1
	FROM KarmotrineCount, RefinedDrinkRecipe
	WHERE n < RefinedDrinkRecipe.karmotrine
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
), AddIceAction AS (
	SELECT 'ADD_ICE' AS action
	FROM RefinedDrinkRecipe
	WHERE add_ice
), AgeAction AS (
	SELECT 'AGE' AS action
	FROM RefinedDrinkRecipe
	WHERE age
), MixAction AS (
	SELECT 'MIX' AS action
	UNION ALL
	SELECT
		CASE
			WHEN wait THEN 'LONG_WAIT'
			ELSE 'SHORT_WAIT'
		END AS action
	FROM RefinedDrinkRecipe
	UNION ALL
	SELECT 'MIX' AS action
), ServeAction AS (
	SELECT 'MIX' AS action
	FROM RefinedInput
	WHERE serve
), RecipeActions AS (
	SELECT
		action AS src,
		action AS dst
	FROM ResetAction
	UNION ALL
	SELECT
		action AS src,
		action AS dst
	FROM SelectSlotAction
	UNION ALL
	SELECT
		action AS src,
		'BLENDER' AS dst
	FROM IngredientActions
	UNION ALL
	SELECT
		action AS src,
		action AS dst
	FROM AddIceAction
	UNION ALL
	SELECT
		action AS src,
		action AS dst
	FROM AgeAction
	UNION ALL
	SELECT
		action AS src,
		action AS dst
	FROM MixAction
	UNION ALL
	SELECT
		action AS src,
		action AS dst
	FROM ServeAction
)
SELECT
	src AS action,
	Source.xCoord AS src_x,
	Source.yCoord AS src_y,
	Destination.xCoord AS dst_x,
	Destination.yCoord AS dst_y,
	Source.shortcut
FROM RecipeActions
LEFT JOIN
ScreenElement Source
ON src = Source.name
LEFT JOIN
ScreenElement Destination
ON dst = Destination.name
;

" Input.csv DrinkRecipe.csv ScreenElement.csv | csvlook
