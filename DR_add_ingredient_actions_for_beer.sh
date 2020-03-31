#!/usr/bin/env bash

csvsql --query "

WITH BeerRow AS (
	SELECT *
	FROM DrinkRecipe
	WHERE name = 'BEER'
), AdelhydeCount(n) AS (
	SELECT 1
	UNION ALL
	SELECT n + 1
	FROM AdelhydeCount, BeerRow
	WHERE n < BeerRow.adelhyde
), BronsonExtractCount(n) AS (
	SELECT 1
	UNION ALL
	SELECT n + 1
	FROM BronsonExtractCount, BeerRow
	WHERE n < BeerRow.bronson_extract
), PowderedDeltaCount(n) AS (
	SELECT 1
	UNION ALL
	SELECT n + 1
	FROM PowderedDeltaCount, BeerRow
	WHERE n < BeerRow.powdered_delta
), FlanergideCount(n) AS (
	SELECT 1
	UNION ALL
	SELECT n + 1
	FROM FlanergideCount, BeerRow
	WHERE n < BeerRow.flanergide
), KarmotrineCount(n) AS (
	SELECT 1
	UNION ALL
	SELECT n + 1
	FROM KarmotrineCount, BeerRow
	WHERE n < BeerRow.karmotrine
)
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
;

" DrinkRecipe.csv | csvlook
