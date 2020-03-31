#!/usr/bin/env bash

DRINK=Beer

csvsql --query "

WITH ${DRINK}Row AS (
	SELECT *
	FROM DrinkRecipe
	WHERE name = UPPER('${DRINK}')
), AdelhydeCount(n) AS (
	SELECT 1
	UNION ALL
	SELECT n + 1
	FROM AdelhydeCount, ${DRINK}Row
	WHERE n < ${DRINK}Row.adelhyde
), BronsonExtractCount(n) AS (
	SELECT 1
	UNION ALL
	SELECT n + 1
	FROM BronsonExtractCount, ${DRINK}Row
	WHERE n < ${DRINK}Row.bronson_extract
), PowderedDeltaCount(n) AS (
	SELECT 1
	UNION ALL
	SELECT n + 1
	FROM PowderedDeltaCount, ${DRINK}Row
	WHERE n < ${DRINK}Row.powdered_delta
), FlanergideCount(n) AS (
	SELECT 1
	UNION ALL
	SELECT n + 1
	FROM FlanergideCount, ${DRINK}Row
	WHERE n < ${DRINK}Row.flanergide
), KarmotrineCount(n) AS (
	SELECT 1
	UNION ALL
	SELECT n + 1
	FROM KarmotrineCount, ${DRINK}Row
	WHERE n < ${DRINK}Row.karmotrine
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
