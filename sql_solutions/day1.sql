DROP TABLE IF EXISTS input;
CREATE TABLE input (int int8);
INSERT INTO input SELECT unnest(ARRAY[1721,979,366,299,675,1456]);

WITH part_1 AS (
	SELECT 	distinct ON (i1.int * i2.int) i1.int * i2.int as answer
	FROM 	input i1
	CROSS JOIN input i2
	WHERE i1.int + i2.int = 2020
)

, part_2 AS (
	SELECT 	distinct ON (i1.int * i2.int * i3.int) i1.int * i2.int * i3.int as answer
	FROM 	input i1
	CROSS JOIN input i2
	CROSS JOIN input i3
	WHERE i1.int + i2.int + i3.int = 2020
)

SELECT 	'part1' as part, *
FROM 	part_1
UNION
SELECT  'part2' as part, *
FROM 	part_2
