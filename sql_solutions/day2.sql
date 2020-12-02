DROP TABLE IF EXISTS input;
CREATE TABLE input (strings text);
INSERT INTO input SELECT unnest(ARRAY['1-3 a: abcde','1-3 b: cdefg','2-9 c: ccccccccc']);

WITH initial_split as (
	SELECT	strings, regexp_split_to_array(strings, E' ') as initial_split_array
	FROM	input
)

, split as (
	SELECT 	strings, regexp_split_to_array(initial_split_array[1], E'-') as pos_array, trim(trailing ':' from initial_split_array[2]) as char, initial_split_array[3] as password
	FROM	initial_split
)

, part_1 as (
	SELECT	CASE
		  WHEN length(password) - length(regexp_replace(password, char, '', 'g')) >= pos_array[1]::integer and length(password) - length(regexp_replace(password, char, '', 'g')) <= pos_array[2]::integer THEN 'match'
		  ELSE 'not_match'
		END as result
	FROM	split
)

, part_2 as (
	SELECT 	CASE
		  WHEN (substring(password from pos_array[1]::integer for 1) = char or substring(password from pos_array[2]::integer for 1) = char) and not substring(password from pos_array[1]::integer for 1) = substring(password from pos_array[2]::integer for 1)
		    THEN 'match'
		  ELSE
		    'no_match'
		END as result
	FROM 	split
)

SELECT 	'part1' as part, count(result) as res
FROM 	part_1
WHERE	result = 'match'
GROUP BY 1
UNION
SELECT 	'part2' as part, count(result) as res
FROM 	part_2
WHERE	result = 'match'
GROUP BY 1
