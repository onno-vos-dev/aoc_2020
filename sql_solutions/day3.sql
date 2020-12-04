DROP SEQUENCE seq_one; CREATE SEQUENCE seq_one INCREMENT 1 START 1;
DROP SEQUENCE seq_one_double_row; CREATE SEQUENCE seq_one_double_row INCREMENT 1 START 1;
DROP SEQUENCE seq_three; CREATE SEQUENCE seq_three INCREMENT 3 START 1;
DROP SEQUENCE seq_five; CREATE SEQUENCE seq_five INCREMENT 5 START 1;
DROP SEQUENCE seq_seven; CREATE SEQUENCE seq_seven INCREMENT 7 START 1;

DROP TABLE IF EXISTS input;
CREATE TABLE input (row_num int, strings text);
INSERT INTO input SELECT generate_series(1, array_length(ARRAY['..##.......','#...#...#..','.#....#..#.','..#.#...#.#','.#...##..#.','..#.##.....','.#.#.#....#','.#........#','#.##...#...','#...##....#','.#..#...#.#'], 1)),
			 unnest(ARRAY['..##.......','#...#...#..','.#....#..#.','..#.#...#.#','.#...##..#.','..#.##.....','.#.#.#....#','.#........#','#.##...#...','#...##....#','.#..#...#.#']);

WITH add_pos_single_row AS (
	SELECT	*, 
		nextval('seq_one') as one_pos, nextval('seq_three') as three_pos, nextval('seq_five') as five_pos, nextval('seq_seven') as seven_pos
	FROM	input
)

, add_pos_double_row AS (
	SELECT	*, 
		nextval('seq_one_double_row') as pos
	FROM	input
	WHERE 	row_num % 2 <> 0
	AND 	row_num != 1
)

, part_1 as (
	SELECT	*, 
		CASE 
	  	  WHEN three_pos % length(strings) = 0 THEN substring(strings FROM length(strings) for 1) -- 0 means end of string apparently...
		  ELSE substring(strings from three_pos::integer % length(strings) for 1)
		END as maybe_tree
	FROM	add_pos_single_row
)

, part_2_single_rows AS (
	SELECT	*, 
		CASE 
	  	  WHEN one_pos % length(strings) = 0 THEN substring(strings FROM length(strings) for 1) -- 0 means end of string apparently...
		  ELSE substring(strings from one_pos::integer % length(strings) for 1)
		END as maybe_tree_one,
		CASE 
	  	  WHEN three_pos % length(strings) = 0 THEN substring(strings FROM length(strings) for 1) -- 0 means end of string apparently...
		  ELSE substring(strings from three_pos::integer % length(strings) for 1)
		END as maybe_tree_three,
		CASE 
	  	  WHEN five_pos % length(strings) = 0 THEN substring(strings FROM length(strings) for 1) -- 0 means end of string apparently...
		  ELSE substring(strings from five_pos::integer % length(strings) for 1)
		END as maybe_tree_five,
		CASE 
	  	  WHEN seven_pos % length(strings) = 0 THEN substring(strings FROM length(strings) for 1) -- 0 means end of string apparently...
		  ELSE substring(strings from seven_pos::integer % length(strings) for 1)
		END as maybe_tree_seven
	FROM	add_pos_single_row
)

, part_2_two_rows AS (
	SELECT *, 
		CASE
	          WHEN pos - 1 % length(strings) = 0 THEN substring(strings FROM length(strings) for 1) -- 0 means end of string apparently...
		  ELSE substring(strings from pos::integer - 1 % length(strings) for 1)
		END as maybe_tree_one
	FROM 	add_pos_double_row
	WHERE 	row_num % 2 <> 0
	AND 	row_num != 1
)

SELECT	'part_1' AS part,
	(SELECT count(maybe_tree) FROM part_1 WHERE maybe_tree = '#') as answer
UNION ALL
SELECT 	'part_2' as part, 
	(SELECT count(maybe_tree_one) FROM part_2_single_rows WHERE maybe_tree_one = '#') *
	  (SELECT count(maybe_tree_three) FROM part_2_single_rows WHERE maybe_tree_three = '#') *
	  (SELECT count(maybe_tree_five) FROM part_2_single_rows WHERE maybe_tree_five = '#') *
	  (SELECT count(maybe_tree_seven) FROM part_2_single_rows WHERE maybe_tree_seven = '#') *
	  (SELECT count(maybe_tree_one) FROM part_2_two_rows WHERE maybe_tree_one = '#') as answer