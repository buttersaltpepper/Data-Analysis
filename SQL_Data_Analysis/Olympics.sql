DROP TABLE IF EXISTS "athletes";
	CREATE TABLE athletes(
	 	"id" int,
		name VARCHAR(255),
		gender VARCHAR(8),
		age int,
		height int,
		weight int);

DROP TABLE IF EXISTS "summer_games";
	CREATE TABLE summer_games(
	 	sport VARCHAR(255),
		event VARCHAR(255),
		year date,
		athlete_id int,
		country_id int,
		bronze float,
		silver float,
		gold float);

DROP TABLE IF EXISTS "winter_games";
	CREATE TABLE winter_games(
	 	sport VARCHAR(255),
		event VARCHAR(255),
		year date,
		athlete_id int,
		country_id int,
		bronze float,
		silver float,
		gold float);

DROP TABLE IF EXISTS "countries";
	CREATE TABLE countries(
	 	"id" int,
		country VARCHAR(255),
		region varchar(50));

DROP TABLE IF EXISTS "country_stats";
	CREATE TABLE country_stats(
	 	"year" VARCHAR(255),
		country_id int,
		gdp float,
		pop_in_millions VARCHAR(255),
		nobel_prize_winners int);

COPY athletes
	FROM PROGRAM 'curl "http://assets.datacamp.com/production/repositories/3815/datasets/a5c114363d3f60f514a30683969b1b48b7bc0fe8/athletes_updated.csv"' (DELIMITER ',', FORMAT CSV, HEADER);

COPY summer_games
	FROM PROGRAM 'curl "http://assets.datacamp.com/production/repositories/3815/datasets/174bc4db929ab36891538612c6b1e2cdce11a73b/summer_games_updated.csv"' (DELIMITER ',', FORMAT CSV, HEADER);

COPY winter_games
	FROM PROGRAM 'curl "http://assets.datacamp.com/production/repositories/3815/datasets/1aec560f1e9d22956288a19b1f46f2a21dee0a74/winter_games_updated.csv"' (DELIMITER ',', FORMAT CSV, HEADER);

COPY countries
	FROM PROGRAM 'curl "https://assets.datacamp.com/production/repositories/3815/datasets/3ef4cdfd931e29bc3b1e612d518cf825d56a0362/countries_messy.csv"' (DELIMITER ',', FORMAT CSV, HEADER);

COPY country_stats
	FROM PROGRAM 'curl "http://assets.datacamp.com/production/repositories/3815/datasets/b08d09328a1ab49397e671ee196e957f350bc672/country_stats_updated.csv"' (DELIMITER ',', FORMAT CSV, HEADER);
	