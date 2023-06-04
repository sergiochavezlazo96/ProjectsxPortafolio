################################## RESEARCH QUESTIONS

#1. Which continent manage better the pandemic (infection/death rate)
#2. Which are the 5 countries that managed worst the pandemic
#3. How different is the spread of covid infection in countries of South America
#4. How different is the spread of covid deaths in countries of South America
#5. When was the worst day for each country in South America in terms of infections
#6. When was the worst day for each country in South America in terms of deaths
#7. Which continent has more people fully vacinnated
#8. When was the best day for each country of South America in terms of vaccinate more people
#9. When was the worst day for each country of South America in terms of vaccinate more people (between june 30 of 2021 and 2022)
#10. What is the percentage of vaccination in the world
#11. Which continent have a better ratio of vaccination (% of population vaccinated)
#12. Which country of South America has a better ratio of vaccination (% of population vaccinated)

################################## SET UP

CREATE DATABASE IF NOT EXISTS covid2;
USE covid2;

################################## IMPORTING DATA

## I. COVID DEATHS 

# a. Scheme to make importation more efficient
CREATE TABLE coviddeaths_t
(iso_code VARCHAR (250),
continent VARCHAR (250),
location VARCHAR (250),
date DATE,
population INT DEFAULT 0,
total_cases INT DEFAULT 0,
new_cases INT DEFAULT 0, 
new_cases_smoothed INT DEFAULT 0,
total_deaths INT DEFAULT 0,
new_deaths INT DEFAULT 0,
new_deaths_smoothed INT DEFAULT 0,
icu_patients INT DEFAULT 0,
hosp_patients INT DEFAULT 0,
total_tests INT DEFAULT 0,
PRIMARY KEY (location, date));

# b. Importation of data (We need to access a local file)
### Check availability
### show global variables like 'local_infile'; #If OFF is disable
### set global local_infile=true;
### show global variables like 'local_infile'; #Now it should be on
SHOW VARIABLES LIKE "secure_file_priv"; #To know if u are with a working directory activated - If value is empty that mean we can access any local file just using the correct pathway - How to make this possible: https://www.youtube.com/watch?v=QCwuxzSeNEQ
LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Data\\covid\\CovidDeaths_tr.csv' #Double slash is neccesary
INTO TABLE coviddeaths_t
FIELDS TERMINATED BY ','
IGNORE 1 ROWS; #In this project, this procedure requiered to clean the data in R first and adequate the date format

# c. Review
SELECT *
FROM coviddeaths_t;

## II. COVID VACCINATIONS 

# a. Scheme to make importation more efficient
CREATE TABLE covidvaccinations_t
(iso_code VARCHAR (250),
continent VARCHAR (250),
location VARCHAR (250),
date DATE,
total_vaccinations BIGINT,
people_vaccinated BIGINT,
people_fully_vaccinated BIGINT,
new_vaccinations BIGINT,
population_density FLOAT,
median_age FLOAT,
gdp_per_capita FLOAT,
extreme_poverty FLOAT,
cardiovasc_death_rate FLOAT,
diabetes_prevalence FLOAT,
hospital_beds_per_thousand FLOAT,
life_expectancy FLOAT,
human_development_index FLOAT,
PRIMARY KEY (location, date));


# b. Importation of data (We need to access a local file)
LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Data\\covid\\CovidVaccinations_r.csv' #Double slash is neccesary
INTO TABLE covidvaccinations_t
FIELDS TERMINATED BY ','
IGNORE 1 ROWS; 

# c. Review
SELECT *
FROM covidvaccinations_t;

################################## ANALYSIS

#1. Which continent manage better the pandemic (infection/death rate)
#2. Which are the 5 countries that managed worst the pandemic
#3. How different is the spread of covid infection in countries of South America
#4. How different is the spread of covid deaths in countries of South America
#5. When was the worst day for each country in South America in terms of infections
#6. When was the worst day for each country in South America in terms of deaths
#7. Which continent has more people fully vacinnated
#8. When was the best day for each country of South America in terms of vaccinate more people
#9. When was the worst day for each country of South America in terms of vaccinate more people (between june 30 of 2021 and 2022)
#10. What is the percentage of vaccination in the world
#11. Which continent have a better ratio of vaccination (% of population vaccinated)
#12. Which country of South America has a better ratio of vaccination (% of population vaccinated)

#0. Basic exploration (Nulls, counting, distinct values)

SELECT * FROM coviddeaths_t WHERE iso_code is NULL OR continent is NULL or location is NULL;
SELECT * FROM covidvaccinations_t WHERE iso_code is NULL OR continent is NULL or location is NULL;

SELECT COUNT(DISTINCT continent) FROM coviddeaths_t;
SELECT COUNT(DISTINCT continent) FROM covidvaccinations_t; #Why we have an extra continent in vaccinations
SELECT DISTINCT continent FROM covidvaccinations_t;
SELECT * FROM covidvaccinations_t WHERE continent ='""'; #Bad data
DELETE FROM covidvaccinations_t
WHERE continent='""'; #Done :)

SELECT COUNT(DISTINCT location) FROM coviddeaths_t;
SELECT COUNT(DISTINCT location) FROM covidvaccinations_t;

SELECT location, COUNT(DISTINCT date) AS 'N of days registered' FROM coviddeaths_t WHERE new_cases IS NOT NULL GROUP BY location ORDER BY 'N of days registered' DESC ;
SELECT location, COUNT(DISTINCT date) AS 'N of days registered' FROM covidvaccinations_t WHERE new_vaccinations IS NOT NULL GROUP BY location ORDER BY 'N of days registered' DESC ;

#1. Which continent manage better the pandemic (infection/death rate)
CREATE VIEW continent_management AS
SELECT continent,COUNT(DISTINCT location) AS 'Number of countries' ,SUM(population) as population, SUM(total_cases) as total_cases, SUM(total_deaths) as total_deaths, (SUM(total_cases)/SUM(population)*100) as infection_rate100, 
(SUM(total_deaths)/SUM(population)*100000) as death_rate100000, RANK() OVER (ORDER BY (SUM(total_deaths)/SUM(population)*100000) DESC) as ranking
FROM (
SELECT continent, location,MAX(population) as population, MAX(total_cases) as total_cases, MAX(total_deaths) as total_deaths
FROM coviddeaths_t
GROUP BY location) as clean_country #The idea here is to create a previous table and then query from that one :) (it is important to avoid misscalculations given that is longitudinal data and that not all countries have the same number of records). It is important to notice that the subquery table is grouped by location (country) while the final output is grouped by continent
GROUP BY continent
ORDER BY ranking ASC ;

#2. Which are the 5 countries that managed worst the pandemic
CREATE VIEW worst_countriesworst_countries AS
SELECT location, MAX(population) as population, MAX(total_cases) as total_cases, MAX(total_deaths) as total_deaths, 
(MAX(total_deaths)/MAX(population)*1000) as death_rate1000, RANK() OVER (ORDER BY (MAX(total_deaths)/MAX(population)*1000) DESC) as ranking
FROM coviddeaths_t
GROUP BY location
ORDER BY ranking ASC
LIMIT 5;

#3. How different is the spread of covid infection in countries of South America
CREATE VIEW spread_cases_LA AS
SELECT iso_code, continent, location, date, new_cases FROM coviddeaths_t WHERE continent LIKE '%South America%' ORDER BY date;

#4. How different is the spread of covid deaths in countries of South America
CREATE VIEW spread_deaths_LA AS
SELECT iso_code, continent, location, date, new_deaths FROM coviddeaths_t WHERE continent LIKE '%South America%' ORDER BY date;

#5. When was the worst day for each country in South America in terms of infections
CREATE VIEW worst_days_infections AS
SELECT c1.continent, c1.location, c1.date, c1.new_cases, RANK() OVER (ORDER BY c1.new_cases DESC) ranking
FROM coviddeaths_t c1
JOIN (
SELECT c2.location, MAX(c2.new_cases) AS Worst_day
FROM coviddeaths_t c2
WHERE c2.continent LIKE '%South America%'
GROUP BY c2.location) AS Worst_day_table
ON c1.location=Worst_day_table.location
AND c1.new_cases = Worst_day_table.Worst_Day
AND c1.date = (SELECT MIN(date)
FROM coviddeaths_t c3
WHERE c3.location = c1.location
AND c3.new_cases=c1.new_cases)
ORDER BY new_cases DESC;


#6. When was the worst day for each country in South America in terms of deaths
CREATE VIEW worst_days_deaths AS
SELECT c1.continent, c1.location, c1.date, c1.new_deaths, RANK() OVER (ORDER BY c1.new_deaths DESC) ranking
FROM coviddeaths_t c1
JOIN (
SELECT c2.location, MAX(c2.new_deaths) AS Worst_day
FROM coviddeaths_t c2
WHERE c2.continent LIKE '%South America%'
GROUP BY c2.location) AS Worst_day_table
ON c1.location=Worst_day_table.location
AND c1.new_deaths = Worst_day_table.Worst_Day
AND c1.date = (SELECT MIN(date)
FROM coviddeaths_t c3
WHERE c3.location = c1.location
AND c3.new_deaths=c1.new_deaths)
ORDER BY new_deaths DESC;

#7. Which continent has more people fully vacinnated
CREATE VIEW continent_fullvaccinations AS
SELECT continent, SUM(vaccinations_total) people_fully_vaccinated FROM
(SELECT continent, MAX(people_fully_vaccinated) as vaccinations_total
FROM covidvaccinations_t
GROUP BY location) AS t2
GROUP BY continent
ORDER BY people_fully_vaccinated DESC ;

#8. When was the best day for each country of South America in terms of vaccinate more people
#this query selects the location, date, and new vaccinations from the "covidvaccinations_t" table and joins it with a subquery to retrieve the maximum new vaccinations per day for each location in South America. It then filters the results to show only the earliest date with the maximum new vaccinations and orders the results based on the "new_vaccinations" column.
CREATE VIEW LA_bestvaccinationday AS
SELECT tb1.location, tb1.date, tb1.new_vaccinations
FROM covidvaccinations_t tb1
JOIN (
SELECT tb2.location, MAX(tb2.new_vaccinations) AS vaccinations
FROM covidvaccinations_t tb2
WHERE tb2.continent LIKE '%South America%'
GROUP BY tb2.location) AS join1
ON tb1.location=join1.location
AND tb1.new_vaccinations = join1.vaccinations
AND tb1.date = (SELECT MIN(date) FROM covidvaccinations_t tb3
WHERE tb3.location=tb1.location
AND tb3.new_vaccinations = tb1.new_vaccinations)
ORDER BY new_vaccinations DESC;

#9. When was the worst day for each country of South America in terms of vaccinate more people (between june 30 of 2021 and 2022)
CREATE VIEW LA_worstvaccinationday AS
SELECT tb1.location, tb1.date, tb1.new_vaccinations
FROM covidvaccinations_t tb1
JOIN (
SELECT tb2.location, MIN(tb2.new_vaccinations) AS vaccinations
FROM covidvaccinations_t tb2
WHERE tb2.continent LIKE '%South America%' AND
(date BETWEEN '2021-01-30' AND '2022-01-30') AND
new_vaccinations!=0
GROUP BY tb2.location) AS join1
ON tb1.location=join1.location
AND tb1.new_vaccinations = join1.vaccinations
AND tb1.date = (SELECT MIN(date) FROM covidvaccinations_t tb3 #Min date is key
WHERE tb3.location=tb1.location #A join with where
AND tb3.new_vaccinations = tb1.new_vaccinations)
ORDER BY new_vaccinations DESC;

#10. What is the percentage of vaccination in the world
CREATE VIEW world_vaccination AS
SELECT 'World', SUM(population) as population, SUM(people_vaccinated) as people_vaccinated, ROUND((SUM(people_vaccinated)/SUM(population)*100),2) as Percentage,
RANK() OVER (ORDER BY ROUND((SUM(people_vaccinated)/SUM(population)*100),2) DESC) AS ranking
FROM (SELECT c1.continent, c1.location, c1.date, MAX(population) as population, MAX(people_vaccinated) as people_vaccinated
FROM coviddeaths_t c1
JOIN covidvaccinations_t c2
ON c1.location = c2.location AND
c1.date = c2.date
GROUP BY location) AS tab
ORDER BY Percentage DESC;

#11. Which continent have a better ratio of vaccination (% of population vaccinated)
CREATE VIEW continent_vaccinations AS
SELECT continent, SUM(population) as population, SUM(people_vaccinated) as people_vaccinated, ROUND((SUM(people_vaccinated)/SUM(population)*100),2) as Percentage,
RANK() OVER (ORDER BY ROUND((SUM(people_vaccinated)/SUM(population)*100),2) DESC) AS ranking
FROM (SELECT c1.continent, c1.location, c1.date, MAX(population) as population, MAX(people_vaccinated) as people_vaccinated
FROM coviddeaths_t c1
JOIN covidvaccinations_t c2
ON c1.location = c2.location AND
c1.date = c2.date
GROUP BY location) AS tab
GROUP BY continent
ORDER BY Percentage DESC;

#12. Which country of South America has a better ratio of vaccination (% of population vaccinated)
CREATE VIEW LA_ratiovaccinations AS
SELECT coviddeaths_t.location,MAX(population) as population,
 MAX(people_vaccinated) as people_vaccinated, ROUND((MAX(people_vaccinated)/MAX(population)*100),2) AS Percentage,
RANK() OVER (ORDER BY ROUND((MAX(people_vaccinated)/MAX(population)*100),2) DESC) AS ranking
FROM coviddeaths_t
JOIN covidvaccinations_t
ON coviddeaths_t.location = covidvaccinations_t.location AND
coviddeaths_t.date = covidvaccinations_t.date
WHERE coviddeaths_t.continent LIKE '%South%'  #Where, group by and order by comes after the joins
GROUP BY location
ORDER BY Percentage DESC;

#13. Track the progress of Peru and Chile in terms of ratio of vaccinations per 100 people
CREATE VIEW andinos_progress_ratio AS
SELECT coviddeaths_t.location, coviddeaths_t.date, population, new_vaccinations,
SUM(new_vaccinations) OVER (PARTITION BY covidvaccinations_t.location ORDER BY covidvaccinations_t.location, covidvaccinations_t.date) dosis_available,
ROUND((SUM(new_vaccinations) OVER (PARTITION BY coviddeaths_t.location ORDER BY coviddeaths_t.location, coviddeaths_t.date) /population*100),2) AS available_dosisper100
FROM coviddeaths_t
JOIN covidvaccinations_t
ON coviddeaths_t.date = covidvaccinations_t.date AND
coviddeaths_t.location = covidvaccinations_t.location
WHERE coviddeaths_t.location in ('Peru', 'Bolivia','Ecuador')
ORDER BY coviddeaths_t.location DESC, coviddeaths_t.date ASC;

#CTE FORMAT

WITH PRUEBA (Location, Date, Population, new_vaccinations, dosis_available, available_dosis)
AS (
SELECT coviddeaths_t.location, coviddeaths_t.date, population, new_vaccinations,
SUM(new_vaccinations) OVER (PARTITION BY covidvaccinations_t.location ORDER BY covidvaccinations_t.location, covidvaccinations_t.date) dosis_available,
ROUND((SUM(new_vaccinations) OVER (PARTITION BY coviddeaths_t.location ORDER BY coviddeaths_t.location, coviddeaths_t.date) /population*100),2) AS available_dosisper100
FROM coviddeaths_t
JOIN covidvaccinations_t
ON coviddeaths_t.date = covidvaccinations_t.date AND
coviddeaths_t.location = covidvaccinations_t.location
WHERE coviddeaths_t.location in ('Peru', 'Chile')
ORDER BY coviddeaths_t.location DESC, coviddeaths_t.date ASC)
SELECT *
FROM PRUEBA; #We can continue modifying this

#Tables for Tableu Project

#1. Overall
SELECT SUM(new_cases) as total_cases, SUM(new_deaths) as total_deaths, SUM(new_deaths)/SUM(new_cases)*100 as DeathPercentage
FROM coviddeaths_t;
#2. Deaths in each continent
SELECT continent, SUM(new_deaths) AS total_deaths
FROM coviddeaths_t
GROUP BY continent
ORDER BY 2 DESC;
#3. Infection by country
SELECT location, population, MAX(total_cases) as HighestInfection, ROUND(MAX(total_cases)/MAX(population)*100,2) as InfectionRate100, ROUND(MAX(total_deaths)/MAX(population)*1000,2) as DeathRate1000
FROM coviddeaths_t
WHERE total_cases != 0
GROUP BY location, population
ORDER BY 4 DESC;
#4. Countries evolution for vaccinations
SELECT coviddeaths_t.location, coviddeaths_t.date, population, 
SUM(new_vaccinations) OVER (PARTITION BY covidvaccinations_t.location ORDER BY covidvaccinations_t.location, covidvaccinations_t.date) dosis_available,
ROUND((SUM(new_vaccinations) OVER (PARTITION BY coviddeaths_t.location ORDER BY coviddeaths_t.location, coviddeaths_t.date) /population*100),2) AS available_dosisper100
FROM coviddeaths_t
JOIN covidvaccinations_t
ON coviddeaths_t.date = covidvaccinations_t.date AND
coviddeaths_t.location = covidvaccinations_t.location
ORDER BY coviddeaths_t.location DESC, coviddeaths_t.date ASC;
#5. Countries evolution for infections
SELECT location, population, date, MAX(total_cases) as total_infection, population, MAX(total_cases)/population*100 as InfectionPercentage, total_deaths/population*1000 as DeathRate
FROM coviddeaths_t
GROUP BY location, date
ORDER BY location, date;
#6. Countries evolution for infections
SELECT *
FROM coviddeaths_t;























