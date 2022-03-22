CREATE SCHEMA `new_schema` DEFAULT CHARACTER SET utf8 COLLATE utf8_bin ;
/* create a database named new_schema in MySQL server, and the table will use utf8 rule to store data*/
USE `new_schema`;
/*USE new_schema database*/
CREATE TABLE `country` ( /*create country table */
  `country_code` varchar(10) NOT NULL, /*create the string 'country_code'column, the length is 10 characters*/
  PRIMARY KEY (`country_code`)); /*set country_code as primary key */

CREATE TABLE `state` ( /*create state table */
  `state_code` varchar(10) NOT NULL, /*create the string 'state_code'column, the length is 10 characters,no null value*/
  `country_code` varchar(10) DEFAULT NULL,/*create the string 'country_code'column, the length is 10 characters, default null value*/
  PRIMARY KEY (`state_code`), /*set state_code as primary key */
  KEY `country_fk_idx` (`country_code`), /*set country_code as foreigner key */
  CONSTRAINT `country_fk` FOREIGN KEY (`country_code`) REFERENCES `country` (`country_code`)); /*set country_code constraints and referrance country_code in country table  */
  
CREATE TABLE `city` ( /*create city table */
  `city_name` varchar(254) NOT NULL, /*create the string 'city_name'column, the length is 254 characters,no null value*/
  `state_code` varchar(10) NOT NULL,/*create the string 'state_code'column, the length is 10 characters,no null value*/
  PRIMARY KEY (`city_name`,`state_code`),/*set state_code,city_name as primary key */
  KEY `stat_fk_idx` (`state_code`),/*set state_code as foreigner key */
  CONSTRAINT `stat_fk` FOREIGN KEY (`state_code`) REFERENCES `state` (`state_code`));/*set state_code constraints and referrance state_code in state table  */
  
CREATE TABLE `airport` ( /*create airport table */
  `iata_code` varchar(10) NOT NULL, /*create the string iata_codecolumn, the length is 10 characters,no null value*/
  `airport_name` varchar(254) DEFAULT NULL, /*create the string 'city_name'column, the length is 254 characters,default null value*/
  `lat` double DEFAULT NULL, /*create the double lat column, default null value*/
  `long` double DEFAULT NULL,/*create the double long column, default null value*/
  `city_name` varchar(254) DEFAULT NULL, /*create the string 'city_name'column, the length is 254 characters,default null value*/
  `state_code` varchar(10) DEFAULT NULL, /*create the string 'state_code'column, the length is 10 characters,default null value*/
  PRIMARY KEY (`iata_code`), /*set iata_code as primary key */
  KEY `city_fk_idx` (`city_name`,`state_code`), /*set state_code,city_name as foreigner key */
  CONSTRAINT `city_fk` FOREIGN KEY (`city_name`, `state_code`) REFERENCES `city` (`city_name`, `state_code`)); /*set state_code,city_name constraints and referrance state_code in city table  */
  
CREATE TABLE `statistics` ( /*create statistics table */
  `id` int(11) NOT NULL AUTO_INCREMENT, /*create the int id, NOT NULL value, AUTO_INCREMENT*/
  `iata_code` varchar(10) DEFAULT NULL, /*create the string 'iata_code'column, the length is 10 characters, default null value*/
  `cnt` int(11) DEFAULT NULL, /*create the int 'cnt'column,default null value*/
  PRIMARY KEY (`id`), /*set id as primary key */
  KEY `airport_fk_idx` (`iata_code`), /*set siata_code as foreigner key */
  CONSTRAINT `airport_fk` FOREIGN KEY (`iata_code`) REFERENCES `airport` (`iata_code`));/*set iata_code,city_name constraints and referrance state_code in airport table  */

SELECT a.iata_code,airport_name, lat,`long` AS longitude, a.city_name, cnt /*select attributes, change name*/
FROM city AS c /*select table, change name*/
   LEFT JOIN /*LEFT JOIN table*/
   airport AS a /*change name*/
   ON a.city_name = c.city_name /* LEFT JOIN table by city_name*/
   LEFT JOIN/*LEFT JOIN table*/
   statistics AS s /*change name*/
   ON a.iata_code = s.iata_code /* LEFT JOIN table by  iata_code*/
   WHERE c.city_name = 'New York';/* select vaue equal 'New York'*/

SELECT airport_name, a.city_name, st.state_code AS state_name /*select attributes, change name*/
FROM city AS c /*select table, change name*/
   INNER JOIN /*Inner JOIN table*/
   airport AS a /*change name*/
   ON a.city_name = c.city_name /*Inner JOIN table by city_name*/
   INNER JOIN
   state AS st /*change name*/
   ON a.state_code = st.state_code /*Inner JOIN table by state_code*/
   WHERE st.state_code = 'LA'; /* select vaue equal 'LA'*/
   
SELECT airport_name,lat AS latitude,`long` AS longitude, a.city_name, a.state_code AS state_name
FROM airport AS a /*select table, change name*/
   RIGHT OUTER JOIN
   state AS st/*change name*/
   ON a.state_code = st.state_code /*Inner JOIN table by state_code*/
   RIGHT OUTER JOIN
    city AS c/*change name*/
   ON a.city_name = c.city_name /*RIGHT OUTER JOIN table by city_name*/
   WHERE a.lat BETWEEN 30 AND 31 /* select vaue BETWEEN 30 AND 31*/
   ORDER BY a.lat; /*  ascending display data baded on lat*/
