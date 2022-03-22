CREATE SCHEMA `DB1_bao` DEFAULT CHARACTER SET utf8 COLLATE utf8_bin;
/* create a database named DB1_ao in MySQL server, and the table will use utf8 rule to store data*/
CREATE TABLE `DB1_bao`.`Tech_table` (/* create a table under DB1_bao database to store the outsource data */
  `company` VARCHAR(45) NOT NULL,/* create the string 'company'column, the length is 45 characters and cannot include null value */
  `numEmps` INT NULL,/*create the numerical 'numEmps'column and can include null value */
  `category` VARCHAR(45) NULL,/*create the string 'category'column, the length is 45 characters and can include null value */
  `city` VARCHAR(45) NULL,/*create the string 'city'column, the length is 45 characters and can include null value */
  `state` VARCHAR(45) NOT NULL,/*create the string 'state'column, the length is 45 characters and cannot include null value */
  `fundedDate` VARCHAR(45) NOT NULL,/*create the string 'fundedDte'column, the length is 45 characters and cannot include null value */
  `raisedAmt` INT NOT NULL,/* create the numerical 'raisedAmt'column cannot include null value*/
  `raisedCurrency` VARCHAR(45) NOT NULL,/*create the string 'raisedCurrency'column, the length is 45 characters and cannot include null value */
  `round` VARCHAR(45) NULL);/*create the string 'round'column, the length is 45 characters and can include null value */
  
  SELECT * FROM `DB1_bao`.`Tech_table`;/*show all attributes and records in Tech_table*/
  
SELECT round,raisedCurrency, SUM(raisedAmt)AS sum,COUNT(raisedAmt) AS count, MAX(raisedAmt) AS max, 
MIN(raisedAmt) AS min,AVG(raisedAmt) AS avg 
FROM `DB1_bao`.`Tech_table`
/*select round,raisedCurrency attributes from Tech_table and caculate the sum, count, max,min,avg of raisedAmt
, and change the name as sum, count, max,min,avg */
WHERE raisedCurrency='USD'/* only select raisedCurrency is equil to 'USD' dataset  */
GROUP BY (round);/* based on round type to display caculated valued*/

SELECT category,COUNT(company) AS count FROM `DB1_bao`.`Tech_table`
/* select category attributes from Tech_table and change the name as count*/
GROUP BY category/*based on category to display caculated valued */
HAVING COUNT(company)>=1/* only select the number of company is great or equil to 'USD' dataset*/
ORDER BY COUNT(category) DESC;/*  Descending display data baded on the number of category*/

SELECT * FROM `DB1_bao`.`Tech_table`/*  select all attributes from Tech_table*/
 WHERE state IN ('MA')/* only select state dataset that is MA  */
 ORDER BY raisedAmt DESC;/* Descending display data baded on the raisedAmt */
 
/*
SELECT * FROM `DB1_bao`.`Tech_table`
WHERE raisedAmt BETWEEN 45000000 AND 1000000
AND state IN ('MA')
ORDER BY raisedAmt DESC;
*/ /* I don't know why this code cannot show results, the code don't have any mistakes*/

