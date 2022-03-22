CREATE SCHEMA `week4_alcohol_adolescents` DEFAULT CHARACTER SET utf8 COLLATE utf8_bin ;
/* create a database named*/
USE week4_alcohol_adolescents;
/*USE  database*/
CREATE TABLE `week4_alcohol_adolescents`.`questions` (/*create  table*/
  `questions_id` INT NOT NULL AUTO_INCREMENT,  /*create the int column,no null value and aotu increment*/
  `questions` VARCHAR(255) NULL, /*create the string column, null value*/
  PRIMARY KEY (`questions_id`));/*set primary key*/

CREATE TABLE `week4_alcohol_adolescents`.`answer` (/*create  table*/
  `answer_id` INT NOT NULL AUTO_INCREMENT,  /*create the int column,no null value and aotu increment*/
  `answer` VARCHAR(45) NULL,/*create the string column, null value*/
  PRIMARY KEY (`answer_id`));/*set primary key*/
  
CREATE TABLE `week4_alcohol_adolescents`.`category` (/*create  table*/
  `category_id` INT NOT NULL AUTO_INCREMENT,  /*create the int column,no null value and aotu increment*/
  `Break_Out` VARCHAR(255) NULL,/*create the string column, null value*/
  `Break_Out_Category` VARCHAR(255) NULL,/*create the string column, null value*/
  PRIMARY KEY (`category_id`));/*set primary key*/

CREATE TABLE `week4_alcohol_adolescents`.`statistic` (/*create  table*/
  `statistic_id` INT NOT NULL AUTO_INCREMENT,  /*create the int column,no null value and aotu increment*/
  `Sample_Size` INT NULL,/*create the int column,null value*/
  `Data_value` DECIMAL(10,1) NULL,/*create the decimal column with one decimal,null value*/
  `questionID_fk` INT NULL,/*create the int column,null value*/
  `answerID_fk` INT NULL,/*create the int column,null value*/
  `categoryID_fk` INT NULL,/*create the int column,null value*/
  `zipcodeID_fk` INT NULL,/*create the int column,null value*/
  PRIMARY KEY (`statistic_id`));/*set primary key*/

CREATE TABLE `week4_alcohol_adolescents`.`location` (/*create  table*/
  `City` VARCHAR(255) NULL,/*create the string column, null value*/
  `County` VARCHAR(255) NULL,/*create the string column, null value*/
  `ZipCode` INT NOT NULL,/*create the int column, no null value*/
  PRIMARY KEY (`ZipCode`));/*set primary key*/
  
ALTER TABLE `week4_alcohol_adolescents`.`statistic` /*change table attributes*/
ADD INDEX `questioncol_fk_idx` (`questionID_fk` ASC) VISIBLE,/*Add Index and ascending */
ADD INDEX `answercol_fk_idx` (`answerID_fk` ASC) VISIBLE, /*Add Index and ascending */
ADD INDEX `categorycol_fk_idx` (`categoryID_fk` ASC) VISIBLE; /*Add Index and ascending */
;
ALTER TABLE `week4_alcohol_adolescents`.`category`  /*change table attributes*/
ADD UNIQUE INDEX `Break_Out_UNIQUE` (`Break_Out` ASC, `Break_Out_Category` ASC) VISIBLE; /*Add UNIQUE INDEX  and ascending */
;
ALTER TABLE `week4_alcohol_adolescents`.`statistic`  /*change table attributes*/
ADD CONSTRAINT `questioncol_fk`  /*Add constraints for foreign key*/
  FOREIGN KEY (`questionID_fk`) /*indicate foreign key*/
  REFERENCES `week4_alcohol_adolescents`.`questions` (`questions_id`) /* foreign key align with table  */
  ON DELETE RESTRICT /*Add RESTRICT for foreign key */
  ON UPDATE RESTRICT,/*Add RESTRICT for foreign key */
ADD CONSTRAINT `answercol_fk` /*Add constraints for foreign key*/
  FOREIGN KEY (`answerID_fk`)/*indicate foreign key*/
  REFERENCES `week4_alcohol_adolescents`.`answer` (`answer_id`)/* foreign key align with table  */
  ON DELETE RESTRICT/*Add RESTRICT for foreign key */
  ON UPDATE RESTRICT,/*Add RESTRICT for foreign key */
ADD CONSTRAINT `categorycol_fk`/*Add constraints for foreign key*/
  FOREIGN KEY (`categoryID_fk`)/*indicate foreign key*/
  REFERENCES `week4_alcohol_adolescents`.`category` (`category_id`)/* foreign key align with table  */
  ON DELETE RESTRICT/*Add RESTRICT for foreign key */
  ON UPDATE RESTRICT,/*Add RESTRICT for foreign key */
ADD CONSTRAINT `locationcol_fk`/*Add constraints for foreign key*/
  FOREIGN KEY (`zipcodeID_fk`)/*indicate foreign key*/
  REFERENCES `week4_alcohol_adolescents`.`location` (`ZipCode`)/* foreign key align with table  */
  ON DELETE RESTRICT/*Add RESTRICT for foreign key */
  ON UPDATE RESTRICT;/*Add RESTRICT for foreign key */

INSERT INTO `questions` (questions) /* insert data  */
SELECT DISTINCT `Question` FROM `brfss_ok-1`;/* select different data from table  */
SET sql_safe_updates=OFF;/* close the safe mode*/
UPDATE `brfss_ok-1` a SET questionID_fk = (SELECT questions_id FROM `questions` b WHERE a.Question = b.questions);/* updata data*/

INSERT INTO `answer` (answer) /* insert data  */
SELECT DISTINCT `Response` FROM `brfss_ok-1`;/* select different data from table  */
UPDATE `brfss_ok-1` a SET answerID_fk = (SELECT answer_id FROM `answer` b WHERE `Response` = b.answer);/* updata data*/

INSERT INTO category (Break_Out,Break_Out_Category) /* insert data  */
SELECT DISTINCT Break_Out,Break_Out_Category FROM `brfss_ok-1`;/* select different data from table  */
UPDATE `brfss_ok-1` a SET categoryID_fk = (SELECT category_id FROM category b WHERE a.Break_Out = b.Break_Out AND a.Break_Out_Category=b.Break_Out_Category);/* updata data*/

INSERT INTO statistic /* insert data  */
SELECT NULL,Sample_size,Data_value,questionID_fk,answerID_fk,categoryID_fk,ZipCode FROM `brfss_ok-1`;/* select data from table inclue null  */


SElECT category_id,Break_Out,Break_Out_Category,Data_value,Sample_Size,City,ZipCode /*select attributes*/
FROM category /*select table*/
INNER JOIN statistic /*select table*/
ON category_id=categoryID_fk /*Inner JOIN two tables by category_id*/
INNER JOIN location/*select table*/
ON ZipCode=zipcodeID_fk/*select category_id=5*/
WHERE category_id=5/*select category_id=5*/
ORDER BY Sample_Size DESC; /*  descending data */

SElECT category_id,Break_Out,Break_Out_Category,Data_value,Sample_Size,County,ZipCode /*select attributes*/
FROM category/*select table*/
INNER JOIN statistic/*select table*/
ON category_id=categoryID_fk/*Inner JOIN two tables by category_id*/
INNER JOIN location/*select table*/
ON ZipCode=zipcodeID_fk/*select category_id=5*/
WHERE category_id=5/*select category_id=5*/
ORDER BY Sample_Size DESC;/*  descending data */

/*
DELETE FROM statistic;
ALTER TABLE `week4_alcohol_adolescents`.`statistic` 
CHANGE COLUMN `Data_value` `Data_value` DECIMAL(10,1) NULL DEFAULT NULL ;
*/