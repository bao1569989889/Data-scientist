CREATE SCHEMA `Hospital_Database_week3` DEFAULT CHARACTER SET utf8 COLLATE utf8_bin ;
/* create a database named*/
USE Hospital_Database_week3;
/*USE  database*/

CREATE TABLE `Hospital_Database_week3`.`Hospital_Bed_Fact` ( /*create  table*/
  `ims_org_id` VARCHAR(45) NOT NULL, /*create the string column,no null value*/
  `bed_id` INT NULL,/*create the integer column,can be null value*/
  `license_beds` INT NULL,/*create the integer column,can be null value*/
  `census_beds` INT NULL,/*create the integer column,can be null value*/
  `staffed_beds` INT NULL,/*create the integer column,can be null value*/
  PRIMARY KEY (`ims_org_id`));/*set primary key*/

CREATE TABLE `Hospital_Database_week3`.`Bed_Classification` (/*create  table*/
  `bed_id` INT NOT NULL, /*create the string column,no null value*/
  `bed_code` VARCHAR(45) NULL, /*create the string column,can be null value*/
  `bed_desc` VARCHAR(255) NULL,/*create the string column,can be null value*/
  PRIMARY KEY (`bed_id`));/*set primary key*/

CREATE TABLE `Hospital_Database_week3`.`Hospital_Information` (/*create  table*/
  `ims_org_id` VARCHAR(45) NOT NULL,/*create the string column,no null value*/
  `hospital_name` VARCHAR(255) NULL,/*create the string column,can be null value*/
  `ttl_license_beds` INT NULL,/*create the integer column,can be null value*/
  `ttl_census_beds` INT NULL,/*create the integer column,can be null value*/
  `ttl_staffed_beds` INT NULL,/*create the integer column,can be null value*/
  `bed_cluster_id` INT NULL,/*create the integer column,can be null value*/
  PRIMARY KEY (`ims_org_id`));/*set primary key*/

ALTER TABLE `Hospital_Database_week3`.`Hospital_Bed_Fact` /*change table attributes*/
ADD INDEX `bed_id_fk_idx` (`bed_id` ASC) VISIBLE; /*Add Index for bed_id and ascending */
;
ALTER TABLE `Hospital_Database_week3`.`Hospital_Bed_Fact` /*change table attributes*/
ADD CONSTRAINT `bed_id_fk` /*Add constraints for foreign key bed_id_fk  */
  FOREIGN KEY (`bed_id`) /*indicate foreign key  bed_id  */
  REFERENCES `Hospital_Database_week3`.`Bed_Classification` (`bed_id`) /* foreign key align with Bed_Classification table  */
  ON DELETE RESTRICT/*Add RESTRICT for foreign key bed_id_fk in delete query  */
  ON UPDATE RESTRICT;/*Add RESTRICT for foreign key bed_id_fk in update query  */

ALTER TABLE `Hospital_Database_week3`.`Hospital_Bed_Fact` /*change table attributes*/
ADD CONSTRAINT `ims_org_id_fk`/*Add constraints for foreign key ims_org_id_fk  */
  FOREIGN KEY (`ims_org_id`)/*indicate foreign key  ims_org_id  */
  REFERENCES `Hospital_Database_week3`.`Hospital_Information` (`ims_org_id`)/* foreign key align with Hospital_Information table  */
  ON DELETE RESTRICT /*Add RESTRICT for foreign key ims_org_id_fk in delete query  */
  ON UPDATE RESTRICT;/*Add RESTRICT for foreign key ims_org_id_fk in update query  */

SELECT HB.ims_org_id,hospital_name, /*select attributes*/
CASE WHEN HB.bed_id=4 THEN  license_beds ELSE 0 END AS license_beds_ICU,/*select bed_id=4 attributes as a new column, change license_beds name as license_beds_ICU,if no exit bed_id=4 return 0*/
CASE WHEN HB.bed_id=15 THEN  license_beds ELSE 0 END AS license_beds_SICU,/*select bed_id=15 attributes as a new column, change license_beds name as license_beds_SICU,if no exit bed_id=15 return 0*/
CASE WHEN HB.bed_id=4 THEN  census_beds ELSE 0 END AS census_beds_ICU,/*select bed_id=4 attributes as a new column, change census_beds name as census_beds_ICU,if no exit bed_id=4 return 0*/
CASE WHEN HB.bed_id=15 THEN  census_beds ELSE 0 END AS census_beds_SICU,/*select bed_id=15 attributes as a new column, change census_beds name as license_beds_SICU,if no exit bed_id=15 return 0*/
CASE WHEN HB.bed_id=4 THEN  staffed_beds ELSE 0 END AS staffed_beds_ICU, /*select bed_id=4 attributes as a new column, change staffed_beds name as staffed_beds_ICU,if no exit bed_id=4 return 0*/
CASE WHEN HB.bed_id=15 THEN  staffed_beds ELSE 0 END AS staffed_beds_SICU/*select bed_id=15 attributes as a new column, change staffed_beds name as staffed_beds_SICU,if no exit bed_id=15 return 0*/
FROM Hospital_Database_week3.Hospital_Bed_Fact AS HB /*select table and change name*/
INNER  JOIN Bed_Classification AS BC /*change name*/
ON HB.bed_id=BC.bed_id /*Inner JOIN two tables by bed_id*/
RIGHT JOIN Hospital_Database_week3.Hospital_Information AS HI /*change name*/
ON HI.ims_org_id=HB.ims_org_id /*RIGHT JOIN two tables by bed_id*/
WHERE BC.bed_id=15 OR BC.bed_id=4 /*select bed_id=4 and 15 attributes*/
GROUP BY HB.ims_org_id /* group data by license_beds */
ORDER BY license_beds DESC; /*  descending data by license_beds */


