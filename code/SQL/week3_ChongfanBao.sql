CREATE SCHEMA `Hospital_Database` DEFAULT CHARACTER SET utf8 COLLATE utf8_bin ;
USE Hospital_Database;

CREATE TABLE `Hospital_database`.`Hospital_Bed_Fact` (
  `ims_org_id` VARCHAR(45) NOT NULL,
  `bed_id` INT NULL,
  `number_id` INT NULL,
  PRIMARY KEY (`ims_org_id`));

CREATE TABLE `Hospital_database`.`Bed_Classification` (
  `bed_id` INT NOT NULL,
  `bed_code` VARCHAR(45) NULL,
  `bed_desc` VARCHAR(255) NULL,
  PRIMARY KEY (`bed_id`));

CREATE TABLE `Hospital_database`.`Hospital_Information` (
  `ims_org_id` VARCHAR(45) NOT NULL,
  `business_name` VARCHAR(255) NULL,
  `ttl_license_beds` INT NULL,
  `ttl_census_beds` INT NULL,
  `ttl_staffed_beds` INT NULL,
  `bed_cluster_id` VARCHAR(45) NULL,
  PRIMARY KEY (`ims_org_id`));

CREATE TABLE `Hospital_database`.`Number_of_Bed` (
  `number_id` INT NOT NULL AUTO_INCREMENT,
  `license_beds` INT NULL,
  `census_beds` INT NULL,
  `Number_of_Bedcol` INT NULL,
  PRIMARY KEY (`number_id`));
