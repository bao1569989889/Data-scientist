SELECT HB.ims_org_id,hospital_name,BC.bed_id,bed_desc,license_beds,census_beds,staffed_beds,
ttl_license_beds,ttl_census_beds,ttl_staffed_beds
FROM Hospital_Database_week3.Hospital_Bed_Fact AS HB
INNER JOIN Bed_Classification AS BC
ON HB.bed_id=BC.bed_id
INNER JOIN Hospital_Database_week3.Hospital_Information AS HI
ON HI.ims_org_id=HB.ims_org_id
WHERE BC.bed_id=5
GROUP BY HB.ims_org_id
ORDER BY ttl_license_beds DESC;

SELECT HB.ims_org_id,hospital_name,
license_beds AS license_beds_ICU ,
census_beds AS census_beds_ICU,
staffed_beds AS staffed_beds_ICU
FROM Hospital_Database_week3.Hospital_Bed_Fact AS HB
INNER JOIN Bed_Classification AS BC
ON HB.bed_id=BC.bed_id
INNER JOIN Hospital_Database_week3.Hospital_Information AS HI
ON HI.ims_org_id=HB.ims_org_id
WHERE BC.bed_id=4
GROUP BY HB.ims_org_id;


SELECT ims_org_id,hospital_name,bed_id,license_beds,census_beds,staffed_beds,ttl_bed_desc,ttl_license_beds,ttl_census_beds /*select attributes, change name*/
FROM Hospital_Database_week3.Hospital_Bed_Fact AS c /*select table, change name*/
   INNER JOIN /*Inner JOIN table*/
   airport AS a /*change name*/
   ON a.city_name = c.city_name /*Inner JOIN table by city_name*/
   INNER JOIN
   state AS st /*change name*/
   ON a.state_code = st.state_code /*Inner JOIN table by state_code*/
   WHERE st.state_code = 'LA'; /* select vaue equal 'LA'*/