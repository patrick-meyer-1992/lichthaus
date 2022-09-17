DELETE 
FROM bewertet 
WHERE upload_time > '2022-09-17'
RETURNING *;

DELETE 
FROM schlaegt_vor
WHERE upload_time > '2022-09-17'
RETURNING *;

DELETE 
FROM stimmt_fuer
WHERE upload_time > '2022-09-17'
RETURNING *;

DELETE 
FROM film
WHERE upload_time > '2022-09-17'
RETURNING *;