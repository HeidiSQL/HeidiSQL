SELECT t1.*
FROM table1 t1
LEFT JOIN table2 AS t2 ON t1.id = t2.relid
WHERE
	t2.relid IS NULL