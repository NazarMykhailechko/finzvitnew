SELECT CAST
                      ((SELECT 
    TIN = [TIN]
    ,child =JSON_QUERY((
           SELECT 
                [ROW] = [ROW],
				[SUMM] = [SUMM]

           FROM (select * from [finzvit].[dbo].[xxxxx]) AS c
           WHERE c.[TIN] = p.[TIN]
           FOR JSON PATH
       ))
FROM 
    (SELECT distinct [TIN] FROM [finzvit].[dbo].[xxxxx]) AS p
FOR JSON PATH) AS VARCHAR(MAX)) AS json