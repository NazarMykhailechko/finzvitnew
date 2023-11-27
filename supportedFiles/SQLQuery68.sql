SELECT CAST
                      ((SELECT 
    TIN = [TIN]
    ,child =JSON_QUERY((
           SELECT 
                [UOname] = [UOname],
				[OKPO] = [OKPO],
				[UOfeature] = [UOfeature]

           FROM (select * FROM [UOdata].[dbo].[UO]) AS c
           WHERE c.[EDRPOU] = p.[TIN]
           FOR JSON PATH
       ))
FROM 
    (Select [TIN]
      
        FROM [finzvit].[dbo].[xxx5] inner join (SELECT distinct [EDRPOU]

  FROM [UOdata].[dbo].[UO]
) AS Q on [TIN] = [EDRPOU] where [TIN] in('43372592', '43954883')) AS p

FOR JSON PATH) AS VARCHAR(MAX)) AS json