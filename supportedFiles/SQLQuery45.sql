select EDRPOU, UOname, OKPO, UOfeature into UO from (select EDRPOU, UOname, OKPO, UOfeature from ( select QQ.[RECORD], EDRPOU, UOname, OKPO, UOfeature from (SELECT RECORD, EDRPOU, BENEFICIARY, LTRIM(LEFT([BENEFICIARY], CHARINDEX(';', [BENEFICIARY]) - 1)) AS UOname, '' AS OKPO, 'Бенефіціар' AS UOfeature
FROM     UOBeneficiares
UNION ALL
SELECT UOfounders.RECORD, UOfounders.EDRPOU, [FOUNDER], IIf([FOUNDER] LIKE '%,%', LTRIM(LEFT([FOUNDER], CHARINDEX(',', [FOUNDER]) - 1)), [FOUNDER]) AS UOname, iif(isnumeric(substring([FOUNDER], CHARINDEX(',', 
                  IIf([FOUNDER] LIKE '%,%', LTRIM(LEFT([FOUNDER], CHARINDEX(',', [FOUNDER]))), [FOUNDER])) + 2, 8)) = 1 AND substring([FOUNDER], CHARINDEX(',', IIf([FOUNDER] LIKE '%,%', LTRIM(LEFT([FOUNDER], CHARINDEX(',', [FOUNDER]))), 
                  [FOUNDER])) + 2, 8) NOT LIKE '% %', substring([FOUNDER], CHARINDEX(',', IIf([FOUNDER] LIKE '%,%', LTRIM(LEFT([FOUNDER], CHARINDEX(',', [FOUNDER]))), [FOUNDER])) + 2, 8), '') AS OKPO, 
                  format(IIf([FOUNDER] LIKE '%розмір внеску до статутного фонду%', CAST(iif(ISNUMERIC(Replace(Replace(RIGHT([FOUNDER], CHARINDEX('-', REVERSE([FOUNDER])) - 1), 'грн.', ''), ',', '.')) = 1, Replace(Replace(RIGHT([FOUNDER], 
                  CHARINDEX('-', REVERSE([FOUNDER])) - 1), 'грн.', ''), ',', '.'), '0.00') AS FLOAT), '0.00') / REPLACE(IIf([AUTHORIZED_CAPITAL] = '0,00' OR
                  [AUTHORIZED_CAPITAL] = '', '100.00', [AUTHORIZED_CAPITAL]), ',', '.'), 'P') AS UOfeature
FROM     UOfounders INNER JOIN
                  UOinfo ON UOfounders.RECORD = UOinfo.RECORD) as QQ inner join (
				  
				  select [RECORD] from (SELECT [RECORD]
      ,[EDRPOU]
      ,[REGISTRATION]
  FROM [UOdata].[dbo].[UOinfo]) AS Q inner join (SELECT EDRPOU
      ,max([REGISTRATION]) as [REGISTRATION]

  FROM [UOdata].[dbo].[UOinfo]
  where STAN = 'зареєстровано'
  group by  EDRPOU) AS QQ on Q.[EDRPOU] = QQ.[EDRPOU] and Q.REGISTRATION = QQ.REGISTRATION
				  
				  ) AS QQQ on QQ.RECORD = QQQ.RECORD) AS QQQQQ) AS QQQQQQQQQQQQQQ inner join (select TIN FROM [finzvit].[dbo].[xxx5]) AS QQQQQ on QQQQQQQQQQQQQQ.EDRPOU = QQQQQ.TIN
  