/* Export JSON data to a file */
DECLARE @sql varchar(1000)

-- Paging through sample data
DECLARE @Skip INT = 0 ;
DECLARE @Fetch INT = 1000;
DECLARE @LoopCnt INT;
 
--SELECT @LoopCnt = COUNT(*) / @Fetch 
--FROM xxx3;
--print LoopCnt

--166

SET @LoopCnt = 335


WHILE @LoopCnt > 0
BEGIN
  SET @LoopCnt = @LoopCnt - 1;
 


SET @sql =	'bcp   "SELECT CAST ((SELECT * from finzvit.dbo.View_5 order by 1 OFFSET ' + CONVERT(varchar(10), @Skip) + ' ROWS FETCH NEXT ' +  CONVERT(varchar(10), @Fetch) + ' ROWS ONLY FOR JSON PATH) AS VARCHAR(MAX)) AS json" ' +
    'queryout d:\XML\222222222222\www' + CONVERT(varchar(10), @LoopCnt) + '.json ' + 
    '-c -C65001 -S LAPTOP-0915C010\SQLEXPRESS -T'

	print @sql


 EXEC sys.XP_CMDSHELL @sql

-- Adjust the rows to skip
  SET @SKIP = @SKIP + @Fetch;
END