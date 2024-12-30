with drink_by_date as (
	select date, 
	    sum(case when drink_name = 'Eggnog' then quantity else 0 end) eggnog,
		sum(case when drink_name = 'Hot Cocoa' then quantity else 0 end) cocoa,	
		sum(case when drink_name = 'Peppermint Schnapps' then quantity else 0 end) schnapps
		from Drinks
	group by date)
select *
from drink_by_date
where eggnog = 198 and cocoa = 38 and schnapps = 298; 


HotCocoa: 38
PeppermintSchnapps: 298
Eggnog: 198