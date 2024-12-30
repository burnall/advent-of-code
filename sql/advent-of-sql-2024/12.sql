select b.gift_id, b.gift_name, count(*),
	percent_rank() over (order by count(*))
from gift_requests a
inner join gifts b on a.gift_id = b.gift_id
group by b.gift_id, b.gift_name
order by 3 desc, 2
limit 20;


select a, percent_rank() over (order by a) from 
(select 1 a union all select 2 union all select 3 union all select 3);