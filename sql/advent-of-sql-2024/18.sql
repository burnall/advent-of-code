with recursive paths(staff_id, path, level) as (
	select staff_id, cast(staff_id as varchar), 1 
	from staff
	where manager_id is null
	    union all
	select b.staff_id, a.path || ',' || b.staff_id, a.level + 1
	from paths a
	inner join staff b on a.staff_id = b.manager_id	
)
select min(staff_id), level, count(*) cnt
from paths
group by level
order by cnt desc;