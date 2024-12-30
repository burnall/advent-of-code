with numbers as 
	(select generate_series as i from generate_series(1, 10000))

select (select string_agg(generate_series::varchar, ',' order by 1) from generate_series(mini, maxi))
from (
	select a.id + 1 as mini, b.id - 1 as maxi
	from sequence_table a
	inner join sequence_table b on a.id + 1 < b.id
	where not exists(select * from sequence_table c where c.id > a.id and c.id < b.id));


select a.id + 1 as mini, b.id - 1 as maxi
from sequence_table a
inner join sequence_table b on a.id + 1 < b.id
where not exists(select * from sequence_table c where c.id > a.id and c.id < b.id);	


with no_rights as (
	select id
	from sequence_table a
	where not exists(select * from sequence_table b where a.id + 1 = b.id)),
	no_lefts as (
	select id
	from sequence_table a
	where not exists(select * from sequence_table b where a.id - 1 = b.id))

select *
from (select id, 1 from no_rights 
	union
	select id, 2 from no_lefts)
order by id	 


