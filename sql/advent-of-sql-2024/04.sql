with tags as (
select toy_id, unnest(previous_tags) ptag, unnest(new_tags) ntag
from toy_production)

select a.toy_id, added, removed, unchanged
from (
	select toy_id, count(*) added
	from tags a
	where a.ntag is not null and not exists(select * from tags b where a.toy_id = b.toy_id and a.ntag = b.ptag)
	group by toy_id) a
left join (
	select toy_id, count(*) removed
	from tags a
	where a.ptag is not null and not exists(select * from tags b where a.toy_id = b.toy_id and a.ptag = b.ntag)
	group by toy_id) b on a.toy_id = b.toy_id
left join (
	select toy_id, count(*) unchanged
	from tags a
	where exists(select * from tags b where a.toy_id = b.toy_id and a.ptag = b.ntag)
	group by toy_id) c on a.toy_id = c.toy_id
order by added desc;




