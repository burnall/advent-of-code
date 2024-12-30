with s as (
	select regexp_split_to_array(skills, ',') skills
	from elves)
select count(*)
from s
where skills @> array['SQL'];