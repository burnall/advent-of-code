with Avgs as (
	select field_name, harvest_year, unnest(Array[
		sum(case when season in ('Spring') then trees_harvested else 0 end),
		sum(case when season in ('Spring', 'Summer') then trees_harvested else 0 end)/2.0,
		sum(case when season in ('Spring', 'Summer', 'Fall') then trees_harvested else 0 end)/3.0,
		sum(case when season in ('Summer', 'Fall', 'Winter') then trees_harvested else 0 end)/3.0]) trees
	from TreeHarvests
	group by field_name, harvest_year)
select max(trees) from avgs;

