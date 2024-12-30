with Coord as (
	select coordinate
	from sleigh_locations 
	order by timestamp desc 
	limit 1)
select *
from Areas, Coord
where st_intersects(polygon::geometry, coordinate::geometry);