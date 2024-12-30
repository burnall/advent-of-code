with Locs as (
	select row_number() over(order by timestamp) as i, timestamp, coordinate
	from sleigh_locations)

select sum(EXTRACT(EPOCH FROM (b.timestamp - a.timestamp))/3600), c.place_name
from Locs a
inner join Locs b on a.i + 1 = b.i
inner join areas c on st_intersects(c.polygon::geometry, a.coordinate::geometry)
group by c.place_name;