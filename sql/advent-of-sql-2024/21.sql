with stat as (
	select extract(year from sale_date) as year, 
		floor((2 + extract(month from sale_date)) / 3) quarter, 
		sum(amount) amount,
		row_number() over() i
	from sales
	group by extract(year from sale_date), floor((2 + extract(month from sale_date)) / 3)
	order by 1, 2)

	select a.year, a.quarter, (a.amount - b.amount) / b.amount * 100.0
	from stat a
	inner join stat b on a.i = b.i + 1
	order by 3 desc;