select a.name, b.name, b.price
from children a 
inner join gifts b on a.child_id = b.child_id
where price > (select avg(price) as average_price from gifts)
order by b.price asc
limit 10;

