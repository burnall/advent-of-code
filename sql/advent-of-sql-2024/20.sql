INSERT INTO web_requests (url) VALUES
('http://example.com/page?param1=value1¶m2=value2'),
('https://shop.example.com/items?item=toy&color=red&size=small&ref=google&utm_source=advent-of-sql'),



select regexp_split_to_array('123=33=77', '=');


with params as (
	select request_id, url, regexp_split_to_array(substring(url, position('?' in url) +  1), '&') ps 
	from web_requests)
select request_id, array_length(ps, 1), url, ps
from params
where ps @> array['utm_source=advent-of-sql']
order by 2 desc, url
limit 10;


select regexp_split_to_array('item=toy&color=red&size=small&ref=google&utm_source=advent-of-sql', '&')


select array['utm_source=et-ut','utm_source=advent-of-sql','utm_source=advent-of-sql','utm_source=advent-of-sql'] @> array['utm_source=advent-of-sql'];
