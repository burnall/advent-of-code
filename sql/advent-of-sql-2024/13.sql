select substring(email, position('@' in email) + 1), count(*)
from (
	select unnest(email_addresses) email
	from contact_list)
group by substring(email, position('@' in email) + 1)
order by 2 desc;