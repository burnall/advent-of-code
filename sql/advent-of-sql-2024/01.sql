select a.name, 
    b.wishes->>'first_choice' as primary_wish, 
    b.wishes->>'second_choice' as backup_wish,
    b.wishes->'colors'->0 as favorite_color,
    json_array_length(b.wishes->'colors') as color_count,
	case when difficulty_to_make = 1 then 'Simple Gift'
	     when difficulty_to_make = 2 then 'Moderate Gift'
	     else 'Complex Gift' end as gift_complexity,
	case when category = 'outdoor' then 'Outside Workshop'
	     when category = 'educational' then 'Learning Workshop'
	     else 'General Workshop' end as workshop_assignment
from children a
inner join wish_lists b on a.child_id = b.child_id
inner join toy_catalogue c on b.wishes->>'first_choice' = c.toy_name
order by a.name
limit 5;


select a.name, 
    b.wishes->>'first_choice' as primary_wish, 
    b.wishes->>'second_choice' as backup_wish,
    b.wishes->'colors'->0 as favorite_color,
    json_array_length(b.wishes->'colors') as color_count,
    case when difficulty_to_make = 1 then 'Simple Gift'
         when difficulty_to_make = 2 then 'Moderate Gift'
         else 'Complex Gift' end as gift_complexity,
    case when category = 'outdoor' then 'Outside Workshop'
         when category = 'educational' then 'Learning Workshop'
         else 'General Workshop' end as workshop_assignment    
from children a
inner join (select child_id, max(submitted_date) as submitted_date from wish_lists group by child_id) b0 on a.child_id = b0.child_id
inner join wish_lists b on a.child_id = b.child_id and b.submitted_date = b0.submitted_date
inner join toy_catalogue c on b.wishes->>'first_choice' = c.toy_name
order by a.name
limit 5;

select child_id, count(*)
from  wish_lists
group by child_id
having count(*) > 1


Abagail,Building sets,LEGO blocks,"Blue",1,Complex Gift,Learning Workshop
Abbey,Stuffed animals,Teddy bears,"White",4,Complex Gift,General Workshop
Abbey,Toy trains,Toy trains,"Pink",2,Complex Gift,General Workshop
Abbey,Barbie dolls,Play-Doh,"Purple",1,Moderate Gift,General Workshop
Abbey,Yo-yos,Building blocks,"Blue",5,Simple Gift,General Workshop