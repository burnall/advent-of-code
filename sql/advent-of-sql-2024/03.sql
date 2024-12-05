select xpath('/menu/total_count/text()', menu_data), xpath('/menu/total_guests/text()', menu_data)
from christmas_menus;


/polar_celebration/event_administration/participant_metrics/attendance_details/headcount/total_present


select *
from christmas_menus
where (xpath('/polar_celebration/event_administration/participant_metrics/attendance_details/headcount/total_present/text()', menu_data))[1]::text::integer is null and 
(xpath('/christmas_feast/organizational_details/attendance_record/total_guests/text()', menu_data))[1]::text::integer is null and
(xpath('/northpole_database/annual_celebration/event_metadata/dinner_details/guest_registry/total_count/text()', menu_data))[1]::text::integer is null
limit 1;

select xpath('event_administration/culinary_records/menu_analysis/text()', menu_data)
from christmas_menus
where (xpath('/polar_celebration/event_administration/participant_metrics/attendance_details/headcount/total_present/text()', menu_data))[1]::text::integer is not null limit 1;


xpath('event_administration/culinary_records/menu_analysis/text(), menu_data)

/item_performance
          <food_item_id>


select dish_id, count(*) as cnt
from (
select unnest(xpath('/polar_celebration/event_administration/culinary_records/menu_analysis/item_performance/food_item_id/text()', menu_data))::text::integer as dish_id
from christmas_menus
where (xpath('/polar_celebration/event_administration/participant_metrics/attendance_details/headcount/total_present/text()', menu_data))[1]::text::integer > 78
    union all
select unnest(xpath('/christmas_feast/organizational_details/menu_registry/course_details/dish_entry/food_item_id/text()', menu_data))::text::integer
from christmas_menus
where (xpath('/christmas_feast/organizational_details/attendance_record/total_guests/text()', menu_data))[1]::text::integer > 78
    union all
select unnest(xpath('/northpole_database/annual_celebration/event_metadata/menu_items/food_category/food_category/dish/food_item_id/text()', menu_data))::text::integer
from christmas_menus
where (xpath('/northpole_database/annual_celebration/event_metadata/dinner_details/guest_registry/total_count/text()', menu_data))[1]::text::integer > 78)
group by dish_id
order by 2 desc 
limit 5;

