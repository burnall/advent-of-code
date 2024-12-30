 with Speeds_By_Exercice as (
        select reindeer_id, exercise_name, avg(speed_record) as speed
        from Training_Sessions
        where reindeer_id != 9
        group by reindeer_id, exercise_name)
select b.reindeer_name, round(max(speed), 2) as top_speed
from Speeds_By_Exercice a
inner join Reindeers b on a.reindeer_id = b.reindeer_id
group by b.reindeer_name
order by 2 desc;