select a.song_id, b.song_title, count(*) as played,
    sum(case when a.duration is null or a.duration < b.song_duration then 1 else 0 end) skipped
from user_plays a
inner join songs b on a.song_id = b.song_id
group by a.song_id, b.song_title
order by played desc, skipped asc;

