with Expertise as (
	select primary_skill,
	    min(years_experience) as min_years,
		max(years_experience) as max_years
	from workshop_elves
	group by primary_skill)

select min(c.elf_id), min(b.elf_id), a.primary_skill
from Expertise a
inner join workshop_elves b on a.primary_skill = b.primary_skill 
	and a.min_years = b.years_experience
inner join workshop_elves c on a.primary_skill = c.primary_skill 
	and a.max_years = c.years_experience
group by a.primary_skill	
order by a.primary_skill;
