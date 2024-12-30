INSERT INTO employees (name, salary, year_end_performance_scores) VALUES
('Alice Johnson', 75000.00, ARRAY[85, 90, 88, 92])

with perf as ( 
	select employee_id, salary, year_end_performance_scores[array_upper(year_end_performance_scores, 1)] score
	from employees)
select sum(salary * case when score > avg_score then 1.15 else 1.0 end)
from perf, (select avg(score) avg_score from perf); 

