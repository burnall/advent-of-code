select json_array_elements(cleaning_receipts::json)
from SantaRecords
where cleaning_receipts @> '[{"color": "green", "garment": "suit"}]'
limit 3;

order by 

SantaRecords (record_id, record_date, cleaning_receipts) VALUES 
(3, '2024-12-10', '[
    {
        "receipt_id": "R124",
        "garment": "suit",
        "color": "green",
        "cost": 29.99,
        "drop_off": "2024-12-10",
        "pickup": "2024-12-12"
    },
    {
        "receipt_id": "R125",
        "garment": "scarf",
        "color": "green",
        "cost": 10.99,
        "drop_off": "2024-12-10",
        "pickup": "2024-12-12"
    }]')

select receipt ->> 'drop_off'
from (
    select json_array_elements(cleaning_receipts::json) receipt
    from SantaRecords
    where cleaning_receipts @> '[{"color": "green", "garment": "suit"}]')
where receipt ->> 'garment' = 'suit' and receipt ->> 'color' = 'green'
order by cast(receipt ->> 'drop_off' as date) desc;
