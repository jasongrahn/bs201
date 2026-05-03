select 
    id as payment_id
    , order_id
    , payment_method
    , amount / 100.0 as amount -- convert cents to dollars
from {{ ref("raw_payments") }}
where amount is not null 