select
    customer_id,
    first_name,
    last_name,
    email,
    current_timestamp as _loaded_at
from {{ ref('stg_customers') }}
