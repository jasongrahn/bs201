with orders as (
    select * from {{ ref('stg_orders') }}
),
payments as (
    select * from {{ ref('stg_payments') }}
),
order_payments as (
    select
        order_id,
        {% for payment_method in get_payment_methods() %}
        sum(case when payment_method = '{{ payment_method }}' then amount else 0 end) as {{ payment_method }}_amount,
        {% endfor %}
        sum(amount) as total_amount
    from payments
    group by 1
)
select
    orders.order_id,
    orders.customer_id,
    orders.order_date,
    orders.status,
    {% for payment_method in get_payment_methods() %}
    order_payments.{{ payment_method }}_amount,
    {% endfor %}
    order_payments.total_amount
from orders
left join order_payments using (order_id)
