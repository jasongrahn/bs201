{% macro get_payment_methods() %}
    {{ return(['credit_card', 'coupon', 'bank_transfer', 'gift_card']) }}
{% endmacro %}
