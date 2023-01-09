{% wire id="signup_form" type="submit" postback={signup xs_props=xs_props} delegate=signup_delegate %}
<form id="signup_form" class="setcookie" method="post" action="postback">
    <input type="hidden" name="page" value="{{ page|default:q.p|escape }}">
    {% include form_fields_tpl %}
</form>

{% if page|default:q.p == '#reload' %}
    {% javascript %}
        const path = window.location.pathname + window.location.search;
        if (!path.match('^/([a-z][a-z]/)?$')) {
            $('#signup_form input[name="page"]').val(path);
        }
    {% endjavascript %}
{% endif %}
