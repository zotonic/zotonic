{% extends "email_base.tpl" %}

{% block title %}{_ How to reset your password _}{% endblock %}

{% block body %}
{% if not id or not username %}
    <p>{_ Hello _},</p>

    <p>{_ You've requested a new password for _} <a href="https://{{ m.site.hostname }}/">{{ m.site.hostname }}</a>.</p>

    <p>{_ However this email address does not belong to one of our registered users so you will not be able to change the password. _}</p><br/>
    <p> {_ Are you sure you have created an account? If not, you can create one here: _} https://{{ m.site.hostname }}/signup</p><br/>

    <p>{_ If you think you do have an account, please try again using the correct email address. _}</p>
{% else %}
    <p>{_ Hello _} {{ m.rsc[id].name_first|default:m.rsc[id].title }},</p>

    <p>{_ You've requested a new password for _} <a href="https://{{ m.site.hostname }}/">{{ m.site.hostname }}</a>.</p>

    {% with m.identity[id].username as username %}
        <p>{_ Username _}: <strong>{{ username|escape }}</strong>.
    {% if username != email|default:(m.rsc[id].email_raw) %} 
        <br/>{_ Email _}: <strong>{{ email|default:(m.rsc[id].email_raw)|escape }}</strong>.{% endif %}</p>

        {% all include "_logon_extra_email_reset.tpl" identity_types=m.identity[id].all_types %}

        <p>{_ Click on the link to enter a new password or copy and paste the whole link. _}</p>

        <p><a href="{% url logon_reset secret=secret u=username absolute_url %}">{% url logon_reset secret=secret u=username absolute_url %}</a></p>
    {% endwith %}
{% endif %}

<p>{_ If you don't want to reset your password, you can ignore this email. Maybe someone made an error typing his or her email address. _}</p>

{% endblock %}

{% block disclaimer %}
<p style="color: #666; font-size: 80%;">--<br/>
{_ You received this email as you entered your username or email address to reset your password. You will not receive any other emails because of this request. _}</p>
{% endblock %}
