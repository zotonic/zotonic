{% extends "page.tpl" %}

{% block below_body %}

{% wire id="contact-form" type="submit" postback={contact} delegate="resource_default_contact" %}
<form id="contact-form" method="post" action="postback">

    <label for="name">Name</label>
    <input type="text" name="name" id="name" />

    <label for="email">E-mail</label>
    <input type="text" name="mail" id="mail" />
    {% validate id="mail" type={email} type={presence} %}

    <label for="message">Message</label>
    <textarea name="message" id="message" cols="60" rows="8"></textarea>
    {% validate id="message" type={presence} %}

    <input type="submit" value="Send" />

</form>

{% endblock %}

