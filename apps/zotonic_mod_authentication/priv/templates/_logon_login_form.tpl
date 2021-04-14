<form id="logon_form" class="z_logon_form {% if is_show_passcode %}z-logon-passcode{% endif %}" method="post" action="#" target="logonTarget"
      data-onsubmit-topic="model/auth/post/form/logon"
      {% if q.options.is_username_checked %}
            data-onsubmit-message="{{ %{ username: q.options.username|default:q.username }|to_json|escape }}"
      {% endif %}
      >
    {#  <input type="hidden" name="onauth" value="{{ page|escape }}" /> #}

    {% include form_fields_tpl %}

</form>
<iframe src="" id="logonTarget" name="logonTarget" style="display:none"></iframe>
