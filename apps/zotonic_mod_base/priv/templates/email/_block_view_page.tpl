{% with m.rsc[blk.rsc_id].id as id %}
{% if id.is_a.image or id.is_a.video %}
    {% include "email/_spacer.tpl" %}
    <tr>
        <td style="background-color: #ffffff;">
            <img src="{% image_url id upscale width=1200 height=600 crop absolute_url %}" width="600" height="" alt="{{ id.title }}" border="0" style="width: 100%; max-width: 600px; height: auto; background: #dddddd; font-family: sans-serif; font-size: 15px; line-height: 15px; color: #555555; margin: auto; display: block;" class="g-img">
        </td>
    </tr>
{% endif %}
{% endwith %}
