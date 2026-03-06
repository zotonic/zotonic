{# Overlay for the lightbox - show a media item with prev/next
 # Media viewer using an overlay - opened via mod_wired notify observer.
 #}
<div id="{{ #media }}">
    <div class="mediaoverlay--media">
        {% block media %}
            {% media id mediaclass="mediaoverlay" autoplay %}
        {% endblock %}
    </div>
    <div class="mediaoverlay--caption">
        {% block caption %}
            {% if id.is_editable %}
                {% if m.modules.active.mod_admin_frontend %}
                    <a href="{% url admin_frontend_edit id=id %}" class="btn btn-primary btn-xs pull-right">{_ Edit _}</a>
                {% elseif m.acl.is_allowed.use.mod_admin %}
                    <a href="{% url admin_edit_rsc id=id %}" class="btn btn-primary btn-xs pull-right">{_ Edit _}</a>
                {% endif %}
            {% endif %}
            {{ id|summary|default:id.title|default:id.original_filename }}
        {% endblock %}
    </div>

    {% if ids|length > 1 %}
        <button class="mediaoverlay--nav mediaoverlay--prev" rel="button" title="{_ Previous image _}" id="{{ #next }}">
            ＜
        </button>

        <button class="mediaoverlay--nav mediaoverlay--next" rel="button" title="{_ Next image _}" id="{{ #prev }}">
            ＞
        </button>

        {% wire id=#next
                postback={mediaoverlay_update element_id=#media id=id ids=ids is_next=true}
                delegate="mod_wires"
        %}
        {% wire id=#prev
                postback={mediaoverlay_update element_id=#media id=id ids=ids is_next=false}
                delegate="mod_wires"
        %}
    {% endif %}
</div>
