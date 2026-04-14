{% extends "admin_base.tpl" %}

{% block title %}{_ Translations _}: {{ id.title|default:id.short_title|default:_"Untitled" }}{% endblock %}

{% block navigation %}
{% endblock %}

{% block container %}

{% fragment line %}
    <tr style="border-bottom: 2px solid black">
        <th></th>
        {% for code, lang in m.translation.language_list_editable %}
            {% if code|member:r_lang %}
                <td></td>
            {% endif %}
        {% endfor %}
    </tr>
{% endfragment %}

{% with m.rsc[id].language|default:[z_language] as r_lang %}
    <div style="padding: 0 10px">
        <h1>
            {{ id.title|default:id.short_title|default:_"<em>Untitled</em>" }}

            <a id="link-edit" href="{% url admin_edit_rsc id=id %}" class="btn btn-primary">
                {% if q.close %}
                    {_ Close _}
                    {% javascript %}
                       document.getElementById('link-edit').addEventListener("click", (e) => {
                            window.close();
                        });
                    {% endjavascript %}
                {% else %}
                    {_ Edit _}
                {% endif %}
            </a>
        </h1>

        <p class="help-block">
            {_ Translated page texts for every editable language that has a translation in the resource. _}
        </p>
    </div>

    <table class="table table-striped">
        <thead style="position: sticky; top: 0; background-color: white">
            <tr>
                <th width="5%">{_ Property _}</th>
                {% for code, lang in m.translation.language_list_editable %}
                    {% if code|member:r_lang %}
                    <th>
                        <span {% include "_language_attrs.tpl" language=code %}>{{ lang.name }}</span>
                        <span class="text-muted">&ndash; {% if code != 'en' %}{{ lang.name_en }}{% endif %} ({{ code }})</span>
                    </th>
                    {% endif %}
                {% endfor %}
            </tr>
        </thead>
        <tbody>
        {% with [
                'chapeau',
                'title',
                'short_title',
                'subtitle',
                'summary',
                'body',
                'body_extra'
            ] as base_props
        %}
        {% with id|translated_texts as trs %}
            {% for p in base_props %}
                {% if trs[p]|is_defined %}
                    <tr>
                        <th>{{ p|escape }}</th>
                        {% for code, lang in m.translation.language_list_editable %}
                            {% if code|member:r_lang %}
                                <td>{{ trs[p] with z_language=code }}</td>
                            {% endif %}
                        {% endfor %}
                    </tr>
                {% endif %}
            {% endfor %}

            {% for p,tr in trs %}
                {% if not p|member:base_props and not p|match:"^seo_" %}
                    <tr>
                        <th>{{ p|escape }}</th>
                        {% for code, lang in m.translation.language_list_editable %}
                            {% if code|member:r_lang %}
                                <td>{{ trs[p] with z_language=code }}</td>
                            {% endif %}
                        {% endfor %}
                    </tr>
                {% endif %}
            {% endfor %}

            {% use line %}

            {% for b in id.blocks %}
                {% if b.type == 'survey_page_options' %}
                {% elseif b.type == 'survey_page_break' %}
                    <tr style="border-bottom: 2px dashed black;">
                        <th>
                            <small class="text-muted">{_ page break _}</small>
                        </th>
                        {% for code, lang in m.translation.language_list_editable %}
                            {% if code|member:r_lang %}
                                <td></td>
                            {% endif %}
                        {% endfor %}
                    </tr>
                {% elseif b.type|match:"survey_.*" %}
                    <tr>
                        <th rowspan="3">
                            {{ b.name }}<br>
                            <small class="text-muted">{{ b.type|replace:'survey_':''|replace:'_':' ' }}</small>
                        </th>
                        {% for code, lang in m.translation.language_list_editable %}
                            {% if code|member:r_lang %}
                                <td>{{ b.prompt with z_language=code }}</td>
                            {% endif %}
                        {% endfor %}
                    </tr>
                    <tr>
                        {% for code, lang in m.translation.language_list_editable %}
                            {% if code|member:r_lang %}
                                <td>{{ b.explanation with z_language=code }}</td>
                            {% endif %}
                        {% endfor %}
                    </tr>
                    <tr>
                        {% for code, lang in m.translation.language_list_editable %}
                            {% if code|member:r_lang %}
                            <td>
                                {% if b.type == 'survey_thurstone' %}
                                    <ol>
                                        {% for ans in b.answers %}
                                            <li>{{ ans.option with z_language=code }}</li>
                                        {% endfor %}
                                    </ol>
                                {% elseif b.type == 'survey_likert' %}
                                    <ol>
                                        <li>{{ b.agree|default:_"Strongly Agree" with z_language=code }}</li>
                                        <li>{{ b.disagree|default:_"Strongly Disagree" with z_language=code }}</li>
                                    </ol>
                                {% elseif b.type == 'survey_yesno' %}
                                    <ol>
                                        <li>{{ b.yes|default:_"Yes" with z_language=code }}</li>
                                        <li>{{ b.no|default:_"No" with z_language=code }}</li>
                                    </ol>
                                {% elseif b.type == 'survey_truefalse' %}
                                    <ol>
                                        <li>{{ b.yes|default:_"True" with z_language=code }}</li>
                                        <li>{{ b.no|default:_"False" with z_language=code }}</li>
                                    </ol>
                                {% elseif b.type == 'survey_matching' %}
                                    {{ b.matching with z_language=code }}
                                {% elseif b.type == 'survey_multiple_choice' %}
                                    {{ b.choices with z_language=code }}
                                {% elseif b.type == 'survey_narrative' %}
                                    {{ b.narrative with z_language=code }}
                                {% endif %}
                            </td>
                            {% endif %}
                        {% endfor %}
                    </tr>
                {% else %}
                    <tr>
                        <th>
                            {{ b.name }}<br>
                            <small class="text-muted">{{ b.type }}</small>
                        </th>
                        {% for code, lang in m.translation.language_list_editable %}
                            {% if code|member:r_lang %}
                                <td>
                                    {% for p, tr in b|translated_texts %}
                                        {% if p == 'header' %}
                                            <h3>{{ tr with z_language=code }}</h3>
                                        {% else %}
                                            <div class="body">{{ tr with z_language=code }}</div>
                                        {% endif %}
                                    {% endfor %}

                                    {% if b.rsc_id %}
                                        <p>
                                            <span class="text-muted">{_ Page _}:</span>
                                            {{ b.rsc_id.title with z_language=code }}
                                        </p>
                                    {% endif %}
                                </td>
                            {% endif %}
                        {% endfor %}
                    </tr>
                {% endif %}
            {% endfor %}

            {% use line %}

            {% for p,tr in trs %}
                {% if p|match:"^seo_" %}
                    <tr>
                        <th>{{ p|escape }}</th>
                        {% for code, lang in m.translation.language_list_editable %}
                            {% if code|member:r_lang %}
                                <td>{{ trs[p] with z_language=code }}</td>
                            {% endif %}
                        {% endfor %}
                    </tr>
                {% endif %}
            {% endfor %}

        {% endwith %}
        {% endwith %}
        </tbody>
    </table>
{% endwith %}
{% endblock %}

{% block footer %}
{% endblock %}
