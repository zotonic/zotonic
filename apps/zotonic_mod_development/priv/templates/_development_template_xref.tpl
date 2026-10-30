
<h3>{_ Syntax errors _}</h3>
<p class="help-block">
    {_ These templates have syntax errors and could not be checked for included templates. _}
</p>

<table class="table table-bordered">
    <thead>
        <tr>
            <th>{_ Module _}</th>
            <th>{_ Template _}</th>
            <th>{_ Line _}</th>
            <th>{_ Column _}</th>
            <th>{_ Error _}</th>
        </tr>
    </thead>
    <tbody>
        {% for tpl, error in xref.errors %}
            <tr>
                {% with tpl|development_filename|split:"/priv/templates/" as ref %}
                    <td>
                        {{ ref[1]|escape_check }}
                    </td>
                    <td>
                        {{ ref[2]|escape_check }}
                        {% button text=_"View" class="btn btn-primary btn-xs" postback={template_view template=tpl} delegate=`mod_development` %}
                        {% include "_development_editor_button.tpl" editor_file=tpl editor_line=error.line editor_column=error.column editor_label=_"Edit" editor_compact editor_title=_"Edit template" %}
                    </td>
                {% endwith %}
                <td>
                    {{ error.line }}
                </td>
                <td>
                    {{ error.column }}
                </td>
                <td>
                    {{ error.text|escape_check }}
                </td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="5">
                    <span class="text-muted">
                        {_ No template syntax errors found. _}
                    </span>
                </td>
            </tr>
        {% endfor %}
    </tbody>
</table>

<h3>{_ Missing included templates _}</h3>
<p class="help-block">
    {% trans "These templates have non optional <tt>{include}</tt> statements that are referring to missing templates."
        include="include"
    %}
</p>

{% include "_development_template_xref_includes.tpl" references_by_file=xref.missing %}


<h3>{_ Missing optional included templates _}</h3>
<p class="help-block">
    {% trans "These templates have <tt>{optional}</tt> statements that are referring to missing templates."
        optional="optional include"
    %}
</p>

{% include "_development_template_xref_includes.tpl" references_by_file=xref.optional %}

<h3>
    {% trans "Missing {extends} or {overrules}"
         extends="extends"
         overrules="overrules"
    %}
</h3>
<p class="help-block">
    {% trans "These templates have <tt>{extends}</tt> or <tt>{overrules}</tt> statements that are referring to a missing template."
         extends="extends"
         overrules="overrules"
    %}
</p>

{% include "_development_template_xref_inheritance.tpl" inheritance_by_file=xref.extend_errors %}

<details>
    <summary>{_ All included templates _}</summary>
{% include "_development_template_xref_includes.tpl" references_by_file=xref.includes %}
</details>
<details>
    <summary>{_ All extended and overruled templates _}</summary>
{% include "_development_template_xref_inheritance.tpl" inheritance_by_file=xref.inheritance %}

</details>
