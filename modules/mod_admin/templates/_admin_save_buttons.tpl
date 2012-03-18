<div class="save-buttons">
    <div class="pull-right">
	{% button class="btn" text=_"cancel" action={redirect back} %}
	{% button class="btn btn-primary" text=_"save this page" disabled=not is_editable %}
    </div>
</div>
