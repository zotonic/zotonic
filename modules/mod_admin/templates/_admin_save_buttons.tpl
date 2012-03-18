<div class="save-buttons">
    <div class="pull-right">
	{% button class="btn" text=_"Cancel" action={redirect back} %}
	{% button class="btn btn-primary" text=_"Save this page" disabled=not is_editable %}
    </div>
</div>
