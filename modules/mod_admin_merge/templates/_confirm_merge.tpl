{#
Params
- winner_id
- loser_id
- left
- right
#}
{% wire id="merge-form" type="submit" postback={merge winner_id=winner_id loser_id=loser_id} delegate=`mod_admin_merge` %}
<form id="merge-form" method="post" action="postback">

    <fieldset class="form-horizontal">
        <div class="control-group row-fluid">
            <input type="hidden" name="merge_action" value="merge_delete" />
            <div class="span10">
                <h5 class="form-control-static"><label for="merge_delete">{_ Merge and delete _}</label></h5>
            </div>
        </div>
        <div class="control-group row-fluid">
            <div class="span4">
                <div class="panel panel-default">
                    <div class="panel-body">
                        {{ loser_id.title|truncate_html:80 }}
                    </div>
                </div>
            </div>
            <div class="span2">
                <div class="panel panel-default" style="border: none; box-shadow: none; text-align: center">
                    <div class="panel-body">
                        {% if right %}&rarr;{% else %}&larr;{% endif %}
                    </div>
                </div>
            </div>
            <div class="span4">
                <div class="panel panel-default">
                    <div class="panel-body">
                        {{ winner_id.title|truncate_html:80 }}
                    </div>
                </div>
            </div>
        </div>
        <div class="control-group row-fluid">
            <div class="span4">
                {% if right %}
                    <div class="panel panel-default">
                        <div class="panel-body">
                            <s>{{ loser_id.title }}</s>
                        </div>
                    </div>
                {% endif %}
            </div>
            <div class="span2">
                <div class="panel panel-default" style="border: none; box-shadow: none; text-align: center">
                    <div class="panel-body"></div>
                </div>
            </div>
            <div class="span4">
                {% if left %}
                    <div class="panel panel-default">
                        <div class="panel-body">
                            <s>{{ winner_id.title }}</s>
                        </div>
                    </div>
                {% endif %}
            </div>
        </div>
        <div class="control-group row-fluid">
            <div class="span10">
                {_ Deletion cannot be undone. _}
            </div>
        </div>
    </fieldset>
    <div class="modal-footer clearfix">
        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
        {% button class="btn btn-primary" text=_"Merge" %}
    </div>
</form>
