{#
Params
- winner_id
- loser_id
- left
- right
#}
{% wire id="merge-form" type="submit" postback={merge winner_id=winner_id loser_id=loser_id} delegate=`mod_admin_merge` %}
<form id="merge-form" method="post" action="postback">
{#
    <fieldset class="form-horizontal">
        <div class="form-group row">
            <label class="control-label col-md-2">
                <div class="checkbox">
                    <label>
                        <input type="radio" id="merge_only" name="merge_action" value="merge_only" checked="checked" />
                    </label>
                </div>
            </label>
            <div class="col-md-10">
                <h5 class="form-control-static"><label for="merge_only">{_ Only merge _}</label></h5>
            </div>
        </div>
        <div class="form-group row">
            <div class="control-label col-md-2"></div>
            <div class="col-md-4">
                <div class="panel panel-default">
                    <div class="panel-body">
                        {{ loser_id.title|truncate_html:80 }}
                    </div>
                </div>
            </div>
            <div class="col-md-2">
                <div class="panel panel-default" style="border: none; box-shadow: none; text-align: center">
                    <div class="panel-body">
                        {% if right %}&rarr;{% else %}&larr;{% endif %}
                    </div>
                </div>
            </div>
            <div class="col-md-4">
                <div class="panel panel-default">
                    <div class="panel-body">
                        {{ winner_id.title|truncate_html:80 }}
                    </div>
                </div>
            </div>
        </div>
    </fieldset>
#}
    <fieldset class="form-horizontal">
        <div class="form-group row">
            <input type="hidden" name="merge_action" value="merge_delete" />
{#
            <label class="control-label col-md-2">
                <div class="checkbox">
                    <label>
                        <input type="radio" id="merge_delete" name="merge_action" value="merge_delete" />
                    </label>
                </div>
            </label>
#}
            <div class="col-md-10">
                <h5 class="form-control-static"><label for="merge_delete">{_ Merge and delete _}</label></h5>
            </div>
        </div>
        <div class="form-group row">
{#
            <div class="control-label col-md-2"></div>
#}
            <div class="col-md-4">
                <div class="panel panel-default">
                    <div class="panel-body">
                        {{ loser_id.title|truncate_html:80 }}
                    </div>
                </div>
            </div>
            <div class="col-md-2">
                <div class="panel panel-default" style="border: none; box-shadow: none; text-align: center">
                    <div class="panel-body">
                        {% if right %}&rarr;{% else %}&larr;{% endif %}
                    </div>
                </div>
            </div>
            <div class="col-md-4">
                <div class="panel panel-default">
                    <div class="panel-body">
                        {{ winner_id.title|truncate_html:80 }}
                    </div>
                </div>
            </div>
        </div>
        <div class="form-group row">
{#
            <div class="control-label col-md-2"></div>
#}
            <div class="col-md-4">
                {% if right %}
                    <div class="panel panel-default">
                        <div class="panel-body">
                            <s>{{ loser_id.title }}</s>
                        </div>
                    </div>
                {% endif %}
            </div>
            <div class="col-md-2">
                <div class="panel panel-default" style="border: none; box-shadow: none; text-align: center">
                    <div class="panel-body"></div>
                </div>
            </div>
            <div class="col-md-4">
                {% if left %}
                    <div class="panel panel-default">
                        <div class="panel-body">
                            <s>{{ winner_id.title }}</s>
                        </div>
                    </div>
                {% endif %}
            </div>
        </div>
        <div class="form-group row">
{#
            <div class="control-label col-md-2"></div>
#}
            <div class="col-md-10">
                {_ Deletion cannot be undone. _}
            </div>
        </div>
    </fieldset>
        <div class="modal-footer clearfix">
            {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
            {% button class="btn btn-primary" text=_"Merge" %}
        </div>
</form>
