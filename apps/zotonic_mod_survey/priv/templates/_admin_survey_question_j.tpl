{#
    The page jumps are stored as blocks.
    With up to two jumps per block.

    In mod_survey.erl there is a admin_rscform observer which calls
    code in survey_admin.erl to map the jump-conditions to blocks.
#}
<li class="page-jump form-horizontal" id="{{ #j }}">
    <div class="drag-handle"></div>

    <div class="page-jump-details">
        <div class="row">
            <label class="control-label col-sm-2 col-md-1">{_ if _}</label>
            <div class="col-sm-9 col-md-10 col-lg-11">
                <input name="jump-condition-{{#j}}" id="jump-condition-{{#j}}" class="form-control jump-condition {% if nosubmit %}nosubmit{% endif %}" type="text" placeholder="{_ question == 1 _}" value="{{ condition }}">
            </div>
        </div>
        <div class="row">
            <label class="control-label col-sm-2 col-md-1">{_ go to _}</label>
            <div class="col-sm-6">
                <input name="jump-target-{{#j}}" id="jump-target-{{#j}}" class="form-control jump-target {% if nosubmit %}nosubmit{% endif %}" type="text" placeholder="{_ Question label _}" value="{{ target }}">
            </div>
        </div>

        <div class="row">
            <div class="col-sm-2 col-md-1"></div>
            <div class="col-sm-10">
            	<ul class="nav nav-pills">
            		<li><a href="#jump-go">{_ Go to question _}</a></li>
            		<li><a href="#jump-delete">{_ Delete page jump _}</a></li>
            	</ul>
            </div>
        </div>
    </div>
</li>
