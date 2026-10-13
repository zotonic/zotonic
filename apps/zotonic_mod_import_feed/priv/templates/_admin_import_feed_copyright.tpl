<fieldset>
    <legend>{_ Default copyright _}</legend>
    <div class="form-group">
        <label for="feed-rights">{_ License _}</label>
        <select id="feed-rights" name="rights" class="form-control">
            <option value="">{_ Default _}</option>
            <option value="CR" {% if feed.rights == `CR` %}selected{% endif %}>© {_ All Rights Reserved _}</option>
            <option value="PD" {% if feed.rights == `PD` %}selected{% endif %}>{_ Public Domain _}</option>
            <optgroup label="Creative Commons">
                {% for lic in m.copyright.list.creative_commons %}
                    <option value="{{ lic.name|escape }}" {% if lic.name == feed.rights %}selected{% endif %}>{{ lic.title|escape }}</option>
                {% endfor %}
            </optgroup>
            <optgroup label="Rights Statements">
                {% for lic in m.copyright.list.rights_statements %}
                    <option value="{{ lic.name|escape }}" {% if lic.name == feed.rights %}selected{% endif %}>{{ lic.title|escape }}</option>
                {% endfor %}
            </optgroup>
        </select>
    </div>

    <div class="form-group">
        <label for="feed-rights-attribution">{_ Attribution _}</label>
        <input id="feed-rights-attribution" name="rights_attribution" class="form-control" value="{{ feed.rights_attribution|escape }}" maxlength="1000">
    </div>

    <div class="form-group">
        <label for="feed-rights-year">{_ Year _}</label>
        <input id="feed-rights-year" name="rights_year" class="form-control" value="{{ feed.rights_year|escape }}" maxlength="100">
    </div>
    <p class="help-block">{_ These defaults apply to imported articles and their dependent media. Leave empty to use the standard copyright defaults. _}</p>
</fieldset>
