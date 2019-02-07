<div class="form-group">
    <label for="username" class="control-label">{_ Username _}</label>
    <input class="form-control" type="text" id="username" name="username" value="" autofocus="autofocus" autocomplete="on" />
    {% validate id="username"
        type={presence}
        only_on_submit
    %}
</div>

<div class="form-group">
    <label for="password" class="control-label">{_ Password _}</label>
    <input class="form-control" type="password" id="password" name="password" value="" autocomplete="on" />
    {% validate id="password"
        type={presence}
        only_on_submit
    %}
</div>

<div class="form-group passcode">
    <label for="password" class="control-label">{_ Passcode _}</label>
    <input class="form-control" type="number" id="passcode" name="passcode" value="" autocomplete="off" placeholder="{_ Two-factor passcode _}" />
</div>

<div class="form-group">
    <div class="checkbox">
        <label>
            <input type="checkbox" name="rememberme" value="1" />
            {_ Keep me signed in _}
        </label>
    </div>
</div>

<div class="form-group">
    <button class="btn btn-primary" type="submit">{_ Sign in _}</button>
</div>
