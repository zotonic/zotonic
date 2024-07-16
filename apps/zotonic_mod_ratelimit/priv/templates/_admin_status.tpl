{% if m.acl.use.mod_ratelimit or m.acl.is_admin %}
    <div class="form-group">
        {% button class="btn btn-default"
                  text=_"Reset ratelimit"
                  postback={reset_ratelimit}
                  delegate=`mod_ratelimit`
        %}
        <span class="help-block">{_ Reset the rate limit administration. Useful when encountering rate limit errors during testing. Be careful to use on production as this enables more password tries. _}</span>
    </div>
{% endif %}
