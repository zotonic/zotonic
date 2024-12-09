{% if m.acl.use.mod_ratelimit or m.acl.is_admin %}
    <div class="form-group">
        {% button class="btn btn-default"
                  text=_"Reset rate limit counters"
                  postback={reset_ratelimit}
                  delegate=`mod_ratelimit`
        %}
        <span class="help-block">{_ Reset the authentication attempts counters. Useful when encountering rate limit errors during testing. Be careful as this enables more password tries. _}</span>
    </div>
{% endif %}
