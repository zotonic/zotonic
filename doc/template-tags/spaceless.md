Removes whitespace between HTML tags.

Note

spaceless does not remove non breaking spaces and other whitespace.

Example:


```erlang
{% spaceless %}
<div>
    <p>Test test test</p>
</div>
{% endspaceless %}
```

After rendering:


```erlang
<div><p>Test test test</p></div>
```

New in version 0.8.