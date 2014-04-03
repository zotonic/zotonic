{% extends "admin_base.tpl" %}

{% block title %}{_ Cloud File Store Configuration _}{% endblock %}

{% block content %}

<div class="admin-header">

    <h2>{_ Cloud File Store Configuration _}</h2>

    <p>{_ Zotonic can store uploaded and resized files in the cloud. Here you can configure the location and access keys for the cloud service. _}</p>

    <p>{_ Currently Zotonic supports services that are compatible with the S3 file services API. These include: _}</p>
    <ul>
        <li><a target="_blank" href="https://www.greenqloud.com/">GreenQloud</a></li>
        <li><a target="_blank" href="http://aws.amazon.com/s3/">Amazon Simple Storage Service (S3)</a></li>
        <li><a target="_blank" href="https://developers.google.com/storage/">Google Cloud Storage</a></li>
    </ul>

    {% if m.acl.is_allowed.use.mod_admin_config %}
        {% wire id="admin_filestore" 
                type="submit" 
                postback=`admin_filestore`
                delegate=`filestore_admin`
        %}
        <form name="admin_filestore" id="admin_filestore" method="POST" action="postback">

            <div class="widget">
                <h3 class="widget-header">{_ S3 Cloud Location and Credentials _}</h3>
                <div class="widget-content">
                    <div class="control-group">
                        <label for="app_id">{_ Base Url _}</label>
                        <div class="controls">
                            <input type="text" id="s3url" name="s3url" 
                                   value="{{ m.config.mod_filestore.s3url.value|escape }}" class="span8"
                                   placeholder="https://s.greenqloud.com/account-bucket/mysite"
                            />
                        </div>
                    </div>
                    
                    <div class="control-group">
                        <label class="control-label" for="s3key">{_ API Key _}</label>
                        <div class="controls">
                            <input type="text" id="s3key" name="s3key" value="{{ m.config.mod_filestore.s3key.value|escape }}" class="span6" />
                        </div>
                    </div>
                    
                    <div class="control-group">
                        <label class="control-label" for="s3secret">{_ API Secret _}</label>
                        <div class="controls">
                            <input type="text" id="s3secret" name="s3secret" value="{{ m.config.mod_filestore.s3secret.value|default:'email'|escape }}" class="span6" />
                        </div>
                    </div>

                    <div class="control-group">
                        <div class="controls">
                            <label class="checkbox" for="is_upload_enabled">
                                <input type="checkbox" id="is_upload_enabled" name="is_upload_enabled" {% if m.config.mod_filestore.is_upload_enabled.value %}checked{% endif %} />
                                {_ Upload new files to the cloud _}
                            </label>
                        </div>
                    </div>

                    <p class="help-block">{_ Before the settings are saved they will be checked by uploading (and removing) a small file. _}</p>

                    <p id="s3ok" class="alert alert-success" style="display:none">{_ Settings are working fine and are saved. _}</p>
                    <p id="s3error" class="alert alert-error" style="display:none">{_ Could not access the service, double check your settings and try again. _}</p>

                    <div class="control-group">
                        <div class="controls">
                            <button class="btn btn-primary" type="submit">{_ Save Settings _}</button>
                        </div>
                    </div>
                    
                </div>
            </div>
            
        </form>

        <div class="widget">
            <h3 class="widget-header">{_ S3 Cloud Utilities and Statistics _}</h3>
            <div class="widget-content">
                {% wire id="admin_filestore_queue"
                    type='submit'
                    postback=`admin_filestore_queue`
                    delegate=`filestore_admin`
                %}
                <form name="admin_filestore_queue" id="admin_filestore_queue" method="POST" action="postback">

                    {% with m.filestore.stats as stats %}
                    <table class="table condensed" style="width: auto">
                        <thead>
                            <tr>
                                <th></th>
                                <th>{_ Uploaded _}</th>
                                <th>{_ Local _}</th>
                                <th>{_ Cloud _}</th>
                                <th>{_ Queued _}</th>
                            </tr>
                        </thead>
                        <tbody>
                            <tr>
                                <th>{_ Files _}</th>
                                <td>{{ stats.archived }}</td>
                                <td>{{ stats.local }}</td>
                                <td>{{ stats.cloud }}</td>
                                <td id="s3queue">{{ stats.queued }}</td>
                            </tr>
                            <tr>
                                <th>{_ Storage _}</th>
                                <td>{{ stats.archive_size|filesizeformat }}</td>
                                <td>{{ stats.local_size|filesizeformat }}</td>
                                <td>{{ stats.cloud_size|filesizeformat }}</td>
                                <td></td>
                            </tr>
                        </tbody>
                    </table>
                    {% endwith %}

                    <p class="help-block">
                        {_ All local uploaded and preview files can be moved to the cloud. _}<br/>
                        {_ This will queue the files for later asynchronous upload. _}
                    </p>

                    <p id="s3error-queue" class="alert alert-error" style="display:none">{_ Could not access the service, double check your settings and try again. _}</p>
                    <p id="s3ok-queue" class="alert alert-success" style="display:none">{_ Queued all uploaded files, uploads will be done in the background. _}</p>

                    <div class="control-group">
                        <div class="controls">
                            <button id="queue-all" type="submit" class="btn btn-danger">{_ Move all files to S3 _}</button>
                        </div>
                    </div>
                </form>
            </div>

        </div>
    {% else %}
        <p class="alert alert-error">
            {_ You are not allowed to change these settings. _}
        </p>
    {% endif %}

</div>


{% endblock %}
