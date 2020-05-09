{% extends "admin_base.tpl" %}

{% block title %}{_ Cloud File Store Configuration _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Cloud File Store Configuration _}</h2>

        <p>{_ Zotonic can store uploaded and resized files in the cloud. Here you can configure the location and access keys for the cloud service. _}</p>

        <p>{_ Currently Zotonic supports services that are compatible with the S3 file services API. These include: _}</p>
        <ul>
            <li><a target="_blank" href="http://aws.amazon.com/s3/">Amazon Simple Storage Service (S3)</a></li>
            <li><a target="_blank" href="https://developers.google.com/storage/">Google Cloud Storage</a></li>
        </ul>
    </div>

    {% if m.acl.is_allowed.use.mod_admin_config %}
        {% wire id="admin_filestore"
            type="submit"
            postback=`admin_filestore`
            delegate=`filestore_admin`
        %}

        <div class="row">
            <div class="col-md-6">
                <form name="admin_filestore" id="admin_filestore" method="POST" action="postback" class="form">
                    <div class="widget">
                        <h3 class="widget-header">{_ S3 Cloud Location and Credentials _}</h3>
                        <div class="widget-content">
                            <div class="form-group row">
                                <label class="control-label col-md-12" for="s3url">{_ Base URL _}</label>
                                <div class="col-md-12">
                                    <input type="text" id="s3url" name="s3url"
                                        value="{{ m.config.mod_filestore.s3url.value|escape }}" class="form-control"
                                        placeholder="https://mybucket.s3.amazonaws.com/mysite"
                                    />
                                </div>
                            </div>

                            <div class="form-group row">
                                <label class="control-label col-md-12" for="s3key">{_ API Key _}</label>
                                <div class="col-md-12">
                                    <input type="text" id="s3key" name="s3key" value="{{ m.config.mod_filestore.s3key.value|escape }}" class="form-control" />
                                </div>
                            </div>

                            <div class="form-group row">
                                <label class="control-label col-md-12" for="s3secret">{_ API Secret _}</label>
                                <div class="col-md-12">
                                    <input type="text" id="s3secret" name="s3secret" value="{{ m.config.mod_filestore.s3secret.value|escape }}" class="form-control" />
                                </div>
                            </div>

                            <div class="form-group row">
                                <div class="col-md-12 form-inline">
                                    <label class="checkbox">
                                        <input type="checkbox" class="checkbox" id="is_upload_enabled" name="is_upload_enabled"
                                            {% if m.config.mod_filestore.is_upload_enabled.value == "true" %}checked{% endif %} />
                                        {_ Upload new media files to the cloud file store _}
                                    </label>
                                </div>
                            </div>
                            <div class="form-group row">
                                <div class="col-md-12 form-inline">
                                    <label class="checkbox">
                                        <input type="checkbox" id="is_create_bucket" name="is_create_bucket">
                                        {_ Try to create a private bucket if the bucket does not exist _}
                                    </label>
                                </div>
                            </div>

                            <div class="form-group">
                                <div class="form-inline">
                                    <label for="delete_interval" class="control-label">{_ Delete files from the cloud file store _}&nbsp;</label>
                                    {% with m.config.mod_filestore.delete_interval.value|if_undefined:"0" as value %}
                                         <select class="form-control input-sm" id="delete_interval" name="delete_interval">
                                             <option value="0"{% if value == "0" %} selected{% endif %}>{_ Immediately _}</option>
                                             <option value="1 week"{% if value == "1 week" %} selected{% endif %}>{_ After 1 week _}</option>
                                             <option value="1 month"{% if value == "1 month" %} selected{% endif %}>{_ After 1 month _}</option>
                                             <option value="3 months"{% if value == "3 months" %} selected{% endif %}>{_ After 3 months _}</option>
                                             <option value="false"{% if value == "false" %} selected{% endif %}>{_ Never _}</option>
                                         </select>
                                     {% endwith %}
                                </div>
                            </div>

                            <p class="help-block">{_ Before the settings are saved they will be checked by uploading (and removing) a small file. _}</p>

                            <p id="s3ok" class="alert alert-success" style="display:none">{_ Settings are working fine and are saved. _}</p>
                            <p id="s3error" class="alert alert-danger" style="display:none">{_ Could not access the service, double check your settings and try again. Make sure that API key has access rights to create and remove a (temporary) <code>-zotonic-filestore-test-file-</code> file. _}</p>

                            <div class="form-group">
                                <div>
                                    <button class="btn btn-primary" type="submit">{_ Save Settings _}</button>
                                </div>
                            </div>

                        </div>
                    </div>
                </form>
            </div>
            <div class="col-md-6">
            <div class="widget">
                <h3 class="widget-header">{_ S3 Cloud Utilities and Statistics _}</h3>
                <div class="widget-content">
                    {% with m.filestore.stats as stats %}
                        <table class="table table-striped">
                            <thead>
                                <tr>
                                    <th></th>
                                    <th>{_ Media Resources _}</th>
                                    <th>{_ Local Files _}</th>
                                    <th>{_ Cloud Files _}</th>
                                </tr>
                            </thead>
                            <tbody>
                                <tr>
                                    <th>{_ Files _}</th>
                                    <td>{{ stats.archived }}</td>
                                    <td>{{ stats.local }}</td>
                                    <td>{{ stats.cloud }}</td>
                                </tr>
                                <tr>
                                    <th>{_ Storage _}</th>
                                    <td>{{ stats.archive_size|filesizeformat }}</td>
                                    <td>{{ stats.local_size|filesizeformat }}</td>
                                    <td>{{ stats.cloud_size|filesizeformat }}</td>
                                </tr>
                            </tbody>

                            <thead>
                                <tr>
                                    <th></th>
                                    <th><br>{_ Upload Queue _}</th>
                                    <th><br>{_ Download Queue _}</th>
                                    <th><br>{_ Delete Queue_}</th>
                                </tr>
                            </thead>
                            <tbody>
                                <tr>
                                    <th>{_ Files _}</th>
                                    <td id="s3queue">{{ stats.queued|default:0 }}</td>
                                    <td id="s3queue-local">{{ stats.queued_local|default:0 }}</td>
                                    <td>{{ stats.queued_deleted|default:0 }}</td>
                                </tr>
                            </tbody>
                        </table>

                    {% endwith %}

                    <h3>{_ Actions _}</h3>

                    <p class="help-block">
                        {_ All local uploaded and preview files can be moved to the cloud. _}<br/>
                        {_ This will queue the files for later asynchronous upload. _}
                        {_ There is a 10 minute wait before files are uploaded. _}
                    </p>

                    <p id="s3error-queue" class="alert alert-error" style="display:none">{_ Could not access the service, double check your settings and try again. _}</p>
                    <p id="s3ok-queue" class="alert alert-success" style="display:none">{_ All files will be queued, uploads will start in the background within 10 minutes. _}</p>
                    <p id="s3ok-queue-local" class="alert alert-success" style="display:none">{_ All cloud files will be queued for download. _}</p>

                    <div class="form-group">
                        <div>
                            <button id="queue-all" name="queue-all" type="submit" class="btn btn-danger">
                                {_ Move all local files to S3 _}
                            </button>
                            {% wire id="queue-all"
                                    action={confirm
                                        text=_"Are you sure you want to move all files to the cloud?"
                                        ok=_"Move files to cloud"
                                        is_danger
                                        postback={admin_filestore_queue is_to_cloud}
                                        delegate=`filestore_admin`
                                    }
                            %}
                        </div>
                    </div>

                    <p class="help-block">
                        {_ All cloud files can be moved back to the file system on the server. _} 
                        {_ Ensure yourself there is enough disk space before starting this process. _}
                    </p>

                    <div class="form-group">
                        <div>
                            <button id="queue-local" name="queue-local" type="submit" class="btn btn-default">
                                {_ Move all S3 files to local _}
                            </button>
                            {% wire id="queue-local"
                                    action={confirm
                                        text=_"Are you sure you want to move all files to the server?"
                                        ok=_"Move files to local"
                                        is_danger
                                        postback={admin_filestore_queue is_to_local}
                                        delegate=`filestore_admin`
                                    }
                            %}
                        </div>
                    </div>
                </div>
            </div>
        </div>
        </div>

    {% else %}
        <p class="alert alert-danger">
            {_ You are not allowed to change these settings. _}
        </p>
    {% endif %}

{% endblock %}
