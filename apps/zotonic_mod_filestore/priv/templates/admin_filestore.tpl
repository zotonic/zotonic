{% extends "admin_base.tpl" %}

{% block title %}{_ Cloud File Store Configuration _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Cloud File Store Configuration _}</h2>

        <p>{_ Zotonic can store uploaded and resized files in the cloud. Here you can configure the location and access keys for the cloud service. _}</p>

        <p>{_ Currently Zotonic supports FTP and services that are compatible with the S3 file services API. These include: _}</p>

        <ul>
            <li><a target="_blank" href="https://european-alternatives.eu/alternative-to/amazon-s3">European Block Storage providers <span class="glyphicon glyphicon-new-window"></span></a></li>
            <li><a target="_blank" href="https://aws.amazon.com/s3/">USA: Amazon Simple Storage Service (S3) <span class="glyphicon glyphicon-new-window"></span></a></li>
            <li><a target="_blank" href="https://developers.google.com/storage/">USA: Google Cloud Storage <span class="glyphicon glyphicon-new-window"></span></a></li>
        </ul>

        <p>{_ If you use an FTP server then that server MUST support FTPS (secure ftp) _}</p>

        <p>{_ With WebDAV the username and password are transfered in the clear, so ensure that you use <tt>webdavs:</tt> URLs (which makes use of https). _}</p>
    </div>


    {% if m.acl.is_allowed.use.mod_admin_config %}
        {% wire id="admin_filestore"
            type="submit"
            postback=`admin_filestore`
            delegate=`filestore_admin`
        %}

        <div class="row">
            <div class="col-md-6">
            {% if m.filestore.is_config_locked %}
                <div class="widget">
                    <h3 class="widget-header">{_ S3 Cloud Location and Credentials _}</h3>
                    <div class="widget-content">
                        <p class="alert alert-info"><span class="glyphicon glyphicon-info-sign"></span> {_ The cloud file store uses a global Zotonic configuration. The configuration cannot be changed here. _}</p>
                        <p>{_ The configuration is: _}</p>

                        <table class="table table-compact">
                            <tr>
                                <td>{_ Service _}</td>
                                <td><b>{{ m.filestore.service|escape }}</b></td>
                            </tr>
                            <tr>
                                <td>{_ S3 Cloud Location _}</td>
                                <td><b>{{ m.filestore.s3url|escape }}</b></td>
                            </tr>
                            <tr>
                                <td>{_ Is upload enabled? _}</td>
                                <td><b>{{ m.filestore.is_upload_enabled|if:_"Yes":_"No" }}</b></td>
                            </tr>
                            <tr>
                                <td>{_ Keep local files? _}</td>
                                <td>
                                    <b>
                                        {% if m.filestore.is_local_keep %}
                                            {_ Yes, keep local files after upload _}
                                        {% else %}
                                            {_ No, local files are deleted after upload _}
                                        {% endif %}
                                    </b>
                                </td>
                            </tr>
                            <tr>
                                <td>{_ Delete interval _}</td>
                                <td>
                                    <b>
                                        {% if m.filestore.delete_interval == '0' %}
                                            {_ Immediately _}
                                        {% elseif not m.filestore.delete_interval %}
                                            {_ Never _}
                                        {% else %}
                                            {{ m.filestore.delete_interval|escape }}
                                        {% endif %}
                                    </b>
                            </tr>
                        </table>
                    </div>
                </div>
            {% else %}
                <form name="admin_filestore" id="admin_filestore" method="POST" action="postback" class="form">
                    <div class="widget">
                        <h3 class="widget-header">{_ S3 Cloud Location and Credentials _}</h3>
                        <div class="widget-content">
                            <div class="form-group">
                                <label class="control-label" for="s3url">{_ Base URL _}</label>
                                <input type="text" id="s3url" name="s3url"
                                    value="{{ m.filestore.s3url|escape }}" class="form-control"
                                    placeholder="https://mybucket.s3.amazonaws.com/mysite"
                                />
                                {% validate id="s3url" type={format pattern="^(davs?:|webdavs?:|https?:|ftps?:)//.*$"} %}

                                <p class="help-block">
                                    {_ For S3 the URL must start with <b><tt>https:</tt></b> or <b><tt>http:</tt></b> _}<br>
                                    {_ For FTP the URL must start with <b><tt>ftps:</tt></b> or <b><tt>ftp:</tt></b> _}<br>
                                    {_ For WebDAV the URL must start with <b><tt>webdavs:</tt></b> or <b><tt>webdav:</tt></b> _}
                                </p>
                            </div>

                            <div class="form-group">
                                <label class="control-label" for="s3key">{_ S3 API Key or FTP/WebDAV username_}</label>
                                <input type="text" id="s3key" name="s3key" value="{{ m.filestore.s3key|escape }}" class="form-control" />
                            </div>

                            <div class="form-group">
                                <label class="control-label" for="s3secret">{_ S3 API Secret or FTP/WebDAV password _}</label>
                                <input type="password" id="s3secret" name="s3secret" value="{{ m.filestore.s3secret|escape }}" class="form-control" />
                            </div>

                            <div class="form-group">
                                <div class="form-inline">
                                    <label class="checkbox">
                                        <input type="checkbox" class="checkbox" id="is_upload_enabled" name="is_upload_enabled"
                                            {% if m.filestore.is_upload_enabled %}checked{% endif %} />
                                        {_ Upload new media files to the cloud file store _}
                                    </label>
                                </div>
                            </div>
                            <div class="form-group">
                                <div class="form-inline">
                                    <label class="checkbox">
                                        <input type="checkbox" class="checkbox" id="is_local_keep" name="is_local_keep"
                                            {% if m.filestore.is_local_keep %}checked{% endif %} />
                                        {_ Keep local files after upload to the cloud file store _}
                                    </label>
                                    <p class="help-block">{_ Enable this to let the filestore act as a backup of  your local media files. _}</p>
                                </div>
                            </div>
                            <div class="form-group">
                                <div class="form-inline">
                                    <label class="checkbox">
                                        <input type="checkbox" id="is_create_bucket" name="is_create_bucket">
                                        {_ Try to create a private S3 bucket if the S3 bucket does not exist _}
                                    </label>
                                    <p class="help-block">{_ With FTP and WebDAV the directories are always created automatically. _}</p>
                                </div>
                            </div>

                            <div class="form-group">
                                <div class="form-inline">
                                    <label for="delete_interval" class="control-label">{_ Delete files from the cloud file store _}&nbsp;</label>
                                    {% with m.filestore.delete_interval as value %}
                                         <select class="form-control input-sm" id="delete_interval" name="delete_interval">
                                             <option value="0"{% if value == "0" %} selected{% endif %}>{_ No extra delay _}</option>
                                             <option value="1 week"{% if value == "1 week" %} selected{% endif %}>{_ After 1 week _}</option>
                                             <option value="1 month"{% if value == "1 month" %} selected{% endif %}>{_ After 1 month _}</option>
                                             <option value="3 months"{% if value == "3 months" %} selected{% endif %}>{_ After 3 months _}</option>
                                             <option value="false"{% if value == "false" %} selected{% endif %}>{_ Never _}</option>
                                         </select>
                                     {% endwith %}
                                     <p class="help-block">
                                        {_ To allow recovery of deleted pages, Zotonic keeps files for 5 weeks. _} {_ This extra deletion delay extends the period files can be recovered. _}
                                     </p>
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
            {% endif %}
            </div>
            <div class="col-md-6">
                <div class="widget">
                    <h3 class="widget-header">{_ Cloud File Store Utilities and Statistics _}</h3>
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
                            {_ There is a 1 minute wait before files are uploaded. _}
                        </p>

                        <p class="help-block">
                            {_ If you have selected ”Keep local files after upload to the cloud file store” then the local files are not deleted, as the remote files are considered a backup of the local files. _}
                        </p>

                        <p id="s3error-queue" class="alert alert-danger" style="display:none">{_ Could not access the service, double check your settings and try again. _}</p>
                        <p id="s3ok-queue" class="alert alert-success" style="display:none">{_ All files will be queued, uploads will start in the background within 10 minutes. _}</p>

                        <div class="form-group">
                            <div>
                                <button id="queue-all" name="queue-all" type="submit" class="btn btn-danger">
                                    {_ Move all local files to the remote storage _}
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

                        <hr>

                        <p class="help-block">
                            {_ All cloud files can be moved back to the file system on the server. _}
                            {_ Ensure yourself there is enough disk space before starting this process. _}
                        </p>

                        <p class="help-block">
                            {_ If you have selected ”Keep local files after upload to the cloud file store” then the remote files are not deleted, as the remote files are considered a backup of the local files. _}
                        </p>

                        <p id="s3ok-queue-local" class="alert alert-success" style="display:none">{_ All cloud files will be queued for download. _}</p>

                        <div class="form-group">
                            <div>
                                <button id="queue-local" name="queue-local" type="submit" class="btn btn-default">
                                    {_ Move all remote files to local _}
                                </button>
                                {% wire id="queue-local"
                                        action={confirm
                                            text=_"Are you sure you want to move all files to the disk of the server?"
                                            ok=_"Move files to server disk"
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
