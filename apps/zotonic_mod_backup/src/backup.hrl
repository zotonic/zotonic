
% Config values for daily backup
-define(BACKUP_NONE, 0).
-define(BACKUP_DB, 2).
-define(BACKUP_ALL, 1).

% Name of the singular job queue for updating config files.
% We only want a single updater to prevent race conditions when
% updating the config file.
-define(CONFIG_UPDATE_JOB, mod_backup_config_update).
