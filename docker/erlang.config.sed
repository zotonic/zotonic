# Set mnesia dir to /tmp/mnesia
s,priv/mnesia,/tmp/mnesia,

# Disbable access logs
/log_dir/d

# Remove file backend console logs since they go to stdout
/lager_file_backend/d

# After removal of above, fix syntax by removing comma of preceding line
s/\(.*lager_console_backend.*\),/\1/

