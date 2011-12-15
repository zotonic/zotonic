erl -pa `pwd`/ebin -sname $1  -boot start_sasl -sasl errlog_type  all -s zynamo

