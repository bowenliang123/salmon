{application, salmon,
 [
  {description, ""},
  {vsn, "1.0.1"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, {salmon, []}},
  {env, [{default_hook_interval, 1000},
  		{max_ezk_conns_count, 50}]}
 ]}.