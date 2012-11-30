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
  {env, [{default_hook_interval, 500}]}
 ]}.