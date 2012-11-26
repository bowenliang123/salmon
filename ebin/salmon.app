{application, salmon,
 [
  {description, ""},
  {vsn, "1.0.1"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  sasl
                 ]},
  {mod, {salmon, []}},
  {env, [{default_hook_interval, 5000}]}
 ]}.