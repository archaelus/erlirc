{application, app_name,
 [{description, "App Description"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib]},
  {mod, {app_name_app, []}},
  {modules, [app_name_app, app_name_sup]},
  {registered, []}
 ]}.
