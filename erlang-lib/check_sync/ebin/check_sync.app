%% This is an -*- erlang -*- file

{application, check_sync,
 [
  {description, "Application skeleton"},
  {vsn, "2.0"},
  {modules, [check_sync, check_sync_sup, check_sync_server]},
  {mod, {check_sync, []}},
  {registered, [check_sync_server]},
  {applications, [stdlib, kernel]},
  {env, []}
 ]}.
