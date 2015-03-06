# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project, this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for third-
# party users, it should be done in your mix.exs file.

# Sample configuration:
#
#     config :logger, :console,
#       level: :info,
#       format: "$date $time [$level] $metadata$message\n",
#       metadata: [:user_id]

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#{Mix.env}.exs"

#config :logger, [
#	level: :debug,
#	backends: [Logger.Backends.Syslog, :console],
#	syslog: [facility: :local1, port: 1999, appid: "PASS3"]
#]



config :lager, [
	handlers: [
		lager_console_backend: :info
		#[{:lager_console_backend, :info}]
		#[{:lager_syslog_backend, ["pass3_app", :local1, :debug]}]
	]
]

config :lager, [
	handlers: [
		lager_syslog_backend: ['pass3_app', :local1, :debug]
	]
]


#{lager, [
#  {log_root, "/var/log/hello"},
#  {handlers, [
#    {lager_console_backend, info},
#    {lager_file_backend, [{file, "error.log"}, {level, error}]},
#    {lager_file_backend, [{file, "console.log"}, {level, info}]}
#  ]}
#		]}.
#
#{handlers, [
#  {lager_syslog_backend, ["riak", local1, info]},
#] },
