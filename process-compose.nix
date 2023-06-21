{ config, pkgs, lib, ... }:
let
  srvname = "passetto";
  dbName = "passetto";
  userName = "passetto";
  pgcfg = config.services.postgres;
in
{
  options = {
    services.passetto = lib.mkOption {
      type = lib.types.submodule {
        options = {
          enable = lib.mkEnableOption "Enable passetto service";
          pgurl = lib.mkOption {
            type = lib.types.str;
            default = "postgresql://${userName}@${pgcfg.listen_addresses}:${builtins.toString pgcfg.port}/${dbName}";
            description = "Postgres connection string";
          };
          pgweb.enable = lib.mkEnableOption "Enable pgweb on passetto db";
        };
      };
    };
  };
  config =
    let
      cfg = config.services.passetto;
    in
    lib.mkIf cfg.enable {
      services.postgres = {
        enable = true;
        name = "${srvname}-db";
        listen_addresses = "127.0.0.1";
        # TODO: Configure these (matching docker image behaviour)
        # POSTGRES_HOST_AUTH_METHOD = "scram-sha-256";
        # POSTGRES_INITDB_ARGS = "--auth=scram-sha-256";
        # initdbArgs = ["--auth=scram-sha-256"];
        initialScript = ''
          CREATE ROLE ${userName} SUPERUSER;
          ALTER ROLE ${userName} WITH LOGIN;
        '';
        initialDatabases = [
          {
            name = dbName;
            schema = ./service/pgsql/create_schema.sql;
          }
        ];
      };
      settings = {
        processes."${srvname}-pgweb" = lib.mkIf cfg.pgweb.enable {
          environment.PGWEB_DATABASE_URL = cfg.pgurl;
          command = pkgs.pgweb;
          depends_on."passetto-db".condition = "process_healthy";
        };
      };
    };
}
