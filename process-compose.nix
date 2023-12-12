{ config, pkgs, lib, ... }:
let
  srvname = "passetto";
  dbName = "passetto";
  userName = "passetto";
  pgcfg = config.services.postgres."${srvname}-db";
in
{
  options = {
    services.passetto = lib.mkOption {
      type = lib.types.submodule {
        options = {
          enable = lib.mkEnableOption "Enable passetto service";
          package = lib.mkPackageOption pkgs.haskellPackages "passetto-service" { };
          pgurl = lib.mkOption {
            type = lib.types.str;
            default = "postgresql://${userName}@${pgcfg.listen_addresses}:${builtins.toString pgcfg.port}/${dbName}";
            description = "Postgres connection string";
          };
          pgweb.enable = lib.mkEnableOption "Enable pgweb on passetto db";
          initialScript = lib.mkOption {
            type = lib.types.str;
            default = "";
            description = ''
              Initial SQL Commands to run during databse initialization. This can be multiple 
              SQL expressions separated by a semi-colon.
              '';
            example = lib.literalExpression ''
              CREATE USER postgres SUPERUSER;
              CREATE USER bar;
            '';
          };
        };
      };
    };
  };
  config =
    let
      cfg = config.services.passetto;
    in
    lib.mkIf cfg.enable {
      services.postgres."${srvname}-db" = {
        enable = true;
        listen_addresses = "127.0.0.1";
        hbaConf = [
          # Equivalent to `POSTGRES_INITDB_ARGS = "--auth=scram-sha-256";`, sets the auth for all users
          # connecting through unix sockets.
          { type = "local"; database = "all"; user = "all"; address = ""; method = "scram-sha-256"; }
          # Equivalent to `POSTGRES_HOST_AUTH_METHOD = "scram-sha-256";`, sets the auth for all users
          # connecting through loopback ipv4/v6
          { type = "host"; database = "all"; user = "all"; address = "127.0.0.1/32"; method = "scram-sha-256"; }
          { type = "host"; database = "all"; user = "all"; address = "::1/128"; method = "scram-sha-256"; }
        ];
        initialScript.before = ''
          CREATE ROLE ${userName} SUPERUSER;
          ALTER ROLE ${userName} WITH LOGIN;
        '' + "\n" + cfg.initialScript;
        initialDatabases = [
          {
            name = dbName;
            schema = ./service/pgsql/create_schema.sql;
          }
        ];
      };
      settings = {
        processes = {
          "${srvname}-pgweb" = lib.mkIf cfg.pgweb.enable {
            environment.PGWEB_DATABASE_URL = cfg.pgurl;
            command = pkgs.pgweb;
            depends_on."${srvname}-db".condition = "process_healthy";
          };
          passetto-service = { name, ... }: {
            environment.PASSETTO_PG_BACKEND_CONN_STRING = cfg.pgurl;
            depends_on."${srvname}-db".condition = "process_healthy";
            command = pkgs.writeShellApplication {
              inherit name;
              text = ''
                export PATH=${cfg.package}/bin:$PATH
                set -x
                password="1"
                keys=3
                passetto-init $password $keys
                MASTER_PASSWORD=$password passetto-server
              '';
            };
          };
        };
      };
    };
}
