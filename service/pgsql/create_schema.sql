-- SCHEMA: Passetto

CREATE SCHEMA IF NOT EXISTS "Passetto"
    AUTHORIZATION passetto;

-- Table: Passetto.Master
CREATE TABLE IF NOT EXISTS "Passetto"."Master"
(
    "key" bytea NOT NULL,
    "updatedat" timestamp with time zone default now()
)

TABLESPACE pg_default;

ALTER TABLE "Passetto"."Master"
    OWNER to passetto;

-- Table: Passetto.Keys
CREATE TABLE IF NOT EXISTS "Passetto"."Keys"
(
    "id" serial PRIMARY KEY,
    "encryptedkeypair" bytea NOT NULL,
    "createdat" timestamp with time zone default now()
)

TABLESPACE pg_default;

ALTER TABLE "Passetto"."Keys"
    OWNER to passetto;
