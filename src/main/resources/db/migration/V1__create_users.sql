create table "users" (
  "id" BLOB(16) PRIMARY KEY NOT NULL,
  "name" VARCHAR(254) NOT NULL,
  "email" VARCHAR(254),
  "password" VARCHAR(254) NOT NULL,
  "session_token" VARCHAR(254) NOT NULL
);
create unique index "unique_name" on "users" ("name");