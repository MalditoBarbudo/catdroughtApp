# connection
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  'ifn', 'laboratoriforestal.creaf.cat', 5432, Sys.getenv('ifn_pass'),
  'ifn'
)

# create database and activate postgis
sql_table_creation_1 <- glue::glue_sql(
  .con = conn,
  "
  CREATE DATABASE catdrought_db;
  "
)
sql_table_creation_2 <- glue::glue_sql(
  .con = conn,
  "
  GRANT ALL PRIVILEGES ON DATABASE catdrought_db TO ifn;
  "
)
sql_table_creation_3 <- glue::glue_sql(
  .con = conn,
  "
  GRANT CONNECT ON DATABASE catdrought_db TO guest;
  "
)

RPostgres::dbExecute(conn, sql_table_creation_1)
RPostgres::dbExecute(conn, sql_table_creation_2)
RPostgres::dbExecute(conn, sql_table_creation_3)

# change conn
RPostgres::dbDisconnect(conn)
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  'catdrought_db', 'laboratoriforestal.creaf.cat',
  5432, Sys.getenv('ifn_pass'), 'ifn'
)

schema_query <- glue::glue_sql(
  .con = conn,
  "CREATE SCHEMA IF NOT EXISTS daily;"
)

sql_guest_activation_1 <- glue::glue_sql(
  "
  GRANT USAGE ON SCHEMA public TO guest;
  "
)
sql_guest_activation_2 <- glue::glue_sql(
  "
  GRANT SELECT ON ALL TABLES IN SCHEMA public TO guest;
  "
)
sql_guest_activation_3 <- glue::glue_sql(
  "
  ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT SELECT ON TABLES TO guest;
  "
)
sql_guest_activation_4 <- glue::glue_sql(
  "
  GRANT USAGE ON SCHEMA daily TO guest;
  "
)
sql_guest_activation_5 <- glue::glue_sql(
  "
  GRANT SELECT ON ALL TABLES IN SCHEMA daily TO guest;
  "
)
sql_guest_activation_6 <- glue::glue_sql(
  "
  ALTER DEFAULT PRIVILEGES IN SCHEMA daily
    GRANT SELECT ON TABLES TO guest;
  "
)

RPostgres::dbExecute(conn, sql_guest_activation_1)
RPostgres::dbExecute(conn, sql_guest_activation_2)
RPostgres::dbExecute(conn, sql_guest_activation_3)
RPostgres::dbExecute(conn, sql_guest_activation_4)
RPostgres::dbExecute(conn, sql_guest_activation_5)
RPostgres::dbExecute(conn, sql_guest_activation_6)
RPostgres::dbExecute(conn, schema_query)

# postgis extension
rpostgis::pgPostGIS(conn, topology = TRUE, sfcgal = TRUE)

# create original tables
drop_table_query_high <- glue::glue_sql(
  .con = conn,
  "DROP TABLE IF EXISTS daily.catdrought_high CASCADE;"
)
drop_table_query_low <- glue::glue_sql(
  .con = conn,
  "DROP TABLE IF EXISTS daily.catdrought_low CASCADE;"
)
drop_table_query_smooth <- glue::glue_sql(
  .con = conn,
  "DROP TABLE IF EXISTS daily.catdrought_smooth CASCADE;"
)

create_table_query_high <- glue::glue_sql(
  .con = conn,
  "
   CREATE TABLE daily.catdrought_high (
       id serial NOT NULL PRIMARY KEY,
       rid int NOT NULL,
       day date NOT NULL,
       rast raster
   );
   "
)

create_table_query_low <- glue::glue_sql(
  .con = conn,
  "
   CREATE TABLE daily.catdrought_low (
       id serial NOT NULL PRIMARY KEY,
       rid int NOT NULL,
       day date NOT NULL,
       rast raster
   );
   "
)

create_table_query_smooth <- glue::glue_sql(
  .con = conn,
  "
   CREATE TABLE daily.catdrought_smooth (
       id serial NOT NULL PRIMARY KEY,
       rid int NOT NULL,
       day date NOT NULL,
       rast raster
   );
  "
)

pool::dbExecute(conn, drop_table_query_high)
pool::dbExecute(conn, create_table_query_high)
pool::dbExecute(conn, drop_table_query_low)
pool::dbExecute(conn, create_table_query_low)
pool::dbExecute(conn, drop_table_query_smooth)
pool::dbExecute(conn, create_table_query_smooth)


RPostgres::dbDisconnect(conn)
