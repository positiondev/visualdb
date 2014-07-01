{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Database.Migrate
import Site
import Snap.Snaplet.Groundhog.Postgresql
import NeatInterpolation

main = runMainSnap app $ do
  upSql upstr
  downSql downstr


upstr = [string|
CREATE TABLE Artist_Subject (
  id serial primary key,
  artist_id integer not null references Artist(id),
  subject_id integer not null references Subject(id)
  );
CREATE TABLE Focus (
  id serial primary key,
  title text not null
  );
CREATE TABLE Artist_Focus (
  id serial primary key,
  artist_id integer not null references Artist(id),
  focus_id integer not null references Focus(id)
  );
ALTER TABLE Artist DROP COLUMN subject_id;
ALTER TABLE Artist DROP COLUMN focus;
|]


downstr = [string|
  DROP TABLE Artist_Subject CASCADE;
  DROP TABLE Focus CASCADE;
  DROP TABLE Artist_Focus CASCADE;
  ALTER TABLE Artist ADD COLUMN subject_id integer not null references Subject(id) DEFAULT 1;
  ALTER TABLE Artist ADD COLUMN focus text not null DEFAULT '';
|]
