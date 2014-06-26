{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Database.Migrate
import Site
import Snap.Snaplet.Groundhog.Postgresql
import NeatInterpolation

main = runMainSnap app $ do
  upSql upstr
  downSql downstr


upstr = [string|
CREATE TABLE Subject (
  id serial primary key,
  title text not null
  );

CREATE TABLE Artist (
  id serial primary key,
  name text not null,
  website text not null,
  typ text not null,
  email text not null,
  agency text not null,
  city text not null,
  country text not null,
  focus text not null,
  subject_id integer references subject(id),
  notes text not null
  );
|]


downstr = [string|
  DROP TABLE Artist;
  DROP TABLE Subject;
|]
