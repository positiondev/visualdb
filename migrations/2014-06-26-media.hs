{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Database.Migrate
import Site
import Snap.Snaplet.Groundhog.Postgresql
import NeatInterpolation

main = runMainSnap app $ do
  upSql upstr
  downSql downstr


upstr = [string|
CREATE TABLE Media (
  id serial primary key,
  artist_id integer not null references Artist(id),
  url text not null
  );
|]


downstr = [string|
  DROP TABLE Media;
|]
