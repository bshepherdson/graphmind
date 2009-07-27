
CREATE TABLE User (
    _id         INTEGER NOT NULL PRIMARY KEY
   ,username    VARCHAR(30) NOT NULL
   ,password    CHAR(64) NOT NULL
   );

CREATE TABLE Node (
    _id           INTEGER NOT NULL PRIMARY KEY
   ,user_id       INTEGER NOT NULL REFERENCES User (_id)
   ,title         VARCHAR(100) NOT NULL
   ,text          TEXT
   );


CREATE TABLE Link (
    _id           INTEGER NOT NULL PRIMARY KEY
   ,user_id       INTEGER NOT NULL REFERENCES User (_id)
   ,node_from     INTEGER NOT NULL REFERENCES Node (_id)
   ,node_to       INTEGER NOT NULL REFERENCES Node (_id)
   );

