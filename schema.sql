
CREATE TABLE User (
    _id           INTEGER NOT NULL PRIMARY KEY
   ,username      VARCHAR(50) NOT NULL
   ,password      CHAR(40) NOT NULL
   ,focus         INTEGER NOT NULL REFERENCES Node (_id)
   );

CREATE TABLE Session (
    _id           INTEGER NOT NULL PRIMARY KEY
   ,user          INTEGER NOT NULL REFERENCES User (_id)
   ,hash          CHAR(40) NOT NULL
   );

CREATE TABLE Node (
    _id           INTEGER NOT NULL PRIMARY KEY
   ,user          INTEGER NOT NULL REFERENCES User (_id)
   ,title         VARCHAR(100) NOT NULL
   ,text          TEXT
   );

-- no need to restrict by User here, the Node indices will already provide
CREATE TABLE Link (
    _id           INTEGER NOT NULL PRIMARY KEY
   ,node_from     INTEGER NOT NULL REFERENCES Node (_id)
   ,node_to       INTEGER NOT NULL REFERENCES Node (_id)
   );

